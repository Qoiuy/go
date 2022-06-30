// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package reflect

import (
	"internal/abi"
	"internal/goexperiment"
	"unsafe"
)

// These variables are used by the register assignment
// algorithm in this file.
// 本文件中记录了 一些变量, 这些变量用于 寄存器分配算法（ register assignment algorithm ）
//
// They should be modified with care (no other reflect code
// may be executing) and are generally only modified
// when testing this package.
// 你应该谨慎又小心地修改它们( 反射功能只有这里一个实现 没有其他的实现 ) ，通常只在测试此包时修改它们。
//
// They should never be set higher than their internal/abi
// constant counterparts, because the system relies on a
// structure that is at least large enough to hold the
// registers the system supports.
// 它们永远不应该设置得比对应的内部/abi常量高，因为系统依赖的结构至少要足够大，以容纳系统支持的寄存器。
//
// Currently they're set to zero because using the actual
// constants will break every part of the toolchain that
// uses reflect to call functions (e.g. go test, or anything
// that uses text/template). The values that are currently
// commented out there should be the actual values once
// we're ready to use the register ABI everywhere.
// 目前它们被设置为0，因为使用实际的常量将破坏使用反射调用函数的工具链的每个部分(例如go test，或任何使用文本/模板的东西)。
// 当我们准备在所有地方使用寄存器ABI时，当前注释掉的值应该是实际值。

var (
	// IntArgRegs is the number of registers dedicated
	// to passing integer argument values. Result registers are identical
	// to argument registers, so this number is used for those too.
	// IntArgRegs是专用于传递整型参数值的寄存器的数量。
	// 结果寄存器与参数寄存器相同，所以这个数字也用于这些寄存器。
	// 参考 src/internal/abi/abi_generic.go:29
	//
	// 在原始的值的基础上 乘上一个固定的值 这里是 goexperiment.RegabiArgsInt
	intArgRegs   = abi.IntArgRegs * goexperiment.RegabiArgsInt
	floatArgRegs = abi.FloatArgRegs * goexperiment.RegabiArgsInt
	floatRegSize = uintptr(abi.EffectiveFloatRegSize * goexperiment.RegabiArgsInt)
)

// abiStep represents an ABI "instruction." Each instruction
// describes one part of how to translate between a Go value
// in memory and a call frame.
// abiStep表示一条ABI“指令”。 每条指令都描述了如何在内存中的Go值和调用帧之间进行转换的一部分。
type abiStep struct {
	kind abiStepKind

	// offset and size together describe a part of a Go value
	// in memory. offset和size一起描述了内存中Go值的一部分。
	offset uintptr
	size   uintptr // size in bytes of the part 部件的字节大小

	// These fields describe the ABI side of the translation.  这些字段描述翻译的ABI端。
	stkOff uintptr // stack offset, used if kind == abiStepStack  堆栈偏移量
	ireg   int     // integer register index, used if kind == abiStepIntReg or kind == abiStepPointer 整数寄存器索引
	freg   int     // FP register index, used if kind == abiStepFloatReg FP寄存器指数
}

// abiStepKind is the "op-code" for an abiStep instruction.
// abiStepKind是abiStep指令的“操作码”。
type abiStepKind int

const (
	abiStepBad      abiStepKind = iota // 默认值 叫 => bad 坏
	abiStepStack                // copy to/from stack => 堆栈
	abiStepIntReg               // copy to/from integer register => 整数
	abiStepPointer              // copy pointer to/from integer register => 指针
	abiStepFloatReg             // copy to/from FP register => 浮点数
)

// abiSeq represents a sequence of ABI instructions for copying
// from a series of reflect.Values to a call frame (for call arguments)
// or vice-versa (for call results).
// abiSeq表示一个ABI指令序列，用于从一系列reflect中复制。值到调用框架(用于调用参数)，反之亦然(用于调用结果)。
//
// An abiSeq should be populated by calling its addArg method.
// abiSeq应该通过调用它的addArg方法来填充。
type abiSeq struct {
	// steps is the set of instructions.
	// 步骤是指令的集合。
	//
	// The instructions are grouped together by whole arguments,
	// with the starting index for the instructions
	// of the i'th Go value available in valueStart.
	// 这些指令由整个参数组合在一起，valueStart中提供了第i个Go值的指令的起始索引。
	//
	// For instance, if this abiSeq represents 3 arguments
	// passed to a function, then the 2nd argument's steps
	// begin at steps[valueStart[1]].
	// 例如，如果这个abiSeq表示传递给一个函数的3个参数，那么第二个参数的步骤从步骤[valueStart[1]]开始。
	//
	// Because reflect accepts Go arguments in distinct
	// Values and each Value is stored separately, each abiStep
	// that begins a new argument will have its offset
	// field == 0.
	// 因为reflect接受不同的值的Go参数，每个值是单独存储的，每个开始一个新参数的abiStep将有它的偏移字段== 0。
	steps      []abiStep
	valueStart []int

	stackBytes   uintptr // stack space used 栈空间使用
	iregs, fregs int     // registers used 注册使用
}

func (a *abiSeq) dump() {
	for i, p := range a.steps {
		println("part", i, p.kind, p.offset, p.size, p.stkOff, p.ireg, p.freg)
	}
	print("values ")
	for _, i := range a.valueStart {
		print(i, " ")
	}
	println()
	println("stack", a.stackBytes)
	println("iregs", a.iregs)
	println("fregs", a.fregs)
}

// stepsForValue returns the ABI instructions for translating
// the i'th Go argument or return value represented by this
// abiSeq to the Go ABI.
// stepsForValue返回ABI指令，用于将第i个Go参数或由abiSeq表示的返回值转换为Go ABI。
func (a *abiSeq) stepsForValue(i int) []abiStep {
	s := a.valueStart[i]
	var e int
	if i == len(a.valueStart)-1 {
		e = len(a.steps)
	} else {
		e = a.valueStart[i+1]
	}
	return a.steps[s:e]
}

// addArg extends the abiSeq with a new Go value of type t.
// addArg将abiSeq扩展为一个新的t类型的Go值。
//
// If the value was stack-assigned, returns the single
// abiStep describing that translation, and nil otherwise.
// 如果该值是堆栈赋值的，则返回描述该转换的单个abiStep，否则返回nil。
func (a *abiSeq) addArg(t *rtype) *abiStep {
	// We'll always be adding a new value, so do that first.
	// 我们总是会添加一个新值，所以先做这个。
	pStart := len(a.steps)
	a.valueStart = append(a.valueStart, pStart)
	if t.size == 0 {
		// If the size of the argument type is zero, then
		// in order to degrade gracefully into ABI0, we need
		// to stack-assign this type. The reason is that
		// although zero-sized types take up no space on the
		// stack, they do cause the next argument to be aligned.
		// So just do that here, but don't bother actually
		// generating a new ABI step for it (there's nothing to
		// actually copy).
		// 如果参数类型的大小为0，那么为了优雅地降级为ABI0，我们需要对该类型进行堆栈赋值。
		// 原因是，尽管零大小的类型不占用堆栈空间，但它们确实会导致下一个参数被对齐。
		// 因此，只需在这里这样做，但不必为它实际生成一个新的ABI步骤(实际上没有什么要复制的)。
		//
		// We cannot handle this in the recursive case of
		// regAssign because zero-sized *fields* of a
		// non-zero-sized struct do not cause it to be
		// stack-assigned. So we need a special case here
		// at the top.
		// 我们无法在regAssign的递归情况下处理这个问题，因为非零大小struct的零大小的*字段*不会导致它被堆栈赋值。
		// 所以我们需要一个特殊的例子。
		a.stackBytes = align(a.stackBytes, uintptr(t.align))
		return nil
	}
	// Hold a copy of "a" so that we can roll back if
	// register assignment fails.
	// 保存“a”的副本，以便在寄存器赋值失败时进行回滚。
	aOld := *a
	if !a.regAssign(t, 0) {
		// Register assignment failed. Roll back any changes
		// and stack-assign.
		// 寄存器分配失败。回滚所有更改和堆栈赋值。
		*a = aOld
		a.stackAssign(t.size, uintptr(t.align))
		return &a.steps[len(a.steps)-1]
	}
	return nil
}

// addRcvr extends the abiSeq with a new method call
// receiver according to the interface calling convention.
// addRcvr根据接口调用约定对abiSeq进行了扩展，增加了一个新的方法call receiver。
//
// If the receiver was stack-assigned, returns the single
// abiStep describing that translation, and nil otherwise.
// Returns true if the receiver is a pointer.
// 如果接收方是堆栈赋值的，则返回描述转换的abiStep，否则返回nil。如果接收方是指针则返回true。
func (a *abiSeq) addRcvr(rcvr *rtype) (*abiStep, bool) {
	// The receiver is always one word.
	// 接受者总是一个词。
	a.valueStart = append(a.valueStart, len(a.steps))
	var ok, ptr bool
	if ifaceIndir(rcvr) || rcvr.pointers() {
		ok = a.assignIntN(0, ptrSize, 1, 0b1)
		ptr = true
	} else {
		// TODO(mknyszek): Is this case even possible?
		// The interface data work never contains a non-pointer
		// value. This case was copied over from older code
		// in the reflect package which only conditionally added
		// a pointer bit to the reflect.(Value).Call stack frame's
		// GC bitmap.
		// 这种情况可能吗?
		// 接口数据工作从不包含非指针值。
		// 这种情况是从reflect包中的旧代码复制过来的，该代码只是有条件地向reflect添加了一个指针位(Value)。
		// 调用堆栈帧的GC位图。
		ok = a.assignIntN(0, ptrSize, 1, 0b0)
		ptr = false
	}
	if !ok {
		a.stackAssign(ptrSize, ptrSize)
		return &a.steps[len(a.steps)-1], ptr
	}
	return nil, ptr
}

// regAssign attempts to reserve argument registers for a value of
// type t, stored at some offset.
//
// It returns whether or not the assignment succeeded, but
// leaves any changes it made to a.steps behind, so the caller
// must undo that work by adjusting a.steps if it fails.
//
// This method along with the assign* methods represent the
// complete register-assignment algorithm for the Go ABI.
// regAssign尝试为t类型的值保留参数寄存器，存储在某个偏移位置。
// 无论赋值是否成功，它都会返回，但对a.steps所做的任何更改都会留在后面，
// 因此，如果赋值失败，调用者必须通过调整a.steps来撤销所做的工作。
// 这个方法和assign*方法代表了Go ABI的完整寄存器分配算法。
func (a *abiSeq) regAssign(t *rtype, offset uintptr) bool {
	switch t.Kind() {
	case UnsafePointer, Ptr, Chan, Map, Func:
		return a.assignIntN(offset, t.size, 1, 0b1)
	case Bool, Int, Uint, Int8, Uint8, Int16, Uint16, Int32, Uint32, Uintptr:
		return a.assignIntN(offset, t.size, 1, 0b0)
	case Int64, Uint64:
		switch ptrSize {
		case 4:
			return a.assignIntN(offset, 4, 2, 0b0)
		case 8:
			return a.assignIntN(offset, 8, 1, 0b0)
		}
	case Float32, Float64:
		return a.assignFloatN(offset, t.size, 1)
	case Complex64:
		return a.assignFloatN(offset, 4, 2)
	case Complex128:
		return a.assignFloatN(offset, 8, 2)
	case String:
		return a.assignIntN(offset, ptrSize, 2, 0b01)
	case Interface:
		return a.assignIntN(offset, ptrSize, 2, 0b10)
	case Slice:
		return a.assignIntN(offset, ptrSize, 3, 0b001)
	case Array:
		tt := (*arrayType)(unsafe.Pointer(t))
		switch tt.len {
		case 0:
			// There's nothing to assign, so don't modify
			// a.steps but succeed so the caller doesn't
			// try to stack-assign this value.
			// 没有什么要赋值的，所以不要修改。steps，但是会成功，所以调用者不会尝试堆栈赋值。
			return true
		case 1:
			return a.regAssign(tt.elem, offset)
		default:
			return false
		}
	case Struct:
		st := (*structType)(unsafe.Pointer(t))
		for i := range st.fields {
			f := &st.fields[i]
			if !a.regAssign(f.typ, offset+f.offset()) {
				return false
			}
		}
		return true
	default:
		print("t.Kind == ", t.Kind(), "\n")
		panic("unknown type kind")
	}
	panic("unhandled register assignment path")
}

// assignIntN assigns n values to registers, each "size" bytes large,
// from the data at [offset, offset+n*size) in memory. Each value at
// [offset+i*size, offset+(i+1)*size) for i < n is assigned to the
// next n integer registers.
// assignIntN从内存中的[offset, offset+n*size]为寄存器分配n个值，每个“size”字节为大。
// 对于i < n，在[offset+i*size, offset+(i+1)*size)处的每个值被分配给下一个n整数寄存器。
//
// Bit i in ptrMap indicates whether the i'th value is a pointer.
// n must be <= 8.
// ptrMap中的第i位表示第i个值是否为指针。N必须<= 8。返回赋值是否成功。
//
// Returns whether assignment succeeded.
// N必须<= 8。返回赋值是否成功。
func (a *abiSeq) assignIntN(offset, size uintptr, n int, ptrMap uint8) bool {
	if n > 8 || n < 0 {
		panic("invalid n")
	}
	if ptrMap != 0 && size != ptrSize {
		panic("non-empty pointer map passed for non-pointer-size values")
	}
	if a.iregs+n > intArgRegs {
		return false
	}
	for i := 0; i < n; i++ {
		kind := abiStepIntReg
		if ptrMap&(uint8(1)<<i) != 0 {
			kind = abiStepPointer
		}
		a.steps = append(a.steps, abiStep{
			kind:   kind,
			offset: offset + uintptr(i)*size,
			size:   size,
			ireg:   a.iregs,
		})
		a.iregs++
	}
	return true
}

// assignFloatN assigns n values to registers, each "size" bytes large,
// from the data at [offset, offset+n*size) in memory. Each value at
// [offset+i*size, offset+(i+1)*size) for i < n is assigned to the
// next n floating-point registers.
// assignFloatN从内存中[offset, offset+n*size]的数据向寄存器分配n个值，每个“size”字节为大。
// 对于i < n， [offset+i*size, offset+(i+1)*size)处的每个值被赋给下一个n个浮点寄存器。
//
// Returns whether assignment succeeded.
// 返回赋值是否成功。
func (a *abiSeq) assignFloatN(offset, size uintptr, n int) bool {
	if n < 0 {
		panic("invalid n")
	}
	if a.fregs+n > floatArgRegs || floatRegSize < size {
		return false
	}
	for i := 0; i < n; i++ {
		a.steps = append(a.steps, abiStep{
			kind:   abiStepFloatReg,
			offset: offset + uintptr(i)*size,
			size:   size,
			freg:   a.fregs,
		})
		a.fregs++
	}
	return true
}

// stackAssign reserves space for one value that is "size" bytes
// large with alignment "alignment" to the stack.
//
// Should not be called directly; use addArg instead.
// 为一个大小为“size”字节的值预留空间，并与堆栈对齐。不应直接调用;使用addArg代替。
func (a *abiSeq) stackAssign(size, alignment uintptr) {
	a.stackBytes = align(a.stackBytes, alignment)
	a.steps = append(a.steps, abiStep{
		kind:   abiStepStack,
		offset: 0, // Only used for whole arguments, so the memory offset is 0. 只用于整个参数，所以内存偏移量为0。
		size:   size,
		stkOff: a.stackBytes,
	})
	a.stackBytes += size
}

// abiDesc describes the ABI for a function or method.
//abiDesc用于描述函数或方法的ABI。
type abiDesc struct {
	// call and ret represent the translation steps for
	// the call and return paths of a Go function.
	//call和ret表示Go函数的调用路径和返回路径的转换步骤。
	call, ret abiSeq

	// These fields describe the stack space allocated
	// for the call. stackCallArgsSize is the amount of space
	// reserved for arguments but not return values. retOffset
	// is the offset at which return values begin, and
	// spill is the size in bytes of additional space reserved
	// to spill argument registers into in case of preemption in
	// reflectcall's stack frame.
	//这些字段描述为调用分配的堆栈空间。stackCallArgsSize是为参数而不是返回值保留的空间量。retOffset是返回值开始的偏移量，而spill是为防止在reflectcall的堆栈帧中被抢占而预留给spill参数寄存器的额外空间(以字节为单位)。
	stackCallArgsSize, retOffset, spill uintptr

	// stackPtrs is a bitmap that indicates whether
	// each word in the ABI stack space (stack-assigned
	// args + return values) is a pointer. Used
	// as the heap pointer bitmap for stack space
	// passed to reflectcall.
	//stackPtrs是一个位图，它指示ABI堆栈空间中的每个单词(堆栈赋值参数+返回值)是否为指针。用作传递给反射调用的堆栈空间的堆指针位图。
	stackPtrs *bitVector

	// inRegPtrs is a bitmap whose i'th bit indicates
	// whether the i'th integer argument register contains
	// a pointer. Used by makeFuncStub and methodValueCall
	// to make result pointers visible to the GC.
	//
	// outRegPtrs is the same, but for result values.
	// Used by reflectcall to make result pointers visible
	// to the GC.
	// inRegPtrs是一个位图，它的第i位表示第i个整数参数寄存器是否包含一个指针。由makeFuncStub和methodValueCall使用，使结果指针对GC可见。outRegPtrs是相同的，但用于结果值。由reflectcall使用，使结果指针对GC可见。
	inRegPtrs, outRegPtrs abi.IntArgRegBitmap
}

func (a *abiDesc) dump() {
	println("ABI")
	println("call")
	a.call.dump()
	println("ret")
	a.ret.dump()
	println("stackCallArgsSize", a.stackCallArgsSize)
	println("retOffset", a.retOffset)
	println("spill", a.spill)
	print("inRegPtrs:")
	dumpPtrBitMap(a.inRegPtrs)
	println()
	print("outRegPtrs:")
	dumpPtrBitMap(a.outRegPtrs)
	println()
}

func dumpPtrBitMap(b abi.IntArgRegBitmap) {
	for i := 0; i < intArgRegs; i++ {
		x := 0
		if b.Get(i) {
			x = 1
		}
		print(" ", x)
	}
}

func newAbiDesc(t *funcType, rcvr *rtype) abiDesc {
	// We need to add space for this argument to
	// the frame so that it can spill args into it.
	// 我们需要在框架中为这个参数添加空间，这样它就可以将参数输入到框架中。
	//
	// The size of this space is just the sum of the sizes
	// of each register-allocated type.
	// 这个空间的大小就是每个寄存器分配类型的大小之和。
	//
	// TODO(mknyszek): Remove this when we no longer have
	// caller reserved spill space.
	// TODO(mknyszek):当我们不再有调用者保留溢出空间时，删除这个。
	spill := uintptr(0)

	// Compute gc program & stack bitmap for stack arguments
	// 计算gc程序和堆栈参数的堆栈位图
	stackPtrs := new(bitVector)

	// Compute the stack frame pointer bitmap and register
	// pointer bitmap for arguments.
	// 计算参数的堆栈帧指针位图和寄存器指针位图。
	inRegPtrs := abi.IntArgRegBitmap{}

	// Compute abiSeq for input parameters.
	// 计算输入参数的abiSeq。
	var in abiSeq
	if rcvr != nil {
		stkStep, isPtr := in.addRcvr(rcvr)
		if stkStep != nil {
			if isPtr {
				stackPtrs.append(1)
			} else {
				stackPtrs.append(0)
			}
		} else {
			spill += ptrSize
		}
	}
	for i, arg := range t.in() {
		stkStep := in.addArg(arg)
		if stkStep != nil {
			addTypeBits(stackPtrs, stkStep.stkOff, arg)
		} else {
			spill = align(spill, uintptr(arg.align))
			spill += arg.size
			for _, st := range in.stepsForValue(i) {
				if st.kind == abiStepPointer {
					inRegPtrs.Set(st.ireg)
				}
			}
		}
	}
	spill = align(spill, ptrSize)

	// From the input parameters alone, we now know
	// the stackCallArgsSize and retOffset.
	// 仅从输入参数，我们现在就知道了stackCallArgsSize和retOffset。
	stackCallArgsSize := in.stackBytes
	retOffset := align(in.stackBytes, ptrSize)

	// Compute the stack frame pointer bitmap and register
	// pointer bitmap for return values.
	// 计算堆栈帧指针位图和返回值的寄存器指针位图。
	outRegPtrs := abi.IntArgRegBitmap{}

	// Compute abiSeq for output parameters.
	// 计算输出参数的abiSeq。
	var out abiSeq
	// Stack-assigned return values do not share
	// space with arguments like they do with registers,
	// so we need to inject a stack offset here.
	// Fake it by artificially extending stackBytes by
	// the return offset.
	// 堆栈赋值返回值不像寄存器那样与实参共享空间，因此我们需要在这里注入堆栈偏移量。
	// 通过人为地通过返回偏移量扩展stackBytes来伪造它。
	out.stackBytes = retOffset
	for i, res := range t.out() {
		stkStep := out.addArg(res)
		if stkStep != nil {
			addTypeBits(stackPtrs, stkStep.stkOff, res)
		} else {
			for _, st := range out.stepsForValue(i) {
				if st.kind == abiStepPointer {
					outRegPtrs.Set(st.ireg)
				}
			}
		}
	}
	// Undo the faking from earlier so that stackBytes
	// is accurate.
	// 撤销之前的伪造，以便stackBytes是准确的。
	out.stackBytes -= retOffset
	return abiDesc{in, out, stackCallArgsSize, retOffset, spill, stackPtrs, inRegPtrs, outRegPtrs}
}
