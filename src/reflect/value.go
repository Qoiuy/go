// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package reflect

import (
	"internal/abi"
	"internal/itoa"
	"internal/unsafeheader"
	"math"
	"runtime"
	"unsafe"
)

const ptrSize = 4 << (^uintptr(0) >> 63) // unsafe.Sizeof(uintptr(0)) but an ideal const

// Value is the reflection interface to a Go value.
// 值是Go值的反射接口。
//
// Not all methods apply to all kinds of values. Restrictions,
// if any, are noted in the documentation for each method.
// Use the Kind method to find out the kind of value before
// calling kind-specific methods. Calling a method
// inappropriate to the kind of type causes a run time panic.
// 并非所有方法都适用于所有类型的值。 每种方法的文档中都注明了限制条件(如果有的话)。
// 在调用特定于类型的方法之前，使用Kind方法找出值的类型。 调用不适合类型类型的方法会导致运行时panic。
//
// The zero Value represents no value.
// Its IsValid method returns false, its Kind method returns Invalid,
// its String method returns "<invalid Value>", and all other methods panic.
// Most functions and methods never return an invalid value.
// If one does, its documentation states the conditions explicitly.
// 零值表示没有值。
// 它的IsValid方法返回false，它的Kind方法返回Invalid，
// 它的String方法返回“<无效值>”，所有其他方法都为panic。
// 大多数函数和方法从不返回无效值。 如果有，它的文档会显式地说明条件。
//
// A Value can be used concurrently by multiple goroutines provided that
// the underlying Go value can be used concurrently for the equivalent
// direct operations.
// 一个值可以被多个gor例程并发使用，前提是底层的Go值可以并发地用于等价的直接操作。
//
// To compare two Values, compare the results of the Interface method.
// Using == on two Values does not compare the underlying values
// they represent.
// 要比较两个值，请比较Interface方法的结果。对两个value使用==不会比较它们表示的基础值。
type Value struct {
	// typ holds the type of the value represented by a Value.
	// typ保存value所表示的值的类型。
	typ *rtype

	// Pointer-valued data or, if flagIndir is set, pointer to data.
	// Valid when either flagIndir is set or typ.pointers() is true.
	// 指针值数据，如果设置了flagIndir，则指向数据的指针。
	// 当设置了flagIndir或type .pointer()为真时有效。
	ptr unsafe.Pointer

	// flag holds metadata about the value.
	// The lowest bits are flag bits:
	//	- flagStickyRO: obtained via unexported not embedded field, so read-only
	//	- flagEmbedRO: obtained via unexported embedded field, so read-only
	//	- flagIndir: val holds a pointer to the data
	//	- flagAddr: v.CanAddr is true (implies flagIndir)
	//	- flagMethod: v is a method value.
	// The next five bits give the Kind of the value.
	// This repeats typ.Kind() except for method values.
	// The remaining 23+ bits give a method number for method values.
	// If flag.kind() != Func, code can assume that flagMethod is unset.
	// If ifaceIndir(typ), code can assume that flagIndir is set.
	// 标志保存有关该值的元数据。最低位是标志位:
	//  — flagStickyRO:通过未导出的未嵌入字段获得，因此是只读的
	//  — flagEmbedRO:通过未导出的嵌入字段获取，因此是只读的
	//  — flagIndir: Val保存一个指向数据的指针。
	//	- flagAddr: v.CanAddr为true(提示为flagIndir)
	//	- flagMethod: V是一个方法值。
	//  接下来的5位给出了值的类型。这重复了type.kind()，方法值除外。
	//  剩下的23+位表示方法值的方法编号。如果flag.kind() != Func，代码可以假定flagMethod未设置。
	//  如果ifaceIndir(typ)，代码可以假定设置了flagIndir。
	flag

	// A method value represents a curried method invocation
	// like r.Read for some receiver r. The typ+val+flag bits describe
	// the receiver r, but the flag's Kind bits say Func (methods are
	// functions), and the top bits of the flag give the method number
	// in r's type's method table.
	// 方法值代表一个curry过的方法调用，如r。
	// read用于接收方r。
	// typ+val+标志位描述接收方r，但标志的Kind位表示Func(方法是函数)，标志的顶部位给出r类型的方法表中的方法号。
}

type flag uintptr

const (
	flagKindWidth        = 5 // there are 27 kinds
	flagKindMask    flag = 1<<flagKindWidth - 1
	flagStickyRO    flag = 1 << 5
	flagEmbedRO     flag = 1 << 6
	flagIndir       flag = 1 << 7
	flagAddr        flag = 1 << 8
	flagMethod      flag = 1 << 9
	flagMethodShift      = 10
	flagRO               = flagStickyRO | flagEmbedRO
)

func (f flag) kind() Kind {
	return Kind(f & flagKindMask)
}

func (f flag) ro() flag {
	if f&flagRO != 0 {
		return flagStickyRO
	}
	return 0
}

// pointer returns the underlying pointer represented by v.
// v.Kind() must be Ptr, Map, Chan, Func, or UnsafePointer
// if v.Kind() == Ptr, the base type must not be go:notinheap.
// 如果v.v.k ind() == Ptr，基类型不能为go:notinheap，
// 则v.v.k ind()返回的底层指针必须是Ptr、Map、Chan、Func或UnsafePointer。
func (v Value) pointer() unsafe.Pointer {
	if v.typ.size != ptrSize || !v.typ.pointers() {
		panic("can't call pointer on a non-pointer Value")
	}
	if v.flag&flagIndir != 0 {
		return *(*unsafe.Pointer)(v.ptr)
	}
	return v.ptr
}

// packEface converts v to the empty interface.
// packEface将v转换为空接口。
func packEface(v Value) interface{} {
	t := v.typ
	var i interface{}
	e := (*emptyInterface)(unsafe.Pointer(&i))
	// First, fill in the data portion of the interface.
	// 首先，填写接口的数据部分。
	switch {
	case ifaceIndir(t):
		if v.flag&flagIndir == 0 {
			panic("bad indir")
		}
		// Value is indirect, and so is the interface we're making.
		// 价值是间接的，我们所创造的界面也是如此。
		ptr := v.ptr
		if v.flag&flagAddr != 0 {
			// TODO: pass safe boolean from valueInterface so
			// we don't need to copy if safe==true?
			// TODO:从valueInterface传递安全布尔值，
			// 所以我们不需要复制，如果安全==true?
			c := unsafe_New(t)
			typedmemmove(t, c, ptr)
			ptr = c
		}
		e.word = ptr
	case v.flag&flagIndir != 0:
		// Value is indirect, but interface is direct. We need
		// to load the data at v.ptr into the interface data word.
		// 价值是间接的，但界面是直接的。我们需要将v.ptr的数据加载到接口数据字中。
		e.word = *(*unsafe.Pointer)(v.ptr)
	default:
		// Value is direct, and so is the interface.
		// 值是直接的，接口也是。
		e.word = v.ptr
	}
	// Now, fill in the type portion. We're very careful here not
	// to have any operation between the e.word and e.typ assignments
	// that would let the garbage collector observe the partially-built
	// interface value.
	// 现在，填写类型部分。我们在这里非常小心，不要在e.word和e.typ赋值之间进行任何操作，以免垃圾收集器观察到部分构建的接口值。
	e.typ = t
	return i
}

// unpackEface converts the empty interface i to a Value.
// unpackEface将空接口i转换为Value。
func unpackEface(i interface{}) Value {
	e := (*emptyInterface)(unsafe.Pointer(&i))
	// NOTE: don't read e.word until we know whether it is really a pointer or not.
	// 注意:在我们知道e.word是否真的是一个指针之前，不要读它。
	t := e.typ
	if t == nil {
		return Value{}
	}
	f := flag(t.Kind())
	if ifaceIndir(t) {
		f |= flagIndir
	}
	return Value{t, e.word, f}
}

// A ValueError occurs when a Value method is invoked on
// a Value that does not support it. Such cases are documented
// in the description of each method.
// 当在不支持Value方法的情况下调用Value方法时，会发生ValueError。
// 这种情况在每种方法的描述中都有记录。
type ValueError struct {
	Method string
	Kind   Kind
}

func (e *ValueError) Error() string {
	if e.Kind == 0 {
		return "reflect: call of " + e.Method + " on zero Value"
	}
	return "reflect: call of " + e.Method + " on " + e.Kind.String() + " Value"
}

// methodName returns the name of the calling method,
// assumed to be two stack frames above.
// methodName返回调用方法的名称，假设是上面两个堆栈帧。
func methodName() string {
	pc, _, _, _ := runtime.Caller(2)
	f := runtime.FuncForPC(pc)
	if f == nil {
		return "unknown method"
	}
	return f.Name()
}

// methodNameSkip is like methodName, but skips another stack frame.
// This is a separate function so that reflect.flag.mustBe will be inlined.
// methodNameSkip类似于methodName，但会跳过另一个堆栈帧。
// 这是一个单独的函数，因此reflect.flag.mustBe将会内联。
func methodNameSkip() string {
	pc, _, _, _ := runtime.Caller(3)
	f := runtime.FuncForPC(pc)
	if f == nil {
		return "unknown method"
	}
	return f.Name()
}

// emptyInterface is the header for an interface{} value.
// emptyInterface是一个接口{}值的头文件。
type emptyInterface struct {
	typ  *rtype
	word unsafe.Pointer
}

// nonEmptyInterface is the header for an interface value with methods.
// nonEmptyInterface是带有方法的接口值的头。
type nonEmptyInterface struct {
	// see ../runtime/iface.go:/Itab 看到 ../runtime/iface.go:/Itab
	itab *struct {
		ityp *rtype // static interface type 静态的接口类型
		typ  *rtype // dynamic concrete type 动态的具体类型
		hash uint32 // copy of typ.hash typ.hash副本
		_    [4]byte
		fun  [100000]unsafe.Pointer // method table 方法表
	}
	word unsafe.Pointer
}

// mustBe panics if f's kind is not expected.
// Making this a method on flag instead of on Value
// (and embedding flag in Value) means that we can write
// the very clear v.mustBe(Bool) and have it compile into
// v.flag.mustBe(Bool), which will only bother to copy the
// single important word for the receiver.
// 如果没有f的同类，一定会惊慌失措。
// 将其作为标记方法而不是Value方法(并将标记嵌入到Value中)意味着我们可以编写非常清晰的v.m mustbe (Bool)，
// 并将其编译为v.m flag. mustbe (Bool)，它只会为接收方复制单个重要的单词。
func (f flag) mustBe(expected Kind) {
	// TODO(mvdan): use f.kind() again once mid-stack inlining gets better
	// TODO(mvdan):一旦栈中内联变得更好，就再次使用f.kind()
	if Kind(f&flagKindMask) != expected {
		panic(&ValueError{methodName(), f.kind()})
	}
}

// mustBeExported panics if f records that the value was obtained using
// an unexported field.
// mustBeExported如果f记录了该值是使用未导出字段获得的，则会出现panic。
func (f flag) mustBeExported() {
	if f == 0 || f&flagRO != 0 {
		f.mustBeExportedSlow()
	}
}

func (f flag) mustBeExportedSlow() {
	if f == 0 {
		panic(&ValueError{methodNameSkip(), Invalid})
	}
	if f&flagRO != 0 {
		panic("reflect: " + methodNameSkip() + " using value obtained using unexported field")
	}
}

// mustBeAssignable panics if f records that the value is not assignable,
// which is to say that either it was obtained using an unexported field
// or it is not addressable.
// mustBeAssignable将在f记录该值不可分配的情况下惊慌，也就是说，
// 该值是使用未导出的字段获得的，或者它是不可寻址的。
func (f flag) mustBeAssignable() {
	if f&flagRO != 0 || f&flagAddr == 0 {
		f.mustBeAssignableSlow()
	}
}

func (f flag) mustBeAssignableSlow() {
	if f == 0 {
		panic(&ValueError{methodNameSkip(), Invalid})
	}
	// Assignable if addressable and not read-only.
	// 如果可寻址且非只读，则可赋值。
	if f&flagRO != 0 {
		panic("reflect: " + methodNameSkip() + " using value obtained using unexported field")
	}
	if f&flagAddr == 0 {
		panic("reflect: " + methodNameSkip() + " using unaddressable value")
	}
}

// Addr returns a pointer value representing the address of v.
// It panics if CanAddr() returns false.
// Addr is typically used to obtain a pointer to a struct field
// or slice element in order to call a method that requires a
// pointer receiver.
// Addr返回一个表示v.地址的指针值。如果CanAddr()返回false，就会出现恐慌。
// Addr通常用于获取指向struct字段或slice元素的指针，以便调用需要指针接收器的方法。
func (v Value) Addr() Value {
	if v.flag&flagAddr == 0 {
		panic("reflect.Value.Addr of unaddressable value")
	}
	// Preserve flagRO instead of using v.flag.ro() so that
	// v.Addr().Elem() is equivalent to v (#32772)
	// 保留flagRO而不是使用v.flag.ro()，以便v. addr ().Elem()等价于v (#32772)
	fl := v.flag & flagRO
	return Value{v.typ.ptrTo(), v.ptr, fl | flag(Ptr)}
}

// Bool returns v's underlying value.
// It panics if v's kind is not Bool.
// Bool返回v的基础值。如果v不是Bool类型，它会惊慌。
func (v Value) Bool() bool {
	v.mustBe(Bool)
	return *(*bool)(v.ptr)
}

// Bytes returns v's underlying value.
// It panics if v's underlying value is not a slice of bytes.
// Bytes返回v的底层值。如果v的底层值不是一个字节片，它就会惊慌。
func (v Value) Bytes() []byte {
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Uint8 {
		panic("reflect.Value.Bytes of non-byte slice")
	}
	// Slice is always bigger than a word; assume flagIndir.
	// 切片永远大于一个字;假设flagIndir。
	return *(*[]byte)(v.ptr)
}

// runes returns v's underlying value.
// It panics if v's underlying value is not a slice of runes (int32s).
// Runes返回v的基础值。如果v的基础值不是一块符文(32秒)，它会惊慌。
func (v Value) runes() []rune {
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Int32 {
		panic("reflect.Value.Bytes of non-rune slice")
	}
	// Slice is always bigger than a word; assume flagIndir.
	// 切片永远大于一个字;假设flagIndir。
	return *(*[]rune)(v.ptr)
}

// CanAddr reports whether the value's address can be obtained with Addr.
// Such values are called addressable. A value is addressable if it is
// an element of a slice, an element of an addressable array,
// a field of an addressable struct, or the result of dereferencing a pointer.
// If CanAddr returns false, calling Addr will panic.
// CanAddr报告是否可以通过Addr获取该值的地址。这样的值称为可寻址的。
// 如果一个值是片的一个元素，可寻址数组的一个元素，可寻址结构的一个字段，或解引用指针的结果，那么这个值就是可寻址的。
// 如果CanAddr返回false，调用Addr会引起恐慌。
func (v Value) CanAddr() bool {
	return v.flag&flagAddr != 0
}

// CanSet reports whether the value of v can be changed.
// A Value can be changed only if it is addressable and was not
// obtained by the use of unexported struct fields.
// If CanSet returns false, calling Set or any type-specific
// setter (e.g., SetBool, SetInt) will panic.
// CanSet报告是否可以修改v的值。只有当值是可寻址的且不是通过使用未导出的结构字段获得时，才可以更改值。
// 如果CanSet返回false，调用Set或任何类型特定的setter(例如SetBool, SetInt)将会出现panic。
func (v Value) CanSet() bool {
	return v.flag&(flagAddr|flagRO) == flagAddr
}

// Call calls the function v with the input arguments in.
// For example, if len(in) == 3, v.Call(in) represents the Go call v(in[0], in[1], in[2]).
// Call panics if v's Kind is not Func.
// It returns the output results as Values.
// As in Go, each input argument must be assignable to the
// type of the function's corresponding input parameter.
// If v is a variadic function, Call creates the variadic slice parameter
// itself, copying in the corresponding values.
// 调用输入参数为in的函数v。
// 例如，如果len(in) == 3, v. call (in)表示Go调用v(in[0]， in[1]， in[2])。
// 如果v星人不是Func就叫恐慌。它以值的形式返回输出结果。
// 在Go中，每个输入参数必须可赋值给函数对应的输入参数的类型。
// 如果v是一个可变参数函数，Call创建可变参数片本身，复制相应的值。
func (v Value) Call(in []Value) []Value {
	v.mustBe(Func)
	v.mustBeExported()
	return v.call("Call", in)
}

// CallSlice calls the variadic function v with the input arguments in,
// assigning the slice in[len(in)-1] to v's final variadic argument.
// For example, if len(in) == 3, v.CallSlice(in) represents the Go call v(in[0], in[1], in[2]...).
// CallSlice panics if v's Kind is not Func or if v is not variadic.
// It returns the output results as Values.
// As in Go, each input argument must be assignable to the
// type of the function's corresponding input parameter.
// CallSlice使用输入参数in调用可变参数函数v，并将[len(in)-1]中的切片赋值给v的最后一个可变参数。
// 例如，如果len(in) == 3, v. callslice (in)表示Go调用v(in[0]， in[1]， in[2]…)如果v的Kind不是Func或者v不是可变参数，CallSlice将会惊慌。
// 它以值的形式返回输出结果。在Go中，每个输入参数必须可赋值给函数对应的输入参数的类型。
func (v Value) CallSlice(in []Value) []Value {
	v.mustBe(Func)
	v.mustBeExported()
	return v.call("CallSlice", in)
}

var callGC bool // for testing; see TestCallMethodJump and TestCallArgLive 进行测试;参见TestCallMethodJump和TestCallArgLive

const debugReflectCall = false

func (v Value) call(op string, in []Value) []Value {
	// Get function pointer, type. 获取函数指针、类型。
	t := (*funcType)(unsafe.Pointer(v.typ))
	var (
		fn       unsafe.Pointer
		rcvr     Value
		rcvrtype *rtype
	)
	if v.flag&flagMethod != 0 {
		rcvr = v
		rcvrtype, t, fn = methodReceiver(op, v, int(v.flag)>>flagMethodShift)
	} else if v.flag&flagIndir != 0 {
		fn = *(*unsafe.Pointer)(v.ptr)
	} else {
		fn = v.ptr
	}

	if fn == nil {
		panic("reflect.Value.Call: call of nil function")
	}

	isSlice := op == "CallSlice"
	n := t.NumIn()
	isVariadic := t.IsVariadic()
	if isSlice {
		if !isVariadic {
			panic("reflect: CallSlice of non-variadic function")
		}
		if len(in) < n {
			panic("reflect: CallSlice with too few input arguments")
		}
		if len(in) > n {
			panic("reflect: CallSlice with too many input arguments")
		}
	} else {
		if isVariadic {
			n--
		}
		if len(in) < n {
			panic("reflect: Call with too few input arguments")
		}
		if !isVariadic && len(in) > n {
			panic("reflect: Call with too many input arguments")
		}
	}
	for _, x := range in {
		if x.Kind() == Invalid {
			panic("reflect: " + op + " using zero Value argument")
		}
	}
	for i := 0; i < n; i++ {
		if xt, targ := in[i].Type(), t.In(i); !xt.AssignableTo(targ) {
			panic("reflect: " + op + " using " + xt.String() + " as type " + targ.String())
		}
	}
	if !isSlice && isVariadic {
		// prepare slice for remaining values 为剩余值准备切片
		m := len(in) - n
		slice := MakeSlice(t.In(n), m, m)
		elem := t.In(n).Elem()
		for i := 0; i < m; i++ {
			x := in[n+i]
			if xt := x.Type(); !xt.AssignableTo(elem) {
				panic("reflect: cannot use " + xt.String() + " as type " + elem.String() + " in " + op)
			}
			slice.Index(i).Set(x)
		}
		origIn := in
		in = make([]Value, n+1)
		copy(in[:n], origIn)
		in[n] = slice
	}

	nin := len(in)
	if nin != t.NumIn() {
		panic("reflect.Value.Call: wrong argument count")
	}
	nout := t.NumOut()

	// Register argument space. 注册参数空间。
	var regArgs abi.RegArgs

	// Compute frame type. 计算帧类型。
	frametype, framePool, abi := funcLayout(t, rcvrtype)

	// Allocate a chunk of memory for frame if needed. 如果需要，为帧分配一块内存。
	var stackArgs unsafe.Pointer
	if frametype.size != 0 {
		if nout == 0 {
			stackArgs = framePool.Get().(unsafe.Pointer)
		} else {
			// Can't use pool if the function has return values.
			// We will leak pointer to args in ret, so its lifetime is not scoped.
			// 如果函数有返回值，就不能使用池。我们将在ret中泄漏指向args的指针，因此它的生存期没有作用域。
			stackArgs = unsafe_New(frametype)
		}
	}
	frameSize := frametype.size

	if debugReflectCall {
		println("reflect.call", t.String())
		abi.dump()
	}

	// Copy inputs into args.
	// 将输入复制到args中。

	// Handle receiver.
	// 处理接收机。
	inStart := 0
	if rcvrtype != nil {
		// Guaranteed to only be one word in size,
		// so it will only take up exactly 1 abiStep (either
		// in a register or on the stack).
		// 保证只有一个字的大小，所以它将只占用1 abiStep(在寄存器或堆栈上)。
		switch st := abi.call.steps[0]; st.kind {
		case abiStepStack:
			storeRcvr(rcvr, stackArgs)
		case abiStepIntReg, abiStepPointer:
			// Even pointers can go into the uintptr slot because
			// they'll be kept alive by the Values referenced by
			// this frame. Reflection forces these to be heap-allocated,
			// so we don't need to worry about stack copying.
			// 甚至指针也可以进入uintptr插槽，因为它们会被帧引用的值保持活跃性。
			// 反射强制将它们分配到堆中，因此我们不需要担心堆栈复制。
			storeRcvr(rcvr, unsafe.Pointer(&regArgs.Ints[st.ireg]))
		case abiStepFloatReg:
			storeRcvr(rcvr, unsafe.Pointer(&regArgs.Floats[st.freg]))
		default:
			panic("unknown ABI parameter kind")
		}
		inStart = 1
	}

	// Handle arguments.
	// 处理参数。
	for i, v := range in {
		v.mustBeExported()
		targ := t.In(i).(*rtype)
		// TODO(mknyszek): Figure out if it's possible to get some
		// scratch space for this assignment check. Previously, it
		// was possible to use space in the argument frame.
		// TODO(mknyszek):弄清楚是否有可能为这个作业检查获得一些临时空间。以前，可以在论证框架中使用空间。
		v = v.assignTo("reflect.Value.Call", targ, nil)
	stepsLoop:
		for _, st := range abi.call.stepsForValue(i + inStart) {
			switch st.kind {
			case abiStepStack:
				// Copy values to the "stack."
				// 将值复制到“堆栈”
				addr := add(stackArgs, st.stkOff, "precomputed stack arg offset")
				if v.flag&flagIndir != 0 {
					typedmemmove(targ, addr, v.ptr)
				} else {
					*(*unsafe.Pointer)(addr) = v.ptr
				}
				// There's only one step for a stack-allocated value.
				// 对于堆栈分配的值，只有一个步骤。
				break stepsLoop
			case abiStepIntReg, abiStepPointer:
				// Copy values to "integer registers."
				// 将值复制到“整数寄存器”。
				if v.flag&flagIndir != 0 {
					offset := add(v.ptr, st.offset, "precomputed value offset")
					if st.kind == abiStepPointer {
						// Duplicate this pointer in the pointer area of the
						// register space. Otherwise, there's the potential for
						// this to be the last reference to v.ptr.
						// 在寄存器空间的指针区域复制该指针。否则，这可能是v.ptr的最后一个引用。
						regArgs.Ptrs[st.ireg] = *(*unsafe.Pointer)(offset)
					}
					memmove(unsafe.Pointer(&regArgs.Ints[st.ireg]), offset, st.size)
				} else {
					if st.kind == abiStepPointer {
						// See the comment in abiStepPointer case above.
						//参见上面abiStepPointer案例中的注释。
						regArgs.Ptrs[st.ireg] = v.ptr
					}
					regArgs.Ints[st.ireg] = uintptr(v.ptr)
				}
			case abiStepFloatReg:
				// Copy values to "float registers."
				// 将值复制到“浮点寄存器”
				if v.flag&flagIndir == 0 {
					panic("attempted to copy pointer to FP register")
				}
				offset := add(v.ptr, st.offset, "precomputed value offset")
				memmove(unsafe.Pointer(&regArgs.Floats[st.freg]), offset, st.size)
			default:
				panic("unknown ABI part kind")
			}
		}
	}
	// TODO(mknyszek): Remove this when we no longer have
	// caller reserved spill space.
	// TODO(mknyszek):当我们不再有这个时，删除它
	frameSize = align(frameSize, ptrSize)
	frameSize += abi.spill

	// Mark pointers in registers for the return path.
	// 在寄存器中标记返回路径的指针。
	regArgs.ReturnIsPtr = abi.outRegPtrs

	if debugReflectCall {
		regArgs.Dump()
	}

	// For testing; see TestCallArgLive.
	// 进行测试;看到TestCallArgLive。
	if callGC {
		runtime.GC()
	}

	// Call. 调用。
	call(frametype, fn, stackArgs, uint32(frametype.size), uint32(abi.retOffset), uint32(frameSize), &regArgs)

	// For testing; see TestCallMethodJump.
	// 进行测试;看到TestCallMethodJump。
	if callGC {
		runtime.GC()
	}

	var ret []Value
	if nout == 0 {
		if stackArgs != nil {
			typedmemclr(frametype, stackArgs)
			framePool.Put(stackArgs)
		}
	} else {
		if stackArgs != nil {
			// Zero the now unused input area of args,
			// 零现在未使用的参数输入区域，
			// because the Values returned by this function contain pointers to the args object,
			// and will thus keep the args object alive indefinitely.
			// 因为此函数返回的值包含指向args对象的指针，因此将使args对象无限期地存活
			typedmemclrpartial(frametype, stackArgs, 0, abi.retOffset)
		}

		// Wrap Values around return values in args.
		// 将返回值封装在args中 o
		ret = make([]Value, nout)
		for i := 0; i < nout; i++ {
			tv := t.Out(i)
			if tv.Size() == 0 {
				// For zero-sized return value, args+off may point to the next object.
				// In this case, return the zero value instead.
				// 对于零大小的返回值，args+off可能指向下一个对象 在这种情况下，返回0值
				ret[i] = Zero(tv)
				continue
			}
			steps := abi.ret.stepsForValue(i)
			if st := steps[0]; st.kind == abiStepStack {
				// This value is on the stack. If part of a value is stack
				// allocated, the entire value is according to the ABI. So
				// just make an indirection into the allocated frame.
				// 这个值在堆栈上 如果堆栈分配了一个值的一部分，那么整个值是根据ABI分配的
				// 所以在分配的坐标系中做一个间接的操作
				fl := flagIndir | flag(tv.Kind())
				ret[i] = Value{tv.common(), add(stackArgs, st.stkOff, "tv.Size() != 0"), fl}
				// Note: this does introduce false sharing between results -
				// if any result is live, they are all live.
				// (And the space for the args is live as well, but as we've
				// cleared that space it isn't as big a deal.)
				// 注意:这引入了错误的结果之间的共享-如果任何结果是活的，它们都是活的
				// (游戏邦注:args的空间也很活跃，但因为我们已经清理了空间，所以这并不是什么大问题 )
				continue
			}

			// Handle pointers passed in registers.
			// 处理寄存器中传递的指针
			if !ifaceIndir(tv.common()) {
				// Pointer-valued data gets put directly
				// into v.ptr.
				// 指针值数据被直接放入v.ptr
				if steps[0].kind != abiStepPointer {
					print("kind=", steps[0].kind, ", type=", tv.String(), "\n")
					panic("mismatch between ABI description and types")
				}
				ret[i] = Value{tv.common(), regArgs.Ptrs[steps[0].ireg], flag(tv.Kind())}
				continue
			}

			// All that's left is values passed in registers that we need to
			// create space for and copy values back into.
			// 剩下的就是在寄存器中传递的值，我们需要为这些值创建空间并将值复制回去
			//
			// TODO(mknyszek): We make a new allocation for each register-allocated
			// value, but previously we could always point into the heap-allocated
			// stack frame. This is a regression that could be fixed by adding
			// additional space to the allocated stack frame and storing the
			// register-allocated return values into the allocated stack frame and
			// referring there in the resulting Value.
			// TODO(mknyszek):我们为每个寄存器分配的值进行新的分配，但以前我们总是可以指向堆分配的堆栈帧
			// 这是一种可以通过向已分配的堆栈帧添加额外空间并将寄存器分配的返回值存储到已分配的堆栈帧并在结果值中引用那里来修复的回归
			// s: = unsafe_New (tv.common ()
			s := unsafe_New(tv.common())
			for _, st := range steps {
				switch st.kind {
				case abiStepIntReg:
					offset := add(s, st.offset, "precomputed value offset")
					memmove(offset, unsafe.Pointer(&regArgs.Ints[st.ireg]), st.size)
				case abiStepPointer:
					s := add(s, st.offset, "precomputed value offset")
					*((*unsafe.Pointer)(s)) = regArgs.Ptrs[st.ireg]
				case abiStepFloatReg:
					offset := add(s, st.offset, "precomputed value offset")
					memmove(offset, unsafe.Pointer(&regArgs.Floats[st.freg]), st.size)
				case abiStepStack:
					panic("register-based return value has stack component")
				default:
					panic("unknown ABI part kind")
				}
			}
			ret[i] = Value{tv.common(), s, flagIndir | flag(tv.Kind())}
		}
	}

	return ret
}

// callReflect is the call implementation used by a function
// returned by MakeFunc. In many ways it is the opposite of the
// method Value.call above. The method above converts a call using Values
// into a call of a function with a concrete argument frame, while
// callReflect converts a call of a function with a concrete argument
// frame into a call using Values.
//
// It is in this file so that it can be next to the call method above.
// The remainder of the MakeFunc implementation is in makefunc.go.
//
// NOTE: This function must be marked as a "wrapper" in the generated code,
// so that the linker can make it work correctly for panic and recover.
// The gc compilers know to do that for the name "reflect.callReflect".
//
// ctxt is the "closure" generated by MakeFunc.
// frame is a pointer to the arguments to that closure on the stack.
// retValid points to a boolean which should be set when the results
// section of frame is set.
//
// regs contains the argument values passed in registers and will contain
// the values returned from ctxt.fn in registers.
// 注意:此函数必须在生成的代码中标记为“包装器”，以便链接器可以使其在恐慌和恢复时正常工作
// gc编译器知道对名称“reflect.callReflect”这样做
// ctext是由MakeFunc生成的“闭包” Frame是指向堆栈上那个闭包的参数的指针
// retValid指向一个布尔值，该值应该在设置帧的结果部分时设置
// Regs包含在寄存器中传递的参数值，并将包含从ctext返回的值
// fn在寄存器中
// 注意:此函数必须在生成的代码中标记为“包装器”，以便链接器可以使其在恐慌和恢复时正常工作
// gc编译器知道对名称“reflect.callReflect”这样做
// ctext是由MakeFunc生成的“闭包” Frame是指向堆栈上那个闭包的参数的指针
// retValid指向一个布尔值，该值应该在设置帧的结果部分时设置
// Regs包含在寄存器中传递的参数值，并将包含从ctext返回的值
// fn在寄存器中
func callReflect(ctxt *makeFuncImpl, frame unsafe.Pointer, retValid *bool, regs *abi.RegArgs) {
	if callGC {
		// Call GC upon entry during testing.
		// Getting our stack scanned here is the biggest hazard, because
		// our caller (makeFuncStub) could have failed to place the last
		// pointer to a value in regs' pointer space, in which case it
		// won't be visible to the GC.
		// 在测试期间进入时调用GC 在这里扫描堆栈是最大的危险，因为我们的调用者(makeFuncStub)可能没有将最后一个指向值的指针放在regs的指针空间中，在这种情况下，它对GC是不可见的
		runtime.GC()
	}
	ftyp := ctxt.ftyp
	f := ctxt.fn

	_, _, abi := funcLayout(ftyp, nil)

	// Copy arguments into Values.
	// 将参数复制到Values中
	ptr := frame
	in := make([]Value, 0, int(ftyp.inCount))
	for i, typ := range ftyp.in() {
		if typ.Size() == 0 {
			in = append(in, Zero(typ))
			continue
		}
		v := Value{typ, nil, flag(typ.Kind())}
		steps := abi.call.stepsForValue(i)
		if st := steps[0]; st.kind == abiStepStack {
			if ifaceIndir(typ) {
				// value cannot be inlined in interface data.
				// Must make a copy, because f might keep a reference to it,
				// and we cannot let f keep a reference to the stack frame
				// after this function returns, not even a read-only reference.
				// 值不能内联在接口数据中 必须创建一个副本，因为f可能会保留对它的引用，并且在函数返回后不能让f保留对堆栈帧的引用，即使是只读引用也不行
				v.ptr = unsafe_New(typ)
				if typ.size > 0 {
					typedmemmove(typ, v.ptr, add(ptr, st.stkOff, "typ.size > 0"))
				}
				v.flag |= flagIndir
			} else {
				v.ptr = *(*unsafe.Pointer)(add(ptr, st.stkOff, "1-ptr"))
			}
		} else {
			if ifaceIndir(typ) {
				// All that's left is values passed in registers that we need to
				// create space for the values.
				// 剩下的就是在寄存器中传递的值，我们需要为这些值创建空间
				v.flag |= flagIndir
				v.ptr = unsafe_New(typ)
				for _, st := range steps {
					switch st.kind {
					case abiStepIntReg:
						offset := add(v.ptr, st.offset, "precomputed value offset")
						memmove(offset, unsafe.Pointer(&regs.Ints[st.ireg]), st.size)
					case abiStepPointer:
						s := add(v.ptr, st.offset, "precomputed value offset")
						*((*unsafe.Pointer)(s)) = regs.Ptrs[st.ireg]
					case abiStepFloatReg:
						offset := add(v.ptr, st.offset, "precomputed value offset")
						memmove(offset, unsafe.Pointer(&regs.Floats[st.freg]), st.size)
					case abiStepStack:
						panic("register-based return value has stack component")
					default:
						panic("unknown ABI part kind")
					}
				}
			} else {
				// Pointer-valued data gets put directly
				// into v.ptr.
				// // 指针值数据被直接放入v.ptr
				if steps[0].kind != abiStepPointer {
					print("kind=", steps[0].kind, ", type=", typ.String(), "\n")
					panic("mismatch between ABI description and types")
				}
				v.ptr = regs.Ptrs[steps[0].ireg]
			}
		}
		in = append(in, v)
	}

	// Call underlying function.
	// 调用底层函数
	out := f(in)
	numOut := ftyp.NumOut()
	if len(out) != numOut {
		panic("reflect: wrong return count from function created by MakeFunc")
	}

	// Copy results back into argument frame and register space.
	// 将结果复制回参数框架和寄存器空间
	if numOut > 0 {
		for i, typ := range ftyp.out() {
			v := out[i]
			if v.typ == nil {
				panic("reflect: function created by MakeFunc using " + funcName(f) +
					" returned zero Value")
			}
			if v.flag&flagRO != 0 {
				panic("reflect: function created by MakeFunc using " + funcName(f) +
					" returned value obtained from unexported field")
			}
			if typ.size == 0 {
				continue
			}

			// Convert v to type typ if v is assignable to a variable
			// of type t in the language spec.
			// See issue 28761.
			// 如果v在语言规范中可赋值给t类型的变量，则将v转换为typ类型
			// 参见第28761期
			//
			// TODO(mknyszek): In the switch to the register ABI we lost
			// the scratch space here for the register cases (and
			// temporarily for all the cases).
			// TODO(mknyszek):在切换到寄存器ABI的过程中，我们在这里失去了寄存器案例(以及所有案例的临时scratch空间)
			//
			// If/when this happens, take note of the following:
			// 如果/当这种情况发生时，请注意以下几点:
			//
			// We must clear the destination before calling assignTo,
			// in case assignTo writes (with memory barriers) to the
			// target location used as scratch space. See issue 39541.
			// 我们必须在调用assignTo之前清除目标，以防assignTo将(带内存屏障)写入用作划痕空间的目标位置
			// 看到发行39541
			v = v.assignTo("reflect.MakeFunc", typ, nil)
		stepsLoop:
			for _, st := range abi.ret.stepsForValue(i) {
				switch st.kind {
				case abiStepStack:
					// Copy values to the "stack."
					// 将值复制到“堆栈” 将值复制到“堆栈”
					addr := add(ptr, st.stkOff, "precomputed stack arg offset")
					// Do not use write barriers. The stack space used
					// for this call is not adequately zeroed, and we
					// are careful to keep the arguments alive until we
					// return to makeFuncStub's caller.
					// 不要使用写障碍 用于此调用的堆栈空间没有充分归零，并且在返回makeFuncStub的调用方之前，我们要小心地保持参数为活动状态
					if v.flag&flagIndir != 0 {
						memmove(addr, v.ptr, st.size)
					} else {
						// This case must be a pointer type.
						// 这种情况下必须是指针类型
						*(*uintptr)(addr) = uintptr(v.ptr)
					}
					// There's only one step for a stack-allocated value.
					// 对于堆栈分配的值，只有一个步骤
					break stepsLoop
				case abiStepIntReg, abiStepPointer:
					// Copy values to "integer registers."
					// 将值复制到“整数寄存器”
					if v.flag&flagIndir != 0 {
						offset := add(v.ptr, st.offset, "precomputed value offset")
						memmove(unsafe.Pointer(&regs.Ints[st.ireg]), offset, st.size)
					} else {
						// Only populate the Ints space on the return path.
						// This is safe because out is kept alive until the
						// end of this function, and the return path through
						// makeFuncStub has no preemption, so these pointers
						// are always visible to the GC.
						// 只在返回路径上填充int型空间 这是安全的，因为out一直保持活动状态，直到这个函数结束，并且通过makeFuncStub的返回路径没有抢占，所以这些指针对GC总是可见的
						regs.Ints[st.ireg] = uintptr(v.ptr)
					}
				case abiStepFloatReg:
					// Copy values to "float registers."
					// 将值复制到“浮点寄存器”
					if v.flag&flagIndir == 0 {
						panic("attempted to copy pointer to FP register")
					}
					offset := add(v.ptr, st.offset, "precomputed value offset")
					memmove(unsafe.Pointer(&regs.Floats[st.freg]), offset, st.size)
				default:
					panic("unknown ABI part kind")
				}
			}
		}
	}

	// Announce that the return values are valid.
	// After this point the runtime can depend on the return values being valid.
	// 宣布返回值是有效的 在此之后，运行时可以依赖于返回值是否有效
	*retValid = true

	// We have to make sure that the out slice lives at least until
	// the runtime knows the return values are valid. Otherwise, the
	// return values might not be scanned by anyone during a GC.
	// (out would be dead, and the return slots not yet alive.)
	// 我们必须确保输出切片至少在运行时知道返回值是有效的之前是有效的
	// 否则，在GC期间，返回值可能不会被任何人扫描
	// (out是死的，return槽还没有活 )
	runtime.KeepAlive(out)

	// runtime.getArgInfo expects to be able to find ctxt on the
	// stack when it finds our caller, makeFuncStub. Make sure it
	// doesn't get garbage collected.
	// 运行时 getArgInfo期望在找到调用者makeFuncStub时能够在堆栈中找到ctext
	// 确保它不会被垃圾收集
	runtime.KeepAlive(ctxt)
}

// methodReceiver returns information about the receiver
// described by v. The Value v may or may not have the
// flagMethod bit set, so the kind cached in v.flag should
// not be used.
// methodReceiver返回v描述的接收器的信息 Value v可能设置了也可能没有设置flagMethod位，所以缓存在v.flag中的类型不应该使用
// The return value rcvrtype gives the method's actual receiver type.
// The return value t gives the method type signature (without the receiver).
// The return value fn is a pointer to the method code.
// 返回值rcvrtype给出了方法的实际接收方类型
// 返回值t给出了方法类型签名(没有接收方)
// 返回值fn是一个指向方法代码的指针
func methodReceiver(op string, v Value, methodIndex int) (rcvrtype *rtype, t *funcType, fn unsafe.Pointer) {
	i := methodIndex
	if v.typ.Kind() == Interface {
		tt := (*interfaceType)(unsafe.Pointer(v.typ))
		if uint(i) >= uint(len(tt.methods)) {
			panic("reflect: internal error: invalid method index")
		}
		m := &tt.methods[i]
		if !tt.nameOff(m.name).isExported() {
			panic("reflect: " + op + " of unexported method")
		}
		iface := (*nonEmptyInterface)(v.ptr)
		if iface.itab == nil {
			panic("reflect: " + op + " of method on nil interface value")
		}
		rcvrtype = iface.itab.typ
		fn = unsafe.Pointer(&iface.itab.fun[i])
		t = (*funcType)(unsafe.Pointer(tt.typeOff(m.typ)))
	} else {
		rcvrtype = v.typ
		ms := v.typ.exportedMethods()
		if uint(i) >= uint(len(ms)) {
			panic("reflect: internal error: invalid method index")
		}
		m := ms[i]
		if !v.typ.nameOff(m.name).isExported() {
			panic("reflect: " + op + " of unexported method")
		}
		ifn := v.typ.textOff(m.ifn)
		fn = unsafe.Pointer(&ifn)
		t = (*funcType)(unsafe.Pointer(v.typ.typeOff(m.mtyp)))
	}
	return
}

// v is a method receiver. Store at p the word which is used to
// encode that receiver at the start of the argument list.
// Reflect uses the "interface" calling convention for
// methods, which always uses one word to record the receiver.
// V是一个方法接收器 将用于在参数列表开始处对接收方进行编码的单词存储在p
// Reflect对方法使用“接口”调用约定，它总是使用一个单词来记录接收者
func storeRcvr(v Value, p unsafe.Pointer) {
	t := v.typ
	if t.Kind() == Interface {
		// the interface data word becomes the receiver word
		iface := (*nonEmptyInterface)(v.ptr)
		*(*unsafe.Pointer)(p) = iface.word
	} else if v.flag&flagIndir != 0 && !ifaceIndir(t) {
		*(*unsafe.Pointer)(p) = *(*unsafe.Pointer)(v.ptr)
	} else {
		*(*unsafe.Pointer)(p) = v.ptr
	}
}

// align returns the result of rounding x up to a multiple of n.
// n must be a power of two.
// Align返回x四舍五入到n的倍数的结果，n必须是2的幂
func align(x, n uintptr) uintptr {
	return (x + n - 1) &^ (n - 1)
}

// callMethod is the call implementation used by a function returned
// by makeMethodValue (used by v.Method(i).Interface()).
// It is a streamlined version of the usual reflect call: the caller has
// already laid out the argument frame for us, so we don't have
// to deal with individual Values for each argument.
// It is in this file so that it can be next to the two similar functions above.
// The remainder of the makeMethodValue implementation is in makefunc.go.
// callMethod是由makeMethodValue(由v.Method(i). interface())返回的函数使用的调用实现
// 它是通常反射调用的精简版本:调用者已经为我们设置了参数框架，因此我们不必为每个参数处理单独的Values
// 它在这个文件中，这样它就可以放在上面两个类似的函数旁边
// makeMethodValue实现的其余部分在makefc .go中
//
// NOTE: This function must be marked as a "wrapper" in the generated code,
// so that the linker can make it work correctly for panic and recover.
// The gc compilers know to do that for the name "reflect.callMethod".
// 注意:此函数必须在生成的代码中标记为“包装器”，以便链接器可以使其在恐慌和恢复时正常工作
// gc编译器知道对' reflect.callMethod '这样做
//
// ctxt is the "closure" generated by makeVethodValue.
// frame is a pointer to the arguments to that closure on the stack.
// retValid points to a boolean which should be set when the results
// section of frame is set.
// ctext是makevethdvalue生成的'闭包' Frame是指向堆栈上那个闭包的参数的指针
// retValid指向一个布尔值，该值应该在设置帧的结果部分时设置
//
// regs contains the argument values passed in registers and will contain
// the values returned from ctxt.fn in registers.
// Regs包含在寄存器中传递的参数值，并将包含从ctext返回的值
// fn在寄存器中
func callMethod(ctxt *methodValue, frame unsafe.Pointer, retValid *bool, regs *abi.RegArgs) {
	rcvr := ctxt.rcvr
	rcvrType, valueFuncType, methodFn := methodReceiver("call", rcvr, ctxt.method)

	// There are two ABIs at play here.
	// 这里有两个abi
	//
	// methodValueCall was invoked with the ABI assuming there was no
	// receiver ("value ABI") and that's what frame and regs are holding.
	// methodValueCall被ABI调用，假设没有接收器('值ABI ')，这是帧和规则持有的
	//
	// Meanwhile, we need to actually call the method with a receiver, which
	// has its own ABI ("method ABI"). Everything that follows is a translation
	// between the two.
	// 同时，我们需要实际调用带有接收器的方法，接收器有自己的ABI('方法ABI ')
	// 接下来的一切都是两者之间的转换
	_, _, valueABI := funcLayout(valueFuncType, nil)
	valueFrame, valueRegs := frame, regs
	methodFrameType, methodFramePool, methodABI := funcLayout(valueFuncType, rcvrType)

	// Make a new frame that is one word bigger so we can store the receiver.
	// This space is used for both arguments and return values.
	// 做一个大一个字的新帧，这样我们就可以存储接收器了
	// 此空间用于参数和返回值
	methodFrame := methodFramePool.Get().(unsafe.Pointer)
	var methodRegs abi.RegArgs

	// Deal with the receiver. It's guaranteed to only be one word in size.
	// 处理接收者 它保证只有一个单词的大小
	if st := methodABI.call.steps[0]; st.kind == abiStepStack {
		// Only copy the reciever to the stack if the ABI says so.
		// Otherwise, it'll be in a register already.
		// 只有在ABI允许的情况下才将接收方复制到堆栈中
		// 否则，它就已经在收银机里了
		storeRcvr(rcvr, methodFrame)
	} else {
		// Put the receiver in a register.
		// 把听筒放在收银机里
		storeRcvr(rcvr, unsafe.Pointer(&methodRegs.Ints))
	}

	// Translate the rest of the arguments.
	// 翻译其余的论点
	for i, t := range valueFuncType.in() {
		valueSteps := valueABI.call.stepsForValue(i)
		methodSteps := methodABI.call.stepsForValue(i + 1)

		// Zero-sized types are trivial: nothing to do.
		// 零大小的类型是微不足道的:不需要做任何事情
		if len(valueSteps) == 0 {
			if len(methodSteps) != 0 {
				panic("method ABI and value ABI do not align")
			}
			continue
		}

		// There are four cases to handle in translating each
		// argument:
		// 在翻译每个参数时需要处理四种情况:
		// 1. Stack -> stack translation.
		// 2. Stack -> registers translation.
		// 3. Registers -> stack translation.
		// 4. Registers -> registers translation.
		// TODO(mknyszek): Cases 2 and 3 below only work on little endian
		// architectures. This is OK for now, but this needs to be fixed
		// before supporting the register ABI on big endian architectures.
		// TODO(mknyszek):下面的案例2和3只适用于小端的架构
		// 目前这是可以的，但是需要在大端体系结构上支持寄存器ABI之前解决这个问题

		// If the value ABI passes the value on the stack,
		// then the method ABI does too, because it has strictly
		// fewer arguments. Simply copy between the two.
		// 如果值ABI在堆栈上传递值，那么方法ABI也会这样做，因为它的参数更少
		// 只需在两者之间复制即可
		if vStep := valueSteps[0]; vStep.kind == abiStepStack {
			mStep := methodSteps[0]
			// Handle stack -> stack translation.
			if mStep.kind == abiStepStack {
				if vStep.size != mStep.size {
					panic("method ABI and value ABI do not align")
				}
				typedmemmove(t,
					add(methodFrame, mStep.stkOff, "precomputed stack offset"),
					add(valueFrame, vStep.stkOff, "precomputed stack offset"))
				continue
			}
			// Handle stack -> register translation.
			for _, mStep := range methodSteps {
				from := add(valueFrame, vStep.stkOff+mStep.offset, "precomputed stack offset")
				switch mStep.kind {
				case abiStepPointer:
					// Do the pointer copy directly so we get a write barrier.
					// 直接复制指针，这样就得到一个写屏障
					methodRegs.Ptrs[mStep.ireg] = *(*unsafe.Pointer)(from)
					fallthrough // We need to make sure this ends up in Ints, too.
				case abiStepIntReg:
					memmove(unsafe.Pointer(&methodRegs.Ints[mStep.ireg]), from, mStep.size)
				case abiStepFloatReg:
					memmove(unsafe.Pointer(&methodRegs.Floats[mStep.freg]), from, mStep.size)
				default:
					panic("unexpected method step")
				}
			}
			continue
		}
		// Handle register -> stack translation.
		if mStep := methodSteps[0]; mStep.kind == abiStepStack {
			for _, vStep := range valueSteps {
				to := add(methodFrame, mStep.stkOff+vStep.offset, "precomputed stack offset")
				switch vStep.kind {
				case abiStepPointer:
					// Do the pointer copy directly so we get a write barrier.
					// 直接复制指针，这样就得到一个写屏障
					*(*unsafe.Pointer)(to) = valueRegs.Ptrs[vStep.ireg]
				case abiStepIntReg:
					memmove(to, unsafe.Pointer(&valueRegs.Ints[vStep.ireg]), vStep.size)
				case abiStepFloatReg:
					memmove(to, unsafe.Pointer(&valueRegs.Floats[vStep.freg]), vStep.size)
				default:
					panic("unexpected value step")
				}
			}
			continue
		}
		// Handle register -> register translation.
		if len(valueSteps) != len(methodSteps) {
			// Because it's the same type for the value, and it's assigned
			// to registers both times, it should always take up the same
			// number of registers for each ABI.
			// 因为值的类型是相同的，并且两次都将它分配给寄存器，所以对于每个ABI，它应该总是占用相同数量的寄存器
			panic("method ABI and value ABI don't align")
		}
		for i, vStep := range valueSteps {
			mStep := methodSteps[i]
			if mStep.kind != vStep.kind {
				panic("method ABI and value ABI don't align")
			}
			switch vStep.kind {
			case abiStepPointer:
				// Copy this too, so we get a write barrier.
				// 复制这个，我们得到一个写屏障
				methodRegs.Ptrs[mStep.ireg] = valueRegs.Ptrs[vStep.ireg]
				fallthrough
			case abiStepIntReg:
				methodRegs.Ints[mStep.ireg] = valueRegs.Ints[vStep.ireg]
			case abiStepFloatReg:
				methodRegs.Floats[mStep.freg] = valueRegs.Floats[vStep.freg]
			default:
				panic("unexpected value step")
			}
		}
	}

	methodFrameSize := methodFrameType.size
	// TODO(mknyszek): Remove this when we no longer have
	// caller reserved spill space.
	// TODO(mknyszek):当我们不再有调用者保留溢出空间时，删除这个
	methodFrameSize = align(methodFrameSize, ptrSize)
	methodFrameSize += methodABI.spill

	// Mark pointers in registers for the return path.
	// 在寄存器中标记返回路径的指针
	methodRegs.ReturnIsPtr = methodABI.outRegPtrs

	// Call.
	// Call copies the arguments from scratch to the stack, calls fn,
	// and then copies the results back into scratch.
	// 调用 Call将参数从头复制到堆栈，调用fn，然后将结果复制回从头
	call(methodFrameType, methodFn, methodFrame, uint32(methodFrameType.size), uint32(methodABI.retOffset), uint32(methodFrameSize), &methodRegs)

	// Copy return values.
	// 副本返回值
	//
	// This is somewhat simpler because both ABIs have an identical
	// return value ABI (the types are identical). As a result, register
	// results can simply be copied over. Stack-allocated values are laid
	// out the same, but are at different offsets from the start of the frame
	// Ignore any changes to args.
	// 这稍微简单一些，因为两个ABI都有相同的返回值ABI(类型相同)
	// 因此，可以简单地复制寄存器结果 堆栈分配的值布局相同，但从帧的开始有不同的偏移量
	// Avoid constructing out-of-bounds pointers if there are no return values.
	// because the arguments may be laid out differently.
	// 如果没有返回值，避免构造出界指针 因为论点可能会以不同的方式展开
	if valueRegs != nil {
		*valueRegs = methodRegs
	}
	if retSize := methodFrameType.size - methodABI.retOffset; retSize > 0 {
		valueRet := add(valueFrame, valueABI.retOffset, "valueFrame's size > retOffset")
		methodRet := add(methodFrame, methodABI.retOffset, "methodFrame's size > retOffset")
		// This copies to the stack. Write barriers are not needed.
		// 这将复制到堆栈 写入障碍不需要
		memmove(valueRet, methodRet, retSize)
	}

	// Tell the runtime it can now depend on the return values
	// being properly initialized.
	// 告诉运行时它现在可以依赖于正确初始化的返回值
	*retValid = true

	// Clear the scratch space and put it back in the pool.
	// This must happen after the statement above, so that the return
	// values will always be scanned by someone.
	// 清理干净擦痕处，把它放回泳池 这必须发生在上面的语句之后，以便返回值总是会被某人扫描
	typedmemclr(methodFrameType, methodFrame)
	methodFramePool.Put(methodFrame)

	// See the comment in callReflect.
	// 参见callReflect中的注释
	runtime.KeepAlive(ctxt)

	// Keep valueRegs alive because it may hold live pointer results.
	// The caller (methodValueCall) has it as a stack object, which is only
	// scanned when there is a reference to it.
	// 保持valueRegs为活的，因为它可以保存活的指针结果
	// 调用者(methodValueCall)将它作为一个堆栈对象，只有当有对它的引用时才会扫描它
	runtime.KeepAlive(valueRegs)
}

// funcName returns the name of f, for use in error messages.
// funcName返回f的名称，用于错误消息中
func funcName(f func([]Value) []Value) string {
	pc := *(*uintptr)(unsafe.Pointer(&f))
	rf := runtime.FuncForPC(pc)
	if rf != nil {
		return rf.Name()
	}
	return "closure"
}

// Cap returns v's capacity.
// It panics if v's Kind is not Array, Chan, or Slice.
// Cap返回v的容量 如果v的种类不是Array, Chan或Slice，它就会惊慌
func (v Value) Cap() int {
	k := v.kind()
	switch k {
	case Array:
		return v.typ.Len()
	case Chan:
		return chancap(v.pointer())
	case Slice:
		// Slice is always bigger than a word; assume flagIndir.
		return (*unsafeheader.Slice)(v.ptr).Cap
	}
	panic(&ValueError{"reflect.Value.Cap", v.kind()})
}

// Close closes the channel v.
// It panics if v's Kind is not Chan.
// 关闭v通道，如果v的Kind不是Chan就会恐慌
func (v Value) Close() {
	v.mustBe(Chan)
	v.mustBeExported()
	chanclose(v.pointer())
}

// Complex returns v's underlying value, as a complex128.
// It panics if v's Kind is not Complex64 or Complex128
// Complex返回v的基础值，作为complex128 如果v的Kind不是Complex64或Complex128，它就会惊慌
func (v Value) Complex() complex128 {
	k := v.kind()
	switch k {
	case Complex64:
		return complex128(*(*complex64)(v.ptr))
	case Complex128:
		return *(*complex128)(v.ptr)
	}
	panic(&ValueError{"reflect.Value.Complex", v.kind()})
}

// Elem returns the value that the interface v contains
// or that the pointer v points to.
// It panics if v's Kind is not Interface or Ptr.
// It returns the zero Value if v is nil.
// Elem返回接口v所包含的值或v所指向的指针的值
// 如果v的种类不是界面或Ptr，它会惊慌 如果v为nil，则返回零值
func (v Value) Elem() Value {
	k := v.kind()
	switch k {
	case Interface:
		var eface interface{}
		if v.typ.NumMethod() == 0 {
			eface = *(*interface{})(v.ptr)
		} else {
			eface = (interface{})(*(*interface {
				M()
			})(v.ptr))
		}
		x := unpackEface(eface)
		if x.flag != 0 {
			x.flag |= v.flag.ro()
		}
		return x
	case Ptr:
		ptr := v.ptr
		if v.flag&flagIndir != 0 {
			ptr = *(*unsafe.Pointer)(ptr)
		}
		// The returned value's address is v's value.
		if ptr == nil {
			return Value{}
		}
		tt := (*ptrType)(unsafe.Pointer(v.typ))
		typ := tt.elem
		fl := v.flag&flagRO | flagIndir | flagAddr
		fl |= flag(typ.Kind())
		return Value{typ, ptr, fl}
	}
	panic(&ValueError{"reflect.Value.Elem", v.kind()})
}

// Field returns the i'th field of the struct v.
// It panics if v's Kind is not Struct or i is out of range.
// Field返回结构体v的第i个字段 如果v的Kind不是结构体，或者i超出了范围，则会产生恐慌
func (v Value) Field(i int) Value {
	if v.kind() != Struct {
		panic(&ValueError{"reflect.Value.Field", v.kind()})
	}
	tt := (*structType)(unsafe.Pointer(v.typ))
	if uint(i) >= uint(len(tt.fields)) {
		panic("reflect: Field index out of range")
	}
	field := &tt.fields[i]
	typ := field.typ

	// Inherit permission bits from v, but clear flagEmbedRO.
	// 从v继承权限位，但清除flagemdro
	fl := v.flag&(flagStickyRO|flagIndir|flagAddr) | flag(typ.Kind())
	// Using an unexported field forces flagRO.
	// 使用未导出的野战部队标志
	if !field.name.isExported() {
		if field.embedded() {
			fl |= flagEmbedRO
		} else {
			fl |= flagStickyRO
		}
	}
	// Either flagIndir is set and v.ptr points at struct,
	// or flagIndir is not set and v.ptr is the actual struct data.
	// In the former case, we want v.ptr + offset.
	// In the latter case, we must have field.offset = 0,
	// so v.ptr + field.offset is still the correct address.
	// 要么设置了flagIndir且v.ptr指向struct，要么未设置flagIndir且v.ptr是实际的struct数据
	// 在前一种情况下，我们需要v.ptr +偏移量
	// 在后一种情况下，我们必须有field Offset = 0，所以v.ptr + field
	// 偏移量仍然是正确的地址
	ptr := add(v.ptr, field.offset(), "same as non-reflect &v.field")
	return Value{typ, ptr, fl}
}

// FieldByIndex returns the nested field corresponding to index.
// It panics if v's Kind is not struct.
// FieldByIndex返回对应于index的嵌套字段 如果v星人不正常，它就会惊慌
func (v Value) FieldByIndex(index []int) Value {
	if len(index) == 1 {
		return v.Field(index[0])
	}
	v.mustBe(Struct)
	for i, x := range index {
		if i > 0 {
			if v.Kind() == Ptr && v.typ.Elem().Kind() == Struct {
				if v.IsNil() {
					panic("reflect: indirection through nil pointer to embedded struct")
				}
				v = v.Elem()
			}
		}
		v = v.Field(x)
	}
	return v
}

// FieldByName returns the struct field with the given name.
// It returns the zero Value if no field was found.
// It panics if v's Kind is not struct.
// FieldByName返回带有给定名称的结构字段 如果没有找到字段，则返回零值
// 如果v星人不正常，它就会惊慌
func (v Value) FieldByName(name string) Value {
	v.mustBe(Struct)
	if f, ok := v.typ.FieldByName(name); ok {
		return v.FieldByIndex(f.Index)
	}
	return Value{}
}

// FieldByNameFunc returns the struct field with a name
// that satisfies the match function.
// It panics if v's Kind is not struct.
// It returns the zero Value if no field was found.
// FieldByNameFunc返回一个名称满足匹配函数的结构字段
// 如果v星人不正常，它就会惊慌 如果没有找到字段，则返回零值
func (v Value) FieldByNameFunc(match func(string) bool) Value {
	if f, ok := v.typ.FieldByNameFunc(match); ok {
		return v.FieldByIndex(f.Index)
	}
	return Value{}
}

// Float returns v's underlying value, as a float64.
// It panics if v's Kind is not Float32 or Float64
// Float返回v的基础值，为float64 如果v的种类不是floati32或floati64，它会惊慌
func (v Value) Float() float64 {
	k := v.kind()
	switch k {
	case Float32:
		return float64(*(*float32)(v.ptr))
	case Float64:
		return *(*float64)(v.ptr)
	}
	panic(&ValueError{"reflect.Value.Float", v.kind()})
}

var uint8Type = TypeOf(uint8(0)).(*rtype)

// Index returns v's i'th element.
// It panics if v's Kind is not Array, Slice, or String or i is out of range.
// Index返回v的第i个元素 如果v的类型不是数组、切片或字符串，或者i超出了范围，它会惊慌
func (v Value) Index(i int) Value {
	switch v.kind() {
	case Array:
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		if uint(i) >= uint(tt.len) {
			panic("reflect: array index out of range")
		}
		typ := tt.elem
		offset := uintptr(i) * typ.size

		// Either flagIndir is set and v.ptr points at array,
		// or flagIndir is not set and v.ptr is the actual array data.
		// In the former case, we want v.ptr + offset.
		// In the latter case, we must be doing Index(0), so offset = 0,
		// so v.ptr + offset is still the correct address.
		// 要么设置了flagIndir并且v.ptr指向数组，要么没有设置flagIndir并且v.ptr是实际的数组数据
		// 在前一种情况下，我们需要v.ptr +偏移量
		// 在后一种情况下，我们必须使用Index(0)，所以offset = 0，所以v.p trr + offset仍然是正确的地址
		val := add(v.ptr, offset, "same as &v[i], i < tt.len")
		fl := v.flag&(flagIndir|flagAddr) | v.flag.ro() | flag(typ.Kind()) // bits same as overall array
		return Value{typ, val, fl}

	case Slice:
		// Element flag same as Elem of Ptr.
		// Addressable, indirect, possibly read-only.
		// 元素标记与Ptr的Elem相同 可寻址，间接，可能是只读的
		s := (*unsafeheader.Slice)(v.ptr)
		if uint(i) >= uint(s.Len) {
			panic("reflect: slice index out of range")
		}
		tt := (*sliceType)(unsafe.Pointer(v.typ))
		typ := tt.elem
		val := arrayAt(s.Data, i, typ.size, "i < s.Len")
		fl := flagAddr | flagIndir | v.flag.ro() | flag(typ.Kind())
		return Value{typ, val, fl}

	case String:
		s := (*unsafeheader.String)(v.ptr)
		if uint(i) >= uint(s.Len) {
			panic("reflect: string index out of range")
		}
		p := arrayAt(s.Data, i, 1, "i < s.Len")
		fl := v.flag.ro() | flag(Uint8) | flagIndir
		return Value{uint8Type, p, fl}
	}
	panic(&ValueError{"reflect.Value.Index", v.kind()})
}

// Int returns v's underlying value, as an int64.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64.
// Int返回v的基础值，作为int64 如果v的Kind不是Int、Int8、Int16、Int32或Int64，则会惊慌
func (v Value) Int() int64 {
	k := v.kind()
	p := v.ptr
	switch k {
	case Int:
		return int64(*(*int)(p))
	case Int8:
		return int64(*(*int8)(p))
	case Int16:
		return int64(*(*int16)(p))
	case Int32:
		return int64(*(*int32)(p))
	case Int64:
		return *(*int64)(p)
	}
	panic(&ValueError{"reflect.Value.Int", v.kind()})
}

// CanInterface reports whether Interface can be used without panicking.
// CanInterface报告Interface是否可以正常使用
func (v Value) CanInterface() bool {
	if v.flag == 0 {
		panic(&ValueError{"reflect.Value.CanInterface", Invalid})
	}
	return v.flag&flagRO == 0
}

// Interface returns v's current value as an interface{}.
// It is equivalent to:
// 返回v的当前值作为一个接口{} 它等价于:
//	var i interface{} = (v's underlying value)
// It panics if the Value was obtained by accessing
// unexported struct fields.
// 如果Value是通过访问未导出的struct字段获得的，它会惊慌
func (v Value) Interface() (i interface{}) {
	return valueInterface(v, true)
}

func valueInterface(v Value, safe bool) interface{} {
	if v.flag == 0 {
		panic(&ValueError{"reflect.Value.Interface", Invalid})
	}
	if safe && v.flag&flagRO != 0 {
		// Do not allow access to unexported values via Interface,
		// because they might be pointers that should not be
		// writable or methods or function that should not be callable.
		// 不允许通过Interface访问未导出的值，因为它们可能是不应可写的指针或不应可调用的方法或函数
		panic("reflect.Value.Interface: cannot return value obtained from unexported field or method")
	}
	if v.flag&flagMethod != 0 {
		v = makeMethodValue("Interface", v)
	}

	if v.kind() == Interface {
		// Special case: return the element inside the interface.
		// Empty interface has one layout, all interfaces with
		// methods have a second layout.
		// 特殊情况:返回接口内部的元素 空接口有一个布局，所有带有方法的接口都有第二个布局
		if v.NumMethod() == 0 {
			return *(*interface{})(v.ptr)
		}
		return *(*interface {
			M()
		})(v.ptr)
	}

	// TODO: pass safe to packEface so we don't need to copy if safe==true?
	// TODO: pass safe to packkeface所以我们不需要复制如果safe==true?
	return packEface(v)
}

// InterfaceData returns a pair of unspecified uintptr values.
// It panics if v's Kind is not Interface.
// 接口数据返回一对未指定的uintptr值 如果v族不是界面族，它就会惊慌
//
// In earlier versions of Go, this function returned the interface's
// value as a uintptr pair. As of Go 1.4, the implementation of
// interface values precludes any defined use of InterfaceData.
// 在Go的早期版本中，该函数以uintptr对的形式返回接口的值
// 从Go 1.4开始，接口值的实现排除了InterfaceData的任何定义使用
//
// Deprecated: The memory representation of interface values is not
// compatible with InterfaceData.
// 弃用:接口值的内存表示与InterfaceData不兼容
func (v Value) InterfaceData() [2]uintptr {
	v.mustBe(Interface)
	// We treat this as a read operation, so we allow
	// it even for unexported data, because the caller
	// has to import "unsafe" to turn it into something
	// that can be abused.
	// Interface value is always bigger than a word; assume flagIndir.
	// 我们将其视为一个读取操作，因此即使是未导出的数据也允许使用它，因为调用方必须导入“不安全”的数据，将其转换为可能被滥用的数据
	// 接口值总是大于一个单词，假设为flagIndir
	return *(*[2]uintptr)(v.ptr)
}

// IsNil reports whether its argument v is nil. The argument must be
// a chan, func, interface, map, pointer, or slice value; if it is
// not, IsNil panics. Note that IsNil is not always equivalent to a
// regular comparison with nil in Go. For example, if v was created
// by calling ValueOf with an uninitialized interface variable i,
// i==nil will be true but v.IsNil will panic as v will be the zero
// Value.
// IsNil报告参数v是否为nil 参数必须是chan, func, interface, map, pointer，或者slice值，否则IsNil会惊慌
// 注意，IsNil并不总是等同于在Go中与nil的常规比较
// 例如，如果v是通过调用ValueOf和一个未初始化的接口变量i创建的，i==nil将为真，但v. isnil将因为v将是0值而恐慌
func (v Value) IsNil() bool {
	k := v.kind()
	switch k {
	case Chan, Func, Map, Ptr, UnsafePointer:
		if v.flag&flagMethod != 0 {
			return false
		}
		ptr := v.ptr
		if v.flag&flagIndir != 0 {
			ptr = *(*unsafe.Pointer)(ptr)
		}
		return ptr == nil
	case Interface, Slice:
		// Both interface and slice are nil if first word is 0.
		// Both are always bigger than a word; assume flagIndir.
		// 如果第一个单词为0，则interface和slice都为nil
		// 两者都比一个词大假定flagIndir
		return *(*unsafe.Pointer)(v.ptr) == nil
	}
	panic(&ValueError{"reflect.Value.IsNil", v.kind()})
}

// IsValid reports whether v represents a value.
// It returns false if v is the zero Value.
// If IsValid returns false, all other methods except String panic.
// Most functions and methods never return an invalid Value.
// If one does, its documentation states the conditions explicitly.
// IsValid报告v是否代表一个值 如果v是零值，则返回false
// 如果IsValid返回false，则除String外的所有其他方法都为false
// 大多数函数和方法从不返回无效值 如果有，它的文档会显式地说明条件
func (v Value) IsValid() bool {
	return v.flag != 0
}

// IsZero reports whether v is the zero value for its type.
// It panics if the argument is invalid.
// IsZero报告v是否为其类型的0值 如果论点无效，它会惊慌失措
func (v Value) IsZero() bool {
	switch v.kind() {
	case Bool:
		return !v.Bool()
	case Int, Int8, Int16, Int32, Int64:
		return v.Int() == 0
	case Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
		return v.Uint() == 0
	case Float32, Float64:
		return math.Float64bits(v.Float()) == 0
	case Complex64, Complex128:
		c := v.Complex()
		return math.Float64bits(real(c)) == 0 && math.Float64bits(imag(c)) == 0
	case Array:
		for i := 0; i < v.Len(); i++ {
			if !v.Index(i).IsZero() {
				return false
			}
		}
		return true
	case Chan, Func, Interface, Map, Ptr, Slice, UnsafePointer:
		return v.IsNil()
	case String:
		return v.Len() == 0
	case Struct:
		for i := 0; i < v.NumField(); i++ {
			if !v.Field(i).IsZero() {
				return false
			}
		}
		return true
	default:
		// This should never happens, but will act as a safeguard for
		// later, as a default value doesn't makes sense here.
		// 这种情况永远不应该发生，但是可以作为以后的安全措施，因为默认值在这里没有意义
		panic(&ValueError{"reflect.Value.IsZero", v.Kind()})
	}
}

// Kind returns v's Kind.
// If v is the zero Value (IsValid returns false), Kind returns Invalid.
// Kind返回v的Kind 如果v是零值(IsValid返回false)， Kind返回Invalid
func (v Value) Kind() Kind {
	return v.kind()
}

// Len returns v's length.
// It panics if v's Kind is not Array, Chan, Map, Slice, or String.
// Len返回v的长度 如果v的Kind不是Array, Chan, Map, Slice或String，它会惊慌
func (v Value) Len() int {
	k := v.kind()
	switch k {
	case Array:
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		return int(tt.len)
	case Chan:
		return chanlen(v.pointer())
	case Map:
		return maplen(v.pointer())
	case Slice:
		// Slice is bigger than a word; assume flagIndir.
		// Slice比一个字大假定为flagIndir
		return (*unsafeheader.Slice)(v.ptr).Len
	case String:
		// String is bigger than a word; assume flagIndir.
		// 字符串比一个单词大，假设为flagIndir
		return (*unsafeheader.String)(v.ptr).Len
	}
	panic(&ValueError{"reflect.Value.Len", v.kind()})
}

// MapIndex returns the value associated with key in the map v.
// It panics if v's Kind is not Map.
// It returns the zero Value if key is not found in the map or if v represents a nil map.
// As in Go, the key's value must be assignable to the map's key type.
// MapIndex返回map v中与key相关联的值，如果v的Kind不是map则会惊慌
// 如果key没有在map中找到，或者v表示空map，则返回零值
// 在Go中，键的值必须可赋值给映射的键类型
func (v Value) MapIndex(key Value) Value {
	v.mustBe(Map)
	tt := (*mapType)(unsafe.Pointer(v.typ))

	// Do not require key to be exported, so that DeepEqual
	// and other programs can use all the keys returned by
	// MapKeys as arguments to MapIndex. If either the map
	// or the key is unexported, though, the result will be
	// considered unexported. This is consistent with the
	// behavior for structs, which allow read but not write
	// of unexported fields.
	// 不需要导出键，以便DeepEqual和其他程序可以使用MapKeys返回的所有键作为MapIndex的参数
	// 如果映射或键没有导出，那么结果将被认为是未导出的
	// 这与struct的行为是一致的，struct允许读取但不允许写入未导出的字段
	key = key.assignTo("reflect.Value.MapIndex", tt.key, nil)

	var k unsafe.Pointer
	if key.flag&flagIndir != 0 {
		k = key.ptr
	} else {
		k = unsafe.Pointer(&key.ptr)
	}
	e := mapaccess(v.typ, v.pointer(), k)
	if e == nil {
		return Value{}
	}
	typ := tt.elem
	fl := (v.flag | key.flag).ro()
	fl |= flag(typ.Kind())
	return copyVal(typ, fl, e)
}

// MapKeys returns a slice containing all the keys present in the map,
// in unspecified order.
// It panics if v's Kind is not Map.
// It returns an empty slice if v represents a nil map.
// MapKeys返回一个包含映射中所有键的切片，但没有指定顺序
// 如果v族不是地图，它就会惊慌 如果v表示一个空映射，则返回一个空切片
func (v Value) MapKeys() []Value {
	v.mustBe(Map)
	tt := (*mapType)(unsafe.Pointer(v.typ))
	keyType := tt.key

	fl := v.flag.ro() | flag(keyType.Kind())

	m := v.pointer()
	mlen := int(0)
	if m != nil {
		mlen = maplen(m)
	}
	it := mapiterinit(v.typ, m)
	a := make([]Value, mlen)
	var i int
	for i = 0; i < len(a); i++ {
		key := mapiterkey(it)
		if key == nil {
			// Someone deleted an entry from the map since we
			// called maplen above. It's a data race, but nothing
			// we can do about it.
			// 有人从地图上删除了一个条目，因为我们在上面调用了枫树
			// 这是一场数据竞赛，但我们无能为力
			break
		}
		a[i] = copyVal(keyType, fl, key)
		mapiternext(it)
	}
	return a[:i]
}

// A MapIter is an iterator for ranging over a map.
// See Value.MapRange.
// MapIter是一个迭代器，用于在map上进行范围搜索
// 看到Value.MapRange
type MapIter struct {
	m  Value
	it unsafe.Pointer
}

// Key returns the key of the iterator's current map entry.
// 返回迭代器当前map项的键
func (it *MapIter) Key() Value {
	if it.it == nil {
		panic("MapIter.Key called before Next")
	}
	if mapiterkey(it.it) == nil {
		panic("MapIter.Key called on exhausted iterator")
	}

	t := (*mapType)(unsafe.Pointer(it.m.typ))
	ktype := t.key
	return copyVal(ktype, it.m.flag.ro()|flag(ktype.Kind()), mapiterkey(it.it))
}

// Value returns the value of the iterator's current map entry.
// Value返回迭代器当前map项的值
func (it *MapIter) Value() Value {
	if it.it == nil {
		panic("MapIter.Value called before Next")
	}
	if mapiterkey(it.it) == nil {
		panic("MapIter.Value called on exhausted iterator")
	}

	t := (*mapType)(unsafe.Pointer(it.m.typ))
	vtype := t.elem
	return copyVal(vtype, it.m.flag.ro()|flag(vtype.Kind()), mapiterelem(it.it))
}

// Next advances the map iterator and reports whether there is another
// entry. It returns false when the iterator is exhausted; subsequent
// calls to Key, Value, or Next will panic.
// 接下来推进map迭代器并报告是否有另一个条目
// 当迭代器耗尽时，将返回false，随后对Key、Value或Next的调用将出现混乱
func (it *MapIter) Next() bool {
	if it.it == nil {
		it.it = mapiterinit(it.m.typ, it.m.pointer())
	} else {
		if mapiterkey(it.it) == nil {
			panic("MapIter.Next called on exhausted iterator")
		}
		mapiternext(it.it)
	}
	return mapiterkey(it.it) != nil
}

// MapRange returns a range iterator for a map.
// It panics if v's Kind is not Map.
// MapRange返回一个映射的范围迭代器 如果v族不是地图，它就会惊慌
//
// Call Next to advance the iterator, and Key/Value to access each entry.
// Next returns false when the iterator is exhausted.
// MapRange follows the same iteration semantics as a range statement.
// 调用Next来推进迭代器，并调用Key/Value来访问每个条目
// Next在迭代器耗尽时返回false MapRange遵循与range语句相同的迭代语义
//
// Example:
//
//	iter := reflect.ValueOf(m).MapRange()
// 	for iter.Next() {
//		k := iter.Key()
//		v := iter.Value()
//		...
//	}
//
func (v Value) MapRange() *MapIter {
	v.mustBe(Map)
	return &MapIter{m: v}
}

// copyVal returns a Value containing the map key or value at ptr,
// allocating a new variable as needed.
// copyVal返回一个包含映射键或ptr值的值，根据需要分配一个新变量
func copyVal(typ *rtype, fl flag, ptr unsafe.Pointer) Value {
	if ifaceIndir(typ) {
		// Copy result so future changes to the map
		// won't change the underlying value.
		// 复制结果，以便以后对映射的更改不会更改基础值
		c := unsafe_New(typ)
		typedmemmove(typ, c, ptr)
		return Value{typ, c, fl | flagIndir}
	}
	return Value{typ, *(*unsafe.Pointer)(ptr), fl}
}

// Method returns a function value corresponding to v's i'th method.
// The arguments to a Call on the returned function should not include
// a receiver; the returned function will always use v as the receiver.
// Method panics if i is out of range or if v is a nil interface value.
// 方法返回对应于v的第i个方法的函数值 调用返回函数的参数不应该包含接收方，返回函数将始终使用v作为接收方
// 如果i超出了范围，或者v为nil接口值，则方法会惊慌
func (v Value) Method(i int) Value {
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.Method", Invalid})
	}
	if v.flag&flagMethod != 0 || uint(i) >= uint(v.typ.NumMethod()) {
		panic("reflect: Method index out of range")
	}
	if v.typ.Kind() == Interface && v.IsNil() {
		panic("reflect: Method on nil interface value")
	}
	fl := v.flag.ro() | (v.flag & flagIndir)
	fl |= flag(Func)
	fl |= flag(i)<<flagMethodShift | flagMethod
	return Value{v.typ, v.ptr, fl}
}

// NumMethod returns the number of exported methods in the value's method set.
// NumMethod返回在值的方法集中导出的方法的数量
func (v Value) NumMethod() int {
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.NumMethod", Invalid})
	}
	if v.flag&flagMethod != 0 {
		return 0
	}
	return v.typ.NumMethod()
}

// MethodByName returns a function value corresponding to the method
// of v with the given name.
// The arguments to a Call on the returned function should not include
// a receiver; the returned function will always use v as the receiver.
// It returns the zero Value if no method was found.
// MethodByName返回一个函数值，对应于v中给定名称的方法
// 调用返回函数的参数不应该包含接收方，返回函数将始终使用v作为接收方
// 如果没有找到方法，则返回零值
func (v Value) MethodByName(name string) Value {
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.MethodByName", Invalid})
	}
	if v.flag&flagMethod != 0 {
		return Value{}
	}
	m, ok := v.typ.MethodByName(name)
	if !ok {
		return Value{}
	}
	return v.Method(m.Index)
}

// NumField returns the number of fields in the struct v.
// It panics if v's Kind is not Struct.
// NumField返回结构v中字段的数量，如果v的Kind不是struct则会惊慌
func (v Value) NumField() int {
	v.mustBe(Struct)
	tt := (*structType)(unsafe.Pointer(v.typ))
	return len(tt.fields)
}

// OverflowComplex reports whether the complex128 x cannot be represented by v's type.
// It panics if v's Kind is not Complex64 or Complex128.
// OverflowComplex报告complex128x是否不能用v的类型表示
// 如果v的Kind不是Complex64或Complex128，它就会惊慌
func (v Value) OverflowComplex(x complex128) bool {
	k := v.kind()
	switch k {
	case Complex64:
		return overflowFloat32(real(x)) || overflowFloat32(imag(x))
	case Complex128:
		return false
	}
	panic(&ValueError{"reflect.Value.OverflowComplex", v.kind()})
}

// OverflowFloat reports whether the float64 x cannot be represented by v's type.
// It panics if v's Kind is not Float32 or Float64.
// OverflowFloat报告float64x是否不能用v的类型表示
// 如果v的种类不是floati32或floati64，它会惊慌
func (v Value) OverflowFloat(x float64) bool {
	k := v.kind()
	switch k {
	case Float32:
		return overflowFloat32(x)
	case Float64:
		return false
	}
	panic(&ValueError{"reflect.Value.OverflowFloat", v.kind()})
}

func overflowFloat32(x float64) bool {
	if x < 0 {
		x = -x
	}
	return math.MaxFloat32 < x && x <= math.MaxFloat64
}

// OverflowInt reports whether the int64 x cannot be represented by v's type.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64.
// OverflowInt报告int64 x是否不能用v的类型表示
// 如果v的Kind不是Int、Int8、Int16、Int32或Int64，则会惊慌
func (v Value) OverflowInt(x int64) bool {
	k := v.kind()
	switch k {
	case Int, Int8, Int16, Int32, Int64:
		bitSize := v.typ.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic(&ValueError{"reflect.Value.OverflowInt", v.kind()})
}

// OverflowUint reports whether the uint64 x cannot be represented by v's type.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64.
// OverflowUint报告uint64 x是否不能用v的类型表示
// 如果v的Kind不是Uint, Uintptr, Uint8, Uint16, Uint32或Uint64则会触发
func (v Value) OverflowUint(x uint64) bool {
	k := v.kind()
	switch k {
	case Uint, Uintptr, Uint8, Uint16, Uint32, Uint64:
		bitSize := v.typ.size * 8
		trunc := (x << (64 - bitSize)) >> (64 - bitSize)
		return x != trunc
	}
	panic(&ValueError{"reflect.Value.OverflowUint", v.kind()})
}

//go:nocheckptr
// This prevents inlining Value.Pointer when -d=checkptr is enabled,
// which ensures cmd/compile can recognize unsafe.Pointer(v.Pointer())
// and make an exception.
// 这将防止内联Value 启用-d=checkptr时的指针，这确保cmd/compile可以识别unsafe.Pointer(v.m Pointer())并产生异常

// Pointer returns v's value as a uintptr.
// It returns uintptr instead of unsafe.Pointer so that
// code using reflect cannot obtain unsafe.Pointers
// without importing the unsafe package explicitly.
// It panics if v's Kind is not Chan, Func, Map, Ptr, Slice, or UnsafePointer.
// 指针返回v的值作为uintptr 它返回uintptr而不是unsafe
// 指针，使使用反射的代码无法获得不安全的
// 指针，而不显式导入不安全包 如果v的种类不是Chan, Func, Map, Ptr, Slice或UnsafePointer，它会恐慌
//
// If v's Kind is Func, the returned pointer is an underlying
// code pointer, but not necessarily enough to identify a
// single function uniquely. The only guarantee is that the
// result is zero if and only if v is a nil func Value.
// 如果v的Kind是Func，返回的指针是一个底层代码指针，但不一定足以唯一地标识一个函数
// 唯一的保证是，当且仅当v为nil func值时，结果为零
//
// If v's Kind is Slice, the returned pointer is to the first
// element of the slice. If the slice is nil the returned value
// is 0.  If the slice is empty but non-nil the return value is non-zero.
// 如果v的类型是Slice，则返回的指针指向Slice的第一个元素
// 如果切片为nil，返回值为0 如果切片为空但非nil，返回值是非零的
func (v Value) Pointer() uintptr {
	// TODO: deprecate
	k := v.kind()
	switch k {
	case Ptr:
		if v.typ.ptrdata == 0 {
			// Handle pointers to go:notinheap types directly,
			// so we never materialize such pointers as an
			// unsafe.Pointer. (Such pointers are always indirect.)
			// 直接处理指向:notinheap类型的指针，因此从不将此类指针物化为unsafe.Pointer
			// (这样的指针总是间接的 )
			// See issue 42076.
			return *(*uintptr)(v.ptr)
		}
		fallthrough
	case Chan, Map, UnsafePointer:
		return uintptr(v.pointer())
	case Func:
		if v.flag&flagMethod != 0 {
			// As the doc comment says, the returned pointer is an
			// underlying code pointer but not necessarily enough to
			// identify a single function uniquely. All method expressions
			// created via reflect have the same underlying code pointer,
			// so their Pointers are equal. The function used here must
			// match the one used in makeMethodValue.
			// 正如文档注释所说，返回的指针是一个底层代码指针，但不一定足以惟一地标识一个函数
			// 所有通过反射创建的方法表达式都有相同的底层代码指针，因此它们的指针是相等的
			// 这里使用的函数必须与makeMethodValue中使用的函数匹配
			f := methodValueCall
			return **(**uintptr)(unsafe.Pointer(&f))
		}
		p := v.pointer()
		// Non-nil func value points at data block.
		// First word of data block is actual code.
		// 非空func值点在数据块 数据块的第一个字是实际代码
		if p != nil {
			p = *(*unsafe.Pointer)(p)
		}
		return uintptr(p)

	case Slice:
		return (*SliceHeader)(v.ptr).Data
	}
	panic(&ValueError{"reflect.Value.Pointer", v.kind()})
}

// Recv receives and returns a value from the channel v.
// It panics if v's Kind is not Chan.
// The receive blocks until a value is ready.
// The boolean value ok is true if the value x corresponds to a send
// on the channel, false if it is a zero value received because the channel is closed.
// Recv从通道v接收并返回一个值，如果v的Kind不是Chan就会惊慌
// 接收块直到一个值准备好 如果x值对应于通道上的发送，则布尔值ok为真，如果通道关闭而接收到的值为零则为假
func (v Value) Recv() (x Value, ok bool) {
	v.mustBe(Chan)
	v.mustBeExported()
	return v.recv(false)
}

// internal recv, possibly non-blocking (nb).
// v is known to be a channel.
// 内部recv，可能是非阻塞的(nb) 已知V是一个通道
func (v Value) recv(nb bool) (val Value, ok bool) {
	tt := (*chanType)(unsafe.Pointer(v.typ))
	if ChanDir(tt.dir)&RecvDir == 0 {
		panic("reflect: recv on send-only channel")
	}
	t := tt.elem
	val = Value{t, nil, flag(t.Kind())}
	var p unsafe.Pointer
	if ifaceIndir(t) {
		p = unsafe_New(t)
		val.ptr = p
		val.flag |= flagIndir
	} else {
		p = unsafe.Pointer(&val.ptr)
	}
	selected, ok := chanrecv(v.pointer(), nb, p)
	if !selected {
		val = Value{}
	}
	return
}

// Send sends x on the channel v.
// It panics if v's kind is not Chan or if x's type is not the same type as v's element type.
// As in Go, x's value must be assignable to the channel's element type.
// Send在通道v上发送x，如果v的类型不是Chan，或者x的类型与v的元素类型不相同，它就会惊慌
// 在Go中，x的值必须可赋值给通道的元素类型
func (v Value) Send(x Value) {
	v.mustBe(Chan)
	v.mustBeExported()
	v.send(x, false)
}

// internal send, possibly non-blocking.
// v is known to be a channel.
// 内部发送，可能是非阻塞的 已知V是一个通道
func (v Value) send(x Value, nb bool) (selected bool) {
	tt := (*chanType)(unsafe.Pointer(v.typ))
	if ChanDir(tt.dir)&SendDir == 0 {
		panic("reflect: send on recv-only channel")
	}
	x.mustBeExported()
	x = x.assignTo("reflect.Value.Send", tt.elem, nil)
	var p unsafe.Pointer
	if x.flag&flagIndir != 0 {
		p = x.ptr
	} else {
		p = unsafe.Pointer(&x.ptr)
	}
	return chansend(v.pointer(), p, nb)
}

// Set assigns x to the value v.
// It panics if CanSet returns false.
// As in Go, x's value must be assignable to v's type.
// Set将x赋值为v，如果CanSet返回false则会惊慌
// 在Go中，x的值必须可以赋值给v的类型
func (v Value) Set(x Value) {
	v.mustBeAssignable()
	x.mustBeExported() // do not let unexported x leak
	var target unsafe.Pointer
	if v.kind() == Interface {
		target = v.ptr
	}
	x = x.assignTo("reflect.Set", v.typ, target)
	if x.flag&flagIndir != 0 {
		if x.ptr == unsafe.Pointer(&zeroVal[0]) {
			typedmemclr(v.typ, v.ptr)
		} else {
			typedmemmove(v.typ, v.ptr, x.ptr)
		}
	} else {
		*(*unsafe.Pointer)(v.ptr) = x.ptr
	}
}

// SetBool sets v's underlying value.
// It panics if v's Kind is not Bool or if CanSet() is false.
// SetBool设置v的基础值 如果v的Kind不是Bool或CanSet()为false，则会触发恐慌
func (v Value) SetBool(x bool) {
	v.mustBeAssignable()
	v.mustBe(Bool)
	*(*bool)(v.ptr) = x
}

// SetBytes sets v's underlying value.
// It panics if v's underlying value is not a slice of bytes.
// SetBytes设置v的基础值 如果v的底层值不是一个字节片，它就会惊慌
func (v Value) SetBytes(x []byte) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Uint8 {
		panic("reflect.Value.SetBytes of non-byte slice")
	}
	*(*[]byte)(v.ptr) = x
}

// setRunes sets v's underlying value.
// It panics if v's underlying value is not a slice of runes (int32s).
// setRunes设置v的底层值 如果v的基础值不是一块符文(32秒)，它会惊慌
func (v Value) setRunes(x []rune) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	if v.typ.Elem().Kind() != Int32 {
		panic("reflect.Value.setRunes of non-rune slice")
	}
	*(*[]rune)(v.ptr) = x
}

// SetComplex sets v's underlying value to x.
// It panics if v's Kind is not Complex64 or Complex128, or if CanSet() is false.
// SetComplex将v的基础值设置为x 如果v的Kind不是Complex64或Complex128，或者CanSet()为false，则会触发恐慌
func (v Value) SetComplex(x complex128) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetComplex", v.kind()})
	case Complex64:
		*(*complex64)(v.ptr) = complex64(x)
	case Complex128:
		*(*complex128)(v.ptr) = x
	}
}

// SetFloat sets v's underlying value to x.
// It panics if v's Kind is not Float32 or Float64, or if CanSet() is false.
// SetFloat将v的基础值设置为x 如果v的Kind不是Float32或Float64，或者CanSet()为false，则会触发恐慌
func (v Value) SetFloat(x float64) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetFloat", v.kind()})
	case Float32:
		*(*float32)(v.ptr) = float32(x)
	case Float64:
		*(*float64)(v.ptr) = x
	}
}

// SetInt sets v's underlying value to x.
// It panics if v's Kind is not Int, Int8, Int16, Int32, or Int64, or if CanSet() is false.
// SetInt将v的基础值设置为x 如果v的Kind不是Int、Int8、Int16、Int32或Int64，或者CanSet()为false，则会触发恐慌
func (v Value) SetInt(x int64) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetInt", v.kind()})
	case Int:
		*(*int)(v.ptr) = int(x)
	case Int8:
		*(*int8)(v.ptr) = int8(x)
	case Int16:
		*(*int16)(v.ptr) = int16(x)
	case Int32:
		*(*int32)(v.ptr) = int32(x)
	case Int64:
		*(*int64)(v.ptr) = x
	}
}

// SetLen sets v's length to n.
// It panics if v's Kind is not Slice or if n is negative or
// greater than the capacity of the slice.
// SetLen将v的长度设置为n 如果v的Kind不是Slice，或者n是负的或大于Slice的容量，它会惊慌
func (v Value) SetLen(n int) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	s := (*unsafeheader.Slice)(v.ptr)
	if uint(n) > uint(s.Cap) {
		panic("reflect: slice length out of range in SetLen")
	}
	s.Len = n
}

// SetCap sets v's capacity to n.
// It panics if v's Kind is not Slice or if n is smaller than the length or
// greater than the capacity of the slice.
// SetCap将v的容量设置为n 如果v的Kind不是Slice，或者n小于Slice的长度或大于Slice的容量，它就会惊慌
func (v Value) SetCap(n int) {
	v.mustBeAssignable()
	v.mustBe(Slice)
	s := (*unsafeheader.Slice)(v.ptr)
	if n < s.Len || n > s.Cap {
		panic("reflect: slice capacity out of range in SetCap")
	}
	s.Cap = n
}

// SetMapIndex sets the element associated with key in the map v to elem.
// It panics if v's Kind is not Map.
// If elem is the zero Value, SetMapIndex deletes the key from the map.
// Otherwise if v holds a nil map, SetMapIndex will panic.
// As in Go, key's elem must be assignable to the map's key type,
// and elem's value must be assignable to the map's elem type.
// SetMapIndex将map v中key关联的元素设置为elem
// 如果v族不是地图，它就会惊慌 如果elem是0值，SetMapIndex将从映射中删除该键
// 否则，如果v持有一个nil映射，SetMapIndex将会出现混乱
// 在Go中，key的elem必须可赋值给映射的键类型，elem的值必须可赋值给映射的elem类型
func (v Value) SetMapIndex(key, elem Value) {
	v.mustBe(Map)
	v.mustBeExported()
	key.mustBeExported()
	tt := (*mapType)(unsafe.Pointer(v.typ))
	key = key.assignTo("reflect.Value.SetMapIndex", tt.key, nil)
	var k unsafe.Pointer
	if key.flag&flagIndir != 0 {
		k = key.ptr
	} else {
		k = unsafe.Pointer(&key.ptr)
	}
	if elem.typ == nil {
		mapdelete(v.typ, v.pointer(), k)
		return
	}
	elem.mustBeExported()
	elem = elem.assignTo("reflect.Value.SetMapIndex", tt.elem, nil)
	var e unsafe.Pointer
	if elem.flag&flagIndir != 0 {
		e = elem.ptr
	} else {
		e = unsafe.Pointer(&elem.ptr)
	}
	mapassign(v.typ, v.pointer(), k, e)
}

// SetUint sets v's underlying value to x.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64, or if CanSet() is false.
// SetUint设置v的基础值为x 如果v的类型不是Uint, Uintptr, Uint8, Uint16, Uint32或Uint64，或如果CanSet()为false，则会触发
func (v Value) SetUint(x uint64) {
	v.mustBeAssignable()
	switch k := v.kind(); k {
	default:
		panic(&ValueError{"reflect.Value.SetUint", v.kind()})
	case Uint:
		*(*uint)(v.ptr) = uint(x)
	case Uint8:
		*(*uint8)(v.ptr) = uint8(x)
	case Uint16:
		*(*uint16)(v.ptr) = uint16(x)
	case Uint32:
		*(*uint32)(v.ptr) = uint32(x)
	case Uint64:
		*(*uint64)(v.ptr) = x
	case Uintptr:
		*(*uintptr)(v.ptr) = uintptr(x)
	}
}

// SetPointer sets the unsafe.Pointer value v to x.
// It panics if v's Kind is not UnsafePointer.
// SetPointer设置不安全的 指针值v指向x 如果v的Kind不是UnsafePointer，它会惊慌
func (v Value) SetPointer(x unsafe.Pointer) {
	v.mustBeAssignable()
	v.mustBe(UnsafePointer)
	*(*unsafe.Pointer)(v.ptr) = x
}

// SetString sets v's underlying value to x.
// It panics if v's Kind is not String or if CanSet() is false.
// SetString将v的基础值设置为x 如果v的Kind不是String或如果CanSet()为false则会触发恐慌
func (v Value) SetString(x string) {
	v.mustBeAssignable()
	v.mustBe(String)
	*(*string)(v.ptr) = x
}

// Slice returns v[i:j].
// It panics if v's Kind is not Array, Slice or String, or if v is an unaddressable array,
// or if the indexes are out of bounds.
// 如果v的Kind不是Array、Slice或String，或者v是一个无法寻址的数组，或者索引越界，它会惊慌
func (v Value) Slice(i, j int) Value {
	var (
		cap  int
		typ  *sliceType
		base unsafe.Pointer
	)
	switch kind := v.kind(); kind {
	default:
		panic(&ValueError{"reflect.Value.Slice", v.kind()})

	case Array:
		if v.flag&flagAddr == 0 {
			panic("reflect.Value.Slice: slice of unaddressable array")
		}
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		cap = int(tt.len)
		typ = (*sliceType)(unsafe.Pointer(tt.slice))
		base = v.ptr

	case Slice:
		typ = (*sliceType)(unsafe.Pointer(v.typ))
		s := (*unsafeheader.Slice)(v.ptr)
		base = s.Data
		cap = s.Cap

	case String:
		s := (*unsafeheader.String)(v.ptr)
		if i < 0 || j < i || j > s.Len {
			panic("reflect.Value.Slice: string slice index out of bounds")
		}
		var t unsafeheader.String
		if i < s.Len {
			t = unsafeheader.String{Data: arrayAt(s.Data, i, 1, "i < s.Len"), Len: j - i}
		}
		return Value{v.typ, unsafe.Pointer(&t), v.flag}
	}

	if i < 0 || j < i || j > cap {
		panic("reflect.Value.Slice: slice index out of bounds")
	}

	// Declare slice so that gc can see the base pointer in it.
	// 声明切片，以便gc可以看到其中的基指针
	var x []unsafe.Pointer

	// Reinterpret as *unsafeheader.Slice to edit.
	// * unsafeheader重新诠释 片编辑
	s := (*unsafeheader.Slice)(unsafe.Pointer(&x))
	s.Len = j - i
	s.Cap = cap - i
	if cap-i > 0 {
		s.Data = arrayAt(base, i, typ.elem.Size(), "i < cap")
	} else {
		// do not advance pointer, to avoid pointing beyond end of slice
		s.Data = base
	}

	fl := v.flag.ro() | flagIndir | flag(Slice)
	return Value{typ.common(), unsafe.Pointer(&x), fl}
}

// Slice3 is the 3-index form of the slice operation: it returns v[i:j:k].
// It panics if v's Kind is not Array or Slice, or if v is an unaddressable array,
// or if the indexes are out of bounds.
// Slice3是切片操作的3索引形式:它返回v[i:j:k]
// 如果v的Kind不是Array或Slice，或者v是一个无法寻址的数组，或者索引越界，它就会惊慌
func (v Value) Slice3(i, j, k int) Value {
	var (
		cap  int
		typ  *sliceType
		base unsafe.Pointer
	)
	switch kind := v.kind(); kind {
	default:
		panic(&ValueError{"reflect.Value.Slice3", v.kind()})

	case Array:
		if v.flag&flagAddr == 0 {
			panic("reflect.Value.Slice3: slice of unaddressable array")
		}
		tt := (*arrayType)(unsafe.Pointer(v.typ))
		cap = int(tt.len)
		typ = (*sliceType)(unsafe.Pointer(tt.slice))
		base = v.ptr

	case Slice:
		typ = (*sliceType)(unsafe.Pointer(v.typ))
		s := (*unsafeheader.Slice)(v.ptr)
		base = s.Data
		cap = s.Cap
	}

	if i < 0 || j < i || k < j || k > cap {
		panic("reflect.Value.Slice3: slice index out of bounds")
	}

	// Declare slice so that the garbage collector
	// can see the base pointer in it.
	// 声明片，以便垃圾收集器可以看到其中的基指针
	var x []unsafe.Pointer

	// Reinterpret as *unsafeheader.Slice to edit.
	// * unsafeheader重新诠释 片编辑
	s := (*unsafeheader.Slice)(unsafe.Pointer(&x))
	s.Len = j - i
	s.Cap = k - i
	if k-i > 0 {
		s.Data = arrayAt(base, i, typ.elem.Size(), "i < k <= cap")
	} else {
		// do not advance pointer, to avoid pointing beyond end of slice
		// 不要提前指针，以避免指向超出切片的结束
		s.Data = base
	}

	fl := v.flag.ro() | flagIndir | flag(Slice)
	return Value{typ.common(), unsafe.Pointer(&x), fl}
}

// String returns the string v's underlying value, as a string.
// String is a special case because of Go's String method convention.
// Unlike the other getters, it does not panic if v's Kind is not String.
// String以字符串形式返回String v的基础值 String是一种特殊的情况，因为Go的String方法约定
// 与其他getter不同，如果v的Kind不是String，它不会惊慌
// Instead, it returns a string of the form "<T value>" where T is v's type.
// 相反，它返回一个“<T value>”形式的字符串，其中T是v的类型
// The fmt package treats Values specially. It does not call their String
// method implicitly but instead prints the concrete values they hold.
// fmt包特别对待Values 它不会隐式地调用它们的String方法，而是打印它们保存的具体值
// func (v值)String() String {
func (v Value) String() string {
	switch k := v.kind(); k {
	case Invalid:
		return "<invalid Value>"
	case String:
		return *(*string)(v.ptr)
	}
	// If you call String on a reflect.Value of other type, it's better to
	// print something than to panic. Useful in debugging.
	// 如果你在反射上调用String 其他类型的价值，与其恐慌，不如打印一些东西
	// 有用的调试
	return "<" + v.Type().String() + " Value>"
}

// TryRecv attempts to receive a value from the channel v but will not block.
// It panics if v's Kind is not Chan.
// If the receive delivers a value, x is the transferred value and ok is true.
// If the receive cannot finish without blocking, x is the zero Value and ok is false.
// If the channel is closed, x is the zero value for the channel's element type and ok is false.
// TryRecv尝试从通道v接收一个值，但不会被阻塞
// 如果v星人不是陈，就会恐慌 如果接收发送了一个值，x是传递的值，ok为真
// 如果接收不能在不阻塞的情况下完成，x为0值，ok为false
// 如果通道关闭，x是通道元素类型的0值，ok为false
func (v Value) TryRecv() (x Value, ok bool) {
	v.mustBe(Chan)
	v.mustBeExported()
	return v.recv(true)
}

// TrySend attempts to send x on the channel v but will not block.
// It panics if v's Kind is not Chan.
// It reports whether the value was sent.
// As in Go, x's value must be assignable to the channel's element type.
// TrySend尝试在通道v上发送x，但不会被阻塞
// 如果v星人不是陈，就会恐慌 它报告是否发送了值
// 在Go中，x的值必须可赋值给通道的元素类型
func (v Value) TrySend(x Value) bool {
	v.mustBe(Chan)
	v.mustBeExported()
	return v.send(x, true)
}

// Type returns v's type.
// Type返回v的类型
func (v Value) Type() Type {
	f := v.flag
	if f == 0 {
		panic(&ValueError{"reflect.Value.Type", Invalid})
	}
	if f&flagMethod == 0 {
		// Easy case
		// 简单的情况
		return v.typ
	}

	// Method value.
	// v.typ describes the receiver, not the method type.
	// v.t ype描述的是接收者，而不是方法类型
	i := int(v.flag) >> flagMethodShift
	if v.typ.Kind() == Interface {
		// Method on interface.
		// 方法接口
		tt := (*interfaceType)(unsafe.Pointer(v.typ))
		if uint(i) >= uint(len(tt.methods)) {
			panic("reflect: internal error: invalid method index")
		}
		m := &tt.methods[i]
		return v.typ.typeOff(m.typ)
	}
	// Method on concrete type.
	// 混凝土类型的方法
	ms := v.typ.exportedMethods()
	if uint(i) >= uint(len(ms)) {
		panic("reflect: internal error: invalid method index")
	}
	m := ms[i]
	return v.typ.typeOff(m.mtyp)
}

// Uint returns v's underlying value, as a uint64.
// It panics if v's Kind is not Uint, Uintptr, Uint8, Uint16, Uint32, or Uint64.
// Uint返回v的底层值，为uint64 如果v的Kind不是Uint, Uintptr, Uint8, Uint16, Uint32或Uint64则会触发
func (v Value) Uint() uint64 {
	k := v.kind()
	p := v.ptr
	switch k {
	case Uint:
		return uint64(*(*uint)(p))
	case Uint8:
		return uint64(*(*uint8)(p))
	case Uint16:
		return uint64(*(*uint16)(p))
	case Uint32:
		return uint64(*(*uint32)(p))
	case Uint64:
		return *(*uint64)(p)
	case Uintptr:
		return uint64(*(*uintptr)(p))
	}
	panic(&ValueError{"reflect.Value.Uint", v.kind()})
}

//go:nocheckptr
// This prevents inlining Value.UnsafeAddr when -d=checkptr is enabled,
// which ensures cmd/compile can recognize unsafe.Pointer(v.UnsafeAddr())
// and make an exception.
// 这将防止内联Value 当-d=checkptr启用时，将确保cmd/compile可以识别unsafe.Pointer(v.UnsafeAddr())并产生异常

// UnsafeAddr returns a pointer to v's data.
// It is for advanced clients that also import the "unsafe" package.
// It panics if v is not addressable.
// UnsafeAddr返回一个指向v的数据的指针 它适用于同时导入“不安全”包的高级客户端
// 如果v无法寻址，它就会惊慌
func (v Value) UnsafeAddr() uintptr {
	// TODO: deprecate
	if v.typ == nil {
		panic(&ValueError{"reflect.Value.UnsafeAddr", Invalid})
	}
	if v.flag&flagAddr == 0 {
		panic("reflect.Value.UnsafeAddr of unaddressable value")
	}
	return uintptr(v.ptr)
}

// StringHeader is the runtime representation of a string.
// It cannot be used safely or portably and its representation may
// change in a later release.
// Moreover, the Data field is not sufficient to guarantee the data
// it references will not be garbage collected, so programs must keep
// a separate, correctly typed pointer to the underlying data.
// StringHeader是字符串的运行时表示 它不能安全或可移植地使用，并且它的表示可能会在以后的版本中改变
// 此外，Data字段不足以保证它引用的数据不会被垃圾收集，因此程序必须保持一个独立的、类型正确的指向底层数据的指针
type StringHeader struct {
	Data uintptr
	Len  int
}

// SliceHeader is the runtime representation of a slice.
// It cannot be used safely or portably and its representation may
// change in a later release.
// Moreover, the Data field is not sufficient to guarantee the data
// it references will not be garbage collected, so programs must keep
// a separate, correctly typed pointer to the underlying data.
// SliceHeader是片的运行时表示 它不能安全或可移植地使用，并且它的表示可能会在以后的版本中改变
// 此外，Data字段不足以保证它引用的数据不会被垃圾收集，因此程序必须保持一个独立的、类型正确的指向底层数据的指针
type SliceHeader struct {
	Data uintptr
	Len  int
	Cap  int
}

func typesMustMatch(what string, t1, t2 Type) {
	if t1 != t2 {
		panic(what + ": " + t1.String() + " != " + t2.String())
	}
}

// arrayAt returns the i-th element of p,
// an array whose elements are eltSize bytes wide.
// The array pointed at by p must have at least i+1 elements:
// it is invalid (but impossible to check here) to pass i >= len,
// because then the result will point outside the array.
// whySafe must explain why i < len. (Passing "i < len" is fine;
// the benefit is to surface this assumption at the call site.)
// arrayAt返回p的第i个元素，该数组的元素都是eltSize字节宽的
// p指向的数组必须至少有i+1个元素:传入i = len是无效的(但这里不可能检查)，因为那样结果将指向数组外部
// 为什么安全必须解释我为什么不去 (传递' ilen '是可以的，其好处是在调用点显示这个假设)
func arrayAt(p unsafe.Pointer, i int, eltSize uintptr, whySafe string) unsafe.Pointer {
	return add(p, uintptr(i)*eltSize, "i < len")
}

// grow grows the slice s so that it can hold extra more values, allocating
// more capacity if needed. It also returns the old and new slice lengths.
// Grow对片进行增长，以便它可以保存更多的值，并在需要时分配更多的容量
// 它还返回旧的和新的切片长度
func grow(s Value, extra int) (Value, int, int) {
	i0 := s.Len()
	i1 := i0 + extra
	if i1 < i0 {
		panic("reflect.Append: slice overflow")
	}
	m := s.Cap()
	if i1 <= m {
		return s.Slice(0, i1), i0, i1
	}
	if m == 0 {
		m = extra
	} else {
		for m < i1 {
			if i0 < 1024 {
				m += m
			} else {
				m += m / 4
			}
		}
	}
	t := MakeSlice(s.Type(), i1, m)
	Copy(t, s)
	return t, i0, i1
}

// Append appends the values x to a slice s and returns the resulting slice.
// As in Go, each x's value must be assignable to the slice's element type.
// Append将值x追加到一个切片s，并返回结果切片
// 在Go中，每个x的值必须可赋值给切片的元素类型
func Append(s Value, x ...Value) Value {
	s.mustBe(Slice)
	s, i0, i1 := grow(s, len(x))
	for i, j := i0, 0; i < i1; i, j = i+1, j+1 {
		s.Index(i).Set(x[j])
	}
	return s
}

// AppendSlice appends a slice t to a slice s and returns the resulting slice.
// The slices s and t must have the same element type.
// 追加一个切片t到一个切片s，并返回结果切片
// 切片s和t必须具有相同的元素类型
func AppendSlice(s, t Value) Value {
	s.mustBe(Slice)
	t.mustBe(Slice)
	typesMustMatch("reflect.AppendSlice", s.Type().Elem(), t.Type().Elem())
	s, i0, i1 := grow(s, t.Len())
	Copy(s.Slice(i0, i1), t)
	return s
}

// Copy copies the contents of src into dst until either
// dst has been filled or src has been exhausted.
// It returns the number of elements copied.
// Dst and src each must have kind Slice or Array, and
// dst and src must have the same element type.
// Copy将src的内容复制到dst中，直到dst被填充或src耗尽
// 它返回复制的元素数量 Dst和src必须具有类型Slice或Array，并且Dst和src必须具有相同的元素类型
//
// As a special case, src can have kind String if the element type of dst is kind Uint8.
// 作为一种特殊情况，如果dst的元素类型是Uint8, src可以有kind String
func Copy(dst, src Value) int {
	dk := dst.kind()
	if dk != Array && dk != Slice {
		panic(&ValueError{"reflect.Copy", dk})
	}
	if dk == Array {
		dst.mustBeAssignable()
	}
	dst.mustBeExported()

	sk := src.kind()
	var stringCopy bool
	if sk != Array && sk != Slice {
		stringCopy = sk == String && dst.typ.Elem().Kind() == Uint8
		if !stringCopy {
			panic(&ValueError{"reflect.Copy", sk})
		}
	}
	src.mustBeExported()

	de := dst.typ.Elem()
	if !stringCopy {
		se := src.typ.Elem()
		typesMustMatch("reflect.Copy", de, se)
	}

	var ds, ss unsafeheader.Slice
	if dk == Array {
		ds.Data = dst.ptr
		ds.Len = dst.Len()
		ds.Cap = ds.Len
	} else {
		ds = *(*unsafeheader.Slice)(dst.ptr)
	}
	if sk == Array {
		ss.Data = src.ptr
		ss.Len = src.Len()
		ss.Cap = ss.Len
	} else if sk == Slice {
		ss = *(*unsafeheader.Slice)(src.ptr)
	} else {
		sh := *(*unsafeheader.String)(src.ptr)
		ss.Data = sh.Data
		ss.Len = sh.Len
		ss.Cap = sh.Len
	}

	return typedslicecopy(de.common(), ds, ss)
}

// A runtimeSelect is a single case passed to rselect.
// This must match ../runtime/select.go:/runtimeSelect
// runtimeSelect是传递给rselect的单个case 这必须匹配../runtime/select.go:/runtimeSelect
type runtimeSelect struct {
	dir SelectDir      // SelectSend, SelectRecv or SelectDefault
	typ *rtype         // channel type
	ch  unsafe.Pointer // channel
	val unsafe.Pointer // ptr to data (SendDir) or ptr to receive buffer (RecvDir)
}

// rselect runs a select. It returns the index of the chosen case.
// If the case was a receive, val is filled in with the received value.
// The conventional OK bool indicates whether the receive corresponds
// to a sent value.
// Rselect运行一个选择 它返回所选案例的索引
// 如果案例是接收到的，则用接收到的值填充val
// 常规的OK bool指示接收值是否对应于发送值
//go:noescape
func rselect([]runtimeSelect) (chosen int, recvOK bool)

// A SelectDir describes the communication direction of a select case.
// SelectDir用于描述选择案例的通信方向
type SelectDir int

// NOTE: These values must match ../runtime/select.go:/selectDir.

const (
	_             SelectDir = iota
	SelectSend              // case Chan <- Send
	SelectRecv              // case <-Chan:
	SelectDefault           // default
)

// A SelectCase describes a single case in a select operation.
// The kind of case depends on Dir, the communication direction.
// SelectCase描述选择操作中的单个case 这种情况取决于迪尔，沟通的方向
//
// If Dir is SelectDefault, the case represents a default case.
// Chan and Send must be zero Values.
// 如果Dir为SelectDefault，则大小写表示默认情况
// Chan和Send值必须为零
//
// If Dir is SelectSend, the case represents a send operation.
// Normally Chan's underlying value must be a channel, and Send's underlying value must be
// assignable to the channel's element type. As a special case, if Chan is a zero Value,
// then the case is ignored, and the field Send will also be ignored and may be either zero
// or non-zero.
// 如果Dir为SelectSend, case表示发送操作 通常，Chan的基础值必须是通道，而Send的基础值必须可赋值给通道的元素类型
// 作为一个特殊的情况，如果Chan是一个零值，那么这种情况会被忽略，并且字段Send也会被忽略，可以是零或非零
//
// If Dir is SelectRecv, the case represents a receive operation.
// Normally Chan's underlying value must be a channel and Send must be a zero Value.
// If Chan is a zero Value, then the case is ignored, but Send must still be a zero Value.
// When a receive operation is selected, the received Value is returned by Select.
// 如果Dir为SelectRecv, case表示接收操作 通常，Chan的基础值必须是通道，而Send必须是零值
// 如果Chan是一个零值，那么case被忽略，但是Send必须仍然是一个零值
// 当选择接收操作时，接收的值由Select返回
//
type SelectCase struct {
	Dir  SelectDir // direction of case
	Chan Value     // channel to use (for send or receive)
	Send Value     // value to send (for send)
}

// Select executes a select operation described by the list of cases.
// Like the Go select statement, it blocks until at least one of the cases
// can proceed, makes a uniform pseudo-random choice,
// and then executes that case. It returns the index of the chosen case
// and, if that case was a receive operation, the value received and a
// boolean indicating whether the value corresponds to a send on the channel
// (as opposed to a zero value received because the channel is closed).
// Select supports a maximum of 65536 cases.
// Select执行案例列表所描述的选择操作 与Go select语句一样，它会阻塞，直到至少有一种情况可以继续，做出统一的伪随机选择，然后执行该情况
// 它返回所选案例的索引，如果该案例是接收操作，则返回所接收的值和一个布尔值，该布尔值指示该值是否对应于通道上的发送(而不是由于通道关闭而接收的零值)
// 选择最多支持65536例
func Select(cases []SelectCase) (chosen int, recv Value, recvOK bool) {
	if len(cases) > 65536 {
		panic("reflect.Select: too many cases (max 65536)")
	}
	// NOTE: Do not trust that caller is not modifying cases data underfoot.
	// The range is safe because the caller cannot modify our copy of the len
	// and each iteration makes its own copy of the value c.
// 注意:不要相信调用者没有修改案例数据
// range是安全的，因为调用者不能修改len的副本，而且每次迭代都有自己的值c的副本
	var runcases []runtimeSelect
	if len(cases) > 4 {
		// Slice is heap allocated due to runtime dependent capacity.
// 由于与运行时相关的容量，片被堆分配
		runcases = make([]runtimeSelect, len(cases))
	} else {
		// Slice can be stack allocated due to constant capacity.
// 由于容量不变，片可以堆栈分配
		runcases = make([]runtimeSelect, len(cases), 4)
	}

	haveDefault := false
	for i, c := range cases {
		rc := &runcases[i]
		rc.dir = c.Dir
		switch c.Dir {
		default:
			panic("reflect.Select: invalid Dir")

		case SelectDefault: // default
			if haveDefault {
				panic("reflect.Select: multiple default cases")
			}
			haveDefault = true
			if c.Chan.IsValid() {
				panic("reflect.Select: default case has Chan value")
			}
			if c.Send.IsValid() {
				panic("reflect.Select: default case has Send value")
			}

		case SelectSend:
			ch := c.Chan
			if !ch.IsValid() {
				break
			}
			ch.mustBe(Chan)
			ch.mustBeExported()
			tt := (*chanType)(unsafe.Pointer(ch.typ))
			if ChanDir(tt.dir)&SendDir == 0 {
				panic("reflect.Select: SendDir case using recv-only channel")
			}
			rc.ch = ch.pointer()
			rc.typ = &tt.rtype
			v := c.Send
			if !v.IsValid() {
				panic("reflect.Select: SendDir case missing Send value")
			}
			v.mustBeExported()
			v = v.assignTo("reflect.Select", tt.elem, nil)
			if v.flag&flagIndir != 0 {
				rc.val = v.ptr
			} else {
				rc.val = unsafe.Pointer(&v.ptr)
			}

		case SelectRecv:
			if c.Send.IsValid() {
				panic("reflect.Select: RecvDir case has Send value")
			}
			ch := c.Chan
			if !ch.IsValid() {
				break
			}
			ch.mustBe(Chan)
			ch.mustBeExported()
			tt := (*chanType)(unsafe.Pointer(ch.typ))
			if ChanDir(tt.dir)&RecvDir == 0 {
				panic("reflect.Select: RecvDir case using send-only channel")
			}
			rc.ch = ch.pointer()
			rc.typ = &tt.rtype
			rc.val = unsafe_New(tt.elem)
		}
	}

	chosen, recvOK = rselect(runcases)
	if runcases[chosen].dir == SelectRecv {
		tt := (*chanType)(unsafe.Pointer(runcases[chosen].typ))
		t := tt.elem
		p := runcases[chosen].val
		fl := flag(t.Kind())
		if ifaceIndir(t) {
			recv = Value{t, p, fl | flagIndir}
		} else {
			recv = Value{t, *(*unsafe.Pointer)(p), fl}
		}
	}
	return chosen, recv, recvOK
}

/*
 * constructors
 */

// implemented in package runtime
// 在runtime包实现
func unsafe_New(*rtype) unsafe.Pointer
func unsafe_NewArray(*rtype, int) unsafe.Pointer

// MakeSlice creates a new zero-initialized slice value
// for the specified slice type, length, and capacity.
// MakeSlice为指定的片类型、长度和容量创建一个新的初始化为零的片值
func MakeSlice(typ Type, len, cap int) Value {
	if typ.Kind() != Slice {
		panic("reflect.MakeSlice of non-slice type")
	}
	if len < 0 {
		panic("reflect.MakeSlice: negative len")
	}
	if cap < 0 {
		panic("reflect.MakeSlice: negative cap")
	}
	if len > cap {
		panic("reflect.MakeSlice: len > cap")
	}

	s := unsafeheader.Slice{Data: unsafe_NewArray(typ.Elem().(*rtype), cap), Len: len, Cap: cap}
	return Value{typ.(*rtype), unsafe.Pointer(&s), flagIndir | flag(Slice)}
}

// MakeChan creates a new channel with the specified type and buffer size.
// MakeChan创建一个具有指定类型和缓冲区大小的新通道
func MakeChan(typ Type, buffer int) Value {
	if typ.Kind() != Chan {
		panic("reflect.MakeChan of non-chan type")
	}
	if buffer < 0 {
		panic("reflect.MakeChan: negative buffer size")
	}
	if typ.ChanDir() != BothDir {
		panic("reflect.MakeChan: unidirectional channel type")
	}
	t := typ.(*rtype)
	ch := makechan(t, buffer)
	return Value{t, ch, flag(Chan)}
}

// MakeMap creates a new map with the specified type.
// MakeMap创建一个具有指定类型的新映射
func MakeMap(typ Type) Value {
	return MakeMapWithSize(typ, 0)
}

// MakeMapWithSize creates a new map with the specified type
// and initial space for approximately n elements.
// MakeMapWithSize为大约n个元素创建一个具有指定类型和初始空间的新映射
func MakeMapWithSize(typ Type, n int) Value {
	if typ.Kind() != Map {
		panic("reflect.MakeMapWithSize of non-map type")
	}
	t := typ.(*rtype)
	m := makemap(t, n)
	return Value{t, m, flag(Map)}
}

// Indirect returns the value that v points to.
// If v is a nil pointer, Indirect returns a zero Value.
// If v is not a pointer, Indirect returns v.
// 间接返回v所指向的值 如果v是一个空指针，间接返回一个零值
// 如果v不是指针，则Indirect返回v
func Indirect(v Value) Value {
	if v.Kind() != Ptr {
		return v
	}
	return v.Elem()
}

// ValueOf returns a new Value initialized to the concrete value
// stored in the interface i. ValueOf(nil) returns the zero Value.
// ValueOf返回一个新值，初始化为存储在接口i中的具体值
// ValueOf(nil)返回0值
func ValueOf(i interface{}) Value {
	if i == nil {
		return Value{}
	}

	// TODO: Maybe allow contents of a Value to live on the stack.
	// For now we make the contents always escape to the heap. It
	// makes life easier in a few places (see chanrecv/mapassign
	// comment below).
// 要做的事情:可能允许Value的内容存在于堆栈中
// 现在，我们让内容总是转义到堆 它使生活在一些地方更容易(见下面的chanrecv/mapassign评论)
	escapes(i)

	return unpackEface(i)
}

// Zero returns a Value representing the zero value for the specified type.
// The result is different from the zero value of the Value struct,
// which represents no value at all.
// For example, Zero(TypeOf(42)) returns a Value with Kind Int and value 0.
// The returned value is neither addressable nor settable.
// Zero返回一个值，表示指定类型的0值 结果与value结构的零值不同，零值表示根本没有值
// 例如，Zero(TypeOf(42))返回一个具有类型Int和值0的Value
// 返回值既不能寻址也不能设置
func Zero(typ Type) Value {
	if typ == nil {
		panic("reflect: Zero(nil)")
	}
	t := typ.(*rtype)
	fl := flag(t.Kind())
	if ifaceIndir(t) {
		var p unsafe.Pointer
		if t.size <= maxZero {
			p = unsafe.Pointer(&zeroVal[0])
		} else {
			p = unsafe_New(t)
		}
		return Value{t, p, fl | flagIndir}
	}
	return Value{t, nil, fl}
}

// must match declarations in runtime/map.go.
// 必须匹配runtime/map.go中的声明
const maxZero = 1024

//go:linkname zeroVal runtime.zeroVal
var zeroVal [maxZero]byte

// New returns a Value representing a pointer to a new zero value
// for the specified type. That is, the returned Value's Type is PtrTo(typ).
// New返回一个Value，表示指向指定类型的新零值的指针
// 即返回值的类型为PtrTo(typ)
func New(typ Type) Value {
	if typ == nil {
		panic("reflect: New(nil)")
	}
	t := typ.(*rtype)
	pt := t.ptrTo()
	if ifaceIndir(pt) {
		// This is a pointer to a go:notinheap type.
// 这是一个指向go:notinheap类型的指针
		panic("reflect: New of type that may not be allocated in heap (possibly undefined cgo C type)")
	}
	ptr := unsafe_New(t)
	fl := flag(Ptr)
	return Value{pt, ptr, fl}
}

// NewAt returns a Value representing a pointer to a value of the
// specified type, using p as that pointer.
// NewAt返回一个Value，表示一个指向指定类型值的指针，使用p作为该指针
func NewAt(typ Type, p unsafe.Pointer) Value {
	fl := flag(Ptr)
	t := typ.(*rtype)
	return Value{t.ptrTo(), p, fl}
}

// assignTo returns a value v that can be assigned directly to typ.
// It panics if v is not assignable to typ.
// For a conversion to an interface type, target is a suggested scratch space to use.
// target must be initialized memory (or nil).
// assignTo返回一个可以直接赋给typ的值v 如果v不能赋值给typ，它就会惊慌
// 对于向接口类型的转换，建议使用target临时空间
// 目标必须被初始化内存(或nil)
func (v Value) assignTo(context string, dst *rtype, target unsafe.Pointer) Value {
	if v.flag&flagMethod != 0 {
		v = makeMethodValue(context, v)
	}

	switch {
	case directlyAssignable(dst, v.typ):
		// Overwrite type so that they match.
		// Same memory layout, so no harm done.
// 覆盖类型以使它们匹配 相同的内存布局，所以没有造成伤害
		fl := v.flag&(flagAddr|flagIndir) | v.flag.ro()
		fl |= flag(dst.Kind())
		return Value{dst, v.ptr, fl}

	case implements(dst, v.typ):
		if target == nil {
			target = unsafe_New(dst)
		}
		if v.Kind() == Interface && v.IsNil() {
			// A nil ReadWriter passed to nil Reader is OK,
			// but using ifaceE2I below will panic.
			// Avoid the panic by returning a nil dst (e.g., Reader) explicitly.
// 将一个nil ReadWriter传递给nil Reader是可以的，但使用下面的ifaceE2I会造成混乱
// 通过显式返回一个nil dst(例如Reader)来避免恐慌
			return Value{dst, nil, flag(Interface)}
		}
		x := valueInterface(v, false)
		if dst.NumMethod() == 0 {
			*(*interface{})(target) = x
		} else {
			ifaceE2I(dst, x, target)
		}
		return Value{dst, target, flagIndir | flag(Interface)}
	}

	// Failed.
	panic(context + ": value of type " + v.typ.String() + " is not assignable to type " + dst.String())
}

// Convert returns the value v converted to type t.
// If the usual Go conversion rules do not allow conversion
// of the value v to type t, or if converting v to type t panics, Convert panics.
// Convert返回转换为t类型的值v 如果通常的Go转换规则不允许将值v转换为t类型，或者如果将v转换为t类型，则Convert panic
func (v Value) Convert(t Type) Value {
	if v.flag&flagMethod != 0 {
		v = makeMethodValue("Convert", v)
	}
	op := convertOp(t.common(), v.typ)
	if op == nil {
		panic("reflect.Value.Convert: value of type " + v.typ.String() + " cannot be converted to type " + t.String())
	}
	return op(v, t)
}

// CanConvert reports whether the value v can be converted to type t.
// If v.CanConvert(t) returns true then v.Convert(t) will not panic.
// CanConvert报告v值是否可以转换为t类型 如果v.CanConvert(t)返回true，那么v. convert (t)将不会出现恐慌
func (v Value) CanConvert(t Type) bool {
	vt := v.Type()
	if !vt.ConvertibleTo(t) {
		return false
	}
	// Currently the only conversion that is OK in terms of type
	// but that can panic depending on the value is converting
	// from slice to pointer-to-array.
// 目前唯一的转换是可以的类型，但可以恐慌取决于值是从片转换到指针数组
	if vt.Kind() == Slice && t.Kind() == Ptr && t.Elem().Kind() == Array {
		n := t.Elem().Len()
		h := (*unsafeheader.Slice)(v.ptr)
		if n > h.Len {
			return false
		}
	}
	return true
}

// convertOp returns the function to convert a value of type src
// to a value of type dst. If the conversion is illegal, convertOp returns nil.
// convertOp返回将src类型的值转换为dst类型的值的函数
// 如果转换是非法的，convertOp返回nil
func convertOp(dst, src *rtype) func(Value, Type) Value {
	switch src.Kind() {
	case Int, Int8, Int16, Int32, Int64:
		switch dst.Kind() {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
			return cvtInt
		case Float32, Float64:
			return cvtIntFloat
		case String:
			return cvtIntString
		}

	case Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
		switch dst.Kind() {
		case Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
			return cvtUint
		case Float32, Float64:
			return cvtUintFloat
		case String:
			return cvtUintString
		}

	case Float32, Float64:
		switch dst.Kind() {
		case Int, Int8, Int16, Int32, Int64:
			return cvtFloatInt
		case Uint, Uint8, Uint16, Uint32, Uint64, Uintptr:
			return cvtFloatUint
		case Float32, Float64:
			return cvtFloat
		}

	case Complex64, Complex128:
		switch dst.Kind() {
		case Complex64, Complex128:
			return cvtComplex
		}

	case String:
		if dst.Kind() == Slice && dst.Elem().PkgPath() == "" {
			switch dst.Elem().Kind() {
			case Uint8:
				return cvtStringBytes
			case Int32:
				return cvtStringRunes
			}
		}

	case Slice:
		if dst.Kind() == String && src.Elem().PkgPath() == "" {
			switch src.Elem().Kind() {
			case Uint8:
				return cvtBytesString
			case Int32:
				return cvtRunesString
			}
		}
		// "x is a slice, T is a pointer-to-array type,
		// and the slice and array types have identical element types."
		if dst.Kind() == Ptr && dst.Elem().Kind() == Array && src.Elem() == dst.Elem().Elem() {
			return cvtSliceArrayPtr
		}

	case Chan:
		if dst.Kind() == Chan && specialChannelAssignability(dst, src) {
			return cvtDirect
		}
	}

	// dst and src have same underlying type.
// DST和SRC具有相同的基础类型
	if haveIdenticalUnderlyingType(dst, src, false) {
		return cvtDirect
	}

	// dst and src are non-defined pointer types with same underlying base type.
// DST和SRC是具有相同基础基类型的非定义指针类型
	if dst.Kind() == Ptr && dst.Name() == "" &&
		src.Kind() == Ptr && src.Name() == "" &&
		haveIdenticalUnderlyingType(dst.Elem().common(), src.Elem().common(), false) {
		return cvtDirect
	}

	if implements(dst, src) {
		if src.Kind() == Interface {
			return cvtI2I
		}
		return cvtT2I
	}

	return nil
}

// makeInt returns a Value of type t equal to bits (possibly truncated),
// where t is a signed or unsigned int type.
// makeInt返回一个t类型的值，它等于位(可能被截断)，其中t是有符号或无符号的int类型
func makeInt(f flag, bits uint64, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	switch typ.size {
	case 1:
		*(*uint8)(ptr) = uint8(bits)
	case 2:
		*(*uint16)(ptr) = uint16(bits)
	case 4:
		*(*uint32)(ptr) = uint32(bits)
	case 8:
		*(*uint64)(ptr) = bits
	}
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

// makeFloat returns a Value of type t equal to v (possibly truncated to float32),
// where t is a float32 or float64 type.
// makeFloat返回一个t类型等于v(可能被截断为float32)的Value，其中t是float32或float64类型
func makeFloat(f flag, v float64, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	switch typ.size {
	case 4:
		*(*float32)(ptr) = float32(v)
	case 8:
		*(*float64)(ptr) = v
	}
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

// makeFloat returns a Value of type t equal to v, where t is a float32 type.
// makeFloat返回一个t类型等于v的Value，其中t是float32类型
func makeFloat32(f flag, v float32, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	*(*float32)(ptr) = v
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

// makeComplex returns a Value of type t equal to v (possibly truncated to complex64),
// where t is a complex64 or complex128 type.
// makeComplex返回类型为t的Value，其值等于v(可能被截断为complex64)，其中t是complex64或complex128类型
func makeComplex(f flag, v complex128, t Type) Value {
	typ := t.common()
	ptr := unsafe_New(typ)
	switch typ.size {
	case 8:
		*(*complex64)(ptr) = complex64(v)
	case 16:
		*(*complex128)(ptr) = v
	}
	return Value{typ, ptr, f | flagIndir | flag(typ.Kind())}
}

func makeString(f flag, v string, t Type) Value {
	ret := New(t).Elem()
	ret.SetString(v)
	ret.flag = ret.flag&^flagAddr | f
	return ret
}

func makeBytes(f flag, v []byte, t Type) Value {
	ret := New(t).Elem()
	ret.SetBytes(v)
	ret.flag = ret.flag&^flagAddr | f
	return ret
}

func makeRunes(f flag, v []rune, t Type) Value {
	ret := New(t).Elem()
	ret.setRunes(v)
	ret.flag = ret.flag&^flagAddr | f
	return ret
}

// These conversion functions are returned by convertOp
// for classes of conversions. For example, the first function, cvtInt,
// takes any value v of signed int type and returns the value converted
// to type t, where t is any signed or unsigned int type.
// 这些转换函数由convertOp为转换类返回 例如，第一个函数cvtInt接受任何有符号int类型的值v，并返回转换为t类型的值，其中t是任何有符号或无符号int类型

// convertOp: intXX -> [u]intXX
func cvtInt(v Value, t Type) Value {
	return makeInt(v.flag.ro(), uint64(v.Int()), t)
}

// convertOp: uintXX -> [u]intXX
func cvtUint(v Value, t Type) Value {
	return makeInt(v.flag.ro(), v.Uint(), t)
}

// convertOp: floatXX -> intXX
func cvtFloatInt(v Value, t Type) Value {
	return makeInt(v.flag.ro(), uint64(int64(v.Float())), t)
}

// convertOp: floatXX -> uintXX
func cvtFloatUint(v Value, t Type) Value {
	return makeInt(v.flag.ro(), uint64(v.Float()), t)
}

// convertOp: intXX -> floatXX
func cvtIntFloat(v Value, t Type) Value {
	return makeFloat(v.flag.ro(), float64(v.Int()), t)
}

// convertOp: uintXX -> floatXX
func cvtUintFloat(v Value, t Type) Value {
	return makeFloat(v.flag.ro(), float64(v.Uint()), t)
}

// convertOp: floatXX -> floatXX
func cvtFloat(v Value, t Type) Value {
	if v.Type().Kind() == Float32 && t.Kind() == Float32 {
		// Don't do any conversion if both types have underlying type float32.
		// This avoids converting to float64 and back, which will
		// convert a signaling NaN to a quiet NaN. See issue 36400.
// 如果两种类型的基础类型都是float32，则不要进行任何转换
// 这避免了转换到float64和返回，这将把一个信号NaN转换为一个安静的NaN
// 看到发行36400
		return makeFloat32(v.flag.ro(), *(*float32)(v.ptr), t)
	}
	return makeFloat(v.flag.ro(), v.Float(), t)
}

// convertOp: complexXX -> complexXX
func cvtComplex(v Value, t Type) Value {
	return makeComplex(v.flag.ro(), v.Complex(), t)
}

// convertOp: intXX -> string
func cvtIntString(v Value, t Type) Value {
	s := "\uFFFD"
	if x := v.Int(); int64(rune(x)) == x {
		s = string(rune(x))
	}
	return makeString(v.flag.ro(), s, t)
}

// convertOp: uintXX -> string
func cvtUintString(v Value, t Type) Value {
	s := "\uFFFD"
	if x := v.Uint(); uint64(rune(x)) == x {
		s = string(rune(x))
	}
	return makeString(v.flag.ro(), s, t)
}

// convertOp: []byte -> string
func cvtBytesString(v Value, t Type) Value {
	return makeString(v.flag.ro(), string(v.Bytes()), t)
}

// convertOp: string -> []byte
func cvtStringBytes(v Value, t Type) Value {
	return makeBytes(v.flag.ro(), []byte(v.String()), t)
}

// convertOp: []rune -> string
func cvtRunesString(v Value, t Type) Value {
	return makeString(v.flag.ro(), string(v.runes()), t)
}

// convertOp: string -> []rune
func cvtStringRunes(v Value, t Type) Value {
	return makeRunes(v.flag.ro(), []rune(v.String()), t)
}

// convertOp: []T -> *[N]T
func cvtSliceArrayPtr(v Value, t Type) Value {
	n := t.Elem().Len()
	h := (*unsafeheader.Slice)(v.ptr)
	if n > h.Len {
		panic("reflect: cannot convert slice with length " + itoa.Itoa(h.Len) + " to pointer to array with length " + itoa.Itoa(n))
	}
	return Value{t.common(), h.Data, v.flag&^(flagIndir|flagAddr|flagKindMask) | flag(Ptr)}
}

// convertOp: direct copy
func cvtDirect(v Value, typ Type) Value {
	f := v.flag
	t := typ.common()
	ptr := v.ptr
	if f&flagAddr != 0 {
		// indirect, mutable word - make a copy
// 间接的，可变的词-复制
		c := unsafe_New(t)
		typedmemmove(t, c, ptr)
		ptr = c
		f &^= flagAddr
	}
	return Value{t, ptr, v.flag.ro() | f} // v.flag.ro()|f == f?
}

// convertOp: concrete -> interface
func cvtT2I(v Value, typ Type) Value {
	target := unsafe_New(typ.common())
	x := valueInterface(v, false)
	if typ.NumMethod() == 0 {
		*(*interface{})(target) = x
	} else {
		ifaceE2I(typ.(*rtype), x, target)
	}
	return Value{typ.common(), target, v.flag.ro() | flagIndir | flag(Interface)}
}

// convertOp: interface -> interface
func cvtI2I(v Value, typ Type) Value {
	if v.IsNil() {
		ret := Zero(typ)
		ret.flag |= v.flag.ro()
		return ret
	}
	return cvtT2I(v.Elem(), typ)
}

// implemented in ../runtime
func chancap(ch unsafe.Pointer) int
func chanclose(ch unsafe.Pointer)
func chanlen(ch unsafe.Pointer) int

// Note: some of the noescape annotations below are technically a lie,
// but safe in the context of this package. Functions like chansend
// and mapassign don't escape the referent, but may escape anything
// the referent points to (they do shallow copies of the referent).
// It is safe in this package because the referent may only point
// to something a Value may point to, and that is always in the heap
// (due to the escapes() call in ValueOf).
// 注意:下面的一些noescape注释在技术上是一个谎言，但是在这个包的上下文中是安全的
// 像chansend和mapassign这样的函数不会转义referent，但是可以转义referent指向的任何东西(它们做referent的浅拷贝)
// 它在这个包中是安全的，因为referent可能只指向Value可能指向的东西，而且它总是在堆中(由于ValueOf中的escape()调用)

//go:noescape
func chanrecv(ch unsafe.Pointer, nb bool, val unsafe.Pointer) (selected, received bool)

//go:noescape
func chansend(ch unsafe.Pointer, val unsafe.Pointer, nb bool) bool

func makechan(typ *rtype, size int) (ch unsafe.Pointer)
func makemap(t *rtype, cap int) (m unsafe.Pointer)

//go:noescape
func mapaccess(t *rtype, m unsafe.Pointer, key unsafe.Pointer) (val unsafe.Pointer)

//go:noescape
func mapassign(t *rtype, m unsafe.Pointer, key, val unsafe.Pointer)

//go:noescape
func mapdelete(t *rtype, m unsafe.Pointer, key unsafe.Pointer)

// m escapes into the return value, but the caller of mapiterinit
// doesn't let the return value escape.
// M转义为返回值，但是mapiterinit的调用者不允许转义返回值
//go:noescape
func mapiterinit(t *rtype, m unsafe.Pointer) unsafe.Pointer

//go:noescape
func mapiterkey(it unsafe.Pointer) (key unsafe.Pointer)

//go:noescape
func mapiterelem(it unsafe.Pointer) (elem unsafe.Pointer)

//go:noescape
func mapiternext(it unsafe.Pointer)

//go:noescape
func maplen(m unsafe.Pointer) int

// call calls fn with "stackArgsSize" bytes of stack arguments laid out
// at stackArgs and register arguments laid out in regArgs. frameSize is
// the total amount of stack space that will be reserved by call, so this
// should include enough space to spill register arguments to the stack in
// case of preemption.
// call调用带' stackArgsSize '字节的堆栈参数在stackArgs和注册参数在regArgs
// frameSize是调用将保留的堆栈空间的总量，因此它应该包含足够的空间，以便在抢占的情况下将寄存器参数溢出堆栈
//
// After fn returns, call copies stackArgsSize-stackRetOffset result bytes
// back into stackArgs+stackRetOffset before returning, for any return
// values passed on the stack. Register-based return values will be found
// in the same regArgs structure.
// fn返回后，调用将stackargsize -stackRetOffset结果字节复制回返回前的stackArgs+stackRetOffset，任何返回值传递给堆栈
// 基于寄存器的返回值将在相同的regArgs结构中找到
//
// regArgs must also be prepared with an appropriate ReturnIsPtr bitmap
// indicating which registers will contain pointer-valued return values. The
// purpose of this bitmap is to keep pointers visible to the GC between
// returning from reflectcall and actually using them.
// reggs还必须准备一个适当的ReturnIsPtr位图，指示哪些寄存器将包含指针值返回值
// 这个位图的目的是在从reflectcall返回和实际使用它们之间保持指针对GC可见
//
// If copying result bytes back from the stack, the caller must pass the
// argument frame type as stackArgsType, so that call can execute appropriate
// write barriers during the copy.
// 如果将结果字节从堆栈复制回来，调用方必须将参数帧类型传递为stackArgsType，以便调用方可以在复制期间执行适当的写屏障
//
// Arguments passed through to call do not escape. The type is used only in a
// very limited callee of call, the stackArgs are copied, and regArgs is only
// used in the call frame.
// 传递给调用的参数不会转义 该类型只在一个非常有限的调用调用中使用，stackArgs被复制，而regArgs只在调用帧中使用
//go:noescape
//go:linkname call runtime.reflectcall
func call(stackArgsType *rtype, f, stackArgs unsafe.Pointer, stackArgsSize, stackRetOffset, frameSize uint32, regArgs *abi.RegArgs)

func ifaceE2I(t *rtype, src interface{}, dst unsafe.Pointer)

// memmove copies size bytes to dst from src. No write barriers are used.
// Memmove将大小字节从src复制到DST 不使用写屏障
//go:noescape
func memmove(dst, src unsafe.Pointer, size uintptr)

// typedmemmove copies a value of type t to dst from src.
// Typedmemmove将类型t的值从src复制到DST
//go:noescape
func typedmemmove(t *rtype, dst, src unsafe.Pointer)

// typedmemmovepartial is like typedmemmove but assumes that
// dst and src point off bytes into the value and only copies size bytes.
// Typedmemmovepartial类似于typedmemmove，但假设DST和SRC将字节指向值，并且只复制size字节
//go:noescape
func typedmemmovepartial(t *rtype, dst, src unsafe.Pointer, off, size uintptr)

// typedmemclr zeros the value at ptr of type t.
// Typedmemclr将t类型的PTR处的值置零
//go:noescape
func typedmemclr(t *rtype, ptr unsafe.Pointer)

// typedmemclrpartial is like typedmemclr but assumes that
// dst points off bytes into the value and only clears size bytes.
// Typedmemclrpartial类似于typedmemclr，但假设DST将字节指向值，并且只清除大小字节
//go:noescape
func typedmemclrpartial(t *rtype, ptr unsafe.Pointer, off, size uintptr)

// typedslicecopy copies a slice of elemType values from src to dst,
// returning the number of elements copied.
// typedslicecopy将一个elemType值的切片从src复制到dst，
//go:noescape
func typedslicecopy(elemType *rtype, dst, src unsafeheader.Slice) int

//go:noescape
func typehash(t *rtype, p unsafe.Pointer, h uintptr) uintptr

// Dummy annotation marking that the value x escapes,
// for use in cases where the reflect code is so clever that
// the compiler cannot follow.
// 标记值x转义的哑注释，用于反射代码非常聪明，以致编译器无法跟上的情况
func escapes(x interface{}) {
	if dummy.b {
		dummy.x = x
	}
}

var dummy struct {
	b bool
	x interface{}
}
