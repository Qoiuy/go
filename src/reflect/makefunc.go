// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// MakeFunc implementation.
// MakeFunc实现。

package reflect

import (
	"internal/abi"
	"unsafe"
)

// makeFuncImpl is the closure value implementing the function
// returned by MakeFunc.
// The first three words of this type must be kept in sync with
// methodValue and runtime.reflectMethodValue.
// Any changes should be reflected in all three.
// makeFuncImpl是实现由MakeFunc返回的函数的闭包值。
// 该类型的前三个单词必须与methodValue和runtime.reflectMethodValue保持同步。
// 任何变化都应该反映在这三个方面。
type makeFuncImpl struct {
	makeFuncCtxt
	ftyp *funcType
	fn   func([]Value) []Value
}

// MakeFunc returns a new function of the given Type
// that wraps the function fn. When called, that new function
// does the following:
//
//	- converts its arguments to a slice of Values.
//	- runs results := fn(args).
//	- returns the results as a slice of Values, one per formal result.
//
// The implementation fn can assume that the argument Value slice
// has the number and type of arguments given by typ.
// If typ describes a variadic function, the final Value is itself
// a slice representing the variadic arguments, as in the
// body of a variadic function. The result Value slice returned by fn
// must have the number and type of results given by typ.
//
// The Value.Call method allows the caller to invoke a typed function
// in terms of Values; in contrast, MakeFunc allows the caller to implement
// a typed function in terms of Values.
//
// The Examples section of the documentation includes an illustration
// of how to use MakeFunc to build a swap function for different types.
//
// MakeFunc返回一个给定类型的新函数，该函数封装了fn函数。
// 当被调用时，这个新函数执行以下操作:-运行结果:= fn(args)。-返回的结果作为一个片的值，每一个正式的结果。实现fn可以假设参数Value slice具有typ给出的参数数量和类型。如果typ描述了一个可变参数函数，那么最终的Value本身就是一个表示可变参数的切片，就像在可变参数函数体中一样。fn返回的结果值切片必须有typ给出的结果数量和类型。值。Call方法允许调用者调用一个值类型的函数;相比之下，MakeFunc允许调用者根据Values实现一个有类型的函数。文档的示例部分演示了如何使用MakeFunc构建不同类型的交换函数。
func MakeFunc(typ Type, fn func(args []Value) (results []Value)) Value {
	if typ.Kind() != Func {
		panic("reflect: call of MakeFunc with non-Func type")
	}

	t := typ.common()
	ftyp := (*funcType)(unsafe.Pointer(t))

	// Indirect Go func value (dummy) to obtain
	// actual code address. (A Go func value is a pointer
	// to a C function pointer. https://golang.org/s/go11func.)
	// 间接Go func值(dummy)获取实际代码地址。
	// Go func值是指向C函数指针的指针。https: golang.org/s/go11func。)
	dummy := makeFuncStub
	code := **(**uintptr)(unsafe.Pointer(&dummy))

	// makeFuncImpl contains a stack map for use by the runtime
	// makeFuncImpl包含一个供运行时使用的堆栈映射
	_, _, abi := funcLayout(ftyp, nil)

	impl := &makeFuncImpl{
		makeFuncCtxt: makeFuncCtxt{
			fn:      code,
			stack:   abi.stackPtrs,
			argLen:  abi.stackCallArgsSize,
			regPtrs: abi.inRegPtrs,
		},
		ftyp: ftyp,
		fn:   fn,
	}

	return Value{t, unsafe.Pointer(impl), flag(Func)}
}

// makeFuncStub is an assembly function that is the code half of
// the function returned from MakeFunc. It expects a *callReflectFunc
// as its context register, and its job is to invoke callReflect(ctxt, frame)
// where ctxt is the context register and frame is a pointer to the first
// word in the passed-in argument frame.
// makeFuncStub是一个汇编函数，它是由MakeFunc返回的函数的代码部分。
// 它期望一个*callReflectFunc作为它的上下文寄存器，它的工作是调用callReflect(ctxt, frame)，
// 其中ctxt是上下文寄存器，frame是一个指向传入参数frame中第一个单词的指针。
func makeFuncStub()

// The first 3 words of this type must be kept in sync with
// makeFuncImpl and runtime.reflectMethodValue.
// Any changes should be reflected in all three.
// 该类型的前3个单词必须与makeFuncImpl和runtime.reflectMethodValue保持同步。
// 任何变化都应该反映在这三个方面。
type methodValue struct {
	makeFuncCtxt
	method int
	rcvr   Value
}

// makeMethodValue converts v from the rcvr+method index representation
// of a method value to an actual method func value, which is
// basically the receiver value with a special bit set, into a true
// func value - a value holding an actual func. The output is
// semantically equivalent to the input as far as the user of package
// reflect can tell, but the true func representation can be handled
// by code like Convert and Interface and Assign.
// makeMethodValue将v从rcvr+方法索引表示的方法值转换为一个实际的方法func值，
// 这基本上是一个设置了特殊位的接收方值，转换为一个真正的func值-一个包含实际func的值。
// 就package reflect的用户而言，输出在语义上等同于输入，
// 但真正的func表示可以通过Convert、Interface和Assign这样的代码来处理。
func makeMethodValue(op string, v Value) Value {
	if v.flag&flagMethod == 0 {
		panic("reflect: internal error: invalid use of makeMethodValue")
	}

	// Ignoring the flagMethod bit, v describes the receiver, not the method type.
	// 忽略标记方法位，v描述接收方，而不是方法类型。
	fl := v.flag & (flagRO | flagAddr | flagIndir)
	fl |= flag(v.typ.Kind())
	rcvr := Value{v.typ, v.ptr, fl}

	// v.Type returns the actual type of the method value.
	// type返回方法值的实际类型。
	ftyp := (*funcType)(unsafe.Pointer(v.Type().(*rtype)))

	// Indirect Go func value (dummy) to obtain
	// actual code address. (A Go func value is a pointer
	// to a C function pointer. https://golang.org/s/go11func.)
	// 间接Go func值(dummy)获取实际代码地址。
	// Go func值是指向C函数指针的指针。https: golang.org/s/go11func。
	dummy := methodValueCall
	code := **(**uintptr)(unsafe.Pointer(&dummy))

	// methodValue contains a stack map for use by the runtime
	// methodValue包含一个供运行时使用的堆栈映射
	_, _, abi := funcLayout(ftyp, nil)
	fv := &methodValue{
		makeFuncCtxt: makeFuncCtxt{
			fn:      code,
			stack:   abi.stackPtrs,
			argLen:  abi.stackCallArgsSize,
			regPtrs: abi.inRegPtrs,
		},
		method: int(v.flag) >> flagMethodShift,
		rcvr:   rcvr,
	}

	// Cause panic if method is not appropriate.
	// The panic would still happen during the call if we omit this,
	// but we want Interface() and other operations to fail early.
	// 如果方法不合适，会引起恐慌。
	// 如果忽略此操作，在调用期间仍然会发生恐慌，但我们希望Interface()和其他操作尽早失败。
	methodReceiver(op, fv.rcvr, fv.method)

	return Value{&ftyp.rtype, unsafe.Pointer(fv), v.flag&flagRO | flag(Func)}
}

// methodValueCall is an assembly function that is the code half of
// the function returned from makeMethodValue. It expects a *methodValue
// as its context register, and its job is to invoke callMethod(ctxt, frame)
// where ctxt is the context register and frame is a pointer to the first
// word in the passed-in argument frame.
// methodValueCall是一个汇编函数，它是makeMethodValue返回的函数的代码部分。
// 它期望一个*methodValue作为它的上下文寄存器，它的工作是调用callMethod(ctxt, frame)，
// 其中ctxt是上下文寄存器，frame是一个指向传入参数frame中第一个单词的指针。
func methodValueCall()

// This structure must be kept in sync with runtime.reflectMethodValue.
// Any changes should be reflected in all both.
// 这个结构必须与runtime.reflectMethodValue保持同步。任何变化都应该在两者中反映出来。
type makeFuncCtxt struct {
	fn      uintptr
	stack   *bitVector // ptrmap for both stack args and results 堆栈参数和结果的Ptrmap
	argLen  uintptr    // just args 只是参数
	regPtrs abi.IntArgRegBitmap
}

// moveMakeFuncArgPtrs uses ctxt.regPtrs to copy integer pointer arguments
// in args.Ints to args.Ptrs where the GC can see them.
// moveMakeFuncArgPtrs使用ctxt。 regPtrs来复制参数中的整型指针参数。int参数。在GC能看到的地方。
//
// This is similar to what reflectcallmove does in the runtime, except
// that happens on the return path, whereas this happens on the call path.
// 这类似于reflectcallmove在运行时所做的，只不过它发生在返回路径上，而这发生在调用路径上。
//
// nosplit because pointers are being held in uintptr slots in args, so
// having our stack scanned now could lead to accidentally freeing
// memory.
// Nosplit，因为指针在args中的uintptr插槽中，所以现在扫描堆栈可能会意外地释放内存。
//go:nosplit
func moveMakeFuncArgPtrs(ctxt *makeFuncCtxt, args *abi.RegArgs) {
	for i, arg := range args.Ints {
		// Avoid write barriers! Because our write barrier enqueues what
		// was there before, we might enqueue garbage.
		// 避免写屏障!因为我们的写屏障对之前存在的内容进行排队，所以我们可能会对垃圾进行排队。
		if ctxt.regPtrs.Get(i) {
			*(*uintptr)(unsafe.Pointer(&args.Ptrs[i])) = arg
		} else {
			// We *must* zero this space ourselves because it's defined in
			// assembly code and the GC will scan these pointers. Otherwise,
			// there will be garbage here.
			// 我们*必须*自己将这个空间归零，因为它是在汇编代码中定义的，GC会扫描这些指针。
			// 否则，这里就会有垃圾。
			*(*uintptr)(unsafe.Pointer(&args.Ptrs[i])) = 0
		}
	}
}
