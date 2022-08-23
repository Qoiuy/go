// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package sort

// Slice sorts the slice x given the provided less function.
// It panics if x is not a slice.
// Slice根据所提供的less函数对Slice x进行排序
// 如果x不是切片，它就会惊慌
//
// The sort is not guaranteed to be stable: equal elements
// may be reversed from their original order.
// For a stable sort, use SliceStable.
// 排序不保证是稳定的:相等的元素可以从它们原来的顺序颠倒过来
// 对于稳定排序，请使用SliceStable
//
// The less function must satisfy the same requirements as
// the Interface type's Less method.
// less函数必须满足与接口类型的less方法相同的要求
func Slice(x interface{}, less func(i, j int) bool) {
	rv := reflectValueOf(x)
	swap := reflectSwapper(x)
	length := rv.Len()
	quickSort_func(lessSwap{less, swap}, 0, length, maxDepth(length))
}

// SliceStable sorts the slice x using the provided less
// function, keeping equal elements in their original order.
// It panics if x is not a slice.
// SliceStable使用所提供的less函数对切片x进行排序，保持相同元素的原始顺序
// 如果x不是切片，它就会惊慌
//
// The less function must satisfy the same requirements as
// the Interface type's Less method.
// less函数必须满足与接口类型的less方法相同的要求
func SliceStable(x interface{}, less func(i, j int) bool) {
	rv := reflectValueOf(x)
	swap := reflectSwapper(x)
	stable_func(lessSwap{less, swap}, rv.Len())
}

// SliceIsSorted reports whether the slice x is sorted according to the provided less function.
// It panics if x is not a slice.
// SliceIsSorted报告切片x是否根据所提供的less函数进行排序
// 如果x不是切片，它就会惊慌
func SliceIsSorted(x interface{}, less func(i, j int) bool) bool {
	rv := reflectValueOf(x)
	n := rv.Len()
	for i := n - 1; i > 0; i-- {
		if less(i, i-1) {
			return false
		}
	}
	return true
}
