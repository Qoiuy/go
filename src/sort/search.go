// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This file implements binary search.

package sort

// Search uses binary search to find and return the smallest index i
// in [0, n) at which f(i) is true, assuming that on the range [0, n),
// f(i) == true implies f(i+1) == true. That is, Search requires that
// f is false for some (possibly empty) prefix of the input range [0, n)
// and then true for the (possibly empty) remainder; Search returns
// the first true index. If there is no such index, Search returns n.
// (Note that the "not found" return value is not -1 as in, for instance,
// strings.Index.)

// 搜索使用二分搜索来查找并返回在[0,n)中f(i)为真的最小索引i，假设在[0,n)范围内，f(i) ==真意味着f(i+1) ==真
// 也就是说，搜索要求f对于输入范围[0,n]的某些前缀(可能为空)为假，然后对于(可能为空)余数为真
// 搜索返回第一个真索引 如果没有这样的索引，Search返回n
// (注意' not found '返回值不是-1，例如，在string . index中
// )
// Search calls f(i) only for i in the range [0, n).
// 搜索只对i在[0,n]范围内调用f(i)
//
// A common use of Search is to find the index i for a value x in
// a sorted, indexable data structure such as an array or slice.
// In this case, the argument f, typically a closure, captures the value
// to be searched for, and how the data structure is indexed and
// ordered.
// Search的一个常见用途是在一个排序的、可索引的数据结构(如数组或切片)中查找值x的索引i
// 在这种情况下，参数f(通常是一个闭包)捕获要搜索的值以及数据结构的索引和排序方式
//
// For instance, given a slice data sorted in ascending order,
// the call Search(len(data), func(i int) bool { return data[i] >= 23 })
// returns the smallest index i such that data[i] >= 23. If the caller
// wants to find whether 23 is in the slice, it must test data[i] == 23
// separately.
// 例如，给定一个按升序排序的切片数据，调用Search(len(data)， func(i int) bool {return data[i] = 23})返回最小的索引i，使data[i] = 23
// 如果调用者想要找到23是否在切片中，它必须单独测试data[i] == 23
//
// Searching data sorted in descending order would use the <=
// operator instead of the >= operator.
// 搜索降序排序的数据将使用=操作符而不是=操作符
//
// To complete the example above, the following code tries to find the value
// x in an integer slice data sorted in ascending order:
// 为了完成上面的例子，下面的代码尝试在一个按升序排序的整数切片数据中找到值x:
//
//	x := 23
//	i := sort.Search(len(data), func(i int) bool { return data[i] >= x })
//	if i < len(data) && data[i] == x {
//		// x is present at data[i]
//	} else {
//		// x is not present in data,
//		// but i is the index where it would be inserted.
//	}
//
// As a more whimsical example, this program guesses your number:
// 作为一个更古怪的例子，这个程序猜测你的数字:
//
//	func GuessingGame() {
//		var s string
//		fmt.Printf("Pick an integer from 0 to 100.\n")
//		answer := sort.Search(100, func(i int) bool {
//			fmt.Printf("Is your number <= %d? ", i)
//			fmt.Scanf("%s", &s)
//			return s != "" && s[0] == 'y'
//		})
//		fmt.Printf("Your number is %d.\n", answer)
//	}
//
func Search(n int, f func(int) bool) int {
	// Define f(-1) == false and f(n) == true.
	// Invariant: f(i-1) == false, f(j) == true.
	i, j := 0, n
	for i < j {
		h := int(uint(i+j) >> 1) // avoid overflow when computing h
		// i ≤ h < j
		if !f(h) {
			i = h + 1 // preserves f(i-1) == false
		} else {
			j = h // preserves f(j) == true
		}
	}
	// i == j, f(i-1) == false, and f(j) (= f(i)) == true  =>  answer is i.
	return i
}

// Convenience wrappers for common cases.
// 普通箱子的方便包装

// SearchInts searches for x in a sorted slice of ints and returns the index
// as specified by Search. The return value is the index to insert x if x is
// not present (it could be len(a)).
// The slice must be sorted in ascending order.
// searchchints在一个已排序的整数片中搜索x，并返回Search指定的索引
// 返回值是插入x的索引，如果x不存在(可以是len(a))
// 切片必须按升序排序
//
func SearchInts(a []int, x int) int {
	return Search(len(a), func(i int) bool { return a[i] >= x })
}

// SearchFloat64s searches for x in a sorted slice of float64s and returns the index
// as specified by Search. The return value is the index to insert x if x is not
// present (it could be len(a)).
// The slice must be sorted in ascending order.
// SearchFloat64s在一个由float64s组成的排序切片中搜索x，并返回Search指定的索引
// 返回值是插入x的索引，如果x不存在(可以是len(a))
// 切片必须按升序排序
//
func SearchFloat64s(a []float64, x float64) int {
	return Search(len(a), func(i int) bool { return a[i] >= x })
}

// SearchStrings searches for x in a sorted slice of strings and returns the index
// as specified by Search. The return value is the index to insert x if x is not
// present (it could be len(a)).
// The slice must be sorted in ascending order.
// SearchStrings在一个排序的字符串切片中搜索x，并返回Search指定的索引
// 返回值是插入x的索引，如果x不存在(可以是len(a))
// 切片必须按升序排序
//
func SearchStrings(a []string, x string) int {
	return Search(len(a), func(i int) bool { return a[i] >= x })
}

// Search returns the result of applying SearchInts to the receiver and x.
// Search返回将searchchints应用到接收者和x的结果
func (p IntSlice) Search(x int) int { return SearchInts(p, x) }

// Search returns the result of applying SearchFloat64s to the receiver and x.
// Search返回将SearchFloat64s应用到接收者和x的结果
func (p Float64Slice) Search(x float64) int { return SearchFloat64s(p, x) }

// Search returns the result of applying SearchStrings to the receiver and x.
// Search返回将SearchStrings应用到接收者的结果
func (p StringSlice) Search(x string) int { return SearchStrings(p, x) }
