// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate go run genzfunc.go

// Package sort provides primitives for sorting slices and user-defined collections.
package sort

// An implementation of Interface can be sorted by the routines in this package.
// The methods refer to elements of the underlying collection by integer index.
// Interface的实现可以按这个包中的例程排序
// 方法通过整数索引引用基础集合的元素
type Interface interface {
	// Len is the number of elements in the collection.
	// Len是集合中元素的个数
	Len() int

	// Less reports whether the element with index i
	// must sort before the element with index j.
	// Less报告索引为i的元素是否必须排在索引为j的元素之前
	//
	// If both Less(i, j) and Less(j, i) are false,
	// then the elements at index i and j are considered equal.
	// Sort may place equal elements in any order in the final result,
	// while Stable preserves the original input order of equal elements.
	// 如果Less(i, j)和Less(j, i)都为假，那么下标i和j处的元素被认为是相等的
	// Sort可以在最终结果中以任何顺序放置相等元素，而Stable则保留相等元素的原始输入顺序
	//
	// Less must describe a transitive ordering:
	//  - if both Less(i, j) and Less(j, k) are true, then Less(i, k) must be true as well.
	//  - if both Less(i, j) and Less(j, k) are false, then Less(i, k) must be false as well.
	// Less必须描述一个传递排序:-如果Less(i, j)和Less(j, k)都为真，那么Less(i, k)也一定为真
	// 如果Less(i, j)和Less(j, k)都是假的，那么Less(i, k)也一定是假的
	//
	// Note that floating-point comparison (the < operator on float32 or float64 values)
	// is not a transitive ordering when not-a-number (NaN) values are involved.
	// See Float64Slice.Less for a correct implementation for floating-point values.
	// 注意，当涉及非数字(NaN)值时，浮点比较(对float32或float64值的操作符)不是传递排序
	// 看到Float64Slice 而浮点值的正确实现则需要更少
	Less(i, j int) bool

	// Swap swaps the elements with indexes i and j.
	// Swap交换索引为i和j的元素
	Swap(i, j int)
}

// insertionSort sorts data[a:b] using insertion sort.
// insertionSort使用插入排序对数据[a:b]进行排序
func insertionSort(data Interface, a, b int) {
	for i := a + 1; i < b; i++ {
		for j := i; j > a && data.Less(j, j-1); j-- {
			data.Swap(j, j-1)
		}
	}
}

// siftDown implements the heap property on data[lo:hi].
// first is an offset into the array where the root of the heap lies.
// siftDown实现了数据的堆属性 首先是对堆根所在的数组的偏移
func siftDown(data Interface, lo, hi, first int) {
	root := lo
	for {
		child := 2*root + 1
		if child >= hi {
			break
		}
		if child+1 < hi && data.Less(first+child, first+child+1) {
			child++
		}
		if !data.Less(first+root, first+child) {
			return
		}
		data.Swap(first+root, first+child)
		root = child
	}
}

func heapSort(data Interface, a, b int) {
	first := a
	lo := 0
	hi := b - a

	// Build heap with greatest element at top.
	// 构建堆，将最大的元素放在顶部
	for i := (hi - 1) / 2; i >= 0; i-- {
		siftDown(data, i, hi, first)
	}

	// Pop elements, largest first, into end of data.
	// 将最大的元素放到数据的末尾
	for i := hi - 1; i >= 0; i-- {
		data.Swap(first, first+i)
		siftDown(data, lo, i, first)
	}
}

// Quicksort, loosely following Bentley and McIlroy,
// ``Engineering a Sort Function,'' SP&E November 1993.
// 快速排序，大致遵循宾利和麦克罗伊，工程排序函数，SP

// medianOfThree moves the median of the three values data[m0], data[m1], data[m2] into data[m1].
// medianOfThree将三个值数据[m0]、数据[m1]、数据[m2]的中位数移动到数据[m1]中
func medianOfThree(data Interface, m1, m0, m2 int) {
	// sort 3 elements
	if data.Less(m1, m0) {
		data.Swap(m1, m0)
	}
	// data[m0] <= data[m1]
	if data.Less(m2, m1) {
		data.Swap(m2, m1)
		// data[m0] <= data[m2] && data[m1] < data[m2]
		if data.Less(m1, m0) {
			data.Swap(m1, m0)
		}
	}
	// now data[m0] <= data[m1] <= data[m2]
}

func swapRange(data Interface, a, b, n int) {
	for i := 0; i < n; i++ {
		data.Swap(a+i, b+i)
	}
}

func doPivot(data Interface, lo, hi int) (midlo, midhi int) {
	m := int(uint(lo+hi) >> 1) // Written like this to avoid integer overflow.
	if hi-lo > 40 {
		// Tukey's ``Ninther,'' median of three medians of three.
		// 图基的九分之一，“三分之一的中位数
		s := (hi - lo) / 8
		medianOfThree(data, lo, lo+s, lo+2*s)
		medianOfThree(data, m, m-s, m+s)
		medianOfThree(data, hi-1, hi-1-s, hi-1-2*s)
	}
	medianOfThree(data, lo, m, hi-1)

	// Invariants are:
	//	data[lo] = pivot (set up by ChoosePivot)
	//	data[lo < i < a] < pivot
	//	data[a <= i < b] <= pivot
	//	data[b <= i < c] unexamined
	//	data[c <= i < hi-1] > pivot
	//	data[hi-1] >= pivot
	pivot := lo
	a, c := lo+1, hi-1

	for ; a < c && data.Less(a, pivot); a++ {
	}
	b := a
	for {
		for ; b < c && !data.Less(pivot, b); b++ { // data[b] <= pivot
		}
		for ; b < c && data.Less(pivot, c-1); c-- { // data[c-1] > pivot
		}
		if b >= c {
			break
		}
		// data[b] > pivot; data[c-1] <= pivot
		data.Swap(b, c-1)
		b++
		c--
	}
	// If hi-c<3 then there are duplicates (by property of median of nine).
	// Let's be a bit more conservative, and set border to 5.
	// 如果hi-c 3，那么有重复项(根据9的中位数性质)
	// 让我们更保守一点，将border设为5
	protect := hi-c < 5
	if !protect && hi-c < (hi-lo)/4 {
		// Lets test some points for equality to pivot
		// 让我们测试一些点是否相等
		dups := 0
		if !data.Less(pivot, hi-1) { // data[hi-1] = pivot
			data.Swap(c, hi-1)
			c++
			dups++
		}
		if !data.Less(b-1, pivot) { // data[b-1] = pivot
			b--
			dups++
		}
		// m-lo = (hi-lo)/2 > 6
		// b-lo > (hi-lo)*3/4-1 > 8
		// ==> m < b ==> data[m] <= pivot
		if !data.Less(m, pivot) { // data[m] = pivot
			data.Swap(m, b-1)
			b--
			dups++
		}
		// if at least 2 points are equal to pivot, assume skewed distribution
		// 如果至少有2个点与主元相等，假设为偏态分布
		protect = dups > 1
	}
	if protect {
		// Protect against a lot of duplicates
		// Add invariant:
		//	data[a <= i < b] unexamined
		//	data[b <= i < c] = pivot
		for {
			for ; a < b && !data.Less(b-1, pivot); b-- { // data[b] == pivot
			}
			for ; a < b && data.Less(a, pivot); a++ { // data[a] < pivot
			}
			if a >= b {
				break
			}
			// data[a] == pivot; data[b-1] < pivot
			data.Swap(a, b-1)
			a++
			b--
		}
	}
	// Swap pivot into middle
	// 把枢轴移到中间
	data.Swap(pivot, b-1)
	return b - 1, c
}

func quickSort(data Interface, a, b, maxDepth int) {
	for b-a > 12 { // Use ShellSort for slices <= 12 elements
		if maxDepth == 0 {
			heapSort(data, a, b)
			return
		}
		maxDepth--
		mlo, mhi := doPivot(data, a, b)
		// Avoiding recursion on the larger subproblem guarantees
		// a stack depth of at most lg(b-a).
		// 避免对更大的子问题进行递归可以保证堆栈深度最多为lg(b-a)
		if mlo-a < b-mhi {
			quickSort(data, a, mlo, maxDepth)
			a = mhi // i.e., quickSort(data, mhi, b)
		} else {
			quickSort(data, mhi, b, maxDepth)
			b = mlo // i.e., quickSort(data, a, mlo)
		}
	}
	if b-a > 1 {
		// Do ShellSort pass with gap 6
		// It could be written in this simplified form cause b-a <= 12
		// 它可以写成这种简化的形式，因为b-a = 12
		for i := a + 6; i < b; i++ {
			if data.Less(i, i-6) {
				data.Swap(i, i-6)
			}
		}
		insertionSort(data, a, b)
	}
}

// Sort sorts data.
// It makes one call to data.Len to determine n and O(n*log(n)) calls to
// data.Less and data.Swap. The sort is not guaranteed to be stable.
// 那种类型的数据 它对数据进行一次调用
// Len来确定对数据的n和O(n*log(n))调用 越来越data.Swap
// 排序不能保证是稳定的
func Sort(data Interface) {
	n := data.Len()
	quickSort(data, 0, n, maxDepth(n))
}

// maxDepth returns a threshold at which quicksort should switch
// to heapsort. It returns 2*ceil(lg(n+1)).
// maxDepth返回快速排序切换到堆排序的阈值
// 它返回2 *装天花板(lg (n + 1))
func maxDepth(n int) int {
	var depth int
	for i := n; i > 0; i >>= 1 {
		depth++
	}
	return depth * 2
}

// lessSwap is a pair of Less and Swap function for use with the
// auto-generated func-optimized variant of sort.go in
// zfuncversion.go.
// lessSwap是一对Less和Swap函数，可与自动生成的func优化的排序变体一起使用
// zfuncversion.go
type lessSwap struct {
	Less func(i, j int) bool
	Swap func(i, j int)
}

type reverse struct {
	// This embedded Interface permits Reverse to use the methods of
	// another Interface implementation.
	// 这个嵌入式接口允许Reverse使用另一个接口实现的方法
	Interface
}

// Less returns the opposite of the embedded implementation's Less method.
// Less返回与嵌入实现的Less方法相反的方法
func (r reverse) Less(i, j int) bool {
	return r.Interface.Less(j, i)
}

// Reverse returns the reverse order for data.
// Reverse返回数据的相反顺序
func Reverse(data Interface) Interface {
	return &reverse{data}
}

// IsSorted reports whether data is sorted.
// IsSorted用于报告数据是否被排序
func IsSorted(data Interface) bool {
	n := data.Len()
	for i := n - 1; i > 0; i-- {
		if data.Less(i, i-1) {
			return false
		}
	}
	return true
}

// Convenience types for common cases
// 常见情况下的方便类型

// IntSlice attaches the methods of Interface to []int, sorting in increasing order.
// IntSlice将Interface的方法附加到[]int，按递增顺序排序
type IntSlice []int

func (x IntSlice) Len() int           { return len(x) }
func (x IntSlice) Less(i, j int) bool { return x[i] < x[j] }
func (x IntSlice) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

// Sort is a convenience method: x.Sort() calls Sort(x).
// Sort是一个方便的方法:x.Sort()调用Sort(x)
func (x IntSlice) Sort() { Sort(x) }

// Float64Slice implements Interface for a []float64, sorting in increasing order,
// with not-a-number (NaN) values ordered before other values.
// Float64Slice实现了一个[]float64的接口，按递增顺序排序，non -a-number (NaN)值排在其他值之前
type Float64Slice []float64

func (x Float64Slice) Len() int { return len(x) }

// Less reports whether x[i] should be ordered before x[j], as required by the sort Interface.
// Note that floating-point comparison by itself is not a transitive relation: it does not
// report a consistent ordering for not-a-number (NaN) values.
// This implementation of Less places NaN values before any others, by using:
// 根据sort Interface的要求，较少报告x[i]是否应该排在x[j]之前
// 请注意，浮点比较本身不是传递关系:它不会报告非数字(NaN)值的一致排序
// Less的实现将NaN值放在其他值之前，方法如下:
//
//	x[i] < x[j] || (math.IsNaN(x[i]) && !math.IsNaN(x[j]))
//
func (x Float64Slice) Less(i, j int) bool { return x[i] < x[j] || (isNaN(x[i]) && !isNaN(x[j])) }
func (x Float64Slice) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

// isNaN is a copy of math.IsNaN to avoid a dependency on the math package.
// isNaN是数学的副本 IsNaN以避免对数学包的依赖
func isNaN(f float64) bool {
	return f != f
}

// Sort is a convenience method: x.Sort() calls Sort(x).
// Sort是一个方便的方法:x.Sort()调用Sort(x)
func (x Float64Slice) Sort() { Sort(x) }

// StringSlice attaches the methods of Interface to []string, sorting in increasing order.
// StringSlice将Interface的方法附加到[]string，按递增顺序排序
type StringSlice []string

func (x StringSlice) Len() int           { return len(x) }
func (x StringSlice) Less(i, j int) bool { return x[i] < x[j] }
func (x StringSlice) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

// Sort is a convenience method: x.Sort() calls Sort(x).
// Sort是一个方便的方法:x.Sort()调用Sort(x)
func (x StringSlice) Sort() { Sort(x) }

// Convenience wrappers for common cases

// Ints sorts a slice of ints in increasing order.
// 整型对象按递增顺序对整型对象片进行排序
func Ints(x []int) { Sort(IntSlice(x)) }

// Float64s sorts a slice of float64s in increasing order.
// Not-a-number (NaN) values are ordered before other values.
// Float64s按递增顺序对Float64s切片进行排序
// Not-a-number (NaN)值排在其他值之前
func Float64s(x []float64) { Sort(Float64Slice(x)) }

// Strings sorts a slice of strings in increasing order.
// 字符串按递增顺序对字符串片进行排序
func Strings(x []string) { Sort(StringSlice(x)) }

// IntsAreSorted reports whether the slice x is sorted in increasing order.
// IntsAreSorted报告切片x是否按递增顺序排序
func IntsAreSorted(x []int) bool { return IsSorted(IntSlice(x)) }

// Float64sAreSorted reports whether the slice x is sorted in increasing order,
// with not-a-number (NaN) values before any other values.
// Float64sAreSorted报告x切片是否按递增顺序排序，非a-number (NaN)值是否排在其他值之前
func Float64sAreSorted(x []float64) bool { return IsSorted(Float64Slice(x)) }

// StringsAreSorted reports whether the slice x is sorted in increasing order.
// StringsAreSorted报告切片x是否按递增顺序排序
func StringsAreSorted(x []string) bool { return IsSorted(StringSlice(x)) }

// Notes on stable sorting:
// The used algorithms are simple and provable correct on all input and use
// only logarithmic additional stack space. They perform well if compared
// experimentally to other stable in-place sorting algorithms.
// 关于稳定排序的注意事项:所使用的算法是简单的，在所有输入上都是正确的，并且只使用对数级的额外堆栈空间
// 与其他稳定的就地排序算法相比，它们在实验中表现良好
//
// Remarks on other algorithms evaluated:
// 对其他算法的评价:
//  - GCC's 4.6.3 stable_sort with merge_without_buffer from libstdc++:
//    Not faster.
// GCC的4.6.3 stable_sort与libstdc++的merge_without_buffer:没有更快
//  - GCC's __rotate for block rotations: Not faster.
// GCC的__rotate块旋转:没有更快
//  - "Practical in-place mergesort" from  Jyrki Katajainen, Tomi A. Pasanen
//    and Jukka Teuhola; Nordic Journal of Computing 3,1 (1996), 27-40:
//    The given algorithms are in-place, number of Swap and Assignments
//    grow as n log n but the algorithm is not stable.
// -来自Jyrki Katajainen, Tomi A. Pasanen和Jukka Teuhola北欧计算期刊3,1(1996)的“实用的原位归并排序”:给定的算法是原位的，交换和分配的数量随着n log n的增长而增长，但算法不稳定
//  - "Fast Stable In-Place Sorting with O(n) Data Moves" J.I. Munro and
//    V. Raman in Algorithmica (1996) 16, 115-160:
//    This algorithm either needs additional 2n bits or works only if there
//    are enough different elements available to encode some permutations
//    which have to be undone later (so not stable on any input).
// -《基于O(n)数据移动的快速稳定原位排序》j.i Munro和V. Raman在Algorithmica(1996) 16,115 -160中:该算法要么需要额外的2n位，要么只有在有足够多的不同元素可用来编码某些排列时才有效(所以在任何输入上都不稳定)
//  - All the optimal in-place sorting/merging algorithms I found are either
//    unstable or rely on enough different elements in each step to encode the
//    performed block rearrangements. See also "In-Place Merging Algorithms",
//    Denham Coates-Evely, Department of Computer Science, Kings College,
//    January 2004 and the references in there.
// -我发现的所有最佳就地排序/合并算法要么不稳定，要么依赖于每个步骤中足够多的不同元素来编码执行的块重排
// 参见“就地合并算法”，Denham coats - evely，国王学院计算机科学系，2004年1月，以及其中的参考文献
//  - Often "optimal" algorithms are optimal in the number of assignments
//    but Interface has only Swap as operation.
// -通常“最优”算法在赋值数量上是最优的，但Interface只有Swap操作

// Stable sorts data while keeping the original order of equal elements.
// Stable对数据进行排序，同时保持相等元素的原始顺序
//
// It makes one call to data.Len to determine n, O(n*log(n)) calls to
// data.Less and O(n*log(n)*log(n)) calls to data.Swap.
// 它对数据进行一次调用 Len来确定n, O(n*log(n))对数据的调用
// Less and O(n*log(n)*log(n))调用data.Swap
func Stable(data Interface) {
	stable(data, data.Len())
}

func stable(data Interface, n int) {
	blockSize := 20 // must be > 0
	a, b := 0, blockSize
	for b <= n {
		insertionSort(data, a, b)
		a = b
		b += blockSize
	}
	insertionSort(data, a, n)

	for blockSize < n {
		a, b = 0, 2*blockSize
		for b <= n {
			symMerge(data, a, a+blockSize, b)
			a = b
			b += 2 * blockSize
		}
		if m := a + blockSize; m < n {
			symMerge(data, a, m, n)
		}
		blockSize *= 2
	}
}

// symMerge merges the two sorted subsequences data[a:m] and data[m:b] using
// the SymMerge algorithm from Pok-Son Kim and Arne Kutzner, "Stable Minimum
// Storage Merging by Symmetric Comparisons", in Susanne Albers and Tomasz
// Radzik, editors, Algorithms - ESA 2004, volume 3221 of Lecture Notes in
// Computer Science, pages 714-723. Springer, 2004.
// symMerge合并两个排序子序列数据[a:m]和数据[m:b]使用来自Pok-Son Kim和Arne Kutzner的symMerge算法，“对称比较的稳定最小存储合并”，在Susanne Albers和Tomasz Radzik，编辑，算法- ESA 2004，计算机科学讲义第3221卷，714-723页
// 施普林格,2004年
//
// Let M = m-a and N = b-n. Wolog M < N.
// The recursion depth is bound by ceil(log(N+M)).
// The algorithm needs O(M*log(N/M + 1)) calls to data.Less.
// The algorithm needs O((M+N)*log(M)) calls to data.Swap.
// 设M = M -a, N = b-n Wolog M N递归深度由ceil(log(N+M))限定
// 该算法需要O(M*log(N/M + 1))次调用数据 该算法需要O((M+N)*log(M))调用data.Swap
//
// The paper gives O((M+N)*log(M)) as the number of assignments assuming a
// rotation algorithm which uses O(M+N+gcd(M+N)) assignments. The argumentation
// in the paper carries through for Swap operations, especially as the block
// swapping rotate uses only O(M+N) Swaps.
// 本文给出了O(M+N)*log(M))作为赋值数，假设一个轮换算法使用O(M+N+gcd(M+N))赋值
// 本文的论证适用于Swap操作，特别是块交换旋转只使用O(M+N)个Swap
//
// symMerge assumes non-degenerate arguments: a < m && m < b.
// symMerge采用非退化参数:一个m
// Having the caller check this condition eliminates many leaf recursion calls,
// which improves performance.
// 让调用者检查这个条件可以消除许多叶递归调用，从而提高性能
func symMerge(data Interface, a, m, b int) {
	// Avoid unnecessary recursions of symMerge
	// by direct insertion of data[a] into data[m:b]
	// if data[a:m] only contains one element.
	// 如果数据[a:m]只包含一个元素，通过直接将数据[a]插入到数据[m:b]中来避免symMerge的不必要递归
	if m-a == 1 {
		// Use binary search to find the lowest index i
		// such that data[i] >= data[a] for m <= i < b.
		// Exit the search loop with i == b in case no such index exists.
		// 使用二进制搜索查找m = i b中data[i] = data[a]的最小索引i，如果不存在这样的索引，则使用i == b退出搜索循环
		i := m
		j := b
		for i < j {
			h := int(uint(i+j) >> 1)
			if data.Less(h, a) {
				i = h + 1
			} else {
				j = h
			}
		}
		// Swap values until data[a] reaches the position before i.
		// 交换值，直到数据[a]到达i之前的位置
		for k := a; k < i-1; k++ {
			data.Swap(k, k+1)
		}
		return
	}

	// Avoid unnecessary recursions of symMerge
	// by direct insertion of data[m] into data[a:m]
	// if data[m:b] only contains one element.
	// 如果数据[m:b]只包含一个元素，通过直接将数据[m]插入到数据[a:m]中来避免symMerge的不必要递归
	if b-m == 1 {
		// Use binary search to find the lowest index i
		// such that data[i] > data[m] for a <= i < m.
		// Exit the search loop with i == m in case no such index exists.
		// 当a = i m时，使用二进制搜索查找data[i] data[m]的最小索引i
		// 如果不存在该索引，则使用i == m退出搜索循环
		i := a
		j := m
		for i < j {
			h := int(uint(i+j) >> 1)
			if !data.Less(m, h) {
				i = h + 1
			} else {
				j = h
			}
		}
		// Swap values until data[m] reaches the position i.
		// 交换值，直到数据[m]到达位置i
		for k := m; k > i; k-- {
			data.Swap(k, k-1)
		}
		return
	}

	mid := int(uint(a+b) >> 1)
	n := mid + m
	var start, r int
	if m > mid {
		start = n - b
		r = mid
	} else {
		start = a
		r = m
	}
	p := n - 1

	for start < r {
		c := int(uint(start+r) >> 1)
		if !data.Less(p-c, c) {
			start = c + 1
		} else {
			r = c
		}
	}

	end := n - start
	if start < m && m < end {
		rotate(data, start, m, end)
	}
	if a < start && start < mid {
		symMerge(data, a, start, mid)
	}
	if mid < end && end < b {
		symMerge(data, mid, end, b)
	}
}

// rotate rotates two consecutive blocks u = data[a:m] and v = data[m:b] in data:
// Data of the form 'x u v y' is changed to 'x v u y'.
// rotate performs at most b-a many calls to data.Swap,
// and it assumes non-degenerate arguments: a < m && m < b.
// rotate旋转两个连续的块u = data[a:m]和v = data[m:b]，其中data:将形式为' x u v y '的数据更改为' x v u y '
// Rotate最多执行b-a多次对数据的调用 交换，它假设非退化参数:a m
func rotate(data Interface, a, m, b int) {
	i := m - a
	j := b - m

	for i != j {
		if i > j {
			swapRange(data, m-i, m, j)
			i -= j
		} else {
			swapRange(data, m-i, m+j-i, i)
			j -= i
		}
	}
	// i == j
	swapRange(data, m-i, m, i)
}

/*
Complexity of Stable Sorting
// 稳定排序的复杂性


Complexity of block swapping rotation
// 块交换旋转的复杂性

Each Swap puts one new element into its correct, final position.
Elements which reach their final position are no longer moved.
Thus block swapping rotation needs |u|+|v| calls to Swaps.
This is best possible as each element might need a move.
// 每次Swap都将一个新元素放入其正确的最终位置
// 到达最终位置的元素不再移动
// ？？？
// 这是最好的，因为每个元素都可能需要移动

Pay attention when comparing to other optimal algorithms which
typically count the number of assignments instead of swaps:
E.g. the optimal algorithm of Dudzinski and Dydek for in-place
rotations uses O(u + v + gcd(u,v)) assignments which is
better than our O(3 * (u+v)) as gcd(u,v) <= u.
// 当与其他最优算法进行比较时要注意，这些算法通常会计算赋值而不是交换的数量:例如，Dudzinski和Dydek的原地旋转最优算法使用O(u + v + gcd(u,v))赋值，这比我们的O(3 example_interface_test
// example_keys_test去 example_multi_test去 example_search_test去
// example_test去 example_wrapper_test去 export_test去
// genzfunc去 去搜索 search_test去 片 slice_go113去
// slice_go14去 slice_go18去 去排序 sort_test去 zfuncversion去
// Go (u+v) = gcd(u,v) = u


Stable sorting by SymMerge and BlockSwap rotations
// 通过SymMerge和BlockSwap旋转稳定排序

SymMerg complexity for same size input M = N:
Calls to Less:  O(M*log(N/M+1)) = O(N*log(2)) = O(N)
Calls to Swap:  O((M+N)*log(M)) = O(2*N*log(N)) = O(N*log(N))
// 相同大小输入的SymMerg复杂度M = N: call to Less: O(M*log(N/M+1)) = O(N*log(2)) = O(N*log(N)) = O(N*log(N))

(The following argument does not fuzz over a missing -1 or
other stuff which does not impact the final result).
// (下面的论证不会因为缺少-1或其他不影响最终结果的东西而模糊)

Let n = data.Len(). Assume n = 2^k.
// 让n = data.Len() 假设n = 2^k

Plain merge sort performs log(n) = k iterations.
On iteration i the algorithm merges 2^(k-i) blocks, each of size 2^i.
// 普通的归并排序执行log(n) = k次迭代 在第i次迭代中，算法合并2^(k-i)块，每个块的大小为2^i

Thus iteration i of merge sort performs:
// 因此，归并排序的第i次迭代执行:
Calls to Less  O(2^(k-i) * 2^i) = O(2^k) = O(2^log(n)) = O(n)
Calls to Swap  O(2^(k-i) * 2^i * log(2^i)) = O(2^k * i) = O(n*i)

In total k = log(n) iterations are performed; so in total:
// 总共进行了k = log(n)次迭代，因此总共:
Calls to Less O(log(n) * n)
Calls to Swap O(n + 2*n + 3*n + ... + (k-1)*n + k*n)
   = O((k/2) * k * n) = O(n * k^2) = O(n * log^2(n))


Above results should generalize to arbitrary n = 2^k + p
and should not be influenced by the initial insertion sort phase:
Insertion sort is O(n^2) on Swap and Less, thus O(bs^2) per block of
size bs at n/bs blocks:  O(bs*n) Swaps and Less during insertion sort.
// 上述结果可推广到任意n = 2^k + p，且不受初始插入排序阶段的影响:插入排序在Swap and Less上为O(n^2)，因此在n/bs块上，每个大小为bs的块为O(bs^2):插入排序时为O(bs*n) Swap and Less
// 归并排序迭代从i = log(bs)开始 t = log(bs)常数:
Merge sort iterations start at i = log(bs). With t = log(bs) constant:
Calls to Less O((log(n)-t) * n + bs*n) = O(log(n)*n + (bs-t)*n)
   = O(n * log(n))
Calls to Swap O(n * log^2(n) - (t^2+t)/2*n) = O(n * log^2(n))

*/
