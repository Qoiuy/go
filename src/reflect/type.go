// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package reflect implements run-time reflection, allowing a program to
// manipulate objects with arbitrary types. The typical use is to take a value
// with static type interface{} and extract its dynamic type information by
// calling TypeOf, which returns a Type.
// 包反射实现运行时反射，允许程序操作具有任意类型的对象
// 典型的用法是使用静态类型接口{}获取一个值，并通过调用TypeOf提取其动态类型信息，它返回一个type
//
// A call to ValueOf returns a Value representing the run-time data.
// Zero takes a Type and returns a Value representing a zero value
// for that type.
// 对ValueOf的调用将返回一个表示运行时数据的Value
// Zero接受一个Type并返回一个Value，表示该类型的0值
//
// See "The Laws of Reflection" for an introduction to reflection in Go:
// https://golang.org/doc/articles/laws_of_reflection.html
package reflect

import (
	"internal/unsafeheader"
	"strconv"
	"sync"
	"unicode"
	"unicode/utf8"
	"unsafe"
)

// Type is the representation of a Go type.
// 类型是围棋类型的表示
//
// Not all methods apply to all kinds of types. Restrictions,
// if any, are noted in the documentation for each method.
// Use the Kind method to find out the kind of type before
// calling kind-specific methods. Calling a method
// inappropriate to the kind of type causes a run-time panic.
// 并非所有方法都适用于所有类型 每种方法的文档中都注明了限制条件(如果有的话)
// 在调用特定于类型的方法之前，使用Kind方法找出类型的类型
// 调用不适合类型类型的方法会导致运行时混乱
//
// Type values are comparable, such as with the == operator,
// so they can be used as map keys.
// Two Type values are equal if they represent identical types.
// 类型值具有可比性，例如==操作符，因此可以将它们用作映射键
// 如果两个Type值表示相同的类型，则它们是相等的
type Type interface {
	// Methods applicable to all types.
	// 适用于所有类型的方法

	// Align returns the alignment in bytes of a value of
	// this type when allocated in memory.
	// 当在内存中分配时，Align返回此类型值的字节对齐方式
	Align() int

	// FieldAlign returns the alignment in bytes of a value of
	// this type when used as a field in a struct.
	// 当在结构中用作字段时，FieldAlign返回此类型值的字节对齐
	FieldAlign() int

	// Method returns the i'th method in the type's method set.
	// It panics if i is not in the range [0, NumMethod()).
	// 方法返回类型的方法集中的第i个方法 如果i不在范围[0,NumMethod()))，它会惊慌
	//
	// For a non-interface type T or *T, the returned Method's Type and Func
	// fields describe a function whose first argument is the receiver,
	// and only exported methods are accessible.
	// 对于非接口类型T或*T，返回的Method的type和Func字段描述了一个函数，它的第一个参数是接收方，并且只有导出的方法是可访问的
	//
	// For an interface type, the returned Method's Type field gives the
	// method signature, without a receiver, and the Func field is nil.
	// 对于接口类型，返回的Method的type字段给出方法签名，没有接收方，Func字段为nil
	//
	// Methods are sorted in lexicographic order.
	// 方法按字典顺序排序
	Method(int) Method

	// MethodByName returns the method with that name in the type's
	// method set and a boolean indicating if the method was found.
	// MethodByName返回类型的方法集中带有该名称的方法，并返回一个指示是否找到该方法的布尔值
	//
	// For a non-interface type T or *T, the returned Method's Type and Func
	// fields describe a function whose first argument is the receiver.
	// 对于非接口类型T或*T，返回的Method的type和Func字段描述了一个函数，其第一个参数是接收方
	//
	// For an interface type, the returned Method's Type field gives the
	// method signature, without a receiver, and the Func field is nil.
	// 对于接口类型，返回的Method的type字段给出方法签名，没有接收方，Func字段为nil
	MethodByName(string) (Method, bool)

	// NumMethod returns the number of methods accessible using Method.
	// NumMethod返回使用Method可访问的方法的数量
	//
	// Note that NumMethod counts unexported methods only for interface types.
	// 注意NumMethod只对接口类型计算未导出的方法
	NumMethod() int

	// Name returns the type's name within its package for a defined type.
	// For other (non-defined) types it returns the empty string.
	// Name返回包中已定义类型的类型名称 对于其他(未定义的)类型，它返回空字符串
	Name() string

	// PkgPath returns a defined type's package path, that is, the import path
	// that uniquely identifies the package, such as "encoding/base64".
	// If the type was predeclared (string, error) or not defined (*T, struct{},
	// []int, or A where A is an alias for a non-defined type), the package path
	// will be the empty string.
	// PkgPath返回一个已定义类型的包路径，即唯一标识包的导入路径，如' encoding/base64 '
	// 如果类型预先声明(string, error)或未定义(*T, struct{}， []int，或A，其中A是一个未定义类型的别名)，包路径将是空字符串
	PkgPath() string

	// Size returns the number of bytes needed to store
	// a value of the given type; it is analogous to unsafe.Sizeof.
	// Size返回存储类似于unsafe.Sizeof的给定类型值所需的字节数
	Size() uintptr

	// String returns a string representation of the type.
	// The string representation may use shortened package names
	// (e.g., base64 instead of "encoding/base64") and is not
	// guaranteed to be unique among types. To test for type identity,
	// compare the Types directly.
	// 返回该类型的字符串表示形式 字符串表示可以使用缩短的包名(例如，base64而不是' encoding/base64 ')，并且不保证类型之间是唯一的
	// 要测试类型标识，直接比较类型
	String() string

	// Kind returns the specific kind of this type.
	// Kind返回该类型的特定类型
	Kind() Kind

	// Implements reports whether the type implements the interface type u.
	// Implements报告是否实现接口类型u
	Implements(u Type) bool

	// AssignableTo reports whether a value of the type is assignable to type u.
	// AssignableTo报告该类型的值是否可赋值给类型u
	AssignableTo(u Type) bool

	// ConvertibleTo reports whether a value of the type is convertible to type u.
	// Even if ConvertibleTo returns true, the conversion may still panic.
	// For example, a slice of type []T is convertible to *[N]T,
	// but the conversion will panic if its length is less than N.
	// ConvertibleTo报告该类型的值是否可转换为u类型
	// 即使ConvertibleTo返回true，转换仍然可能发生
	// 例如，类型为[]T的切片可以转换为*[N]T，但是如果它的长度小于N，转换就会中断
	ConvertibleTo(u Type) bool

	// Comparable reports whether values of this type are comparable.
	// Even if Comparable returns true, the comparison may still panic.
	// For example, values of interface type are comparable,
	// but the comparison will panic if their dynamic type is not comparable.
	// 可比报告该类型的值是否具有可比性 即使可比性回报率是真实的，这种比较仍可能引起恐慌
	// 例如，接口类型的值是可比较的，但是如果它们的动态类型不具有可比性，则比较就会出现混乱
	Comparable() bool

	// Methods applicable only to some types, depending on Kind.
	// The methods allowed for each kind are:
	// 方法仅适用于某些类型，具体取决于Kind
	// 每一种允许的方法是:
	//
	//	Int*, Uint*, Float*, Complex*: Bits
	//	Array: Elem, Len
	//	Chan: ChanDir, Elem
	//	Func: In, NumIn, Out, NumOut, IsVariadic.
	//	Map: Key, Elem
	//	Ptr: Elem
	//	Slice: Elem
	//	Struct: Field, FieldByIndex, FieldByName, FieldByNameFunc, NumField

	// Bits returns the size of the type in bits.
	// It panics if the type's Kind is not one of the
	// sized or unsized Int, Uint, Float, or Complex kinds.
	// Bits返回以位为单位的类型的大小 如果类型的类型不是有大小或无大小的Int、Uint、Float或Complex类型之一，则会惊慌
	Bits() int

	// ChanDir returns a channel type's direction.
	// It panics if the type's Kind is not Chan.
	// ChanDir返回通道类型的方向 如果这类型的仁慈不是陈，它会恐慌
	ChanDir() ChanDir

	// IsVariadic reports whether a function type's final input parameter
	// is a "..." parameter. If so, t.In(t.NumIn() - 1) returns the parameter's
	// implicit actual type []T.
	// IsVariadic报告一个函数类型的最终输入参数是否为'…的参数
	// 如果是，T. in (T. numin() - 1)返回参数的隐式实际类型[]T
	//
	// For concreteness, if t represents func(x int, y ... float64), then
	// 具体来说，如果t表示func(x int, y…float64),然后
	//
	//	t.NumIn() == 2
	//	t.In(0) is the reflect.Type for "int"
	//	t.In(1) is the reflect.Type for "[]float64"
	//	t.IsVariadic() == true
	//
	// IsVariadic panics if the type's Kind is not Func.
	// 如果类型的种类不是Func，则IsVariadic panic
	IsVariadic() bool

	// Elem returns a type's element type.
	// It panics if the type's Kind is not Array, Chan, Map, Ptr, or Slice.
	// Elem返回类型的元素类型 如果类型的种类不是Array, Chan, Map, Ptr或Slice，它会恐慌
	Elem() Type

	// Field returns a struct type's i'th field.
	// It panics if the type's Kind is not Struct.
	// It panics if i is not in the range [0, NumField()).
	// 字段返回一个结构类型的第i个字段 如果类型的类型不是结构体，它会恐慌
	// 如果i不在范围[0,NumField()))，它会惊慌
	Field(i int) StructField

	// FieldByIndex returns the nested field corresponding
	// to the index sequence. It is equivalent to calling Field
	// successively for each index i.
	// It panics if the type's Kind is not Struct.
	// FieldByIndex返回与索引序列相对应的嵌套字段
	// 它相当于为每个索引i依次调用Field 如果类型的Kind不是Struct，它会惊慌
	FieldByIndex(index []int) StructField

	// FieldByName returns the struct field with the given name
	// and a boolean indicating if the field was found.
	// FieldByName返回具有给定名称的结构字段和一个指示是否找到该字段的布尔值
	FieldByName(name string) (StructField, bool)

	// FieldByNameFunc returns the struct field with a name
	// that satisfies the match function and a boolean indicating if
	// the field was found.
	// FieldByNameFunc返回结构字段，其名称满足匹配函数，并返回一个布尔值，表示是否找到该字段
	//
	// FieldByNameFunc considers the fields in the struct itself
	// and then the fields in any embedded structs, in breadth first order,
	// stopping at the shallowest nesting depth containing one or more
	// fields satisfying the match function. If multiple fields at that depth
	// satisfy the match function, they cancel each other
	// and FieldByNameFunc returns no match.
	// FieldByNameFunc首先考虑结构体本身的字段，然后考虑任何嵌入结构体中的字段，宽度为一阶，停在最浅的嵌套深度，包含满足匹配函数的一个或多个字段
	// 如果该深度的多个字段满足match函数，它们会相互取消，并且FieldByNameFunc返回没有匹配
	// This behavior mirrors Go's handling of name lookup in
	// structs containing embedded fields.
	// 这种行为反映了Go在包含嵌入字段的结构中对名称查找的处理
	FieldByNameFunc(match func(string) bool) (StructField, bool)

	// In returns the type of a function type's i'th input parameter.
	// It panics if the type's Kind is not Func.
	// It panics if i is not in the range [0, NumIn()).
	// In返回函数类型的第i个输入参数的类型
	// 如果“善良”不是“有趣”，它就会惊慌
	// 如果i不在范围[0,NumIn()))，它会惊慌
	In(i int) Type

	// Key returns a map type's key type.
	// It panics if the type's Kind is not Map.
	// 返回映射类型的键类型 如果类型不是地图类型，它会panic
	Key() Type

	// Len returns an array type's length.
	// It panics if the type's Kind is not Array.
	// Len返回数组类型的长度 如果类型的Kind不是Array，它会惊慌
	Len() int

	// NumField returns a struct type's field count.
	// It panics if the type's Kind is not Struct.
	// NumField返回一个结构类型的字段计数 如果类型的类型不是结构体，它会恐慌
	NumField() int

	// NumIn returns a function type's input parameter count.
	// It panics if the type's Kind is not Func.
	// NumIn返回函数类型的输入参数计数 如果“善良”不是“有趣”，它就会惊慌
	NumIn() int

	// NumOut returns a function type's output parameter count.
	// It panics if the type's Kind is not Func.
	// NumOut返回函数类型的输出参数计数 如果“善良”不是“有趣”，它就会惊慌
	NumOut() int

	// Out returns the type of a function type's i'th output parameter.
	// It panics if the type's Kind is not Func.
	// It panics if i is not in the range [0, NumOut()).
	// Out返回函数类型的第i个输出参数的类型
	// 如果“善良”不是“有趣”，它就会惊慌
	// 如果i不在范围[0,NumOut()))，它会惊慌
	Out(i int) Type

	common() *rtype
	uncommon() *uncommonType
}

// BUG(rsc): FieldByName and related functions consider struct field names to be equal
// if the names are equal, even if they are unexported names originating
// in different packages. The practical effect of this is that the result of
// t.FieldByName("x") is not well defined if the struct type t contains
// multiple fields named x (embedded from different packages).
// FieldByName may return one of the fields named x or may report that there are none.
// See https://golang.org/issue/4876 for more details.
// BUG(rsc): FieldByName和相关函数认为结构字段的名称是相等的，如果它们是相等的，即使它们是来自不同包的未导出的名称
// 这样做的实际效果是，如果结构类型t包含多个名为x的字段(嵌入在不同的包中)，则t. fieldbyname (' x ')的结果没有很好地定义
// FieldByName可以返回名为x的字段之一，也可以报告没有字段

/*
 * These data structures are known to the compiler (../../cmd/internal/reflectdata/reflect.go).
 * A few are known to ../runtime/type.go to convey to debuggers.
 * They are also known to ../runtime/type.go.
 */

// A Kind represents the specific kind of type that a Type represents.
// The zero Kind is not a valid kind.
// “类型”表示“类型”所表示的特定类型
// 零种族不是有效的种族
type Kind uint

const (
	Invalid Kind = iota
	Bool
	Int
	Int8
	Int16
	Int32
	Int64
	Uint
	Uint8
	Uint16
	Uint32
	Uint64
	Uintptr
	Float32
	Float64
	Complex64
	Complex128
	Array
	Chan
	Func
	Interface
	Map
	Ptr
	Slice
	String
	Struct
	UnsafePointer
)

// tflag is used by an rtype to signal what extra type information is
// available in the memory directly following the rtype value.
// rtype使用Tflag来表示内存中rtype值后面有哪些额外的类型信息可用
//
// tflag values must be kept in sync with copies in:
// Tflag值必须与副本保持同步:
//	cmd/compile/internal/reflectdata/reflect.go
//	cmd/link/internal/ld/decodesym.go
//	runtime/type.go
type tflag uint8

const (
	// tflagUncommon means that there is a pointer, *uncommonType,
	// just beyond the outer type structure.
	// tflag不凡意味着在外部类型结构之外有一个指针*uncommonType
	//
	// For example, if t.Kind() == Struct and t.tflag&tflagUncommon != 0,
	// then t has uncommonType data and it can be accessed as:
	//
	//	type tUncommon struct {
	//		structType
	//		u uncommonType
	//	}
	//	u := &(*tUncommon)(unsafe.Pointer(t)).u
	tflagUncommon tflag = 1 << 0

	// tflagExtraStar means the name in the str field has an
	// extraneous '*' prefix. This is because for most types T in
	// a program, the type *T also exists and reusing the str data
	// saves binary size.
	// tflagExtraStar表示str字段中的名称有一个额外的“*”前缀
	// 这是因为对于程序中的大多数类型T，类型*T也存在，并且重用str数据可以节省二进制大小
	tflagExtraStar tflag = 1 << 1

	// tflagNamed means the type has a name.
	// tflagNamed表示类型有名称
	tflagNamed tflag = 1 << 2

	// tflagRegularMemory means that equal and hash functions can treat
	// this type as a single region of t.size bytes.
	// tflagRegularMemory意味着相等和散列函数可以将该类型视为t.size字节的单个区域
	tflagRegularMemory tflag = 1 << 3
)

// rtype is the common implementation of most values.
// It is embedded in other struct types.
// Rtype是大多数值的常见实现 它嵌入在其他结构类型中
//
// rtype must be kept in sync with ../runtime/type.go:/^type._type.
// Rtype必须与../runtime/type.go同步
type rtype struct {
	size       uintptr
	ptrdata    uintptr // number of bytes in the type that can contain pointers // 可包含指针的类型的字节数
	hash       uint32  // hash of type; avoids computation in hash tables // 散列的类型;避免在哈希表中进行计算
	tflag      tflag   // extra type information flags // 额外的类型信息标志
	align      uint8   // alignment of variable with this type // 变量与此类型的对齐
	fieldAlign uint8   // alignment of struct field with this type // struct字段与该类型的对齐
	kind       uint8   // enumeration for C //枚举C
	// function for comparing objects of this type  	// 函数，用于比较此类型的对象
	// (ptr to object A, ptr to object B) -> ==?   		// (ptr到对象A, ptr到对象B) -> ==?
	equal     func(unsafe.Pointer, unsafe.Pointer) bool
	gcdata    *byte   // garbage collection data // //垃圾收集数据
	str       nameOff // string form // / /字符串形式
	ptrToThis typeOff // type for pointer to this type, may be zero // // type表示该类型的指针，可以为0
}

// Method on non-interface type
// 方法设置为非接口类型
type method struct {
	name nameOff // name of method
	mtyp typeOff // method type (without receiver)
	ifn  textOff // fn used in interface call (one-word receiver)
	tfn  textOff // fn used for normal method call
}

// uncommonType is present only for defined types or types with methods
// (if T is a defined type, the uncommonTypes for T and *T have methods).
// Using a pointer to this struct reduces the overall size required
// to describe a non-defined type with no methods.
// uncommonType只出现在已定义类型或带有方法的类型中(如果T是已定义类型，T和*T的uncommonTypes有方法)
// 使用指向此结构的指针可以减少描述没有方法的非定义类型所需的总体大小
type uncommonType struct {
	pkgPath nameOff // import path; empty for built-in types like int, string // 进口路径;对于内置类型如int、string为空
	mcount  uint16  // number of methods // 数量的方法
	xcount  uint16  // number of exported methods // 导出的方法个数
	moff    uint32  // offset from this uncommontype to [mcount]method  // //从[mcount]方法的偏移量
	_       uint32  // unused
}

// ChanDir represents a channel type's direction.
// ChanDir表示通道类型的方向
type ChanDir int

const (
	RecvDir ChanDir             = 1 << iota // <-chan
	SendDir                                 // chan<-
	BothDir = RecvDir | SendDir             // chan
)

// arrayType represents a fixed array type.
// arrayType 表示固定的数组类型
type arrayType struct {
	rtype
	elem  *rtype // array element type
	slice *rtype // slice type
	len   uintptr
}

// chanType represents a channel type.
// chanType表示通道类型
type chanType struct {
	rtype
	elem *rtype  // channel element type
	dir  uintptr // channel direction (ChanDir)
}

// funcType represents a function type.
// funcType表示函数类型
//
// A *rtype for each in and out parameter is stored in an array that
// directly follows the funcType (and possibly its uncommonType). So
// a function type with one method, one input, and one output is:
// 每个in和out形参的*rtype存储在一个数组中，该数组直接跟在funcType(也可能是它的uncommonType)之后
// 所以一个函数类型，一个方法，一个输入，一个输出是:
//
//	struct {
//		funcType
//		uncommonType
//		[2]*rtype    // [0] is in, [1] is out
//	}
type funcType struct {
	rtype
	inCount  uint16
	outCount uint16 // top bit is set if last input parameter is ...
}

// imethod represents a method on an interface type
// Imethod表示接口类型上的方法
type imethod struct {
	name nameOff // name of method
	typ  typeOff // .(*FuncType) underneath
}

// interfaceType represents an interface type.
// interfaceType表示接口类型
type interfaceType struct {
	rtype
	pkgPath name      // import path
	methods []imethod // sorted by hash
}

// mapType represents a map type.
// mapType表示映射类型
type mapType struct {
	rtype
	key    *rtype // map key type
	elem   *rtype // map element (value) type
	bucket *rtype // internal bucket structure
	// function for hashing keys (ptr to key, seed) -> hash
	hasher     func(unsafe.Pointer, uintptr) uintptr
	keysize    uint8  // size of key slot
	valuesize  uint8  // size of value slot
	bucketsize uint16 // size of bucket
	flags      uint32
}

// ptrType represents a pointer type.
// ptrType表示指针类型
type ptrType struct {
	rtype
	elem *rtype // pointer element (pointed at) type
}

// sliceType represents a slice type.
// sliceType表示切片类型
type sliceType struct {
	rtype
	elem *rtype // slice element type
}

// Struct field
// 结构体字段
type structField struct {
	name        name    // name is always non-empty
	typ         *rtype  // type of field
	offsetEmbed uintptr // byte offset of field<<1 | isEmbedded
}

func (f *structField) offset() uintptr {
	return f.offsetEmbed >> 1
}

func (f *structField) embedded() bool {
	return f.offsetEmbed&1 != 0
}

// structType represents a struct type.
// structType表示struct类型
type structType struct {
	rtype
	pkgPath name
	fields  []structField // sorted by offset
}

// name is an encoded type name with optional extra data.
// Name是带有可选额外数据的编码类型名称
//
// The first byte is a bit field containing:
// 第一个字节是位字段，包含:
//
//	1<<0 the name is exported
//	1<<1 tag data follows the name
//	1<<2 pkgPath nameOff follows the name and tag
//
// Following that, there is a varint-encoded length of the name,
// followed by the name itself.
// 在此之后，有一个变量编码的名称长度，后面是名称本身
//
// If tag data is present, it also has a varint-encoded length
// followed by the tag itself.
// 如果存在标记数据，它也具有可变编码长度
//
// If the import path follows, then 4 bytes at the end of
// the data form a nameOff. The import path is only set for concrete
// methods that are defined in a different package than their type.
// 如果导入路径紧随其后，那么数据末尾的4个字节将形成一个nameOff
// 仅为定义在与其类型不同的包中的具体方法设置导入路径
//
// If a name starts with "*", then the exported bit represents
// whether the pointed to type is exported.
// 如果名称以' * '开头，则输出的位表示是否输出所指向的类型
//
// Note: this encoding must match here and in:
// 注意:这个编码必须匹配这里和这里:
//   cmd/compile/internal/reflectdata/reflect.go
//   runtime/type.go
//   internal/reflectlite/type.go
//   cmd/link/internal/ld/decodesym.go

type name struct {
	bytes *byte
}

func (n name) data(off int, whySafe string) *byte {
	return (*byte)(add(unsafe.Pointer(n.bytes), uintptr(off), whySafe))
}

func (n name) isExported() bool {
	return (*n.bytes)&(1<<0) != 0
}

func (n name) hasTag() bool {
	return (*n.bytes)&(1<<1) != 0
}

// readVarint parses a varint as encoded by encoding/binary.
// It returns the number of encoded bytes and the encoded value.
// readVarint解析通过encoding/binary编码的变量
// 它返回已编码的字节数和已编码的值
func (n name) readVarint(off int) (int, int) {
	v := 0
	for i := 0; ; i++ {
		x := *n.data(off+i, "read varint")
		v += int(x&0x7f) << (7 * i)
		if x&0x80 == 0 {
			return i + 1, v
		}
	}
}

// writeVarint writes n to buf in varint form. Returns the
// number of bytes written. n must be nonnegative.
// Writes at most 10 bytes.
// writeVarint将n以varint形式写入buf 返回写入的字节数
// N必须是非负的 最多写10个字节
func writeVarint(buf []byte, n int) int {
	for i := 0; ; i++ {
		b := byte(n & 0x7f)
		n >>= 7
		if n == 0 {
			buf[i] = b
			return i + 1
		}
		buf[i] = b | 0x80
	}
}

func (n name) name() (s string) {
	if n.bytes == nil {
		return
	}
	i, l := n.readVarint(1)
	hdr := (*unsafeheader.String)(unsafe.Pointer(&s))
	hdr.Data = unsafe.Pointer(n.data(1+i, "non-empty string"))
	hdr.Len = l
	return
}

func (n name) tag() (s string) {
	if !n.hasTag() {
		return ""
	}
	i, l := n.readVarint(1)
	i2, l2 := n.readVarint(1 + i + l)
	hdr := (*unsafeheader.String)(unsafe.Pointer(&s))
	hdr.Data = unsafe.Pointer(n.data(1+i+l+i2, "non-empty string"))
	hdr.Len = l2
	return
}

func (n name) pkgPath() string {
	if n.bytes == nil || *n.data(0, "name flag field")&(1<<2) == 0 {
		return ""
	}
	i, l := n.readVarint(1)
	off := 1 + i + l
	if n.hasTag() {
		i2, l2 := n.readVarint(off)
		off += i2 + l2
	}
	var nameOff int32
	// Note that this field may not be aligned in memory,
	// so we cannot use a direct int32 assignment here.
	// 注意，这个字段在内存中可能是不对齐的，所以我们不能在这里直接使用int32赋值
	copy((*[4]byte)(unsafe.Pointer(&nameOff))[:], (*[4]byte)(unsafe.Pointer(n.data(off, "name offset field")))[:])
	pkgPathName := name{(*byte)(resolveTypeOff(unsafe.Pointer(n.bytes), nameOff))}
	return pkgPathName.name()
}

func newName(n, tag string, exported bool) name {
	if len(n) >= 1<<29 {
		panic("reflect.nameFrom: name too long: " + n[:1024] + "...")
	}
	if len(tag) >= 1<<29 {
		panic("reflect.nameFrom: tag too long: " + tag[:1024] + "...")
	}
	var nameLen [10]byte
	var tagLen [10]byte
	nameLenLen := writeVarint(nameLen[:], len(n))
	tagLenLen := writeVarint(tagLen[:], len(tag))

	var bits byte
	l := 1 + nameLenLen + len(n)
	if exported {
		bits |= 1 << 0
	}
	if len(tag) > 0 {
		l += tagLenLen + len(tag)
		bits |= 1 << 1
	}

	b := make([]byte, l)
	b[0] = bits
	copy(b[1:], nameLen[:nameLenLen])
	copy(b[1+nameLenLen:], n)
	if len(tag) > 0 {
		tb := b[1+nameLenLen+len(n):]
		copy(tb, tagLen[:tagLenLen])
		copy(tb[tagLenLen:], tag)
	}

	return name{bytes: &b[0]}
}

/*
 * The compiler knows the exact layout of all the data structures above.
 * The compiler does not know about the data structures and methods below.
 */

// Method represents a single method.
type Method struct {
	// Name is the method name.
	Name string

	// PkgPath is the package path that qualifies a lower case (unexported)
	// method name. It is empty for upper case (exported) method names.
	// The combination of PkgPath and Name uniquely identifies a method
	// in a method set.
	// PkgPath是包路径，它限定了小写(未导出)的方法名
	// 对于大写的(导出的)方法名，它是空的 PkgPath和Name的组合唯一地标识方法集中的一个方法
	// See https://golang.org/ref/spec#Uniqueness_of_identifiers
	PkgPath string

	Type  Type  // method type
	Func  Value // func with receiver as first argument
	Index int   // index for Type.Method
}

// IsExported reports whether the method is exported.
func (m Method) IsExported() bool {
	return m.PkgPath == ""
}

const (
	kindDirectIface = 1 << 5
	kindGCProg      = 1 << 6 // Type.gc points to GC program
	kindMask        = (1 << 5) - 1
)

// String returns the name of k.
func (k Kind) String() string {
	if int(k) < len(kindNames) {
		return kindNames[k]
	}
	return "kind" + strconv.Itoa(int(k))
}

var kindNames = []string{
	Invalid:       "invalid",
	Bool:          "bool",
	Int:           "int",
	Int8:          "int8",
	Int16:         "int16",
	Int32:         "int32",
	Int64:         "int64",
	Uint:          "uint",
	Uint8:         "uint8",
	Uint16:        "uint16",
	Uint32:        "uint32",
	Uint64:        "uint64",
	Uintptr:       "uintptr",
	Float32:       "float32",
	Float64:       "float64",
	Complex64:     "complex64",
	Complex128:    "complex128",
	Array:         "array",
	Chan:          "chan",
	Func:          "func",
	Interface:     "interface",
	Map:           "map",
	Ptr:           "ptr",
	Slice:         "slice",
	String:        "string",
	Struct:        "struct",
	UnsafePointer: "unsafe.Pointer",
}

func (t *uncommonType) methods() []method {
	if t.mcount == 0 {
		return nil
	}
	return (*[1 << 16]method)(add(unsafe.Pointer(t), uintptr(t.moff), "t.mcount > 0"))[:t.mcount:t.mcount]
}

func (t *uncommonType) exportedMethods() []method {
	if t.xcount == 0 {
		return nil
	}
	return (*[1 << 16]method)(add(unsafe.Pointer(t), uintptr(t.moff), "t.xcount > 0"))[:t.xcount:t.xcount]
}

// resolveNameOff resolves a name offset from a base pointer.
// The (*rtype).nameOff method is a convenience wrapper for this function.
// Implemented in the runtime package.
// resolveNameOff解析基指针的名称偏移量 (*rtype). nameoff方法是该函数的方便包装器
// 在运行时包中实现
func resolveNameOff(ptrInModule unsafe.Pointer, off int32) unsafe.Pointer

// resolveTypeOff resolves an *rtype offset from a base type.
// The (*rtype).typeOff method is a convenience wrapper for this function.
// Implemented in the runtime package.
// resolveTypeOff解析基类型的*rtype偏移量 (* rtype)
// typeOff方法是该函数的方便包装器 在运行时包中实现
func resolveTypeOff(rtype unsafe.Pointer, off int32) unsafe.Pointer

// resolveTextOff resolves a function pointer offset from a base type.
// The (*rtype).textOff method is a convenience wrapper for this function.
// Implemented in the runtime package.
// resolveTextOff解析基类型的函数指针偏移量
// (* rtype) textOff方法是该函数的方便包装器
// 在运行时包中实现
func resolveTextOff(rtype unsafe.Pointer, off int32) unsafe.Pointer

// addReflectOff adds a pointer to the reflection lookup map in the runtime.
// It returns a new ID that can be used as a typeOff or textOff, and will
// be resolved correctly. Implemented in the runtime package.
// addReflectOff在运行时中添加一个指向反射查找映射的指针
// 它返回一个新的ID，可以用作typeOff或texttoff，并将被正确解析
// 在运行时包中实现
func addReflectOff(ptr unsafe.Pointer) int32

// resolveReflectName adds a name to the reflection lookup map in the runtime.
// It returns a new nameOff that can be used to refer to the pointer.
// resolveReflectName在运行时中将一个名称添加到反射查找映射
// 它返回一个新的nameOff，可用于指向指针
func resolveReflectName(n name) nameOff {
	return nameOff(addReflectOff(unsafe.Pointer(n.bytes)))
}

// resolveReflectType adds a *rtype to the reflection lookup map in the runtime.
// It returns a new typeOff that can be used to refer to the pointer.
// resolveReflectType在运行时中将一个*rtype添加到反射查找映射
// 它返回一个新的typeOff，可用于引用指针
func resolveReflectType(t *rtype) typeOff {
	return typeOff(addReflectOff(unsafe.Pointer(t)))
}

// resolveReflectText adds a function pointer to the reflection lookup map in
// the runtime. It returns a new textOff that can be used to refer to the
// pointer.
// resolveReflectText在运行时向反射查找映射添加一个函数指针
// 它返回一个新的texttoff，可用于引用指针
func resolveReflectText(ptr unsafe.Pointer) textOff {
	return textOff(addReflectOff(ptr))
}

type nameOff int32 // offset to a name
type typeOff int32 // offset to an *rtype
type textOff int32 // offset from top of text section

func (t *rtype) nameOff(off nameOff) name {
	return name{(*byte)(resolveNameOff(unsafe.Pointer(t), int32(off)))}
}

func (t *rtype) typeOff(off typeOff) *rtype {
	return (*rtype)(resolveTypeOff(unsafe.Pointer(t), int32(off)))
}

func (t *rtype) textOff(off textOff) unsafe.Pointer {
	return resolveTextOff(unsafe.Pointer(t), int32(off))
}

func (t *rtype) uncommon() *uncommonType {
	if t.tflag&tflagUncommon == 0 {
		return nil
	}
	switch t.Kind() {
	case Struct:
		return &(*structTypeUncommon)(unsafe.Pointer(t)).u
	case Ptr:
		type u struct {
			ptrType
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	case Func:
		type u struct {
			funcType
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	case Slice:
		type u struct {
			sliceType
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	case Array:
		type u struct {
			arrayType
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	case Chan:
		type u struct {
			chanType
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	case Map:
		type u struct {
			mapType
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	case Interface:
		type u struct {
			interfaceType
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	default:
		type u struct {
			rtype
			u uncommonType
		}
		return &(*u)(unsafe.Pointer(t)).u
	}
}

func (t *rtype) String() string {
	s := t.nameOff(t.str).name()
	if t.tflag&tflagExtraStar != 0 {
		return s[1:]
	}
	return s
}

func (t *rtype) Size() uintptr { return t.size }

func (t *rtype) Bits() int {
	if t == nil {
		panic("reflect: Bits of nil Type")
	}
	k := t.Kind()
	if k < Int || k > Complex128 {
		panic("reflect: Bits of non-arithmetic Type " + t.String())
	}
	return int(t.size) * 8
}

func (t *rtype) Align() int { return int(t.align) }

func (t *rtype) FieldAlign() int { return int(t.fieldAlign) }

func (t *rtype) Kind() Kind { return Kind(t.kind & kindMask) }

func (t *rtype) pointers() bool { return t.ptrdata != 0 }

func (t *rtype) common() *rtype { return t }

func (t *rtype) exportedMethods() []method {
	ut := t.uncommon()
	if ut == nil {
		return nil
	}
	return ut.exportedMethods()
}

func (t *rtype) NumMethod() int {
	if t.Kind() == Interface {
		tt := (*interfaceType)(unsafe.Pointer(t))
		return tt.NumMethod()
	}
	return len(t.exportedMethods())
}

func (t *rtype) Method(i int) (m Method) {
	if t.Kind() == Interface {
		tt := (*interfaceType)(unsafe.Pointer(t))
		return tt.Method(i)
	}
	methods := t.exportedMethods()
	if i < 0 || i >= len(methods) {
		panic("reflect: Method index out of range")
	}
	p := methods[i]
	pname := t.nameOff(p.name)
	m.Name = pname.name()
	fl := flag(Func)
	mtyp := t.typeOff(p.mtyp)
	ft := (*funcType)(unsafe.Pointer(mtyp))
	in := make([]Type, 0, 1+len(ft.in()))
	in = append(in, t)
	for _, arg := range ft.in() {
		in = append(in, arg)
	}
	out := make([]Type, 0, len(ft.out()))
	for _, ret := range ft.out() {
		out = append(out, ret)
	}
	mt := FuncOf(in, out, ft.IsVariadic())
	m.Type = mt
	tfn := t.textOff(p.tfn)
	fn := unsafe.Pointer(&tfn)
	m.Func = Value{mt.(*rtype), fn, fl}

	m.Index = i
	return m
}

func (t *rtype) MethodByName(name string) (m Method, ok bool) {
	if t.Kind() == Interface {
		tt := (*interfaceType)(unsafe.Pointer(t))
		return tt.MethodByName(name)
	}
	ut := t.uncommon()
	if ut == nil {
		return Method{}, false
	}
	// TODO(mdempsky): Binary search.
	for i, p := range ut.exportedMethods() {
		if t.nameOff(p.name).name() == name {
			return t.Method(i), true
		}
	}
	return Method{}, false
}

func (t *rtype) PkgPath() string {
	if t.tflag&tflagNamed == 0 {
		return ""
	}
	ut := t.uncommon()
	if ut == nil {
		return ""
	}
	return t.nameOff(ut.pkgPath).name()
}

func (t *rtype) hasName() bool {
	return t.tflag&tflagNamed != 0
}

func (t *rtype) Name() string {
	if !t.hasName() {
		return ""
	}
	s := t.String()
	i := len(s) - 1
	for i >= 0 && s[i] != '.' {
		i--
	}
	return s[i+1:]
}

func (t *rtype) ChanDir() ChanDir {
	if t.Kind() != Chan {
		panic("reflect: ChanDir of non-chan type " + t.String())
	}
	tt := (*chanType)(unsafe.Pointer(t))
	return ChanDir(tt.dir)
}

func (t *rtype) IsVariadic() bool {
	if t.Kind() != Func {
		panic("reflect: IsVariadic of non-func type " + t.String())
	}
	tt := (*funcType)(unsafe.Pointer(t))
	return tt.outCount&(1<<15) != 0
}

func (t *rtype) Elem() Type {
	switch t.Kind() {
	case Array:
		tt := (*arrayType)(unsafe.Pointer(t))
		return toType(tt.elem)
	case Chan:
		tt := (*chanType)(unsafe.Pointer(t))
		return toType(tt.elem)
	case Map:
		tt := (*mapType)(unsafe.Pointer(t))
		return toType(tt.elem)
	case Ptr:
		tt := (*ptrType)(unsafe.Pointer(t))
		return toType(tt.elem)
	case Slice:
		tt := (*sliceType)(unsafe.Pointer(t))
		return toType(tt.elem)
	}
	panic("reflect: Elem of invalid type " + t.String())
}

func (t *rtype) Field(i int) StructField {
	if t.Kind() != Struct {
		panic("reflect: Field of non-struct type " + t.String())
	}
	tt := (*structType)(unsafe.Pointer(t))
	return tt.Field(i)
}

func (t *rtype) FieldByIndex(index []int) StructField {
	if t.Kind() != Struct {
		panic("reflect: FieldByIndex of non-struct type " + t.String())
	}
	tt := (*structType)(unsafe.Pointer(t))
	return tt.FieldByIndex(index)
}

func (t *rtype) FieldByName(name string) (StructField, bool) {
	if t.Kind() != Struct {
		panic("reflect: FieldByName of non-struct type " + t.String())
	}
	tt := (*structType)(unsafe.Pointer(t))
	return tt.FieldByName(name)
}

func (t *rtype) FieldByNameFunc(match func(string) bool) (StructField, bool) {
	if t.Kind() != Struct {
		panic("reflect: FieldByNameFunc of non-struct type " + t.String())
	}
	tt := (*structType)(unsafe.Pointer(t))
	return tt.FieldByNameFunc(match)
}

func (t *rtype) In(i int) Type {
	if t.Kind() != Func {
		panic("reflect: In of non-func type " + t.String())
	}
	tt := (*funcType)(unsafe.Pointer(t))
	return toType(tt.in()[i])
}

func (t *rtype) Key() Type {
	if t.Kind() != Map {
		panic("reflect: Key of non-map type " + t.String())
	}
	tt := (*mapType)(unsafe.Pointer(t))
	return toType(tt.key)
}

func (t *rtype) Len() int {
	if t.Kind() != Array {
		panic("reflect: Len of non-array type " + t.String())
	}
	tt := (*arrayType)(unsafe.Pointer(t))
	return int(tt.len)
}

func (t *rtype) NumField() int {
	if t.Kind() != Struct {
		panic("reflect: NumField of non-struct type " + t.String())
	}
	tt := (*structType)(unsafe.Pointer(t))
	return len(tt.fields)
}

func (t *rtype) NumIn() int {
	if t.Kind() != Func {
		panic("reflect: NumIn of non-func type " + t.String())
	}
	tt := (*funcType)(unsafe.Pointer(t))
	return int(tt.inCount)
}

func (t *rtype) NumOut() int {
	if t.Kind() != Func {
		panic("reflect: NumOut of non-func type " + t.String())
	}
	tt := (*funcType)(unsafe.Pointer(t))
	return len(tt.out())
}

func (t *rtype) Out(i int) Type {
	if t.Kind() != Func {
		panic("reflect: Out of non-func type " + t.String())
	}
	tt := (*funcType)(unsafe.Pointer(t))
	return toType(tt.out()[i])
}

func (t *funcType) in() []*rtype {
	uadd := unsafe.Sizeof(*t)
	if t.tflag&tflagUncommon != 0 {
		uadd += unsafe.Sizeof(uncommonType{})
	}
	if t.inCount == 0 {
		return nil
	}
	return (*[1 << 20]*rtype)(add(unsafe.Pointer(t), uadd, "t.inCount > 0"))[:t.inCount:t.inCount]
}

func (t *funcType) out() []*rtype {
	uadd := unsafe.Sizeof(*t)
	if t.tflag&tflagUncommon != 0 {
		uadd += unsafe.Sizeof(uncommonType{})
	}
	outCount := t.outCount & (1<<15 - 1)
	if outCount == 0 {
		return nil
	}
	return (*[1 << 20]*rtype)(add(unsafe.Pointer(t), uadd, "outCount > 0"))[t.inCount : t.inCount+outCount : t.inCount+outCount]
}

// add returns p+x.
// 添加返回p + x
//
// The whySafe string is ignored, so that the function still inlines
// as efficiently as p+x, but all call sites should use the string to
// record why the addition is safe, which is to say why the addition
// does not cause x to advance to the very end of p's allocation
// and therefore point incorrectly at the next block in memory.
// whySafe字符串被忽略，这样函数仍然可以像p+x那样高效地内联，但是所有的调用站点都应该使用这个字符串来记录为什么加法是安全的，也就是说为什么加法不会导致x提前到p的分配的最后，从而不正确地指向内存中的下一个块
func add(p unsafe.Pointer, x uintptr, whySafe string) unsafe.Pointer {
	return unsafe.Pointer(uintptr(p) + x)
}

func (d ChanDir) String() string {
	switch d {
	case SendDir:
		return "chan<-"
	case RecvDir:
		return "<-chan"
	case BothDir:
		return "chan"
	}
	return "ChanDir" + strconv.Itoa(int(d))
}

// Method returns the i'th method in the type's method set.
// 方法返回类型的方法集中的第i个方法
func (t *interfaceType) Method(i int) (m Method) {
	if i < 0 || i >= len(t.methods) {
		return
	}
	p := &t.methods[i]
	pname := t.nameOff(p.name)
	m.Name = pname.name()
	if !pname.isExported() {
		m.PkgPath = pname.pkgPath()
		if m.PkgPath == "" {
			m.PkgPath = t.pkgPath.name()
		}
	}
	m.Type = toType(t.typeOff(p.typ))
	m.Index = i
	return
}

// NumMethod returns the number of interface methods in the type's method set.
// NumMethod返回类型方法集中的接口方法的数量
func (t *interfaceType) NumMethod() int { return len(t.methods) }

// MethodByName method with the given name in the type's method set.
// MethodByName方法，在类型的方法集中使用给定的名称
func (t *interfaceType) MethodByName(name string) (m Method, ok bool) {
	if t == nil {
		return
	}
	var p *imethod
	for i := range t.methods {
		p = &t.methods[i]
		if t.nameOff(p.name).name() == name {
			return t.Method(i), true
		}
	}
	return
}

// A StructField describes a single field in a struct.
// StructField描述结构中的单个字段
type StructField struct {
	// Name is the field name.
	// Name是字段名称
	Name string

	// PkgPath is the package path that qualifies a lower case (unexported)
	// field name. It is empty for upper case (exported) field names.
	// PkgPath是包路径，它限定了小写(未导出)字段名
	// 对于大写(导出)字段名，它是空的
	// See https://golang.org/ref/spec#Uniqueness_of_identifiers
	PkgPath string

	Type      Type      // field type
	Tag       StructTag // field tag string
	Offset    uintptr   // offset within struct, in bytes
	Index     []int     // index sequence for Type.FieldByIndex
	Anonymous bool      // is an embedded field
}

// IsExported reports whether the field is exported.
// isexports用于报告该字段是否导出
func (f StructField) IsExported() bool {
	return f.PkgPath == ""
}

// A StructTag is the tag string in a struct field.
// StructTag是struct字段中的标记字符串
//
// By convention, tag strings are a concatenation of
// optionally space-separated key:"value" pairs.
// Each key is a non-empty string consisting of non-control
// characters other than space (U+0020 ' '), quote (U+0022 '"'),
// and colon (U+003A ':').  Each value is quoted using U+0022 '"'
// characters and Go string literal syntax.
// 按照约定，标记字符串是由空格分隔的键:' value '对(可选)拼接而成
// 每个键是一个非空字符串，由除空格以外的非控制字符(U+0020 ' ')、引号(U+0022 ' ')和冒号(U+003A ': ')组成
// 每个值都使用U+0022 ' '字符和Go字符串字面语法引用
type StructTag string

// Get returns the value associated with key in the tag string.
// If there is no such key in the tag, Get returns the empty string.
// If the tag does not have the conventional format, the value
// returned by Get is unspecified. To determine whether a tag is
// explicitly set to the empty string, use Lookup.
// Get返回标记字符串中与key关联的值 如果标签中没有这样的键，Get返回空字符串
// 如果标记没有常规格式，则Get返回的值是未指定的
// 若要确定标记是否显式设置为空字符串，请使用Lookup
func (tag StructTag) Get(key string) string {
	v, _ := tag.Lookup(key)
	return v
}

// Lookup returns the value associated with key in the tag string.
// If the key is present in the tag the value (which may be empty)
// is returned. Otherwise the returned value will be the empty string.
// The ok return value reports whether the value was explicitly set in
// the tag string. If the tag does not have the conventional format,
// the value returned by Lookup is unspecified.
// Lookup返回标记字符串中与key关联的值 如果标记中存在键，则返回值(可能为空)
// 否则返回值将是空字符串 ok返回值报告是否在标记字符串中显式地设置了值
// 如果标记不具有常规格式，则Lookup返回的值是未指定的
func (tag StructTag) Lookup(key string) (value string, ok bool) {
	// When modifying this code, also update the validateStructTag code
	// in cmd/vet/structtag.go.
	// 修改这段代码时，还需要在cmd/vet/structtag.go中更新validateStructTag代码

	for tag != "" {
		// Skip leading space.
		i := 0
		for i < len(tag) && tag[i] == ' ' {
			i++
		}
		tag = tag[i:]
		if tag == "" {
			break
		}

		// Scan to colon. A space, a quote or a control character is a syntax error.
		// Strictly speaking, control chars include the range [0x7f, 0x9f], not just
		// [0x00, 0x1f], but in practice, we ignore the multi-byte control characters
		// as it is simpler to inspect the tag's bytes than the tag's runes.
		// 扫描结肠 空格、引号或控制字符是语法错误
		// 严格地说，控制字符包括[0x7f, 0x9f]范围，而不仅仅是[0x00, 0x1f]，但在实践中，我们忽略多字节控制字符，因为检查标签的字节比检查标签的符文更简单
		i = 0
		for i < len(tag) && tag[i] > ' ' && tag[i] != ':' && tag[i] != '"' && tag[i] != 0x7f {
			i++
		}
		if i == 0 || i+1 >= len(tag) || tag[i] != ':' || tag[i+1] != '"' {
			break
		}
		name := string(tag[:i])
		tag = tag[i+1:]

		// Scan quoted string to find value.
		// 扫描带引号的字符串查找值
		i = 1
		for i < len(tag) && tag[i] != '"' {
			if tag[i] == '\\' {
				i++
			}
			i++
		}
		if i >= len(tag) {
			break
		}
		qvalue := string(tag[:i+1])
		tag = tag[i+1:]

		if key == name {
			value, err := strconv.Unquote(qvalue)
			if err != nil {
				break
			}
			return value, true
		}
	}
	return "", false
}

// Field returns the i'th struct field.
// Field返回第i个struct字段
func (t *structType) Field(i int) (f StructField) {
	if i < 0 || i >= len(t.fields) {
		panic("reflect: Field index out of bounds")
	}
	p := &t.fields[i]
	f.Type = toType(p.typ)
	f.Name = p.name.name()
	f.Anonymous = p.embedded()
	if !p.name.isExported() {
		f.PkgPath = t.pkgPath.name()
	}
	if tag := p.name.tag(); tag != "" {
		f.Tag = StructTag(tag)
	}
	f.Offset = p.offset()

	// NOTE(rsc): This is the only allocation in the interface
	// presented by a reflect.Type. It would be nice to avoid,
	// at least in the common cases, but we need to make sure
	// that misbehaving clients of reflect cannot affect other
	// uses of reflect. One possibility is CL 5371098, but we
	// postponed that ugliness until there is a demonstrated
	// need for the performance. This is issue 2320.
	// 说明(rsc):这是接口中唯一由reflect.Type提供的分配
	// 至少在常见的情况下，最好避免这样做，但是我们需要确保reflect的错误行为客户机不会影响到reflect的其他使用
	// 一种可能是CL 5371098，但我们推迟了它的丑陋，直到有证明需要它的表现
	// 这是第2320期
	f.Index = []int{i}
	return
}

// TODO(gri): Should there be an error/bool indicator if the index
//            is wrong for FieldByIndex?
// TODO(gri):如果索引存在错误/bool指示符

// FieldByIndex returns the nested field corresponding to index.
// FieldByIndex返回对应于index的嵌套字段
func (t *structType) FieldByIndex(index []int) (f StructField) {
	f.Type = toType(&t.rtype)
	for i, x := range index {
		if i > 0 {
			ft := f.Type
			if ft.Kind() == Ptr && ft.Elem().Kind() == Struct {
				ft = ft.Elem()
			}
			f.Type = ft
		}
		f = f.Type.Field(x)
	}
	return
}

// A fieldScan represents an item on the fieldByNameFunc scan work list.
// fieldScan表示fieldByNameFunc扫描工作列表中的一个项目
type fieldScan struct {
	typ   *structType
	index []int
}

// FieldByNameFunc returns the struct field with a name that satisfies the
// match function and a boolean to indicate if the field was found.
// FieldByNameFunc返回struct字段，其名称满足匹配函数，并返回一个布尔值来指示是否找到该字段
func (t *structType) FieldByNameFunc(match func(string) bool) (result StructField, ok bool) {
	// This uses the same condition that the Go language does: there must be a unique instance
	// of the match at a given depth level. If there are multiple instances of a match at the
	// same depth, they annihilate each other and inhibit any possible match at a lower level.
	// The algorithm is breadth first search, one depth level at a time.
	// 这使用了与Go语言相同的条件:在给定的深度级别上必须有一个唯一的匹配实例
	// 如果同一深度存在多个匹配实例，它们将相互消灭并抑制较低层次上的任何可能匹配
	// 算法是广度优先搜索，每次一个深度层次

	// The current and next slices are work queues:
	// current lists the fields to visit on this depth level,
	// and next lists the fields on the next lower level.
	// 当前片和下一个片都是工作队列:current列出了在这个深度级别上访问的字段，next列出了在下一个较低级别上访问的字段
	current := []fieldScan{}
	next := []fieldScan{{typ: t}}

	// nextCount records the number of times an embedded type has been
	// encountered and considered for queueing in the 'next' slice.
	// We only queue the first one, but we increment the count on each.
	// If a struct type T can be reached more than once at a given depth level,
	// then it annihilates itself and need not be considered at all when we
	// process that next depth level.
	// nextCount记录嵌入类型在“下一个”片中遇到并考虑排队的次数
	// 我们只对第一个变量进行排队，但对每个变量递增计数
	// 如果一个结构类型T可以在一个给定的深度级别上被达到不止一次，那么当我们处理下一个深度级别时，它就会自动消失，根本不需要考虑它
	var nextCount map[*structType]int

	// visited records the structs that have been considered already.
	// Embedded pointer fields can create cycles in the graph of
	// reachable embedded types; visited avoids following those cycles.
	// It also avoids duplicated effort: if we didn't find the field in an
	// embedded type T at level 2, we won't find it in one at level 4 either.
	// Visited记录了已经考虑过的结构 嵌入式指针字段可以在访问的可达嵌入式类型图中创建循环，避免遵循这些循环
	// 它还避免了重复的工作:如果我们没有在第2级的嵌入式类型T中找到字段，那么我们也不会在第4级的嵌入式类型中找到它
	visited := map[*structType]bool{}

	for len(next) > 0 {
		current, next = next, current[:0]
		count := nextCount
		nextCount = nil

		// Process all the fields at this depth, now listed in 'current'.
		// The loop queues embedded fields found in 'next', for processing during the next
		// iteration. The multiplicity of the 'current' field counts is recorded
		// in 'count'; the multiplicity of the 'next' field counts is recorded in 'nextCount'.
		// 在这个深度处理所有字段，现在列在' current '中
		// 循环将在' next '中找到的嵌入字段排队，以便在下一个迭代期间进行处理
		// ' current '字段计数的多样性被记录在' count '中，' next '字段计数的多样性被记录在' nextCount '中
		for _, scan := range current {
			t := scan.typ
			if visited[t] {
				// We've looked through this type before, at a higher level.
				// That higher level would shadow the lower level we're now at,
				// so this one can't be useful to us. Ignore it.
				// 我们以前在更高的层次上研究过这种类型
				// 更高的能级会遮蔽我们现在所处的更低的能级，所以这个对我们没用
				// 忽略它
				continue
			}
			visited[t] = true
			for i := range t.fields {
				f := &t.fields[i]
				// Find name and (for embedded field) type for field f.
				// 查找字段f的名称和(对于嵌入字段)类型
				fname := f.name.name()
				var ntyp *rtype
				if f.embedded() {
					// Embedded field of type T or *T.
					// 嵌入式字段类型为T或*T
					ntyp = f.typ
					if ntyp.Kind() == Ptr {
						ntyp = ntyp.Elem().common()
					}
				}

				// Does it match?
				// 比赛吗?
				if match(fname) {
					// Potential match
					// 潜在的匹配
					if count[t] > 1 || ok {
						// Name appeared multiple times at this level: annihilate.
						// 名字在这个关卡多次出现:歼灭
						return StructField{}, false
					}
					result = t.Field(i)
					result.Index = nil
					result.Index = append(result.Index, scan.index...)
					result.Index = append(result.Index, i)
					ok = true
					continue
				}

				// Queue embedded struct fields for processing with next level,
				// but only if we haven't seen a match yet at this level and only
				// if the embedded types haven't already been queued.
				// 将嵌入的结构字段放入队列，以便在下一层进行处理，但前提是我们在这一层还没有看到匹配，而且嵌入的类型还没有被放入队列
				if ok || ntyp == nil || ntyp.Kind() != Struct {
					continue
				}
				styp := (*structType)(unsafe.Pointer(ntyp))
				if nextCount[styp] > 0 {
					nextCount[styp] = 2 // exact multiple doesn't matter
					continue
				}
				if nextCount == nil {
					nextCount = map[*structType]int{}
				}
				nextCount[styp] = 1
				if count[t] > 1 {
					nextCount[styp] = 2 // exact multiple doesn't matter
				}
				var index []int
				index = append(index, scan.index...)
				index = append(index, i)
				next = append(next, fieldScan{styp, index})
			}
		}
		if ok {
			break
		}
	}
	return
}

// FieldByName returns the struct field with the given name
// and a boolean to indicate if the field was found.
// FieldByName返回具有给定名称的结构字段和一个布尔值，以指示是否找到该字段
func (t *structType) FieldByName(name string) (f StructField, present bool) {
	// Quick check for top-level name, or struct without embedded fields.
	// 快速检查顶级名称或没有嵌入字段的结构
	hasEmbeds := false
	if name != "" {
		for i := range t.fields {
			tf := &t.fields[i]
			if tf.name.name() == name {
				return t.Field(i), true
			}
			if tf.embedded() {
				hasEmbeds = true
			}
		}
	}
	if !hasEmbeds {
		return
	}
	return t.FieldByNameFunc(func(s string) bool { return s == name })
}

// TypeOf returns the reflection Type that represents the dynamic type of i.
// If i is a nil interface value, TypeOf returns nil.
// TypeOf返回表示i的动态类型的反射类型 如果i是nil接口值，TypeOf返回nil
func TypeOf(i interface{}) Type {
	eface := *(*emptyInterface)(unsafe.Pointer(&i))
	return toType(eface.typ)
}

// ptrMap is the cache for PtrTo.
// ptrMap是PtrTo的缓存
var ptrMap sync.Map // map[*rtype]*ptrType

// PtrTo returns the pointer type with element t.
// For example, if t represents type Foo, PtrTo(t) represents *Foo.
// PtrTo返回带有元素t的指针类型 例如，如果t表示类型Foo，则PtrTo(t)表示*Foo
func PtrTo(t Type) Type {
	return t.(*rtype).ptrTo()
}

func (t *rtype) ptrTo() *rtype {
	if t.ptrToThis != 0 {
		return t.typeOff(t.ptrToThis)
	}

	// Check the cache.
	// 检查缓存
	if pi, ok := ptrMap.Load(t); ok {
		return &pi.(*ptrType).rtype
	}

	// Look in known types.
	// 看看已知的类型
	s := "*" + t.String()
	for _, tt := range typesByString(s) {
		p := (*ptrType)(unsafe.Pointer(tt))
		if p.elem != t {
			continue
		}
		pi, _ := ptrMap.LoadOrStore(t, p)
		return &pi.(*ptrType).rtype
	}

	// Create a new ptrType starting with the description
	// of an *unsafe.Pointer.
	// 以*unsafe.Pointer的描述开头创建一个新的ptrType
	var iptr interface{} = (*unsafe.Pointer)(nil)
	prototype := *(**ptrType)(unsafe.Pointer(&iptr))
	pp := *prototype

	pp.str = resolveReflectName(newName(s, "", false))
	pp.ptrToThis = 0

	// For the type structures linked into the binary, the
	// compiler provides a good hash of the string.
	// Create a good hash for the new string by using
	// the FNV-1 hash's mixing function to combine the
	// old hash and the new "*".
	// 对于链接到二进制文件中的类型结构，编译器提供了字符串的良好散列
	// 通过使用FNV-1哈希的混合函数来组合旧的哈希和新的' * '，为新字符串创建一个好的哈希值
	pp.hash = fnv1(t.hash, '*')

	pp.elem = t

	pi, _ := ptrMap.LoadOrStore(t, &pp)
	return &pi.(*ptrType).rtype
}

// fnv1 incorporates the list of bytes into the hash x using the FNV-1 hash function.
// fnv1使用FNV-1散列函数将字节列表合并到散列x中
func fnv1(x uint32, list ...byte) uint32 {
	for _, b := range list {
		x = x*16777619 ^ uint32(b)
	}
	return x
}

func (t *rtype) Implements(u Type) bool {
	if u == nil {
		panic("reflect: nil type passed to Type.Implements")
	}
	if u.Kind() != Interface {
		panic("reflect: non-interface type passed to Type.Implements")
	}
	return implements(u.(*rtype), t)
}

func (t *rtype) AssignableTo(u Type) bool {
	if u == nil {
		panic("reflect: nil type passed to Type.AssignableTo")
	}
	uu := u.(*rtype)
	return directlyAssignable(uu, t) || implements(uu, t)
}

func (t *rtype) ConvertibleTo(u Type) bool {
	if u == nil {
		panic("reflect: nil type passed to Type.ConvertibleTo")
	}
	uu := u.(*rtype)
	return convertOp(uu, t) != nil
}

func (t *rtype) Comparable() bool {
	return t.equal != nil
}

// implements reports whether the type V implements the interface type T.
// implements报告类型V是否实现接口类型T
func implements(T, V *rtype) bool {
	if T.Kind() != Interface {
		return false
	}
	t := (*interfaceType)(unsafe.Pointer(T))
	if len(t.methods) == 0 {
		return true
	}

	// The same algorithm applies in both cases, but the
	// method tables for an interface type and a concrete type
	// are different, so the code is duplicated.
	// In both cases the algorithm is a linear scan over the two
	// lists - T's methods and V's methods - simultaneously.
	// Since method tables are stored in a unique sorted order
	// (alphabetical, with no duplicate method names), the scan
	// through V's methods must hit a match for each of T's
	// methods along the way, or else V does not implement T.
	// This lets us run the scan in overall linear time instead of
	// the quadratic time  a naive search would require.
	// 同样的算法适用于这两种情况，但是接口类型和具体类型的方法表是不同的，因此代码是重复的
	// 在这两种情况下，算法都是对两个列表——T的方法和V的方法——同时进行线性扫描
	// 因为方法表是按唯一的排序顺序存储的(字母顺序，没有重复的方法名)，所以扫描V的方法必须在整个过程中匹配T的每个方法，否则V不会实现T
	// 这让我们在整体线性时间内运行扫描，而不是简单搜索所需要的二次元时间
	// See also ../runtime/iface.go.
	if V.Kind() == Interface {
		v := (*interfaceType)(unsafe.Pointer(V))
		i := 0
		for j := 0; j < len(v.methods); j++ {
			tm := &t.methods[i]
			tmName := t.nameOff(tm.name)
			vm := &v.methods[j]
			vmName := V.nameOff(vm.name)
			if vmName.name() == tmName.name() && V.typeOff(vm.typ) == t.typeOff(tm.typ) {
				if !tmName.isExported() {
					tmPkgPath := tmName.pkgPath()
					if tmPkgPath == "" {
						tmPkgPath = t.pkgPath.name()
					}
					vmPkgPath := vmName.pkgPath()
					if vmPkgPath == "" {
						vmPkgPath = v.pkgPath.name()
					}
					if tmPkgPath != vmPkgPath {
						continue
					}
				}
				if i++; i >= len(t.methods) {
					return true
				}
			}
		}
		return false
	}

	v := V.uncommon()
	if v == nil {
		return false
	}
	i := 0
	vmethods := v.methods()
	for j := 0; j < int(v.mcount); j++ {
		tm := &t.methods[i]
		tmName := t.nameOff(tm.name)
		vm := vmethods[j]
		vmName := V.nameOff(vm.name)
		if vmName.name() == tmName.name() && V.typeOff(vm.mtyp) == t.typeOff(tm.typ) {
			if !tmName.isExported() {
				tmPkgPath := tmName.pkgPath()
				if tmPkgPath == "" {
					tmPkgPath = t.pkgPath.name()
				}
				vmPkgPath := vmName.pkgPath()
				if vmPkgPath == "" {
					vmPkgPath = V.nameOff(v.pkgPath).name()
				}
				if tmPkgPath != vmPkgPath {
					continue
				}
			}
			if i++; i >= len(t.methods) {
				return true
			}
		}
	}
	return false
}

// specialChannelAssignability reports whether a value x of channel type V
// can be directly assigned (using memmove) to another channel type T.
// specialChannelAssignability报告是否可以将通道类型V的值x直接分配(使用memmove)给另一个通道类型T
// https://golang.org/doc/go_spec.html#Assignability
// T and V must be both of Chan kind.
// T和V一定都是成龙类型的
func specialChannelAssignability(T, V *rtype) bool {
	// Special case:
	// x is a bidirectional channel value, T is a channel type,
	// x's type V and T have identical element types,
	// and at least one of V or T is not a defined type.
	// x是双向通道值，T是通道类型，x的类型V和T具有相同的元素类型，并且V或T中至少有一个不是定义的类型
	return V.ChanDir() == BothDir && (T.Name() == "" || V.Name() == "") && haveIdenticalType(T.Elem(), V.Elem(), true)
}

// directlyAssignable reports whether a value x of type V can be directly
// assigned (using memmove) to a value of type T.
// directlyAssignable报告是否可以将类型V的值x直接赋值(使用memmove)给类型T的值
// https://golang.org/doc/go_spec.html#Assignability
// Ignoring the interface rules (implemented elsewhere)
// and the ideal constant rules (no ideal constants at run time).
// 忽略接口规则(在其他地方实现)和理想常量规则(在运行时没有理想常量)
func directlyAssignable(T, V *rtype) bool {
	// x's type V is identical to T?
	// x的V和T是相同的吗?
	if T == V {
		return true
	}

	// Otherwise at least one of T and V must not be defined
	// and they must have the same kind.
	// 否则T和V中至少有一个是没有定义的它们必须具有相同的类型
	if T.hasName() && V.hasName() || T.Kind() != V.Kind() {
		return false
	}

	if T.Kind() == Chan && specialChannelAssignability(T, V) {
		return true
	}

	// x's type T and V must have identical underlying types.
	// x的类型T和V必须具有相同的基础类型
	return haveIdenticalUnderlyingType(T, V, true)
}

func haveIdenticalType(T, V Type, cmpTags bool) bool {
	if cmpTags {
		return T == V
	}

	if T.Name() != V.Name() || T.Kind() != V.Kind() || T.PkgPath() != V.PkgPath() {
		return false
	}

	return haveIdenticalUnderlyingType(T.common(), V.common(), false)
}

func haveIdenticalUnderlyingType(T, V *rtype, cmpTags bool) bool {
	if T == V {
		return true
	}

	kind := T.Kind()
	if kind != V.Kind() {
		return false
	}

	// Non-composite types of equal kind have same underlying type
	// (the predefined instance of the type).
	// 相同类型的非复合类型具有相同的基础类型(类型的预定义实例)
	if Bool <= kind && kind <= Complex128 || kind == String || kind == UnsafePointer {
		return true
	}

	// Composite types.
	// 复合类型
	switch kind {
	case Array:
		return T.Len() == V.Len() && haveIdenticalType(T.Elem(), V.Elem(), cmpTags)

	case Chan:
		return V.ChanDir() == T.ChanDir() && haveIdenticalType(T.Elem(), V.Elem(), cmpTags)

	case Func:
		t := (*funcType)(unsafe.Pointer(T))
		v := (*funcType)(unsafe.Pointer(V))
		if t.outCount != v.outCount || t.inCount != v.inCount {
			return false
		}
		for i := 0; i < t.NumIn(); i++ {
			if !haveIdenticalType(t.In(i), v.In(i), cmpTags) {
				return false
			}
		}
		for i := 0; i < t.NumOut(); i++ {
			if !haveIdenticalType(t.Out(i), v.Out(i), cmpTags) {
				return false
			}
		}
		return true

	case Interface:
		t := (*interfaceType)(unsafe.Pointer(T))
		v := (*interfaceType)(unsafe.Pointer(V))
		if len(t.methods) == 0 && len(v.methods) == 0 {
			return true
		}
		// Might have the same methods but still
		// need a run time conversion.
		// 可能具有相同的方法，但仍然需要运行时转换
		return false

	case Map:
		return haveIdenticalType(T.Key(), V.Key(), cmpTags) && haveIdenticalType(T.Elem(), V.Elem(), cmpTags)

	case Ptr, Slice:
		return haveIdenticalType(T.Elem(), V.Elem(), cmpTags)

	case Struct:
		t := (*structType)(unsafe.Pointer(T))
		v := (*structType)(unsafe.Pointer(V))
		if len(t.fields) != len(v.fields) {
			return false
		}
		if t.pkgPath.name() != v.pkgPath.name() {
			return false
		}
		for i := range t.fields {
			tf := &t.fields[i]
			vf := &v.fields[i]
			if tf.name.name() != vf.name.name() {
				return false
			}
			if !haveIdenticalType(tf.typ, vf.typ, cmpTags) {
				return false
			}
			if cmpTags && tf.name.tag() != vf.name.tag() {
				return false
			}
			if tf.offsetEmbed != vf.offsetEmbed {
				return false
			}
		}
		return true
	}

	return false
}

// typelinks is implemented in package runtime.
// It returns a slice of the sections in each module,
// and a slice of *rtype offsets in each module.
// 类型链接是在包运行时实现的 它返回每个模块中的一个section切片，并在每个模块中返回一个*rtype的offset切片
//
// The types in each module are sorted by string. That is, the first
// two linked types of the first module are:
// 每个模块中的类型按字符串排序 即第一个模块的前两种链接类型为:
//
//	d0 := sections[0]
//	t1 := (*rtype)(add(d0, offset[0][0]))
//	t2 := (*rtype)(add(d0, offset[0][1]))
//
// and
//
//	t1.String() < t2.String()
//
// Note that strings are not unique identifiers for types:
// there can be more than one with a given string.
// Only types we might want to look up are included:
// pointers, channels, maps, slices, and arrays.
// 注意，字符串不是类型的唯一标识符:对于给定的字符串，可以有多个
// 只包括我们可能想要查找的类型:指针、通道、映射、片和数组
func typelinks() (sections []unsafe.Pointer, offset [][]int32)

func rtypeOff(section unsafe.Pointer, off int32) *rtype {
	return (*rtype)(add(section, uintptr(off), "sizeof(rtype) > 0"))
}

// typesByString returns the subslice of typelinks() whose elements have
// the given string representation.
// It may be empty (no known types with that string) or may have
// multiple elements (multiple types with that string).
// typesByString返回typelinks()的子片段，它的元素具有给定的字符串表示形式
// 它可以是空的(该字符串没有已知的类型)，也可以有多个元素(该字符串有多个类型)
func typesByString(s string) []*rtype {
	sections, offset := typelinks()
	var ret []*rtype

	for offsI, offs := range offset {
		section := sections[offsI]

		// We are looking for the first index i where the string becomes >= s.
		// This is a copy of sort.Search, with f(h) replaced by (*typ[h].String() >= s).
		// 我们正在寻找字符串变成= s的第一个索引i
		// 这是sort的副本 将f(h)替换为(*typ[h]. string () = s)
		i, j := 0, len(offs)
		for i < j {
			h := i + (j-i)>>1 // avoid overflow when computing h
			// i ≤ h < j
			if !(rtypeOff(section, offs[h]).String() >= s) {
				i = h + 1 // preserves f(i-1) == false
			} else {
				j = h // preserves f(j) == true
			}
		}
		// i == j, f(i-1) == false, and f(j) (= f(i)) == true  =>  answer is i.

		// Having found the first, linear scan forward to find the last.
		// We could do a second binary search, but the caller is going
		// to do a linear scan anyway.
		// 找到第一个后，向前线性扫描找到最后一个
		// 我们可以再做一次二分查找，但调用者还是会做一次线性扫描
		for j := i; j < len(offs); j++ {
			typ := rtypeOff(section, offs[j])
			if typ.String() != s {
				break
			}
			ret = append(ret, typ)
		}
	}
	return ret
}

// The lookupCache caches ArrayOf, ChanOf, MapOf and SliceOf lookups.
// lookupCache缓存ArrayOf, ChanOf, MapOf和SliceOf的查找
var lookupCache sync.Map // map[cacheKey]*rtype

// A cacheKey is the key for use in the lookupCache.
// Four values describe any of the types we are looking for:
// type kind, one or two subtypes, and an extra integer.
// cacheKey是lookupCache中使用的键 四个值描述我们正在寻找的任何类型:类型类型、一个或两个子类型和一个额外的整数
type cacheKey struct {
	kind  Kind
	t1    *rtype
	t2    *rtype
	extra uintptr
}

// The funcLookupCache caches FuncOf lookups.
// FuncOf does not share the common lookupCache since cacheKey is not
// sufficient to represent functions unambiguously.
// funcLookupCache缓存FuncOf查找 FuncOf不共享公共的lookupCache，因为cacheKey不足以明确地表示函数
var funcLookupCache struct {
	sync.Mutex // Guards stores (but not loads) on m.

	// m is a map[uint32][]*rtype keyed by the hash calculated in FuncOf.
	// Elements of m are append-only and thus safe for concurrent reading.
	// m是一个映射[uint32][]*rtype，由FuncOf中计算的散列键值确定
	// m的元素是只能追加的，因此对于并发读取是安全的
	m sync.Map
}

// ChanOf returns the channel type with the given direction and element type.
// For example, if t represents int, ChanOf(RecvDir, t) represents <-chan int.
// ChanOf返回带有给定方向和元素类型的通道类型
// 例如，如果t表示int, ChanOf(RecvDir, t)表示-chan int
//
// The gc runtime imposes a limit of 64 kB on channel element types.
// If t's size is equal to or exceeds this limit, ChanOf panics.
// gc运行时对通道元素类型施加64 kB的限制
// 如果t的大小等于或超过这个限制，ChanOf会变差
func ChanOf(dir ChanDir, t Type) Type {
	typ := t.(*rtype)

	// Look in cache.
	// 在缓存中
	ckey := cacheKey{Chan, typ, nil, uintptr(dir)}
	if ch, ok := lookupCache.Load(ckey); ok {
		return ch.(*rtype)
	}

	// This restriction is imposed by the gc compiler and the runtime.
	// 这个限制是由gc编译器和运行时施加的
	if typ.size >= 1<<16 {
		panic("reflect.ChanOf: element size too large")
	}

	// Look in known types.
	// 看看已知的类型
	var s string
	switch dir {
	default:
		panic("reflect.ChanOf: invalid dir")
	case SendDir:
		s = "chan<- " + typ.String()
	case RecvDir:
		s = "<-chan " + typ.String()
	case BothDir:
		typeStr := typ.String()
		if typeStr[0] == '<' {
			// typ is recv chan, need parentheses as "<-" associates with leftmost
			// chan possible, see:
			// Typ是recv chan，需要圆括号作为' - '与最左边的chan可能，参见:
			// * https://golang.org/ref/spec#Channel_types
			// * https://github.com/golang/go/issues/39897
			s = "chan (" + typeStr + ")"
		} else {
			s = "chan " + typeStr
		}
	}
	for _, tt := range typesByString(s) {
		ch := (*chanType)(unsafe.Pointer(tt))
		if ch.elem == typ && ch.dir == uintptr(dir) {
			ti, _ := lookupCache.LoadOrStore(ckey, tt)
			return ti.(Type)
		}
	}

	// Make a channel type.
	// 制作通道类型
	var ichan interface{} = (chan unsafe.Pointer)(nil)
	prototype := *(**chanType)(unsafe.Pointer(&ichan))
	ch := *prototype
	ch.tflag = tflagRegularMemory
	ch.dir = uintptr(dir)
	ch.str = resolveReflectName(newName(s, "", false))
	ch.hash = fnv1(typ.hash, 'c', byte(dir))
	ch.elem = typ

	ti, _ := lookupCache.LoadOrStore(ckey, &ch.rtype)
	return ti.(Type)
}

// MapOf returns the map type with the given key and element types.
// For example, if k represents int and e represents string,
// MapOf(k, e) represents map[int]string.
// MapOf返回具有给定键和元素类型的映射类型
// 例如，如果k代表int和e代表字符串，MapOf(k, e)代表map[int]字符串
//
// If the key type is not a valid map key type (that is, if it does
// not implement Go's == operator), MapOf panics.
// 如果键类型不是有效的映射键类型(也就是说，如果它没有实现Go的==操作符)，MapOf就会panic
func MapOf(key, elem Type) Type {
	ktyp := key.(*rtype)
	etyp := elem.(*rtype)

	if ktyp.equal == nil {
		panic("reflect.MapOf: invalid key type " + ktyp.String())
	}

	// Look in cache.
	// 在缓存中
	ckey := cacheKey{Map, ktyp, etyp, 0}
	if mt, ok := lookupCache.Load(ckey); ok {
		return mt.(Type)
	}

	// Look in known types.
	// 看看已知的类型
	s := "map[" + ktyp.String() + "]" + etyp.String()
	for _, tt := range typesByString(s) {
		mt := (*mapType)(unsafe.Pointer(tt))
		if mt.key == ktyp && mt.elem == etyp {
			ti, _ := lookupCache.LoadOrStore(ckey, tt)
			return ti.(Type)
		}
	}

	// Make a map type.
	// Note: flag values must match those used in the TMAP case
	// 制作一个地图类型 注意:标志值必须与TMAP中使用的值匹配
	// in ../cmd/compile/internal/reflectdata/reflect.go:writeType.
	var imap interface{} = (map[unsafe.Pointer]unsafe.Pointer)(nil)
	mt := **(**mapType)(unsafe.Pointer(&imap))
	mt.str = resolveReflectName(newName(s, "", false))
	mt.tflag = 0
	mt.hash = fnv1(etyp.hash, 'm', byte(ktyp.hash>>24), byte(ktyp.hash>>16), byte(ktyp.hash>>8), byte(ktyp.hash))
	mt.key = ktyp
	mt.elem = etyp
	mt.bucket = bucketOf(ktyp, etyp)
	mt.hasher = func(p unsafe.Pointer, seed uintptr) uintptr {
		return typehash(ktyp, p, seed)
	}
	mt.flags = 0
	if ktyp.size > maxKeySize {
		mt.keysize = uint8(ptrSize)
		mt.flags |= 1 // indirect key
	} else {
		mt.keysize = uint8(ktyp.size)
	}
	if etyp.size > maxValSize {
		mt.valuesize = uint8(ptrSize)
		mt.flags |= 2 // indirect value
	} else {
		mt.valuesize = uint8(etyp.size)
	}
	mt.bucketsize = uint16(mt.bucket.size)
	if isReflexive(ktyp) {
		mt.flags |= 4
	}
	if needKeyUpdate(ktyp) {
		mt.flags |= 8
	}
	if hashMightPanic(ktyp) {
		mt.flags |= 16
	}
	mt.ptrToThis = 0

	ti, _ := lookupCache.LoadOrStore(ckey, &mt.rtype)
	return ti.(Type)
}

// TODO(crawshaw): as these funcTypeFixedN structs have no methods,
// they could be defined at runtime using the StructOf function.
// TODO(crawshaw):因为这些funcTypeFixedN结构没有方法，所以可以在运行时使用StructOf函数定义它们
type funcTypeFixed4 struct {
	funcType
	args [4]*rtype
}
type funcTypeFixed8 struct {
	funcType
	args [8]*rtype
}
type funcTypeFixed16 struct {
	funcType
	args [16]*rtype
}
type funcTypeFixed32 struct {
	funcType
	args [32]*rtype
}
type funcTypeFixed64 struct {
	funcType
	args [64]*rtype
}
type funcTypeFixed128 struct {
	funcType
	args [128]*rtype
}

// FuncOf returns the function type with the given argument and result types.
// For example if k represents int and e represents string,
// FuncOf([]Type{k}, []Type{e}, false) represents func(int) string.
// FuncOf返回带有给定参数和结果类型的函数类型
// 例如，k表示int, e表示string, FuncOf([]Type{k}， []Type{e}， false)表示func(int)字符串
//
// The variadic argument controls whether the function is variadic. FuncOf
// panics if the in[len(in)-1] does not represent a slice and variadic is
// true.
// 可变参数控制函数是否为可变参数 如果in[len(in)-1]不表示一个切片并且可变参数为真，FuncOf会出错
func FuncOf(in, out []Type, variadic bool) Type {
	if variadic && (len(in) == 0 || in[len(in)-1].Kind() != Slice) {
		panic("reflect.FuncOf: last arg of variadic func must be slice")
	}

	// Make a func type.
	var ifunc interface{} = (func())(nil)
	prototype := *(**funcType)(unsafe.Pointer(&ifunc))
	n := len(in) + len(out)

	var ft *funcType
	var args []*rtype
	switch {
	case n <= 4:
		fixed := new(funcTypeFixed4)
		args = fixed.args[:0:len(fixed.args)]
		ft = &fixed.funcType
	case n <= 8:
		fixed := new(funcTypeFixed8)
		args = fixed.args[:0:len(fixed.args)]
		ft = &fixed.funcType
	case n <= 16:
		fixed := new(funcTypeFixed16)
		args = fixed.args[:0:len(fixed.args)]
		ft = &fixed.funcType
	case n <= 32:
		fixed := new(funcTypeFixed32)
		args = fixed.args[:0:len(fixed.args)]
		ft = &fixed.funcType
	case n <= 64:
		fixed := new(funcTypeFixed64)
		args = fixed.args[:0:len(fixed.args)]
		ft = &fixed.funcType
	case n <= 128:
		fixed := new(funcTypeFixed128)
		args = fixed.args[:0:len(fixed.args)]
		ft = &fixed.funcType
	default:
		panic("reflect.FuncOf: too many arguments")
	}
	*ft = *prototype

	// Build a hash and minimally populate ft.
	// 构建一个散列并最小填充ft
	var hash uint32
	for _, in := range in {
		t := in.(*rtype)
		args = append(args, t)
		hash = fnv1(hash, byte(t.hash>>24), byte(t.hash>>16), byte(t.hash>>8), byte(t.hash))
	}
	if variadic {
		hash = fnv1(hash, 'v')
	}
	hash = fnv1(hash, '.')
	for _, out := range out {
		t := out.(*rtype)
		args = append(args, t)
		hash = fnv1(hash, byte(t.hash>>24), byte(t.hash>>16), byte(t.hash>>8), byte(t.hash))
	}
	if len(args) > 50 {
		panic("reflect.FuncOf does not support more than 50 arguments")
	}
	ft.tflag = 0
	ft.hash = hash
	ft.inCount = uint16(len(in))
	ft.outCount = uint16(len(out))
	if variadic {
		ft.outCount |= 1 << 15
	}

	// Look in cache.
	// 在缓存中
	if ts, ok := funcLookupCache.m.Load(hash); ok {
		for _, t := range ts.([]*rtype) {
			if haveIdenticalUnderlyingType(&ft.rtype, t, true) {
				return t
			}
		}
	}

	// Not in cache, lock and retry.
	// 不在缓存中，锁定并重试
	funcLookupCache.Lock()
	defer funcLookupCache.Unlock()
	if ts, ok := funcLookupCache.m.Load(hash); ok {
		for _, t := range ts.([]*rtype) {
			if haveIdenticalUnderlyingType(&ft.rtype, t, true) {
				return t
			}
		}
	}

	addToCache := func(tt *rtype) Type {
		var rts []*rtype
		if rti, ok := funcLookupCache.m.Load(hash); ok {
			rts = rti.([]*rtype)
		}
		funcLookupCache.m.Store(hash, append(rts, tt))
		return tt
	}

	// Look in known types for the same string representation.
	// 在已知类型中查找相同的字符串表示形式
	str := funcStr(ft)
	for _, tt := range typesByString(str) {
		if haveIdenticalUnderlyingType(&ft.rtype, tt, true) {
			return addToCache(tt)
		}
	}

	// Populate the remaining fields of ft and store in cache.
	// 填充ft的剩余字段并存储在缓存中
	ft.str = resolveReflectName(newName(str, "", false))
	ft.ptrToThis = 0
	return addToCache(&ft.rtype)
}

// funcStr builds a string representation of a funcType.
// funcStr构建funcType的字符串表示形式
func funcStr(ft *funcType) string {
	repr := make([]byte, 0, 64)
	repr = append(repr, "func("...)
	for i, t := range ft.in() {
		if i > 0 {
			repr = append(repr, ", "...)
		}
		if ft.IsVariadic() && i == int(ft.inCount)-1 {
			repr = append(repr, "..."...)
			repr = append(repr, (*sliceType)(unsafe.Pointer(t)).elem.String()...)
		} else {
			repr = append(repr, t.String()...)
		}
	}
	repr = append(repr, ')')
	out := ft.out()
	if len(out) == 1 {
		repr = append(repr, ' ')
	} else if len(out) > 1 {
		repr = append(repr, " ("...)
	}
	for i, t := range out {
		if i > 0 {
			repr = append(repr, ", "...)
		}
		repr = append(repr, t.String()...)
	}
	if len(out) > 1 {
		repr = append(repr, ')')
	}
	return string(repr)
}

// isReflexive reports whether the == operation on the type is reflexive.
// That is, x == x for all values x of type t.
// isReflexive报告该类型的==操作是否是自反的
// 也就是说，对于所有类型t的值x, x == x
func isReflexive(t *rtype) bool {
	switch t.Kind() {
	case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Uintptr, Chan, Ptr, String, UnsafePointer:
		return true
	case Float32, Float64, Complex64, Complex128, Interface:
		return false
	case Array:
		tt := (*arrayType)(unsafe.Pointer(t))
		return isReflexive(tt.elem)
	case Struct:
		tt := (*structType)(unsafe.Pointer(t))
		for _, f := range tt.fields {
			if !isReflexive(f.typ) {
				return false
			}
		}
		return true
	default:
		// Func, Map, Slice, Invalid
		// Func, Map, Slice，无效
		panic("isReflexive called on non-key type " + t.String())
	}
}

// needKeyUpdate reports whether map overwrites require the key to be copied.
// needKeyUpdate报告map覆盖是否需要复制密钥
func needKeyUpdate(t *rtype) bool {
	switch t.Kind() {
	case Bool, Int, Int8, Int16, Int32, Int64, Uint, Uint8, Uint16, Uint32, Uint64, Uintptr, Chan, Ptr, UnsafePointer:
		return false
	case Float32, Float64, Complex64, Complex128, Interface, String:
		// Float keys can be updated from +0 to -0.
		// String keys can be updated to use a smaller backing store.
		// Interfaces might have floats of strings in them.
		// 浮动键可以从+0更新到-0 字符串键可以更新为使用较小的支持存储
		// 接口中可能有浮点字符串
		return true
	case Array:
		tt := (*arrayType)(unsafe.Pointer(t))
		return needKeyUpdate(tt.elem)
	case Struct:
		tt := (*structType)(unsafe.Pointer(t))
		for _, f := range tt.fields {
			if needKeyUpdate(f.typ) {
				return true
			}
		}
		return false
	default:
		// Func, Map, Slice, Invalid
		// Func, Map, Slice，无效
		panic("needKeyUpdate called on non-key type " + t.String())
	}
}

// hashMightPanic reports whether the hash of a map key of type t might panic.
// hashMightPanic报告类型为t的映射键的散列是否可能为panic
func hashMightPanic(t *rtype) bool {
	switch t.Kind() {
	case Interface:
		return true
	case Array:
		tt := (*arrayType)(unsafe.Pointer(t))
		return hashMightPanic(tt.elem)
	case Struct:
		tt := (*structType)(unsafe.Pointer(t))
		for _, f := range tt.fields {
			if hashMightPanic(f.typ) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

// Make sure these routines stay in sync with ../../runtime/map.go!
// These types exist only for GC, so we only fill out GC relevant info.
// Currently, that's just size and the GC program. We also fill in string
// for possible debugging use.
// 确保这些例程与../../runtime/map.go保持同步!这些类型只针对GC存在，所以我们只填写GC相关信息
// 目前，这只是大小和GC程序 为了可能的调试使用，我们还填充了字符串
const (
	bucketSize uintptr = 8
	maxKeySize uintptr = 128
	maxValSize uintptr = 128
)

func bucketOf(ktyp, etyp *rtype) *rtype {
	if ktyp.size > maxKeySize {
		ktyp = PtrTo(ktyp).(*rtype)
	}
	if etyp.size > maxValSize {
		etyp = PtrTo(etyp).(*rtype)
	}

	// Prepare GC data if any.
	// A bucket is at most bucketSize*(1+maxKeySize+maxValSize)+2*ptrSize bytes,
	// or 2072 bytes, or 259 pointer-size words, or 33 bytes of pointer bitmap.
	// Note that since the key and value are known to be <= 128 bytes,
	// they're guaranteed to have bitmaps instead of GC programs.
	// 如果有，准备GC数据 一个桶最多支持bucketSize*(1+maxKeySize+maxValSize)+2*ptrSize字节，即2072字节，或259个指针大小的单词，或33个指针位图字节
	// 注意，因为键和值已知为= 128字节，所以保证它们使用位图而不是GC程序
	var gcdata *byte
	var ptrdata uintptr
	var overflowPad uintptr

	size := bucketSize*(1+ktyp.size+etyp.size) + overflowPad + ptrSize
	if size&uintptr(ktyp.align-1) != 0 || size&uintptr(etyp.align-1) != 0 {
		panic("reflect: bad size computation in MapOf")
	}

	if ktyp.ptrdata != 0 || etyp.ptrdata != 0 {
		nptr := (bucketSize*(1+ktyp.size+etyp.size) + ptrSize) / ptrSize
		mask := make([]byte, (nptr+7)/8)
		base := bucketSize / ptrSize

		if ktyp.ptrdata != 0 {
			emitGCMask(mask, base, ktyp, bucketSize)
		}
		base += bucketSize * ktyp.size / ptrSize

		if etyp.ptrdata != 0 {
			emitGCMask(mask, base, etyp, bucketSize)
		}
		base += bucketSize * etyp.size / ptrSize
		base += overflowPad / ptrSize

		word := base
		mask[word/8] |= 1 << (word % 8)
		gcdata = &mask[0]
		ptrdata = (word + 1) * ptrSize

		// overflow word must be last
		// 溢出字必须放在最后
		if ptrdata != size {
			panic("reflect: bad layout computation in MapOf")
		}
	}

	b := &rtype{
		align:   ptrSize,
		size:    size,
		kind:    uint8(Struct),
		ptrdata: ptrdata,
		gcdata:  gcdata,
	}
	if overflowPad > 0 {
		b.align = 8
	}
	s := "bucket(" + ktyp.String() + "," + etyp.String() + ")"
	b.str = resolveReflectName(newName(s, "", false))
	return b
}

func (t *rtype) gcSlice(begin, end uintptr) []byte {
	return (*[1 << 30]byte)(unsafe.Pointer(t.gcdata))[begin:end:end]
}

// emitGCMask writes the GC mask for [n]typ into out, starting at bit
// offset base.
// emitGCMask将[n]typ的GC掩码写入到out，从位偏移基数开始
func emitGCMask(out []byte, base uintptr, typ *rtype, n uintptr) {
	if typ.kind&kindGCProg != 0 {
		panic("reflect: unexpected GC program")
	}
	ptrs := typ.ptrdata / ptrSize
	words := typ.size / ptrSize
	mask := typ.gcSlice(0, (ptrs+7)/8)
	for j := uintptr(0); j < ptrs; j++ {
		if (mask[j/8]>>(j%8))&1 != 0 {
			for i := uintptr(0); i < n; i++ {
				k := base + i*words + j
				out[k/8] |= 1 << (k % 8)
			}
		}
	}
}

// appendGCProg appends the GC program for the first ptrdata bytes of
// typ to dst and returns the extended slice.
// appendGCProg将typ的第一个ptrdata字节的GC程序追加到dst，并返回扩展片
func appendGCProg(dst []byte, typ *rtype) []byte {
	if typ.kind&kindGCProg != 0 {
		// Element has GC program; emit one element.
		// 元素有GC程序发出一个元素
		n := uintptr(*(*uint32)(unsafe.Pointer(typ.gcdata)))
		prog := typ.gcSlice(4, 4+n-1)
		return append(dst, prog...)
	}

	// Element is small with pointer mask; use as literal bits.
	// 元素很小，指针掩码用作文字位
	ptrs := typ.ptrdata / ptrSize
	mask := typ.gcSlice(0, (ptrs+7)/8)

	// Emit 120-bit chunks of full bytes (max is 127 but we avoid using partial bytes).
	// 产生120位的完整字节块(max为127，但我们避免使用部分字节)
	for ; ptrs > 120; ptrs -= 120 {
		dst = append(dst, 120)
		dst = append(dst, mask[:15]...)
		mask = mask[15:]
	}

	dst = append(dst, byte(ptrs))
	dst = append(dst, mask...)
	return dst
}

// SliceOf returns the slice type with element type t.
// For example, if t represents int, SliceOf(t) represents []int.
// SliceOf返回元素类型为t的切片类型 例如，如果t表示int，则SliceOf(t)表示[]int
func SliceOf(t Type) Type {
	typ := t.(*rtype)

	// Look in cache.
	// 在缓存中
	ckey := cacheKey{Slice, typ, nil, 0}
	if slice, ok := lookupCache.Load(ckey); ok {
		return slice.(Type)
	}

	// Look in known types.
	// 看看已知的类型
	s := "[]" + typ.String()
	for _, tt := range typesByString(s) {
		slice := (*sliceType)(unsafe.Pointer(tt))
		if slice.elem == typ {
			ti, _ := lookupCache.LoadOrStore(ckey, tt)
			return ti.(Type)
		}
	}

	// Make a slice type.
	// 制作切片类型
	var islice interface{} = ([]unsafe.Pointer)(nil)
	prototype := *(**sliceType)(unsafe.Pointer(&islice))
	slice := *prototype
	slice.tflag = 0
	slice.str = resolveReflectName(newName(s, "", false))
	slice.hash = fnv1(typ.hash, '[')
	slice.elem = typ
	slice.ptrToThis = 0

	ti, _ := lookupCache.LoadOrStore(ckey, &slice.rtype)
	return ti.(Type)
}

// The structLookupCache caches StructOf lookups.
// StructOf does not share the common lookupCache since we need to pin
// the memory associated with *structTypeFixedN.
// structLookupCache缓存StructOf查找 StructOf不共享公共的lookupCache，因为我们需要固定与*structTypeFixedN关联的内存
var structLookupCache struct {
	sync.Mutex // Guards stores (but not loads) on m.

	// m is a map[uint32][]Type keyed by the hash calculated in StructOf.
	// Elements in m are append-only and thus safe for concurrent reading.
	// m是一个映射[uint32][]由StructOf中计算的散列键值确定的类型
	// m中的元素是只能追加的，因此对于并发读取是安全的
	m sync.Map
}

type structTypeUncommon struct {
	structType
	u uncommonType
}

// isLetter reports whether a given 'rune' is classified as a Letter.
// isLetter报告一个给定的“符文”是否被归类为字母
func isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_' || ch >= utf8.RuneSelf && unicode.IsLetter(ch)
}

// isValidFieldName checks if a string is a valid (struct) field name or not.
// isLetter报告一个给定的“符文”是否被归类为字母
//
// According to the language spec, a field name should be an identifier.
// 根据语言规范，字段名应该是一个标识符
//
// identifier = letter { letter | unicode_digit } .
// letter = unicode_letter | "_" .
func isValidFieldName(fieldName string) bool {
	for i, c := range fieldName {
		if i == 0 && !isLetter(c) {
			return false
		}

		if !(isLetter(c) || unicode.IsDigit(c)) {
			return false
		}
	}

	return len(fieldName) > 0
}

// StructOf returns the struct type containing fields.
// The Offset and Index fields are ignored and computed as they would be
// by the compiler.
// StructOf返回包含字段的结构类型 Offset和Index字段被忽略，并按编译器的方式进行计算
//
// StructOf currently does not generate wrapper methods for embedded
// fields and panics if passed unexported StructFields.
// These limitations may be lifted in a future version.
// StructOf目前不生成嵌入字段的包装器方法，如果传入未导出的StructFields则会产生恐慌
// 这些限制可能在未来的版本中被取消
func StructOf(fields []StructField) Type {
	var (
		hash       = fnv1(0, []byte("struct {")...)
		size       uintptr
		typalign   uint8
		comparable = true
		methods    []method

		fs   = make([]structField, len(fields))
		repr = make([]byte, 0, 64)
		fset = map[string]struct{}{} // fields' names

		hasGCProg = false // records whether a struct-field type has a GCProg
	)

	lastzero := uintptr(0)
	repr = append(repr, "struct {"...)
	pkgpath := ""
	for i, field := range fields {
		if field.Name == "" {
			panic("reflect.StructOf: field " + strconv.Itoa(i) + " has no name")
		}
		if !isValidFieldName(field.Name) {
			panic("reflect.StructOf: field " + strconv.Itoa(i) + " has invalid name")
		}
		if field.Type == nil {
			panic("reflect.StructOf: field " + strconv.Itoa(i) + " has no type")
		}
		f, fpkgpath := runtimeStructField(field)
		ft := f.typ
		if ft.kind&kindGCProg != 0 {
			hasGCProg = true
		}
		if fpkgpath != "" {
			if pkgpath == "" {
				pkgpath = fpkgpath
			} else if pkgpath != fpkgpath {
				panic("reflect.Struct: fields with different PkgPath " + pkgpath + " and " + fpkgpath)
			}
		}

		// Update string and hash
		// 更新字符串和散列
		name := f.name.name()
		hash = fnv1(hash, []byte(name)...)
		repr = append(repr, (" " + name)...)
		if f.embedded() {
			// Embedded field
			// 嵌入式领域
			if f.typ.Kind() == Ptr {
				// Embedded ** and *interface{} are illegal
				elem := ft.Elem()
				if k := elem.Kind(); k == Ptr || k == Interface {
					panic("reflect.StructOf: illegal embedded field type " + ft.String())
				}
			}

			switch f.typ.Kind() {
			case Interface:
				ift := (*interfaceType)(unsafe.Pointer(ft))
				for im, m := range ift.methods {
					if ift.nameOff(m.name).pkgPath() != "" {
						// TODO(sbinet).  Issue 15924.
						panic("reflect: embedded interface with unexported method(s) not implemented")
					}

					var (
						mtyp    = ift.typeOff(m.typ)
						ifield  = i
						imethod = im
						ifn     Value
						tfn     Value
					)

					if ft.kind&kindDirectIface != 0 {
						tfn = MakeFunc(mtyp, func(in []Value) []Value {
							var args []Value
							var recv = in[0]
							if len(in) > 1 {
								args = in[1:]
							}
							return recv.Field(ifield).Method(imethod).Call(args)
						})
						ifn = MakeFunc(mtyp, func(in []Value) []Value {
							var args []Value
							var recv = in[0]
							if len(in) > 1 {
								args = in[1:]
							}
							return recv.Field(ifield).Method(imethod).Call(args)
						})
					} else {
						tfn = MakeFunc(mtyp, func(in []Value) []Value {
							var args []Value
							var recv = in[0]
							if len(in) > 1 {
								args = in[1:]
							}
							return recv.Field(ifield).Method(imethod).Call(args)
						})
						ifn = MakeFunc(mtyp, func(in []Value) []Value {
							var args []Value
							var recv = Indirect(in[0])
							if len(in) > 1 {
								args = in[1:]
							}
							return recv.Field(ifield).Method(imethod).Call(args)
						})
					}

					methods = append(methods, method{
						name: resolveReflectName(ift.nameOff(m.name)),
						mtyp: resolveReflectType(mtyp),
						ifn:  resolveReflectText(unsafe.Pointer(&ifn)),
						tfn:  resolveReflectText(unsafe.Pointer(&tfn)),
					})
				}
			case Ptr:
				ptr := (*ptrType)(unsafe.Pointer(ft))
				if unt := ptr.uncommon(); unt != nil {
					if i > 0 && unt.mcount > 0 {
						// Issue 15924.
						panic("reflect: embedded type with methods not implemented if type is not first field")
					}
					if len(fields) > 1 {
						panic("reflect: embedded type with methods not implemented if there is more than one field")
					}
					for _, m := range unt.methods() {
						mname := ptr.nameOff(m.name)
						if mname.pkgPath() != "" {
							// TODO(sbinet).
							// Issue 15924.
							panic("reflect: embedded interface with unexported method(s) not implemented")
						}
						methods = append(methods, method{
							name: resolveReflectName(mname),
							mtyp: resolveReflectType(ptr.typeOff(m.mtyp)),
							ifn:  resolveReflectText(ptr.textOff(m.ifn)),
							tfn:  resolveReflectText(ptr.textOff(m.tfn)),
						})
					}
				}
				if unt := ptr.elem.uncommon(); unt != nil {
					for _, m := range unt.methods() {
						mname := ptr.nameOff(m.name)
						if mname.pkgPath() != "" {
							// TODO(sbinet)
							// Issue 15924.
							panic("reflect: embedded interface with unexported method(s) not implemented")
						}
						methods = append(methods, method{
							name: resolveReflectName(mname),
							mtyp: resolveReflectType(ptr.elem.typeOff(m.mtyp)),
							ifn:  resolveReflectText(ptr.elem.textOff(m.ifn)),
							tfn:  resolveReflectText(ptr.elem.textOff(m.tfn)),
						})
					}
				}
			default:
				if unt := ft.uncommon(); unt != nil {
					if i > 0 && unt.mcount > 0 {
						// Issue 15924.
						panic("reflect: embedded type with methods not implemented if type is not first field")
					}
					if len(fields) > 1 && ft.kind&kindDirectIface != 0 {
						panic("reflect: embedded type with methods not implemented for non-pointer type")
					}
					for _, m := range unt.methods() {
						mname := ft.nameOff(m.name)
						if mname.pkgPath() != "" {
							// TODO(sbinet)
							// Issue 15924.
							panic("reflect: embedded interface with unexported method(s) not implemented")
						}
						methods = append(methods, method{
							name: resolveReflectName(mname),
							mtyp: resolveReflectType(ft.typeOff(m.mtyp)),
							ifn:  resolveReflectText(ft.textOff(m.ifn)),
							tfn:  resolveReflectText(ft.textOff(m.tfn)),
						})

					}
				}
			}
		}
		if _, dup := fset[name]; dup {
			panic("reflect.StructOf: duplicate field " + name)
		}
		fset[name] = struct{}{}

		hash = fnv1(hash, byte(ft.hash>>24), byte(ft.hash>>16), byte(ft.hash>>8), byte(ft.hash))

		repr = append(repr, (" " + ft.String())...)
		if f.name.hasTag() {
			hash = fnv1(hash, []byte(f.name.tag())...)
			repr = append(repr, (" " + strconv.Quote(f.name.tag()))...)
		}
		if i < len(fields)-1 {
			repr = append(repr, ';')
		}

		comparable = comparable && (ft.equal != nil)

		offset := align(size, uintptr(ft.align))
		if ft.align > typalign {
			typalign = ft.align
		}
		size = offset + ft.size
		f.offsetEmbed |= offset << 1

		if ft.size == 0 {
			lastzero = size
		}

		fs[i] = f
	}

	if size > 0 && lastzero == size {
		// This is a non-zero sized struct that ends in a
		// zero-sized field. We add an extra byte of padding,
		// to ensure that taking the address of the final
		// zero-sized field can't manufacture a pointer to the
		// next object in the heap. See issue 9401.
		// 这是一个非零大小的结构体，以零大小的字段结束
		// 我们添加了一个额外的字节填充，以确保获取最后一个大小为零的字段的地址不能产生指向堆中下一个对象的指针
		// 看到发行9401
		size++
	}

	var typ *structType
	var ut *uncommonType

	if len(methods) == 0 {
		t := new(structTypeUncommon)
		typ = &t.structType
		ut = &t.u
	} else {
		// A *rtype representing a struct is followed directly in memory by an
		// array of method objects representing the methods attached to the
		// struct. To get the same layout for a run time generated type, we
		// need an array directly following the uncommonType memory.
		// A similar strategy is used for funcTypeFixed4, ...funcTypeFixedN.
		// 在内存中，表示结构的*rtype后面直接跟着一个方法对象数组，该数组表示附加到该结构的方法
		// 要为运行时生成的类型获得相同的布局，我们需要在非commontype内存后面直接放置一个数组
		// 类似的策略也用于funcTypeFixed4，…funcTypeFixedN
		tt := New(StructOf([]StructField{
			{Name: "S", Type: TypeOf(structType{})},
			{Name: "U", Type: TypeOf(uncommonType{})},
			{Name: "M", Type: ArrayOf(len(methods), TypeOf(methods[0]))},
		}))

		typ = (*structType)(unsafe.Pointer(tt.Elem().Field(0).UnsafeAddr()))
		ut = (*uncommonType)(unsafe.Pointer(tt.Elem().Field(1).UnsafeAddr()))

		copy(tt.Elem().Field(2).Slice(0, len(methods)).Interface().([]method), methods)
	}
	// TODO(sbinet): Once we allow embedding multiple types,
	// methods will need to be sorted like the compiler does.
	// TODO(sbinet):一旦我们允许嵌入多种类型，就需要像编译器那样对方法进行排序
	// TODO(sbinet): Once we allow non-exported methods, we will
	// need to compute xcount as the number of exported methods.
	// TODO(sbinet):一旦我们允许非导出方法，我们将需要计算xcount作为导出方法的数量
	ut.mcount = uint16(len(methods))
	ut.xcount = ut.mcount
	ut.moff = uint32(unsafe.Sizeof(uncommonType{}))

	if len(fs) > 0 {
		repr = append(repr, ' ')
	}
	repr = append(repr, '}')
	hash = fnv1(hash, '}')
	str := string(repr)

	// Round the size up to be a multiple of the alignment.
	// 将大小四舍五入为对齐方式的倍数
	size = align(size, uintptr(typalign))

	// Make the struct type.
	// 创建结构类型
	var istruct interface{} = struct{}{}
	prototype := *(**structType)(unsafe.Pointer(&istruct))
	*typ = *prototype
	typ.fields = fs
	if pkgpath != "" {
		typ.pkgPath = newName(pkgpath, "", false)
	}

	// Look in cache.
	// 在缓存中
	if ts, ok := structLookupCache.m.Load(hash); ok {
		for _, st := range ts.([]Type) {
			t := st.common()
			if haveIdenticalUnderlyingType(&typ.rtype, t, true) {
				return t
			}
		}
	}

	// Not in cache, lock and retry.
	// 不在缓存中，锁定并重试
	structLookupCache.Lock()
	defer structLookupCache.Unlock()
	if ts, ok := structLookupCache.m.Load(hash); ok {
		for _, st := range ts.([]Type) {
			t := st.common()
			if haveIdenticalUnderlyingType(&typ.rtype, t, true) {
				return t
			}
		}
	}

	addToCache := func(t Type) Type {
		var ts []Type
		if ti, ok := structLookupCache.m.Load(hash); ok {
			ts = ti.([]Type)
		}
		structLookupCache.m.Store(hash, append(ts, t))
		return t
	}

	// Look in known types.
	// 看看已知的类型
	for _, t := range typesByString(str) {
		if haveIdenticalUnderlyingType(&typ.rtype, t, true) {
			// even if 't' wasn't a structType with methods, we should be ok
			// as the 'u uncommonType' field won't be accessed except when
			// tflag&tflagUncommon is set.
			return addToCache(t)
		}
	}

	typ.str = resolveReflectName(newName(str, "", false))
	typ.tflag = 0 // TODO: set tflagRegularMemory
	typ.hash = hash
	typ.size = size
	typ.ptrdata = typeptrdata(typ.common())
	typ.align = typalign
	typ.fieldAlign = typalign
	typ.ptrToThis = 0
	if len(methods) > 0 {
		typ.tflag |= tflagUncommon
	}

	if hasGCProg {
		lastPtrField := 0
		for i, ft := range fs {
			if ft.typ.pointers() {
				lastPtrField = i
			}
		}
		prog := []byte{0, 0, 0, 0} // will be length of prog
		var off uintptr
		for i, ft := range fs {
			if i > lastPtrField {
				// gcprog should not include anything for any field after
				// the last field that contains pointer data
				// Gcprog不应该包含包含指针数据的最后一个字段之后的任何字段
				break
			}
			if !ft.typ.pointers() {
				// Ignore pointerless fields.
				// 忽略pointerless字段
				continue
			}
			// Pad to start of this field with zeros.
			// 垫到该字段以0开始
			if ft.offset() > off {
				n := (ft.offset() - off) / ptrSize
				prog = append(prog, 0x01, 0x00) // emit a 0 bit
				if n > 1 {
					prog = append(prog, 0x81)      // repeat previous bit
					prog = appendVarint(prog, n-1) // n-1 times
				}
				off = ft.offset()
			}

			prog = appendGCProg(prog, ft.typ)
			off += ft.typ.ptrdata
		}
		prog = append(prog, 0)
		*(*uint32)(unsafe.Pointer(&prog[0])) = uint32(len(prog) - 4)
		typ.kind |= kindGCProg
		typ.gcdata = &prog[0]
	} else {
		typ.kind &^= kindGCProg
		bv := new(bitVector)
		addTypeBits(bv, 0, typ.common())
		if len(bv.data) > 0 {
			typ.gcdata = &bv.data[0]
		}
	}
	typ.equal = nil
	if comparable {
		typ.equal = func(p, q unsafe.Pointer) bool {
			for _, ft := range typ.fields {
				pi := add(p, ft.offset(), "&x.field safe")
				qi := add(q, ft.offset(), "&x.field safe")
				if !ft.typ.equal(pi, qi) {
					return false
				}
			}
			return true
		}
	}

	switch {
	case len(fs) == 1 && !ifaceIndir(fs[0].typ):
		// structs of 1 direct iface type can be direct
		// 1个直接iface类型的结构体可以是直接的
		typ.kind |= kindDirectIface
	default:
		typ.kind &^= kindDirectIface
	}

	return addToCache(&typ.rtype)
}

// runtimeStructField takes a StructField value passed to StructOf and
// returns both the corresponding internal representation, of type
// structField, and the pkgpath value to use for this field.
// runtimeStructField接受传递给StructOf的StructField值，并返回相应的StructField类型的内部表示，以及用于该字段的pkgpath值
func runtimeStructField(field StructField) (structField, string) {
	if field.Anonymous && field.PkgPath != "" {
		panic("reflect.StructOf: field \"" + field.Name + "\" is anonymous but has PkgPath set")
	}

	if field.IsExported() {
		// Best-effort check for misuse.
		// Since this field will be treated as exported, not much harm done if Unicode lowercase slips through.
		// 尽最大努力检查是否误用 由于该字段将被视为导出字段，因此如果Unicode小写字母遗漏了，也不会造成太大的损害
		c := field.Name[0]
		if 'a' <= c && c <= 'z' || c == '_' {
			panic("reflect.StructOf: field \"" + field.Name + "\" is unexported but missing PkgPath")
		}
	}

	offsetEmbed := uintptr(0)
	if field.Anonymous {
		offsetEmbed |= 1
	}

	resolveReflectType(field.Type.common()) // install in runtime
	f := structField{
		name:        newName(field.Name, string(field.Tag), field.IsExported()),
		typ:         field.Type.common(),
		offsetEmbed: offsetEmbed,
	}
	return f, field.PkgPath
}

// typeptrdata returns the length in bytes of the prefix of t
// containing pointer data. Anything after this offset is scalar data.
// keep in sync with ../cmd/compile/internal/reflectdata/reflect.go
// Typeptrdata以字节为单位返回包含指针数据的t前缀的长度
// 这个偏移量之后的数据都是标量数据 与../cmd/compile/internal/reflectdata/reflect.go保持同步
func typeptrdata(t *rtype) uintptr {
	switch t.Kind() {
	case Struct:
		st := (*structType)(unsafe.Pointer(t))
		// find the last field that has pointers.
		// 找到最后一个有指针的字段
		field := -1
		for i := range st.fields {
			ft := st.fields[i].typ
			if ft.pointers() {
				field = i
			}
		}
		if field == -1 {
			return 0
		}
		f := st.fields[field]
		return f.offset() + f.typ.ptrdata

	default:
		panic("reflect.typeptrdata: unexpected type, " + t.String())
	}
}

// See cmd/compile/internal/reflectdata/reflect.go for derivation of constant.
// 看到cmd /编译/内部/ reflectdata /反映 求常数的导数
const maxPtrmaskBytes = 2048

// ArrayOf returns the array type with the given length and element type.
// For example, if t represents int, ArrayOf(5, t) represents [5]int.
// ArrayOf返回给定长度和元素类型的数组类型
// 例如，如果t表示int，则ArrayOf(5, t)表示[5]int
//
// If the resulting type would be larger than the available address space,
// ArrayOf panics.
// 如果结果类型大于可用的地址空间，则ArrayOf会发生故障
func ArrayOf(length int, elem Type) Type {
	if length < 0 {
		panic("reflect: negative length passed to ArrayOf")
	}

	typ := elem.(*rtype)

	// Look in cache.
	// 在缓存中
	ckey := cacheKey{Array, typ, nil, uintptr(length)}
	if array, ok := lookupCache.Load(ckey); ok {
		return array.(Type)
	}

	// Look in known types.
	// 看看已知的类型
	s := "[" + strconv.Itoa(length) + "]" + typ.String()
	for _, tt := range typesByString(s) {
		array := (*arrayType)(unsafe.Pointer(tt))
		if array.elem == typ {
			ti, _ := lookupCache.LoadOrStore(ckey, tt)
			return ti.(Type)
		}
	}

	// Make an array type.
	// 创建一个数组类型
	var iarray interface{} = [1]unsafe.Pointer{}
	prototype := *(**arrayType)(unsafe.Pointer(&iarray))
	array := *prototype
	array.tflag = typ.tflag & tflagRegularMemory
	array.str = resolveReflectName(newName(s, "", false))
	array.hash = fnv1(typ.hash, '[')
	for n := uint32(length); n > 0; n >>= 8 {
		array.hash = fnv1(array.hash, byte(n))
	}
	array.hash = fnv1(array.hash, ']')
	array.elem = typ
	array.ptrToThis = 0
	if typ.size > 0 {
		max := ^uintptr(0) / typ.size
		if uintptr(length) > max {
			panic("reflect.ArrayOf: array size would exceed virtual address space")
		}
	}
	array.size = typ.size * uintptr(length)
	if length > 0 && typ.ptrdata != 0 {
		array.ptrdata = typ.size*uintptr(length-1) + typ.ptrdata
	}
	array.align = typ.align
	array.fieldAlign = typ.fieldAlign
	array.len = uintptr(length)
	array.slice = SliceOf(elem).(*rtype)

	switch {
	case typ.ptrdata == 0 || array.size == 0:
		// No pointers.
		array.gcdata = nil
		array.ptrdata = 0

	case length == 1:
		// In memory, 1-element array looks just like the element.
		// 在内存中，1个元素的数组看起来就像元素
		array.kind |= typ.kind & kindGCProg
		array.gcdata = typ.gcdata
		array.ptrdata = typ.ptrdata

	case typ.kind&kindGCProg == 0 && array.size <= maxPtrmaskBytes*8*ptrSize:
		// Element is small with pointer mask; array is still small.
		// Create direct pointer mask by turning each 1 bit in elem
		// into length 1 bits in larger mask.
		// 元素很小，但指针掩码数组仍然很小 通过将elem中的每个1位转换为更大的1位掩码来创建直接指针掩码
		mask := make([]byte, (array.ptrdata/ptrSize+7)/8)
		emitGCMask(mask, 0, typ, array.len)
		array.gcdata = &mask[0]

	default:
		// Create program that emits one element
		// and then repeats to make the array.
		// 创建一个程序，该程序发出一个元素，然后重复生成数组
		prog := []byte{0, 0, 0, 0} // will be length of prog
		prog = appendGCProg(prog, typ)
		// Pad from ptrdata to size.
		// 从ptrdata到size
		elemPtrs := typ.ptrdata / ptrSize
		elemWords := typ.size / ptrSize
		if elemPtrs < elemWords {
			// Emit literal 0 bit, then repeat as needed.
			// 发出文本0位，然后根据需要重复
			prog = append(prog, 0x01, 0x00)
			if elemPtrs+1 < elemWords {
				prog = append(prog, 0x81)
				prog = appendVarint(prog, elemWords-elemPtrs-1)
			}
		}
		// Repeat length-1 times.
		// 重复的长度是1次
		if elemWords < 0x80 {
			prog = append(prog, byte(elemWords|0x80))
		} else {
			prog = append(prog, 0x80)
			prog = appendVarint(prog, elemWords)
		}
		prog = appendVarint(prog, uintptr(length)-1)
		prog = append(prog, 0)
		*(*uint32)(unsafe.Pointer(&prog[0])) = uint32(len(prog) - 4)
		array.kind |= kindGCProg
		array.gcdata = &prog[0]
		array.ptrdata = array.size // overestimate but ok; must match program
	}

	etyp := typ.common()
	esize := etyp.Size()

	array.equal = nil
	if eequal := etyp.equal; eequal != nil {
		array.equal = func(p, q unsafe.Pointer) bool {
			for i := 0; i < length; i++ {
				pi := arrayAt(p, i, esize, "i < length")
				qi := arrayAt(q, i, esize, "i < length")
				if !eequal(pi, qi) {
					return false
				}

			}
			return true
		}
	}

	switch {
	case length == 1 && !ifaceIndir(typ):
		// array of 1 direct iface type can be direct
		// 1个直接iface类型的数组可以是直接的
		array.kind |= kindDirectIface
	default:
		array.kind &^= kindDirectIface
	}

	ti, _ := lookupCache.LoadOrStore(ckey, &array.rtype)
	return ti.(Type)
}

func appendVarint(x []byte, v uintptr) []byte {
	for ; v >= 0x80; v >>= 7 {
		x = append(x, byte(v|0x80))
	}
	x = append(x, byte(v))
	return x
}

// toType converts from a *rtype to a Type that can be returned
// to the client of package reflect. In gc, the only concern is that
// a nil *rtype must be replaced by a nil Type, but in gccgo this
// function takes care of ensuring that multiple *rtype for the same
// type are coalesced into a single Type.
// toType从一个*rtype转换为一个可以返回给reflect包的客户端的Type
// 在gc中，唯一的问题是nil *rtype必须被nil Type替换，但在gccgo中，这个函数负责确保同一类型的多个*rtype合并为一个类型
func toType(t *rtype) Type {
	if t == nil {
		return nil
	}
	return t
}

type layoutKey struct {
	ftyp *funcType // function signature
	rcvr *rtype    // receiver type, or nil if none
}

type layoutType struct {
	t         *rtype
	framePool *sync.Pool
	abi       abiDesc
}

var layoutCache sync.Map // map[layoutKey]layoutType

// funcLayout computes a struct type representing the layout of the
// stack-assigned function arguments and return values for the function
// type t.
// funcLayout计算一个结构类型，该结构类型表示堆栈赋值函数参数的布局以及函数类型t的返回值
// If rcvr != nil, rcvr specifies the type of the receiver.
// The returned type exists only for GC, so we only fill out GC relevant info.
// Currently, that's just size and the GC program. We also fill in
// the name for possible debugging use.
// 如果rcvr != nil, rcvr指定接收者的类型 返回的类型只对GC存在，所以我们只填写GC相关的信息
// 目前，这只是大小和GC程序 为了可能的调试使用，我们还填写了名称
func funcLayout(t *funcType, rcvr *rtype) (frametype *rtype, framePool *sync.Pool, abi abiDesc) {
	if t.Kind() != Func {
		panic("reflect: funcLayout of non-func type " + t.String())
	}
	if rcvr != nil && rcvr.Kind() == Interface {
		panic("reflect: funcLayout with interface receiver " + rcvr.String())
	}
	k := layoutKey{t, rcvr}
	if lti, ok := layoutCache.Load(k); ok {
		lt := lti.(layoutType)
		return lt.t, lt.framePool, lt.abi
	}

	// Compute the ABI layout.
	// 计算ABI布局
	abi = newAbiDesc(t, rcvr)

	// build dummy rtype holding gc program
	// 构建虚拟rtype保存gc程序
	x := &rtype{
		align: ptrSize,
		// Don't add spill space here; it's only necessary in
		// reflectcall's frame, not in the allocated frame.
		// TODO(mknyszek): Remove this comment when register
		// spill space in the frame is no longer required.
		// 不要在这里添加溢出空间，它只在反射调用的帧中需要，而不是在已分配的帧中
		// TODO(mknyszek):当不再需要帧中的寄存器溢出空间时，删除此注释
		size:    align(abi.retOffset+abi.ret.stackBytes, ptrSize),
		ptrdata: uintptr(abi.stackPtrs.n) * ptrSize,
	}
	if abi.stackPtrs.n > 0 {
		x.gcdata = &abi.stackPtrs.data[0]
	}

	var s string
	if rcvr != nil {
		s = "methodargs(" + rcvr.String() + ")(" + t.String() + ")"
	} else {
		s = "funcargs(" + t.String() + ")"
	}
	x.str = resolveReflectName(newName(s, "", false))

	// cache result for future callers
	// 为将来的调用者缓存结果
	framePool = &sync.Pool{New: func() interface{} {
		return unsafe_New(x)
	}}
	lti, _ := layoutCache.LoadOrStore(k, layoutType{
		t:         x,
		framePool: framePool,
		abi:       abi,
	})
	lt := lti.(layoutType)
	return lt.t, lt.framePool, lt.abi
}

// ifaceIndir reports whether t is stored indirectly in an interface value.
// ifaceIndir报告t是否间接存储在接口值中
func ifaceIndir(t *rtype) bool {
	return t.kind&kindDirectIface == 0
}

// Note: this type must agree with runtime.bitvector.
// 注意:该类型必须与runtime.bitvector一致
type bitVector struct {
	n    uint32 // number of bits
	data []byte
}

// append a bit to the bitmap.
// 向位图追加一个位
func (bv *bitVector) append(bit uint8) {
	if bv.n%8 == 0 {
		bv.data = append(bv.data, 0)
	}
	bv.data[bv.n/8] |= bit << (bv.n % 8)
	bv.n++
}

func addTypeBits(bv *bitVector, offset uintptr, t *rtype) {
	if t.ptrdata == 0 {
		return
	}

	switch Kind(t.kind & kindMask) {
	case Chan, Func, Map, Ptr, Slice, String, UnsafePointer:
		// 1 pointer at start of representation
		// 表示开始时的1个指针
		for bv.n < uint32(offset/uintptr(ptrSize)) {
			bv.append(0)
		}
		bv.append(1)

	case Interface:
		// 2 pointers
		for bv.n < uint32(offset/uintptr(ptrSize)) {
			bv.append(0)
		}
		bv.append(1)
		bv.append(1)

	case Array:
		// repeat inner type
		// 重复的类型
		tt := (*arrayType)(unsafe.Pointer(t))
		for i := 0; i < int(tt.len); i++ {
			addTypeBits(bv, offset+uintptr(i)*tt.elem.size, tt.elem)
		}

	case Struct:
		// apply fields
		// 应用领域
		tt := (*structType)(unsafe.Pointer(t))
		for i := range tt.fields {
			f := &tt.fields[i]
			addTypeBits(bv, offset+f.offset(), f.typ)
		}
	}
}
