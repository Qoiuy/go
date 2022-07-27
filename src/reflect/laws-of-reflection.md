# The Laws of Reflection

Rob Pike 6 September 2011

## Introduction

Reflection in computing is the ability of a program to examine its own structure, particularly through types; it’s a
form of metaprogramming. It’s also a great source of confusion.
<font face="黑体" size =1>计算中的反射是程序检查其自身结构的能力，特别是通过类型(它是元编程的一种形式) </font>
<font face="黑体" size =1>这也是造成困惑的一大原因 </font>

In this article we attempt to clarify things by explaining how reflection works in Go. Each language’s reflection model
is different (and many languages don’t support it at all), but this article is about Go, so for the rest of this article
the word “reflection” should be taken to mean “reflection in Go”.
<font face="黑体" size =1> 在这篇文章中，我们试图通过解释反射在围棋中的工作原理来澄清一些事情 </font>
<font face="黑体" size =1> 每种语言的反射模型都是不同的(而且许多语言根本不支持它)，但是本文是关于Go的，所以在本文的其余部分，“反射”一词应该被理解为“Go中的反射”</font>

Note added January 2022: This blog post was written in 2011 and predates parametric polymorphism (a.k.a. generics)
in Go. Although nothing important in the article has become incorrect as a result of that development in the language,
it has been tweaked in a few places to avoid confusing someone familiar with modern Go.
<font face="黑体" size =1> 注:这篇博客写于2011年，早于参数多态性(又称参数多态性)</font>
<font face="黑体" size =1> 泛型) 尽管由于语言的发展，文章中没有什么重要的内容变得不正确，但还是在一些地方进行了调整，以避免让熟悉现代围棋的人感到困惑</font>

## Types and interfaces 复习一下Go中的类型

Because reflection builds on the type system, let’s start with a refresher about types in Go.
<font face="黑体" size =1>因为反射建立在类型系统之上，所以让我们先来复习一下Go中的类型</font>

Go is statically typed. Every variable has a static type, that is, exactly one type known and fixed at compile
time: `int`, `float32`, `*MyType`, `[]byte`, and so on.
<font face="黑体" size =1> Go是静态类型的 每个变量都有一个静态类型，也就是说，只有一个在编译时已知并固定的类型: int, float32, *MyType, []byte 等等 </font>

If we declare
<font face="黑体" size =1> 如果我们声明</font>

```
type MyInt int

var i int
var j MyInt
```

then `i` has type `int` and `j` has type `MyInt`. The variables `i` and `j` have distinct static types and, although
they have the same underlying type, they cannot be assigned to one another without a conversion.
<font face="黑体" size =1> 然后i的类型是int j的类型是 MyInt </font>
<font face="黑体" size =1> 变量i和j具有不同的静态类型，尽管它们具有相同的基础类型，但在没有转换的情况下不能相互赋值 </font>

One important category of type is interface types, which represent fixed sets of methods. (When discussing reflection,
we can ignore the use of interface definitions as constraints within polymorphic code.)
An interface variable can store any concrete (non-interface) value as long as that value implements the interface’s
methods. A well-known pair of examples is `io.Reader` and `io.Writer`, the types `Reader`
and `Writer` from the [io package]():</font>
<font face="黑体" size =1> 接口类型是类型的一个重要类别，它表示固定的方法集 </font>
<font face="黑体" size =1>  (在讨论反射时，我们可以忽略在多态代码中使用接口定义作为约束 ) </font>
<font face="黑体" size =1> 接口变量可以存储任何具体(非接口)值，只要该值实现了接口的方法 </font>
<font face="黑体" size =1> 一个典型的例子是来自io包的 io.Reader , io.Writer,Reader和Writer:</font>

```
// Reader is the interface that wraps the basic Read method.
type Reader interface {
    Read(p []byte) (n int, err error)
}

// Writer is the interface that wraps the basic Write method.
type Writer interface {
    Write(p []byte) (n int, err error)
}
```

Any type that implements a `Read` (or `Write`) method with this signature is said to implement `io.Reader`
(or `io.Writer`). For the purposes of this discussion, that means that a variable of type `io.Reader` can hold any value
whose type has a `Read` method:
<font face="黑体" size =1> 任何实现具有此签名的 Read (或Write)方法的类型都被称为实现io.Reader(或io.Writer) </font>
<font face="黑体" size =1> 对于本文的讨论，这意味着io.Reader类型的变量 可以保存实现了Read方法的任何值:</font>

```
var r io.Reader
r = os.Stdin
r = bufio.NewReader(r)
r = new(bytes.Buffer)
// and so on
```

It’s important to be clear that whatever concrete value `r` may hold, `r`’s type is always `io.Reader`: Go is statically
typed and the static type of `r` is `io.Reader`.
<font face="黑体" size =1> 很重要的一点是，不管具体的值是什么，它的类型总是:Go是静态类型，而的静态类型是</font>

An extremely important example of an interface type is the empty interface:
<font face="黑体" size =1> 一个非常重要的接口类型的例子是空接口:</font>

```
interface{}
```

or its equivalent alias, <font face="黑体" size =1> 或者它的别名，</font>

```
any
```

It represents the empty set of methods and is satisfied by any value at all, since every value has zero or more methods.
<font face="黑体" size =1> 它表示空的方法集，可以满足任何值，因为每个值都有0个或多个方法</font>

Some people say that Go’s interfaces are dynamically typed, but that is misleading. They are statically typed:
a variable of interface type always has the same static type, and even though at run time the value stored in the
interface variable may change type, that value will always satisfy the interface. <font face="黑体" size =1>
有人说Go的接口是动态类型的，但这是误导 // 它们是静态类型的:</font>
接口类型的变量总是具有相同的静态类型，即使在运行时存储在接口变量中的值可能改变类型，该值始终满足接口

We need to be precise about all this because reflection and interfaces are closely
related. <font face="黑体" size =1></font>
因为反射和接口是密切相关的，所以我们需要精确地处理所有这些问题

## The representation of an interface

Russ Cox has written
a [detailed blog post](https:<font face="黑体" size =1>research.swtch.com/2009/12/go-data-structures-interfaces.html)</font>
about the representation of interface values in Go. It’s not necessary to repeat the full story here, but a simplified
summary is in order. <font face="黑体" size =1> Russ Cox写了一篇[详细的博客文章]()关于Go中接口值的表示 // 在这里没有必要重复整个故事，但可以做一个简单的总结</font>

A variable of interface type stores a pair: the concrete value assigned to the variable, and that value’s type
descriptor. To be more precise, the value is the underlying concrete data item that implements the interface and the
type describes the full type of that item. For instance, after <font face="黑体" size =1> 接口类型的变量存储一对:
分配给该变量的具体值，以及该值的类型描述符 //</font>
更准确地说，值是实现接口的底层具体数据项，类型描述该项的完整类型 <font face="黑体" size =1> 例如,后</font>

```
var r io.Reader
tty, err := os.OpenFile("/dev/tty", os.O_RDWR, 0)
if err != nil {
    return nil, err
}
r = tty
```

`r` contains, schematically, the (value, type) pair, (`tty`, `*os.File`). Notice that the type `*os.File`
implements methods other than `Read`; even though the interface value provides access only to the `Read`
method, the value inside carries all the type information about that value. That’s why we can do things like this:

```
var w io.Writer
w = r.(io.Writer)
```

The expression in this assignment is a type assertion; what it asserts is that the item inside `r` also
implements `io.Writer`, and so we can assign it to `w`. After the assignment, `w` will contain the pair (`tty`
, `*os.File`). That’s the same pair as was held in `r`. The static type of the interface determines what methods may be
invoked with an interface variable, even though the concrete value inside may have a larger set of methods.

Continuing, we can do this:

```
var empty interface{}
empty = w
```

and our empty interface value `empty` will again contain that same pair, (`tty`, `*os.File`). That’s handy:
an empty interface can hold any value and contains all the information we could ever need about that
value. <font face="黑体" size =1></font>
我们的空接口值将再次包含相同的对，(不是tty，)
<font face="黑体" size =1> 这很方便:一个空的接口可以保存任何值，并包含我们可能需要的关于该值的所有信息</font>

(We don’t need a type assertion here because it’s known statically that `w` satisfies the empty interface. In the
example where we moved a value from a `Reader` to a `Writer`, we needed to be explicit and use a type assertion
because `Writer`’s methods are not a subset of `Reader`’s.)

One important detail is that the pair inside an interface variable always has the form (value, concrete type)
and cannot have the form (value, interface type). Interfaces do not hold interface values. <font face="黑体" size =1>
一个重要的细节是，接口变量中的pair总是具有形式(</font>
值、具体类型)，而不能具有形式(值、接口类型)
<font face="黑体" size =1> 接口不包含接口值</font>

Now we’re ready to reflect. <font face="黑体" size =1> 现在我们准备反思</font>

## The first law of reflection

## 1. Reflection goes from interface value to reflection object.

At the basic level, reflection is just a mechanism to examine the type and value pair stored inside an interface
variable. To get started, there are two types we need to know about in
[package reflect](https:<font face="黑体" size =1>go.dev/pkg/reflect/): [Type](https://go.dev/pkg/reflect/#Type)</font>
and [Value](https:<font face="黑体" size =1>go.dev/pkg/reflect/#Value). Those two types give access to the contents of an
interface variable,</font>
and two simple functions, called `reflect.TypeOf` and `reflect.ValueOf`, retrieve `reflect.Type` and `reflect.Value`
pieces out of an interface value. (Also, from a `reflect.Value` it’s easy to get to the corresponding `reflect.Type`,
but let’s keep the `Value` and `Type` concepts separate for now.)
<font face="黑体" size =1> 在基本的层次上，反射只是一种检查存储在接口变量中的类型和值对的机制 // 首先，我们需要了解in的两种类型package reflect, type 和 value</font>

Let’s start with `TypeOf`:
<font face="黑体" size =1> 让我们从下面开始:</font>

```
package main

import (
    "fmt"
    "reflect"
)

func main() {
    var x float64 = 3.4
    fmt.Println("type:", reflect.TypeOf(x))
}
```

This program prints <font face="黑体" size =1> 这个程序打印</font>

```
type: float64
```

You might be wondering where the interface is here, since the program looks like it’s passing the `float64`
variable `x`, not an interface value, to `reflect.TypeOf`. But it’s there;
as [godoc reports](https:<font face="黑体" size =1>go.dev/pkg/reflect/#TypeOf), the signature of `reflect.TypeOf` includes
an empty interface:</font>
<font face="黑体" size =1> 您可能想知道这里的接口在哪里，因为程序看起来像是传递了变量，而不是接口值 // 但它在那里[godoc报告]()，签名包括一个空接口:</font>

```
<font face="黑体" size =1> TypeOf returns the reflection Type of the value in the interface{}.</font>
func TypeOf(i interface{}) Type
```

When we call `reflect.TypeOf(x)`, `x` is first stored in an empty interface, which is then passed as the argument;
`reflect.TypeOf` unpacks that empty interface to recover the type information. <font face="黑体" size =1></font>
调用时，首先存储在一个空接口中，然后作为解包该空接口的参数传递该参数，以恢复类型信息

The `reflect.ValueOf` function, of course, recovers the value (from here on we’ll elide the boilerplate and focus just
on the executable code):
<font face="黑体" size =1> 这个函数，当然，恢复值(从这里开始，我们将省略样板文件，只关注可执行代码):</font>

```
var x float64 = 3.4
fmt.Println("value:", reflect.ValueOf(x).String())
```

prints

```
value: <float64 Value>
```

(We call the `String` method explicitly because by default the `fmt` package digs into a `reflect.Value` to show the
concrete value inside. The `String` method does not.)

Both `reflect.Type` and `reflect.Value` have lots of methods to let us examine and manipulate them. One important
example is that `Value` has a `Type` method that returns the `Type` of a `reflect.Value`. Another is that both `Type`
and `Value` have a `Kind` method that returns a constant indicating what sort of item is stored: `Uint`, `Float64`
, `Slice`, and so on. Also methods on `Value` with names like `Int`
and `Float` let us grab values (as `int64` and `float64`) stored inside:

```
var x float64 = 3.4
v := reflect.ValueOf(x)
fmt.Println("type:", v.Type())
fmt.Println("kind is float64:", v.Kind() == reflect.Float64)
fmt.Println("value:", v.Float())
```

prints

```
type: float64
kind is float64: true
value: 3.4
```

There are also methods like `SetInt` and `SetFloat` but to use them we need to understand settability, the subject of
the third law of reflection, discussed below. <font face="黑体" size =1> 还有像and
but这样的方法，要使用它们，我们需要理解固化性，这是反射第三定律的主题，下面讨论</font>

The reflection library has a couple of properties worth singling out. First, to keep the API simple, the “getter” and
“setter” methods of `Value` operate on the largest type that can hold the value:
`int64` for all the signed integers, for instance. That is, the `Int` method of `Value` returns an `int64` and
the `SetInt` value takes an `int64`; it may be necessary to convert to the actual type involved:
<font face="黑体" size =1> 反射库有几个值得挑出来的属性 首先，为了保持API的简单性，“getter”和“setter”方法对能够保存值的最大类型进行操作:例如，对所有有符号整数进行操作 //
也就是说，方法返回an，值接受an 可能需要转换为实际的类型:</font>

```
var x uint8 = 'x'
v := reflect.ValueOf(x)
fmt.Println("type:", v.Type())                            <font face="黑体" size =1> uint8.</font>
fmt.Println("kind is uint8: ", v.Kind() == reflect.Uint8) <font face="黑体" size =1> true.</font>
x = uint8(v.Uint())                                       <font face="黑体" size =1> v.Uint returns a uint64.</font>
```

The second property is that the `Kind` of a reflection object describes the underlying type, not the static type. If a
reflection object contains a value of a user-defined integer type, as in

```
type MyInt int
var x MyInt = 7
v := reflect.ValueOf(x)
```

the `Kind` of `v` is still `reflect.Int`, even though the static type of `x` is `MyInt`, not `int`. In other words,
the `Kind` cannot discriminate an `int` from a `MyInt` even though the `Type` can. <font face="黑体" size =1>
of是静止的，而static类型的of不是</font>
换句话说，不能区分an和a，尽管可以

## The second law of reflection

## 2. Reflection goes from reflection object to interface value.

Like physical reflection, reflection in Go generates its own inverse. <font face="黑体" size =1>
就像物理反射一样，围棋中的反射会产生自己的逆</font>

Given a `reflect.Value` we can recover an interface value using the `Interface` method; in effect the method packs the
type and value information back into an interface representation and returns the result:
<font face="黑体" size =1> 给定一个，我们可以使用该方法恢复接口值，实际上，该方法将类型和值信息打包回接口表示，并返回结果:</font>

```
<font face="黑体" size =1> Interface returns v's value as an interface{}.</font>
func (v Value) Interface() interface{}
```

As a consequence we can say <font face="黑体" size =1> 因此我们可以说</font>

```
y := v.Interface().(float64) <font face="黑体" size =1> y will have type float64.</font>
fmt.Println(y)
```

to print the `float64` value represented by the reflection object `v`. <font face="黑体" size =1> 打印由反射对象表示的值</font>

We can do even better, though. The arguments to `fmt.Println`, `fmt.Printf` and so on are all passed as empty interface
values, which are then unpacked by the `fmt` package internally just as we have been doing in the previous examples.
Therefore all it takes to print the contents of a `reflect.Value` correctly is to pass the result of the `Interface`
method to the formatted print routine:

```
fmt.Println(v.Interface())
```

(Since this article was first written, a change was made to the `fmt` package so that it automatically unpacks
a `reflect.Value` like this, so we could just say

```
fmt.Println(v)
```

for the same result, but for clarity we’ll keep the `.Interface()` calls here.)
<font face="黑体" size =1> 为了得到相同的结果，但为了清晰起见，我们将保留这些调用)</font>

Since our value is a `float64`, we can even use a floating-point format if we want:
<font face="黑体" size =1> 由于我们的值是a，如果需要，我们甚至可以使用浮点格式:</font>

```
fmt.Printf("value is %7.1e\n", v.Interface())
```

and get in this case

```
3.4e+00
```

Again, there’s no need to type-assert the result of `v.Interface()` to `float64`; the empty interface value has the
concrete value’s type information inside and `Printf` will recover it. <font face="黑体" size =1>
同样，不需要对空接口值的结果进行类型断言，其中包含具体值的类型信息，并将恢复它</font>

In short, the `Interface` method is the inverse of the `ValueOf` function, except that its result is always of static
type `interface{}`. <font face="黑体" size =1> 简而言之，方法是函数的逆，只不过它的结果总是静态类型</font>

Reiterating: Reflection goes from interface values to reflection objects and back again. <font face="黑体" size =1> 重复:
反射从接口值到反射对象，然后再回来</font>

## The third law of reflection

## 3. To modify a reflection object, the value must be settable.

The third law is the most subtle and confusing, but it’s easy enough to understand if we start from first
principles. <font face="黑体" size =1></font>
第三定律是最微妙和令人困惑的，但如果我们从第一定律开始，它很容易理解

Here is some code that does not work, but is worth studying. <font face="黑体" size =1> 下面是一些不能工作的代码，但值得研究</font>

```
var x float64 = 3.4
v := reflect.ValueOf(x)
v.SetFloat(7.1) <font face="黑体" size =1> Error: will panic.</font>
```

If you run this code, it will panic with the cryptic message <font face="黑体" size =1> 如果您运行此代码，它将因神秘的消息而惊慌</font>

```
panic: reflect.Value.SetFloat using unaddressable value
```

The problem is not that the value `7.1` is not addressable; it’s that `v` is not settable. Settability is a property of
a reflection `Value`, and not all reflection `Values` have it. <font face="黑体" size =1> 问题不在于这个值是不可寻址的，而是它是不可设置的 //
可设置性是反射的一种属性，并不是所有反射都具有该属性</font>

The `CanSet` method of `Value` reports the settability of a `Value`; in our case, <font face="黑体" size =1>
在我们的案例中，该方法报告了a的可固化性，</font>

```
var x float64 = 3.4
v := reflect.ValueOf(x)
fmt.Println("settability of v:", v.CanSet())
```

prints

```
settability of v: false
```

It is an error to call a `Set` method on a non-settable `Value`. But what is settability? <font face="黑体" size =1>
在不可设置的对象上调用方法是错误的 //</font>
但什么是可固化性呢?

Settability is a bit like addressability, but stricter. It’s the property that a reflection object can modify the actual
storage that was used to create the reflection object. Settability is determined by whether the reflection object holds
the original item. When we say <font face="黑体" size =1> 可设置性有点像可寻址性，但更严格 它是反射对象可以修改用于创建反射对象的实际存储的属性 //
可设置性由反射对象是否持有原始项决定 // 当我们说</font>

```
var x float64 = 3.4
v := reflect.ValueOf(x)
```

we pass a copy of `x` to `reflect.ValueOf`, so the interface value created as the argument to `reflect.ValueOf` is a
copy of `x`, not `x` itself. Thus, if the statement <font face="黑体" size =1> 我们传递的是to的副本，因此作为参数创建的接口值是to的副本，而不是它本身 //
因此，if语句</font>

```
v.SetFloat(7.1)
```

were allowed to succeed, it would not update `x`, even though `v` looks like it was created from `x`. Instead, it would
update the copy of `x` stored inside the reflection value and `x` itself would be unaffected. That would be confusing
and useless, so it is illegal, and settability is the property used to avoid this issue. <font face="黑体" size =1>
如果允许成功，它将不会更新，即使它看起来像从 //</font>
相反，它将更新存储在反射值中的副本，本身不会受到影响 <font face="黑体" size =1> 这将是混乱和无用的，所以它是非法的，可设置性是用来避免这个问题的属性</font>

If this seems bizarre, it’s not. It’s actually a familiar situation in unusual garb. Think of passing `x` to a function:
<font face="黑体" size =1> 如果这看起来很奇怪，其实不然 这其实是一个穿着不寻常服装的熟悉场景 // 考虑传递给一个函数:</font>

```
f(x)
```

We would not expect `f` to be able to modify `x` because we passed a copy of `x`’s value, not `x` itself. If we want `f`
to modify `x` directly we must pass our function the address of `x` (that is, a pointer to `x`):
<font face="黑体" size =1> 我们不期望能够修改，因为我们传递的是值的副本，而不是它本身 // 如果要直接修改，则必须向函数传递的地址(即指向的指针):</font>

```
f(&x)
```

This is straightforward and familiar, and reflection works the same way. If we want to modify `x` by reflection, we must
give the reflection library a pointer to the value we want to modify. <font face="黑体" size =1> 这是简单而熟悉的，反射也以同样的方式工作
//</font>
如果我们想通过反射进行修改，就必须给反射库一个指向我们想要修改的值的指针

Let’s do that. First we initialize `x` as usual and then create a reflection value that points to it, called `p`
. <font face="黑体" size =1> 做一下</font>
首先，我们像往常一样初始化，然后创建一个指向它的反射值，称为

```
var x float64 = 3.4
p := reflect.ValueOf(&x) <font face="黑体" size =1> Note: take the address of x.</font>
fmt.Println("type of p:", p.Type())
fmt.Println("settability of p:", p.CanSet())
```

The output so far is

```
type of p: *float64
settability of p: false
```

The reflection object `p` isn’t settable, but it’s not `p` we want to set, it’s (in effect) `*p`. To get to what `p`
points to, we call the `Elem` method of `Value`, which indirects through the pointer, and save the result in a
reflection `Value` called `v`:
<font face="黑体" size =1> 反射对象是不可设置的，但它不是我们想要设置的，它(实际上)是 // 为了得到指向的对象，我们调用了of方法，该方法通过指针进行间接指向，并将结果保存在一个名为:</font>

```
v := p.Elem()
fmt.Println("settability of v:", v.CanSet())
```

Now `v` is a settable reflection object, as the output demonstrates, <font face="黑体" size =1>
现在是一个可设置的反射对象，正如输出所示，</font>

```
settability of v: true
```

and since it represents `x`, we are finally able to use `v.SetFloat` to modify the value of `x`:
<font face="黑体" size =1> 由于它表示，我们终于可以用它来修改的值:</font>

```
v.SetFloat(7.1)
fmt.Println(v.Interface())
fmt.Println(x)
```

The output, as expected, is

```
7.1
7.1
```

Reflection can be hard to understand but it’s doing exactly what the language does, albeit through reflection
`Types` and `Values` that can disguise what’s going on. Just keep in mind that reflection Values need the address of
something in order to modify what they represent. <font face="黑体" size =1> 反射可能很难理解，但它所做的正是语言所做的，尽管通过反射可以掩盖正在发生的事情
//</font>
只要记住反射值需要某些东西的地址，以便修改它们所表示的东西

## Structs

In our previous example `v` wasn’t a pointer itself, it was just derived from one. A common way for this situation to
arise is when using reflection to modify the fields of a structure. As long as we have the address of the structure, we
can modify its fields. <font face="黑体" size =1> 在我们前面的例子中不是指针本身，它只是从一个指针派生出来的 // 出现这种情况的一种常见方式是使用反射修改结构的字段 //
只要我们有这个结构的地址，我们就可以修改它的字段</font>

Here’s a simple example that analyzes a struct value, `t`. We create the reflection object with the address of the
struct because we’ll want to modify it later. Then we set `typeOfT` to its type and iterate over the fields using
straightforward method calls (see [package reflect](https:<font face="黑体" size =1>go.dev/pkg/reflect/) for details).
Note that we extract the</font>
names of the fields from the struct type, but the fields themselves are regular `reflect.Value`
objects. <font face="黑体" size =1></font>
下面是一个分析结构值的简单示例 我们使用结构体的地址创建反射对象，因为稍后我们想要修改它 <font face="黑体" size =1> 然后我们设置它的类型，并使用直接的方法调用遍历字段(
参见[package reflect](https:go.dev/pkg/reflect/)</font>
了解详细信息)
<font face="黑体" size =1> 注意，我们从结构类型中提取字段的名称，但字段本身是常规对象</font>

```
type T struct {
    A int
    B string
}
t := T{23, "skidoo"}
s := reflect.ValueOf(&t).Elem()
typeOfT := s.Type()
for i := 0; i < s.NumField(); i++ {
    f := s.Field(i)
    fmt.Printf("%d: %s %s = %v\n", i,
        typeOfT.Field(i).Name, f.Type(), f.Interface())
}
```

The output of this program is

```
0: A int = 23
1: B string = skidoo
```

There’s one more point about settability introduced in passing here: the field names of `T` are upper case
(exported) because only exported fields of a struct are settable. <font face="黑体" size =1> 这里还顺便介绍了关于可设置性的一点:的字段名是大写的(
导出的)，因为只有导出的结构字段是可设置的</font>

Because `s` contains a settable reflection object, we can modify the fields of the
structure. <font face="黑体" size =1></font>
因为包含一个可设置的反射对象，所以我们可以修改结构的字段

```
s.Field(0).SetInt(77)
s.Field(1).SetString("Sunset Strip")
fmt.Println("t is now", t)
```

And here’s the result:

```
t is now {77 Sunset Strip}
```

If we modified the program so that `s` was created from `t`, not `&t`, the calls to `SetInt` and `SetString`
would fail as the fields of `t` would not be settable.

## Conclusion

Here again are the laws of reflection:
<font face="黑体" size =1> 反思的法则如下:</font>

- Reflection goes from interface value to reflection object.
- Reflection goes from reflection object to interface value.
- To modify a reflection object, the value must be settable. <font face="黑体" size =1> 反射从接口值到反射对象 // 反射从反射对象到接口值 //
  —如果要修改反射对象，该值必须是可设置的</font>

Once you understand these laws reflection in Go becomes much easier to use, although it remains subtle. It’s a powerful
tool that should be used with care and avoided unless strictly necessary. <font face="黑体" size =1>
一旦你理解了这些规律，在围棋中的反射就会变得更容易使用，尽管它仍然很微妙 //</font>
这是一个强大的工具，应该谨慎使用，除非严格必要，否则应避免使用

There’s plenty more to reflection that we haven’t covered — sending and receiving on channels, allocating memory, using
slices and maps, calling methods and functions — but this post is long enough. We’ll cover some of those topics in a
later article. <font face="黑体" size =1> 还有很多我们没有涉及到的反思——通过通道发送和接收、分配内存、使用切片和映射、调用方法和函数——但这篇文章足够长了 //
我们将在后面的文章中介绍其中的一些主题</font>


