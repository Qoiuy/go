// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build !goexperiment.regabireflect
// +build !goexperiment.regabireflect

package abi

const (
	// ABI-related constants.
	// ABI 相关的 常数。
	// ABI 二进制接口 是啥 简单说 就是和二进制交互(编译器)的接口
	// 文档  https://mlog.club/article/1863631
	//		原文印象笔记 =>	https://app.yinxiang.com/shard/s35/nl/8072001/1d96c94b-9c08-40d5-9944-1125d0a41009
	//
	//
	// In the generic case, these are all zero
	// which lets them gracefully degrade to ABI0.
	// 在一般情况下，这些都是0，这让它们优雅地退化到 ABI0。
	// ? ABI0是啥

	// IntArgRegs is the number of registers dedicated
	// to passing integer argument values. Result registers are identical
	// to argument registers, so this number is used for those too.
	// IntArgRegs是专用于传递整型参数值的寄存器的数量。
	// 结果寄存器与参数寄存器相同，所以这个数字也用于这些寄存器。
	//
	IntArgRegs = 0

	// FloatArgRegs is the number of registers dedicated
	// to passing floating-point argument values. Result registers are
	// identical to argument registers, so this number is used for
	// those too.
	FloatArgRegs = 0

	// EffectiveFloatRegSize describes the width of floating point
	// registers on the current platform from the ABI's perspective.
	//
	// Since Go only supports 32-bit and 64-bit floating point primitives,
	// this number should be either 0, 4, or 8. 0 indicates no floating
	// point registers for the ABI or that floating point values will be
	// passed via the softfloat ABI.
	//
	// For platforms that support larger floating point register widths,
	// such as x87's 80-bit "registers" (not that we support x87 currently),
	// use 8.
	// effecvefloatregsize从ABI的角度描述了当前平台上浮点寄存器的宽度。
	// 因为Go只支持32位和64位的浮点原语，所以这个数字应该是0、4或8。0表示ABI没有浮点寄存器，或者浮点值将通过软浮点ABI传递。
	// 对于支持更大浮点寄存器宽度的平台，例如x87的80位“寄存器”(我们目前并不支持x87)，使用8。
	EffectiveFloatRegSize = 0
)
