// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package reflect

// VisibleFields returns all the visible fields in t, which must be a
// struct type. A field is defined as visible if it's accessible
// directly with a FieldByName call. The returned fields include fields
// inside anonymous struct members and unexported fields. They follow
// the same order found in the struct, with anonymous fields followed
// immediately by their promoted fields.
// VisibleFields返回t中所有可见字段，它必须是struct类型
// 如果字段可以通过FieldByName调用直接访问，则该字段被定义为可见
// 返回的字段包括匿名结构成员中的字段和未导出的字段
// 它们遵循与结构中相同的顺序，匿名字段紧跟着它们的提升字段
//
// For each element e of the returned slice, the corresponding field
// can be retrieved from a value v of type t by calling v.FieldByIndex(e.Index).
// 对于返回片中的每个元素e，可以通过调用v. fieldbyindex (e. index)从类型为t的值v中检索对应的字段
func VisibleFields(t Type) []StructField {
	if t == nil {
		panic("reflect: VisibleFields(nil)")
	}
	if t.Kind() != Struct {
		panic("reflect.VisibleFields of non-struct type")
	}
	w := &visibleFieldsWalker{
		byName:   make(map[string]int),
		visiting: make(map[Type]bool),
		fields:   make([]StructField, 0, t.NumField()),
		index:    make([]int, 0, 2),
	}
	w.walk(t)
	// Remove all the fields that have been hidden.
	// Use an in-place removal that avoids copying in
	// the common case that there are no hidden fields.
	// 删除所有隐藏的字段 在没有隐藏字段的常见情况下，使用就地删除来避免复制
	j := 0
	for i := range w.fields {
		f := &w.fields[i]
		if f.Name == "" {
			continue
		}
		if i != j {
			// A field has been removed. We need to shuffle
			// all the subsequent elements up.
			// 字段已被删除 我们需要把所有的后续元素重新排列
			w.fields[j] = *f
		}
		j++
	}
	return w.fields[:j]
}

type visibleFieldsWalker struct {
	byName   map[string]int
	visiting map[Type]bool
	fields   []StructField
	index    []int
}

// walk walks all the fields in the struct type t, visiting
// fields in index preorder and appending them to w.fields
// (this maintains the required ordering).
// Fields that have been overridden have their
// Name field cleared.
// Walk遍历结构类型t中的所有字段，访问索引预序中的字段，并将它们添加到w.fields(这维护了所需的顺序)
// 已被重写的字段的Name字段将被清除
func (w *visibleFieldsWalker) walk(t Type) {
	if w.visiting[t] {
		return
	}
	w.visiting[t] = true
	for i := 0; i < t.NumField(); i++ {
		f := t.Field(i)
		w.index = append(w.index, i)
		add := true
		if oldIndex, ok := w.byName[f.Name]; ok {
			old := &w.fields[oldIndex]
			if len(w.index) == len(old.Index) {
				// Fields with the same name at the same depth
				// cancel one another out. Set the field name
				// to empty to signify that has happened, and
				// there's no need to add this field.
				// 具有相同名称、相同深度的字段会相互抵消
				// 将字段名称设置为空以表示已经发生，并且不需要添加此字段
				old.Name = ""
				add = false
			} else if len(w.index) < len(old.Index) {
				// The old field loses because it's deeper than the new one.
				// 旧油田会损失，因为它比新油田更深
				old.Name = ""
			} else {
				// The old field wins because it's shallower than the new one.
				// 老油田赢了，因为它比新油田浅
				add = false
			}
		}
		if add {
			// Copy the index so that it's not overwritten
			// by the other appends.
			// 复制索引，使其不被其他追加覆盖
			f.Index = append([]int(nil), w.index...)
			w.byName[f.Name] = len(w.fields)
			w.fields = append(w.fields, f)
		}
		if f.Anonymous {
			if f.Type.Kind() == Ptr {
				f.Type = f.Type.Elem()
			}
			if f.Type.Kind() == Struct {
				w.walk(f.Type)
			}
		}
		w.index = w.index[:len(w.index)-1]
	}
	delete(w.visiting, t)
}
