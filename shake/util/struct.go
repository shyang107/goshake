package util

import (
	"errors"
	"fmt"
	"reflect"
)

// GetTypes returns map["Field"]="Type"
func GetTypes(obj interface{}) map[string]string {
	d := reflect.ValueOf(obj)
	t := d.Type()
	n := t.NumField()
	types := make(map[string]string)
	for i := 0; i < n; i++ {
		types[t.Field(i).Name] = t.Field(i).Type.String() //io.Sf("%v", t.Field(i).Type)
	}
	return types
}

// GetTags returns map["Field"]="tag_value"
func GetTags(obj interface{}, tag string) map[string]string {
	d := reflect.ValueOf(obj)
	t := d.Type()
	n := t.NumField()
	tags := make(map[string]string)
	for i := 0; i < n; i++ {
		tags[t.Field(i).Name] = t.Field(i).Tag.Get(tag) //io.Sf("%v", t.Field(i).Tag.Get(tag))
	}
	return tags
}

// GetFieldsInfo return information of fields
func GetFieldsInfo(obj interface{},
	tagname string,
	ignoreFields ...string) (fields, types, kinds, tags []string, err error) {

	if err = isValidStruct(obj); err != nil {
		return nil, nil, nil, nil, err
	}

	v := reflect.ValueOf(obj).Elem()
	for i := 0; i < v.NumField(); i++ {
		t := v.Type().Field(i)
		if isIgnored(t.Name, ignoreFields...) {
			continue
		}
		fields = append(fields, t.Name)
		types = append(types, fmt.Sprintf("%v", t.Type))
		kinds = append(kinds, fmt.Sprintf("%v", t.Type.Kind()))
		if name, ok := t.Tag.Lookup(tagname); ok {
			tags = append(tags, name)
		}
	}
	return fields, types, kinds, tags, nil
}

func isIgnored(fieldName string, ignoreFields ...string) bool {
	for _, l := range ignoreFields {
		if l == fieldName {
			return true
		}
	}
	return false
}

func isValidStruct(e interface{}) error {
	stVal := reflect.ValueOf(e)
	if stVal.Kind() != reflect.Ptr || stVal.IsNil() {
		return errors.New("struct passed is not valid, a pointer was expected")
	}
	structVal := stVal.Elem()
	if structVal.Kind() != reflect.Struct {
		return errors.New("struct passed is not valid, a pointer to struct was expected")
	}

	return nil
}

func isFieldNameValid(e interface{}, fn string) bool {

	s := reflect.ValueOf(e).Elem()

	for i := 0; i < s.NumField(); i++ {
		if s.Type().Field(i).Name == fn {
			return true
		}
	}

	return false
}

//Names returns an array with all the field names (with the same order) as defined on the struct
func Names(e interface{}, ignoredFields ...string) (out []string, err error) {

	if err := isValidStruct(e); err != nil {
		return nil, err
	}

	s := reflect.ValueOf(e).Elem()
	for i := 0; i < s.NumField(); i++ {
		if isIgnored(s.Type().Field(i).Name, ignoredFields...) {
			continue
		}
		out = append(out, s.Type().Field(i).Name)
	}

	return
}

//NamesFromTag returns an array with all the tag names for each field
func NamesFromTag(e interface{}, tag string, ignoredFields ...string) (out []string, err error) {

	if err := isValidStruct(e); err != nil {
		return nil, err
	}

	s := reflect.ValueOf(e).Elem()

	for i := 0; i < s.NumField(); i++ {
		if isIgnored(s.Type().Field(i).Name, ignoredFields...) {
			continue
		}
		if val, ok := s.Type().Field(i).Tag.Lookup(tag); ok {
			out = append(out, val)
		}
	}

	return
}

//Values returns an interface array with all the values
func Values(e interface{}, ignoredFields ...string) (out []interface{}, err error) {

	if err := isValidStruct(e); err != nil {
		return nil, err
	}

	s := reflect.ValueOf(e).Elem()
	for i := 0; i < s.NumField(); i++ {
		if isIgnored(s.Type().Field(i).Name, ignoredFields...) {
			continue
		}
		out = append(out, s.Field(i).Interface())

	}

	return
}

// StrValues returns an string array with all the values
func StrValues(e interface{}, ignoredFields ...string) ([]string, error) {
	values, err := Values(e, ignoredFields...)
	if err != nil {
		return nil, err
	}
	var s = make([]string, 0)
	for _, v := range values {
		s = append(s, fmt.Sprintf("%v", v))
	}
	return s, nil
}

// ValuesCallback is used in (Str)ValuesWithFunc to process line by line during retriving of
//  a field-value
//  example:
/*
var cb ValuesCallback = func(fieldName string,
v interface{}) (value interface{}, isIgnored bool) {
switch fieldName {
case "Model", "Details":
	value, isIgnored = nil, true
case "UINumber":
	a := v.(string)
	value, isIgnored = interface{}(a[0:2]+"-"+a[2:]), false
case "Date":
	value, isIgnored = interface{}(v.(time.Time).Format(ShortDateFormat)), false
case "Total":
	value, isIgnored = interface{}(fmt.Sprintf("%.1f", v.(float64))), false
}
default:
	value, isIgnored = v, false
return value, isIgnored
}
*/
type ValuesCallback func(f reflect.StructField, v interface{}) (value interface{}, isIgnored bool)

// ValuesWithFunc returns an interface array with all the values
func ValuesWithFunc(e interface{},
	cb ValuesCallback,
	ignoredFields ...string) (out []interface{}, err error) {

	if err := isValidStruct(e); err != nil {
		return nil, err
	}

	s := reflect.ValueOf(e).Elem()
	for i := 0; i < s.NumField(); i++ {
		t := s.Type().Field(i)
		value, isIgnored := cb(t, s.Field(i).Interface())
		if isIgnored {
			continue
		}
		out = append(out, value)
	}

	return
}

// StrValuesWithFunc returns an interface array with all the string-values
func StrValuesWithFunc(e interface{},
	cb ValuesCallback,
	ignoredFields ...string) (out []string, err error) {

	if err := isValidStruct(e); err != nil {
		return nil, err
	}

	s := reflect.ValueOf(e).Elem()
	for i := 0; i < s.NumField(); i++ {
		t := s.Type().Field(i)
		value, isIgnored := cb(t, s.Field(i).Interface())
		if isIgnored {
			continue
		}
		out = append(out, fmt.Sprintf("%v", value))
	}

	return
}

// FieldValueMap returns a string to interface map,
// key: field as defined on the struct
// value: the value of the field
func FieldValueMap(e interface{}, ignoredFields ...string) (out map[string]interface{}, err error) {

	if err := isValidStruct(e); err != nil {
		return nil, err
	}

	out = make(map[string]interface{})
	s := reflect.ValueOf(e).Elem()
	for i := 0; i < s.NumField(); i++ {
		if isIgnored(s.Type().Field(i).Name, ignoredFields...) {
			continue
		}
		out[s.Type().Field(i).Name] = s.Field(i).Interface()
	}

	return
}

// FieldValueFromTagMap returns a string to interface map that uses as key the tag name,
// key: tag name for the given field
// value: the value of the field
func FieldValueFromTagMap(e interface{}, tag string, ignoredFields ...string) (out map[string]interface{}, err error) {

	if err := isValidStruct(e); err != nil {
		return nil, err
	}

	out = make(map[string]interface{})
	s := reflect.ValueOf(e).Elem()
	for i := 0; i < s.NumField(); i++ {
		if isIgnored(s.Type().Field(i).Name, ignoredFields...) {
			continue
		}

		if val, ok := s.Type().Field(i).Tag.Lookup(tag); ok {
			out[val] = s.Field(i).Interface()
		}

	}

	return
}
