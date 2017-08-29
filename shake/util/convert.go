package util

import (
	"fmt"
	"strconv"
	"strings"
)

// convert ---------------------------------------------------------

var (
	// Sf is the alias of fmt.Sprintf
	Sf = fmt.Sprintf
	// Ff is the alias of fmt.Fprintf
	Ff = fmt.Fprintf
)

// Atob converts string to bool
func Atob(val string) (bres bool) {
	if strings.ToLower(val) == "true" {
		return true
	}
	if strings.ToLower(val) == "false" {
		return false
	}
	res, err := strconv.Atoi(val)
	if err != nil {
		Panic("cannot parse string representing integer: %s", val)
	}
	if res != 0 {
		bres = true
	}
	return
}

// Atoi converts string to integer
func Atoi(val string) (res int) {
	res, err := strconv.Atoi(val)
	if err != nil {
		Panic("cannot parse string representing integer number: %s", val)
	}
	return
}

// Atof converts string to float64
func Atof(val string) (res float64) {
	res, err := strconv.ParseFloat(val, 64)
	if err != nil {
		Panic("cannot parse string representing float number: %s", val)
	}
	return
}

// Itob converts from integer to bool
//  Note: only zero returns false
//        anything else returns true
func Itob(val int) bool {
	if val == 0 {
		return false
	}
	return true
}

// Btoi converts flag to interger
//  Note: true  => 1
//        false => 0
func Btoi(flag bool) int {
	if flag {
		return 1
	}
	return 0
}

// Btoa converts flag to string
//  Note: true  => "true"
//        false => "false"
func Btoa(flag bool) string {
	if flag {
		return "true"
	}
	return "false"
}

// IntSf is the Sprintf for a slice of integers (without brackets)
func IntSf(msg string, slice []int) string {
	return strings.Trim(fmt.Sprintf(msg, slice), "[]")
}

// DblSf is the Sprintf for a slice of float64 (without brackets)
func DblSf(msg string, slice []float64) string {
	return strings.Trim(fmt.Sprintf(msg, slice), "[]")
}

// StrSf is the Sprintf for a slice of string (without brackets)
func StrSf(msg string, slice []string) string {
	return strings.Trim(fmt.Sprintf(msg, slice), "[]")
}
