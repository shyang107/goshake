package util

import (
	"fmt"
	"math"
	"os"
	"strings"
	"unsafe"

	"github.com/shyang107/goshake/shake/cfg"

	"github.com/cpmech/gosl/chk"
)

const (
	space = " "
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func toString(bs []byte) string {
	return *(*string)(unsafe.Pointer(&bs))
}

// Spaces returns the number of spaces
func Spaces(num int) string {
	return strings.Repeat(space, num)
}

// Hstring return the formatted string what you wants.
// parameters
// 	str 		: the original string
//	explen 		: the length of 'rstr' string
//	fillchar 	: fill 'fillchar' in the spaces of 'rstr' string
//	align		= 1 	: align at the left (default)
//				= 0		: align at the center
//				= -1	: align at the right
// outputs
//	rstr		: the return string
//	err			: the error if therer are errors
func Hstring(str string, explen int, fillchar string, align int) (rstr string) {
	lenstr := len(str)
	if lenstr > explen {
		return str[:explen]
	}
	if fillchar == "" {
		fillchar = space
	} else if len(fillchar) > 1 {
		fillchar = fillchar[:1]
	}
	switch align {
	case 0: // align at the center
		nleft := (explen - lenstr) / 2
		rstr = strings.Repeat(fillchar, nleft) + str + strings.Repeat(fillchar, explen-lenstr-nleft)
	case -1: // align at the right
		rstr = strings.Repeat(fillchar, explen-lenstr) + str
	default: // align at the left (default)
		rstr = str + strings.Repeat(fillchar, explen-lenstr)
	}
	return rstr
}

func vPf(format string, a ...interface{}) {
	if cfg.Verbose {
		fmt.Printf(format, a...)
	}
}
func imin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func imax(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func min(a, b float64) float64 {
	if a < b {
		return a
	}
	return b
}

func max(a, b float64) float64 {
	if a > b {
		return a
	}
	return b
}

func isum(v []int) int {
	var n int
	for i := 0; i < len(v); i++ {
		n += v[i]
	}
	return n
}

// absxmax return the maximum absolute value and the corressponding index of loation in x[]
func absxmax(x []float64) (nxmax int, xmax float64) {
	if x == nil {
		panic(chk.Err("arg. x[] is <nil>"))
	}
	if len(x) == 0 {
		panic(chk.Err("arg. len(x[]) = 0"))
	}
	xmax = 0.
	for i := 0; i < len(x); i++ {
		xa := math.Abs(x[i])
		if xmax <= xa {
			nxmax = i
			xmax = xa
		}
	}
	return
}

func exists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) {
		return false, nil
	}
	return true, err
}

// // ValsTable prints a nice table with input values
// //  Input:
// //   title -- title of table; e.g. INPUT VALUES
// //	 data  -- ready for output
// //   head  -- sets of header in the following order:
// func ValsTable(title string, data []interface{}, head ...interface{}) (table string) {

// }
