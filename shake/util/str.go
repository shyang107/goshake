package util

import (
	"bytes"
	"fmt"
	"math"
	"strings"
	"unicode"
	"unsafe"
)

// string ---------------------------------------------------------

// Rpad adds padding to the right of a string.
func Rpad(s string, padding int) string {
	template := fmt.Sprintf("%%-%ds", padding)
	return fmt.Sprintf(template, s)
}

// BytesSizeToString convert bytes to a human-readable size
func BytesSizeToString(byteCount int) string {
	suf := []string{"B", "KB", "MB", "GB", "TB", "PB", "EB"} //Longs run out around EB
	if byteCount == 0 {
		return "0" + suf[0]
	}
	bytes := math.Abs(float64(byteCount))
	place := int32(math.Floor(math.Log2(bytes) / 10))
	num := bytes / math.Pow(1024.0, float64(place))
	var strnum string
	if place == 0 {
		strnum = fmt.Sprintf("%.0f", num) + suf[place]
	} else {
		strnum = fmt.Sprintf("%.1f", num) + suf[place]
	}
	return strnum
}

// BytesToString convert []byte to string
func BytesToString(bs []byte) string {
	return *(*string)(unsafe.Pointer(&bs))
}

// GetColStr return string use in field
func GetColStr(s string, size int, isleft bool) string {
	_, _, n := CountChars(s)
	spaces := strings.Repeat(" ", size-n)
	// size := nc*2 + ne // s 實際佔位數
	var tab string
	if isleft {
		tab = fmt.Sprintf("%[1]s%[2]s", s, spaces)
	} else {
		tab = fmt.Sprintf("%[2]s%[1]s", s, spaces)
	}
	return " " + tab
}

// AlginedFlag is the type to assign align-format
type AlginedFlag int8

// Align format
const (
	AlginedLeft AlginedFlag = iota
	AlginedMiddle
	AlignedRight
)

// AlignedString returns the aligned string what you want
// 	str 		: the original string
//	lenres 		: the length of `res` string
//	filledChar 	: fill `filedChar` in the spaces of `res` string
//	align		= 1 	: align at the left (default)
//				= 0		: align at the center
//				= -1	: align at the right
// outputs
//	res		: the aligned string
func AlignedString(str string, lenres int, filledChar string, alignFlag AlginedFlag) (res string) {
	_, _, n := CountChars(str)
	if n > lenres {
		return str[:lenres]
	}

	lfilledChar := len(filledChar)
	switch {
	case lfilledChar == 0:
		filledChar = " "
	case lfilledChar > 1:
		filledChar = filledChar[:1]
	}

	switch alignFlag {
	case AlginedMiddle:
		nleft := (lenres - n) / 2
		res = strings.Repeat(filledChar, nleft) + str + strings.Repeat(filledChar, lenres-n-nleft)
	case AlignedRight:
		nleft := lenres - n
		res = strings.Repeat(filledChar, nleft) + str
	default: // AlginedLeft
		nright := lenres - n
		res = str + strings.Repeat(filledChar, nright)
	}
	return res
}

// AlignToRight returns a string aligned to right
func AlignToRight(s string, size int) string {
	_, _, n := CountChars(s)
	spaces := strings.Repeat(" ", size-n)
	s = spaces + s
	return s
}

// CountChars returns the number of each other of chinses and english characters
func CountChars(str string) (nc, ne, n int) {
	for _, r := range str {
		lchar := len(string(r))
		// n += lchar
		if lchar > 1 {
			nc++
		} else {
			ne++
		}
	}
	n = 2*nc + ne
	return nc, ne, n
}

// IsChineseChar judges whether the chinese character exists ?
func IsChineseChar(str string) bool {
	// n := 0
	for _, r := range str {
		// io.Pf("%q ", r)
		if unicode.Is(unicode.Scripts["Han"], r) {
			// n++
			return true
		}
	}
	return false
}

// GetSizes is filled and returns len(e), e is element of slice h
func GetSizes(h []string) []int {
	n := len(h)
	sizes := make([]int, n)
	for i := 0; i < n; i++ {
		_, _, sizes[i] = CountChars(h[i])
	}
	return sizes
}

// ArgsTable prints a nice table with input arguments
//  Input:
//   title -- title of table; e.g. INPUT ARGUMENTS
//   data  -- sets of THREE items in the following order:
//                 description, key, value, ...
//                 description, key, value, ...
//                      ...
//                 description, key, value, ...
func ArgsTable(title string, data ...interface{}) string {
	heads := []string{"description", "key", "value"}
	return ArgsTableN(title, 0, true, heads, data...)
}

// ArgsTableN prints a nice table with input arguments
//  Input:
//   title -- title of table; e.g. INPUT ARGUMENTS
//	 heads -- heads of table; e.g. []string{ col1,  col2, ... }
//	 nledsp -- length of leading spaces in every row
//   data  -- sets of THREE items in the following order:
//                 column1, column2, column3, ...
//                 column1, column2, column3, ...
//                      ...
//                 column1, column2, column3, ...
func ArgsTableN(title string, nledsp int, isleft bool, heads []string, data ...interface{}) string {
	// Sf := fmt.Sprintf
	nf := len(heads)
	ndat := len(data)
	if ndat < nf {
		return ""
	}
	if nledsp < 0 {
		nledsp = 0
	}
	lspaces := StrSpaces(nledsp)
	nlines := ndat / nf

	sizes := GetSizes(heads)

	for i := 0; i < nlines; i++ {
		if i*nf+(nf-1) >= ndat {
			return Sf("ArgsTable: input arguments are not a multiple of %d\n", nf)
		}
		for j := 0; j < nf; j++ {
			str := Sf("%v", data[i*nf+j])
			_, _, nmix := CountChars(str)
			sizes[j] = Imax(sizes[j], nmix)
		}
	}

	var b bytes.Buffer
	bw := b.WriteString

	// strfmt := Sf("%%v  %%v  %%v\n")
	n := Isum(sizes...) + nf*2 // sizes[0] + sizes[1] + sizes[2] + 3 + 4

	if len(title) > 0 {
		_, _, l := CountChars(title)
		m := (n - l) / 2
		bw(StrSpaces(m+nledsp) + title + "\n")
	}

	bw(lspaces + StrThickLine(n))
	sfields := make([]string, nf)
	for i := 0; i < nf; i++ {
		sfields[i] = GetColStr(heads[i], sizes[i], isleft)
		switch i {
		case 0:
			bw(Sf("%v", lspaces+sfields[i]))
		default:
			bw(Sf(" %v", sfields[i]))
		}
	}
	bw("\n")
	bw(lspaces + StrThinLine(n))
	for i := 0; i < nlines; i++ {
		for j := 0; j < nf; j++ {
			sfields[j] = GetColStr(Sf("%v", data[i*nf+j]), sizes[j], isleft)
			switch j {
			case 0:
				bw(Sf("%v", lspaces+sfields[j]))
			default:
				bw(Sf(" %v", sfields[j]))
			}
		}
		bw("\n")
	}
	bw(lspaces + StrThickLine(n))
	return b.String()
}

// StrThickLine returns a thick line (using '=')
func StrThickLine(n int) (l string) {
	l = strings.Repeat("=", n)
	return l + "\n"
}

// StrThinLine returns a thin line (using '-')
func StrThinLine(n int) (l string) {
	l = strings.Repeat("-", n)
	return l + "\n"
}

// StrSpaces returns a line with spaces
func StrSpaces(n int) (l string) {
	l = strings.Repeat(" ", n)
	return
}

// SplitKeys splits keys separeted by spaces
func SplitKeys(keys string) []string {
	return strings.Split(keys, " ")
}

// SplitSpacesQuoted splits string with quoted substrings. e.g. "  a,b, 'c', \"d\"  "
func SplitSpacesQuoted(str string) (res []string) {
	lastQuote := rune(0)
	f := func(c rune) bool {
		switch {
		case c == lastQuote:
			lastQuote = rune(0)
			return false
		case lastQuote != rune(0):
			return false
		case unicode.In(c, unicode.Quotation_Mark):
			lastQuote = c
			return false
		default:
			return unicode.IsSpace(c)

		}
	}
	return strings.FieldsFunc(str, f)
}

// SplitWithinParentheses extracts arguments (substrings) within brackets
// e.g.: "(arg1, (arg2.1, arg2.2),  arg3, arg4, (arg5.1,arg5.2,  arg5.3 ) )"
func SplitWithinParentheses(s string) (res []string) {
	trim := func(l, pfix, sfix string) string {
		l = strings.TrimSpace(l)
		l = strings.TrimPrefix(l, pfix)
		l = strings.TrimSuffix(l, sfix)
		return l
	}
	s = trim(s, "(", ")")
	s = strings.Replace(s, "(", "'", -1)
	s = strings.Replace(s, ")", "'", -1)
	s = strings.Replace(s, ",", " ", -1)
	res = SplitSpacesQuoted(s)
	for i := 0; i < len(res); i++ {
		res[i] = trim(res[i], "'", "'")
	}
	return
}

// SplitInts splits space-separated integers
func SplitInts(str string) (res []int) {
	vals := strings.Fields(str)
	res = make([]int, len(vals))
	for i, v := range vals {
		res[i] = Atoi(v)
	}
	return
}

// SplitFloats splits space-separated float numbers
func SplitFloats(str string) (res []float64) {
	vals := strings.Fields(str)
	res = make([]float64, len(vals))
	for i, v := range vals {
		res[i] = Atof(v)
	}
	return
}
