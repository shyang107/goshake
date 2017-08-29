package util

import (
	"fmt"
	"math"
)

// math ---------------------------------------------------------

// Imax returns the maximum value
func Imax(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Imin returns the minimum value
func Imin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

// Isum returns the summation
func Isum(vals ...int) int {
	sum := 0
	for _, v := range vals {
		sum += v
	}
	return sum
}

// Max returns the maximum value
func Max(a, b float64) float64 {
	if a > b {
		return a
	}
	return b
}

// Min returns the minimum value
func Min(a, b float64) float64 {
	if a < b {
		return a
	}
	return b
}

// Sum returns the summation
func Sum(vals ...float64) float64 {
	sum := 0.0
	for _, v := range vals {
		sum += v
	}
	return sum
}

// Amax return the maximum absolute value and the corressponding index of loation in x[]
func Amax(x ...float64) (idx int, xmax float64, err error) {
	if x == nil {
		err = fmt.Errorf("arg x is %v", x)
	}
	if len(x) == 0 {
		err = fmt.Errorf("arg. len(x[]) = 0")
	}
	if err != nil {
		return 0, 0., err
	}

	idx = 0
	xmax = x[0]
	for i, v := range x {
		xa := math.Abs(v)
		if xa > xmax {
			xmax = xa
			idx = i
		}
	}
	return idx, xmax, nil
}

// IntRange using args [start, stop, step] to generate a set of serial numbers
func IntRange(args ...int) <-chan int {
	var start, stop, step int
	switch len(args) {
	case 1: // 1 argument: stop
		start = 0
		stop = args[0]
		step = 1
	case 2: // 2 arguments: start, stop
		start = args[0]
		stop = args[1]
		step = 1
	case 3: // 3 arguments: start, stop, step
		start = args[0]
		stop = args[1]
		step = args[2]
	default: // invalid argument count
		panic("IntRange takes 1 to 3 arguments.")
	}
	ch := make(chan int)
	if step >= 0 {
		// increment case
		go func() {
			for i := start; i < stop; i += step {
				ch <- i
			}
			close(ch)
		}()
	} else {
		// decrement case
		go func() {
			for i := start; i > stop; i += step {
				ch <- i
			}
			close(ch)
		}()
	}
	return ch
}
