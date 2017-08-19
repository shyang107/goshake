package shake

import (
	"github.com/cpmech/gosl/io"
)

const (
	// MaxSubAccn can specified maximum number of sublayers;
	// if accelerations for more than this number of sublayers are desired, then repeat Option 6
	// as many times as needed
	MaxSubAccn = 15
)

// SubAcceleration is Option#6 for computation of acceleration at top of specified sublayers
type SubAcceleration struct { // Option#6
	// Identification : information for this option
	Identification string
	// LayID : array idicating the numbers of the sublayers at the top of which the acceleration is to be calculated
	LayID []int
	// Type : array specifing types of above sublayer:
	//		0 for outcrooping or
	//		1 for whithin the soil profile
	Type []int
	// Mode : arrat to specify the mode of output for the computed acceleraitons:
	//		0 if only maximum acceleraiton is desired
	//		1 if both the maximum acceleration and the time history of acceleration
	//		  are to be calcylated and saved
	Mode []int
}

func (sa SubAcceleration) String() string {

	tab := io.ArgsTable(
		sa.Identification,
		"numbers of the sublayers", "LayID[]", io.IntSf("%3d", sa.LayID),
		"types of above sublayer", "Type[]", io.IntSf("%3d", sa.Type),
		"mode of output", "Mode[]", io.IntSf("%3d", sa.Mode))
	return tab
}

func (sa *SubAcceleration) read(lines []string, no *int) {
	sa.LayID = io.SplitInts(lines[*no])
	*no++
	sa.Type = io.SplitInts(lines[*no])
	*no++
	sa.Mode = io.SplitInts(lines[*no])
	*no++
	io.Pf("%s", *sa)
}
