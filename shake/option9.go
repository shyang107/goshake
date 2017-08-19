package shake

import (
	"strings"

	"github.com/cpmech/gosl/io"
)

// ComputeResponseSpectrum is Option#9 to compute response spectrum
type ComputeResponseSpectrum struct { // Option#9
	// Identification information
	Identification string
	// ID : sublayer number
	ID int
	// Type = 0 for outcroping or = 1 for within soil profile
	Type int
	// NDamp : number of damping ratio to be used
	NDamp int
	// GGT : acceleration of gravity
	GGT float64
	// Dampings : array for damping ratio (in decimal)
	Dampings []float64
}

func (s ComputeResponseSpectrum) String() string {
	tab := io.ArgsTable(
		s.Identification,
		"sublayer number", "ID", s.ID,
		"Type = 0 for outcroping or = 1 for within", "Type", s.Type,
		"acceleration of gravity", "GGT", s.GGT,
		"number of damping ratio to be used", "NDamp", s.NDamp,
		"array for damping ratio (in decimal)", "Dampings[]", "as the following...")
	tab += io.Sf("%s : %v\n", "Dampings", s.Dampings)
	return tab
}

func (s *ComputeResponseSpectrum) read(lines []string, no *int) {
	vals := io.SplitInts(lines[*no])
	s.ID = vals[0]
	s.Type = vals[1]
	*no++
	vals = io.SplitInts(lines[*no][0:5])
	s.NDamp = vals[0]
	s.GGT = io.Atof(strings.Trim(lines[*no][10:20], " "))
	*no++
	s.Dampings = io.SplitFloats(lines[*no])
	*no++
	io.Pf("%s", *s)
}
