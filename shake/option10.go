package shake

import (
	"strings"

	"github.com/cpmech/gosl/io"
)

// ComputeAmplificationSpectrum is Option#10 to compute amplification spectrum
type ComputeAmplificationSpectrum struct { // Option#10
	// Identification information
	Identification string
	// ID1 : number of first sublayer
	ID1 int
	// Type1 = 0 for outcroping or = 1 for within soil profile
	Type1 int
	// ID2 : number of second sublayer
	ID2 int
	// Type2 = 0 for outcroping or = 1 for within soil profile
	Type2 int
	// DFreq = frequency step (in cycles in second); the amplification spectrum is calculated
	//		   for 200 frequencies using this frrequency step and starting with 0
	DFreq float64
	// Info = Identification information
	Info string
}

func (s ComputeAmplificationSpectrum) String() string {
	tab := io.ArgsTable(
		s.Identification,
		"number of first sublayer", "ID1", s.ID1,
		"Type = 0 for outcroping or = 1 for within", "Type1", s.Type1,
		"number of second sublayer", "ID2", s.ID2,
		"Type = 0 for outcroping or = 1 for within", "Type2", s.Type2,
		"frequency step (in cycles in second)", "DFreq", s.DFreq,
		"Identification information", "Info", s.Info)
	return tab
}

func (s *ComputeAmplificationSpectrum) read(lines []string, no *int) {
	vals := io.SplitInts(lines[*no][:20])
	s.ID1, s.Type1, s.ID2, s.Type2 = vals[0], vals[1], vals[2], vals[3]
	s.DFreq = io.Atof(strings.Trim(lines[*no][20:30], " "))
	s.Info = strings.Trim(lines[*no][30:], " -")
	*no++
	io.Pf("%s", *s)
}
