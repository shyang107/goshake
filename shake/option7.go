package shake

import (
	"strings"

	"github.com/cpmech/gosl/io"
	"github.com/shyang107/util"
)

// ShearStressOrStrain is Option#7 for sublayer at top which time history of shear stress
// or strain is computed and saved
type ShearStressOrStrain struct {
	// Identification : information for this option
	Identification string
	// OPSet : setings of computational of shear stress or strain time history at top
	//			if specified sublayers
	OPSet [][2]SoSOutput
}

var sshead = []string{"ID", "Type", "THMode", "Number", "   Identification"}

func (s ShearStressOrStrain) String() string {
	title := s.Identification
	var sizes = make([]int, len(sshead))
	for i := 0; i < len(sshead); i++ {
		sizes[i] = len(sshead[i])
	}
	for j := 0; j < len(s.OPSet); j++ {
		for i := 0; i < 2; i++ {
			sizes[0] = util.Imax(sizes[0], len(io.Sf("%v", s.OPSet[j][i].ID)))
			sizes[1] = util.Imax(sizes[1], len(io.Sf("%v", s.OPSet[j][i].Type)))
			sizes[2] = util.Imax(sizes[2], len(io.Sf("%v", s.OPSet[j][i].THMode)))
			sizes[3] = util.Imax(sizes[3], len(io.Sf("%v", s.OPSet[j][i].Number)))
			sizes[4] = util.Imax(sizes[4], len(io.Sf("%v", s.OPSet[j][i].Identification)))
		}
	}
	sizes[4] += 3
	n := util.Isum(sizes...) + len(sizes)
	l := len(title)
	m := (n - l) / 2
	tab := io.StrSpaces(m)
	tab += title
	tab += "\n"
	tab += io.StrThickLine(n)
	for i := 0; i < len(sshead); i++ {
		tab += io.Sf(" %*s", sizes[i], sshead[i])
	}
	tab += "\n"
	tab += io.StrThinLine(n)
	for j := 0; j < len(s.OPSet); j++ {
		for i := 0; i < 2; i++ {
			tab += io.Sf(" %*v", sizes[0], s.OPSet[j][i].ID)
			tab += io.Sf(" %*v", sizes[1], s.OPSet[j][i].Type)
			tab += io.Sf(" %*v", sizes[2], s.OPSet[j][i].THMode)
			tab += io.Sf(" %*v", sizes[3], s.OPSet[j][i].Number)
			tab += io.Sf(" -- %*v", sizes[4]-3, s.OPSet[j][i].Identification)
			tab += "\n"
		}
	}
	tab += io.StrThickLine(n)
	return tab
}

// SoSOutput is computational of shear stress or strain time history at top if specified sublayers
type SoSOutput struct { // Option#6
	// ID number of sublayer
	ID int
	// Type = 0 for strain or 1 for stress
	Type int
	// Mode = 1 to save time history of strain or stress
	THMode int
	// Number : number of values to be saved; typically this should be equal to
	//			the number NV (see option 3)
	Number int
	// Identification information
	Identification string
}

func ssoToString(ss [2]SoSOutput) string {
	var sizes = make([]int, len(sshead))
	for i := 0; i < len(sshead); i++ {
		sizes[i] = len(sshead[i])
	}
	for i := 0; i < 2; i++ {
		sizes[0] = util.Imax(sizes[0], len(io.Sf("%v", ss[i].ID)))
		sizes[1] = util.Imax(sizes[1], len(io.Sf("%v", ss[i].Type)))
		sizes[2] = util.Imax(sizes[2], len(io.Sf("%v", ss[i].THMode)))
		sizes[3] = util.Imax(sizes[3], len(io.Sf("%v", ss[i].Number)))
		sizes[4] = util.Imax(sizes[4], len(io.Sf("%v", ss[i].Identification)))
	}
	sizes[4] += 3
	n := util.Isum(sizes...) + len(sizes)
	tab := io.StrThickLine(n)
	for i := 0; i < len(sshead); i++ {
		tab += io.Sf(" %*s", sizes[i], sshead[i])
	}
	tab += "\n"
	tab += io.StrThinLine(n)
	for i := 0; i < 2; i++ {
		tab += io.Sf(" %*v", sizes[0], ss[i].ID)
		tab += io.Sf(" %*v", sizes[1], ss[i].Type)
		tab += io.Sf(" %*v", sizes[2], ss[i].THMode)
		tab += io.Sf(" %*v", sizes[3], ss[i].Number)
		tab += io.Sf(" -- %*v", sizes[4]-3, ss[i].Identification)
		tab += "\n"
	}
	tab += io.StrThickLine(n)
	return tab
}

func (s *ShearStressOrStrain) read(lines []string, no *int) {
	var so [2]SoSOutput
	vals := io.SplitInts(lines[*no][0:25])
	so[0].ID, so[0].Type, so[0].THMode, so[0].Number = vals[0], vals[1], vals[2], vals[4]
	so[0].Identification = strings.Trim(lines[*no][25:], " -")
	*no++
	vals = io.SplitInts(lines[*no][0:25])
	so[1].ID, so[1].Type, so[1].THMode, so[1].Number = vals[0], vals[1], vals[2], vals[4]
	so[1].Identification = strings.Trim(lines[*no][25:], " -")
	*no++
	s.OPSet = append(s.OPSet, so)
	io.Pf("%s", ssoToString(so))
	// io.Pf("%s", *s)
}
