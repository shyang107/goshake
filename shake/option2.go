package shake

import (
	"strings"

	"github.com/cpmech/gosl/io"
	"github.com/shyang107/go-twinvoices/util"
)

// SoilProfile is Option#2 that the data for soil profile
type SoilProfile struct { //Option#2
	// Number : soil dposit number, may be left blank
	// Indentification : identification for soil profile
	optionIndentification
	// NLayer : number of sublayers, including the half-space
	NLayer int
	// Layer[] : sublayers
	Layer []Sublayer
}

// Sublayer : propperties of sublayer
type Sublayer struct {
	ID    int     // sublayer number
	SID   int     // soil type
	Thick float64 // thickness of sublayer in feet
	Gmax  float64 // maximum shear modulus for the sublayer, in ksf
	// (Gmax = 0.0, if maximum shear velocity for the sublayer is given)
	Damping0   float64 // initial estimate of damping (decimal)
	UnitWeight float64 // total unit weight, in ksf
	Vsmax      float64 // maximum shear wave velocity for the sublayer, in ft/sec
	// (Vsmax = 0.0, if maximum shear modulus for the sublayer is given)
}

func (sp SoilProfile) String() string {
	tab := io.ArgsTable(
		sp.Identification,
		"soil dposit number", "Number", sp.Number,
		"number of sublayers", "NLayer", sp.NLayer,
		"data of sublayers", "Layer[]", "as the following...",
	)
	title := "Layer[]"
	heads := []string{"ID", "SID", "Thick", "Gmax", "Damping0", "UnitWeight", "Vsmax"}
	nheads := len(heads)
	nlines := len(sp.Layer)
	var sizes = make([]int, nheads)
	for i := 0; i < nheads; i++ {
		sizes[i] = len(heads[i])
	}
	for i := 0; i < nlines; i++ {
		sizes[0] = util.Imax(sizes[0], len(io.Sf("%v", sp.Layer[i].ID)))
		sizes[1] = util.Imax(sizes[1], len(io.Sf("%v", sp.Layer[i].SID)))
		sizes[2] = util.Imax(sizes[2], len(io.Sf("%v", sp.Layer[i].Thick)))
		sizes[3] = util.Imax(sizes[3], len(io.Sf("%v", sp.Layer[i].Gmax)))
		sizes[4] = util.Imax(sizes[4], len(io.Sf("%v", sp.Layer[i].Damping0)))
		sizes[5] = util.Imax(sizes[5], len(io.Sf("%v", sp.Layer[i].UnitWeight)))
		sizes[6] = util.Imax(sizes[6], len(io.Sf("%v", sp.Layer[i].Vsmax)))
	}
	var n int
	var strfmt string
	for i := 0; i < nheads; i++ {
		n += sizes[i]
		strfmt += io.Sf(" %%%dv", sizes[i]+1)
	}
	n += nheads * 2
	strfmt += "\n"
	// strfmt = strfmt[1:]
	l := len(title)
	m := (n - l) / 2
	tab += io.StrSpaces(m)
	tab += title + "\n"
	tab += io.StrThickLine(n)
	for i := 0; i < nheads; i++ {
		tab += io.Sf(" %*v", sizes[i]+1, heads[i])
	}
	tab += "\n"
	tab += io.StrThinLine(n)
	for i := 0; i < nlines; i++ {
		tab += io.Sf(strfmt,
			sp.Layer[i].ID,
			sp.Layer[i].SID,
			sp.Layer[i].Thick,
			sp.Layer[i].Gmax,
			sp.Layer[i].Damping0,
			sp.Layer[i].UnitWeight,
			sp.Layer[i].Vsmax)
	}
	tab += io.StrThickLine(n)
	return tab
}

func (sp *SoilProfile) read(lines []string, no *int) {
	sp.Number = io.Atoi(strings.Trim(lines[*no][0:5], " "))
	sp.NLayer = io.Atoi(strings.Trim(lines[*no][5:10], " "))
	sp.Identification = strings.Trim(lines[*no][10:], " ")
	*no++
	sp.Layer = make([]Sublayer, sp.NLayer)
	for i := 0; i < sp.NLayer; i++ {
		sID := strings.Trim(lines[*no][0:5], " ")
		sSID := strings.Trim(lines[*no][5:10], " ")
		sThick := strings.Trim(lines[*no][10:25], " ")
		checkZero(&sThick)
		sGmax := strings.Trim(lines[*no][25:35], " ")
		checkZero(&sGmax)
		sDamping0 := strings.Trim(lines[*no][35:45], " ")
		sUnitWeight := strings.Trim(lines[*no][45:55], " ")
		sVsmax := strings.Trim(lines[*no][55:65], " ")
		checkZero(&sVsmax)
		sp.Layer[i] = Sublayer{
			ID:         io.Atoi(sID),
			SID:        io.Atoi(sSID),
			Thick:      io.Atof(sThick),
			Gmax:       io.Atof(sGmax),
			Damping0:   io.Atof(sDamping0),
			UnitWeight: io.Atof(sUnitWeight),
			Vsmax:      io.Atof(sVsmax),
		}
		*no++
	}
	io.Pf("%s", sp)
}

func checkZero(s *string) {
	if len(*s) == 0 {
		*s = "0.0"
	}
}
