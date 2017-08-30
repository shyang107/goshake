package shake

import (
	"fmt"
	"strings"

	"github.com/shyang107/goshake/shake/util"
)

const (
	// MaxMaterial (op1) = maximum number of material included
	MaxMaterial = 13
	// MaxStrainValues (op1) = maximum number of strain values
	MaxStrainValues = 20
	// MaxDampingValues (op1) = maximum number of strain values
	MaxDampingValues = 20
)

// DynamicSoilProperty is Option#1 for dynamic soil property
type DynamicSoilProperty struct { // Option#1
	// Indentification:
	// Number : number of material included (<= MaxMaterial=13)
	optionIndentification
	// Number in STN : number of strain values to be readed (<= MaxStrainValues = 20)
	// Indentification in STN : identification for this set of modulus reduction values
	// Values[] in STN : strain values, in percent, begining with the lowest value.
	// GRT[] in STN : values of modulus reduction (G/Gmax) each corresponding to the shear strain
	//				  provided in the  strain values; these values should be in decimal
	//			      not in percent
	STN []strainValues
	// Number in DMP[] : number of damping values to be readed (<= MaxStrainValues = 20)
	// Indentification in DMP[] : identification for this set of damping values
	// Values[] in DMP[] : damping values in percent
	// GRT[] in DMP[] : values of modulus reduction (G/Gmax) each corresponding to the dampings
	//				  provided in the damping values; these values should be in decimal
	//			      not in percent
	DMP []dampingValues
	// N is the number of materials to be used in the this analysis
	N int
	// ID stored the material number which will be used, len(ID) = N
	ID []int
}

type reductionValues struct {
	// Values are dyanmic strain values
	Values []float64
	// GRT are G/Gmax corresponding to Values[]
	GRT []float64
}

type strainValues struct {
	optionIndentification
	reductionValues
}

func (s strainValues) String() string {
	shead := "Strains = ["
	ghead := "G/Gmax = ["
	return s.reductionValues.valTable(s.Identification, shead, ghead)
}

func (r reductionValues) valTable(title, shead, ghead string) string {
	nval := len(r.Values)
	var sizes = make([]int, nval+1)
	sizes[0] = len(shead)
	sizes[0] = util.Imax(sizes[0], len(ghead))
	n := sizes[0]
	for i := 0; i < nval; i++ {
		sizes[i+1] = len(util.Sf("%v", r.Values[i]))
		sizes[i+1] = util.Imax(sizes[i+1], len(util.Sf("%v", r.GRT[i])))
		n += sizes[i+1]
		n++
	}
	n += 2 // for " ]"
	l := len(title)
	m := (n - l) / 2
	tab := util.StrThickLine(n)
	tab += util.StrSpaces(m)
	tab += title + "\n"
	sNumber := util.Sf("Number of values = %d", len(r.Values))
	ln := len(sNumber)
	mn := (n - ln) / 2
	tab += util.StrSpaces(mn)
	tab += sNumber + "\n"
	tab += util.StrThinLine(n)
	tab += util.Sf("%*s", sizes[0], shead)
	for i := 0; i < nval; i++ {
		tab += util.Sf(" %*v", sizes[i+1], r.Values[i])
	}
	tab += " ]\n"
	tab += util.Sf("%*s", sizes[0], ghead)
	for i := 0; i < nval; i++ {
		tab += util.Sf(" %*v", sizes[i+1], r.GRT[i])
	}
	tab += " ]\n"
	tab += util.StrThickLine(n)
	return tab
}

type dampingValues struct {
	optionIndentification
	reductionValues
}

func (s dampingValues) String() string {
	shead := "Dampings = ["
	ghead := "G/Gmax = ["
	return s.reductionValues.valTable(s.Identification, shead, ghead)
}

func (dsp *DynamicSoilProperty) read(lines []string, no *int) {
	util.DebugPrintCaller()

	var nmat int
	_, err := fmt.Sscanf(lines[*no], "%5d", &nmat)
	util.CheckErr(err)
	*no++
	util.Pf("%15s = %d\n", "number of material", nmat)
	if dsp.Number > MaxMaterial {
		err := fmt.Errorf("number of material (%d) > %d", nmat, MaxMaterial)
		util.CheckErr(err)
	}
	//
	dsp.Number = nmat
	dsp.STN = make([]strainValues, dsp.Number)
	dsp.DMP = make([]dampingValues, dsp.Number)
	for i := 0; i < dsp.Number; i++ {
		util.Pfgreen("Material #%d:\n", i+1)
		dsp.readStrains(i, lines, no)
		dsp.readDampings(i, lines, no)
	}
	v := util.SplitInts(lines[*no])
	*no++
	dsp.N = v[0]
	dsp.ID = v[1:]
	tab := util.ArgsTable(
		"",
		"number of material to be used", "N", dsp.N,
		"naterial number which will be used", "ID[]", util.IntSf("%v", dsp.ID))
	util.Pf("%v", tab)
}

func (dsp *DynamicSoilProperty) readStrains(i int, lines []string, no *int) {
	_, err := fmt.Sscanf(lines[*no], "%5d", &dsp.STN[i].Number)
	util.CheckErr(err)
	if dsp.STN[i].Number > MaxStrainValues {
		err := fmt.Errorf("number of strian values (%d) > %d", dsp.STN[i].Number, MaxStrainValues)
		util.CheckErr(err)
	}
	dsp.STN[i].Identification = strings.Trim(lines[*no][5:], " ")
	*no++
	// util.Pf("\tnumber of strain values = %d -- %s\n", dsp.STN[i].Number, dsp.STN[i].Identification)
	dsp.STN[i].Values = readOpt1Values(lines, no, dsp.STN[i].Number)
	// util.Pf("\t  mat.%2d: %v : %d\n", i+1, strains, len(strains))
	dsp.STN[i].GRT = readOpt1Values(lines, no, dsp.STN[i].Number)
	// util.Pf("\t  mat.%2d: %v : %d\n", i+1, stngratios, len(stngratios))
	util.Pf("%s", dsp.STN[i])
}

func (dsp *DynamicSoilProperty) readDampings(i int, lines []string, no *int) {
	_, err := fmt.Sscanf(lines[*no], "%5d", &dsp.DMP[i].Number)
	util.CheckErr(err)
	if dsp.DMP[i].Number > MaxDampingValues {
		err := fmt.Errorf("number of damping values (%d) > %d", dsp.DMP[i].Number, MaxDampingValues)
		util.CheckErr(err)
	}
	dsp.DMP[i].Identification = strings.Trim(lines[*no][5:], " ")
	*no++
	// util.Pf("\tnumber of damping values = %d -- %s\n", dsp.DMP[i].Number, dsp.DMP[i].Identification)
	dsp.DMP[i].Values = readOpt1Values(lines, no, dsp.DMP[i].Number)
	// util.Pf("\t  mat.%2d: %v : %d\n", i+1, strains, len(strains))
	dsp.DMP[i].GRT = readOpt1Values(lines, no, dsp.DMP[i].Number)
	// util.Pf("\t  mat.%2d: %v : %d\n", i+1, stngratios, len(stngratios))
	util.Pf("%s", dsp.DMP[i])
}

func readOpt1Values(lines []string, no *int, nStrains int) []float64 {
	var stn []float64
	for i := 0; i < nStrains; i += 8 {
		values := util.SplitFloats(lines[*no])
		stn = append(stn, values...)
		*no++
	}
	return stn
}
