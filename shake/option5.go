package shake

import (
	"strings"

	"github.com/cpmech/gosl/io"
)

// IterStrain is Option$#5 for number of iterations specified
// and ratio of uniform strain to maxstrain
type IterStrain struct { // Option$#5
	// Identification information for this option
	Identification string
	// IsSaveProp : parameter used to specify whether the strain-compatible soil properties
	//		are saved after the final iteration;
	//		set true if these properties are to be saved; otherwise set 0 (blank)
	IsSaveProp bool
	// NIteration : number of iterations
	NIteration int
	// ESR : ratio of equivalent uniform strain divided by maximum strain;
	//		 typically this ratio ranges from  0.4 to 0.75 depending on the input motion and
	//		 which magnitude earthquake it is itended to represent.
	//		 the following equation may be used to eastimate this ratio:
	//				[ratio = (M-1)/10]
	//		 in which M is the magnitude of the earthquake. Thus, for M = 5,
	//		 the ratio would be 0.4, for M = 7.5, the ratio would be 0.65 ... etc.
	ESR float64
}

func (it IterStrain) String() string {
	tab := io.ArgsTable(
		it.Identification,
		"save the strain-compatible soil properties after the final iteration", "IsSaveProp", it.IsSaveProp,
		"number of iterations", "NIteration", it.NIteration,
		"ratio of equivalent uniform strain divided by maximum strain", "ESR", it.ESR)
	return tab
}

func (it *IterStrain) read(lines []string, no *int) {
	sIsSaveProp := strings.Trim(lines[*no][0:5], " ")
	if len(sIsSaveProp) == 0 {
		it.IsSaveProp = false
	} else {
		it.IsSaveProp = io.Atob(sIsSaveProp)
	}
	it.NIteration = io.Atoi(strings.Trim(lines[*no][5:10], " "))
	it.ESR = io.Atof(strings.Trim(lines[*no][10:], " "))
	*no++
	io.Pf("%s", *it)
}
