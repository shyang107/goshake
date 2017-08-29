package shake

import (
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/shyang107/goshake/shake/cfg"

	"github.com/cpmech/gosl/io"
)

var (
	// Options are all potions and data needed in the analysis
	Options *OptionAll
)

// OptionAll is that all potion and data using in the analysis
type OptionAll struct {
	DSP *DynamicSoilProperty // Option#1
	SP  *SoilProfile         // Option#2
	IM  *InputMotion         // Option#3
	AOM *AssignObjectMotion  // Option#4
	IS  *IterStrain          // Option#5
	SA  *SubAcceleration     // Option#6
	SS  *ShearStressOrStrain // Option#7
	// OM  ObjectMotion                 // Option#8
	CRS *ComputeResponseSpectrum      // Option#9
	CAS *ComputeAmplificationSpectrum // Option#10
	// CFA ComputeFourierAmplitude      // Option#11
}

type optionIndentification struct {
	Identification string // Identification information for this option (this line cannot be blank)
	Number         int    // somtimes is Option number
}

func (o optionIndentification) String() string {
	return fmt.Sprintf("%s\n%15s = %5d", o.Identification, "Option number", o.Number)
}

// // ObjectMotion is Option#8 to save time history of object motion
// type ObjectMotion struct {
// 	optionIndentification
// }

// ComputeFourierAmplitude is Option#11 to compute Fourier amplitudes
type ComputeFourierAmplitude struct {
	optionIndentification
}

// ReadOptions read the opetions
func (o *OptionAll) ReadOptions() {
	b, err := ioutil.ReadFile(cfg.InputFileName)
	check(err)
	str := strings.Replace(toString(b), "\r", "", -1)
	lines := strings.Split(str, "\n")
	o.Read(lines)
}

func (o *OptionAll) Read(lines []string) {
	var no int
	stop := false
	for !stop {
		stop = o.readData(lines, &no) // read Option #N data
	}
}

func (o *OptionAll) readData(lines []string, no *int) (stop bool) {
	var hopt optionIndentification
	hopt.Identification = strings.Trim(lines[*no], " ")
	*no++
	_, err := fmt.Sscanf(lines[*no], "%5d", &hopt.Number)
	check(err)
	io.PfCyan(FORMAT[23], hopt.Number)
	if hopt.Number == 0 {
		io.PfYel("%s\n", hopt.Identification)
		io.PfBlue(FORMAT[24], hopt.Number)
		io.Pfdyel2("%s", io.StrThickLine(60))
		return true
	}
	*no++
	o.readOptionNo(lines, no, hopt)
	io.PfBlue(FORMAT[24], hopt.Number)
	io.Pfdyel2("%s", io.StrThickLine(60))
	return false
}

func (o *OptionAll) readOptionNo(lines []string, no *int, op optionIndentification) {
	switch op.Number {
	case 1: // Option#1: dynamic soil property
		var dsp = new(DynamicSoilProperty)
		dsp.Identification = op.Identification
		dsp.read(lines, no)
		o.DSP = dsp
	case 2: // Option#2: the data for soil profile
		var sp = new(SoilProfile)
		sp.Identification = op.Identification
		sp.read(lines, no)
		o.SP = sp
	case 3: // Option#3: input(object) motion
		var im = new(InputMotion)
		im.Identification = op.Identification
		im.read(lines, no)
		o.IM = im
	case 4: // Option#4: assignment of object motion
		var aom = new(AssignObjectMotion)
		aom.Identification = op.Identification
		aom.read(lines, no)
		o.AOM = aom
	case 5: // Option$#5: number of iterations specified
		var is = new(IterStrain)
		is.Identification = op.Identification
		is.read(lines, no)
		o.IS = is
	case 6: // Option#6: computation of acceleration at top of specified sublayers
		var sa = new(SubAcceleration)
		sa.Identification = op.Identification
		sa.read(lines, no)
		// o.SA = sa
		io.Pfred("all of option 6:\n")
		if o.SA == nil {
			o.SA = sa
		} else {
			o.SA.LayID = append(o.SA.LayID, sa.LayID...)
			o.SA.Mode = append(o.SA.Mode, sa.Mode...)
			o.SA.Type = append(o.SA.Type, sa.Type...)
		}
		io.Pfpink("%v\n", o.SA)
	case 7: // Option#7: computational of shear stress or strain time history at top if specified sublayers
		var ss = new(ShearStressOrStrain)
		ss.Identification = op.Identification
		ss.read(lines, no)
		// o.SS = ss
		io.Pfred("all of option 7:\n")
		if o.SS == nil {
			o.SS = ss
		} else {
			o.SS.OPSet = append(o.SS.OPSet, ss.OPSet...)
		}
		io.Pfpink("%v\n", o.SS)
	case 9: // Option#9: compute response spectrum
		var ss = new(ComputeResponseSpectrum)
		ss.Identification = op.Identification
		ss.read(lines, no)
		o.CRS = ss
	case 10: // Option#10: compute amplification spectrum
		var ss = new(ComputeAmplificationSpectrum)
		ss.Identification = op.Identification
		ss.read(lines, no)
		o.CAS = ss
	}
}
