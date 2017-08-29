package shake

import (
	"os"
	"strings"

	"github.com/shyang107/goshake/shake/cfg"

	"github.com/cpmech/gosl/chk"
	"github.com/cpmech/gosl/io"
)

const (
	// MAMAX = number of values for Fourier transform
	MAMAX = 4096
)

// InputMotion is Option#3 for input(object) motion
type InputMotion struct { // Option#3
	// Identification : identification for  input(object) motion
	Identification string
	// NV : number of acceleration values to be read for input motion
	NV int
	// MA = number of values for use in Fourier transform;
	//		MA should be a power of 2 (typically, this number is 1024, 2048, or 4096).
	//		Note that MA should always be grater than NV. The following may be used as a guide:
	//		NV <=  800 => MA <- 1024
	//		NV <= 1800 => MA <- 2048
	//		NV <= 3800 => MA <- 4096
	MA int
	// Dt = time interval between acceleration values, in seconds
	Dt float64
	// AccFilename = name of file for input(object) motion
	AccFilename string
	// Format is for reading acceleration values
	Format string
	// Factor = multiplication factor for adjusting acceleration values; use only if PGA = 0.0
	Factor float64
	// PGA = maximum acceleration to be used, in g's; the acceleration values read-in will be
	// 		 scaled to provide the maximum acceleration specified in these PGA.
	//		 PGA = 0.0 if Factor .NE. 0.0
	PGA float64
	// CutoffFreq = cut-off frequency to be used in the analysis
	CutoffFreq float64
	// NHeadlines = number of header lines in file containing object motion
	NHeadlines int
	// Header = header in file containing object motion
	// Header string
	// NAccPerLine = number of acceleration values per line in file containing object motion
	NAccPerLine int
	// AccValues[] = the values of acceleration
	AccValues []float64
}

func (im InputMotion) String() string {
	var strAcc string
	isOutputAcc := true
	if len(im.AccValues) == 0 {
		strAcc = "<nil>"
		isOutputAcc = false
	} else {
		strAcc = "as the following..."
	}
	tab := io.ArgsTable(
		im.Identification,
		"number of acc. values", "NV", im.NV,
		"number of values for use in FFT", "MA", im.MA,
		"time interval", "Dt", im.Dt,
		"name of file for input motion", "AccFilename", im.AccFilename,
		"format reading acc. values", "Format", im.Format,
		"multiplication factor", "Factor", im.Factor,
		"maximum acc.", "PGA", im.PGA,
		"cut-off frequency", "CutoffFreq", im.CutoffFreq,
		"number of header lines in AccFilename", "NHeadlines", im.NHeadlines,
		// "header in file", "Header", im.Header,
		"number of acc. values per line in AccFilename", "NAccPerLine", im.NAccPerLine,
		"the values of acc.", "AccValues[]", strAcc)
	if isOutputAcc {
		title := io.Sf("Data of acceleration, AccValues[]: %d", len(im.AccValues))
		n := im.NAccPerLine*10 + im.NAccPerLine // 10 <- %10.6f
		l := len(title)
		m := (n - l) / 2
		tab += io.StrThickLine(n)
		tab += io.StrSpaces(m)
		tab += title + "\n"
		tab += io.StrThinLine(n)
		for i := 0; i < 3; i++ {
			for j := 0; j < im.NAccPerLine; j++ {
				ij := i*im.NAccPerLine + j
				tab += io.Sf(" %10.6f", im.AccValues[ij])
			}
			tab += "\n"
		}
		for i := 0; i < 3; i++ {
			for j := 0; j < im.NAccPerLine; j++ {
				tab += io.Sf(" %10s", "     :    ")
			}
			tab += "\n"
		}
		tail := len(im.AccValues) - 3*im.NAccPerLine
		for i := 0; i < 3; i++ {
			for j := 0; j < im.NAccPerLine; j++ {
				ij := i*im.NAccPerLine + j
				tab += io.Sf(" %10.6f", im.AccValues[tail+ij])
			}
			tab += "\n"
		}
		tab += io.StrThickLine(n)
	}
	return tab
}

func (im *InputMotion) read(lines []string, no *int) {
	im.NV = io.Atoi(strings.Trim(lines[*no][0:5], " "))
	im.MA = io.Atoi(strings.Trim(lines[*no][5:10], " "))
	im.Dt = io.Atof(strings.Trim(lines[*no][10:20], " "))
	im.AccFilename = strings.Trim(lines[*no][20:50], " ")
	chkAccFilename(&im.AccFilename)
	im.Format = strings.Trim(lines[*no][50:], " ")
	*no++
	//
	sFactor := strings.Trim(lines[*no][0:10], " ")
	sPGA := strings.Trim(lines[*no][10:20], " ")
	im.Factor, im.PGA = chkMaxAcc(sFactor, sPGA)
	im.CutoffFreq = io.Atof(strings.Trim(lines[*no][20:30], " "))
	im.NHeadlines = io.Atoi(strings.Trim(lines[*no][30:35], " "))
	im.NAccPerLine = io.Atoi(strings.Trim(lines[*no][35:40], " "))
	*no++
	//
	im.readAccData()
	io.Pf("%s", *im)
}
func (im *InputMotion) readAccData() {
	n := 0
	io.ReadLines(im.AccFilename, func(idx int, line string) (stop bool) {
		if idx < im.NHeadlines {
			return false
		}
		vals := io.SplitFloats(line)
		n += len(vals)
		if n > im.NV {
			m := n - im.NV
			im.AccValues = append(im.AccValues, vals[0:m]...)
			return true
		}
		im.AccValues = append(im.AccValues, vals...)
		return false
	})
}
func chkMaxAcc(sFactor, sPGA string) (factor, pga float64) {
	if len(sFactor) == 0 && len(sPGA) == 0 {
		panic(chk.Err("multiplication factor = %q and maximum acceleration = %q", sFactor, sPGA))
	}
	if len(sFactor) == 0 {
		pga = io.Atof(sPGA)
	} else {
		factor = io.Atof(sFactor)
	}
	return
}

func chkAccFilename(afn *string) {
	afname := os.ExpandEnv(*afn)
	cfname := os.ExpandEnv(cfg.AccFileName)
	// io.Pf("afname = %s   cfname = %s\n", afname, cfname)
	if _, aerr := os.Stat(afname); os.IsNotExist(aerr) {
		if _, cerr := os.Stat(cfname); cerr == nil {
			afname = cfname
		}
	}
	*afn = afname
	// io.Pf("im.AccFilename = %s\n", *afn)
}
