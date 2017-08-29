package util

import (
	"fmt"

	"github.com/shyang107/go-twinvoices/pencil/ansi256"
	"github.com/shyang107/go-twinvoices/pencil/ansi8"
)

// code of string format
const (
	Fcstart = 101
	Fcstop  = 102
	Fostart = 111
	Fostop  = 112
	Ffstart = 21
	Ffstop  = 22
)

var (

	// Verbose activates display of messages on console
	Verbose = true

	// ColorsOn activates use of colors on console
	ColorsOn = true

	// Format is the format of message
	Format = map[int]string{
		// config
		Fcstart: "# Start to configure. -- %q\n",
		Fcstop:  "# Configuration has been concluded. -- %q\n",
		// option
		Fostart: "# Start to get case-options. -- %q\n",
		Fostop:  "# Case-options has been concluded. -- %q\n",
		// start/end function
		Ffstart: "* Function %q start.\n",
		Ffstop:  "* Function %q stop.\n",
	}
)

// specified function for output
var (
	Pfstart = PfCyan
	Pfstop  = PfBlue
	Pfsep   = Pfdyel2
	Prun    = PfYel
	Pchk    = Pfgreen2
	Pstat   = Pfyel
	Plog    = Pf
	Pinfo   = Pfcyan2
	Pwarn   = Pforan
	Perr    = Pfred
	Pdebug  = Pfgreen2
)

// print ---------------------------------------------------------

// PrintSepline print the separate-line
func PrintSepline(n int) {
	if n <= 0 {
		n = 60
	}
	Pfsep("%s", StrThinLine(n))
}

// PrintFormat commands ---------------------------------------------------------
// modified from "github.com/cpmech/gosl/io"

// Pl prints new line
func Pl() {
	if !Verbose {
		return
	}
	fmt.Println()
}

// low intensity

// Pf prints formatted string
func Pf(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	fmt.Printf(format, prm...)
}

// Pfcyan prints formatted string in cyan
func Pfcyan(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[0;36m"+format+"[0m", prm...)
		fmt.Print(ansi8.CyanString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfcyan2 prints formatted string in another shade of cyan
func Pfcyan2(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;50m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadeCyanString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfyel prints formatted string in yellow
func Pfyel(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[0;33m"+format+"[0m", prm...)
		fmt.Print(ansi8.YellowString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfdyel prints formatted string in dark yellow
func Pfdyel(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;58m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadeYellowString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfdyel2 prints formatted string in another shade of dark yellow
func Pfdyel2(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;94m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadeYellowString2(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfred prints formatted string in red
func Pfred(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[0;31m"+format+"[0m", prm...)
		fmt.Print(ansi8.RedString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfgreen prints formatted string in green
func Pfgreen(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[0;32m"+format+"[0m", prm...)
		fmt.Print(ansi8.GreenString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfblue prints formatted string in blue
func Pfblue(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[0;34m"+format+"[0m", prm...)
		fmt.Print(ansi8.BlueString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfmag prints formatted string in magenta
func Pfmag(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[0;35m"+format+"[0m", prm...)
		fmt.Print(ansi8.MagentaString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pflmag prints formatted string in light magenta
func Pflmag(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[0;95m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiMagentaString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfpink prints formatted string in pink
func Pfpink(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;205m"+format+"[0m", prm...)
		fmt.Print(ansi256.PinkString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfdgreen prints formatted string in dark green
func Pfdgreen(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;22m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadeGreenString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfgreen2 prints formatted string in another shade of green
func Pfgreen2(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;2m"+format+"[0m", prm...)
		fmt.Print(ansi256.GreenString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfpurple prints formatted string in purple
func Pfpurple(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;55m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadePurpleString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfgrey prints formatted string in grey
func Pfgrey(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;59m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadeGrayString1(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfblue2 prints formatted string in another shade of blue
func Pfblue2(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;69m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadeBlueString2(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pfgrey2 prints formatted string in another shade of grey
func Pfgrey2(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;60m"+format+"[0m", prm...)
		fmt.Print(ansi256.ShadeGrayString2(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// Pforan prints formatted string in orange
func Pforan(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[38;5;202m"+format+"[0m", prm...)
		fmt.Print(ansi256.OrangeString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// high intensity

// PfCyan prints formatted string in high intensity cyan
func PfCyan(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[1;36m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiCyanString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// PfYel prints formatted string in high intensity yello
func PfYel(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[1;33m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiYellowString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// PfRed prints formatted string in high intensity red
func PfRed(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[1;31m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiRedString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// PfGreen prints formatted string in high intensity green
func PfGreen(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[1;32m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiGreenString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// PfBlue prints formatted string in high intensity blue
func PfBlue(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[1;34m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiBlueString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// PfMag prints formatted string in high intensity magenta
func PfMag(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[1;35m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiMagentaString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}

// PfWhite prints formatted string in high intensity white
func PfWhite(format string, prm ...interface{}) {
	if !Verbose {
		return
	}
	if ColorsOn {
		// fmt.Printf("[1;37m"+format+"[0m", prm...)
		fmt.Print(ansi8.HiWhiteString(format, prm...))
	} else {
		fmt.Printf(format, prm...)
	}
}
