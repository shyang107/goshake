package cfg

import (
	"github.com/cpmech/gosl/io"
	"github.com/widuu/goini"
)

const (
	CfgFileName = "./config.ini"
	Version     = "v0.0.1"
)

var (
	conf *goini.Config
	// Verbose is the state of turn-on/off verbose logging
	Verbose bool = true

	ColorsOn bool = true
	// InputFileName is the file name of input-data
	InputFileName string
	// AccFileName is the file name of acceleration records
	AccFileName string
	// OutputFileName is the file name of output-data
	OutputFileName string
)

func init() {
	Verbose = true
	conf = goini.SetConfig(CfgFileName)
}

// SetVerbose sets the state of Verbose
func SetVerbose(verbose bool) {
	Verbose = verbose
}

// GetConfig gets the configuration from cfgFileName
func GetConfig() {
	InputFileName = conf.GetValue("input", "InputFileName")
	AccFileName = conf.GetValue("input", "AccFileName")
	OutputFileName = conf.GetValue("output", "OutputFileName")
	Verbose = io.Atob(conf.GetValue("output", "Verbose"))
	SetVerbose(Verbose)
	io.Verbose = Verbose
	if Verbose {
		tab := io.ArgsTable(
			io.Sf("Reading the basic configuration from %q:", CfgFileName),
			"Input file", "InputFileName", InputFileName,
			"Acceleration data", "AccFileName", AccFileName,
			"Output file", "OutputFileName", OutputFileName,
			"Show messages on console", "Verbose", Verbose)
		io.Pfcyan2("%v\n", tab)
	}
}
