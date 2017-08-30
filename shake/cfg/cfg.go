package cfg

import (
	"github.com/shyang107/util"
	"github.com/widuu/goini"
	yaml "gopkg.in/yaml.v2"
)

const (
	// DefaultCasesPath is the default case path of `goshake`
	DefaultCasesPath = "./cases.yaml"

	// CfgFileName = "./config.ini"

	// Version of `goshake`
	Version = "v0.0.1"
)

var (
	conf *goini.Config
	// Verbose is the state of turn-on/off verbose logging
	Verbose bool = true

	ColorsOn bool = true
	// // InputFileName is the file name of input-data
	// InputFileName string
	// // AccFileName is the file name of acceleration records
	// AccFileName string
	// // OutputFileName is the file name of output-data
	// OutputFileName string

	// // CasesPath is the path of case file use in `goshake`
	// CasesPath string

	// // Cases contains the information of all case-files
	// Cases []*Case
)

func init() {
	Verbose = true
	// conf = goini.SetConfig(CfgFileName)
}

// SetVerbose sets the state of Verbose
func SetVerbose(verbose bool) {
	Verbose = verbose
}

// GetConfig gets the configuration from cfgFileName
func GetConfig(casesPath string) (cases []*Case) {
	util.DebugPrintCaller()

	util.Glog.Infof("➥  Reading configuration from  [%s] ...", util.LogColorString("info", casesPath))

	if !util.IsFileExist(casesPath) {
		// panic(fmt.Errorf("☠  the cases-path must exist! (%q)", CasesPath))
		util.Glog.Fatalf("☠  the cases-path must exist! (%q)", casesPath)
	}

	b, err := util.ReadFile(casesPath)
	if err != nil {
		util.Glog.Fatalf("☠  reading case failed: %v", err.Error())
	}
	err = yaml.Unmarshal(b, &cases)
	if err != nil {
		util.Glog.Fatalf("☠  unmarshal case failed: %v", err.Error())
	}

	return cases
	// for _, c := range Cases {
	// 	util.Glog.Info(c)
	// }
}

// InputFile contains the paths of input-data and acceleration
type InputFile struct {
	DataPath string
	AccPath  string
}

// OutputFile contains the paths of output-data
type OutputFile struct {
	DataPath  string
	PunchPath string
}

// Case cotains the input- and output-file information
type Case struct {
	Head   string
	Input  *InputFile
	Output *OutputFile
}

func (c *Case) String() string {
	tab := util.ArgsTable("Condiguration of case",
		"Input data file", "Input.DataPath", c.Input.DataPath,
		"Acceleration data", "Input.AccPath", c.Input.AccPath,
		"Output data file", "Output.DataPath", c.Output.DataPath,
		"Punch file", "Output.PunchPath", c.Output.PunchPath,
	)
	return tab
}

// Table is the table of `Case`
func (c *Case) Table(title string) string {
	if len(title) == 0 {
		title = "Condiguration of "
	}
	title += c.Head
	tab := util.ArgsTable(title,
		"Input data file", "Input.DataPath", c.Input.DataPath,
		"Acceleration data", "Input.AccPath", c.Input.AccPath,
		"Output data file", "Output.DataPath", c.Output.DataPath,
		"Punch file", "Output.PunchPath", c.Output.PunchPath,
	)
	return tab
}
