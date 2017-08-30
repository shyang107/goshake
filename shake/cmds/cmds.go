package cmds

import (
	"fmt"
	"os"
	"strings"

	sk "github.com/shyang107/goshake/shake"

	"github.com/shyang107/goshake/shake/cfg"
	"github.com/shyang107/goshake/shake/util"
	"github.com/urfave/cli"
)

// Init uses to initial all program
func Init() {
	util.InitLogger()
	util.DebugPrintCaller()

	sk.Options = new(sk.OptionAll)

	app := cli.NewApp()

	app.Name = "goshake"
	app.Version = cfg.Version
	app.Authors = []cli.Author{
		{Name: "YANG, S. H.", Email: "shyang107@gmail.com"},
	}

	app.Usage = "Go edition of SHAKE91"
	// app.Description = `Transfer the Fortan ed. to Go ed. of SHAKE91`
	app.Description = sk.FORMAT[100]

	// app.Commands = []cli.Command{
	// 	ExecuteCommand(),
	// }

	app.Action = appAction

	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:  "cases,C",
			Usage: `assign the specified case path including filename`,
		},
		cli.StringFlag{
			Name: "verbose,V",
			Usage: `verbose output of logging information (default log-level is "info") 
		logging-levle are "disable", "info", "warn", "error", and "debug"
		"disable" will disable printer
		"error" will print only errors
		"warn" will print errors and warnings
		"info" will print errors, warnings and infos
		"debug" will print on any level, errors, warnings, infos and debug messages`,
		},
	}

	if err := app.Run(os.Args); err != nil {
		fmt.Println(err)
		os.Exit(-1)
	}
}

func checkVerbose(c *cli.Context) {
	util.DebugPrintCaller()

	level := strings.ToLower(c.GlobalString("verbose")) // check command line options: "verbose"
	// util.Glog.Debugf("log level: %s\n", level)
	setglog(level)
}

func setglog(level string) {
	util.DebugPrintCaller()

	if len(level) > 0 {
		cfg.Verbose, util.Verbose = true, true
		// util.ColorsOn = sk.Cfg.ColorsOn
	}

	switch level {
	case "disable":
		cfg.SetVerbose(false)
	case "fatal":
	case "error":
	case "warn":
	case "debug":
	// case "goro":
	default:
		level = "info"
	}
	util.Glog.Infof("log level: %s", level)
	util.Glog.SetLevel(level)
}

func checkCase(c *cli.Context) (casesPath string) {
	util.DebugPrintCaller()

	casesPath = c.GlobalString("cases")
	if len(casesPath) == 0 {
		casesPath = cfg.DefaultCasesPath
	}
	casesPath = os.ExpandEnv(casesPath)
	return casesPath
}

func appAction(c *cli.Context) error {
	checkVerbose(c)

	util.DebugPrintCaller()

	casesPath := checkCase(c)

	cases := cfg.GetConfig(casesPath)

	for i, c := range cases {
		title := fmt.Sprintf("Case #%d - ", i+1)
		util.Glog.Infof("\n%s", c.Table(title))
		if err := executeCase(c); err != nil { // run procedures of program
			// ut.Pwarn(err.Error())
			util.DebugPrintCaller()
			util.Glog.Fatal(err.Error())
		}
	}
	return nil
}

//
func executeCase(c *cfg.Case) (err error) {
	util.DebugPrintCaller()

	sk.Options.ReadOptions(c)

	return nil
}
