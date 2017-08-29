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
	util.DebugPrintCaller()
	util.InitLogger()

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

	app.Commands = []cli.Command{
		ExecuteCommand(),
	}

	app.Action = appAction

	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:  "config,C",
			Usage: `assign the specified config-filename`,
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
	level := strings.ToLower(c.GlobalString("verbose")) // check command line options: "verbose"
	// util.Glog.Debugf("log level: %s\n", level)
	setglog(level)
}

func setglog(level string) {
	if len(level) > 0 {
		cfg.Verbose, util.Verbose = true, true
		// util.ColorsOn = sk.Cfg.ColorsOn
	}

	cfg.SetVerbose(true)

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
	util.Glog.SetLevel(level)
}

func appAction(c *cli.Context) error {
	checkVerbose(c)

	util.DebugPrintCaller()

	cfg.GetConfig()

	if err := appExecute(); err != nil { // run procedures of program
		// ut.Pwarn(err.Error())
		util.DebugPrintCaller()
		util.Glog.Fatal(err.Error())
	}
	return nil
}

//
func appExecute() (err error) {
	util.DebugPrintCaller()

	sk.Options.ReadOptions()

	return nil
}
