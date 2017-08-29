package cmds

import (
	"fmt"
	"os"

	vp "github.com/shyang107/go-twinvoices"
	"github.com/shyang107/go-twinvoices/util"
	"github.com/urfave/cli"
)

// ExecuteCommand return cli.Command
func ExecuteCommand() cli.Command {
	return cli.Command{
		Name:    "execute",
		Aliases: []string{"e"},
		Usage: `Import the original invoice data file from the E-Invoice platform of Ministry of Finance,
backup to invoices.db (sqlite3), and output specified file-type.`,
		Action: executeAction,
		Flags: []cli.Flag{
			cli.StringFlag{
				Name:  "case,c",
				Usage: "specify the case file",
			},
		},
	}
}

func executeAction(c *cli.Context) error {
	checkVerbose(c)

	util.DebugPrintCaller()

	if err := vp.Cfg.ReadConfigs(); err != nil { // reading config
		return err
	}

	fln := os.ExpandEnv(c.String("case")) // check command line options: "case"
	if len(fln) > 0 {
		if !util.IsFileExist(fln) {
			// ut.Perr("The specified case-configuration-file %q does not exist!\n", fln)
			util.Glog.Fatalf("The specified case-configuration-file %q does not exist!", fln)
		}
		vp.Cfg.CaseFilename = fln
	}

	if err := execute(); err != nil { // run procedures of program
		// ut.Pwarn(err.Error())
		util.DebugPrintCaller()
		util.Glog.Fatal(err.Error())
	}
	return nil
}

//
func execute() (err error) {
	util.DebugPrintCaller()

	util.Glog.Infof("\n%v", vp.Cfg) // print out config

	vp.Cases, err = vp.Cfg.ReadCaseConfigs() // reading settings of cases
	if err != nil {
		util.Glog.Error(err.Error())
		return err
	}

	vp.Connectdb() // connect to database

	// if err = single(); err != nil {
	// 	return err
	// }

	if err = multi(); err != nil {
		return err
	}

	return nil
}

func multi() error {
	type item struct {
		msg    string
		errors error
	}

	ch := make(chan item, len(vp.Cases))
	for idx, c := range vp.Cases { // run every case

		go func(c *vp.Case, idx int) {
			var it item
			util.Glog.Infof("\n%v", c)

			if it.errors = c.UpdateFileBunker(); it.errors != nil {
				ch <- it
				// return err
			}

			var pvs *vp.InvoiceCollection
			pvs, it.errors = (&c.Input).ReadInvoices()
			if it.errors != nil {
				ch <- it
				// return err
			}

			for _, out := range c.Outputs {
				if out.IsOutput {
					it.errors = out.WriteInvoices(pvs)
					if it.errors != nil {
						// return err
						ch <- it
					}
				}
			}
			it.msg = fmt.Sprintf("* ch: ending case #%d: %q", idx+1, c.Input.Filename)
			ch <- it
		}(c, idx)
	}

	// Wait for goroutines to complete.
	for range vp.Cases {
		it := <-ch

		if it.errors != nil {
			return it.errors
		}

		if len(it.msg) > 0 {
			// fmt.Println(it.msg)
			util.Glog.Print("\n")
			// util.Glog.Infof("%s\n", it.msg)
			util.Glog.Child("<- Goroutine").Infof("%s\n", it.msg)
		}
	}
	return nil
}

func single() error {
	for _, c := range vp.Cases { // run every case
		util.Glog.Infof("\n%v", c)

		if err := c.UpdateFileBunker(); err != nil {
			util.Glog.Error(err.Error())
			return err
		}

		pvs, err := (&c.Input).ReadInvoices()
		if err != nil {
			util.Glog.Errorf("%v\n", err)
			return err
		}

		for _, out := range c.Outputs {
			if out.IsOutput {
				err = out.WriteInvoices(pvs)
				if err != nil {
					util.Glog.Error(err.Error())
					return err
				}
			}
		}
	}
	return nil
}

// initConfig reads in config file and ENV variables if set.
func initConfig(c *cli.Context) error {
	// ut.Pdebug(">> root.initConfig called\n")
	util.DebugPrintCaller()
	if err := vp.Cfg.ReadConfigs(); err != nil {
		return err
	}
	return nil
}
