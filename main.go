package main

import (
	"time"

	"github.com/shyang107/goshake/shake/cmds"
	"github.com/shyang107/goshake/shake/util"
)

func main() {
	start := time.Now()

	cmds.Init()

	util.Glog.Println()
	util.Glog.Child("main()").Infof("run-time elapsed: %s\n", time.Since(start).String())
}
