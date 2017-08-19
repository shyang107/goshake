package main

import (
	"fmt"
	sk "shake17g/shake"
	"shake17g/shake/cfg"
	"time"
)

func init() {
	sk.Options = new(sk.OptionAll)
}

func main() {
	startTime := time.Now()
	fmt.Printf("Start time: %v\n\n", startTime)
	// read basic configuration
	readBasicConfig()
	// read options
	sk.Options.ReadOptions()
	endTime := time.Now()
	fmt.Printf("\nEnd time:  %v\n", endTime)
	fmt.Println("Duration of process = ", endTime.Sub(startTime))
}

func readBasicConfig() {
	cfg.SetVerbose(true) // set verbose logging
	cfg.GetConfig()      // get the basic configuration

}

// func test1() {
// 	fmt.Println(sk.FORMAT[100])
// 	fmt.Println("SHAKE17g")
// 	head := sk.Head{"Title"}
// 	fmt.Println(head)
// 	x := []float64{1., 2., 9., -5, -11., 2., -7.}
// 	fmt.Println("x = ", x)
// 	xmax, nxmax := sk.XMX(x)
// 	fmt.Printf("max value: x[%1d] = %f \n", nxmax, xmax)
// 	fmt.Println(x)
// 	//
// 	k := 0
// 	for i := 0; i < 40; i++ {
// 		if k > 9 {
// 			k = 0
// 		}
// 		fmt.Print(k)
// 		k++
// 	}
// 	fmt.Print("\n")
// 	str := "This a test(left)"
// 	fmt.Println(sk.Hstring(str, 40, "*", 1))
// 	str = "This a test(center)"
// 	fmt.Println(sk.Hstring(str, 40, "*", 0))
// 	str = "This a test(right)"
// 	fmt.Println(sk.Hstring(str, 40, "*", -1))
// }
