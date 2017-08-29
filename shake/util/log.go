package util

import (
	"fmt"
	"os"
	"strings"
	"time"

	"github.com/kataras/golog"
	"github.com/shyang107/go-twinvoices/pencil"
	"github.com/shyang107/go-twinvoices/pencil/rgb16b"
	"golang.org/x/image/colornames"
)

var (
	// Logger is a basic logger of golog
	// Logger = golog.New()

	// Glog is a logger of golog
	Glog = golog.New() // Logger.Child("util")
)

// GoroutineLevel is go-routine log level to log something about go routines
// const GoroutineLevel golog.Level = 6

// InitLogger initialize logger's environment
func InitLogger() {
	golog.NewLine("\n")
	golog.InfoText("[INFO]", LogColorString("info", "[INFO]"))
	golog.WarnText("[WARN]", LogColorString("warn", "[WARN]"))
	golog.ErrorText("[ERRO]", LogColorString("error", "[ERRO]"))
	golog.DebugText("[DBUG]", LogColorString("debug", "[DBUG]"))
	// Time Format defaults to: "2006/01/02 15:04"
	// you can change it to something else or disable it with:
	// Glog.SetTimeFormat("2006/01/02 15:04:05")
	Glog.SetTimeFormat("")
	Glog.SetLevel("info")
	pencil.NoColor = !Glog.Printer.IsTerminal

	// GoroutineLevel = golog.InfoLevel
	// Register our level, just three fields.

	// golog.Levels[GoroutineLevel] = &golog.LevelMetadata{
	// 	Name:             "goro",
	// 	AlternativeNames: []string{"goroutine"},
	// 	RawText:          "[GORO]",
	// 	// ColorfulText (Green Color[GORO])
	// 	ColorfulText: LogColorString("goro", "[GORO]"),
	// }
}

//---------------------------------------------------------

var (
	errorColorStr = rgb16b.New(
		colornames.Violet, pencil.Foreground,
		// rgb16b.RGBAttribute{GroundFlag: pencil.Foreground, Color: colornames.White},
		// rgb16b.RGBAttribute{[GroundFlag]: pencil.Foreground, Color: colornames.Violet},
		// rgb16b.RGBAttribute{GroundFlag: pencil.Background, Color: colornames.Goldenrod},
	).SprintFunc()
	warnColorStr = rgb16b.New(
		colornames.Goldenrod, pencil.Foreground,
		// rgb16b.RGBAttribute{GroundFlag: pencil.Foreground, Color: colornames.White},
		// rgb16b.RGBAttribute{GroundFlag: pencil.Foreground, Color: colornames.Goldenrod},
		// rgb16b.RGBAttribute{GroundFlag: pencil.Background, Color: colornames.Goldenrod},
	).SprintFunc()
	infoColorStr = rgb16b.New(
		colornames.Cyan, pencil.Foreground,
		// rgb16b.RGBAttribute{GroundFlag: pencil.Foreground, Color: colornames.White},
		// rgb16b.RGBAttribute{GroundFlag: pencil.Foreground, Color: colornames.Cyan},
		// rgb16b.RGBAttribute{GroundFlag: pencil.Background, Color: colornames.Goldenrod},
	).SprintFunc()
	debugColorStr = rgb16b.New(
		colornames.Orangered, pencil.Foreground,
		// rgb16b.RGBAttribute{GroundFlag: pencil.Foreground, Color: colornames.White},
		// rgb16b.RGBAttribute{GroundFlag: pencil.Foreground, Color: colornames.Orangered},
		// rgb16b.RGBAttribute{GroundFlag: pencil.Background, Color: colornames.Goldenrod},
	).SprintFunc()
	debugColorStr2 = rgb16b.New(colornames.Darksalmon, pencil.Foreground).SprintFunc()
	debugColorStr3 = rgb16b.New(colornames.Darkorange, pencil.Foreground).SprintFunc()
	goroColorStr   = rgb16b.New(colornames.Greenyellow, pencil.Foreground).SprintFunc()
	// LogColorStringFuncs maps to a serious of [*]ColorString functions with key as golog.Level
	LogColorStringFuncs = map[string]func(a ...interface{}) string{
		// golog.DisableLevel: func(s string) string { return s },
		// golog.ErrorLevel:   func(s string) string { return HiRedString(s) },
		// golog.WarnLevel:    func(s string) string { return HiMagentaString(s) },
		// golog.InfoLevel:    func(s string) string { return HiCyanString(s) },
		// golog.DebugLevel:   func(s string) string { return OrangeString(s) },
		"disable": fmt.Sprint,
		"error":   errorColorStr,
		"warn":    warnColorStr,
		"info":    infoColorStr,
		"debug":   debugColorStr,
		"debug2":  debugColorStr2,
		"debug3":  debugColorStr2,
		"goro":    goroColorStr,
	}

	// LogfMapFuncs are the maps of Glog functions
	LogfMapFuncs = map[string]func(string, ...interface{}){
		"error": Glog.Errorf,
		"warn":  Glog.Warnf,
		"info":  Glog.Infof,
		"debug": Glog.Debugf,
		"fatal": Glog.Fatalf,
	}

	// LogMapFuncs are the maps of Glog functions
	LogMapFuncs = map[string]func(...interface{}){
		"error": Glog.Error,
		"warn":  Glog.Warn,
		"info":  Glog.Info,
		"debug": Glog.Debug,
		"fatal": Glog.Fatal,
		"print": Glog.Print,
	}
)

// fromLevelName return Level code w.r.t levelname
func fromLevelName(levelName string) golog.Level {
	switch strings.ToLower(levelName) {
	case "error":
		return golog.ErrorLevel
	case "warning":
		fallthrough
	case "warn":
		return golog.WarnLevel
	case "info":
		return golog.InfoLevel
	case "debug":
		return golog.DebugLevel
	// case "goroutine":
	// 	fallthrough
	// case "goro":
	// 	return GoroutineLevel
	default:
		return golog.DisableLevel
	}
}

// LogColorString gives a colorful string w.r.t. levelname
func LogColorString(levelname, s string) string {
	if !Glog.Printer.IsTerminal {
		return s
	}
	return LogColorStringFuncs[levelname](s)
	// return ColorStringFuncs[Level(glog.Level)](s)
}

//---------------------------------------------------------

// EnableLoggerOutToFile enables the "Glog" out to os.Stderr and os.File
func EnableLoggerOutToFile(levelname string) {
	// file, err := os.OpenFile(util.TodayFilename(), os.O_CREATE|os.O_WRONLY, 0666)
	Glog.SetLevel(levelname)
	file, err := NewLogFile()
	// defer file.Close()
	if err == nil {
		Glog.AddOutput(file)
	} else {
		Glog.Errorf("Failed to log to file, using default stderr")
	}
	pencil.NoColor = true
}

// TodayFilename get a filename based on the date, file logs works that way the most times
// but these are just a sugar.
func TodayFilename() string {
	// today := time.Now().Format("Jan 02 2006")
	today := time.Now().Format("2006-01-02")
	return today + ".log"
}

// NewLogFile get a new logging file handler
func NewLogFile() (*os.File, error) {
	filename := TodayFilename()
	// open an output file, this will append to the today's file if server restarted.
	// f, err := os.OpenFile(filename, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	return os.OpenFile(filename, os.O_CREATE|os.O_WRONLY, 0666)
}
