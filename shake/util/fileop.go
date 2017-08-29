package util

import (
	"bufio"
	"bytes"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
)

// file ---------------------------------------------------------

// FnKey returns the file name key (without path and extension, if any)
func FnKey(fn string) string {
	base := filepath.Base(fn)
	return base[:len(base)-len(filepath.Ext(base))]
}

// FnExt returns the extension of a file name.
// The extension is the suffix beginning at the final dot in the final element of path; it is empty if there is no dot.
func FnExt(fn string) string {
	return filepath.Ext(fn)
}

// PathKey returs the full path except the extension
func PathKey(fn string) string {
	return fn[:len(fn)-len(filepath.Ext(fn))]
}

// RemoveAll deletes all files matching filename specified by key (be careful)
func RemoveAll(key string) {
	fns, _ := filepath.Glob(os.ExpandEnv(key))
	for _, fn := range fns {
		os.RemoveAll(fn)
	}
}

// IsFileExist checks whether a file exist
func IsFileExist(filename string) bool {
	path := os.ExpandEnv(filename)
	isExist := true
	if _, err := os.Stat(path); os.IsNotExist(err) {
		isExist = false
	}
	return isExist
}

// OpenFileR opens a file for reading data
func OpenFileR(fn string) (fil *os.File, err error) {
	fil, err = os.Open(os.ExpandEnv(fn))
	return
}

// ReadFile reads bytes from a file
func ReadFile(fn string) (b []byte, err error) {
	return ioutil.ReadFile(os.ExpandEnv(fn))
}

// ReadLinesCallback is used in ReadLines to process line by line during reading of a file
type ReadLinesCallback func(idx int, line string) (stop bool)

// ReadLinesFile reads lines from a file and calls ReadLinesCallback to process each line being read
func ReadLinesFile(fil *os.File, cb ReadLinesCallback) (oserr error) {
	r := bufio.NewReader(fil)
	idx := 0
	for {
		lin, prefix, errl := r.ReadLine()
		if prefix {
			return Err("cannot read long line. file = <%s>", fil.Name())
		}
		if errl == io.EOF {
			break
		}
		if errl != nil {
			return Err("cannot read line. file = <%s>", fil.Name())
		}
		stop := cb(idx, string(lin))
		if stop {
			break
		}
		idx++
	}
	return
}

// ReadLines reads lines from a []byte and calls ReadLinesCallback to process each line being read
func ReadLines(b []byte, cb ReadLinesCallback) {
	r := bytes.NewReader(b)
	scanner := bufio.NewScanner(r)
	scanner.Split(bufio.ScanLines)

	idx := 0
	for scanner.Scan() {
		line := scanner.Text()
		stop := cb(idx, line)
		if stop {
			break
		}
		idx++
	}
	return
}

// AppendToFile appends data to an existent (or new) file
func AppendToFile(fn string, buffer ...*bytes.Buffer) {
	fil, err := os.OpenFile(os.ExpandEnv(fn), os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0666)
	if err != nil {
		Panic("cannot create file <%s>", fn)
	}
	defer fil.Close()
	for k := range buffer {
		if buffer[k] != nil {
			fil.Write(buffer[k].Bytes())
		}
	}
}

// WriteFile writes data to a new file with given bytes.Buffer(s)
func WriteFile(fn string, buffer ...*bytes.Buffer) {
	fil, err := os.Create(os.ExpandEnv(fn))
	if err != nil {
		Panic("cannot create file <%s>", fn)
	}
	defer fil.Close()
	for k := range buffer {
		if buffer[k] != nil {
			fil.Write(buffer[k].Bytes())
		}
	}
}

// WriteBytesToFile writes slice of bytes to a new file
func WriteBytesToFile(fn string, b []byte) {
	fil, err := os.Create(os.ExpandEnv(fn))
	if err != nil {
		Panic("cannot create file <%s>", fn)
	}
	defer fil.Close()
	if _, err = fil.Write(b); err != nil {
		Panic("%v", err)
	}
}
