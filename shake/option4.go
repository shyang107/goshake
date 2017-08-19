package shake

import (
	"strings"

	"github.com/cpmech/gosl/io"
)

// AssignObjectMotion is Option#4 for assignment of object motion
// to the top of a specified sublayer
type AssignObjectMotion struct { // Option#4
	// Identification information for this option
	Identification string
	// LayerID = number of sublayer at the top which the object motion is assigned
	LayerID int
	// OMPOS : the location code where the object motion is applied
	//	OMPOS = 0 : if the the object motion is to be assinged as outcrop motion
	//	OMPOS = 1 : if the the object motion is applied within the soil profile
	//				at the top of the assigned sublayer
	OMPOS int
}

func (a AssignObjectMotion) String() string {
	tab := io.ArgsTable(
		a.Identification,
		"number of sublayer at the top which the object motion is assigned", "LayerID", a.LayerID,
		"the location code where the object motion is applied", "OMPOS", a.OMPOS)
	return tab
}

func (a *AssignObjectMotion) read(lines []string, no *int) {
	a.LayerID = io.Atoi(strings.Trim(lines[*no][0:5], " "))
	a.OMPOS = io.Atoi(strings.Trim(lines[*no][5:10], " "))
	*no++
	io.Pf("%s", *a)
}
