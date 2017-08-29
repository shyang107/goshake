package shake

// Head is presnet the head of case of SHAKE
type Head struct {
	Title string
	// ID     string
	// IDNT string
	// IDMP   string
	// IBlank string
}

const (
	// PIW = 2Pi
	PIW = 6.28318530717958623199592693708837032318115234375
)

var (
	// FORMAT stores standard output message format
	FORMAT = map[int]string{
		23: "# Option NO. %02d  is started.\n",
		24: "# Option NO. %02d  has been concluded.\n",
		100: `***********************************************************
   * SHAKE  --   A COMPUTER PROGRAM FOR EARTHQUAKE RESPONSE  *
   *             ANALYSIS OF HORIZONTALLY LAYERED SITES      *
   *             by: Per B. Schnabel & John Lysmer -- 1970   *
   * ------------------------------------------------------- *
   * shake85     IBM-PC version of SHAKE                     *
   *             by: S.S. (Willie) Lai, January 1985         *
   * ------------------------------------------------------- *
   * shake88   : New modulus reduction curves for clays added*
   *             using results from Sun et al (1988)         *
   *             by: J. I. Sun & Ramin Golesorkhi            *
   *             February 26, 1988                           *
   * ------------------------------------------------------- *
   * SHAKE90/91: Adjust last iteration; Input now is either  *
   *             Gmax or max Vs; up to 13 material types can *
   *             be specified by user; up to 50 Layers can   *
   *             be specified; object motion can be read in  *
   *             from a separate file and can have user      *
   *             specified format; Different periods for     *
   *             response spectral calculations; options     *
   *             are renumbered; and general cleanup         *
   *             by: J. I. Sun, I. M. Idriss & P. Dirrim     *
   *             June 1990 - February 1991                   *
   * ------------------------------------------------------- *
   * SHAKE91   : General cleanup and finalization of input/  *
   *             output format ... etc                       *
   *             by: I. M. Idriss                            *
   *             December 1991                               *
   ***********************************************************`,
	}
)

// var (
// 	inFilename, outFilename, punchname string
// 	inFile                             io.Reader
// 	outFile                            io.Writer
// 	punch                              io.Writer
// 	head                               Head
// 	absis, abspr, abscl                string
// 	ophead, finpeq                     string
// 	x, ax                              complex128
// 	G                                  complex128
// 	nu                                 complex128
// 	Plus                               complex128
// 	Minus                              complex128
// 	LL                                 [3]int64
// 	LT                                 [3]int64
// 	LNSW                               [3]int64
// 	LLL                                [2]int64
// 	LLGS                               [2]int64
// 	LLPCH                              [2]int64
// 	LLPL                               [2]int64
// 	LNV                                [2]int64
// 	SK                                 [2]float64
// 	// X     [300]float64
// 	// AX    [3][270]float64
// 	// AA    [2][550]float64
// 	// S     [70]float64
// 	// INV   [70]int64
// 	LL5   [15]int64
// 	LT5   [15]int64
// 	LP5   [15]int64
// 	LP    [3]int64
// 	IDAMP [27][11]string
// 	MMM   [3]int64
// 	ID    [27][11]string
// )

/*
originallly coded by Per Schnabel in 1970-71
modified by Sun, Dirrim & Idriss in 1990-91 to
increase number of layers to 50;
renumber the Options & other cleanup
*/
// 		100: "  *****************************************************"
// +"  * SHAKE  --   A COMPUTER PROGRAM FOR EARTHQUAKE RESPONSE  *"
// +"  *             ANALYSIS OF HORIZONTALLY LAYERED SITES      *"
// +"  *             by: Per B. Schnabel & John Lysmer -- 1970   *"
// +"  * ------------------------------------------------------- *"
// +"  * shake85     IBM-PC version of SHAKE                     *"
// +"  *             by: S.S. (Willie) Lai, January 1985         *"
// +"  * ------------------------------------------------------- *"
// +"  * shake88   : New modulus reduction curves for clays added*"
// +"  *             using results from Sun et al (1988)         *"
// +"  *             by: J. I. Sun & Ramin Golesorkhi            *"
// +"  *             February 26, 1988                           *"
// +"  * ------------------------------------------------------- *"
// +"  * SHAKE90/91: Adjust last iteration; Input now is either  *"
// +"  *             Gmax or max Vs; up to 13 material types can *"
// +"  *             be specified by user; up to 50 Layers can   *"
// +"  *             be specified; object motion can be read in  *"
// +"  *             from a separate file and can have user      *"
// +"  *             specified format; Different periods for     *"
// +"  *             response spectral calculations; options     *"
// +"  *             are renumbered; and general cleanup         *"
// +"  *             by: J. I. Sun, I. M. Idriss & P. Dirrim     *"
// +"  *             June 1990 - February 1991                   *"
// +"  * ------------------------------------------------------- *"
// +"  * SHAKE91   : General cleanup and finalization of input/  *"
// +"  *             output format ... etc                       *"
// +"  *             by: I. M. Idriss                            *"
// +"  *             December 1991                               *"
// +"  ***********************************************************",
