# SHAKE16G

This is a modernized version of SHAKE91 to run with modern `golang` compilers. Here's what we changed:




If you see test failures, you should file an issue with information about your platform.

## License

The original source was published into the public domain. In keeping with that practice, we have kept an open source public domain license. You are free to use, modify, copy, distribute this source or any resulting programs in any way you wish with no restrictions.

## AS-IS Software

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Original Readme

Program: SHAKE-91

Title: Equivalent Linear Seismic Response Analysis of Horizontally
Layered Soil Deposits

Developer: P. B. Schnabel, J. Lysmer, and H. B. Seed, Department of Civil
Engineering, University of California, Berkeley 1972.

Modified: I. M. Idriss and J. I. Sun, Department of Civil & Environmental
Engineering, University of California, Davis 1992.

Category: Geotechnical

Platform: PC DOS 6, MS PowerStation Fortran, v. 1.0

Reference: Idriss, I.M., and J.I. Sun, "User's Manual for SHAKE91,"
Department of Civil & Environmental Engineering, University of California,
Davis, California, November 1992.

Schnabel, P.B., J. Lysmer, and H.B. Seed, "SHAKE - A Computer Program for
Earthquake Response Analysis of Horizontally Layered Sites," Earthquake
Engineering Research Center, Report No. UCB/EERC-72/12. University of
California, Berkeley, December 1972.

Summary: The SHAKE program has been by far the most widely used program
for computing the seismic response of horizontally layered soil deposits.
The program computes the response of a semi-infinite horizontally layered
soil deposit overlying a uniform half-space subjected to vertically
propagating shear waves. The analysis is done in the frequency domain,
and, therefore, for any set of properties, it is a linear analysis. An
iterative procedure is used to account for the nonlinear behavior of the
soils. The object motion (i.e., the motion that is considered to be known)
can be specified at the top of any sublayer within the soil profile or at
the corresponding outcrop.

The main modifications incorporated in SHAKE91 include the following:
The number of sublayers was increased from 20 to 50; this should permit a
more accurate representation of deeper and/or softer soil deposits. All
built-in modulus reduction and damping relationships were removed. These
relationships are now specified by the user. The maximum shear velocity
or the maximum modulus are now specified for each sublayer; again these
are part of the input and therefore the program no longer calculates
modulus values as a function of either confining pressure or shear
strength. Object motion is now read from a separate file. Other clean-up
includes: renumbering of options, elimination of infrequently used options,
user specified periods for calculating spectral ordinates.

[SHAKE91 Manual](https://github.com/ocrickard/SHAKE16/raw/master/SHAKE91%20User%20Manual.pdf)
