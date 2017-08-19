C $NOFLOATCALLS
C $NODEBUG
C ...................................................................
      program SHAKE91
      character*256 FIN,FOUT,PUNCH
      common X(25620)
      common /TIME/ T(9)
      common /WGK/  WW, GT, SKO
C ...................................................................
      write(*,100)
  100 format(2X,'*****************************************************'/
     + 2X,'* SHAKE  --   A COMPUTER PROGRAM FOR EARTHQUAKE RESPONSE  *'/
     + 2X,'*             ANALYSIS OF HORIZONTALLY LAYERED SITES      *'/
     + 2X,'*             by: Per B. Schnabel & John Lysmer -- 1970   *'/
     + 2X,'* ------------------------------------------------------- *'/
     + 2X,'* shake85     IBM-PC version of SHAKE                     *'/
     + 2X,'*             by: S.S. (Willie) Lai, January 1985         *'/
     + 2X,'* ------------------------------------------------------- *'/
     + 2X,'* shake88   : New modulus reduction curves for clays added*'/
     + 2X,'*             using results from Sun et al (1988)         *'/
     + 2X,'*             by: J. I. Sun & Ramin Golesorkhi            *'/
     + 2X,'*             February 26, 1988                           *'/
     + 2X,'* ------------------------------------------------------- *'/
     + 2X,'* SHAKE90/91: Adjust last iteration; Input now is either  *'/
     + 2X,'*             Gmax or max Vs; up to 13 material types can *'/
     + 2X,'*             be specified by user; up to 50 Layers can   *'/
     + 2X,'*             be specified; object motion can be read in  *'/
     + 2X,'*             from a separate file and can have user      *'/
     + 2X,'*             specified format; Different periods for     *'/
     + 2X,'*             response spectral calculations; options     *'/
     + 2X,'*             are renumbered; and general cleanup         *'/
     + 2X,'*             by: J. I. Sun, I. M. Idriss & P. Dirrim     *'/
     + 2X,'*             June 1990 - February 1991                   *'/
     + 2X,'* ------------------------------------------------------- *'/
     + 2X,'* SHAKE91   : General cleanup and finalization of input/  *'/
     + 2X,'*             output format ... etc                       *'/
     + 2X,'*             by: I. M. Idriss                            *'/
     + 2X,'*             December 1991                               *'/
     + 2X,'***********************************************************')

C     We allow users to specify input/output file names via command-line
C     arguments. First argument is defined to be the input file, second and
C     third are the output files.

      call get_command_argument(1, FIN)

      if (len_trim(FIN) .gt. 0) then
        call get_command_argument(2, FOUT)

        call get_command_argument(3, PUNCH)
      else
C       No command-line arguments were provided, use traditional format
        write(*,200)
200     format(4X,'Name of Input File =')
        read(*,10) FIN

        write(*,300)
300     format(4X,'Name of Output File #1 input, peak values .. etc =')
        read(*,10) FOUT

        write(*,400)
400     format(4X,'Name of Output File #2 time histories .. etc =')
        read(*,10) PUNCH

10      format(A32)
      end if

      open(5,FILE=FIN,STATUS='OLD')
      open(6,FILE=FOUT,STATUS='NEW')
      open(7,FILE=PUNCH,STATUS='NEW')

      write(6,100)
      WW = .0624
      GT = 32.2
      MAMAX=4096
C ......................................................................
C
      NAX = MAMAX + 5
      NAA = NAX + 3*(MAMAX + 4)
      NS = NAA + 2*MAMAX
      NINV = NS + NAX/8 + 1
      NTOT = NINV + NAX/8 + 1
      if (SKO .LT. .000001)   SKO = .45
      write(6,2000) MAMAX, NTOT
C
      call SHAKIT(X(1), X(NAX), X(NAA), X(NS), X(NINV))
C
      stop
C ****************************************************
 1000 format(I5, F10.0)
 2000 format( 45H  MAX. NUMBER OF TERMS IN FOURIER TRANSFORM =   I10/
     1          45H  NECESSARY LENGTH OF BLANK COMMON X        =   I10)
      end
C********************************************************************
