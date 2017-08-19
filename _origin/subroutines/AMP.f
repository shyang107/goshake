C************************************
      subroutine AMP( N1,IN,INT,LL,LT,KPL,IDAMP,NA,DF)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C   THIS ROUTINE COMPUTES THE AMPLIFICATION SPECTRUM BETWEEN ANY TWO
C   LAYERS
C
C        N1      = NUMBER OF SOIL LAYERS EXCLUDING ROCK
C        IN      = NUMBER OF SUBLAYER FROM WHICH AMPLIFICATION IS COMP.
C        INT     = SUBLAYER TYPE
C                       0 - OUTCROPPING LAYER
C                       1 - LAYER WITHIN PROFILE
C        LL      = NUMBER OF SUBLAYER TO WHICH AMPLIFICATION IS COMP.
C        LT      = SUBLAYER TYPE
C                       0 - OUTCROPPING LAYER
C                       1 - LAYER WITHIN PROFILE
C        DF      = FREQUENCY STEPS IN AMP. FUNCTION
C        NA      = CURVE NUMBER IN PLOTTING
C        IDAMP   = IDENTIFICATION
C
C   CODED PER B SCHNABEL  FEB. 1971
C   modified to increase number of sublayers to 50
C   February 1991
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      complex G, V, PLUS, MINUS
      complex E, F, EE, FF, A, EX, AIN, IPI2,AA
      character*60 ABSIS
      character*6 ID,IDNT,IDAMP
C
      dimension IDAMP (27,11),T(200)
      common /JOE4/ ST(27,200)
      common /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      common /CSOIL/ G(51), V(51), PLUS(51), MINUS(51)
      common /CCG/ ID(27,11)
C
      ABSIS = ' CYCLES/SEC. '
C
      IPI2 = CMPLX(0., 6.283185307)
      FREQ = 0.
      ST(NA,1) = 1.
      do 19 I = 2,200
      E = 1.
      FF = 1.
      FREQ = FREQ + DF
      A = FREQ*IPI2
      do 191 K = 1,N1
      if (K.NE.IN) go to 192
      AIN = E + FF
      if (INT.EQ.0) AIN = 2.*E
  192 if (K.NE.LL) go to 11
      AA    = E + FF
      if (LT.EQ.0) AA = 2.*E
   11 EX = CEXP(H(K)*A/V(K))
      EE = E*EX
      F = FF/EX
      E = EE*PLUS(K) + MINUS(K)*F
      FF = PLUS(K)*F + MINUS(K)*EE
  191 continue
      if (IN.NE.N1+1) go to 193
      AIN = E + FF
      if (INT.EQ.0) AIN = 2.*E
  193 if (LL   .NE.N1+1) go to 21
      AA    = E + FF
      if (LT.EQ.0)  AA = 2.*E
   21 ST(NA,I) = CABS(AA/AIN)
   19 continue
      do 23 I = 1,200
   23 T(I) = DF*FLOAT(I-1)
      AMAX = 0.
      write(6,2)
      do 22 I = 1,200
      if (KPL .GE. 2) write(6,1) T(I), ST(NA,I)
      if (ST(NA,I) .LT.  AMAX) go to 22
      TMAX = T(I)
      AMAX = ST(NA,I)
   22 continue
      if (NA.LT.9) NA=NA+1
      PERIOD = 1./TMAX
      if (TMAX.LT. .0001)  write(6,1001) AMAX, TMAX
      if (TMAX.GT. .0001)  write(6,1001) AMAX, TMAX,PERIOD
      if (KPL.EQ.0) return
      write(6,1000)
      N = NA-1
      NA = 1
      return
    1 format(1X,F10.4, 3X, F10.4)
    2 format(/2X,'FREQUENCY    AMPLITUDE')
 1000 format(33H1  PLOT OF AMPLIFICATION SPECTRA  /)
 1001 format(25H MAXIMUM AMPLIFICATION =   F6.2/
     1 25H FOR FREQUENCY         = F6.2, 7H C/SEC. /
     1 25H     PERIOD            =  F6.2, 5H SEC. )
      end