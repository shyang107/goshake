C***********************************************************
      subroutine MOTION(N1,IN,INT,LL,LT, X,AX)
***********************************************************
C   THIS ROUTINE CALCULATES THE MOTION IN ANY TWO SOIL LAYERS OR IN
C   ROCK FROM MOTION GIVEN IN ANY LAYER OR IN ROCK
C
C        N1      = NUMBER OF SOIL LAYERS EXCLUDING ROCK
C        IN      = NUMBER OF LAYER WHERE OBJECT MOTION IS GIVEN
C        INT     = MOTION TYPE
C                  IF EQUEL 0     OUTCROPPING LAYER
C        LL()    = NUMBER OF LAYERS WHERE OUTPUT MOTION IS WANTED
C                  MAX  3 LAYERS
C        LT()    = MOTION TYPE
C                  0 - OUTCROPPING LAYER
C                  1 - LAYER WITHIN PROFILE
C        X()     = OBJECT MOTION
C        AX()    = OUTPUT MOTION
C
C   CODED BY PER B SCHNABEL  OCT 1970
C   modified to increase the number of layers to 50
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      integer LL(3), LT(3)
      character*6 TITLE,IDNT
      complex AA(3)
      complex X, AX
      complex G, V, PLUS, MINUS
      complex E, F, EE, FF, A, EX, AIN, IPI2
C
      dimension X(300),AX(3,270),S(70)
      common /EQ/ MFOLD,MA2,TITLE(5),DT, MA , MMA, DF,MX
      common /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      common /CSOIL/ G(51), V(51), PLUS(51), MINUS(51)
      common/FRCUT/ NCUT,NZERO
C
      IPI2 = CMPLX(0., 6.283185307)
      do 20 L = 1,3
      if (LL(L) .GT. 0) AX(L,1) = X(1)
      IF(NCUT.EQ.MFOLD) go to 20
      do 30 I=NZERO,MFOLD
      AX(L,I)=CMPLX(0.,0.)
   30 continue
   20 continue
      FREQ = 0.
      do 19 I=2,NCUT
      E = 1.
      FF = 1.
      FREQ = FREQ + DF
      A = FREQ*IPI2
      do 191 K = 1,N1
      if (K.NE.IN) go to 192
      AIN = E + FF
      if (INT.EQ.0) AIN = 2.*E
C   FIND SUBLAYER WHERE MOTION IS WANTED
  192 do 11 L = 1,3
      if (K.NE.LL(L)) go to 11
C   AMPLIFICATION FACTOR FOR SUBLAYER WITHIN PROFILE
      AA(L) = E + FF
C   AMPLIFICATION FACTOR FOR OUTCROPPING SUBLAYER
      if (LT(L).EQ.0)  AA(L) = 2.*E
   11 continue
      EX = CEXP(H(K)*A/V(K))
      EE = E*EX
      F = FF/EX
      E  = EE*PLUS(K) + MINUS(K)*F
      FF = PLUS(K)*F + MINUS(K)*EE
  191 continue
      if (IN.NE.N1+1) go to 193
      AIN = E + FF
      if (INT.EQ.0) AIN = 2.*E
  193 do 21 L = 1,3
      if (LL(L).NE.N1+1) go to 21
      AA(L) = E + FF
      if (LT(L).EQ.0) AA(L) = 2.*E
   21 continue
      do 23 L = 1,3
      if (LL(L) .GT. 0) AX(L,I) = X(I)*AA(L)/AIN
   23 continue
   19 continue
      return
      end