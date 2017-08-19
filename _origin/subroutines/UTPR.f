C******************************************
      subroutine UTPR(KK,DPTH,LS,K2,LH,LT,X,AX,S,INV)
C***********************************************************************
C
C   THIS ROUTINE TRANSFERS THE VALUES IN AX(LH, ) INTO THE TIME DOMAIN
C   IN X( ), TRANSFERS RESULTS TO OUTPUT FILE
C
C        KK      = 5 TABULATE MAX. ACC.
C                  6 PRINT MAX ACC. SEPARATELY
C        DPTH    = DEPTH OF LAYER
C        X( )    = OBJECT MOTION
C        AX(LS, )= COMPUTED MOTION
C        LS      = COMPUTED MOTION NUMBER
C                  0 IF OBJECT MOTION
C        LH      = SUBLAYER NUMBER
C        LT      = SUBLAYER TYPE
C                  0 - OUTCROPPING
C                  1 - INSIDE
C        S,INV  SCRATCH ARRAYS
C
C   CODED  PER B SCHNABEL  OCT. 1970
C   MODIFIED PBS AUG. 1971
C   modified to increase number of layers to 50
C***********************************************************************
      character*6 TITLE,IDNT
      complex SAVE
      complex X, AX
C
      dimension XR(8)
      dimension X(300),AX(3,270),S(70),INV(70)
      common /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      common /EQ/ MFOLD,MA2,TITLE(5),DT, MA , MMA, DF,MX
C
      FREQ = 0.
      SFX = 0.
      SXX = 0.
C   TRANSFORM VALUES IN X OR IN AX INTO THE TIME DOMAIN
      do 24  I = 1,MFOLD
      if (LS.EQ.0) go to 241
      SAVE = X(I)
      X(I) = AX(LS,I)
      AX(LS,I) = SAVE
  241 XA = CABS(X(I))
      SXX= SXX + XA*XA
      SFX = SFX + XA*FREQ*XA
      FREQ = FREQ + DF
   24 continue
      SFX = SFX/SXX
C
      call RFSN(X,MX,INV,S,IFERR,-2)
C
      call XMX(X,MA,XMAX,NMAX)
      TMAX = DT*FLOAT(NMAX-1)
      XEND = 0.
      N = MA/20
      NN = 9*N
      N = 8*N
      do 25 I = N,NN
      XABS = REAL(X(I))
      XABS = ABS(XABS)
      if (XABS.GT.XEND) XEND = XABS
      XABS = AIMAG(X(I))
      XABS = ABS(XABS)
      if (XABS.GT.XEND) XEND = XABS
   25 continue
      XEND = XEND/XMAX
C
C   SAVE OUTPUT
      N = 1
      NN = 4
      NCARDS=MA/8
      NC = NCARDS
      if (K2.EQ.0) NC = 0
C     if (KK.EQ.5) go to 252
      if (KK.EQ.6) go to 252
      if (LT.EQ.0)  write(6,2000) LH, (IDNT(I),I=1,6)
      if (LT.EQ.1)  write(6,2002) LH, (IDNT(I),I=1,6)
      write(6, 2005) SFX
      write(6,2003) XMAX, TMAX
  252 if (KK.EQ.6.AND.LT.EQ.0)  write(6,2001) DPTH,XMAX,TMAX,SFX,XEND,NC
      if (KK.EQ.6.AND.LT.EQ.1) write(6,2010) DPTH,XMAX,TMAX,SFX, XEND,NC
      if (K2.EQ.0) go to 262
      write(7,2006) XMAX, (TITLE(I),I=1,5)
      if (LT.EQ.1)  write(7,2002) LH, (IDNT(I),I=1,6)
      if (LT.EQ.0)  write(7,2000) LH, (IDNT(I),I=1,6)
      do 26 I = 1,NCARDS
      K = 0
      do 261 J = N,NN
      K = K+ 1
      XR(K) = REAL(X(J))
      K = K + 1
      XR(K) = AIMAG(X(J))
  261 continue
      write(7,2009) (XR(J),J=1,8),I
      if (K2 .EQ. 2) write(6,2019) (XR(J), J = 1,8), I
      NN = 4 + NN
      N = N + 4
   26 continue
  262 call RFFT(X,MX,INV,S,IFERR,2)
      if (LS.EQ.0)   return
      do 27 I = 1,MFOLD
      SAVE = AX(LS,I)
      AX(LS,I) = X(I)
   27 X(I) = SAVE
      return
C
 2000 format(43H  ACCELERATION VALUES AT OUTCROPPING LAYER I3,3H - 6A6)
 2001 format(5X,6HOUTCR. F15.1,F15.5,2F15.2,F20.3,I20)
 2010 format(5X,6HWITHIN F15.1,F15.5,2F15.2,F20.3,I20)
 2002 format(42H  ACCELERATION VALUES AT THE TOP OF LAYER I3,3H - 6A6)
 2003 format(/15H  MAX. ACC. =  F9.6,11H AT TIME = F6.3, 5H SEC. /)
 2005 format(/26H   MEAN SQUARE FREQUENCY = F10.2/)
 2006 format(21X,6HXMAX= F7.4,5A6)
 2008 format(2X, I5, 5X, 8F15.6)
 2009 format(8F9.6,I7)
 2019 format(8F14.6,I10)
      end