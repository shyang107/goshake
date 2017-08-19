C*****************************
      subroutine INCR(IFR,X,AX)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C   THIS ROUTINE INCREASES NUMBER OF POINTS IN THE RECORD
C   BY DECREASING TIMESTEP
C
C        IFR   =   MULTIPLYING FACTOR ON LENGTH OF RECORD
C                  MUST BE A POWER OF 2.
C        DT    =   TIMESTEP IN SEC.
C        DF    =   FREQUENCY STEP IN C/SEC.
C        MA    =   NUMBER OF POINTS USED IN FOURIER TRANSFORM
C        X     =   FOURIER TRANSFORM OF OBJECT MOTION
C        AX    =   FOURIER TRANSFORM OF COMPUTED MOTIONS
C
C
C   CODED BY PER B. SCHNABEL DEC. 1970.
C   MODIFIED OCT.  1971
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      complex X, AX
      character*6 TITLE
C
      dimension X( 68), AX(3, 64)
      common /EQ/ MFOLD,MA2,TITLE(5),DT, MA , MMA, DF,MX
C
      F1 = .5/DT
      FR = FLOAT(IFR)
      DT = DT/FR
      N = MFOLD- 1
      MA = MA*IFR
      MMA = MMA*IFR
      MA2 = MA + 2
      MFOLD = MA2/2
      MFOLD = MFOLD + 1
      do 10 I = N, MFOLD
      X(I) = 0.
      do 10 L = 1,3
   10 AX(L,I) = 0.
      F2 = .5/DT
      write(6,1000) F1,F2,DT, MA
      FMA = FLOAT(MA)
      MX = (ALOG10(FMA)/ALOG10(2.))-1.
      if (MA.GT.2**(MX+1)) MX=MX+1
 1000 format(27H    FREQUENCIES ADDED FROM  F6.2,3H TO F6.2/
     216H NEW TIME STEP = F5.4/19H NUMBER OF VALUES = I5/)
      return
      end