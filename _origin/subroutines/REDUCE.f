C**************************************
      subroutine REDUCE(IFR,X,AX,LL)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C   THIS ROUTINE INCREASES TIME INTERVAL AND REDUCES NUMBER OF VALUES
C
C        IFR   =   DIVIDING FACTOR ON LENGTH OF RECORD
C                  MULTIPLICATION FACTOR ON TIME STEP
C                  MUST BE A POWER OF 2.
C        DT    =   TIMESTEP IN SEC.
C        DF    =   FREQUENCY STEP IN C/SEC.
C        MA    =   NUMBER OF POINTS USED IN FOURIER TRANSFORM
C        X     =   FOURIER TRANSFORM OF OBJECT MOTION
C        AX    =   FOURIER TRANSFORM OF COMPUTED MOTIONS
C
C
C   CODED BY PER B. SCHNABEL DEC. 1970.
C   MODIFIED SEPT. 1971
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      character*6 TITLE
      complex X, AX
C
      dimension X( 68), AX(3, 64), LL(3)
      common /EQ/ MFOLD,MA2,TITLE(5),DT, MA , MMA, DF,MX
      common/FRCUT/ NCUT,NZERO
C
      F1 = .5/DT
      FR = FLOAT(IFR)
      DT = DT*FR
      MA = MA/IFR
      MMA = MMA/IFR
      MA2 = MA + 2
      MFOLD = MA2/2
      N = MFOLD + 1
      do 12 I = MFOLD,N
      X(I) = 0.
C
      do 12 L = 1,3
      if (LL(L).LE.0) go to 12
      AX(L,I) = 0.
   12 continue
      MFOLD = MFOLD + 1
      F2 = .5/DT
      write(6,1000) F1,F2,DT, MA
      FMA = FLOAT(MA)
      MX = (ALOG10(FMA)/ALOG10(2.))-1.
      if (MA.GT.2**(MX+1)) MX=MX+1
      IF(NCUT.LE.MFOLD) go to 15
      NCUT=MFOLD
   15 continue
 1000 format(  20H  FREQUENCIES FROM  F6.2, 3H TO F6.2,14H C/SEC ARE REM
     15HOVED /
     216H NEW TIMESTEP = F5.4/19H NUMBER OF VALUES = I5)
      return
      end