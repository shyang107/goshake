C**********************************************************************
      subroutine DRCTSP(NN, KG, DT, GGT, ID, D, M, A)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C   THIS ROUTINE COMPUTES RESPONSE SPECTRA BY THE STEP BY STEP METHOD
C
C       NN      = RESPONSE SPECTRUM CURVE NUMBER  USED (Canceled)
C       KG      = NUMBER OF ACCELERATION VALUES
C       DT      = TIME STEP BETWEEN EACH ACCELERATION VALUE
C       M       = NUMBER OF PERIODS FOR WHICH RESPONSE IS to BE COMPUTED
C       T       = ARRAY WITH THE PERIODS
C       A       = ACCELERATION VALUES
C       D       = CRITICAL DAMPING RATIO
C       ID      = IDENTIFICATION
C       GGT     = Acceleration of gravity - cm/sec/sec, or in/sec/sec
C                  or ft/sec/sec
C
C   CODED BY I. M. IDRISS 1967
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      character*6 ID
      dimension A(1)
      common /RVAL/ NND(27), ZLD(6),T(200), SA(5,200),SV(5,200)
      dimension PRV(200), PAA(200), RD(200)
      dimension ID(27,11)
C .....................................................................
      zmax =0
      do K = 1, KG
        if (zmax .gt. ABS(A(K))) then
          A(K) = GGT*A(K)
        else
          zmax = ABS(A(K))
        end if
      end do
      PIW = 6.283185307
      SV(NN,1) = zmax*GGT*T(1)/PIW
      SA(NN,1) = zmax
      KUG = KG-1
      RD(1) = zmax*GGT*T(1)*T(1)/(PIW*PIW)
      PRV(1) = zmax*GGT*T(1)/PIW
      PAA(1) = zmax
      write(6,112) D
      N = 1
      YY = SQRT(1.-D*D)
      do LOOP = 2, M
        W = 6.283185307/T(N)
        WD = YY*W
        W2 = W*W
        W3 = W2*W
        call CMPMAX(KUG,T(N),W,W2,W3,WD,D,DT,ZD,ZV,ZA,A)
        SV(NN,N) = ZV
        SA(NN,N) = ZA/GGT
        RD(N) = ZD
        PRV(N) = W*ZD
        PAA(N) = W2*ZD/GGT
        N = N + 1
      end do
      write(6,312) GGT, (ID(NN,I), I = 1,10),D
      SUMSV = 0.
      SUMSA = 0.
      SUMT  = 0.
      SVMAX = 0.
      SAMAX = 0.
      TT1 = .1
      TT2 = 0.
      do N = 1, M
        FREKV = 1./T(N)
        if (T(N) .lt. .0999 .or. TT2.gt.2.4999) then
          continue
        end if
        TT2 = (T(N+1) + T(N))/2.
        if (TT2.gt.2.5)  TT2 = 2.5
        TT = TT2 - TT1
        SUMSA = SA(NN,N)*TT + SUMSA
        SUMSV = SV(NN,N)*TT + SUMSV
        SUMT = SUMT + TT
        TT1 = TT2
        if (SVMAX.lt.SV(NN,N))  SVMAX = SV(NN,N)
        if (SAMAX.lt.SA(NN,N))  SAMAX = SA(NN,N)
        write(6,322) N,T(N),RD(N),SV(NN,N),PRV(N),SA(NN,N),PAA(N),FREKV
      end do
      write(6,2002) SUMSA,SUMSV,SAMAX,SVMAX
      do K = 1,KG
        A(K) = A(K)/GGT
      end do
      return
C
  112 format(/5X,41HTIMES AT WHICH MAX. SPECTRAL VALUES OCCUR /
     1 10X,33HTD = TIME FOR MAX. RELATIVE DISP. /
     2 10X,33HTV = TIME FOR MAX. RELATIVE VEL.  /
     3 10X,33HTA = TIME FOR MAX. ABSOLUTE ACC.   /
     4 5X, 15HDAMPING RATIO =  F5.2)
  312 format(5X,' SPECTRAL VALUES --'/
     15X,' [Acceleration of gravity used =' F8.2 ']'/
     210A6,2X,15HDAMPING RATIO =
     3 F5.2/5X,3HNO.,4X,6HPERIOD,5X,10HREL. DISP.,6X,9HREL. VEL.,3X,
     4 12HPSU.REL.VEL.,6X,9HABS. ACC.,3X,12HPSU.ABS.ACC. 5X,5HFREQ.)
  322 format(I8,F10.2,5F15.5,F10.2)
  402 format(8F9.5)
  412 format(I5,25H ACC. RESPONSE VALUES FOR   , 8A6)
  413 format(I5,25H VEL. RESPONSE VALUES FOR   , 8A6)
 2002 format(10X,40HVALUES IN PERIOD RANGE .1 to 2.5 SEC.        /
     115X,35HAREA OF ACC. RESPONSE SPECTRUM   =   F10.3/
     215X,35HAREA OF VEL. RESPONSE SPECTRUM   =   F10.3/
     315X,35HMAX. ACCELERATION RESPONSE VALUE =   F10.3/
     415X,35HMAX. VELOCITY     RESPONSE VALUE =   F10.3)
      end