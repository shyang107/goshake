C*********************************************
      subroutine STRAIN( LL, LGS, LPCH, LPL,LNV,X,AX,AA,N1,S,INV)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C   THIS subroutine COMPUTES STRAIN AND/OR STRESS TIME-HISTORY AT THE
C   TOP OF ANY LAYER FOR ACCELERATION HISTORY KNOWN IN ANY LAYER
C   TWO RESPONSE HISTORIES ARE COMPUTED IN ONE RUN
C
C        LL    =   SUBLAYER NUMBER WHERE RESPONSE IS TO BE COMPUTED
C        LGS   =   SWITCH FOR STRESS OR STRAIN
C        LPCH  =   SWITCH FOR SAVING OUTPUT
C        LPL   =   SWITCH FOR PLOT
C        X     =   FOURIER TRANSFORM OF OBJECT MOTION
C        AX(1, )   FOURIER TRANSFORM OF SURFACE MOTION
C        AX(2, )   FOURIER TRANSFORM OF FIRST COMPUTED RESPONSE
C        AX(3, )   FOURIER TRANSFORM OF SECOND RESPONSE
C        AA(1, )   TIME HISTORY OF FIRST RESPONSE
C        AA(2, )   TIME HISTORY OF SECOND RESPONSE
C
C   CODED BY PER B. SCHNABEL  JULY 1971
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      integer TP
      character*6 TITLE,IDNT,ID
      character*60 ABSIS
      complex X, AX
      complex G, V, PLUS, MINUS
      complex E,F,EE, A,AH,IPI2, AE,AF,EX,AI
C
      dimension AE(2), AF(2)
      dimension X(1),  AX(3,1), AA(2,1), S(1),  INV(1)
      dimension LL(2), LGS(2),  LPCH(2), LPL(2), LNV(2)
C
      common /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      common /SOILB/ FAC(51), WL(51), TP(51), DEPTH(51), WEIGHT(51)
      common /CSOIL/ G(51), V(51), PLUS(51), MINUS(51)
      common /EQ/ MFOLD,MA2,TITLE(5),DT, MA , MMA, DF,MX
      common /CCG/ ID(27,11)
      common /FRCUT/ NCUT,NZERO
      common /TIME/ T(9)
C
      ABSIS = ' TIME IN SEC '
      IPI2 = CMPLX(0.,6.283185307)
      GT = 32.2
      AX(2,1) = 0.
      AX(3,1) = 0.
      FREQ = 0.
      AI = GT/IPI2
C
C  STARTING AT THE SURFACE THE STRAIN IS COMPUTED SUCCESSIVELY DOWNWARDS
C   FOR EACH FREQUENCY
      do 1 I=2,NCUT
      E = AX(1,I)/2.
      F = E
      FREQ = FREQ + DF
      AH = AI/FREQ
      A = FREQ*IPI2
      do 11  K = 1,N1
      do 12 L = 1,2
      if (K.NE.LL(L)) go to 12
      AE(L) = E/V(K)
      AF(L) = F/V(K)
   12 continue
      EX = CEXP(H(K)*A/V(K))
      E = E*EX
      F = F/EX
      EE = E*PLUS(K) + MINUS(K)*F
      F  = F*PLUS(K) + MINUS(K)*E
      E = EE
   11 continue
      do 13 L = 1,2
      if (LL(L).NE.N1+1) go to 13
      AE(L) = E/V(N1+1)
      AF(L) = F/V(N1+1)
   13 continue
      do 14 L = 1,2
      if (LL(L).GT.0) AX(L+1,I) = (AE(L) -AF(L))*AH
   14 continue
    1 continue
      do 2 I = 1,MFOLD
    2 AX(1,I) = X(I)
      do 3 L = 1,2
      if (LL(L).EQ.0) go to 3
      X(1) = 0.
      do 31 I=2,NCUT
   31 X(I) = AX(L+1,I)
      IF(NCUT.EQ.MFOLD) go to 33
      do 34 II=NZERO,MFOLD
      X(II)=CMPLX(0.,0.)
   34 continue
   33 continue
      call RFSN(X,MX,INV,S,IFERR,-2)
      do 32 I =1,MFOLD
      AA(L,2*I-1) = REAL(X(I))*100.
   32 AA(L,2*I) = AIMAG(X(I))*100.
    3 continue
C
      do 4 I = 1,MFOLD
    4 X(I) = AX(1,I)
C   COMPUTE STRESS IF WANTED AND SAVE COMPUTED RESPONSES
      do 5 L = 1,2
      if (LL(L) .EQ. 0) go to 5
      NVAL = LNV(L)
      if (NVAL.LE.0) NVAL = MMA
      if (NVAL.GT.MA)  NVAL = MA
      if (NVAL.GT.2049) NVAL = 2049
      do 51 I = 1,5
   51 ID(L,I) = TITLE(I)
      N = LL(L)
      ID(L,6) = 'STRAIN'
      if (LGS(L) .EQ.0) go to 53
      ID(L,6) = 'STRESS'
      do 52 I = 1,NVAL
   52 AA(L,I) = GL(N)*AA(L,I)/100.
   53 if (LPCH(L).EQ.0) go to 54
      write(7,2000) (ID(L,I), I=1,11),N
      N = 1
      NCARDS = NVAL/8
      do 55 K = 1,NCARDS
      NN = N + 7
      write(7,2001) (AA(L,I), I = N,NN), K
   55 N = N + 8
   54 if (LPL(L).EQ.0) go to 5
      N = 0
      NSKIP = 1
      do 56 I = 1,NVAL,NSKIP
      N = N + 1
      if (NSKIP.GT.1) AA(L,N) = AA(L,I)
   56 T(N) = DT*FLOAT(I-1)
      IF  (LGS(L).EQ.0)  write(6,2002)
      if (LGS(L).EQ.1)  write(6,2003)
      if (LPL(L).EQ.0) go to 5
      if (LPL(2).EQ.2) go to 5
      if (L.EQ.1)  go to  58
      do 57 I = 1,N
   57 AA(1,I) = AA(2,I)
      do 50 I = 1,11
   50 ID(1,I) = ID(2,I)
   58 continue
      go to 5
    5 continue
 2000 format(11A6,5HLAYER I5)
 2001 format(8F9.6,I7)
 2002 format(41H1  TIME HISTORY OF STRAIN IN PERCENT         )
 2003 format(41H1  TIME HISTORY OF STRESS IN KSF            )
      return
      end
