C ......................................................................
      SUBROUTINE STRT( IT,N1,DGMAX,PRMUL,X,AX,AA,SF,INV)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  *
C
C   THIS ROUTINE CALCULATES STRAIN IN THE MIDDLE OF EACH LAYER AND FIND
C   NEW SOIL PROPERTIES COMPATIBLE WITH THE STRAINS
C
C        IT      = ITERATION NUMBER
C        N1      = NUMBER OF LAYERS EXCLUDING ROCK
C        DGMAX   = MAX ERROR IN SOIL PARAMETERS B OR G IN PERCENT
C        X       = OBJECT MOTION
C        AX(1, ) = ACCELERATION VALUES AT THE SURFACE
C        AX(2, ) = INCIDENT WAVE-COMPONENT
C        AX(3, ) = REFLECTED WAVE-COMPONENT
C        PRMUL   = RATIO EFF. STRAIN/MAX. STRAIN
C
C   CODED PER B SCHNABEL OCT. 1970
C   MODIFIED PBS  SEPT. 1971
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  *
      INTEGER TP
      CHARACTER*6 TITLE,IDNT
      CHARACTER*30 FINPEQ
      COMPLEX IPI2, EX, E, F, EE, FF
      COMPLEX X, AX
      COMPLEX G, V, PLUS, MINUS
C
      DIMENSION TMAX(51),EMAX(51),STR(51)
      DIMENSION X( 68), AX(3, 64), AA(2,128),SF(10), INV(10), ratio(51)
      COMMON /EQ/ MFOLD,MA2,TITLE(5),DT, MA , MMA, DF,MX
      COMMON /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      COMMON /SOILB/ FAC(51), WL(51), TP(51), DEPTH(51), WEIGHT(51)
      COMMON /SOILC/ MSOIL,MWL
      COMMON /SOILD/ GLMAX(51)
      COMMON /SOILDG/ S(27,20), AS(27,20), BS(27,20), NV(27)
      COMMON /CSOIL/ G(51), V(51), PLUS(51), MINUS(51)
      COMMON /JISCK/ JIS,FINPEQ
      COMMON/FRCUT/ NCUT,NZERO
C
      DO 43 I = 1,MFOLD
      AA(1,I) = REAL(X(I))
   43 AA(2,I) = AIMAG(X(I))
      DO 1  I = 1,MFOLD
      AX(2,I) = AX(1,I)/2.
    1 AX(3,I) = AX(2,I)
      PI2=6.283185307
      IPI2=CMPLX(0.,PI2)
      GT = 32.2
      DO 2 K = 1,N1
C     WRITE(*,2029) K
      FREQ = 0.
      X(1) = 0.
      FF = GT/(IPI2*V(K))
      EE = H(K)/2.*IPI2/V(K)
      DO 20 I=2,NCUT
      FREQ = FREQ + DF
      EX = CEXP(FREQ*EE)
      X(I) = (AX(2,I)*EX - AX(3,I)/EX)*FF/FREQ
      EX = EX*EX
      E = AX(2,I)*EX
      F = AX(3,I)/EX
      AX(2,I)= PLUS(K)*E + MINUS(K)*F
      AX(3,I)= PLUS(K)*F + MINUS(K)*E
   20 CONTINUE
      EMAX(K) = 0.
      IF(NCUT.EQ.MFOLD) GO TO 22
      DO 122 II=NZERO,MFOLD
      X(II)=CMPLX(0.,0.)
  122 CONTINUE
   22 CONTINUE
C
C   DETERMINE MAX. STRAIN BY INVERTING FOURIER TRANSFORM OF STRAIN
C   INTO THE TIME DOMAIN
C
      CALL RFSN(X,MX,INV,SF,IFERR,-2)
      CALL XMX(X,MA,XMAX,NXMAX)
C
      EMAX(K) = XMAX
      TMAX(K) = FLOAT(NXMAX-1)*DT
    2 CONTINUE
      IF (IT.GT.1) WRITE(6,2002)
      WRITE(6,2017) FINPEQ, (IDNT(I),I=1,6)
      WRITE(6,2027) IT
C     WRITE(6,2037) PRMUL
      WRITE(6,2000)
      DGMAX = 0.
      DO 23 I = 1,N1
      EM = EMAX(I)*PRMUL*100.
      EMAX(I) = EMAX(I)*100.
      IF (TP(I) .NE. 0)  GO TO 231
      STR(I) = EMAX(I)*GL(I)*10.
      WRITE(6,2107) I, TP(I), DEPTH(I), EM , BL(I), GL(I)
      GO TO 23
C
C   USE UNIFORM STRAIN AMPLITUDE (EM) TO GET NEW VALUES FOR DAMPING
C   AND SHEAR MODULUS
  231 IN = TP(I)*2 - 1
      SS= ABS(EM)
      SL = ALOG10(SS)
      LL = NV(IN)
      DO 31 L = 1,LL
      IF (SS.LE. S(IN,L)) GO TO 311
   31 CONTINUE
      L = LL
  311 GN =AS(IN,L)*SL +BS(IN,L)
      GG = GN*FACT(I)/1000.
      IN = IN + 1
      LL = NV(IN)
      DO 32 L = 1,LL
      IF (SS.LE. S(IN,L)) GO TO 321
   32 CONTINUE
      L = LL
  321 B  =AS(IN,L)*SL +BS(IN,L)
      B = B*BF(I)
C ----------------------------------------------------------------------
C  SHEAR STRESSES ARE COMPUTED USING CURRENT MODULI
C
      STR(I) = EMAX(I)*GL(I)*10.
      RATIO(I) = GL(I) / GLMAX(I)
C ----------------------------------------------------------------------
      B = B/100.
      DG = (GG - GL(I))*100./GG
      DB = ( B - BL(I))*100./B
      WRITE(6,2007) I, TP(I), DEPTH(I), EM, B, BL(I), DB, GG, GL(I), DG,
     + RATIO(I)
      IF (ABS(DG) .GT. DGMAX)  DGMAX = ABS(DG)
      IF (ABS(DB) .GT. DGMAX)  DGMAX = ABS(DB)
      IF (JIS .EQ. 1) GO TO 23
      BL(I) = B
      GL(I) = GG
   23 CONTINUE
      IF (JIS .NE. 1) GO TO 53
      WRITE(6,2011)
      WRITE(6,2001) (I,TP(I), H(I),DEPTH(I), EMAX(I), STR(I), TMAX(I),
     1 I = 1,N1)
   53 CALL CXSOIL(N1)
      DO 44 I = 1,MFOLD
   44 X(I) = CMPLX(AA(1,I),AA(2,I))
      RETURN
C
 2000 FORMAT(/23H  VALUES IN TIME DOMAIN //
     1,' NO TYPE DEPTH  UNIFRM. <---- DAMPING ---->   <---- SHEAR',
     2' MODULUS ----->    G/Go'/
     3'         (FT)   STRAIN  NEW    USED   ERROR      NEW       USED',
     4'    ERROR   RATIO'/
     5'--- ---- ----  ------- ----- ------  ------   -------   -------',
     6'   ------   -----')
 2002 FORMAT(1H1)
 2011 FORMAT(/23H  VALUES IN TIME DOMAIN //
     1 2X, 5HLAYER ,2X,4HTYPE ,6X,9HTHICKNESS ,10X, 5HDEPTH ,5X, 
     2 10HMAX STRAIN ,5X, 10HMAX STRESS ,10X, 4HTIME /
     3 23X, 2HFT 14X, 2HFT 9X, 5HPRCNT 12X, 3HPSF 13X, 3HSEC /)
 2001 FORMAT(2I6, 2F15.1, F15.5,2F15.2)
 2007 FORMAT(2I3, F7.1, F8.5, F6.3, 1X, F6.3 ,F9.1, 1X,
     1 F9.1,1X, F9.1, F9.1, F8.3)
 2107 FORMAT(2I3, F8.1, F9.5, 2F7.3, F8.1, 2F10.1, F8.1, F7.3)
 2027 FORMAT(19H  ITERATION NUMBER   I2)
 2029 FORMAT(1H+,20X,'Processing layer no. ',I2)
 2030 FORMAT(1H+'                                                    ')
 2017 FORMAT(5X,15HEARTHQUAKE   -  A30/5X,15HSOIL PROFILE -   6A6/)
 2037 FORMAT(56H  THE CALCULATION HAS BEEN CARRIED OUT IN THE TIME DOMAI
     121HN WITH EFF. STRAIN = ,F3.2, 1H* 12H MAX. STRAIN )
      END
C****************************************************************