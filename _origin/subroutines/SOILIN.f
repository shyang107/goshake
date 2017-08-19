      SUBROUTINE SOILIN(N1)
C***********************************************************************
C
C   THIS ROUTINE READS PROPERTIES OF A SOIL PROFILE, ASSIGNS VALUES TO
C   EACH LAYER, CALCULATES TOTAL PRESSURE AND DEPTH IN MIDDLE OF
C   EACH LAYER AND PRINTS THE RESULTS
C
C        IDNT    = IDENTIFIER FOR SOIL PROFILE
C        BL      = RATIO OF CRITICAL DAMPING
C        GL      = SHEAR MODULUS
C        FACT    = FACTOR FOR CALCULATING SHEAR MODULUS FROM STRAIN
C        H       = LAYER THICKNESS
C        R       = DENSITY
C        WL      = UNIT WEIGHT
C        TP      = SOIL TYPE
C        DEPTH   = DEPTH TO MIDDLE OF LAYER
C        WEIGTH  = TOTAL PRESSURE
C        ML      = NUMBER OF LAYERS INCLUDING HALFSPACE
C        N1      = NUMBER OF SUBLAYERS EXCLUDING HALFSPACE
C        NLN     = NUMBER OF SUBLAYERS IN EACH LAYER
C        W       = UNIT WEIGHT
C        VS      = SHEAR WAVE VELOCITY
C        BFAC    = FACTOR ON DAMPING
C        FACTOR  = FACTOR ON SHEAR MODULUS
C        HL      = THICKNESS OF LAYER
C        H       = THICKNESS OF SUBLAYER
C        GMOD    = SHEAR MODULUS
C        B       = CRITICAL DAMPING RATIO
C
C   CODED BY PER B SCHNABEL OCT. 1970
C   MODIFIED APRIL 1972
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      INTEGER TP, TYPE
      CHARACTER*6 IDNT
      DIMENSION SMEAN(51)
C
      COMMON /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      COMMON /SOILB/ FAC(51), WL(51), TP(51), DEPTH(51), WEIGHT(51)
      COMMON /SOILC/ MSOIL,MWL
      COMMON /SOILD/ GLMAX(51)
      COMMON /WGK/  WW, GT, SKO
C
      READ(5,1003) MSOIL, ML, MWL, (IDNT(I),I=1,6)
      WRITE(6,2020) MSOIL, (IDNT(I),I=1,6)
C
C READ SOIL PROPERTIES FOR EACH LAYER AND ASSIGN VALUES TO EACH SUBLAYER
C
      J = 0
      DO 14 N =1, ML
      READ(5,1004) K, TYPE, NLN,   HL, GMOD, B , W, VS
      FACTOR = 1.
       BFAC = 1.
      IF (NLN.EQ. 0) NLN = 1
      IF (K .EQ. N) GO TO 141
      WRITE(6,2004) N
      STOP
C
C   COMPUTE MODULUS FROM SHEAR WAVE VELOCITY
C
  141 IF (GMOD .EQ. 0.) GMOD = VS*VS*W/GT
      DO 14 I = 1,NLN
      J = J+1
      BL(J) = B
      GL(J) = GMOD
      GLMAX(J) = GMOD
      FAC(J) = 1.
      FACT(J) =  1.
      BF(J) = 1.
      WL(J) = W
      H(J) = HL/NLN
      TP(J) = TYPE
   14 R(J) = W/GT
      N1 = J -1
C
C   CALCULATE AVERAGE DEPTH AND TOTAL PRESSURE IN EACH LAYER
C
      W1 = WL(1)
      IF (MWL .EQ. 1) W1 = WL(1) - WW
      DEPTH(1) = H(1)/2.
      WEIGHT(1) = H(1)*W1/2.
      SMEAN(1) = WEIGHT(1)*(1.+2.*SKO)/3.
      IF  (N1 .EQ. 1)  GO TO 151
      DO 15 I = 2,N1
      W2 = WL(I)
      IF (MWL .LT. I+1) W2 = WL(I) - WW
      DEPTH(I) = DEPTH(I-1) + H(I)/2. + H(I-1)/2.
      WEIGHT(I) = WEIGHT(I-1) + H(I)*W2/2. + H(I-1)*W1/2.
      SMEAN(I) = WEIGHT(I)*(1.+2.*SKO)/3.
   15 W1 = W2
  151 TD = DEPTH(N1) + H(N1)/2.
      IF (MWL .LT. N1+1)  WD = DEPTH(MWL)- H(MWL)/2.
      IF (MWL .EQ. N1+1)  WD = DEPTH(MWL-1) + H(MWL-1)/2.
C
C   CALCULATE FACTOR FOR SHEAR MODULUS
      DO 16 I = 1,N1
      IF (TP(I) .EQ. 0) GO TO 16
      IF (BF(I).LT..01) BF(I) = 2.53 - .45*ALOG10(WEIGHT(I)*1000.)
      NTP = TP(I)
C ----------------------------------------------------------------------
C  A total of 13 G/Gmax material types can be used
c-----------------------------------------------------------------------
      FAC(I) = FACT(I)
      FACT(I) = GL(I) * 1000. * FACT(I)
c-----------------------------------------------------------------------
   16 CONTINUE
  131 WRITE(6,2021) ML,TD
      WRITE(6,2015)
      DO 17 I = 1,N1
      VS = SQRT( GL(I)/R(I))
      WRITE(6,2005) I, TP(I), H(I),DEPTH(I)
     1,WEIGHT(I),GL(I),BL(I),WL(I),VS
   17 CONTINUE
      I = N1 + 1
      VS = SQRT(GL(I)/R(I))
      WRITE(6,2105) I, GL(I), BL(I), WL(I), VS
      CALL CXSOIL(N1)
 1003 FORMAT(3I5, 6A6)
 1004 FORMAT(3I5, 6F10.0,F5.0)
 2004 FORMAT(17H   SOIL CARD NO. I4,17H OUT OF SEQUENCE )
 2020 FORMAT(22H NEW SOIL PROFILE NO. ,I3,5X,17H IDENTIFICATION   ,6A6)
 2021 FORMAT(17H NUMBER OF LAYERS ,I20,10X,16HDEPTH TO BEDROCK,F14.2/)
 2015 FORMAT(  '  NO. TYPE  THICKNESS   DEPTH  ',
     1   'Tot. PRESS.  MODULUS  DAMPING   UNIT WT.  SHEAR VEL' /
     3   '              (ft)      (ft)      (ksf)       (ksf)',
     4    '             (kcf)    (fps)')
 2005 FORMAT(I4,I5,F10.2,F10.2,F10.2,F12.0,F8.3,F9.3,F10.1)
 2105 FORMAT(    I4, 3X, 4HBASE  25X, F15.0, F8.3, F9.3, F10.1)
      RETURN
      END