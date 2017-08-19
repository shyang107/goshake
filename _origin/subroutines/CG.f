C $NODEBUG

C $NOFLOATCALLS
C $NODEBUG
C*****************************************************
      SUBROUTINE CG
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * *
C
C   THE SUBROUTINE READ POINTS ON A CURVE AND GENERATES NEW POINTS
C   BETWEEN THE GIVEN POINTS IN ARITHMETIC OR HALFLOGARITMIC SCALE
C   NECESSARY SUBROUTINES    CURVEG(),
C
C        NST     = NUMBER OF SOILTYPES
C        ABSIS   = TITLE ON ORDINATE FOR PLOTTING
C        NN      = NUMBER OF VALUES IN EACH 10 FOR SEMILOGPLOT
C        SC      = SCALE FOR PLOTTING
C        NC      = NUMBER OF CURVES
C        NV      = NUMBER OF VALUES WHERE STRAIN/PROPERTY-RELATION
C                  IS GIVEN
C        FPL     = MULTIPLICATION FACTOR FOR PLOTTING
C        ID      = IDENTIFICATION
C        X       = STRAIN VALUES
C        Y       = PROPERTY VALUES
C
C   CODED BY PER B SCHNABEL SEPT 1970
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  * * *
C
      CHARACTER*6 ID
      CHARACTER*60 ABSIS
C
C     DIMENSION Y(27,20), TSTEP(27),NT(27),FPL(27),V(27,200),T(200)
      INTEGER ECHO(13)
      COMMON /JOE1/ Y(27,20), TSTEP(27),NT(27),FPL(27),V(27,200),T(200)
      COMMON /SOILDG/ S(27,20),AS(27,20),BS(27,20), NV(27)
      COMMON /CCG/ ID(27,11)
C
      ABSIS = ' STRAIN IN PERCENT '
C
      READ(5,*) NST
      NC = 2*NST
      DO 1 L = 1,NC
      READ(5,2001) NV(L), (ID(L,I), I=1,11)
      M = NV(L)
      READ(5,1002) (S(L,I), I = 1,M)
      READ(5,1002) (Y(L,I), I = 1,M)
    1 CONTINUE
C---------------------------------------------
C     ECHO INPUT DYNAMIC PROPERTY CURVES
C---------------------------------------------
      READ (5, 1007) NECHO,(ECHO(I), I=1,NECHO)
C      DO 10, I=1, NC, 2
      DO 10, K=1,NECHO
C      MTYPE=(I+1)/2
      MTYPE=ECHO(K)
      I=ECHO(K)*2-1
      WRITE(6,1003) MTYPE
      WRITE(6,1004) I, (ID(I,J), J=1,10), I+1, (ID(I+1,J),J=1,10)
      WRITE(6,1005) I,I+1
      M=MAX0(NV(I),NV(I+1))
   10 WRITE(6,1006) (S(I,J),Y(I,J),S(I+1,J),Y(I+1,J), J=1,M)
      CALL CURVEG( NC, NV,  2, AS, BS, 10, TSTEP, NT, T, V, S, Y, NSTEP)
C--------------------------
 1000 FORMAT(3I5,F10.0)
 1001 FORMAT(/I5,F10.2, 11A6)
 1002 FORMAT(8F10.3)
 1003 FORMAT (/' **********************'/
     +       '  MATERIAL TYPE NO.',I2,/
     +       ' **********************')
 1004 FORMAT(/2(1X,'CURVE NO. ',I2,': ', 10a6/)/)
 1005 FORMAT(  '      CURVE NO.',I2,'              CURVE NO.',I2,'    '/
     1         '  ===================       =================='/
     2         '    STRAIN    G/Gmax         STRAIN    DAMPING'/
     3         '  --------   -------        --------  --------')
 1006 FORMAT(1X,F9.4,4X,F6.3,5X,1X,F9.4,4X,F6.2,5X)
 1007 FORMAT(16I5)
 2001 FORMAT(I5, 11A6)
 2002 FORMAT( 12F10.4)
 3000 FORMAT(53H  MODULUS AND DAMPING VALUES ARE SCALED FOR PLOTTING   )
 3001 FORMAT(55H    CURVES FOR RELATION STRAIN VERSUS SHEAR MODULUS AND
     1 8H DAMPING /)
      RETURN
      END

