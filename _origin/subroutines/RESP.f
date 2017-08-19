C***********************************************************
      subroutine RESP(LN,LS,NN,X,AX,A,S,INV)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C   THIS PROGRAM readS DATA FOR RESPONSE SPECTRUM ANALYSIS
C   NECESSARY subroutineS    DRCTSP,  CMPMAX
C
C        NN      = RESPONSE SPECTRUM NUMBER
C        ND      = NUMBER OF DAMPING VALUES
C        X       = FOURIER TRANSFORM OF OBJECT MOTION
C        AX      = FOURIER TRANSFORM OF COMPUTED MOTIONS
C        T       = PERIODS FOR WHICH RESPONSE IS to BE COMPUTED
C
C   CODED PER B SCHNABEL DEC. 1970
C   New Sets of Periods -- included in February 1991
C
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      character*6 TITLE,ID,IBLANK,IDNT
      character*60 ABSIS
      character*32 FPERIOD
      character*80 headerd
      complex X, AX
C
      dimension X(64), AX(3,64),A(2,64), S(10),INV(10)
      dimension ID(27,11)
C
      common /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      common /EQ/ MFOLD,MA2,TITLE(5),DT, MA , MMA, DF,MX
      common /RVAL/ NND(27), ZLD(6),T(200), SA(5,200),SV(5,200)
C
      IBLANK = '      '
      ABSIS = ' PERIOD IN SEC.'
C
      read(5,4) ND, KPER, GGT
    4 format(2I5,F10.2)
      read(5,5) (ZLD(I), I = 1,ND)
    5 format(6F10.3)
      write(6,9001) LN, (ZLD(I), I = 1,ND)
C -------------------------------------------------------------------
C   if KPER = 0; Periods from 0.03 to 10 sec are included in data block
C                in this subroutine
C    otherwise, periods are specified by user (maximum is 200 periods)
C -------------------------------------------------------------------
      if (KPER.eq. 0) go to 99
      read(5,'(A32)') FPERIOD
      write(6,60) FPERIOD
 60   format(' File from which periods were read: ' A32)
      open(8,FILE=FPERIOD,STATUS='OLD')
      read (8,4) NLINES, NNM
      do I = 1, NLINES
        read(8,*) headerd
        write(6,*) headerd
      end do
      read(8,*) (T(I), I=1, NNM)
      close(8)
      go to 101
C ----------------------------------------------------------------------
C  default periods for calculating response spectra
C  ---------------------------------------------------------------------
   99   NNM=152
      T(1) = .01
      data (t(i), i=2,152)/
     1     0.03,    0.04,    0.05,    0.06,    0.07,    0.08,    0.09,
     2     0.10,    0.11,    0.12,    0.13,    0.14,    0.15,    0.16,
     3     0.17,    0.18,    0.19,    0.20,    0.21,    0.22,    0.23,
     4     0.24,    0.25,    0.26,    0.27,    0.28,    0.29,    0.30,
     5     0.31,    0.32,    0.33,    0.34,    0.35,    0.36,    0.37,
     6     0.38,    0.39,    0.40,    0.41,    0.42,    0.43,    0.44,
     7     0.45,    0.46,    0.47,    0.48,    0.49,    0.50,    0.51,
     8     0.52,    0.53,    0.54,    0.55,    0.56,    0.57,    0.58,
     9     0.60,    0.62,    0.64,    0.66,    0.68,    0.70,    0.72,
     T     0.74,    0.76,    0.78,    0.80,    0.82,    0.84,    0.86,
     1     0.88,    0.90,    0.92,    0.94,    0.96,    0.98,    1.00,
     2     1.05,    1.10,    1.15,    1.20,    1.25,    1.30,    1.35,
     3     1.40,    1.45,    1.50,    1.55,    1.60,    1.65,    1.70,
     4     1.75,    1.80,    1.85,    1.90,    1.95,    2.00,    2.05,
     5     2.10,    2.15,    2.20,    2.25,    2.30,    2.35,    2.40,
     6     2.50,    2.60,    2.70,    2.80,    2.90,    3.00,    3.10,
     7     3.20,    3.30,    3.40,    3.50,    3.60,    3.70,    3.80,
     8     3.90,    4.00,    4.10,    4.20,    4.30,    4.40,    4.50,
     9     4.60,    4.70,    4.80,    4.90,    5.00,    5.10,    5.20,
     T     5.40,    5.60,    5.80,    6.00,    6.20,    6.40,    6.60,
     1     6.80,    7.00,    7.20,    7.40,    7.60,    7.80,    8.00,
     2     8.50,    9.00,    9.50,   10.00/
c ---------------------------------------------------------------------
C   SAVE VALUES OF X IN AA
  101 do I = 1,MFOLD
        A(1,I) = real(X(I))
        A(2,I) = AIMAG(X(I))
        if (LS .ne. 0) then
          X(I) = AX(LS,I)
        end if
      end do
C
C   TRANSFORM VALUES IN X OR AX INto THE TIME DOMAIN
      call RFSN(X,MX,INV,S,IFERR,-2)
      do L = 1,ND
        if (NN .ge. 5)  NN= 0
        NN = NN + 1
        do I = 1,5
          ID(NN,I) = TITLE(I)
        end do
        do I = 6,11
          ID(NN,I) = IDNT(I-5)
          if (LS .eq. 0) ID(NN,I) = IBLANK
        end do
C
C       COMPUTE RESPONSE FOR ACCELERATION VALUES IN AA(1, )FOR THE PERIODS
C       GIVEN IN T( )
        call  DRCTSP(NN,MMA, DT, GGT, ID, ZLD(L),NNM,X)
      end do
C
C   GIVE X BACK ORIGINAL VALUES
      do I = 1,MFOLD
        X(I) = CMPLX(A(1,I),A(2,I))
      end do
C     ==============================================================
  134 NN = 0
      return
 1000 format(10I5)
 9000 format( 8F10.3)
 9001 format( 50H  RESPONSE SPECTRUM ANALYSIS FOR LAYER NUMBER     I4
     1/26H CALCULATED FOR DAMPING    8F10.3)
      end