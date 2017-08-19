C**********************************************************************
      subroutine  CURVEG(NC, NV, K1, A, B, NN, TSTEP, NT, T,V,X,Y,NSTEP)
C***********************************************************************
C   THE PROGRAM GENERATES NEW POINTS ON A CURVE BY LINEAR INTERPOLATION
C   USING AN ARITHMETIC OR A HALFLOGARITHMIC SCALE
C
C        NV(I)     =    NUMBER OF VALUES ON CURVE I
C        NC        =    NUMBER OF CURVES
C        K1        =    SWITCH    K1 = 1  ARITHMETIC SCALE
C                                 K1 = 2  HALFLOGARITHMIC SCALE
C        A,B       =    PARAMETERS FOR CALCULATING NEW VALUES
C                       Y = A*X + B
C        X,Y       =    KNOWN POINTS ON CURVE
C        T         =    VALUES ON ABSISSA WHERE NEW POINTS ARE GENERATED
C        V         =    NEW ORDINATE VALUES
C
C        ARITHMETIC SCALE  K1 = 1
C        NN        =    NUMBER OF INTERVALS
C        TSTEP     =    LARGEST VALUE IN EACH INTERVAL
C        NT        =    NUMBER OF STEPS IN EACH INTERVAL
C
C        HALFLOGARITHMIC SCALE
C        NN        =    NUMBER OF VALUES IN EACH LOG10
C
C   CODED BY PER B SCHNABEL SEPT 1970
C
C***********************************************************************
C
      dimension X(27,20),Y(27,20),A(27,20),B(27,20),NV(27),TSTEP(27)
      dimension NT(27), T(200), V(27,200)
C
      XMIN = 100000000.
      XMAX = 0.
      do L= 1,NC
        M = NV(L)
        if (XMAX .lt. X(L,M))   XMAX = X(L,M)
        if (XMIN .gt. X(L,1)) XMIN = X(L,1)
        M = M - 1
        do I = 1,M
          X1 = X(L,I)
          X2 = X(L,I+1)
          if (K1 .eq. 2)  X1 = ALOG10(X1)
          if (K1 .eq. 2)  X2 = ALOG10(X2)
          X(L,I) = X(L,I+1)
          A(L,I) = (Y(L,I+1) - Y(L,I))/(X2 - X1)
          B(L,I) = -A(L,I)*X1 + Y(L,I)
        end do
      end do
C
      call STEPG(K1, NN, TSTEP, NT, XMIN, XMAX, T, NSTEP)
C
      do L = 1,NC
        M = NV(L) - 1
        do I = 1,NSTEP
          do J = 1,M
            if (T(I) .lt. X(L,J))  go to  31
          end do
          J = M
   31     TT = T(I)
          if (K1 .eq. 2) TT = ALOG10(TT)
          V(L,I) = A(L,J)*TT  + B(L,J)
        end do
      end do
      return
      end