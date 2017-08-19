C*********************************************************************
      subroutine STEPG(KK, NN, TSTEP, NT, T1, TN, T, NSTEP)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  *
C
C   THE ROUTINE GENERATES STEPS IN LINEAR OR LOGARITHMIC INCREMENT
C
C        KK      = SWITCH    KK = 1    STEP INCREASE OF VALUES
C                            KK = 2    LOGARITHMIC INCREASE OF VALUES
C        NN      = NUMBER OF STEPS   OR NUMBER OF VALUES IN EACH 10
C        TSTEP   = LARGEST VALUE IN EACH STEP
C        NT      = NUMBER OF VALUES IN EACH STEP
C        T1      = FIRST VALUE IN LOG-STEP
C        TN      = LAST  VALUE IN LOG-STEP
C        T       = VALUES GENERATED
C        NSTEP   = NUMBER OF VALUES
C
C   CODED PER B SCHNABEL SEPT. 1970
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  *
C
      dimension T(200), TSTEP(27), NT(27)
C
      if (KK .eq. 1) then
        K = 1
        T(K) = 0.
        SAVE = 0.
        do N = 1,NN
          M = NT(N)
          STEP = (TSTEP(N) - SAVE)/FLOAT(M)
          SAVE = TSTEP(N)
          do I = 1,M
            K = K + 1
            T(K) = T(K-1) + STEP
          end do
        end do
        NSTEP = K
        return
      else if (KK .eq. 2) then
        NST = ALOG10(T1)
        if (T1.lt. 1.) NST = NST - 1
        STEP = 1./NN
        K = 1
        TA = 10.**FLOAT(NST)
        T(1) = TA
        do J = 2,NN
          K = K + 1
          T(K) = TA*10.**(STEP*FLOAT(J))
          if (T(K) .gt. T1) go to 221
        end do
  221   TA = T(K-1)
        K = 0
  211   do J = 1,NN
          K = K + 1
          T(K) = TA*10.**(STEP*FLOAT(J))
          if (T(K) .gt. TN)  go to 212
        end do
        TA = TA*10.
        go to 211
  212   NSTEP = K
        return
      end if
      end