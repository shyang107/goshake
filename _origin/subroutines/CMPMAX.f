C  ********************************************************************
      subroutine CMPMAX (KUG,PR,W,W2,W3,WD,D,DT,ZD,ZV,ZA, UG)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C   THIS ROUTINE COMPUTES RESPONSE VALUES FOR ONE SINGLE DEGREE OF
C   FREEDOM SYSTEM USING STEP BY STEP METHOD
C
C   EXPLANATIOS to PARAMETERS GIVEN IN DRCTSP
C
C   CODED BY I. M. IDRISS  1967
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C     TODO: These arrays need to be checked for size
      dimension XD(2), XV(2), T(3)
      dimension UG(1)
C
      ZA = 0.
      ZD = 0.
      ZV = 0.
      XD(1) = 0.
      XV(1) = 0.
      F1 = 2.*D/(W3*DT)
      F2 = 1./W2
      F3 = D*W
      F4 = 1./WD
      F5 = F3*F4
      F6 = 2.*F3
       E = EXP(-F3*DT)
       S = SIN(WD*DT)
       C= COS(WD*DT)
      G1 = E*S
      G2 = E*C
      H1 = WD*G2 - F3*G1
      H2 = WD*G1 + F3*G2
      do K = 1, KUG
        Y = K-1
        DUG = UG(K+1) - UG(K)
        Z1 = F2*DUG
        Z2 = F2*UG(K)
        Z3 = F1*DUG
        Z4 = Z1/DT
        B = XD(1) + Z2 -Z3
        A = F4*XV(1) + F5*B + F4*Z4
        XD(2) = A*G1 + B*G2 + Z3 - Z2 - Z1
        XV(2) = A*H1 - B*H2 - Z4
        XD(1) = XD(2)
        XV(1) = XV(2)
        AA = -F6*XV(1) - W2*XD(1)
        F = ABS(XD(1))
        G = ABS(XV(1))
        H = ABS(AA)
        if (F .gt. ZD) then
          T(1) = Y
          ZD = F
        end if
        if (G .gt. ZV) then
          T(2) = Y
          ZV = G
        end if
        if (H .gt. ZA) then
          T(3) = Y
          ZA = H
        end if
      end do
      do L = 1, 3
        T(L) = DT*T(L)
      end do
      write(6,112) PR, (T(L),L=1,3)
  112 format(5X,5HPER = F5.2,5X,19HTIMES FOR MAXIMA -- ,3X,
     14HTD = F8.4,3X,4HTV = F8.4,3X,4HTA = F8.4)
      return
      end