C**********************************************
      subroutine RFFT (A,M,INV,S,IFERR,IFSET)
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      dimension A(1), L(3), INV(1), S(1)
C
C     IFSET=1
      L(1)=M
      L(2)=0
      L(3)=0
      NTOT=2**M
      NTOT2=2*NTOT
      FN=NTOT
      do I=2,NTOT2,2
        A(I)=-A(I)
      end do
      do I=1,NTOT2
        A(I)=A(I)/FN
      end do
      call FFT (A,L,INV,S,IFSET,IFERR)
C
C     MOVE LAST HALF OF A(J)S DOWN ONE SLOT AND ADD A(N) AT BOTTOM TO
C     GIVE ARRAY FOR A1PRIME AND A2PRIME CALCULATION
C
      do I=1,NTOT,2
        J0=NTOT2+2-I
        A(J0)=A(J0-2)
        A(J0+1)=A(J0-1)
      end do
      A(NTOT2+3)=A(1)
      A(NTOT2+4)=A(2)
C
C     CALCULATE A1PRIMES AND STORE IN FIRST N SLOTS
C     CALCULATE A2PRIMES AND STORE IN SECOND N SLOTS IN REVERSE ORDER
C
      K0=NTOT+1
      do I=1,K0,2
        K1=NTOT2-I+4
        AP1RE=.5*(A(I)+A(K1))
        AP2RE=-.5*(A(I+1)+A(K1+1))
        AP1IM=.5*(-A(I+1)+A(K1+1))
        AP2IM=-.5*(A(I)-A(K1))
        A(I)=AP1RE
        A(I+1)=AP1IM
        A(K1)=AP2RE
        A(K1+1)=AP2IM
      end do
      NTO=NTOT/2
      NT=NTO+1
      DEL=3.1415927/FLOAT(NTOT)
      SS=SIN(DEL)
      SC=COS(DEL)
      SI=0.0
      CO=1.0
C
C     COMPUTE C(J)S FOR J=0 THRU J=N
      do I=1,NT
        K6=NTOT2-2*I+5
        AP2RE=A(K6)*CO+A(K6+1)*SI
        AP2IM=-A(K6)*SI+A(K6+1)*CO
        CIRE=.5*(A(2*I-1)+AP2RE)
        CIIM=.5*(A(2*I)+AP2IM)
        CNIRE=.5*(A(2*I-1)-AP2RE)
        CNIIM=.5*(A(2*I)-AP2IM)
        A(2*I-1)=CIRE
        A(2*I)=CIIM
        A(K6)=CNIRE
        A(K6+1)=-CNIIM
        SIS=SI
        SI=SI*SC+CO*SS
        CO=CO*SC-SIS*SS
      end do
C
C     SHIFT C(J)S FOR J=N/2+1 to J=N UP ONE SLOT
      do I=1,NTOT,2
        K8=NTOT+4+I
        A(K8-2)=A(K8)
        A(K8-1)=A(K8+1)
      end do
      do I=3,NTOT2,2
        A(I)=2.*A(I)
        A(I+1)= 2.*A(I+1)
      end do
      return
      end