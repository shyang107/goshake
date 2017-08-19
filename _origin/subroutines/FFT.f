C    ****************************************
      subroutine FFT (A,M,INV,S,IFSET,IFERR)
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C     TODO: Check dimensions on these arrays
      dimension A(1),INV(1),S(1),N(3),M(3),NP(3),W(2),W2(2),W3(2)
      equivalence (N1,N(1)), (N2,N(2)), (N3,N(3))

      M1=M(1)
      M2=M(2)
      M3=M(3)
      MTT=M1-2
      MT=MAX0(2,MTT)
      NT=2**MT
10    if (IABS(IFSET)-1) 610,610,20
610   MT=MAX0(M(1),M(2),M(3))-2
      MT=MAX0(2,MT)
      if (MT-20) 630,630,620
620   IFERR=1
      go to 600
630   IFERR=0
      NT=2**MT
      NTV2=NT/2
      THETA=.7853981634
      JSTEP=NT
      JDIF=NTV2
      S(JDIF)=SIN(THETA)
      do L=2,MT
        THETA=THETA/2.
        JSTEP2=JSTEP
        JSTEP=JDIF
        JDIF=JSTEP/2
        S(JDIF)=SIN(THETA)
        JC1=NT-JDIF
        S(JC1)=COS(THETA)
        JLAST=NT-JSTEP2
        if (JLAST-JSTEP .ge. 0) then
          do J=JSTEP,JLAST,JSTEP
            JC=NT-J
            JD=J+JDIF
            S(JD)=S(J)*S(JC1)+S(JDIF)*S(JC)
          end do
        end if
      end do
C
C     SET UP INV(J) TABLE
      MTLEXP=NTV2
      LM1EXP=1
      INV(1)=0
      do L=1,MT
        INV(LM1EXP+1)=MTLEXP
        do J=2,LM1EXP
          JJ=J+LM1EXP
          INV(JJ)=INV(J)+MTLEXP
        end do
        MTLEXP=MTLEXP/2
        LM1EXP=LM1EXP*2
      end do
      if (IFSET .eq. 0) then
        return
      end if
20    MTT=MAX0(M(1),M(2),M(3))-2
      ROOT2=SQRT(2.)
      if (MTT-MT .gt. 0) then
        IFERR=1
        write(6,1000)
        stop
1000    format(31H --- ERROR IN FOURIER TRANSFORM  )
      end if
      IFERR=0
C     M1=M(1)
C     M2=M(2)
C     M3=M(3)
      N1=2**M1
      N2=2**M2
      N3=2**M3
      if (IFSET .le. 0) then
        NX=N1*N2*N3
        FN=NX
        do I=1,NX
          A(2*I-1)=A(2*I-1)/FN
          A(2*I)=-A(2*I)/FN
        end do
      end if
      NP(1)=N1*2
      NP(2)=NP(1)*N2
      NP(3)=NP(2)*N3
      do ID=1,3
        IL=NP(3)-NP(ID)
        IL1=IL+1
        MI=M(ID)
        if (MI) 330,330,80
80      IDIF=NP(ID)
        KBIT=NP(ID)
        MEV=2*(MI/2)
        if (MI-MEV .gt. 0) then
          KBIT=KBIT/2
          KL=KBIT-2
          do I=1,IL1,IDIF
            KLAST=KL+I
            do K=I,KLAST,2
              KD=K+KBIT
              T=A(KD)
              A(KD)=A(K)-T
              A(K)=A(K)+T
              T=A(KD+1)
              A(KD+1)=A(K+1)-T
              A(K+1)=A(K+1)+T
            end do
          end do
          if (MI-1 .gt. 0) then
            LFIRST=3
            JLAST=1
          else
            go to 330
          end if
        else
120       LFIRST=2
          JLAST=0
        end if
130     do 320 L=LFIRST,MI,2
        JJDIF=KBIT
        KBIT=KBIT/4
        KL=KBIT-2
        do I=1,IL1,IDIF
          KLAST=I+KL
          do K=I,KLAST,2
            K1=K+KBIT
            K2=K1+KBIT
            K3=K2+KBIT
            T=A(K2)
            A(K2)=A(K)-T
            A(K)=A(K)+T
            T=A(K2+1)
            A(K2+1)=A(K+1)-T
            A(K+1)=A(K+1)+T
            T=A(K3)
            A(K3)=A(K1)-T
            A(K1)=A(K1)+T
            T=A(K3+1)
            A(K3+1)=A(K1+1)-T
            A(K1+1)=A(K1+1)+T
            T=A(K1)
            A(K1)=A(K)-T
            A(K)=A(K)+T
            T=A(K1+1)
            A(K1+1)=A(K+1)-T
            A(K+1)=A(K+1)+T
            R=-A(K3+1)
            T=A(K3)
            A(K3)=A(K2)-R
            A(K2)=A(K2)+R
            A(K3+1)=A(K2+1)-T
            A(K2+1)=A(K2+1)+T
          end do
        end do
        if (JLAST) 310,310,150
150     JJ=JJDIF+1
        ILAST=IL+JJ
        do I=JJ,ILAST,IDIF
          KLAST=KL+I
          do K=I,KLAST,2
            K1=K+KBIT
            K2=K1+KBIT
            K3=K2+KBIT
            R=-A(K2+1)
            T=A(K2)
            A(K2)=A(K)-R
            A(K)=A(K)+R
            A(K2+1)=A(K+1)-T
            A(K+1)=A(K+1)+T
            AWR = A(K1)-A(K1+1)
            AWI=A(K1+1)+A(K1)
            R=-A(K3)-A(K3+1)
            T=A(K3)-A(K3+1)
            A(K3)=(AWR-R)/ROOT2
            A(K3+1)=(AWI-T)/ROOT2
            A(K1)=(AWR+R)/ROOT2
            A(K1+1)=(AWI+T)/ROOT2
            T=A(K1)
            A(K1)=A(K)-T
            A(K)=A(K)+T
            T=A(K1+1)
            A(K1+1)=A(K+1)-T
            A(K+1)=A(K+1)+T
            R=-A(K3+1)
            T=A(K3)
            A(K3)=A(K2)-R
            A(K2)=A(K2)+R
            A(K3+1)=A(K2+1)-T
            A(K2+1)=A(K2+1)+T
          end do
        end do
        if (JLAST-1) 310,310,170
170     JJ=JJ+JJDIF
        do J=2,JLAST
          I=INV(J+1)
          IC=NT-I
          W(1)=S(IC)
          W(2)=S(I)
          I2=2*I
          I2C=NT-I2
          if (I2C) 200,190,180
180       W2(1)=S(I2C)
          W2(2)=S(I2)
          go to 210
190       W2(1)=0.
          W2(2)=1.
          go to 210
200       I2CC=I2C+NT
          I2C=-I2C
          W2(1)=-S(I2C)
          W2(2)=S(I2CC)
210       I3=I+I2
          I3C=NT-I3
          if (I3C) 240,230,220
220       W3(1)=S(I3C)
          W3(2)=S(I3)
          go to 280
230       W3(1)=0.
          W3(2)=1.
          go to 280
240       I3CC=I3C+NT
          if (I3CC) 270,260,250
250       I3C=-I3C
          W3(1)=-S(I3C)
          W3(2)=S(I3CC)
          go to 280
260       W3(1)=-1.
          W3(2)=0.
          go to 280
270       I3CCC=NT+I3CC
          I3CC=-I3CC
          W3(1)=-S(I3CCC)
          W3(2)=-S(I3CC)
280       ILAST=IL+JJ
          do I=JJ,ILAST,IDIF
            KLAST=KL+I
            do K=I,KLAST,2
              K1=K+KBIT
              K2=K1+KBIT
              K3=K2+KBIT
              R=A(K2)*W2(1)-A(K2+1)*W2(2)
              T=A(K2)*W2(2)+A(K2+1)*W2(1)
              A(K2)=A(K)-R
              A(K)=A(K)+R
              A(K2+1)=A(K+1)-T
              A(K+1)=A(K+1)+T
              R=A(K3)*W3(1)-A(K3+1)*W3(2)
              T=A(K3)*W3(2)+A(K3+1)*W3(1)
              AWR=A(K1)*W(1)-A(K1+1)*W(2)
              AWI=A(K1)*W(2)+A(K1+1)*W(1)
              A(K3)=AWR-R
              A(K3+1)=AWI-T
              A(K1)=AWR+R
              A(K1+1)=AWI+T
              T=A(K1)
              A(K1)=A(K)-T
              A(K)=A(K)+T
              T=A(K1+1)
              A(K1+1)=A(K+1)-T
              A(K+1)=A(K+1)+T
              R=-A(K3+1)
              T=A(K3)
              A(K3)=A(K2)-R
              A(K2)=A(K2)+R
              A(K3+1)=A(K2+1)-T
              A(K2+1)=A(K2+1)+T
            end do
          end do
          JJ=JJDIF+JJ
        end do
310     JLAST=4*JLAST+3
320     continue
330     continue
      end do
      NTSQ=NT*NT
      M3MT=M3-MT
      if (M3MT) 350,340,340
340   IGO3=1
      N3VNT=N3/NT
      MINN3=NT
      go to 360
350   IGO3=2
      N3VNT=1
      NTVN3=NT/N3
      MINN3=N3
360   JJD3=NTSQ/N3
      M2MT=M2-MT
      if (M2MT) 380,370,370
370   IGO2=1
      N2VNT=N2/NT
      MINN2=NT
      go to 390
380   IGO2=2
      N2VNT=1
      NTVN2=NT/N2
      MINN2=N2
390   JJD2=NTSQ/N2
      M1MT=M1-MT
      if (M1MT) 410,400,400
400   IGO1=1
      N1VNT=N1/NT
      MINN1=NT
      go to 420
410   IGO1=2
      N1VNT=1
      NTVN1=NT/N1
      MINN1=N1
420   JJD1=NTSQ/N1
      JJ3=1
      J=1
      do JPP3=1,N3VNT
        IPP3=INV(JJ3)
        do JP3=1,MINN3
          go to (430,440), IGO3
430       IP3=INV(JP3)*N3VNT
          go to 450
440       IP3=INV(JP3)/NTVN3
450       I3=(IPP3+IP3)*N2
          JJ2=1
          do JPP2=1,N2VNT
            IPP2=INV(JJ2)+I3
            do JP2=1,MINN2
              go to (460,470), IGO2
460           IP2=INV(JP2)*N2VNT
              go to 480
470           IP2=INV(JP2)/NTVN2
480           I2=(IPP2+IP2)*N1
              JJ1=1
              do JPP1=1,N1VNT
                IPP1=INV(JJ1)+I2
                do JP1=1,MINN1
                  go to (490,500), IGO1
490               IP1=INV(JP1)*N1VNT
                  go to 510
500               IP1=INV(JP1)/NTVN1
510               I=2*(IPP1+IP1)+1
                  if (J-I) 520,530,530
520               T=A(I)
                  A(I)=A(J)
                  A(J)=T
                  T=A(I+1)
                  A(I+1)=A(J+1)
                  A(J+1)=T
530               continue
                  J=J+2
                end do
                JJ1=JJ1+JJD1
              end do
            end do
            JJ2=JJ2+JJD2
          end do
        end do
        JJ3=JJ3+JJD3
      end do
      if (IFSET) 580,600,600
580   do I=1,NX
        A(2*I)=-A(2*I)
      end do
600   return
      end