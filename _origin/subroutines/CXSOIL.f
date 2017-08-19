C*************************************************
      subroutine CXSOIL(N1)
C***********************************************************************
C
C   THIS ROUTINE CALCULATES THE complex SOIL PROPERTIES AND TRANSFER
C   FUNCTIONS FOR THE LAYERS
C
C        N1      = NUMBER OF SOIL LAYERS
C        BL      = RATIO OF CRITICAL DAMPING
C        GL      = SHEAR MODULUS
C        R       = DENSITY
C        G       = complex SHEAR MODULUS
C        V       = complex SHEAR WAVE VELOCITY
C        PLUS    = complex TRANSFER FUNCTION
C        MINUS   = complex TRANSFER FUNCTION
C
C   CODED BY PER B SCHNABEL OCT 1971
C
C***********************************************************************
C
      complex G, V, PLUS, MINUS, MU
      character*6 IDNT
      common /SOILA/ IDNT(6),BL(51),GL(51),FACT(51),H(51),R(51),BF(51)
      common /CSOIL/ G(51), V(51), PLUS(51), MINUS(51)
C
      N = N1 + 1
      do 1 I = 1,N
      GIMAG=2.*BL(I)*GL(I)*SQRT(1.-BL(I)*BL(I))
      GREAL=GL(I)*(1.-2.*BL(I)*BL(I))
      G(I)=CMPLX(GREAL,GIMAG)
      V(I) = CSQRT(G(I)/R(I))
    1 continue
      do 2 I = 1,N1
      J = I + 1
      MU = CSQRT(R(I)/R(J)*G(I)/G(J))
      PLUS(I) = (1. + MU)/2.
      MINUS(I)= (1. - MU)/2.
    2 continue
      return
      end