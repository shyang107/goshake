C*************************************
      subroutine XMX(X,MX,XMAX,NXMAX)
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C   THIS ROUTINE FIND MAX. VALUE, XMAX, AND NUMBER OF MAX. VALUE, NXMAX.
C   OF ARRAY X WITH MX NUMBER OF VALUES
C
C   CODED PER B SCHNABEL OCT. 1971
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
      dimension X(1)
      XMAX = 0.
      do I = 1,MX
        XA = ABS(X(I))
        if (XMAX .le. XA) then
          NXMAX = I
          XMAX = XA
        end if
      end do
      return
      end
