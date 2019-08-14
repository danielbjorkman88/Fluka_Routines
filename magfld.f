*$ CREATE MAGFLD.FOR
*COPY MAGFLD

*===magfld=============================================================*

      SUBROUTINE MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

************************************************************************
*                                                                      *
*     Implements a 2-dimension approximation of the ATLAS field map,   *
*     given as br, bphi and bz components (in T) in a binning of       *
*     10x10 cm covering the ATLAS cavern                               *
*                                                                      *
*     Input variables:                                                 *
*            X,Y,Z = current position                                  *
*            NREG  = current region                                    *
*     Output variables:                                                *
*            BTX,BTY,BTZ = cosines of the magn. field vector           *
*            B = magnetic field intensity (Tesla)                      *
*            IDISC = set to 1 if the particle has to be discarded      *
*                                                                      *
*     M. Huhtinen (CERN), 01.04.2015                                   *
*                                                                      *
************************************************************************

      DATA IFIRST /1234/
      LOGICAL LINTERPO
c     Using interpolation makes the routine ~20% slower 
c     (but think it's worth it)
      DATA LINTERPO /.TRUE./

      COMMON / MAGMAP / 
     &         ATMAPR(134,481),ATMAPZ(134,481),ATMAPPHI(134,481),
     &         RMAPMIN,RMAPMAX,ZMAPMIN,ZMAPMAX,DRMAP,DZMAP
      
      REAL*4 ATMAPR,ATMAPZ,ATMAPPHI

      IF (IFIRST.EQ.1234) THEN
         ZBORDER=0D0
         RBORDER=0D0
         
         IF (LINTERPO) THEN
            ZBORDER=DZMAP
            RBORDER=DRMAP
         ENDIF 

         IFIRST=0
      ENDIF
      
      IF (Z.LT.ZMAPMIN.OR.Z.GE.ZMAPMAX-ZBORDER) THEN
         BTX = 0.D+00
         BTY = 0.D+00
         BTZ = 1.D+00
         B   = 0.D+00
         RETURN
      ENDIF

      R = SQRT(X**2+Y**2)

      IF (R.LT.RMAPMIN.OR.R.GE.RMAPMAX-RBORDER) THEN
         BTX = 0.D+00
         BTY = 0.D+00
         BTZ = 1.D+00
         B   = 0.D+00
         RETURN
      ENDIF

      IRCELL = INT(R-RMAPMIN)/DRMAP+1
      IZCELL = INT(Z-ZMAPMIN)/DZMAP+1

      IF (LINTERPO) THEN
         RLOW   = RMAPMIN + IRCELL*DRMAP
         ZLOW   = ZMAPMIN + IZCELL*DZMAP
      
         BR00   = ATMAPR   (IRCELL,   IZCELL)
         BZ00   = ATMAPZ   (IRCELL,   IZCELL)
         BPHI00 = ATMAPPHI (IRCELL,   IZCELL)
         BR10   = ATMAPR   (IRCELL+1, IZCELL)
         BZ10   = ATMAPZ   (IRCELL+1, IZCELL)
         BPHI10 = ATMAPPHI (IRCELL+1, IZCELL)
         BR01   = ATMAPR   (IRCELL,   IZCELL+1)
         BZ01   = ATMAPZ   (IRCELL,   IZCELL+1)
         BPHI01 = ATMAPPHI (IRCELL,   IZCELL+1)
         BR11   = ATMAPR   (IRCELL+1, IZCELL+1)
         BZ11   = ATMAPZ   (IRCELL+1, IZCELL+1)
         BPHI11 = ATMAPPHI (IRCELL+1, IZCELL+1)

         BR0    = BR00   + (BR10-BR00)     * (R-RLOW) / DRMAP
         BR1    = BR01   + (BR11-BR01)     * (R-RLOW) / DRMAP
         BRR    = BR0    + (BR1-BR0)       * (Z-ZLOW) / DZMAP
         BZ0    = BZ00   + (BZ10-BZ00)     * (R-RLOW) / DRMAP
         BZ1    = BZ01   + (BZ11-BZ01)     * (R-RLOW) / DRMAP
         BZZ    = BZ0    + (BZ1-BZ0)       * (Z-ZLOW) / DZMAP
         BPHI0  = BPHI00 + (BPHI10-BPHI00) * (R-RLOW) / DRMAP
         BPHI1  = BPHI01 + (BPHI11-BPHI01) * (R-RLOW) / DRMAP
         BPHI   = BPHI0  + (BPHI1-BPHI0)   * (Z-ZLOW) / DZMAP
      
      ELSE
         BRR  = ATMAPR  (IRCELL,IZCELL)
         BZZ  = ATMAPZ  (IRCELL,IZCELL)
         BPHI = ATMAPPHI(IRCELL,IZCELL)
      
      ENDIF

      PHI = ATAN2(Y,X)

      BXX = BRR*COS(PHI) - BPHI*SIN(PHI)
      BYY = BRR*SIN(PHI) + BPHI*COS(PHI)

      B   = SQRT(BXX*BXX + BYY*BYY + BZZ*BZZ)
      BTX = BXX/B
      BTY = BYY/B
      BTZ = BZZ/B

      RETURN
      END


      SUBROUTINE MAGINI
*=======================================================================
* MAGnetic field INItialization
* To be called at USRINI.f routine          
*=======================================================================      
      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      
      COMMON / MAGMAP / 
     &         ATMAPR(134,481),ATMAPZ(134,481),ATMAPPHI(134,481),
     &         RMAPMIN,RMAPMAX,ZMAPMIN,ZMAPMAX,DRMAP,DZMAP

      REAL*4 ATMAPR,ATMAPZ,ATMAPPHI
      
      RMAPMIN = 0.D0
      RMAPMAX = 1330.D0
      ZMAPMIN = -2400.D0
      ZMAPMAX = 2400.D0
      DRMAP   = 10.D0
      DZMAP   = 10.D0

      OPEN(20,FILE='../magfld.txt',ERR=999,STATUS='OLD')
      DO 10 IZ=1,481
         DO 20 IR=1,134
            READ(20,*) IRCHK,IZCHK,ATMAPR(IR,IZ),ATMAPZ(IR,IZ),
     &                 ATMAPPHI(IR,IZ)
            IF (IRCHK.LT.RMAPMIN.OR.IRCHK.GT.RMAPMAX) THEN
               WRITE(99,*) ' *** Map r-value out of range ', IRCHK
               STOP
            ENDIF
            IF (IZCHK.LT.ZMAPMIN.OR.IZCHK.GT.ZMAPMAX) THEN
               WRITE(99,*) ' *** Map z-value out of range ', IZCHK
               STOP
            ENDIF
   20    CONTINUE
   10 CONTINUE
      
      WRITE(*,*) '*** Field map initialised ***'
      RETURN
  997 CONTINUE
      STOP ' *** Error reading magnetic field map     '
  998 CONTINUE
      STOP ' *** Unexpected end of magnetic field map '
  999 CONTINUE
      STOP ' *** Failed to open magnetic field map    ' 
      
      RETURN
      END


      FUNCTION RANMAR (IDUMMY)
*=======================================================================
* Universal random generator using lagged fibonacci method.
* Doc. given in: G.Marsaglia & al. Stat. Prob. Lett. 9 (1990) 35
*=======================================================================
c      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /RANDO/ U(97),C,CD,CM,I,J
      RANMAR = U(I)-U(J)
      IF ( RANMAR.LT.0.0 ) RANMAR = RANMAR+1.0
      U(I) = ranmar
      I    = I-1
      IF ( I.EQ.0 ) I = 97
      J    = J-1
      IF ( J.EQ.0 ) J = 97
      C    = C-CD
      IF ( C.LT.0.0 ) C = C+CM
      RANMAR = RANMAR-C
      IF ( RANMAR.LT.0.0 ) RANMAR = RANMAR+1.0
      
      RETURN
      END
 
 
      SUBROUTINE RMARIN (NA1,NA2,NA3,NB1)
*=======================================================================
* Initialization of random number generator
*=======================================================================
c      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /RANDO/ U(97),C,CD,CM,I,J
      MA1 = NA1
      MA2 = NA2
      MA3 = NA3
      MB1 = NB1
      I   = 97
      J   = 33
      DO 20 II2 = 1,97
         S = 0.0
         T = 0.5
         DO 10 II1 = 1,24
            MAT  = MOD(MOD(MA1*MA2,179)*MA3,179)
            MA1  = MA2
            MA2  = MA3
            MA3  = MAT
            MB1  = MOD(53*MB1+1,169)
            IF ( MOD(MB1*MAT,64).GE.32 ) S = S+T
10          T    = 0.5*T
20       U(II2) = S
      C  =   362436./16777216.
      CD =  7654321./16777216.
      CM = 16777213./16777216.
      
      RETURN
      END
