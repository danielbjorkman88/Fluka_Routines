*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2006      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA200x:                                *
*                                                                      *
*     Created on 07 january 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 03-mar-06     by    Alfredo Ferrari               *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*  Read source term from a file                                        *
*     Author: Vasilis.Vlachoudis@cern.ch                               *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(BEAMCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(IOIOCM)'
      INCLUDE '(LTCLCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SOURCM)'
      INCLUDE '(SUMCOU)'
*
      LOGICAL LFIRST

      PARAMETER (NMAX=5000000)

*
      SAVE LFIRST
      DATA LFIRST / .TRUE. /

      CHARACTER*500 LINE,KK1,KK2,KK3,KK4,KK5,KK6,KK7,KK8
      INTEGER    NNN
      DIMENSION  XXX(NMAX), YYY(NMAX)
      DIMENSION  UUU(NMAX), VVV(NMAX)

      SAVE XXX, YYY
      SAVE UUU, VVV

*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.
*  |  *** User initialization ***

*  |  WHASOU(1) = UNIT number (File name is linked with OPEN)
         LUNRD  = NINT(WHASOU(1))
*  |  WHASOU(2) = sigma(cm) of randomness in posision
         SGMPOS = WHASOU(2)
*  |  WHASOU(3) = sigma(rad) of -//- in direction
         SGMANG = WHASOU(3)
         
        
         DIST=WHASOU(4)   

         WRITE (LUNOUT,*)
         WRITE (LUNOUT,*) " ** rdsource: SGMPOS=",SGMPOS
         WRITE (LUNOUT,*) " ** rdsource: SGMANG=",SGMANG
         WRITE (LUNOUT,*) " ** rdsource: DIST= ",DIST," cm"
         
         
         NNN = 0
 10      CONTINUE
            READ( LUNRD, '(A)', ERR=9999, END=20 ) LINE
*            WRITE(LUNOUT,*)LINE
            IF (LINE(1:1) .EQ. '*'.OR.LINE(1:1).EQ.'$'.OR.LINE(1:1)
     c      .EQ.'@') GO TO 10
            READ (LINE,*,ERR=10) 
     c      KK1,KK2,KK3,KK4,KK5,KK6,X, XP, Y, YP,KK7,KK8

*            print *,X,',',XP,',',Y,',',YP

* | increase counter
            NNN = NNN + 1
            IF (NNN.GT.NMAX) CALL FLABRT('SOURCE','Increase NMAX')

            XXX(NNN) = X * 100.D0 + XP*DIST
            YYY(NNN) = Y * 100.D0 + YP*DIST

* |  Find the direction cosines
*            XP = XP / 1000.D0
*            YP = YP / 1000.D0
            UVW = SQRT(XP**2 + YP**2 + ONEONE)
            UUU(NNN) = XP / UVW
            VVV(NNN) = YP / UVW

* | Energy in GeV * ERG(NNN) = E * 1.0D-3
         GOTO 10
 20      CONTINUE
         IF (NNN.EQ.0) CALL FLABRT('SOURCE','Error reading file')
         WRITE (LUNOUT,*) '*** rdsource: ',NNN,' particles loaded'
         WRITE (LUNOUT,*)
      END IF
*  |
*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
* Npflka is the stack counter: of course any time source is called it
* must be =0
      NPFLKA = NPFLKA + 1
*  +-------------------------------------------------------------------*
*  | Choose a random particle
      RNDSIG = FLRNDM (RNDSIG)
      N = INT(NNN*RNDSIG)+1
*  |
*  +-------------------------------------------------------------------*
* Wt is the weight of the particle
      WTFLK  (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
* Particle type (1=proton.....). Ijbeam is the type set by the BEAM
* card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID = IJBEAM
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
      END IF
*  |
*  +-------------------------------------------------------------------*

* Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
* User dependent flag:
      LOUSE  (NPFLKA) = 0
* User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE

* User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
* Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
* ... to this point: don't change anything
* Particle age (s)
      AGESTK (NPFLKA) = ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
* Group number for "low" energy neutrons, set to 0 anyway
      IGROUP (NPFLKA) = 0
* Kinetic energy of the particle (GeV)
      TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 ) - AM (IONID)
*      TKEFLK (NPFLKA) = ERG(N)
* Particle momentum
      PMOFLK (NPFLKA) = PBEAM
*      PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*     &                       + TWOTWO * AM (IONID) ) )

* Cosines (tx,ty,tz)
      IF (SGMANG.GT.ZERZER) THEN
         CALL FLNRR2(RU, RV)
         U = UUU(N) + RU * SGMANG
         V = VVV(N) + RV * SGMANG
      ELSE
         U = UUU(N)
         V = VVV(N)
      END IF
      W = SQRT (ONEONE - U**2 - V**2)
      UVW = SQRT(U**2 + V**2 + W**2)
      TXFLK  (NPFLKA) = U / UVW
      TYFLK  (NPFLKA) = V / UVW
      TZFLK  (NPFLKA) = W / UVW

* Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER

* Particle coordinates
* with randomness in position
      IF (SGMPOS.GT.ZERZER) THEN
         CALL FLNRR2(RX, RY)
         XFLK   (NPFLKA) = XBEAM + XXX(N) + RX*SGMPOS
         YFLK   (NPFLKA) = YBEAM + YYY(N) + RY*SGMPOS
      ELSE
         XFLK   (NPFLKA) = XBEAM + XXX(N)
         YFLK   (NPFLKA) = YBEAM + YYY(N)
      END IF
      ZFLK   (NPFLKA) = ZBEAM

*  Calculate the total kinetic energy of the primaries: don't change
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV
      RETURN
*=== End of subroutine Source =========================================*
 9999 CONTINUE
      CALL FLABRT('SOURCE','Error reading source file')
      END
