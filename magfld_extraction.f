*$ CREATE MAGFLD.FOR
*COPY MAGFLD
*
*===magfld=============================================================*
*
      SUBROUTINE MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Created  in 1988    by     Alberto Fasso`, CERN - TIS            *
*                                                                      *
*     Last change on 20-may-96     by    Alfredo Ferrari               *
*                                                                      *
*     Input variables:                                                 *
*            x,y,z = current position                                  *
*            nreg  = current region                                    *
*     Output variables:                                                *
*            btx,bty,btz = cosines of the magn. field vector           *
*            B = magnetic field intensity (Tesla)                      *
*            idisc = set to 1 if the particle has to be discarded      *
*                                                                      *
*                                                                      *
*     A.Mereghetti, 21/05/2010                                         *
*     routine for linking a magnetic field to a specific replica       *
*       (originally received from M.Mauri for the LHC Phase 1 UPGRADE  *
*       simulations, re-adapted for the LineBuilder)                   *
*                                                                      *
*     basic assumptions:                                               *
*     - the magnetic settings are assigned to a replica by means of the*
*       lattice number (unique for any element). They are stored in    *
*       arrays declared in the maginfo.inc COMMON (filled by means of  *
*       the USRGCALL cards/ usrglo.f user routine at initialisation),  *
*       thus the needed information can be accessed by means of the    *
*       lattice number;                                                *
*     - the ROT-DEFI (needed in order to reconstruct the position of   *
*       the particle inside the element, i.e. in the local reference   *
*       system) is automatically identified through the LATTICE region;*
*       in this way, there is no need to save its index in the         *
*       maginfo.inc COMMON;                                            *
*     - LATTICE name and REGION name MUST be the same;                 *
*                                                                      *
*     Nota Bene:                                                       *
*     - if the particle is not inside a LATTICE, the routine will      *
*       return a NULL magnetic field, i.e. the default values:         *
*         BTX   = ZERZER                                               *
*         BTY   = ZERZER                                               *
*         BTZ   = ONEONE                                               *
*         B     = ZERZER                                               *
*     - the present routine deals with only analytical expressions     *
*       for magnetic fields: no support for handling maps is given     *
*       at the moment; for the moment, no solenoidal field is          *
*       implemented;                                                   *
*     - LDEBUG is a flag for activating the debug mode, i.e. the       *
*       position and the magnetic field thus computed are dumped into  *
*       a file at EACH call of the routine: thus, pay attention to disk*
*       space!!                                                        *
*                                                                      *
*     useful routines in FLUKA:                                        *
*                                                                      *
*                                                           LATTICE    *
*     - GEON2L( NAME, LATT_REG, IRTDUM, IR2DUM, IERR )                 *
*          from NAME to LATT_REG (lattice number)                      *
*     - GEOL2N( I, LATNAM, IRTDUM, IR2DUM, IERR )                      *
*          from I (lattice number) to LATNAM (lattice name)            *
*                                                                      *
*                                                           REGION     *
*     - GEON2R( REGNAM, NREG, IERR)                                    *
*          from REGNAM to NREG (region number)                         *
*     - GEOR2N( NREG, REGNAM, IERR)                                    *
*          from NREG to REGNAM (region name)                           *
*                                                                      *
*                                                           ROTDEFI    *
*     - DOTRSF ( 1, SB (1), SB (2), SB (3), KROTAT ):                  *
*          transform SB position array from real to transformed,       *
*                via the transformation identified by KROTAT index;    *
*     - DORTNO ( 1, UB (1), UB (2), UB (3), KROTAT )                   *
*          transform UB direction array from real to transformed,      *
*                via the transformation identified by KROTAT index;    *
*     - UNDRTO ( 1, UN (1), UN (2), UN (3), KROTAT )                   *
*          transform back normalised UN direction array from           *
*                transformed to real via the transformation identified *
*                by KROTAT index;                                      *
*                                                                      *
*     NB: IERR=1 means error;                                          *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(LTCLCM)'
      INCLUDE '(RTDFCM)'
      INCLUDE 'maginfo.inc'
*     needed by electric field mimicking
      INCLUDE '(CSMCRY)'
      INCLUDE '(TRACKR)'
      INCLUDE '(PAPROP)'
      INCLUDE '(FHEAVY)'
*     ZS active regions and rotation
      SAVE NSEP1, NSEP2, NSEP3, KROTAT

*     Modulate QFA/QFD magnetic field according the particle NUMBER
      LOGICAL LQFLDMOD
      SAVE LQFLDMOD
      DATA LQFLDMOD / .FALSE. / 
*     QFA/QDA active regions and modulation parameters
      SAVE NQF, NQD
*     QF current changes from 1776 to 1781 amps during the extraction
*     QD current changes from 1772 to 1774 amps
      PARAMETER (QF_CURR_MIN  = 1776.0D+00)
      PARAMETER (QF_CURR_MAX  = 1781.0D+00)
      PARAMETER (QF_CURR_MEAN = ((QF_CURR_MIN+QF_CURR_MAX)/TWOTWO) )
      PARAMETER (QD_CURR_MIN  = 1772.0D+00)
      PARAMETER (QD_CURR_MAX  = 1774.0D+00)
      PARAMETER (QD_CURR_MEAN = ((QD_CURR_MIN+QD_CURR_MAX)/TWOTWO) )

*     particles simulated in Mad-X
      PARAMETER (N_MADX = 2745000)
      
      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /

      CHARACTER RUTNAM*6, RUTSAV*10, RUTAUT*20
      SAVE RUTNAM, RUTSAV, RUTAUT
      DATA RUTNAM / 'MAGFLD' /
      DATA RUTSAV / '17/09/2014' /
      DATA RUTAUT / 'A. Mereghetti' /

*     debug mode
      LOGICAL LDEBUG
      SAVE LDEBUG
      DATA LDEBUG / .FALSE. /
      CHARACTER*20 DEBUG_FILE_NAME
      DATA DEBUG_FILE_NAME / 'magcalls.dat' /
      INTEGER LUN_DBUG
      DATA LUN_DBUG / 79 /

*     run-time variables
      CHARACTER*8 LATNAM
      CHARACTER*10 ROTNAME
      DIMENSION P( 3 ), P_TRANSV( 3 ), BB( 4 )
      LOGICAL LABORT

*     default values of returned variables
      IDISC = 0
      BTX   = ZERZER
      BTY   = ZERZER
      BTZ   = ONEONE
      B     = ZERZER

*----------------------------------------------------------------------*
*     first call                                                       *
*----------------------------------------------------------------------*

      IF ( LFIRST ) THEN

*        dump a tag about the routine:
         CALL TAGRUT( RUTNAM, RUTSAV, RUTAUT )

*        finalise user initialisation:
*        - main bends:
         BTHETA  = TWOTWO * PIPIPI / DBLE( NBENDs )
         BR      = ( BLENGTH / TWOTWO ) / SIN( BTHETA / TWOTWO )
*        - radial limit on magnetic field calculation:
         IF ( RMAX .GE. ZERZER ) LIMIBOR = .TRUE.
*        - lattices:
         IF ( RMAX .NE. ZERZER ) THEN
            DO IMLATTC=1,MXELMTS
               IF ( R_FLD_TYPE( IMLATTC ) .GT. -1 ) THEN
                  IF ( R_FLD_INTN( 1, IMLATTC ) .NE. ZERZER .OR.
     &                 R_FLD_INTN( 2, IMLATTC ) .NE. ZERZER ) THEN
*
*                    a magnetic field is applied
                     LMAGLT( IMLATTC ) = .TRUE.
*
*                    is the present lattice a twin-bores element?
                     RRR = SQRT( R_D(1,IMLATTC)**2 + R_D(2,IMLATTC)**2 + 
     &                           R_D(3,IMLATTC)**2 )
                     IF ( RRR .GT. ZERZER ) R_TWIN_BORE(IMLATTC)=.TRUE.
*
*                    magfield with additional rotation about the bore axis:
                     IF ( R_FLD_ANGL( 1, IMLATTC ) .NE. ZERZER ) THEN
                        LUSEAN( 1, IMLATTC ) = .TRUE.
                     ENDIF
                     IF ( R_TWIN_BORE(IMLATTC) .AND. 
     &                       R_FLD_ANGL( 2, IMLATTC ) .NE. ZERZER ) THEN
                        LUSEAN( 2, IMLATTC ) = .TRUE.
                     ENDIF
*
*                    is the magnetic field actually offcentred wrt beam pipe?
                     RRR = SQRT( R_B(1,IMLATTC)**2 + R_B(2,IMLATTC)**2 + 
     &                           R_B(3,IMLATTC)**2 )
                     IF ( RRR .GT. ZERZER ) LUSERB( IMLATTC ) = .TRUE.

                  ENDIF
               ENDIF
            ENDDO
         ENDIF

*        initialisation messages:
         IF ( LIMIBOR ) THEN
            WRITE(LUNOUT,*) '      max radius [cm]: ',RMAX
         ENDIF
         WRITE(LUNOUT,*) ''
         WRITE(LUNOUT,*) '      treatment of main bending magnets:'
         WRITE(LUNOUT,*) '      . number:                   ', NBENDs
         WRITE(LUNOUT,*) '      . straight length [cm]:     ', BLENGTH
         WRITE(LUNOUT,*) '      . theta [rad]:              ', BTHETA
         WRITE(LUNOUT,*) '      . radius of curvature [cm]: ', BR
         WRITE(LUNOUT,*) ''

*        magnetic element:
         WRITE(LUNOUT,*) ''
         WRITE(LUNOUT,*) '      echo of settings of magnetic elements:'
         LABORT = .FALSE.
         DO IMLATTC=1,MXELMTS
            IF ( LMAGLT( IMLATTC ) ) THEN
               CALL DUMP_MAGNFO( IMLATTC )
               CALL CHCK_FLKMAG( IMLATTC, LATNAM, IRCELL, IROTDEFI, 
     &                           ROTNAME, IERRR )
               IF ( IERRR .EQ. 1 ) THEN
                  WRITE (LUNOUT,*) "No LATTICE name",
     &" corresponding to MLATTC ",MLATTC
                  LABORT = .TRUE.
               ELSEIF ( IERRR .EQ. 2 ) THEN
                  WRITE (LUNOUT,*) "No region name corresponding to:"
                  WRITE (LUNOUT,*) "  LATTICE number: ", MLATTC
                  WRITE (LUNOUT,*) "  LATTICE name:   ", LATNAM
                  LABORT = .TRUE.
               ELSE
*                 save transformation index:
                  IROTMG( IMLATTC ) = IROTDEFI
*                 verify if the current lattice is subject to an actual
*                   rotation or to a simple translation:
                  DO II=1,3
                     IF ( RTMTRX( II, II, IROTDEFI ) .NE. ONEONE ) THEN
                        LISTLT( IMLATTC ) = .TRUE.
                        GOTO 1984
                     ENDIF
                  ENDDO
 1984             CONTINUE
                  CALL DUMP_FLKMAG( IMLATTC, LATNAM, IRCELL,
     &                   IROTMG( IMLATTC ), ROTNAME, LISTLT( IMLATTC ) )
               ENDIF
               WRITE (LUNOUT,*) 
     &"----------------------------------------------"
            ENDIF
         ENDDO
         WRITE(LUNOUT,*) ''
         IF ( LABORT ) THEN
            CALL FLABRT(RUTNAM, 'problem with magnetic settings')
         ENDIF

*        debug mode:
         IF ( LDEBUG ) THEN
            WRITE(LUNOUT,*) '      debug mode: ACTIVE'
            CALL OAUXFI( DEBUG_FILE_NAME, LUN_DBUG, 'NEW', IERR )
            IF ( IERR .GT. 0 ) THEN
               CALL FLABRT(RUTNAM,'Error opening file '
     &//DEBUG_FILE_NAME)
            ENDIF
            WRITE(LUN_DBUG,1981) 'LATNAM', 'MLATTC',
     &'P_TRANSV(1)', 'P_TRANSV(2)', 'P_TRANSV(3)', 'X', 'Y', 'Z', 
     &'BTX', 'BTY', 'BTZ', 'B'
            WRITE(LUNOUT,*) '         data will be printed in file '//
     &DEBUG_FILE_NAME//' on unit',LUN_DBUG
         ENDIF

* identify regions where the electric field equivalent is active
         CALL GEON2R( 'ZSvacEFa', NSEP1, IERR)
         IF(IERR.EQ.1) CALL FLABRT ('MAGFLD','ZSvacEFa not found')
         CALL GEON2R( 'ZSvacEFb', NSEP2, IERR)
         IF(IERR.EQ.1) CALL FLABRT ('MAGFLD','ZSvacEFb not found')
         CALL GEON2R( 'ZSvacEFc', NSEP3, IERR)
         IF(IERR.EQ.1) CALL FLABRT ('MAGFLD','ZSvacEFc not found')
*     and the relevant ROT-DEFI number for rotation purposes
*     only a single ROT-DEFI is selected since all the electrostatic septa
*     are rotated by the same angle
         CALL GEON2L ( 'ZS21633 ', IDONOTCARE, KROTAT, IRTLT2, IERR )
         IF(IERR.EQ.1) CALL FLABRT ('MAGFLD','ZS21633 not found')
         IF (IRTLT2.NE.0) CALL FLABRT ('MAGFLD',
     &                  'Two transformations ?!')         
*
         WRITE(LUNOUT,*) "A single ROT-DEFI is picked up for ZS, since"
         WRITE(LUNOUT,*) "all ZS's are rotated by the same angle"
         WRITE(LUNOUT,*) ""

*     identify active regions of QFA/QDA
         IF( LQFLDMOD ) THEN
            CALL GEON2R( 'MQE__BPI', NQF, IERR)
            IF(IERR.EQ.1) CALL FLABRT ('MAGFLD','MQE__BPI not found')
            CALL GEON2R( 'MQO__BPI', NQD, IERR)
            IF(IERR.EQ.1) CALL FLABRT ('MAGFLD','MQO__BPI not found')
            WRITE(LUNOUT,*) "QFA/QDA field modulation"
            WRITE(LUNOUT,*) "QFA current min,max,mean: ", QF_CURR_MIN,
     &           QF_CURR_MAX, QF_CURR_MEAN
            WRITE(LUNOUT,*) "QFD current min,max,mean: ", QD_CURR_MIN,
     &           QD_CURR_MAX, QD_CURR_MEAN
            WRITE(LUNOUT,*) "N_MADX", N_MADX
            WRITE(LUNOUT,*) ""
         ENDIF
         
         CALL FFLUSH
         CALL FLUSH(LUNOUT)
         CALL FLUSH(LUNERR)

         LFIRST = .FALSE.
      ENDIF

*----------------------------------------------------------------------*
*     core of the routine                                              *
*----------------------------------------------------------------------*

      IF(NREG.EQ.NSEP1.OR.NREG.EQ.NSEP2.OR.NREG.EQ.NSEP3) THEN
***** Electric field approximation
***** by Daniel Björkman and friends, CERN Radiation Protection 2017      

         BTX = ZERZER
         EFIELD = 11.D6         ! [V/m]

         IF ( JTRACK .GE. -6 ) THEN
            AMASS = AM ( JTRACK )
         ELSE
            AMASS = AMNHEA (-JTRACK )
         ENDIF

         GAM = ETRACK/AMASS
         BETA = SQRT (ONEONE - ONEONE/(GAM**2))
         VEL = BETA * CLIGHT /1.D2 ! [m/s]

* going to the prototype where the electrical field is directed along x

         ACPADX = CXTRCK
         ACPADY = CYTRCK
         ACPADZ = CZTRCK
         CALL DORTNO ( 1, ACPADX, ACPADY, ACPADZ, KROTAT )

* Each particle will be missing a max energy equal to its net charge
* times 220keV
* since we take into account only the E component
* orthogonal to the particle velocity
      
         IF(ACPADX.GE.ONEMNS) THEN
            BTY = ZERZER
            BTZ = ONEONE
            B = ZERZER         
         ELSE
            BTY = - ACPADZ / SQRT(ACPADY**2+ACPADZ**2)
            BTZ = ACPADY / SQRT(ACPADY**2+ACPADZ**2)
            B = EFIELD / VEL * SQRT(ONEONE-ACPADX**2) ! [T]
         END IF

         CALL UNDRTO ( 1, BTX, BTY, BTZ, KROTAT )
      
         RETURN
      END IF
      
*     store the lattice number in a regular INTEGER*4 variable, to prevent
*       the compiler from complaining about data format:
      IMLATTC = MLATTC

      IF ( IMLATTC .GT. 0 .AND. LMAGLT( IMLATTC ) ) THEN

*        get the ROTDEFI associated to the current LATTICE region:
         IROTDEFI = IROTMG( IMLATTC )

*        go to prototype:
         IF ( LISTLT( IMLATTC ) ) THEN
            P(1) = X
            P(2) = Y
            P(3) = Z
            CALL DOTRSF ( 1, P(1), P(2), P(3), IROTDEFI )
         ELSE
            P(1) = X + RTOFST( 1, IROTDEFI )
            P(2) = Y + RTOFST( 2, IROTDEFI )
            P(3) = Z + RTOFST( 3, IROTDEFI )
         ENDIF

*        get the position on the transverse plane wrt the centre of BP
*        . get bore index:
         IF ( R_TWIN_BORE( IMLATTC ) ) THEN
            CALL GET__IBORE( IMLATTC, P, IBORE )
         ELSE
            IBORE = 1
         ENDIF
*        . get actual transverse position:
         CALL GET__P_TRANSV( IMLATTC, P, IBORE, P_TRANSV )

*        limitation on R:
         IF ( LIMIBOR ) THEN
            RRR = SQRT( P_TRANSV(1)**2 + P_TRANSV(2)**2 )
            IF ( RRR .GT. RMAX ) THEN
               BTX = ZERZER
               BTY = ZERZER
               BTZ = ONEONE
               B   = ZERZER
               IF ( LDEBUG ) THEN
*                 keep track of values for debug printout:
                  BTX_DBG = BTX
                  BTY_DBG = BTY
                  BTZ_DBG = BTZ
               ENDIF
               GOTO 1983
            ENDIF
         ENDIF 

*        field intensity (T, T/m, T/m2 ...):
         B = R_FLD_INTN( IBORE, IMLATTC )

*        modulate field in QFA/QFD
         IF(LQFLDMOD .AND. ISPUSR(1).NE.0 ) THEN

*        ISPUSR(1) = NUMBER * 10 + TURN
            IF(NREG.EQ.NQF) THEN
               NUMBER = ISPUSR(1) / 10
               CURR = (QF_CURR_MAX - QF_CURR_MIN) * ((NUMBER-ONEONE)
     &              / (N_MADX - ONEONE)) + QF_CURR_MIN
               B = B * CURR/QF_CURR_MEAN
            ELSE IF( NREG.EQ.NQD) THEN
               NUMBER = ISPUSR(1) / 10
               CURR = (QD_CURR_MAX - QD_CURR_MIN) * ((NUMBER-ONEONE)
     &              / (N_MADX - ONEONE)) + QD_CURR_MIN
               B = B * CURR/QD_CURR_MEAN               
            ENDIF
            
         ENDIF
         
*        compute the correct magnetic field:
         SELECT CASE( R_FLD_TYPE( IMLATTC ) )
            CASE (2)
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANDIPL( P_TRANSV, B, BB )
            CASE (4)
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANQUAD( P_TRANSV, B, BB )
            CASE (6)
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANSEXT( P_TRANSV, B, BB )
            CASE (8)
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANOCTU( P_TRANSV, B, BB )
            CASE (10)
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANDECA( P_TRANSV, B, BB )
            CASE (12)
               CALL BEND__P_TRANSV( P_TRANSV, C_THETA, S_THETA )
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANDIPL( P_TRANSV, B, BB )
               CALL BB_BACK_TO_REAL( BB, C_THETA, S_THETA )
            CASE (14)
               CALL BEND__P_TRANSV( P_TRANSV, C_THETA, S_THETA )
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANQUAD( P_TRANSV, B, BB )
               CALL BB_BACK_TO_REAL( BB, C_THETA, S_THETA )
            CASE (16)
               CALL BEND__P_TRANSV( P_TRANSV, C_THETA, S_THETA )
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANSEXT( P_TRANSV, B, BB )
               CALL BB_BACK_TO_REAL( BB, C_THETA, S_THETA )
            CASE (18)
               CALL BEND__P_TRANSV( P_TRANSV, C_THETA, S_THETA )
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANOCTU( P_TRANSV, B, BB )
               CALL BB_BACK_TO_REAL( BB, C_THETA, S_THETA )
            CASE (20)
               CALL BEND__P_TRANSV( P_TRANSV, C_THETA, S_THETA )
               IF ( LUSERB( IMLATTC ) ) THEN
                  CALL OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
               ENDIF
               IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
                  CALL ROTATE__P_TRANSV( P_TRANSV, 
     &                                 -R_FLD_ANGL( IBORE, IMLATTC ) )
               ENDIF
               CALL ANDECA( P_TRANSV, B, BB )
               CALL BB_BACK_TO_REAL( BB, C_THETA, S_THETA )
            CASE DEFAULT
               WRITE (LUNOUT,*) RUTNAM//': error in the type of field'
               CALL DUMP_MAGNFO( IMLATTC )
               CALL FLABRT(RUTNAM, 'error in the type of field')
         END SELECT
*        normalise array:
         IF ( BB(4) .LT. ANGLGB ) THEN
*           the field intensity is almost ZERZER, i.e. there is almost
*             no magnetic field:
            BTX = ZERZER
            BTY = ZERZER
            BTZ = ONEONE
            B   = ZERZER
            IF ( LDEBUG ) THEN
*              keep track of values for debug printout:
               BTX_DBG = BTX
               BTY_DBG = BTY
               BTZ_DBG = BTZ
            ENDIF
            GOTO 1983
         ELSE
            DO II=1,3
               BB( II ) = BB( II ) / BB( 4 )
            ENDDO
         ENDIF

*        correct for the angle:
         IF ( LUSEAN( IBORE, IMLATTC ) ) THEN
            CALL ROTATE__B( BB, R_FLD_ANGL( IBORE, IMLATTC ) )
         ENDIF
         BTX = BB(1)
         BTY = BB(2)
         BTZ = BB(3)
         B   = BB(4)
         IF ( LDEBUG ) THEN
*           keep track of values for debug printout:
            BTX_DBG = BTX
            BTY_DBG = BTY
            BTZ_DBG = BTZ
         ENDIF

*        change coordinate system: from prototype to present position
         IF ( LISTLT( IMLATTC ) ) THEN
            CALL UNDRTO ( 1, BTX, BTY, BTZ, IROTDEFI )
         ENDIF
*        re-normalise:
         BTN = SQRT( BTX**2 + BTY**2 + BTZ**2 )
         BTX = BTX / BTN
         BTY = BTY / BTN
         BTZ = BTZ / BTN

*        print in case of debug:
 1983    IF ( LDEBUG ) THEN
            WRITE( LUN_DBUG, 1982 ) LATNAM, R_FLD_TYPE( IMLATTC ),
     &P_TRANSV(1), P_TRANSV(2), P_TRANSV(3), X, Y, Z, 
     &BTX_DBG, BTY_DBG, BTZ_DBG, B
            CALL FFLUSH
            CALL FLUSH( LUN_DBUG )
         ENDIF

      END IF

*----------------------------------------------------------------------*

      RETURN
 1981 FORMAT ('#',A7,1X,A6,1X,10(A15,1X))
 1982 FORMAT (A8,1X,I6,1X,10(1PE15.8,1X))
*=== End of subroutine magfld =========================================*
      END

*----------------------------------------------------------------------*
      SUBROUTINE GET__IBORE( IMLATTC, P, IBORE )
*     decide in which bore the particle is                             *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE 'maginfo.inc'

      DIMENSION P( 3 )

      IF ( R_D( 2, IMLATTC ) .EQ. ZERZER ) THEN

*        two horizontal bores
         IF ( R_D( 1, IMLATTC ) .GT. ZERZER ) THEN
*           bore 1 on the positive x:
            IF ( P(1) .GT. R_C( 1, IMLATTC ) ) THEN
               IBORE = 1
            ELSE
               IBORE = 2
            ENDIF
         ELSE
*           bore 1 on the negative x:
            IF ( P(1) .LT. R_C( 1, IMLATTC ) ) THEN
               IBORE = 1
            ELSE
               IBORE = 2
            ENDIF
         ENDIF

      ELSE

*        two vertical bores:
         IF ( R_D( 2, IMLATTC ) .GT. ZERZER ) THEN
*           bore 1 on the positive y:
            IF ( P(2) .GT. R_C( 2, IMLATTC ) ) THEN
               IBORE = 1
            ELSE
               IBORE = 2
            ENDIF
         ELSE
*           bore 1 on the negative y:
            IF ( P(2) .LT. R_C( 2, IMLATTC ) ) THEN
               IBORE = 1
            ELSE
               IBORE = 2
            ENDIF
         ENDIF

      ENDIF

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE GET__P_TRANSV( IMLATTC, P, IBORE, P_TRANSV )
*     get the transverse position of the particle in the local         *
*       reference system of the magnet (straight)                      *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE 'maginfo.inc'

      DIMENSION P( 3 ), P_TRANSV( 3 ), P0( 3 )

*     take into account centre of prototype:
      P0(1) = R_C( 1, IMLATTC )
      P0(2) = R_C( 2, IMLATTC )
      P0(3) = R_C( 3, IMLATTC )
*     take into account centre of bore:
      IF ( R_TWIN_BORE( IMLATTC ) ) THEN
         IF ( IBORE .EQ. 1 ) THEN
            P0(1) = P0(1) +R_D( 1, IMLATTC )
            P0(2) = P0(2) +R_D( 2, IMLATTC )
            P0(3) = P0(3) +R_D( 3, IMLATTC )
         ELSE
            P0(1) = P0(1) -R_D( 1, IMLATTC )
            P0(2) = P0(2) -R_D( 2, IMLATTC )
            P0(3) = P0(3) -R_D( 3, IMLATTC )
         ENDIF
      ENDIF

*     coordinates on the transverse plane:
      P_TRANSV(1) = P(1) - P0(1)
      P_TRANSV(2) = P(2) - P0(2)
      P_TRANSV(3) = P(3) - P0(3)

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE BEND__P_TRANSV( P_TRANSV, C_THETA, S_THETA )
*     get the transverse position of the particle in the local         *
*       reference system of the magnet (bent path);                    *
*     NB: ONLY HORIZONTALLY bent reference systems;                    *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE 'maginfo.inc'

      DIMENSION P_TRANSV( 3 )

*     distance to the centre of curvature:
      P_TRANSV(1) = P_TRANSV(1) + BR * COS( BTHETA / TWOTWO ) ! in x
      PO = SQRT( P_TRANSV(1)**2 + P_TRANSV(3)**2 )            ! radial
*     cumulative angle:
      C_THETA = P_TRANSV(1) / PO
      S_THETA = P_TRANSV(3) / PO
*     radial distance to local bent path:
      P_TRANSV(1) = PO -BR

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE OFFCENTRE__P_TRANSV( P_TRANSV, IMLATTC, IBORE )
*     take into account a possible offset between the magnetic field   *
*       and the beam vacuum pipe                                       *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE 'maginfo.inc'

      DIMENSION P_TRANSV( 3 )

      IF ( IBORE .EQ. 1 ) THEN
         P_TRANSV(1) = P_TRANSV(1) -R_B(1,IMLATTC)
         P_TRANSV(2) = P_TRANSV(2) -R_B(2,IMLATTC)
         P_TRANSV(3) = P_TRANSV(3) -R_B(3,IMLATTC)
      ELSE
         P_TRANSV(1) = P_TRANSV(1) +R_B(1,IMLATTC)
         P_TRANSV(2) = P_TRANSV(2) +R_B(2,IMLATTC)
         P_TRANSV(3) = P_TRANSV(3) +R_B(3,IMLATTC)
      ENDIF

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE ROTATE__P_TRANSV( P_TRANSV, THETA )
*     take into account a possible tilt of the magnetic field about the*
*       axis of the beam vacuum pipe                                   *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

      DIMENSION P_TRANSV( 3 )
      DIMENSION P_TEMP  ( 3 )

      P_TEMP(1) = P_TRANSV(1)
      P_TEMP(2) = P_TRANSV(2)

      P_TRANSV(1) = P_TEMP(1)*COS(THETA) -P_TEMP(2)*SIN(THETA)
      P_TRANSV(2) = P_TEMP(1)*SIN(THETA) +P_TEMP(2)*COS(THETA)

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE ROTATE__B( BB, THETA )
*     take into account a possible tilt of the magnetic field about the*
*       axis of the beam vacuum pipe                                   *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

      DIMENSION BB( 4 )
      DIMENSION BT( 4 )

      BT(1) = BB(1)
      BT(2) = BB(2)

      BB(1) = BT(1)*COS(THETA) -BT(2)*SIN(THETA)
      BB(2) = BT(1)*SIN(THETA) +BT(2)*COS(THETA)

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE BB_BACK_TO_REAL( BB, C_THETA, S_THETA )
*     convert the transverse field components on a bent path into those*
*       in the global reference system;                                *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE 'maginfo.inc'

      DIMENSION BB( 3 )

*     - transform:
      TMP_BBX = BB(1) * C_THETA -BB(3) * S_THETA
      TMP_BBZ = BB(1) * S_THETA +BB(3) * C_THETA
*     - overwrite:
      BB(1) = TMP_BBX
      BB(3) = TMP_BBZ

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE ANDIPL( P_TRANSV, B, BB )
*     compute the analytical dipole field at the position P_TRANSV     *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      DIMENSION BB ( 4 ), P_TRANSV( 3 )

      BB(1) = ZERZER
      BB(2) = B
      BB(3) = ZERZER
      BB(4) = ABS( B )

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE ANQUAD( P_TRANSV, B, BB )
*     compute the analytical quadrupole field at the position P_TRANSV *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      DIMENSION BB ( 4 ), P_TRANSV( 3 )

      BB(1) = B * P_TRANSV(2) / 1.0D+02
      BB(2) = B * P_TRANSV(1) / 1.0D+02
      BB(3) = ZERZER
      BB(4) = SQRT( BB(1)**2 + BB(2)**2 )

      RETURN
      END     

*----------------------------------------------------------------------*
      SUBROUTINE ANSEXT( P_TRANSV, B, BB )
*     compute the analytical sextupole field at the position P_TRANSV  *
*     original formulae:                                               *
*         Bx = mxy;                                                    *
*         By = 1/2 m (x^2-y^2);                                        *
*     actually implemented as:                                         *
*         Bx = mxy;                                                    *
*         By = 1/2 m (x+y)*(x-y);                                      *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      DIMENSION BB ( 4 ), P_TRANSV( 3 )

      XXM = P_TRANSV(1) / 1.0D+02
      YYM = P_TRANSV(2) / 1.0D+02

      BB(1) = B * XXM * YYM
      BB(2) = HLFHLF * B * ( XXM + YYM ) * ( XXM - YYM )
      BB(3) = ZERZER
      BB(4) = SQRT( BB(1)**2 + BB(2)**2 )

      RETURN
      END     

*----------------------------------------------------------------------*
      SUBROUTINE ANOCTU( P_TRANSV, B, BB )
*     compute the analytical octupole field at the position P_TRANSV   *
*     original formulae:                                               *
*         Bx = 1/24 r (3yx^2 -y^3);                                    *
*         By = 1/24 r (x^3 -3xy^2);                                    *
*     actually implemented as:                                         *
*         Bx = 1/24 ry (sqrt(3)x+y)*(sqrt(3)x-y);                      *
*         By = 1/24 rx (x+sqrt(3)y)*(x-sqrt(3)y);                      *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      DIMENSION BB ( 4 ), P_TRANSV( 3 )

      XXM = P_TRANSV(1) / 1.0D+02
      YYM = P_TRANSV(2) / 1.0D+02

      BB(1) = B*YYM/2.4D+01 * ( SQRTHR*XXM+YYM ) * ( SQRTHR*XXM-YYM )
      BB(2) = B*XXM/2.4D+01 * ( XXM+SQRTHR*YYM ) * ( XXM-SQRTHR*YYM )
      BB(3) = ZERZER
      BB(4) = SQRT( BB(1)**2 + BB(2)**2 )

      RETURN
      END     

*----------------------------------------------------------------------*
      SUBROUTINE ANDECA( P_TRANSV, B, BB )
*     compute the analytical decapole field at the position P_TRANSV   *
*     original formulae:                                               *
*         Bx = 1/6  d (yx^3 -xy^3);                                    *
*         By = 1/24 d (x^4 -6x^2y^2 +y^4);                             *
*     actually implemented as:                                         *
*         Bx = 1/6  dxy (x+y)*(x-y);                                   *
*         By = 1/24 dxy ( x*(x+sqrt(3)y)*(x-sqrt(3)y)*x -              *
*                         y*(sqrt(3)x+y)*(sqrt(3)x-y)*y );             *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      DIMENSION BB ( 4 ), P_TRANSV( 3 )

      XXM = P_TRANSV(1) / 1.0D+02
      YYM = P_TRANSV(2) / 1.0D+02

      BB(1) = B*XXM*YYM/6.0D+00 * ( XXM + YYM ) * ( XXM - YYM )
      BB(2) = B/2.4D+01 *( 
     &        XXM*( XXM +SQRTHR*YYM )*( XXM -SQRTHR*YYM )*XXM 
     &       -YYM*( SQRTHR*XXM +YYM )*( SQRTHR*XXM -YYM )*YYM )
      BB(3) = ZERZER
      BB(4) = SQRT( BB(1)**2 + BB(2)**2 )

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE CHCK_FLKMAG(IMLATTC, LATNAM, IRCELL, IROTDEFI, ROTNAME,
     &                       IERRR )
*     check lattice information of the present magnetic element        *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE '(RTDFCM)'
      INCLUDE '(COMEPS)'

*     run-time variables
      CHARACTER*8 LATNAM
      CHARACTER*10 ROTNAME

*     error status:
      IERRR = 0
      
*     get the ROTDEFI associated to the current LATTICE region:
*     . translate the LATTICE number into LATTICE name:
      CALL GEOL2N( IMLATTC, LATNAM, IRTLAT, IRTLT2, IERR )
      IF ( IERR .GT. 0 ) THEN
         IERRR = 1
         RETURN
      ENDIF
*     . get the index of the REGION having the same name as the LATTICE
      CALL GEON2R( LATNAM, IRCELL, IERR ) 
      IF ( IERR .GT. 0 ) THEN
         IERRR = 2
         RETURN
      ENDIF
*     . (finally) get the rotation index and name for the current LATTICE 
*       region (from lattic.f):
      IROTDEFI = ILTRTN( IRCELL )
!->      IF ( ABS ( IROTDEFI ) .GE. MODRTN )
!->     &     CALL FLABRT ('CHCK_FLKMAG',
!->     &                  'Two transformations not implemented yet')
      ROTNAME  = RTNAME( IROTDEFI )

      RETURN
      END

*----------------------------------------------------------------------*
      SUBROUTINE DUMP_FLKMAG(IMLATTC, LATNAM, IRCELL, IROTDEFI, ROTNAME,
     &                       LISTLT )
*     echo lattice information of the present magnetic element         *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      
*     run-time variables
      CHARACTER*8 LATNAM
      CHARACTER*10 ROTNAME
      LOGICAL LISTLT

      WRITE(LUNOUT,*) "LATTICE:"
      WRITE(LUNOUT,1987) IMLATTC, LATNAM
      WRITE(LUNOUT,*) "REGION:"
      WRITE(LUNOUT,1987) IRCELL, LATNAM
      WRITE(LUNOUT,*) "ROT-DEFI:"
      WRITE(LUNOUT,1988) IROTDEFI, ROTNAME
      IF ( LISTLT ) THEN
         WRITE(LUNOUT,*) ' -> transformation actually ROTATEs replica;'
      ELSE
         WRITE(LUNOUT,*) ' -> transformation only TRANSLATIONAL;'
      ENDIF

      RETURN
 1987 FORMAT('   index: ',I6,'; name: "',A8,'";')
 1988 FORMAT('   index: ',I6,'; name: "',A10,'";')
      END

*----------------------------------------------------------------------*
      SUBROUTINE DUMP_MAGNFO( IMLATTC )
*     echo magnetic settings of the current lattice                    *
*----------------------------------------------------------------------*

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
      INCLUDE 'maginfo.inc'
      
      CHARACTER TMP_TYPE*30
      CHARACTER TMP_UNIT*8
      LOGICAL   L_MAP
      DATA      L_MAP / .FALSE. /

*     according to the type of magnetic field, change some messages:
      SELECT CASE( R_FLD_TYPE( IMLATTC ) )
      CASE (2)
         TMP_TYPE = "DIPOLE (fully analytical)"
         TMP_unit = " T"
      CASE (3)
         TMP_TYPE = "DIPOLE (with map)"
         TMP_unit = " T"
         L_MAP = .TRUE.
      CASE (4)
         TMP_TYPE = "QUADRUPOLE (fully analytical)"
         TMP_unit = " T/m"
      CASE (5)
         TMP_TYPE = "QUADRUPOLE (with map)"
         TMP_unit = " T/m"
         L_MAP = .TRUE.
      CASE (6)
         TMP_TYPE = "SEXTUPOLE (fully analytical)"
         TMP_unit = " T/m2"
      CASE (7)
         TMP_TYPE = "SEXTUPOLE (with map)"
         TMP_unit = " T/m2"
         L_MAP = .TRUE.
      CASE (8)
         TMP_TYPE = "OCTUPOLE (fully analytical)"
         TMP_unit = " T/m3"
      CASE (9)
         TMP_TYPE = "OCTUPOLE (with map)"
         TMP_unit = " T/m3"
         L_MAP = .TRUE.
      CASE (10)
         TMP_TYPE = "DECAPOLE (fully analytical)"
         TMP_unit = " T/m4"
      CASE (11)
         TMP_TYPE = "DECAPOLE (with map)"
         TMP_unit = " T/m4"
         L_MAP = .TRUE.
      CASE (12)
         TMP_TYPE = "DIPOLE (fully analytical - bent local ref sys)"
         TMP_unit = " T"
      CASE (13)
         TMP_TYPE = "DIPOLE (with map - bent local ref sys)"
         TMP_unit = " T"
         L_MAP = .TRUE.
      CASE (14)
         TMP_TYPE = "QUADRUPOLE (fully analytical - bent local ref sys)"
         TMP_unit = " T/m"
      CASE (15)
         TMP_TYPE = "QUADRUPOLE (with map - bent local ref sys)"
         TMP_unit = " T/m"
         L_MAP = .TRUE.
      CASE (16)
         TMP_TYPE = "SEXTUPOLE (fully analytical - bent local ref sys)"
         TMP_unit = " T/m2"
      CASE (17)
         TMP_TYPE = "SEXTUPOLE (with map - bent local ref sys)"
         TMP_unit = " T/m2"
         L_MAP = .TRUE.
      CASE (18)
         TMP_TYPE = "OCTUPOLE (fully analytical - bent local ref sys)"
         TMP_unit = " T/m3"
      CASE (19)
         TMP_TYPE = "OCTUPOLE (with map - bent local ref sys)"
         TMP_unit = " T/m3"
         L_MAP = .TRUE.
      CASE (20)
         TMP_TYPE = "DECAPOLE (fully analytical - bent local ref sys)"
         TMP_unit = " T/m4"
      CASE (21)
         TMP_TYPE = "DECAPOLE (with map - bent local ref sys)"
         TMP_unit = " T/m4"
         L_MAP = .TRUE.
      CASE DEFAULT
         WRITE (LUNOUT,*) 'un-recognized type of field'
         WRITE (LUNOUT,*) '               MLATTC=', IMLATTC
         WRITE (LUNOUT,*) 'R_FLD_TYPE( IMLATTC )=',
     &        R_FLD_TYPE( IMLATTC )
         STOP
      END SELECT

*     dump information:
      WRITE(LUNOUT,*) ''
      WRITE(LUNOUT,*) "----------------------------------------------"
      WRITE(LUNOUT,*) "MAGFLD for MLATTC:",IMLATTC
      WRITE(LUNOUT,*) "----------------------------------------------"
      WRITE(LUNOUT,*) "          TYPE: ",TMP_TYPE
      WRITE(LUNOUT,*) "     INTENSITY (bore 1):",
     &                  R_FLD_INTN( 1, IMLATTC ),TMP_unit
      WRITE(LUNOUT,*) "REF. DIRECTION (bore 1):",
     &                  R_FLD_ANGL( 1, IMLATTC )," deg"
      R_FLD_ANGL( 1, IMLATTC ) = R_FLD_ANGL( 1, IMLATTC ) /
     &                            1.8D+02 * PIPIPI
      WRITE(LUNOUT,*) "                        ",
     &                 R_FLD_ANGL( 1, IMLATTC )," rad"
      IF ( LUSEAN( 1, IMLATTC ) ) THEN
         WRITE(LUNOUT,*) " -> additional tilt to be CONSIDERED!"
      ELSE
         WRITE(LUNOUT,*) " -> NO additional tilt to be considered!"
      ENDIF

*     in case the present element is a two-bores magnet:
      IF ( R_TWIN_BORE( IMLATTC ) ) THEN
         WRITE(LUNOUT,*) "     INTENSITY (bore 2):",
     &        R_FLD_INTN( 2, IMLATTC ),TMP_unit
         WRITE(LUNOUT,*) "REF. DIRECTION (bore 2):",
     &        R_FLD_ANGL( 2, IMLATTC )," deg"
         R_FLD_ANGL( 2, IMLATTC ) = R_FLD_ANGL( 2, IMLATTC ) /
     &        1.8D+02 * PIPIPI
         WRITE(LUNOUT,*) "                        ",
     &        R_FLD_ANGL( 2, IMLATTC )," rad"
         IF ( LUSEAN( 2, IMLATTC ) ) THEN
            WRITE(LUNOUT,*) " -> additional tilt to be CONSIDERED!"
         ELSE
            WRITE(LUNOUT,*) " -> NO additional tilt to be considered!"
         ENDIF
      ENDIF

*     in case a map is linked to the present element:
      IF ( L_MAP ) THEN 
         WRITE(LUNOUT,*) "      MAP NAME:",R_MAP_NAME( IMLATTC )
      ENDIF

*     geometrical information:
      WRITE(LUNOUT,*) "R_C( 1, IMLATTC ): ", R_C( 1, IMLATTC )
      WRITE(LUNOUT,*) "R_C( 2, IMLATTC ): ", R_C( 2, IMLATTC )
      WRITE(LUNOUT,*) "R_C( 3, IMLATTC ): ", R_C( 3, IMLATTC )
      WRITE(LUNOUT,*) "R_D( 1, IMLATTC ): ", R_D( 1, IMLATTC )
      WRITE(LUNOUT,*) "R_D( 2, IMLATTC ): ", R_D( 2, IMLATTC )
      WRITE(LUNOUT,*) "R_D( 3, IMLATTC ): ", R_D( 3, IMLATTC )
      IF ( R_TWIN_BORE( IMLATTC ) ) THEN
         WRITE(LUNOUT,*) " -> it's a TWIN-bores magnet;"
      ELSE
         WRITE(LUNOUT,*) " -> it's a SINGLE-bore magnet;"
      ENDIF
      WRITE(LUNOUT,*) "off-centre of magnetic field (wrt R_D)"
      WRITE(LUNOUT,*) "R_B( 1, IMLATTC ): ", R_B( 1, IMLATTC )
      WRITE(LUNOUT,*) "R_B( 2, IMLATTC ): ", R_B( 2, IMLATTC )
      WRITE(LUNOUT,*) "R_B( 3, IMLATTC ): ", R_B( 3, IMLATTC )
      IF ( LUSERB( IMLATTC ) ) THEN
         WRITE(LUNOUT,*) " -> mag field OFF-CENTRED wrt bore;"
      ELSE
         WRITE(LUNOUT,*) " -> mag field NOT off-centred wrt bore;"
      ENDIF
      WRITE(LUNOUT,*) "----------------------------------------------"

      CALL FFLUSH
      CALL FLUSH(LUNOUT)

      RETURN
      END
