*$ CREATE FLUSCW.FOR
*COPY FLUSCW
*                                                                      *
*=== fluscw ===========================================================*
*                                                                      *
      DOUBLE PRECISION FUNCTION FLUSCW ( IJ    , PLA   , TXX   , TYY   ,
     &                                   TZZ   , WEE   , XX    , YY    ,
     &                                   ZZ    , NREG  , IOLREG, LLO   ,
     &                                   NSURF )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1989-2005      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*     New version of Fluscw for FLUKA9x-FLUKA200x:                     *
*                                                                      *
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *
*     !!! This is a completely dummy routine for Fluka9x/200x. !!!     *
*     !!! The  name has been kept the same as for older  Fluka !!!     *
*     !!! versions for back-compatibility, even though  Fluscw !!!     *
*     !!! is applied only to estimators which didn't exist be- !!!     *
*     !!! fore Fluka89.                                        !!!     *
*     !!! User  developed versions  can be used for  weighting !!!     *
*     !!! flux-like quantities at runtime                      !!!     *
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *
*                                                                      *
*     Input variables:                                                 *
*                                                                      *
*           Ij = (generalized) particle code (Paprop numbering)        *
*          Pla = particle laboratory momentum (GeV/c) (if > 0),        *
*                or kinetic energy (GeV) (if <0 )                      *
*    Txx,yy,zz = particle direction cosines                            *
*          Wee = particle weight                                       *
*     Xx,Yy,Zz = position                                              *
*         Nreg = (new) region number                                   *
*       Iolreg = (old) region number                                   *
*          Llo = particle generation                                   *
*        Nsurf = transport flag (ignore!)                              *
*                                                                      *
*     Output variables:                                                *
*                                                                      *
*       Fluscw = factor the scored amount will be multiplied by        *
*       Lsczer = logical flag, if true no amount will be scored        *
*                regardless of Fluscw                                  *
*                                                                      *
*     Useful variables (common SCOHLP):                                *
*                                                                      *
*     Flux like binnings/estimators (Fluscw):                          *
*          ISCRNG = 1 --> Boundary crossing estimator                  *
*          ISCRNG = 2 --> Track  length     binning                    *
*          ISCRNG = 3 --> Track  length     estimator                  *
*          ISCRNG = 4 --> Collision density estimator                  *
*          ISCRNG = 5 --> Yield             estimator                  *
*          JSCRNG = # of the binning/estimator                         *
*                                                                      *
*     A.Mereghetti, 19/04/2012                                         *
*     the routine has been modified in order to:                       *
*     1. dump position and direction of the scored particle, in case   *
*        the SDUM of the present USRBDX detector is set to a special   *
*        value (see the value of the DUMP_SDUM variable)               *
*     2. disentangle replica of the same scoring on different LATTICEs:*
*        the SDUM of the current detector is compared to the LATTICE   *
*        name: in case of matching, the FLUSCW variable is assigned    *
*        ONEONE and the scoring is thus performed; otherwise, if the   *
*        particle is being tracked in a LATTICE, the FLUSCW variable is*
*        assigned ZERZER and the scoring is thus not performed,        *
*        otherwise the scoring is regularly performed;                 *
*                                                                      *
*     useful routines in FLUKA:                                        *
*                                                                      *
*                                                           LATTICE    *
*     - GEON2L( NAME, LATT_REG, IRTDUM, IERR )                         *
*          from NAME to LATT_REG (lattice number)                      *
*     - GEOL2N( I, LATNAM, IRTDUM, IERR )                              *
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
      INCLUDE '(SCOHLP)'
      INCLUDE '(USRBDX)'
      INCLUDE '(USRYLD)'
      INCLUDE '(USRBIN)'
      INCLUDE '(USRTRC)'
      INCLUDE '(LTCLCM)'

      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /

      CHARACTER RUTNAM*6, RUTSAV*10, RUTAUT*20
      SAVE RUTNAM, RUTSAV, RUTAUT
      DATA RUTNAM / 'FLUSCW' /
      DATA RUTSAV / '12/05/2012' /
      DATA RUTAUT / 'A. Mereghetti' /

*     dump mode
      CHARACTER*20 FILENAME
      DATA FILENAME / 'part_at_USRBDX.dat' /
      INTEGER LUN_PRINT
      DATA LUN_PRINT / 78 /

      LOGICAL LDUMPM
      SAVE LDUMPM
      DATA LDUMPM / .FALSE. /

*     run-time variables
      CHARACTER*8 LATNAM
      CHARACTER*10 SDUM

*     default values of returned variables
      FLUSCW = ONEONE
      LSCZER = .FALSE.

*----------------------------------------------------------------------*
*     first call                                                       *
*----------------------------------------------------------------------*

      IF ( LFIRST ) THEN
         CALL TAGRUT ( RUTNAM, RUTSAV, RUTAUT )
         LFIRST = .FALSE.

*        check if the dump mode should be activated, ie if there are 
*           special USRBDX cards:
         DO II=1,NUSRBX
            IF ( TITUSX(JSCRNG)(10:10) .EQ. 'R' .OR. 
     &           TITUSX(JSCRNG)(10:10) .EQ. 'P' ) THEN
               LDUMPM = .TRUE.
               GOTO 1983
            ENDIF
 1983    ENDDO

*        in case the dump mode is on, declare it in LUNOUT and 
*        open the corresponding unit
         IF ( LDUMPM ) THEN
            WRITE(LUNOUT,*)'      dump mode activated'
            WRITE(LUNOUT,*)' '
            CALL FFLUSH
            CALL FLUSH(LUNOUT)
            CALL OAUXFI( FILENAME, LUN_PRINT, 'NEW', IERR )
            IF ( IERR .GT. 0 ) THEN
               CALL FLABRT(RUTNAM,'Error opening file '//FILENAME)
            ENDIF
            WRITE( LUN_PRINT, 1981 ) 'SDUM', 'XX [cm]', 'YY [cm]',
     &           'ZZ [cm]', 'TXX []', 'TYY []', 'TZZ []'
         ENDIF
      ENDIF

*----------------------------------------------------------------------*
*     core of the routine                                              *
*----------------------------------------------------------------------*
      
      IF ( LDUMPM ) THEN 
*        dump mode:

         IF ( ISCRNG .EQ. 1 ) THEN 
*           USRBDX card:

            IF ( TITUSX(JSCRNG)(10:10) .EQ. 'R' .OR. 
     &           TITUSX(JSCRNG)(10:10) .EQ. 'P' ) THEN
*              special USRBDX card?

*              get the LATTICE name, for a clearer output
               IF ( MLATTC .GT. 0 ) THEN
*                 translate the LATTICE number into LATTICE name:
                  CALL GEOL2N( MLATTC, LATNAM, IRTLAT, IERR )
                  IF ( IERR .GT. 0 ) THEN
                     WRITE (LUNOUT,*) 
     &"No LATTICE name corresponding to MLATTC ",MLATTC
                     CALL FLABRT(RUTNAM, 'Un-named MLATTC')
                  ENDIF

                  IF ( TITUSX(JSCRNG)(1:8) .EQ. LATNAM ) THEN
*                    dump coordinates only if the LATTICE name matches the 
*                       first 8 chars of the USRBDX SDUM

                     IF ( TITUSX(JSCRNG)(10:10) .EQ. 'R' ) THEN
*                       dump coordinates in real geometry:
                        XXX  = XX
                        YYY  = YY
                        ZZZ  = ZZ
                        TXXX = TXX
                        TYYY = TYY
                        TZZZ = TZZ
                     ELSE
*                       dump coordinates in prototype geometry:
           
*                       . get the index of the REGION having the same name 
*                         as the LATTICE
                        CALL GEON2R( LATNAM, IRCELL, IERR ) 
                        IF ( IERR .GT. 0 ) THEN
                           WRITE (LUNOUT,*) 
     &       "No region name corresponding to: "
                           WRITE (LUNOUT,*) "  LATTICE number: ", MLATTC
                           WRITE (LUNOUT,*) "  LATTICE name:   ", LATNAM
                           CALL FLABRT(RUTNAM, 'No region name'//LATNAM)
                        ENDIF
*                       . get the transformation index for the current 
*                         LATTICE region (from lattic.f):
                        IROTDEFI = ILTRTN( IRCELL )
           
*                       . do the transformation:
                        XXX = XX
                        YYY = YY
                        ZZZ = ZZ
                        CALL DOTRSF ( 1,  XXX,  YYY,  ZZZ, IROTDEFI )
                        TXXX = TXX
                        TYYY = TYY
                        TZZZ = TZZ
                        CALL DORTNO ( 1, TXXX, TYYY, TZZZ, IROTDEFI )

                     ENDIF
                     CALL BLK2UN ( TITUSX(JSCRNG), SDUM )
                     WRITE ( LUN_PRINT, 1982 ) SDUM,
     &                            XXX,  YYY,  ZZZ, TXXX, TYYY, TZZZ
                  ENDIF
               ELSE
*     dump coordinates in real geometry:
                  XXX  = XX
                  YYY  = YY
                  ZZZ  = ZZ
                  TXXX = TXX
                  TYYY = TYY
                  TZZZ = TZZ
                  CALL BLK2UN ( TITUSX(JSCRNG), SDUM )
                  WRITE ( LUN_PRINT, 1982 ) SDUM,
     &                            XXX,  YYY,  ZZZ, TXXX, TYYY, TZZZ
               ENDIF
            ENDIF
         ENDIF
      ENDIF

*     geometry with LATTICEs?          
      IF ( MLATTC .GT. 0 ) THEN

*        read the SDUM and decide what to do accordingly:
         SELECT CASE( ISCRNG )
             CASE( 1 )
*               USRBDX card
                LATNAM = TITUSX(JSCRNG)(1:8)
             CASE( 3 )
*               USRTRACK card:
                LATNAM = TITUTC(JSCRNG)(1:8)
             CASE( 5 )
*               USRYIELD card:
                LATNAM = TITUYL(JSCRNG)(1:8)
             CASE DEFAULT
*               any other card: do not check SDUM, and return:
                RETURN 
         END SELECT
          
*        get the LATTICE number corresponding to the SDUM:
         CALL GEON2L( LATNAM, LATT_REG, IRTDUM, IERR )
         IF ( IERR .EQ. 1 ) THEN
            WRITE (LUNOUT,*) 'No LATTICE named: '//LATNAM
            CALL FLABRT(RUTNAM, 'No LATTICE named '//LATNAM )
         ENDIF
          
*        check the LATTICE number of the particle against the LATTICE number 
*          associated to the scoring and decide if to score or not:
         IF ( MLATTC .EQ. LATT_REG ) THEN
            FLUSCW = ONEONE
            LSCZER = .FALSE.
         ELSE
            FLUSCW = ZERZER
            LSCZER = .TRUE.
         ENDIF

      ENDIF

      RETURN
 1981 FORMAT ('#',1X,A8,1X,6(A23,1X))
 1982 FORMAT (A10,1X,6(1PE23.16,1X))
*=== End of function Fluscw ===========================================*
      END

      SUBROUTINE BLK2UN( NAME1, NAME2 )
      
      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

      CHARACTER*10 NAME1, NAME2
      
      DO II=1,10
         IF ( NAME1(II:II) .EQ. ' ' ) THEN
            NAME2(II:II) = "_"
         ELSE
            NAME2(II:II) = NAME1(II:II)
         ENDIF
      ENDDO
      RETURN
      END
