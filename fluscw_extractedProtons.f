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
*     Copyright (C) 1989-2015      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*     New version of Fluscw for FLUKA9x-FLUKA20xy:                     *
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
*----------------------------------------------------------------------*
*
      INCLUDE '(SCOHLP)'
      INCLUDE '(TRACKR)'
      INCLUDE '(USRBDX)'
      INCLUDE 'EXTRST.inc'
*
      INTEGER LUN_FLUSCW
      DATA LUN_FLUSCW / -23 /

      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.

         WRITE ( LUNOUT,* ) ""
         WRITE ( LUNOUT,* ) "FLUSCW to count extracted protons"
         WRITE ( LUNOUT,* ) "FLUSCW logical unit", LUN_FLUSCW
         WRITE ( LUNOUT,* ) ""
         CALL FFLUSH(LUNOUT)

      END IF

*     set initial flag to scoring
      FLUSCW = ONEONE
      LSCZER = .FALSE.

*     check the unit number
      IF (IPUSBX(JSCRNG) .NE. LUN_FLUSCW) GOTO 100

*     check that the particle is a proton
      IF (JTRACK.NE.1) GOTO 100

*     at this point set initial flag to no scoring
      LSCZER = .TRUE.

*     compute TURN
      ITURN = MOD(ISPUSR(1), 10)

*     activate scoring if there is correspondence between
*     detector name and turn
      IF     (TITUSX(JSCRNG).EQ.'bp1_ext   ' .AND. ITURN.EQ.1 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR1( ITURN ) = NEXTR1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p1_ext    ' .AND. ITURN.EQ.1) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR ( ITURN ) = NEXTR ( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'bp2_ext   ' .AND. ITURN.EQ.2 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR1( ITURN ) = NEXTR1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p2_ext    ' .AND. ITURN.EQ.2) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR ( ITURN ) = NEXTR ( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'bp3_ext   ' .AND. ITURN.EQ.3 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR1( ITURN ) = NEXTR1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p3_ext    ' .AND. ITURN.EQ.3) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR ( ITURN ) = NEXTR ( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'bp4_ext   ' .AND. ITURN.EQ.4 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR1( ITURN ) = NEXTR1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p4_ext    ' .AND. ITURN.EQ.4) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NEXTR ( ITURN ) = NEXTR ( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'bp1_cir   ' .AND. ITURN.EQ.1 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC1( ITURN ) = NCIRC1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p1_cir    ' .AND. ITURN.EQ.1) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC ( ITURN ) = NCIRC ( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'bp2_cir   ' .AND. ITURN.EQ.2 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC1( ITURN ) = NCIRC1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p2_cir    ' .AND. ITURN.EQ.2) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC ( ITURN ) = NCIRC ( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'bp3_cir   ' .AND. ITURN.EQ.3 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC1( ITURN ) = NCIRC1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p3_cir    ' .AND. ITURN.EQ.3) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC ( ITURN ) = NCIRC ( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'bp4_cir   ' .AND. ITURN.EQ.4 .AND.
     &     LTRACK.EQ.1 ) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC1( ITURN ) = NCIRC1( ITURN ) + 1
      ELSE IF(TITUSX(JSCRNG).EQ.'p4_cir    ' .AND. ITURN.EQ.4) THEN
         LSCZER = .FALSE.
         IF( NREG.EQ.NR2USX(JSCRNG) .AND. IOLREG.EQ.NR1USX(JSCRNG))
     +        NCIRC ( ITURN ) = NCIRC ( ITURN ) + 1        
      END IF

 100  CONTINUE
 101  FORMAT(I3,4(1X,1PE10.3),5I5)
      RETURN
*=== End of function Fluscw ===========================================*
      END
