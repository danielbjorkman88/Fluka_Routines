*$ CREATE USRGLO.FOR
*COPY USRGLO
*
*=== usrglo ===========================================================*
*
      SUBROUTINE USRGLO ( WHAT, SDUM )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2006-2006      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR GLObal settings: this routine is called before whichever    *
*                           FLUKA initialization starts every time the *
*                           USRGCALL card is found anywhere in the     *
*                           input stream                               *
*                                                                      *
*                                                                      *
*     Created on 11 september 2006  by   Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 11-sep-06      by   Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE 'maginfo.inc'

      DIMENSION WHAT (6)
      CHARACTER SDUM*8

      CHARACTER RUTNAM*6, RUTSAV*10, RUTAUT*20
      SAVE RUTNAM, RUTSAV, RUTAUT
      DATA RUTNAM / 'usrglo' /
      DATA RUTSAV / '17/09/2014' /
      DATA RUTAUT / 'A. Mereghetti' /

      SAVE IMLATTC
      DATA IMLATTC / 0 /

      LOGICAL LFIRST
      SAVE LFIRST
      DATA LFIRST / .TRUE. /

*  Don't change the following line:
      LUSRGL = .TRUE.
* *** Write from here on *** *

* variable initialization:
      IF ( LFIRST ) THEN
*        dump a tag about the routine:
         CALL TAGRUT( RUTNAM, RUTSAV, RUTAUT )
         CALL FFLUSH
         CALL FLUSH(LUNOUT)
         CALL FLUSH(LUNERR)
         LFIRST = .FALSE.

*        initialisation of maginfo.inc common
*        NB: see header in common file for meaning of variables;
         RMAX    = ZERZER
         LIMIBOR = .FALSE.
*        - information related to main bending magnets:
         BTHETA  = ZERZER
         BR      = ZERZER
         BLENGTH = ZERZER
         NBENDs  = 0
*        - information related to elements:
         DO I=1, MXELMTS
            R_FLD_TYPE ( I )    = -1
            R_MAP_NAME ( I )    = ""
            R_FLD_INTN ( 1, I ) = ZERZER
            R_FLD_INTN ( 2, I ) = ZERZER
            R_FLD_ANGL ( 1, I ) = ZERZER
            R_FLD_ANGL ( 2, I ) = ZERZER
            R_C        ( 1, I ) = ZERZER
            R_C        ( 2, I ) = ZERZER
            R_C        ( 3, I ) = ZERZER
            R_D        ( 1, I ) = ZERZER
            R_D        ( 2, I ) = ZERZER
            R_D        ( 3, I ) = ZERZER
            R_B        ( 1, I ) = ZERZER
            R_B        ( 2, I ) = ZERZER
            R_B        ( 3, I ) = ZERZER
            R_TWIN_BORE( I )    = .FALSE.
         ENDDO
*        - information for speeding up the calculation:
         DO I=1, MXELMTS
            IROTMG( I ) = -1
            LMAGLT( I ) = .FALSE.
            LUSERB( I ) = .FALSE.
            LISTLT( I ) = .FALSE.
            LUSEAN( 1, I ) = .FALSE.
            LUSEAN( 2, I ) = .FALSE.
         ENDDO
      ENDIF
*
      IF ( SDUM(1:8) .EQ. INSDUM ) THEN
*        initialise user customisation variables
*          (mainly related to main bending magnets):
         RMAX    = WHAT(1)
         NBENDs  = INT( WHAT(2) + 1.0D-03 )
         BLENGTH = WHAT(3)

      ELSEIF ( SDUM(1:2) .EQ. "&&" ) THEN
*        second continuation card
         R_B( 1, IMLATTC ) = WHAT(1)
         R_B( 2, IMLATTC ) = WHAT(2)
         R_B( 3, IMLATTC ) = WHAT(3)
         
      ELSEIF ( SDUM(1:1) .EQ. "&" ) THEN
*        first continuation card
         R_C( 1, IMLATTC ) = WHAT(1)
         R_C( 2, IMLATTC ) = WHAT(2)
         R_C( 3, IMLATTC ) = WHAT(3)
         R_D( 1, IMLATTC ) = WHAT(4)
         R_D( 2, IMLATTC ) = WHAT(5)
         R_D( 3, IMLATTC ) = WHAT(6)

      ELSE
*        main card
         IMLATTC   = INT( WHAT(1) + 1.0D-03 )
         IF ( IMLATTC .GT. MXELMTS ) THEN
            CALL FLABRT( RUTNAM, 'arrays in common maginfo.inc are'// 
     &' too small!! please increase MXELMTS!!')
         ENDIF
         R_FLD_TYPE( IMLATTC )    = WHAT(2)
         R_FLD_INTN( 1, IMLATTC ) = WHAT(3)
         R_FLD_ANGL( 1, IMLATTC ) = WHAT(4)
         R_FLD_INTN( 2, IMLATTC ) = WHAT(5)
         R_FLD_ANGL( 2, IMLATTC ) = WHAT(6)
         R_MAP_NAME( IMLATTC )    = SDUM
      ENDIF
*
      RETURN
*=== End of subroutine Usrglo =========================================*
      END
