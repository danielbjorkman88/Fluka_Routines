*$ CREATE LATTIC.FOR
*COPY LATTIC
*
*=== lattic ===========================================================*
*
      SUBROUTINE LATTIC ( XB, WB, DIST, SB, UB, IR, IRLTGG, IRLT, IFLAG)

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

*     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*     PARAMETER ( LUNOUT = 11 )
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1993-2011      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     LATTIC: user written routine which must return the tracking point*
*     and direction ( SB, UB ) corresponding to region number IR, cell *
*     number IRLTGG and real position/direction XB, WB                 *
*                                                                      *
*     Created on 16 December 1993  by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  08-Mar-11    by    Alfredo Ferrari               *
*                                                                      *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*     ATLAS LATTIC Routine         by    Thanos Manousos (CERN & AUTH) *
*                                                                      *
*     Mirroring of ATLAS Detector Geometry                             *   
*                                                                      *
*     Last change on  22-Mar-15    by    Thanos Manousos (CERN & AUTH) *
*----------------------------------------------------------------------*

      INCLUDE '(GLTLOC)'
      INCLUDE '(RTGMMV)'
      
      LOGICAL LFIRST
      DIMENSION IRLT (*)
      DIMENSION XB   (3), WB (3), SB (3), UB (3), UN (3)
      SAVE IRLSAV, LFIRST
*
      DATA LFIRST /.TRUE./
      DATA IRLSAV / -1 /
*
*  +-------------------------------------------------------------------*
*  |  First time initialization:
      IF ( LFIRST ) THEN
         LFIRST = .FALSE.
      END IF
*  |
*  +-------------------------------------------------------------------*
*  +-------------------------------------------------------------------*
*  |
      IF (IRLTGG.EQ.101) THEN
         SB (1) = XB (1)
         SB (2) = XB (2)
         SB (3) =-XB (3)
         UB (1) = WB (1)
         UB (2) = WB (2)
         UB (3) =-WB (3)
      
      ELSE IF ( IFLAG .LT. 0 )  THEN
         WRITE (LUNOUT,*)
     &   ' *** Lattic called with both Irltgg=Irlsav and Iflag < 0 ***'
         CALL FLABRT ('LATTIC', 'LATTIC_2')
      
      ELSE
         WRITE (LUNOUT,*)'*** NON-EXISTENT LATTICE: ', IRLTGG, ' ***'
      
      END IF
      
      RETURN
*
*======================================================================*
*                                                                      *
*     Entry LATNOR:                                                    *
*                                                                      *
*======================================================================*
*
      ENTRY LATNOR ( UN, IRLTNO, IRLT )
*
*----------------------------------------------------------------------*
*                                                                      *
*     LATtice cell NORmal transformation:                              *
*                                                                      *
*     Input variables:                                                 *
*                      un(i) = normal components in the tracking re-   *
*                              ference system                          *
*                     irltno = present lattice cell #                  *
*     Output variables:                                                *
*                      un(i) = normal components in the problem re-    *
*                              ference system                          *
*                                                                      *
*----------------------------------------------------------------------*
*
      IF ( IRLTNO .GT.  100 ) THEN
         UN (1) = UN (1)
         UN (2) = UN (2)
         UN (3) =-UN (3)
      ENDIF
      
      RETURN
*=== End of subroutine lattic =========================================*
      END

