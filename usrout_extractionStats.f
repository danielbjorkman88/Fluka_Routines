*$ CREATE USROUT.FOR
*COPY USROUT
*
*=== usrout ===========================================================*
*
      SUBROUTINE USROUT ( WHAT, SDUM )

      INCLUDE 'EXTRST.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1991-2005      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR OUTput: this routine is called every time the USROCALL card *
*                  is found in the input stream                        *
*                                                                      *
*                                                                      *
*     Created on 01 january 1991   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 20-mar-05     by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*
*
      DIMENSION WHAT (6)
      CHARACTER SDUM*8
*
      LUN = 85
      CALL OAUXFI('EXTR_STATS',LUN,'NEW',IERR)
*
      WRITE( LUN, * ) '# ITURN NSAMPLED NEXTR NEXTR1 NCIRC NCIRC1'
      WRITE( LUN, 100 ) 
     +     ( I, NSAMPLED(I), NEXTR(I), NEXTR1(I),
     +     NCIRC(I), NCIRC1(I), I = 1, NTURN )
      RETURN
 100  FORMAT(I3,5I12)
*=== End of subroutine Usrout =========================================*
      END

