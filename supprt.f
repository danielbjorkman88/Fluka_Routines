*$ CREATE SUPPRT.FOR
*COPY SUPPRT

*== tagrut ============================================================*

      SUBROUTINE TAGRUT ( RUTNAM, RUTSAV, RUTAUT )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

*----------------------------------------------------------------------*
*                                                                      *
*     A.Mereghetti, 18/04/2012                                         *
*     routine for simply writing in LUNOUT a tag of a routine, with the*
*       date of the last modifications; very useful if called at       *
*       initialisation of the calling routine                          *
*                                                                      *
*     input variables:                                                 *
*                                                                      *
*       RUTNAM = name (6 letters!!) of the routine calling TAGRUT;     *
*       RUTSAV = tag (10 letters!!) of the routine calling TAGRUT;     *
*                usually, the date in format DD/MM/YYYY of the last    *
*                modification is a good tag;                           *
*       RUTAUT = author name (20 letters!!);                           *
*                                                                      *
*----------------------------------------------------------------------*

      CHARACTER RUTNAM*6,RUTSAV*10,RUTAUT*20

      WRITE(LUNOUT,*) ''
      WRITE(LUNOUT,*) RUTNAM,' routine by ', RUTAUT
      WRITE(LUNOUT,*) '      last version: ', RUTSAV
      WRITE(LUNOUT,*) '      first call'
      WRITE(LUNOUT,*) ''

      RETURN
      END
