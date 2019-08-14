*$ CREATE USRINI.FOR
*COPY USRINI
*
*=== usrini ===========================================================*
*
      SUBROUTINE USRINI ( WHAT, SDUM )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1991-2005      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR INItialization: this routine is called every time the       *
*                          USRICALL card is found in the input stream  *
*                                                                      *
*                                                                      *
*     Created on 01 january 1991   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on 20-mar-05     by    Alfredo Ferrari               *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*
 
*----------------------------------------------------------------------*
*     ATLAS USRINI Routine                                             *
*                                                                      *
*     Magnetic Field Initialization (Executing MAGINI Function         *  
*                                                                      *
*     Last change on  22-Apr-15    by    Thanos Manousos               *
*----------------------------------------------------------------------*
 
      DIMENSION WHAT (6)
      CHARACTER SDUM*8
*
*  Don't change the following line:
      LUSRIN = .TRUE.
* *** Write from here on *** *
      CALL MAGINI
 
      RETURN
*=== End of subroutine Usrini =========================================*
      END
