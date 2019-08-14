*$ CREATE USRMED.FOR
*COPY USRMED
*                                                                      *
*=== usrmed ===========================================================*
*                                                                      *
      SUBROUTINE USRMED ( IJ, EKSCO, PLA, WEE, MREG, NEWREG, XX, YY, ZZ,
     &                    TXX, TYY, TZZ, TXXPOL, TYYPOL, TZZPOL )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1991-2011      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR MEDium dependent directives:                                *
*                                                                      *
*     Created on  10  May  1996    by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on   31-Mar-11   by    Alfredo Ferrari               *
*                                                                      *
*     Input variables:                                                 *
*             ij = particle id                                         *
*          Eksco = particle kinetic energy (GeV)                       *
*            Pla = particle momentum (GeV/c)                           *
*            Wee = particle weight                                     *
*           Mreg = (original) region number                            *
*         Newreg = (final)    region number                            *
*       Xx,Yy,Zz = particle position                                   *
*    Txx,Tyy,Tzz = particle direction                                  *
* Txx,Tyy,Tzzpol = particle polarization direction                     *
*                                                                      *
*     The user is supposed to change only WEE if MREG = NEWREG and     *
*     WEE, NEWREG, TXX, TYY, TZZ (TXXPOL, TYYPOL, TZZPOL) if           *
*     MREG .NE. NEWREG                                                 *
*                                                                      *
* Start_Devel_seq                                                      *
*                                                                      *
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *
*     !!!! BE CAREFUL NOT TO INCLUDE TRACKR, SINCE OFTEN SOME !!!!     *
*     !!!! OF THE ARGUMENTS COME FROM TRACKR!!                !!!!     *
*     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!     *
*                                                                      *
* End_Devel_seq                                                        *
*                                                                      *
*     Dumping routine for FLUKA loss map generation from high cutoff   *
*     runs. It intercepts particles leaving vacuum.                    *
*                                                                      *
*     Last change on   22-May-13   by    Francesco Cerutti             *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(FLKMAT)'
      INCLUDE '(TRACKR)'
*
*    COMMON / PRMORGIN / ORGINZ
*                        
*  usual flag for first call:
      LOGICAL LFIRST, LKILL
      SAVE LFIRST
      DATA LFIRST / .TRUE. /
      PARAMETER ( LKILL = .FALSE. )
      PARAMETER ( Z0 = -300.0D0 )
*
* output file
      CHARACTER*80 USRMED_FILE_NAME
      DATA USRMED_FILE_NAME / 'usrmed.dat' /
      INTEGER LUN_USRMED
      DATA LUN_USRMED / 97 /
* 
      IF ( LFIRST ) THEN
         WRITE(LUNOUT,*) ''
         WRITE(LUNOUT,*) 'usrmed.f modified by L.S. Esposito'
         WRITE(LUNOUT,*) 'Lefosmod       version: 2012-08-22'
         WRITE(LUNOUT,*) 'Position to check? Z >', Z0
         WRITE(LUNOUT,*) 'Particle killed ', LKILL
         WRITE(LUNOUT,*) ''
         CALL OAUXFI( USRMED_FILE_NAME, LUN_USRMED, 'NEW', IERR )
         WRITE(LUN_USRMED,10) "NCASE", "IJ", "PLA", "X", "Y", "Z",
     &        "TXX", "TYY", "TZZ", "WEIGHT", "LTRACK", "ISAMPLE"
         LFIRST = .FALSE.
      ENDIF
     
      IF ( ZZ .LT. Z0 ) GOTO 100
      IF ( MEDFLK(MREG,1) .EQ. 2 .AND. MEDFLK(NEWREG,1) .NE. 2 ) THEN
*     save only protons at the first touch
         IF ( IJ .EQ. 1 .AND. LLOUSE .EQ. 0) THEN
            WRITE(LUN_USRMED,20) NCASE, IJ, PLA, XX, YY, ZZ,
     &           TXX, TYY, TZZ, WEE, LTRACK, ISPUSR(1)
         ENDIF
*     update user flag to count the number of passages vacuum-not vacuum
         LLOUSE = LLOUSE + 1
*     kill the particle
         IF ( LKILL ) WEE = ZERZER
      ENDIF
*
 100  CONTINUE
*
      RETURN
 10   FORMAT ("#",A7,A4,8(1X,A17),A7,A12)
 20   FORMAT (I8,I4,8(1X,1PE17.10),I7,I12)
*=== End of subroutine Usrmed =========================================*
      END
