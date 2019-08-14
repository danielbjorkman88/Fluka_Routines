*$ CREATE MGDRAW.FOR
*COPY MGDRAW
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2006      by        Alfredo Ferrari           *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     MaGnetic field trajectory DRAWing: actually this entry manages   *
*                                        all trajectory dumping for    *
*                                        drawing                       *
*                                                                      *
*     Created on   01 march 1990   by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*     Last change  05-may-06       by        Alfredo Ferrari           *
*                                              INFN - Milan            *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(COMPUT)'
      INCLUDE '(SOURCM)'
      INCLUDE '(FHEAVY)'
* Start_Devel_seq
      INCLUDE '(FLKMAT)'
* End_Devel_seq
      INCLUDE '(FLKSTK)'
      INCLUDE '(GENSTK)'
      INCLUDE '(MGDDCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(QUEMGD)'
      INCLUDE '(SUMCOU)'
      INCLUDE '(TRACKR)'
*
      DIMENSION DTQUEN ( MXTRCK, MAXQMG )
*
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      SAVE LFCOPE
      DATA LFCOPE / .FALSE. /
*
*----------------------------------------------------------------------*
*                                                                      *
*     Icode = 1: call from Kaskad                                      *
*     Icode = 2: call from Emfsco                                      *
*     Icode = 3: call from Kasneu                                      *
*     Icode = 4: call from Kashea                                      *
*     Icode = 5: call from Kasoph                                      *
*                                                                      *
*----------------------------------------------------------------------*
*                                                                      *
* LSE ->       IF ( .NOT. LFCOPE ) THEN
* LSE ->          LFCOPE = .TRUE.
* LSE ->          IF ( KOMPUT .EQ. 2 ) THEN
* LSE ->             FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
* LSE ->          ELSE
* LSE ->             FILNAM = CFDRAW
* LSE ->          END IF
* LSE ->          OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
* LSE ->      &          'UNFORMATTED' )
* LSE ->       END IF
* LSE ->       WRITE (IODRAW) NTRACK, MTRACK, JTRACK, SNGL (ETRACK),
* LSE ->      &               SNGL (WTRACK)
* LSE ->       WRITE (IODRAW) ( SNGL (XTRACK (I)), SNGL (YTRACK (I)),
* LSE ->      &                 SNGL (ZTRACK (I)), I = 0, NTRACK ),
* LSE ->      &               ( SNGL (DTRACK (I)), I = 1, MTRACK ),
* LSE ->      &                 SNGL (CTRACK)
* LSE -> *  +-------------------------------------------------------------------*
* LSE -> *  |  Quenching is activated
* LSE ->       IF ( LQEMGD ) THEN
* LSE ->          IF ( MTRACK .GT. 0 ) THEN
* LSE ->             RULLL  = ZERZER
* LSE ->             CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
* LSE ->             WRITE (IODRAW) ( ( SNGL (DTQUEN (I,JBK)), I = 1, MTRACK ),
* LSE ->      &                         JBK = 1, NQEMGD )
* LSE -> D           IF ( ICHRGE (JTRACK) .EQ. 0 )
* LSE -> D    &         CALL FLABRT ( 'MGDRAW', 'MTRACK>0 && ICH == 0' )
* LSE -> D           IF ( MEDFLK (MREG,IPRODC) .LE. 2 )
* LSE -> D    &         CALL FLABRT ( 'MGDRAW', 'MTRACK>0 && MEDIUM <= 2' )
* LSE -> D        ELSE
* LSE -> D           IF ( MEDFLK (MREG,IPRODC) .GT. 2
* LSE -> D    &          .AND. ICHRGE (JTRACK) .NE. 0 )
* LSE -> D    &      CALL FLABRT ( 'MGDRAW', 'MTRACK=0 .NEQV. MEDIUM <=2' )
* LSE ->          END IF
* LSE ->       END IF
* LSE -> *  |  End of quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     Boundary-(X)crossing DRAWing:                                    *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             19: boundary crossing                                    *
*     Icode = 2x: call from Emfsco                                     *
*             29: boundary crossing                                    *
*     Icode = 3x: call from Kasneu                                     *
*             39: boundary crossing                                    *
*     Icode = 4x: call from Kashea                                     *
*             49: boundary crossing                                    *
*     Icode = 5x: call from Kasoph                                     *
*             59: boundary crossing                                    *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO )
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = 73, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'FORMATTED' )
         WRITE(73,100) "X", "Y", "Z", "TXX", "TYY", "TZZ",
     &        "CTRACK", "CMTRCK", "ATRACK"
      END IF
*     check that it is a proton of first generation
      IF ( JTRACK .NE. 1 ) GOTO 300
      IF ( Ltrack .NE. 1 ) GOTO 300
      WRITE (73,200) XSCO, YSCO, ZSCO,
     &     CXTRCK, CYTRCK, CZTRCK, CTRACK, CMTRCK, ATRACK,
     &     NCASE, ISPUSR(1), LLOUSE
 100  FORMAT ("#", A19, 8(1X,A19))
 200  FORMAT(9E20.11,2I12,I8)
 300  CONTINUE
      RETURN
*
*======================================================================*
*                                                                      *
*     Event End DRAWing:                                               *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY EEDRAW ( ICODE )
      RETURN
*
*======================================================================*
*                                                                      *
*     ENergy deposition DRAWing:                                       *
*                                                                      *
*     Icode = 1x: call from Kaskad                                     *
*             10: elastic interaction recoil                           *
*             11: inelastic interaction recoil                         *
*             12: stopping particle                                    *
*             13: pseudo-neutron deposition                            *
*             14: escape                                               *
*             15: time kill                                            *
*     Icode = 2x: call from Emfsco                                     *
*             20: local energy deposition (i.e. photoelectric)         *
*             21: below threshold, iarg=1                              *
*             22: below threshold, iarg=2                              *
*             23: escape                                               *
*             24: time kill                                            *
*     Icode = 3x: call from Kasneu                                     *
*             30: target recoil                                        *
*             31: below threshold                                      *
*             32: escape                                               *
*             33: time kill                                            *
*     Icode = 4x: call from Kashea                                     *
*             40: escape                                               *
*             41: time kill                                            *
*             42: delta ray stack overflow                             *
*     Icode = 5x: call from Kasoph                                     *
*             50: optical photon absorption                            *
*             51: escape                                               *
*             52: time kill                                            *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY ENDRAW ( ICODE, MREG, RULL, XSCO, YSCO, ZSCO )
* LSE ->       IF ( .NOT. LFCOPE ) THEN
* LSE ->          LFCOPE = .TRUE.
* LSE ->          IF ( KOMPUT .EQ. 2 ) THEN
* LSE ->             FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
* LSE ->          ELSE
* LSE ->             FILNAM = CFDRAW
* LSE ->          END IF
* LSE ->          OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
* LSE ->      &          'UNFORMATTED' )
* LSE ->       END IF
* LSE ->       WRITE (IODRAW)  0, ICODE, JTRACK, SNGL (ETRACK), SNGL (WTRACK)
* LSE ->       WRITE (IODRAW)  SNGL (XSCO), SNGL (YSCO), SNGL (ZSCO), SNGL (RULL)
* LSE -> *  +-------------------------------------------------------------------*
* LSE -> *  |  Quenching is activated : calculate quenching factor
* LSE -> *  |  and store quenched energy in DTQUEN(1, jbk)
* LSE ->       IF ( LQEMGD ) THEN
* LSE ->          RULLL = RULL
* LSE ->          CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
* LSE ->          WRITE (IODRAW) ( SNGL (DTQUEN(1, JBK)), JBK = 1, NQEMGD )
* LSE ->       END IF
* LSE -> *  |  end quenching
* LSE -> *  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
      ENTRY SODRAW
* LSE ->       IF ( .NOT. LFCOPE ) THEN
* LSE ->          LFCOPE = .TRUE.
* LSE ->          IF ( KOMPUT .EQ. 2 ) THEN
* LSE ->             FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
* LSE ->          ELSE
* LSE ->             FILNAM = CFDRAW
* LSE ->          END IF
* LSE ->          OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
* LSE ->      &          'UNFORMATTED' )
* LSE ->       END IF
* LSE ->       WRITE (IODRAW) -NCASE, NPFLKA, NSTMAX, SNGL (TKESUM),
* LSE ->      &                SNGL (WEIPRI)
* LSE -> *  +-------------------------------------------------------------------*
* LSE -> *  |  (Radioactive) isotope: it works only for 1 source particle on
* LSE -> *  |  the stack for the time being
* LSE ->       IF ( ILOFLK (NPFLKA) .GE. 100000 .AND. LRADDC (NPFLKA) ) THEN
* LSE ->          IARES  = MOD ( ILOFLK (NPFLKA), 100000  )  / 100
* LSE ->          IZRES  = MOD ( ILOFLK (NPFLKA), 10000000 ) / 100000
* LSE ->          IISRES = ILOFLK (NPFLKA) / 10000000
* LSE ->          IONID  = ILOFLK (NPFLKA)
* LSE ->          WRITE (IODRAW) ( IONID,SNGL(-TKEFLK(I)),
* LSE ->      &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
* LSE ->      &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
* LSE ->      &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
* LSE ->      &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
* LSE -> *  |
* LSE -> *  +-------------------------------------------------------------------*
* LSE -> *  |  Patch for heavy ions: it works only for 1 source particle on
* LSE -> *  |  the stack for the time being
* LSE ->       ELSE IF ( ABS (ILOFLK (NPFLKA)) .GE. 10000 ) THEN
* LSE ->          IONID = ILOFLK (NPFLKA)
* LSE ->          CALL DCDION ( IONID )
* LSE ->          WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-IONID)),
* LSE ->      &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
* LSE ->      &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
* LSE ->      &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
* LSE ->      &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
* LSE -> *  |
* LSE -> *  +-------------------------------------------------------------------*
* LSE -> *  |  Patch for heavy ions: ???
* LSE ->       ELSE IF ( ILOFLK (NPFLKA) .LT. -6 ) THEN
* LSE ->          WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-ILOFLK(NPFLKA))),
* LSE ->      &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
* LSE ->      &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
* LSE ->      &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
* LSE ->      &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
* LSE -> *  |
* LSE -> *  +-------------------------------------------------------------------*
* LSE -> *  |
* LSE ->       ELSE
* LSE ->          WRITE (IODRAW) ( ILOFLK(I), SNGL (TKEFLK(I)+AM(ILOFLK(I))),
* LSE ->      &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
* LSE ->      &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
* LSE ->      &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
* LSE ->      &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
* LSE ->       END IF
* LSE -> *  |
* LSE -> *  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     USer dependent DRAWing:                                          *
*                                                                      *
*     Icode = 10x: call from Kaskad                                    *
*             100: elastic   interaction secondaries                   *
*             101: inelastic interaction secondaries                   *
*             102: particle decay  secondaries                         *
*             103: delta ray  generation secondaries                   *
*             104: pair production secondaries                         *
*             105: bremsstrahlung  secondaries                         *
*             110: decay products                                      *
*     Icode = 20x: call from Emfsco                                    *
*             208: bremsstrahlung secondaries                          *
*             210: Moller secondaries                                  *
*             212: Bhabha secondaries                                  *
*             214: in-flight annihilation secondaries                  *
*             215: annihilation at rest   secondaries                  *
*             217: pair production        secondaries                  *
*             219: Compton scattering     secondaries                  *
*             221: photoelectric          secondaries                  *
*             225: Rayleigh scattering    secondaries                  *
*     Icode = 30x: call from Kasneu                                    *
*             300: interaction secondaries                             *
*     Icode = 40x: call from Kashea                                    *
*             400: delta ray  generation secondaries                   *
*  For all interactions secondaries are put on GENSTK common (kp=1,np) *
*  but for KASHEA delta ray generation where only the secondary elec-  *
*  tron is present and stacked on FLKSTK common for kp=npflka          *
*                                                                      *
*======================================================================*
*
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO )
* LSE ->       IF ( .NOT. LFCOPE ) THEN
* LSE ->          LFCOPE = .TRUE.
* LSE ->          IF ( KOMPUT .EQ. 2 ) THEN
* LSE ->             FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
* LSE ->          ELSE
* LSE ->             FILNAM = CFDRAW
* LSE ->          END IF
* LSE ->          OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
* LSE ->      &          'UNFORMATTED' )
* LSE ->       END IF
* No output by default:
      RETURN
*=== End of subrutine Mgdraw ==========================================*
      END

