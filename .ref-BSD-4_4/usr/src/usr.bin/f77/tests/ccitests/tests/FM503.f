C***********************************************************************00010503
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020503
C*****   FM503                                                          00030503
C*****                       BLKD2 - (261)                              00040503
C*****   THIS PROGRAM USES FM504 (UNNAMED BLOCK DATA SUBPROGRAM         00050503
C*****   AND SUBROUTINE SN505                                           00060503
C***********************************************************************00070503
C*****  TESTING OF BLOCK DATA SUBPROGRAMS                       ANS REF 00080503
C*****          DATA INTERNAL FORMS                               16    00090503
C*****  THIS SEGMENT USES SEGMENTS 702 AND 703, BLOCK DATA PROGRAM      00100503
C*****  FM504 AND SUBROUTINE SN505                                      00110503
C*****                                                                  00120503
CBB** ********************** BBCCOMNT **********************************00130503
C****                                                                   00140503
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150503
C****                          VERSION 2.0                              00160503
C****                                                                   00170503
C****                                                                   00180503
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190503
C****                   GENERAL SERVICES ADMINISTRATION                 00200503
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210503
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220503
C****                      FALLS CHURCH, VA. 22041                      00230503
C****                                                                   00240503
C****                          (703) 756-6153                           00250503
C****                                                                   00260503
CBE** ********************** BBCCOMNT **********************************00270503
C*****                                                                  00280503
C*****  S P E C I F I C A T I O N S  SEGMENT 261                        00290503
C*****                                                                  00300503
C*****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       00310503
C*****  DOUBLE PRECISION AXVD, DVCORR                                   00320503
C*****  COMPLEX AXVC, ZVCORR                                            00330503
C*****  LOGICAL AXVB                                                    00340503
C*****  CHARACTER*6 A6XVK, B6XVK, CVCORR                                00350503
C*****                                                                  00360503
C*****  COMMON /BLK9/ AXVS, BXVS, IXVI, AXVD, AXVC, AXVB                00370503
C*****  COMMON /BLK7/ A6XVK, B6XVK                                      00380503
C*****                                                                  00390503
C*****                                                                  00400503
        CALL SN505                                                      00410503
        STOP                                                            00420503
C*****                                                                  00430503
C*****          END OF TEST SEGMENT 261                                 00440503
        END                                                             00450503
C***********************************************************************00010504
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C***** FORTRAN 77                                                       00020504
C*****   FM504                  BDS2 (UNNAMED) - (702)                  00030504
C*****   THIS BLOCK DATA SUBPROGRAM IS USED BY FM503                    00040504
C***********************************************************************00050504
C*****                                                                  00060504
C***** GENERAL PURPOSE                                                  00070504
C*****          THIS SEGMENT CONTAINS A BLOCK DATA SUBPROGRAM THAT IS   00080504
C*****  TO BE RUN WITH TEST SEGMENT 261                                 00090504
C*****          THIS SEGMENT WILL TEST INTERNAL DATA FORMS, AS WELL     00100504
C*****  AS THE USE OF UNNAMED BLOCK DATA PROGRAM                        00110504
C*****                                                                  00120504
        BLOCK DATA                                                      00130504
C*****                                                                  00140504
C*****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       00150504
        DOUBLE PRECISION AXVD                                           00160504
        COMPLEX AXVC                                                    00170504
        LOGICAL AXVB                                                    00180504
        CHARACTER*6 A6XVK, B6XVK                                        00190504
C*****                                                                  00200504
        COMMON /BLK9/ AXVS, BXVS, IXVI, AXVD, AXVC, AXVB                00210504
        COMMON /BLK7/ A6XVK, B6XVK                                      00220504
        DATA AXVS, BXVS, IXVI, AXVD, AXVC, AXVB /34.25E-1, 43.23, 21,   00230504
     1       1.23456, (234.23, 34.9), .TRUE./                           00240504
        DATA A6XVK, B6XVK /'ABCDE', 'FGHIJK'/                           00250504
C*****                                                                  00260504
        END                                                             00270504
C***********************************************************************00010505
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C***** FORTRAN 77                                                       00020505
C*****   FM505                  BLKD2Q - (703)                          00030505
C*****   THIS SUBROUTINE IS CALLED BY FM503                             00040505
C***********************************************************************00050505
C*****                                                                  00060505
C***** GENERAL PURPOSE                                                  00070505
C*****  THIS SEGMENT IS TO BE RUN WITH TEST SEGMENT 261                 00080505
C*****     THIS SEGMENT CONTAINS A SUBROUTINE THAT CHECKS TO SEE IF     00090505
C*****  IF THE BLOCK DATA PROGRAM CORRECTLY INITIALIZED THE MANY        00100505
C*****  VARIABLES                                                       00110505
C*****                                                                  00120505
        SUBROUTINE SN505                                                00130505
C*****                                                                  00140505
C*****                                                                  00150505
C*****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       00160505
        DOUBLE PRECISION AXVD, DVCORR                                   00170505
        COMPLEX AXVC, ZVCORR                                            00180505
        LOGICAL AXVB                                                    00190505
        CHARACTER*6 A6XVK, B6XVK, CVCORR                                00200505
C*****                                                                  00210505
        COMMON /BLK9/ AXVS, BXVS, IXVI, AXVD, AXVC, AXVB                00220505
        COMMON /BLK7/ A6XVK, B6XVK                                      00230505
C*****  LOCAL DECLARATION                                               00240505
        DOUBLE PRECISION AVD                                            00250505
        COMPLEX AVC                                                     00260505
        REAL R2E(2)                                                     00270505
        EQUIVALENCE (AVC, R2E)                                          00280505
C*****                                                                  00290505
CBB** ********************** BBCINITA **********************************00300505
C**** SPECIFICATION STATEMENTS                                          00310505
C****                                                                   00320505
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330505
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340505
CBE** ********************** BBCINITA **********************************00350505
CBB** ********************** BBCINITB **********************************00360505
C**** INITIALIZE SECTION                                                00370505
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380505
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390505
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400505
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410505
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420505
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430505
      DATA   REMRKS /'                               '/                 00440505
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450505
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460505
C****                                                                   00470505
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480505
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490505
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500505
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570505
      IVPASS = 0                                                        00580505
      IVFAIL = 0                                                        00590505
      IVDELE = 0                                                        00600505
      IVINSP = 0                                                        00610505
      IVTOTL = 0                                                        00620505
      IVTOTN = 0                                                        00630505
      ICZERO = 0                                                        00640505
C                                                                       00650505
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660505
      I01 = 05                                                          00670505
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680505
      I02 = 06                                                          00690505
C                                                                       00700505
      I01 = 5                                                           00710505
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720505
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730505
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740505
C                                                                       00750505
      I02 = 6                                                           00760505
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770505
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780505
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790505
C                                                                       00800505
CBE** ********************** BBCINITB **********************************00810505
      NUVI = I02                                                        00820505
      IVTOTL = 8                                                        00830505
      ZPROG = 'FM503'                                                   00840505
CBB** ********************** BBCHED0A **********************************00850505
C****                                                                   00860505
C**** WRITE REPORT TITLE                                                00870505
C****                                                                   00880505
      WRITE (I02, 90002)                                                00890505
      WRITE (I02, 90006)                                                00900505
      WRITE (I02, 90007)                                                00910505
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920505
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930505
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940505
CBE** ********************** BBCHED0A **********************************00950505
        WRITE(NUVI,26100)                                               00960505
26100   FORMAT( 1H , /  39H BLKD2 - (261) BLOCK DATA SUBPROGRAM --//    00970505
     1          21H  DATA INTERNAL FORMS//                              00980505
     2          15H  ANS REF. - 16)                                     00990505
CBB** ********************** BBCHED0B **********************************01000505
C**** WRITE DETAIL REPORT HEADERS                                       01010505
C****                                                                   01020505
      WRITE (I02,90004)                                                 01030505
      WRITE (I02,90004)                                                 01040505
      WRITE (I02,90013)                                                 01050505
      WRITE (I02,90014)                                                 01060505
      WRITE (I02,90015) IVTOTL                                          01070505
CBE** ********************** BBCHED0B **********************************01080505
C*****                                                                  01090505
CT001*  TEST 1                            REAL VARIABLE - EXPONENT FORM 01100505
           IVTNUM = 1                                                   01110505
        AVS = AXVS                                                      01120505
           IF (AVS - 0.34248E+01) 20010, 10010, 40010                   01130505
40010      IF (AVS - 0.34252E+01) 10010, 10010, 20010                   01140505
10010      IVPASS = IVPASS + 1                                          01150505
           WRITE (NUVI, 80002) IVTNUM                                   01160505
           GO TO 0011                                                   01170505
20010      IVFAIL = IVFAIL + 1                                          01180505
           RVCORR = 34.25E-1                                            01190505
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01200505
 0011      CONTINUE                                                     01210505
CT002*  TEST 2                        REAL VARIABLE - NON EXPONENT FORM 01220505
           IVTNUM = 2                                                   01230505
        AVS = BXVS                                                      01240505
           IF (AVS - 0.43227E+02) 20020, 10020, 40020                   01250505
40020      IF (AVS - 0.43233E+02) 10020, 10020, 20020                   01260505
10020      IVPASS = IVPASS + 1                                          01270505
           WRITE (NUVI, 80002) IVTNUM                                   01280505
           GO TO 0021                                                   01290505
20020      IVFAIL = IVFAIL + 1                                          01300505
           RVCORR = 43.23                                               01310505
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01320505
 0021      CONTINUE                                                     01330505
CT003*  TEST 3                                         INTEGER VARIABLE 01340505
           IVTNUM = 3                                                   01350505
        IVI = IXVI                                                      01360505
           IF (IVI -    21) 20030, 10030, 20030                         01370505
10030      IVPASS = IVPASS + 1                                          01380505
           WRITE (NUVI, 80002) IVTNUM                                   01390505
           GO TO 0031                                                   01400505
20030      IVFAIL = IVFAIL + 1                                          01410505
           IVCORR =    21                                               01420505
           WRITE (NUVI, 80010) IVTNUM, IVI, IVCORR                      01430505
 0031      CONTINUE                                                     01440505
CT004*  TEST 4                                DOUBLE PRECISION VARIABLE 01450505
           IVTNUM = 4                                                   01460505
        AVD = AXVD                                                      01470505
           IF (AVD - 0.12345D+01) 20040, 10040, 40040                   01480505
40040      IF (AVD - 0.12347D+01) 10040, 10040, 20040                   01490505
10040      IVPASS = IVPASS + 1                                          01500505
           WRITE (NUVI, 80002) IVTNUM                                   01510505
           GO TO 0041                                                   01520505
20040      IVFAIL = IVFAIL + 1                                          01530505
           DVCORR = 1.23456D+0                                          01540505
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01550505
 0041      CONTINUE                                                     01560505
CT005*  TEST 5                                         COMPLEX VARIABLE 01570505
           IVTNUM = 5                                                   01580505
        AVC = AXVC                                                      01590505
           IF (R2E(1) - 0.23421E+03) 20050, 40052, 40051                01600505
40051      IF (R2E(1) - 0.23425E+03) 40052, 40052, 20050                01610505
40052      IF (R2E(2) - 0.34898E+02) 20050, 10050, 40050                01620505
40050      IF (R2E(2) - 0.34902E+02) 10050, 10050, 20050                01630505
10050      IVPASS = IVPASS + 1                                          01640505
           WRITE (NUVI, 80002) IVTNUM                                   01650505
           GO TO 0051                                                   01660505
20050      IVFAIL = IVFAIL + 1                                          01670505
           ZVCORR = (234.23, 34.9)                                      01680505
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01690505
 0051      CONTINUE                                                     01700505
CT006*  TEST 6                                         LOGICAL VARIABLE 01710505
           IVTNUM = 6                                                   01720505
           IVI = 0                                                      01730505
           IF (AXVB) IVI = 1                                            01740505
           IF (IVI - 1) 20060, 10060, 20060                             01750505
10060      IVPASS = IVPASS + 1                                          01760505
           WRITE (NUVI, 80002) IVTNUM                                   01770505
           GO TO 0061                                                   01780505
20060      IVFAIL = IVFAIL + 1                                          01790505
           LVCORR = 1                                                   01800505
           REMRKS = '1 = TRUE ;  0 = FALSE'                             01810505
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           01820505
           WRITE (NUVI, 80024) IVI                                      01830505
           WRITE (NUVI, 80026) LVCORR                                   01840505
 0061      CONTINUE                                                     01850505
CT007*  TEST 7            6 CHARACTER VARIABLE - INIT WITH 5 CHARACTERS 01860505
           IVTNUM = 7                                                   01870505
           IVI = 0                                                      01880505
           IF (A6XVK.EQ.'ABCDE ') IVI = 1                               01890505
           IF (IVI - 1) 20070, 10070, 20070                             01900505
10070      IVPASS = IVPASS + 1                                          01910505
           WRITE (NUVI, 80002) IVTNUM                                   01920505
           GO TO 0071                                                   01930505
20070      IVFAIL = IVFAIL + 1                                          01940505
           CVCORR = 'ABCDE '                                            01950505
           WRITE (NUVI, 80018) IVTNUM, A6XVK, CVCORR                    01960505
 0071      CONTINUE                                                     01970505
CT008*  TEST 8            6 CHARACTER VARIABLE - INIT WITH 6 CHARACTERS 01980505
           IVTNUM = 8                                                   01990505
           IVI = 0                                                      02000505
           IF (B6XVK.EQ.'FGHIJK') IVI = 1                               02010505
           IF (IVI - 1) 20080, 10080, 20080                             02020505
10080      IVPASS = IVPASS + 1                                          02030505
           WRITE (NUVI, 80002) IVTNUM                                   02040505
           GO TO 0081                                                   02050505
20080      IVFAIL = IVFAIL + 1                                          02060505
           CVCORR = 'FGHIJK'                                            02070505
           WRITE (NUVI, 80018) IVTNUM, B6XVK, CVCORR                    02080505
 0081      CONTINUE                                                     02090505
C*****                                                                  02100505
CBB** ********************** BBCSUM0  **********************************02110505
C**** WRITE OUT TEST SUMMARY                                            02120505
C****                                                                   02130505
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02140505
      WRITE (I02, 90004)                                                02150505
      WRITE (I02, 90014)                                                02160505
      WRITE (I02, 90004)                                                02170505
      WRITE (I02, 90020) IVPASS                                         02180505
      WRITE (I02, 90022) IVFAIL                                         02190505
      WRITE (I02, 90024) IVDELE                                         02200505
      WRITE (I02, 90026) IVINSP                                         02210505
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02220505
CBE** ********************** BBCSUM0  **********************************02230505
CBB** ********************** BBCFOOT0 **********************************02240505
C**** WRITE OUT REPORT FOOTINGS                                         02250505
C****                                                                   02260505
      WRITE (I02,90016) ZPROG, ZPROG                                    02270505
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02280505
      WRITE (I02,90019)                                                 02290505
CBE** ********************** BBCFOOT0 **********************************02300505
CBB** ********************** BBCFMT0A **********************************02310505
C**** FORMATS FOR TEST DETAIL LINES                                     02320505
C****                                                                   02330505
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02340505
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02350505
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02360505
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02370505
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02380505
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02390505
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02400505
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02410505
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02420505
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02430505
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02440505
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02450505
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02460505
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02470505
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02480505
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02490505
80050 FORMAT (1H ,48X,A31)                                              02500505
CBE** ********************** BBCFMT0A **********************************02510505
CBB** ********************** BBCFMAT1 **********************************02520505
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02530505
C****                                                                   02540505
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02550505
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02560505
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02570505
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02580505
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02590505
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02600505
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02610505
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02620505
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02630505
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02640505
     21H(,F12.5,2H, ,F12.5,1H))                                         02650505
CBE** ********************** BBCFMAT1 **********************************02660505
CBB** ********************** BBCFMT0B **********************************02670505
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02680505
C****                                                                   02690505
90002 FORMAT (1H1)                                                      02700505
90004 FORMAT (1H )                                                      02710505
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02720505
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02730505
90008 FORMAT (1H ,21X,A13,A17)                                          02740505
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02750505
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02760505
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02770505
     1       7X,7HREMARKS,24X)                                          02780505
90014 FORMAT (1H ,46H----------------------------------------------,    02790505
     1        33H---------------------------------)                     02800505
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02810505
C****                                                                   02820505
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02830505
C****                                                                   02840505
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02850505
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02860505
     1        A13)                                                      02870505
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02880505
C****                                                                   02890505
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02900505
C****                                                                   02910505
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02920505
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02930505
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02940505
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02950505
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02960505
CBE** ********************** BBCFMT0B **********************************02970505
C*****                                                                  02980505
        END                                                             02990505
