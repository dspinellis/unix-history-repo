C***********************************************************************00010815
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020815
C*****   FM815                                                          00030815
C*****                       YCEXP - (180)                              00040815
C*****                                                                  00050815
C***********************************************************************00060815
C*****  GENERAL PURPOSE                                         ANS REF 00070815
C*****    TEST INTRINSIC FUNCTION CEXP                           15.3   00080815
C*****    INTRINSIC FUNCTIONS AIMAG AND CABS ASSUMED WORKING    TABLE 5 00090815
C*****                                                                  00100815
CBB** ********************** BBCCOMNT **********************************00110815
C****                                                                   00120815
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130815
C****                          VERSION 2.0                              00140815
C****                                                                   00150815
C****                                                                   00160815
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170815
C****                   GENERAL SERVICES ADMINISTRATION                 00180815
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190815
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200815
C****                      FALLS CHURCH, VA. 22041                      00210815
C****                                                                   00220815
C****                          (703) 756-6153                           00230815
C****                                                                   00240815
CBE** ********************** BBCCOMNT **********************************00250815
C*****                                                                  00260815
C*****  S P E C I F I C A T I O N S  SEGMENT 180                        00270815
        COMPLEX AVC, BVC, CVC, ZVCORR                                   00280815
        REAL R2E(2)                                                     00290815
        EQUIVALENCE (AVC, R2E)                                          00300815
C*****                                                                  00310815
CBB** ********************** BBCINITA **********************************00320815
C**** SPECIFICATION STATEMENTS                                          00330815
C****                                                                   00340815
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350815
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360815
CBE** ********************** BBCINITA **********************************00370815
CBB** ********************** BBCINITB **********************************00380815
C**** INITIALIZE SECTION                                                00390815
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400815
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410815
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420815
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430815
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440815
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450815
      DATA   REMRKS /'                               '/                 00460815
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470815
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480815
C****                                                                   00490815
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500815
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510815
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520815
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590815
      IVPASS = 0                                                        00600815
      IVFAIL = 0                                                        00610815
      IVDELE = 0                                                        00620815
      IVINSP = 0                                                        00630815
      IVTOTL = 0                                                        00640815
      IVTOTN = 0                                                        00650815
      ICZERO = 0                                                        00660815
C                                                                       00670815
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680815
      I01 = 05                                                          00690815
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700815
      I02 = 06                                                          00710815
C                                                                       00720815
      I01 = 5                                                           00730815
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740815
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750815
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760815
C                                                                       00770815
      I02 = 6                                                           00780815
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790815
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800815
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810815
C                                                                       00820815
CBE** ********************** BBCINITB **********************************00830815
      NUVI = I02                                                        00840815
      IVTOTL = 9                                                        00850815
      ZPROG = 'FM815'                                                   00860815
CBB** ********************** BBCHED0A **********************************00870815
C****                                                                   00880815
C**** WRITE REPORT TITLE                                                00890815
C****                                                                   00900815
      WRITE (I02, 90002)                                                00910815
      WRITE (I02, 90006)                                                00920815
      WRITE (I02, 90007)                                                00930815
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940815
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950815
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960815
CBE** ********************** BBCHED0A **********************************00970815
C*****                                                                  00980815
C*****    HEADER FOR SEGMENT 180                                        00990815
        WRITE(NUVI,18000)                                               01000815
18000   FORMAT(1H , / 35H  YCEXP - (180) INTRINSIC FUNCTIONS//          01010815
     1         28H  CEXP (COMPLEX EXPONENTIAL)//                        01020815
     2         17H  ANS REF. - 15.3)                                    01030815
CBB** ********************** BBCHED0B **********************************01040815
C**** WRITE DETAIL REPORT HEADERS                                       01050815
C****                                                                   01060815
      WRITE (I02,90004)                                                 01070815
      WRITE (I02,90004)                                                 01080815
      WRITE (I02,90013)                                                 01090815
      WRITE (I02,90014)                                                 01100815
      WRITE (I02,90015) IVTOTL                                          01110815
CBE** ********************** BBCHED0B **********************************01120815
C*****                                                                  01130815
CT001*  TEST 1                                                   ZERO   01140815
           IVTNUM = 1                                                   01150815
        BVC = (0.0, 0.0)                                                01160815
        AVC = CEXP(BVC)                                                 01170815
           IF (R2E(1) - 0.99995E+00) 20010, 40012, 40011                01180815
40011      IF (R2E(1) - 0.10001E+01) 40012, 40012, 20010                01190815
40012      IF (R2E(2) + 0.50000E-04) 20010, 10010, 40010                01200815
40010      IF (R2E(2) - 0.50000E-04) 10010, 10010, 20010                01210815
10010      IVPASS = IVPASS + 1                                          01220815
           WRITE (NUVI, 80002) IVTNUM                                   01230815
           GO TO 0011                                                   01240815
20010      IVFAIL = IVFAIL + 1                                          01250815
           ZVCORR = (1.0000000000000, 0.00000000000000)                 01260815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01270815
 0011      CONTINUE                                                     01280815
CT002*  TEST 2          PURELY REAL NUMBERS -- RESULT AGREES WITH EXP   01290815
           IVTNUM = 2                                                   01300815
        AVC = CEXP((1.0, 0.0))                                          01310815
           IF (R2E(1) - 0.27181E+01) 20020, 40022, 40021                01320815
40021      IF (R2E(1) - 0.27185E+01) 40022, 40022, 20020                01330815
40022      IF (R2E(2) + 0.50000E-04) 20020, 10020, 40020                01340815
40020      IF (R2E(2) - 0.50000E-04) 10020, 10020, 20020                01350815
10020      IVPASS = IVPASS + 1                                          01360815
           WRITE (NUVI, 80002) IVTNUM                                   01370815
           GO TO 0021                                                   01380815
20020      IVFAIL = IVFAIL + 1                                          01390815
           ZVCORR = (2.7182818284590, 0.00000000000000)                 01400815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01410815
 0021      CONTINUE                                                     01420815
CT003*  TEST 3          PURELY REAL NUMBERS -- RESULT AGREES WITH EXP   01430815
           IVTNUM = 3                                                   01440815
        BVC = (-3.0, 0.0)                                               01450815
        AVC = CEXP(BVC)                                                 01460815
           IF (R2E(1) - 0.49784E-01) 20030, 40032, 40031                01470815
40031      IF (R2E(1) - 0.49790E-01) 40032, 40032, 20030                01480815
40032      IF (R2E(2) + 0.50000E-04) 20030, 10030, 40030                01490815
40030      IF (R2E(2) - 0.50000E-04) 10030, 10030, 20030                01500815
10030      IVPASS = IVPASS + 1                                          01510815
           WRITE (NUVI, 80002) IVTNUM                                   01520815
           GO TO 0031                                                   01530815
20030      IVFAIL = IVFAIL + 1                                          01540815
           ZVCORR = (0.04978706836785, 0.00000000000000)                01550815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01560815
 0031      CONTINUE                                                     01570815
C*****    TESTS 4 AND 5 - PURELY IMAGINARY NUMBERS--RESULT LIES         01580815
C*****                    ON UNIT CIRCLE                                01590815
CT004*  TEST 4                                                 (0,PI)   01600815
           IVTNUM = 4                                                   01610815
        BVC = (0.0, 3.1415926536)                                       01620815
        AVC = CEXP(BVC * (1.0, 0.0))                                    01630815
           IF (R2E(1) + 0.10001E+01) 20040, 40042, 40041                01640815
40041      IF (R2E(1) + 0.99995E+00) 40042, 40042, 20040                01650815
40042      IF (R2E(2) + 0.50000E-04) 20040, 10040, 40040                01660815
40040      IF (R2E(2) - 0.50000E-04) 10040, 10040, 20040                01670815
10040      IVPASS = IVPASS + 1                                          01680815
           WRITE (NUVI, 80002) IVTNUM                                   01690815
           GO TO 0041                                                   01700815
20040      IVFAIL = IVFAIL + 1                                          01710815
           ZVCORR = (-1.0000000000000, 0.00000000000000)                01720815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01730815
 0041      CONTINUE                                                     01740815
CT005*  TEST 5                                              (0,-PI/2)   01750815
           IVTNUM = 5                                                   01760815
        BVC = (0.0, -3.1415926536)                                      01770815
        AVC = CEXP(BVC / (2.0, 0.0))                                    01780815
           IF (R2E(1) + 0.50000E-04) 20050, 40052, 40051                01790815
40051      IF (R2E(1) - 0.50000E-04) 40052, 40052, 20050                01800815
40052      IF (R2E(2) + 0.10001E+01) 20050, 10050, 40050                01810815
40050      IF (R2E(2) + 0.99995E+00) 10050, 10050, 20050                01820815
10050      IVPASS = IVPASS + 1                                          01830815
           WRITE (NUVI, 80002) IVTNUM                                   01840815
           GO TO 0051                                                   01850815
20050      IVFAIL = IVFAIL + 1                                          01860815
           ZVCORR = (0.00000000000000, -1.0000000000000)                01870815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01880815
 0051      CONTINUE                                                     01890815
CT006*  TEST 6                                             (2.5,PI/4)   01900815
           IVTNUM = 6                                                   01910815
        AVC = CEXP((1.0, 2.0))                                          01920815
           IF (R2E(1) + 0.11313E+01) 20060, 40062, 40061                01930815
40061      IF (R2E(1) + 0.11311E+01) 40062, 40062, 20060                01940815
40062      IF (R2E(2) - 0.24716E+01) 20060, 10060, 40060                01950815
40060      IF (R2E(2) - 0.24719E+01) 10060, 10060, 20060                01960815
10060      IVPASS = IVPASS + 1                                          01970815
           WRITE (NUVI, 80002) IVTNUM                                   01980815
           GO TO 0061                                                   01990815
20060      IVFAIL = IVFAIL + 1                                          02000815
           ZVCORR = (-1.1312043837568, 2.4717266720048)                 02010815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02020815
 0061      CONTINUE                                                     02030815
CT007*  TEST 7                              A VARIABLE SUPPLIED TO CEXP 02040815
           IVTNUM = 7                                                   02050815
        BVC = (-1.75, 4.625)                                            02060815
        AVC = CEXP(BVC)                                                 02070815
           IF (R2E(1) + 0.15168E-01) 20070, 40072, 40071                02080815
40071      IF (R2E(1) + 0.15165E-01) 40072, 40072, 20070                02090815
40072      IF (R2E(2) + 0.17312E+00) 20070, 10070, 40070                02100815
40070      IF (R2E(2) + 0.17310E+00) 10070, 10070, 20070                02110815
10070      IVPASS = IVPASS + 1                                          02120815
           WRITE (NUVI, 80002) IVTNUM                                   02130815
           GO TO 0071                                                   02140815
20070      IVFAIL = IVFAIL + 1                                          02150815
           ZVCORR = (-0.01516660638013, -0.17311082425206)              02160815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02170815
 0071      CONTINUE                                                     02180815
CT008*  TEST 8               POSITIVE REAL, NEGATIVE IMAGINARY ARGUMENT 02190815
           IVTNUM = 8                                                   02200815
        AVC = CEXP((5.5, -1.015625))                                    02210815
           IF (R2E(1) - 0.12896E+03) 20080, 40082, 40081                02220815
40081      IF (R2E(1) - 0.12898E+03) 40082, 40082, 20080                02230815
40082      IF (R2E(2) + 0.20796E+03) 20080, 10080, 40080                02240815
40080      IF (R2E(2) + 0.20793E+03) 10080, 10080, 20080                02250815
10080      IVPASS = IVPASS + 1                                          02260815
           WRITE (NUVI, 80002) IVTNUM                                   02270815
           GO TO 0081                                                   02280815
20080      IVFAIL = IVFAIL + 1                                          02290815
           ZVCORR = (128.97440219594, -207.94168724284)                 02300815
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02310815
 0081      CONTINUE                                                     02320815
CT009*  TEST 9                THE FUNCTION TOGETHER WITH AIMAG AND CABS 02330815
           IVTNUM = 9                                                   02340815
        BVC = (10.0, 3.1415926536)                                      02350815
        CVC = CEXP(BVC / (4.0, 0.0))                                    02360815
        AVS = (AIMAG(CVC) / CABS(CVC)) ** 2                             02370815
           IF (AVS - 0.49997E+00) 20090, 10090, 40090                   02380815
40090      IF (AVS - 0.50003E+00) 10090, 10090, 20090                   02390815
10090      IVPASS = IVPASS + 1                                          02400815
           WRITE (NUVI, 80002) IVTNUM                                   02410815
           GO TO 0091                                                   02420815
20090      IVFAIL = IVFAIL + 1                                          02430815
           RVCORR = 0.5000000                                           02440815
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02450815
 0091      CONTINUE                                                     02460815
C*****                                                                  02470815
CBB** ********************** BBCSUM0  **********************************02480815
C**** WRITE OUT TEST SUMMARY                                            02490815
C****                                                                   02500815
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02510815
      WRITE (I02, 90004)                                                02520815
      WRITE (I02, 90014)                                                02530815
      WRITE (I02, 90004)                                                02540815
      WRITE (I02, 90020) IVPASS                                         02550815
      WRITE (I02, 90022) IVFAIL                                         02560815
      WRITE (I02, 90024) IVDELE                                         02570815
      WRITE (I02, 90026) IVINSP                                         02580815
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02590815
CBE** ********************** BBCSUM0  **********************************02600815
CBB** ********************** BBCFOOT0 **********************************02610815
C**** WRITE OUT REPORT FOOTINGS                                         02620815
C****                                                                   02630815
      WRITE (I02,90016) ZPROG, ZPROG                                    02640815
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02650815
      WRITE (I02,90019)                                                 02660815
CBE** ********************** BBCFOOT0 **********************************02670815
CBB** ********************** BBCFMT0A **********************************02680815
C**** FORMATS FOR TEST DETAIL LINES                                     02690815
C****                                                                   02700815
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02710815
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02720815
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02730815
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02740815
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02750815
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02760815
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02770815
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02780815
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02790815
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02800815
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02810815
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02820815
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02830815
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02840815
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02850815
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02860815
80050 FORMAT (1H ,48X,A31)                                              02870815
CBE** ********************** BBCFMT0A **********************************02880815
CBB** ********************** BBCFMAT1 **********************************02890815
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02900815
C****                                                                   02910815
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02920815
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02930815
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02940815
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02950815
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02960815
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02970815
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02980815
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02990815
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03000815
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03010815
     21H(,F12.5,2H, ,F12.5,1H))                                         03020815
CBE** ********************** BBCFMAT1 **********************************03030815
CBB** ********************** BBCFMT0B **********************************03040815
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03050815
C****                                                                   03060815
90002 FORMAT (1H1)                                                      03070815
90004 FORMAT (1H )                                                      03080815
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03090815
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03100815
90008 FORMAT (1H ,21X,A13,A17)                                          03110815
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03120815
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03130815
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03140815
     1       7X,7HREMARKS,24X)                                          03150815
90014 FORMAT (1H ,46H----------------------------------------------,    03160815
     1        33H---------------------------------)                     03170815
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03180815
C****                                                                   03190815
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03200815
C****                                                                   03210815
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03220815
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03230815
     1        A13)                                                      03240815
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03250815
C****                                                                   03260815
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03270815
C****                                                                   03280815
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03290815
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03300815
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03310815
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03320815
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03330815
CBE** ********************** BBCFMT0B **********************************03340815
C*****                                                                  03350815
C*****    END OF TEST SEGMENT 180                                       03360815
      STOP                                                              03370815
      END                                                               03380815
                                                                        03390815
