C***********************************************************************00010828
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020828
C*****   FM828                                                          00030828
C*****                       YCFOR - (203)                              00040828
C*****                                                                  00050828
C***********************************************************************00060828
C*****  GENERAL PURPOSE                                         ANS REF 00070828
C*****    TEST COMPLEX TRIGONOMETRIC FORMULAE                   15.3    00080828
C*****                                                          TABLE 5 00090828
CBB** ********************** BBCCOMNT **********************************00100828
C****                                                                   00110828
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120828
C****                          VERSION 2.0                              00130828
C****                                                                   00140828
C****                                                                   00150828
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160828
C****                   GENERAL SERVICES ADMINISTRATION                 00170828
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180828
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190828
C****                      FALLS CHURCH, VA. 22041                      00200828
C****                                                                   00210828
C****                          (703) 756-6153                           00220828
C****                                                                   00230828
CBE** ********************** BBCCOMNT **********************************00240828
C*****                                                                  00250828
C*****  S P E C I F I C A T I O N S  SEGMENT 203                        00260828
        COMPLEX AVC, BVC, CVC, DVC, ZVCORR                              00270828
        REAL R2E(2)                                                     00280828
        EQUIVALENCE (AVC, R2E)                                          00290828
C*****                                                                  00300828
CBB** ********************** BBCINITA **********************************00310828
C**** SPECIFICATION STATEMENTS                                          00320828
C****                                                                   00330828
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00340828
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00350828
CBE** ********************** BBCINITA **********************************00360828
CBB** ********************** BBCINITB **********************************00370828
C**** INITIALIZE SECTION                                                00380828
      DATA  ZVERS,                  ZVERSD,             ZDATE           00390828
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00400828
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00410828
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00420828
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00430828
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00440828
      DATA   REMRKS /'                               '/                 00450828
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00460828
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00470828
C****                                                                   00480828
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00490828
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00500828
CZ03  ZPROG  = 'PROGRAM NAME'                                           00510828
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00580828
      IVPASS = 0                                                        00590828
      IVFAIL = 0                                                        00600828
      IVDELE = 0                                                        00610828
      IVINSP = 0                                                        00620828
      IVTOTL = 0                                                        00630828
      IVTOTN = 0                                                        00640828
      ICZERO = 0                                                        00650828
C                                                                       00660828
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00670828
      I01 = 05                                                          00680828
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00690828
      I02 = 06                                                          00700828
C                                                                       00710828
      I01 = 5                                                           00720828
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00730828
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00740828
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00750828
C                                                                       00760828
      I02 = 6                                                           00770828
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00780828
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00790828
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00800828
C                                                                       00810828
CBE** ********************** BBCINITB **********************************00820828
      NUVI = I02                                                        00830828
      IVTOTL = 9                                                        00840828
      ZPROG = 'FM828'                                                   00850828
CBB** ********************** BBCHED0A **********************************00860828
C****                                                                   00870828
C**** WRITE REPORT TITLE                                                00880828
C****                                                                   00890828
      WRITE (I02, 90002)                                                00900828
      WRITE (I02, 90006)                                                00910828
      WRITE (I02, 90007)                                                00920828
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00930828
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00940828
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00950828
CBE** ********************** BBCHED0A **********************************00960828
C*****                                                                  00970828
C*****    HEADER FOR SEGMENT 203                                        00980828
        WRITE(NUVI,20300)                                               00990828
20300   FORMAT(1H , / 35H  YCFOR - (203) INTRINSIC FUNCTIONS//          01000828
     1         32H  COMPLEX TRIGONOMETRIC FORMULAE//                    01010828
     2         17H  ANS REF. - 15.3)                                    01020828
CBB** ********************** BBCHED0B **********************************01030828
C**** WRITE DETAIL REPORT HEADERS                                       01040828
C****                                                                   01050828
      WRITE (I02,90004)                                                 01060828
      WRITE (I02,90004)                                                 01070828
      WRITE (I02,90013)                                                 01080828
      WRITE (I02,90014)                                                 01090828
      WRITE (I02,90015) IVTOTL                                          01100828
CBE** ********************** BBCHED0B **********************************01110828
C*****                                                                  01120828
        PIVS = 3.1415926535897932384626434                              01130828
C*****                                                                  01140828
CT001*  TEST 1                                           SQRT(Z)**2 = Z 01150828
           IVTNUM = 1                                                   01160828
        BVC = (1.0, 0.0) + (0.0, -2.5)                                  01170828
        AVC = CSQRT((1.0, -2.5)) ** 2 - BVC                             01180828
           IF (R2E(1) + 0.50000E-04) 20010, 40012, 40011                01190828
40011      IF (R2E(1) - 0.50000E-04) 40012, 40012, 20010                01200828
40012      IF (R2E(2) + 0.50000E-04) 20010, 10010, 40010                01210828
40010      IF (R2E(2) - 0.50000E-04) 10010, 10010, 20010                01220828
10010      IVPASS = IVPASS + 1                                          01230828
           WRITE (NUVI, 80002) IVTNUM                                   01240828
           GO TO 0011                                                   01250828
20010      IVFAIL = IVFAIL + 1                                          01260828
           ZVCORR = (0.0000, 0.0000)                                    01270828
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01280828
 0011      CONTINUE                                                     01290828
CT002*  TEST 2   ANGLE SUBTENDED BY SQRT(Z) IS 1/2 ANGLE SUBTENDED BY Z 01300828
           IVTNUM = 2                                                   01310828
        BVC = CSQRT((2.0, 3.25))                                        01320828
        CVS = AIMAG(BVC)                                                01330828
        DVS = CABS((BVC + CONJG(BVC)) / (2.0, 0.0))                     01340828
        AVS = ATAN2(3.0 + 0.25, 1.0 * 2.0) - 2.0 * ATAN2(CVS, DVS)      01350828
           IF (AVS + 0.50000E-04) 20020, 10020, 40020                   01360828
40020      IF (AVS - 0.50000E-04) 10020, 10020, 20020                   01370828
10020      IVPASS = IVPASS + 1                                          01380828
           WRITE (NUVI, 80002) IVTNUM                                   01390828
           GO TO 0021                                                   01400828
20020      IVFAIL = IVFAIL + 1                                          01410828
           RVCORR = 0.0000                                              01420828
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01430828
 0021      CONTINUE                                                     01440828
CT003*  TEST 3                                          EXP(LOG(Z)) = Z 01450828
           IVTNUM = 3                                                   01460828
        BVC = (0.0, 0.0) - (1.5, 0.75)                                  01470828
        AVC = CEXP(CLOG(BVC)) + (1.5, 0.75)                             01480828
           IF (R2E(1) + 0.50000E-04) 20030, 40032, 40031                01490828
40031      IF (R2E(1) - 0.50000E-04) 40032, 40032, 20030                01500828
40032      IF (R2E(2) + 0.50000E-04) 20030, 10030, 40030                01510828
40030      IF (R2E(2) - 0.50000E-04) 10030, 10030, 20030                01520828
10030      IVPASS = IVPASS + 1                                          01530828
           WRITE (NUVI, 80002) IVTNUM                                   01540828
           GO TO 0031                                                   01550828
20030      IVFAIL = IVFAIL + 1                                          01560828
           ZVCORR = (0.0000, 0.0000)                                    01570828
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01580828
 0031      CONTINUE                                                     01590828
CT004*  TEST 4                               ABS(EXP(Z)) = EXP(REAL(Z)) 01600828
           IVTNUM = 4                                                   01610828
        AVS = CABS(CEXP((-2.5, 1.375))) - EXP(5.0 / (-2.0))             01620828
           IF (AVS + 0.50000E-04) 20040, 10040, 40040                   01630828
40040      IF (AVS - 0.50000E-04) 10040, 10040, 20040                   01640828
10040      IVPASS = IVPASS + 1                                          01650828
           WRITE (NUVI, 80002) IVTNUM                                   01660828
           GO TO 0041                                                   01670828
20040      IVFAIL = IVFAIL + 1                                          01680828
           RVCORR = 0.0000                                              01690828
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01700828
 0041      CONTINUE                                                     01710828
CT005*  TEST 5            ANGLE SUBTENDED BY EXP(Z) IS IMAG(Z) MOD 2 PI 01720828
           IVTNUM = 5                                                   01730828
        BVC = (0.0625, 0.0)                                             01740828
        CVC = CEXP(BVC + (0.0, 1.125))                                  01750828
        DVS = ATAN2(AIMAG(CVC), CABS((CVC + CONJG(CVC)) / (2.0, 0.0)))  01760828
        AVS = DVS - AMOD(AIMAG((0.0625, 1.125)), 2.0 * PIVS)            01770828
           IF (AVS + 0.50000E-04) 20050, 10050, 40050                   01780828
40050      IF (AVS - 0.50000E-04) 10050, 10050, 20050                   01790828
10050      IVPASS = IVPASS + 1                                          01800828
           WRITE (NUVI, 80002) IVTNUM                                   01810828
           GO TO 0051                                                   01820828
20050      IVFAIL = IVFAIL + 1                                          01830828
           RVCORR = 0.0000                                              01840828
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01850828
 0051      CONTINUE                                                     01860828
CT006*  TEST 6                              EXP(IY) = COS(Y) + I SIN(Y) 01870828
           IVTNUM = 6                                                   01880828
        AVC = CEXP(CMPLX(0.0, 37.5 / 10.0))                             01890828
     1        - CMPLX(COS(3.75), SIN(2.75 + 1.0))                       01900828
           IF (R2E(1) + 0.50000E-04) 20060, 40062, 40061                01910828
40061      IF (R2E(1) - 0.50000E-04) 40062, 40062, 20060                01920828
40062      IF (R2E(2) + 0.50000E-04) 20060, 10060, 40060                01930828
40060      IF (R2E(2) - 0.50000E-04) 10060, 10060, 20060                01940828
10060      IVPASS = IVPASS + 1                                          01950828
           WRITE (NUVI, 80002) IVTNUM                                   01960828
           GO TO 0061                                                   01970828
20060      IVFAIL = IVFAIL + 1                                          01980828
           ZVCORR = (0.0000, 0.0000)                                    01990828
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02000828
 0061      CONTINUE                                                     02010828
CT007*  TEST 7                    COS(Z) = 0.5 * (EXP(I*Z) + EXP(-I*Z)) 02020828
           IVTNUM = 7                                                   02030828
         BVC = CEXP((-1.5, -2.75))                                      02040828
         CVC = (BVC + 1 / BVC) / (2.0, 0.0)                             02050828
         DVC = (2.75, -1.5)                                             02060828
         AVC = CVC - CCOS(DVC * (-1.0, 0.0))                            02070828
            IF (R2E(1) + 0.50000E-04) 20070, 40072, 40071               02080828
40071       IF (R2E(1) - 0.50000E-04) 40072, 40072, 20070               02090828
40072       IF (R2E(2) + 0.50000E-04) 20070, 10070, 40070               02100828
40070       IF (R2E(2) - 0.50000E-04) 10070, 10070, 20070               02110828
10070       IVPASS = IVPASS + 1                                         02120828
            WRITE (NUVI, 80002) IVTNUM                                  02130828
            GO TO 0071                                                  02140828
20070       IVFAIL = IVFAIL + 1                                         02150828
            ZVCORR = (0.0000, 0.0000)                                   02160828
            WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                     02170828
 0071       CONTINUE                                                    02180828
CT008*  TEST 8                                       ABS(EXP(IY)) = 1.0 02190828
           IVTNUM = 8                                                   02200828
        BVC = (3.25, 3.25)                                              02210828
        CVC = (3.25, 0.0)                                               02220828
        AVC = CABS(CEXP(BVC - CVC)) - COS(0.0)                          02230828
           IF (R2E(1) + 0.50000E-04) 20080, 40082, 40081                02240828
40081      IF (R2E(1) - 0.50000E-04) 40082, 40082, 20080                02250828
40082      IF (R2E(2) + 0.50000E-04) 20080, 10080, 40080                02260828
40080      IF (R2E(2) - 0.50000E-04) 10080, 10080, 20080                02270828
10080      IVPASS = IVPASS + 1                                          02280828
           WRITE (NUVI, 80002) IVTNUM                                   02290828
           GO TO 0081                                                   02300828
20080      IVFAIL = IVFAIL + 1                                          02310828
           ZVCORR = (0.0000, 0.0000)                                    02320828
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02330828
 0081      CONTINUE                                                     02340828
CT009*  TEST 9                               DEMOIVRE THEOREM FOR N = 3 02350828
           IVTNUM = 9                                                   02360828
        BVS = 3.0/2.0                                                   02370828
        BVC = CMPLX(COS(1.5), SIN(BVS)) ** 3                            02380828
        AVC = BVC - CMPLX(COS(4.5), -SIN(4.5 + PIVS))                   02390828
           IF (R2E(1) + 0.50000E-04) 20090, 40092, 40091                02400828
40091      IF (R2E(1) - 0.50000E-04) 40092, 40092, 20090                02410828
40092      IF (R2E(2) + 0.50000E-04) 20090, 10090, 40090                02420828
40090      IF (R2E(2) - 0.50000E-04) 10090, 10090, 20090                02430828
10090      IVPASS = IVPASS + 1                                          02440828
           WRITE (NUVI, 80002) IVTNUM                                   02450828
           GO TO 0091                                                   02460828
20090      IVFAIL = IVFAIL + 1                                          02470828
           ZVCORR = (0.0000, 0.0000)                                    02480828
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02490828
 0091      CONTINUE                                                     02500828
C*****                                                                  02510828
CBB** ********************** BBCSUM0  **********************************02520828
C**** WRITE OUT TEST SUMMARY                                            02530828
C****                                                                   02540828
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02550828
      WRITE (I02, 90004)                                                02560828
      WRITE (I02, 90014)                                                02570828
      WRITE (I02, 90004)                                                02580828
      WRITE (I02, 90020) IVPASS                                         02590828
      WRITE (I02, 90022) IVFAIL                                         02600828
      WRITE (I02, 90024) IVDELE                                         02610828
      WRITE (I02, 90026) IVINSP                                         02620828
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02630828
CBE** ********************** BBCSUM0  **********************************02640828
CBB** ********************** BBCFOOT0 **********************************02650828
C**** WRITE OUT REPORT FOOTINGS                                         02660828
C****                                                                   02670828
      WRITE (I02,90016) ZPROG, ZPROG                                    02680828
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02690828
      WRITE (I02,90019)                                                 02700828
CBE** ********************** BBCFOOT0 **********************************02710828
CBB** ********************** BBCFMT0A **********************************02720828
C**** FORMATS FOR TEST DETAIL LINES                                     02730828
C****                                                                   02740828
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02750828
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02760828
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02770828
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02780828
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02790828
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02800828
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02810828
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02820828
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02830828
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02840828
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02850828
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02860828
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02870828
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02880828
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02890828
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02900828
80050 FORMAT (1H ,48X,A31)                                              02910828
CBE** ********************** BBCFMT0A **********************************02920828
CBB** ********************** BBCFMAT1 **********************************02930828
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02940828
C****                                                                   02950828
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02960828
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02970828
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02980828
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02990828
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03000828
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03010828
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03020828
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03030828
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03040828
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03050828
     21H(,F12.5,2H, ,F12.5,1H))                                         03060828
CBE** ********************** BBCFMAT1 **********************************03070828
CBB** ********************** BBCFMT0B **********************************03080828
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03090828
C****                                                                   03100828
90002 FORMAT (1H1)                                                      03110828
90004 FORMAT (1H )                                                      03120828
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03130828
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03140828
90008 FORMAT (1H ,21X,A13,A17)                                          03150828
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03160828
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03170828
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03180828
     1       7X,7HREMARKS,24X)                                          03190828
90014 FORMAT (1H ,46H----------------------------------------------,    03200828
     1        33H---------------------------------)                     03210828
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03220828
C****                                                                   03230828
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03240828
C****                                                                   03250828
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03260828
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03270828
     1        A13)                                                      03280828
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03290828
C****                                                                   03300828
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03310828
C****                                                                   03320828
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03330828
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03340828
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03350828
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03360828
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03370828
CBE** ********************** BBCFMT0B **********************************03380828
C*****                                                                  03390828
C*****    END OF TEST SEGMENT 203                                       03400828
      STOP                                                              03410828
      END                                                               03420828
                                                                        03430828
