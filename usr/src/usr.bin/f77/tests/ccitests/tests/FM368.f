C***********************************************************************00010368
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020368
C*****   FM368                                                          00030368
C*****                       XSQRT - (175)                              00040368
C*****                                                                  00050368
C***********************************************************************00060368
C*****  GENERAL PURPOSE                                      SUBSET REF 00070368
C*****    TEST INTRINSIC FUNCTION SQRT                         15.3     00080368
C*****                                                        TABLE 5   00090368
C*****                                                                  00100368
CBB** ********************** BBCCOMNT **********************************00110368
C****                                                                   00120368
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130368
C****                          VERSION 2.0                              00140368
C****                                                                   00150368
C****                                                                   00160368
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170368
C****                   GENERAL SERVICES ADMINISTRATION                 00180368
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190368
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200368
C****                      FALLS CHURCH, VA. 22041                      00210368
C****                                                                   00220368
C****                          (703) 756-6153                           00230368
C****                                                                   00240368
CBE** ********************** BBCCOMNT **********************************00250368
CBB** ********************** BBCINITA **********************************00260368
C**** SPECIFICATION STATEMENTS                                          00270368
C****                                                                   00280368
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290368
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300368
CBE** ********************** BBCINITA **********************************00310368
CBB** ********************** BBCINITB **********************************00320368
C**** INITIALIZE SECTION                                                00330368
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340368
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350368
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360368
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370368
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380368
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390368
      DATA   REMRKS /'                               '/                 00400368
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410368
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420368
C****                                                                   00430368
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440368
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450368
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460368
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530368
      IVPASS = 0                                                        00540368
      IVFAIL = 0                                                        00550368
      IVDELE = 0                                                        00560368
      IVINSP = 0                                                        00570368
      IVTOTL = 0                                                        00580368
      IVTOTN = 0                                                        00590368
      ICZERO = 0                                                        00600368
C                                                                       00610368
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620368
      I01 = 05                                                          00630368
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640368
      I02 = 06                                                          00650368
C                                                                       00660368
      I01 = 5                                                           00670368
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680368
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690368
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700368
C                                                                       00710368
      I02 = 6                                                           00720368
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730368
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740368
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750368
C                                                                       00760368
CBE** ********************** BBCINITB **********************************00770368
      NUVI = I02                                                        00780368
      IVTOTL = 13                                                       00790368
      ZPROG = 'FM368'                                                   00800368
CBB** ********************** BBCHED0A **********************************00810368
C****                                                                   00820368
C**** WRITE REPORT TITLE                                                00830368
C****                                                                   00840368
      WRITE (I02, 90002)                                                00850368
      WRITE (I02, 90006)                                                00860368
      WRITE (I02, 90007)                                                00870368
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880368
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890368
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900368
CBE** ********************** BBCHED0A **********************************00910368
C*****                                                                  00920368
C*****    HEADER FOR SEGMENT 175                                        00930368
        WRITE(NUVI,17500)                                               00940368
17500   FORMAT(1H , / 35H  XSQRT - (175) INTRINSIC FUNCTIONS//          00950368
     1         20H  SQRT (SQUARE ROOT)//                                00960368
     2         20H  SUBSET REF. - 15.3)                                 00970368
CBB** ********************** BBCHED0B **********************************00980368
C**** WRITE DETAIL REPORT HEADERS                                       00990368
C****                                                                   01000368
      WRITE (I02,90004)                                                 01010368
      WRITE (I02,90004)                                                 01020368
      WRITE (I02,90013)                                                 01030368
      WRITE (I02,90014)                                                 01040368
      WRITE (I02,90015) IVTOTL                                          01050368
CBE** ********************** BBCHED0B **********************************01060368
C*****                                                                  01070368
CT001*  TEST 1                                FIXED POINT OF FUNCTION   01080368
           IVTNUM = 1                                                   01090368
        BVS = 0.0                                                       01100368
        AVS = SQRT(BVS)                                                 01110368
           IF (AVS + 0.50000E-04) 20010, 10010, 40010                   01120368
40010      IF (AVS - 0.50000E-04) 10010, 10010, 20010                   01130368
10010      IVPASS = IVPASS + 1                                          01140368
           WRITE (NUVI, 80002) IVTNUM                                   01150368
           GO TO 0011                                                   01160368
20010      IVFAIL = IVFAIL + 1                                          01170368
           RVCORR = 0.00000000000000                                    01180368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01190368
 0011      CONTINUE                                                     01200368
CT002*  TEST 2                                FIXED POINT OF FUNCTION   01210368
           IVTNUM = 2                                                   01220368
        AVS = SQRT(1.0)                                                 01230368
           IF (AVS - 0.99995E+00) 20020, 10020, 40020                   01240368
40020      IF (AVS - 0.10001E+01) 10020, 10020, 20020                   01250368
10020      IVPASS = IVPASS + 1                                          01260368
           WRITE (NUVI, 80002) IVTNUM                                   01270368
           GO TO 0021                                                   01280368
20020      IVFAIL = IVFAIL + 1                                          01290368
           RVCORR = 1.00000000000000                                    01300368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01310368
 0021      CONTINUE                                                     01320368
CT003*  TEST 3                                                          01330368
           IVTNUM = 3                                                   01340368
        AVS = SQRT(2.0)                                                 01350368
           IF (AVS - 0.14141E+01) 20030, 10030, 40030                   01360368
40030      IF (AVS - 0.14143E+01) 10030, 10030, 20030                   01370368
10030      IVPASS = IVPASS + 1                                          01380368
           WRITE (NUVI, 80002) IVTNUM                                   01390368
           GO TO 0031                                                   01400368
20030      IVFAIL = IVFAIL + 1                                          01410368
           RVCORR = 1.41421356237310                                    01420368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01430368
 0031      CONTINUE                                                     01440368
CT004*  TEST 4                                                          01450368
           IVTNUM = 4                                                   01460368
        AVS = SQRT(4.0)                                                 01470368
           IF (AVS - 0.19999E+01) 20040, 10040, 40040                   01480368
40040      IF (AVS - 0.20001E+01) 10040, 10040, 20040                   01490368
10040      IVPASS = IVPASS + 1                                          01500368
           WRITE (NUVI, 80002) IVTNUM                                   01510368
           GO TO 0041                                                   01520368
20040      IVFAIL = IVFAIL + 1                                          01530368
           RVCORR = 2.00000000000000                                    01540368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01550368
 0041      CONTINUE                                                     01560368
CT005*  TEST 5                                                          01570368
           IVTNUM = 5                                                   01580368
        AVS = SQRT(15.0)                                                01590368
           IF (AVS - 0.38727E+01) 20050, 10050, 40050                   01600368
40050      IF (AVS - 0.38732E+01) 10050, 10050, 20050                   01610368
10050      IVPASS = IVPASS + 1                                          01620368
           WRITE (NUVI, 80002) IVTNUM                                   01630368
           GO TO 0051                                                   01640368
20050      IVFAIL = IVFAIL + 1                                          01650368
           RVCORR = 3.87298334620742                                    01660368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01670368
 0051      CONTINUE                                                     01680368
CT006*  TEST 6                                                          01690368
           IVTNUM = 6                                                   01700368
        AVS = SQRT(31.0)                                                01710368
           IF (AVS - 0.55674E+01) 20060, 10060, 40060                   01720368
40060      IF (AVS - 0.55681E+01) 10060, 10060, 20060                   01730368
10060      IVPASS = IVPASS + 1                                          01740368
           WRITE (NUVI, 80002) IVTNUM                                   01750368
           GO TO 0061                                                   01760368
20060      IVFAIL = IVFAIL + 1                                          01770368
           RVCORR = 5.56776436283002                                    01780368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01790368
 0061      CONTINUE                                                     01800368
CT007*  TEST 7                                                          01810368
           IVTNUM = 7                                                   01820368
        BVS = 2.0/4.0                                                   01830368
        AVS = SQRT(BVS)                                                 01840368
           IF (AVS - 0.70707E+00) 20070, 10070, 40070                   01850368
40070      IF (AVS - 0.70715E+00) 10070, 10070, 20070                   01860368
10070      IVPASS = IVPASS + 1                                          01870368
           WRITE (NUVI, 80002) IVTNUM                                   01880368
           GO TO 0071                                                   01890368
20070      IVFAIL = IVFAIL + 1                                          01900368
           RVCORR = 0.70710678118655                                    01910368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01920368
 0071      CONTINUE                                                     01930368
CT008*  TEST 8                                                          01940368
           IVTNUM = 8                                                   01950368
        BVS = 25.0                                                      01960368
        AVS = SQRT(BVS/100.0)                                           01970368
           IF (AVS - 0.49997E+00) 20080, 10080, 40080                   01980368
40080      IF (AVS - 0.50003E+00) 10080, 10080, 20080                   01990368
10080      IVPASS = IVPASS + 1                                          02000368
           WRITE (NUVI, 80002) IVTNUM                                   02010368
           GO TO 0081                                                   02020368
20080      IVFAIL = IVFAIL + 1                                          02030368
           RVCORR = 0.50000000000000                                    02040368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02050368
 0081      CONTINUE                                                     02060368
CT009*  TEST 9                                                          02070368
           IVTNUM = 9                                                   02080368
        BVS = 0.0875                                                    02090368
        AVS = SQRT(BVS * 10.0)                                          02100368
           IF (AVS - 0.93536E+00) 20090, 10090, 40090                   02110368
40090      IF (AVS - 0.93546E+00) 10090, 10090, 20090                   02120368
10090      IVPASS = IVPASS + 1                                          02130368
           WRITE (NUVI, 80002) IVTNUM                                   02140368
           GO TO 0091                                                   02150368
20090      IVFAIL = IVFAIL + 1                                          02160368
           RVCORR = 0.93541434669349                                    02170368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02180368
 0091      CONTINUE                                                     02190368
CT010*  TEST 10                                                         02200368
           IVTNUM = 10                                                  02210368
        AVS = SQRT(31.0/32.0)                                           02220368
           IF (AVS - 0.98420E+00) 20100, 10100, 40100                   02230368
40100      IF (AVS - 0.98430E+00) 10100, 10100, 20100                   02240368
10100      IVPASS = IVPASS + 1                                          02250368
           WRITE (NUVI, 80002) IVTNUM                                   02260368
           GO TO 0101                                                   02270368
20100      IVFAIL = IVFAIL + 1                                          02280368
           RVCORR = 0.98425098425148                                    02290368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02300368
 0101      CONTINUE                                                     02310368
CT011*  TEST 11                          AN ARGUMENT OF LOW MAGNITUDE   02320368
           IVTNUM = 11                                                  02330368
        AVS = SQRT(1.6E-35)                                             02340368
           IF (AVS - 0.39998E-17) 20110, 10110, 40110                   02350368
40110      IF (AVS - 0.40002E-17) 10110, 10110, 20110                   02360368
10110      IVPASS = IVPASS + 1                                          02370368
           WRITE (NUVI, 80002) IVTNUM                                   02380368
           GO TO 0111                                                   02390368
20110      IVFAIL = IVFAIL + 1                                          02400368
           RVCORR = 0.40000000000000E-17                                02410368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02420368
 0111      CONTINUE                                                     02430368
CT012*  TEST 12                         AN ARGUMENT OF HIGH MAGNITUDE   02440368
           IVTNUM = 12                                                  02450368
        AVS = SQRT(1.0E+35)                                             02460368
           IF (AVS - 0.31621E+18) 20120, 10120, 40120                   02470368
40120      IF (AVS - 0.31625E+18) 10120, 10120, 20120                   02480368
10120      IVPASS = IVPASS + 1                                          02490368
           WRITE (NUVI, 80002) IVTNUM                                   02500368
           GO TO 0121                                                   02510368
20120      IVFAIL = IVFAIL + 1                                          02520368
           RVCORR = 0.31622776601684E+18                                02530368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02540368
 0121      CONTINUE                                                     02550368
CT013*  TEST 13                                                         02560368
           IVTNUM = 13                                                  02570368
        BVS = SQRT(1.6)                                                 02580368
        AVS = SQRT(0.625) * BVS                                         02590368
           IF (AVS - 0.99995E+00) 20130, 10130, 40130                   02600368
40130      IF (AVS - 0.10001E+01) 10130, 10130, 20130                   02610368
10130      IVPASS = IVPASS + 1                                          02620368
           WRITE (NUVI, 80002) IVTNUM                                   02630368
           GO TO 0131                                                   02640368
20130      IVFAIL = IVFAIL + 1                                          02650368
           RVCORR = 1.0000000                                           02660368
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02670368
 0131      CONTINUE                                                     02680368
C*****                                                                  02690368
CBB** ********************** BBCSUM0  **********************************02700368
C**** WRITE OUT TEST SUMMARY                                            02710368
C****                                                                   02720368
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02730368
      WRITE (I02, 90004)                                                02740368
      WRITE (I02, 90014)                                                02750368
      WRITE (I02, 90004)                                                02760368
      WRITE (I02, 90020) IVPASS                                         02770368
      WRITE (I02, 90022) IVFAIL                                         02780368
      WRITE (I02, 90024) IVDELE                                         02790368
      WRITE (I02, 90026) IVINSP                                         02800368
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02810368
CBE** ********************** BBCSUM0  **********************************02820368
CBB** ********************** BBCFOOT0 **********************************02830368
C**** WRITE OUT REPORT FOOTINGS                                         02840368
C****                                                                   02850368
      WRITE (I02,90016) ZPROG, ZPROG                                    02860368
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02870368
      WRITE (I02,90019)                                                 02880368
CBE** ********************** BBCFOOT0 **********************************02890368
CBB** ********************** BBCFMT0A **********************************02900368
C**** FORMATS FOR TEST DETAIL LINES                                     02910368
C****                                                                   02920368
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02930368
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02940368
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02950368
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02960368
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02970368
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02980368
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02990368
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03000368
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03010368
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03020368
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03030368
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03040368
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03050368
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03060368
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03070368
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03080368
80050 FORMAT (1H ,48X,A31)                                              03090368
CBE** ********************** BBCFMT0A **********************************03100368
CBB** ********************** BBCFMT0B **********************************03110368
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03120368
C****                                                                   03130368
90002 FORMAT (1H1)                                                      03140368
90004 FORMAT (1H )                                                      03150368
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03160368
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03170368
90008 FORMAT (1H ,21X,A13,A17)                                          03180368
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03190368
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03200368
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03210368
     1       7X,7HREMARKS,24X)                                          03220368
90014 FORMAT (1H ,46H----------------------------------------------,    03230368
     1        33H---------------------------------)                     03240368
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03250368
C****                                                                   03260368
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03270368
C****                                                                   03280368
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03290368
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03300368
     1        A13)                                                      03310368
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03320368
C****                                                                   03330368
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03340368
C****                                                                   03350368
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03360368
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03370368
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03380368
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03390368
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03400368
CBE** ********************** BBCFMT0B **********************************03410368
C*****                                                                  03420368
C*****    END OF TEST SEGMENT 175                                       03430368
      STOP                                                              03440368
      END                                                               03450368
                                                                        03460368
