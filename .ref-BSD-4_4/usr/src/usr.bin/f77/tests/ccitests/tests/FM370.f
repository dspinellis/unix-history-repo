C***********************************************************************00010370
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020370
C*****   FM370                                                          00030370
C*****                       XALOG - (181)                              00040370
C*****                                                                  00050370
C***********************************************************************00060370
C*****  GENERAL PURPOSE                                      SUBSET REF 00070370
C*****    TEST INTRINSIC FUNCTION ALOG                         15.3     00080370
C*****                                                        TABLE 5   00090370
C*****                                                                  00100370
C*****                                                                  00110370
CBB** ********************** BBCCOMNT **********************************00120370
C****                                                                   00130370
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140370
C****                          VERSION 2.0                              00150370
C****                                                                   00160370
C****                                                                   00170370
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180370
C****                   GENERAL SERVICES ADMINISTRATION                 00190370
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200370
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210370
C****                      FALLS CHURCH, VA. 22041                      00220370
C****                                                                   00230370
C****                          (703) 756-6153                           00240370
C****                                                                   00250370
CBE** ********************** BBCCOMNT **********************************00260370
CBB** ********************** BBCINITA **********************************00270370
C**** SPECIFICATION STATEMENTS                                          00280370
C****                                                                   00290370
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00300370
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00310370
CBE** ********************** BBCINITA **********************************00320370
CBB** ********************** BBCINITB **********************************00330370
C**** INITIALIZE SECTION                                                00340370
      DATA  ZVERS,                  ZVERSD,             ZDATE           00350370
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00360370
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00370370
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00380370
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00390370
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00400370
      DATA   REMRKS /'                               '/                 00410370
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00420370
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00430370
C****                                                                   00440370
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00450370
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00460370
CZ03  ZPROG  = 'PROGRAM NAME'                                           00470370
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00540370
      IVPASS = 0                                                        00550370
      IVFAIL = 0                                                        00560370
      IVDELE = 0                                                        00570370
      IVINSP = 0                                                        00580370
      IVTOTL = 0                                                        00590370
      IVTOTN = 0                                                        00600370
      ICZERO = 0                                                        00610370
C                                                                       00620370
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00630370
      I01 = 05                                                          00640370
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00650370
      I02 = 06                                                          00660370
C                                                                       00670370
      I01 = 5                                                           00680370
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690370
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00700370
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00710370
C                                                                       00720370
      I02 = 6                                                           00730370
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00740370
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00750370
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00760370
C                                                                       00770370
CBE** ********************** BBCINITB **********************************00780370
      NUVI = I02                                                        00790370
      IVTOTL = 16                                                       00800370
      ZPROG = 'FM370'                                                   00810370
CBB** ********************** BBCHED0A **********************************00820370
C****                                                                   00830370
C**** WRITE REPORT TITLE                                                00840370
C****                                                                   00850370
      WRITE (I02, 90002)                                                00860370
      WRITE (I02, 90006)                                                00870370
      WRITE (I02, 90007)                                                00880370
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00890370
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00900370
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00910370
CBE** ********************** BBCHED0A **********************************00920370
C*****    HEADER FOR SEGMENT 181                                        00930370
        WRITE(NUVI,18100)                                               00940370
18100   FORMAT(1H , / 35H  XALOG - (181) INTRINSIC FUNCTIONS//          00950370
     1         26H  ALOG (NATURAL LOGARITHM)//                          00960370
     2         20H  SUBSET REF. - 15.3)                                 00970370
CBB** ********************** BBCHED0B **********************************00980370
C**** WRITE DETAIL REPORT HEADERS                                       00990370
C****                                                                   01000370
      WRITE (I02,90004)                                                 01010370
      WRITE (I02,90004)                                                 01020370
      WRITE (I02,90013)                                                 01030370
      WRITE (I02,90014)                                                 01040370
      WRITE (I02,90015) IVTOTL                                          01050370
CBE** ********************** BBCHED0B **********************************01060370
C*****                                                                  01070370
CT001*  TEST 1                                 ONE, SINCE LN(1.0) = 0.0 01080370
           IVTNUM = 1                                                   01090370
        BVS = 1.0                                                       01100370
        AVS = ALOG(BVS)                                                 01110370
           IF (AVS + 0.50000E-04) 20010, 10010, 40010                   01120370
40010      IF (AVS - 0.50000E-04) 10010, 10010, 20010                   01130370
10010      IVPASS = IVPASS + 1                                          01140370
           WRITE (NUVI, 80002) IVTNUM                                   01150370
           GO TO 0011                                                   01160370
20010      IVFAIL = IVFAIL + 1                                          01170370
           RVCORR = 0.00000000000000                                    01180370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01190370
 0011      CONTINUE                                                     01200370
CT002*  TEST 2                                        VALUES CLOSE TO E 01210370
           IVTNUM = 2                                                   01220370
        AVS = ALOG(2.6875)                                              01230370
           IF (AVS - 0.98856E+00) 20020, 10020, 40020                   01240370
40020      IF (AVS - 0.98866E+00) 10020, 10020, 20020                   01250370
10020      IVPASS = IVPASS + 1                                          01260370
           WRITE (NUVI, 80002) IVTNUM                                   01270370
           GO TO 0021                                                   01280370
20020      IVFAIL = IVFAIL + 1                                          01290370
           RVCORR = 0.98861139345378                                    01300370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01310370
 0021      CONTINUE                                                     01320370
CT003*  TEST 3                                                          01330370
           IVTNUM = 3                                                   01340370
        AVS = ALOG(5.125)                                               01350370
           IF (AVS - 0.16340E+01) 20030, 10030, 40030                   01360370
40030      IF (AVS - 0.16342E+01) 10030, 10030, 20030                   01370370
10030      IVPASS = IVPASS + 1                                          01380370
           WRITE (NUVI, 80002) IVTNUM                                   01390370
           GO TO 0031                                                   01400370
20030      IVFAIL = IVFAIL + 1                                          01410370
           RVCORR = 1.63413052502447                                    01420370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01430370
 0031      CONTINUE                                                     01440370
CT004*  TEST 4                                                          01450370
           IVTNUM = 4                                                   01460370
        AVS = ALOG(10.0)                                                01470370
           IF (AVS - 0.23025E+01) 20040, 10040, 40040                   01480370
40040      IF (AVS - 0.23027E+01) 10040, 10040, 20040                   01490370
10040      IVPASS = IVPASS + 1                                          01500370
           WRITE (NUVI, 80002) IVTNUM                                   01510370
           GO TO 0041                                                   01520370
20040      IVFAIL = IVFAIL + 1                                          01530370
           RVCORR = 2.30258509299405                                    01540370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01550370
 0041      CONTINUE                                                     01560370
CT005*  TEST 5                                                          01570370
           IVTNUM = 5                                                   01580370
        AVS = ALOG(100.0)                                               01590370
           IF (AVS - 0.46049E+01) 20050, 10050, 40050                   01600370
40050      IF (AVS - 0.46054E+01) 10050, 10050, 20050                   01610370
10050      IVPASS = IVPASS + 1                                          01620370
           WRITE (NUVI, 80002) IVTNUM                                   01630370
           GO TO 0051                                                   01640370
20050      IVFAIL = IVFAIL + 1                                          01650370
           RVCORR = 4.60517018598809                                    01660370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01670370
 0051      CONTINUE                                                     01680370
CT006*  TEST 6                                                          01690370
           IVTNUM = 6                                                   01700370
        BVS = 1.0                                                       01710370
        AVS = ALOG(BVS / 4.0)                                           01720370
           IF (AVS + 0.13864E+01) 20060, 10060, 40060                   01730370
40060      IF (AVS + 0.13862E+01) 10060, 10060, 20060                   01740370
10060      IVPASS = IVPASS + 1                                          01750370
           WRITE (NUVI, 80002) IVTNUM                                   01760370
           GO TO 0061                                                   01770370
20060      IVFAIL = IVFAIL + 1                                          01780370
           RVCORR = -1.38629436111989                                   01790370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01800370
 0061      CONTINUE                                                     01810370
CT007*  TEST 7                                                          01820370
           IVTNUM = 7                                                   01830370
        BVS = 1.0                                                       01840370
        CVS = 8.0                                                       01850370
        AVS = ALOG(3.0 * BVS / CVS)                                     01860370
           IF (AVS + 0.98088E+00) 20070, 10070, 40070                   01870370
40070      IF (AVS + 0.98078E+00) 10070, 10070, 20070                   01880370
10070      IVPASS = IVPASS + 1                                          01890370
           WRITE (NUVI, 80002) IVTNUM                                   01900370
           GO TO 0071                                                   01910370
20070      IVFAIL = IVFAIL + 1                                          01920370
           RVCORR = -0.98082925301173                                   01930370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01940370
 0071      CONTINUE                                                     01950370
CT008*  TEST 8                                                          01960370
           IVTNUM = 8                                                   01970370
        AVS = ALOG(50.0 / 100.0)                                        01980370
           IF (AVS + 0.69318E+00) 20080, 10080, 40080                   01990370
40080      IF (AVS + 0.69311E+00) 10080, 10080, 20080                   02000370
10080      IVPASS = IVPASS + 1                                          02010370
           WRITE (NUVI, 80002) IVTNUM                                   02020370
           GO TO 0081                                                   02030370
20080      IVFAIL = IVFAIL + 1                                          02040370
           RVCORR = -0.69314718055995                                   02050370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02060370
 0081      CONTINUE                                                     02070370
CT009*  TEST 9                                                          02080370
           IVTNUM = 9                                                   02090370
        BVS = 68.75                                                     02100370
        AVS = ALOG(BVS * 0.01)                                          02110370
           IF (AVS + 0.37471E+00) 20090, 10090, 40090                   02120370
40090      IF (AVS + 0.37467E+00) 10090, 10090, 20090                   02130370
10090      IVPASS = IVPASS + 1                                          02140370
           WRITE (NUVI, 80002) IVTNUM                                   02150370
           GO TO 0091                                                   02160370
20090      IVFAIL = IVFAIL + 1                                          02170370
           RVCORR = -0.37469344944141                                   02180370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02190370
 0091      CONTINUE                                                     02200370
CT010*  TEST 10                                     VALUES CLOSE TO ONE 02210370
           IVTNUM = 10                                                  02220370
        AVS = ALOG(0.96875)                                             02230370
           IF (AVS + 0.31750E-01) 20100, 10100, 40100                   02240370
40100      IF (AVS + 0.31747E-01) 10100, 10100, 20100                   02250370
10100      IVPASS = IVPASS + 1                                          02260370
           WRITE (NUVI, 80002) IVTNUM                                   02270370
           GO TO 0101                                                   02280370
20100      IVFAIL = IVFAIL + 1                                          02290370
           RVCORR = -0.03174869831458                                   02300370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02310370
 0101      CONTINUE                                                     02320370
CT011*  TEST 11                                                         02330370
           IVTNUM = 11                                                  02340370
        BVS = 1.015625                                                  02350370
        AVS = ALOG(BVS)                                                 02360370
           IF (AVS - 0.15503E-01) 20110, 10110, 40110                   02370370
40110      IF (AVS - 0.15505E-01) 10110, 10110, 20110                   02380370
10110      IVPASS = IVPASS + 1                                          02390370
           WRITE (NUVI, 80002) IVTNUM                                   02400370
           GO TO 0111                                                   02410370
20110      IVFAIL = IVFAIL + 1                                          02420370
           RVCORR = 0.01550418653597                                    02430370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02440370
 0111      CONTINUE                                                     02450370
CT012*  TEST 12                                    VALUES CLOSE TO ZERO 02460370
           IVTNUM = 12                                                  02470370
        BVS = 128.0                                                     02480370
        AVS = ALOG(1.0 / BVS)                                           02490370
           IF (AVS + 0.48523E+01) 20120, 10120, 40120                   02500370
40120      IF (AVS + 0.48518E+01) 10120, 10120, 20120                   02510370
10120      IVPASS = IVPASS + 1                                          02520370
           WRITE (NUVI, 80002) IVTNUM                                   02530370
           GO TO 0121                                                   02540370
20120      IVFAIL = IVFAIL + 1                                          02550370
           RVCORR = -4.85203026391962                                   02560370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02570370
 0121      CONTINUE                                                     02580370
CT013*  TEST 13                                                         02590370
           IVTNUM = 13                                                  02600370
        BVS = 128.0                                                     02610370
        AVS = ALOG(1.0 / (BVS * 4.0))                                   02620370
           IF (AVS + 0.62386E+01) 20130, 10130, 40130                   02630370
40130      IF (AVS + 0.62380E+01) 10130, 10130, 20130                   02640370
10130      IVPASS = IVPASS + 1                                          02650370
           WRITE (NUVI, 80002) IVTNUM                                   02660370
           GO TO 0131                                                   02670370
20130      IVFAIL = IVFAIL + 1                                          02680370
           RVCORR = -6.23832462503951                                   02690370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02700370
 0131      CONTINUE                                                     02710370
CT014*  TEST 14                           AN ARGUMENT OF HIGH MAGNITUDE 02720370
           IVTNUM = 14                                                  02730370
        BVS = 1.0E+37                                                   02740370
        AVS = ALOG(BVS)                                                 02750370
           IF (AVS - 0.85191E+01) 20140, 10140, 40140                   02760370
40140      IF (AVS - 0.85200E+02) 10140, 10140, 20140                   02770370
10140      IVPASS = IVPASS + 1                                          02780370
           WRITE (NUVI, 80002) IVTNUM                                   02790370
           GO TO 0141                                                   02800370
20140      IVFAIL = IVFAIL + 1                                          02810370
           RVCORR = 85.19564844077969                                   02820370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02830370
 0141      CONTINUE                                                     02840370
CT015*  TEST 15                            AN ARGUMENT OF LOW MAGNITUDE 02850370
           IVTNUM = 15                                                  02860370
        BVS = 1.0E-37                                                   02870370
        AVS = ALOG(BVS)                                                 02880370
           IF (AVS + 0.85200E+02) 20150, 10150, 40150                   02890370
40150      IF (AVS + 0.85191E+02) 10150, 10150, 20150                   02900370
10150      IVPASS = IVPASS + 1                                          02910370
           WRITE (NUVI, 80002) IVTNUM                                   02920370
           GO TO 0151                                                   02930370
20150      IVFAIL = IVFAIL + 1                                          02940370
           RVCORR = -85.19564844077969                                  02950370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02960370
 0151      CONTINUE                                                     02970370
CT016*  TEST 16                                                         02980370
           IVTNUM = 16                                                  02990370
        AVS = ALOG(8.0) + ALOG(0.125)                                   03000370
           IF (AVS + 0.50000E-04) 20160, 10160, 40160                   03010370
40160      IF (AVS - 0.50000E-04) 10160, 10160, 20160                   03020370
10160      IVPASS = IVPASS + 1                                          03030370
           WRITE (NUVI, 80002) IVTNUM                                   03040370
           GO TO 0161                                                   03050370
20160      IVFAIL = IVFAIL + 1                                          03060370
           RVCORR = 0.0000000                                           03070370
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03080370
 0161      CONTINUE                                                     03090370
C*****                                                                  03100370
CBB** ********************** BBCSUM0  **********************************03110370
C**** WRITE OUT TEST SUMMARY                                            03120370
C****                                                                   03130370
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03140370
      WRITE (I02, 90004)                                                03150370
      WRITE (I02, 90014)                                                03160370
      WRITE (I02, 90004)                                                03170370
      WRITE (I02, 90020) IVPASS                                         03180370
      WRITE (I02, 90022) IVFAIL                                         03190370
      WRITE (I02, 90024) IVDELE                                         03200370
      WRITE (I02, 90026) IVINSP                                         03210370
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03220370
CBE** ********************** BBCSUM0  **********************************03230370
CBB** ********************** BBCFOOT0 **********************************03240370
C**** WRITE OUT REPORT FOOTINGS                                         03250370
C****                                                                   03260370
      WRITE (I02,90016) ZPROG, ZPROG                                    03270370
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03280370
      WRITE (I02,90019)                                                 03290370
CBE** ********************** BBCFOOT0 **********************************03300370
CBB** ********************** BBCFMT0A **********************************03310370
C**** FORMATS FOR TEST DETAIL LINES                                     03320370
C****                                                                   03330370
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03340370
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03350370
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03360370
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03370370
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03380370
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03390370
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03400370
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03410370
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03420370
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03430370
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03440370
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03450370
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03460370
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03470370
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03480370
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03490370
80050 FORMAT (1H ,48X,A31)                                              03500370
CBE** ********************** BBCFMT0A **********************************03510370
CBB** ********************** BBCFMT0B **********************************03520370
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03530370
C****                                                                   03540370
90002 FORMAT (1H1)                                                      03550370
90004 FORMAT (1H )                                                      03560370
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03570370
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03580370
90008 FORMAT (1H ,21X,A13,A17)                                          03590370
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03600370
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03610370
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03620370
     1       7X,7HREMARKS,24X)                                          03630370
90014 FORMAT (1H ,46H----------------------------------------------,    03640370
     1        33H---------------------------------)                     03650370
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03660370
C****                                                                   03670370
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03680370
C****                                                                   03690370
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03700370
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03710370
     1        A13)                                                      03720370
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03730370
C****                                                                   03740370
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03750370
C****                                                                   03760370
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03770370
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03780370
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03790370
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03800370
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03810370
CBE** ********************** BBCFMT0B **********************************03820370
C*****                                                                  03830370
C*****    END OF TEST SEGMENT 181                                       03840370
      STOP                                                              03850370
      END                                                               03860370
                                                                        03870370
