C***********************************************************************00010379
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020379
C*****   FM379                                                          00030379
C*****                       XRFOR - (201)                              00040379
C*****                                                                  00050379
C***********************************************************************00060379
C*****  GENERAL PURPOSE                                      SUBSET REF 00070379
C*****    TEST TRIGONOMETRIC FORMULAE                          15.3     00080379
C*****                                                        TABLE 5   00090379
C*****                                                                  00100379
CBB** ********************** BBCCOMNT **********************************00110379
C****                                                                   00120379
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130379
C****                          VERSION 2.0                              00140379
C****                                                                   00150379
C****                                                                   00160379
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170379
C****                   GENERAL SERVICES ADMINISTRATION                 00180379
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190379
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200379
C****                      FALLS CHURCH, VA. 22041                      00210379
C****                                                                   00220379
C****                          (703) 756-6153                           00230379
C****                                                                   00240379
CBE** ********************** BBCCOMNT **********************************00250379
CBB** ********************** BBCINITA **********************************00260379
C**** SPECIFICATION STATEMENTS                                          00270379
C****                                                                   00280379
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290379
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300379
CBE** ********************** BBCINITA **********************************00310379
CBB** ********************** BBCINITB **********************************00320379
C**** INITIALIZE SECTION                                                00330379
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340379
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350379
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360379
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370379
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380379
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390379
      DATA   REMRKS /'                               '/                 00400379
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410379
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420379
C****                                                                   00430379
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440379
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450379
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460379
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530379
      IVPASS = 0                                                        00540379
      IVFAIL = 0                                                        00550379
      IVDELE = 0                                                        00560379
      IVINSP = 0                                                        00570379
      IVTOTL = 0                                                        00580379
      IVTOTN = 0                                                        00590379
      ICZERO = 0                                                        00600379
C                                                                       00610379
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620379
      I01 = 05                                                          00630379
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640379
      I02 = 06                                                          00650379
C                                                                       00660379
      I01 = 5                                                           00670379
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680379
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690379
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700379
C                                                                       00710379
      I02 = 6                                                           00720379
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730379
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740379
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750379
C                                                                       00760379
CBE** ********************** BBCINITB **********************************00770379
      NUVI = I02                                                        00780379
      IVTOTL = 10                                                       00790379
      ZPROG = 'FM379'                                                   00800379
CBB** ********************** BBCHED0A **********************************00810379
C****                                                                   00820379
C**** WRITE REPORT TITLE                                                00830379
C****                                                                   00840379
      WRITE (I02, 90002)                                                00850379
      WRITE (I02, 90006)                                                00860379
      WRITE (I02, 90007)                                                00870379
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880379
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890379
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900379
CBE** ********************** BBCHED0A **********************************00910379
C*****                                                                  00920379
C*****    HEADER FOR SEGMENT 201                                        00930379
        WRITE(NUVI,20101)                                               00940379
20101   FORMAT(1H , / 35H  XRFOR - (201) INTRINSIC FUNCTIONS//          00950379
     1         24H  TRIGONOMETRIC FORMULAE//                            00960379
     2         20H  SUBSET REF. - 15.3)                                 00970379
CBB** ********************** BBCHED0B **********************************00980379
C**** WRITE DETAIL REPORT HEADERS                                       00990379
C****                                                                   01000379
      WRITE (I02,90004)                                                 01010379
      WRITE (I02,90004)                                                 01020379
      WRITE (I02,90013)                                                 01030379
      WRITE (I02,90014)                                                 01040379
      WRITE (I02,90015) IVTOTL                                          01050379
CBE** ********************** BBCHED0B **********************************01060379
C*****                                                                  01070379
        PIVS = 3.1415926535897932384626434                              01080379
C*****                                                                  01090379
CT001*  TEST 1                                           LN(EXP(X)) = 1 01100379
           IVTNUM = 1                                                   01110379
        BVS = 17.5                                                      01120379
        AVS = ALOG(EXP(1.75)) - BVS / 10.0                              01130379
           IF (AVS + 0.50000E-04) 20010, 10010, 40010                   01140379
40010      IF (AVS - 0.50000E-04) 10010, 10010, 20010                   01150379
10010      IVPASS = IVPASS + 1                                          01160379
           WRITE (NUVI, 80002) IVTNUM                                   01170379
           GO TO 0011                                                   01180379
20010      IVFAIL = IVFAIL + 1                                          01190379
           RVCORR = 0.0000                                              01200379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01210379
 0011      CONTINUE                                                     01220379
CT002*  TEST 2                                      SIN**2 + COS**2 = 1 01230379
           IVTNUM = 2                                                   01240379
        BVS = 10.0 / 4.0                                                01250379
        CVS = SIN(BVS) ** 2                                             01260379
        DVS = COS(BVS) ** 2                                             01270379
        AVS = CVS + DVS - 1.0                                           01280379
           IF (AVS + 0.50000E-04) 20020, 10020, 40020                   01290379
40020      IF (AVS - 0.50000E-04) 10020, 10020, 20020                   01300379
10020      IVPASS = IVPASS + 1                                          01310379
           WRITE (NUVI, 80002) IVTNUM                                   01320379
           GO TO 0021                                                   01330379
20020      IVFAIL = IVFAIL + 1                                          01340379
           RVCORR = 0.0000                                              01350379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01360379
 0021      CONTINUE                                                     01370379
CT003*  TEST 3                                SIN(2X) = 2*SIN(X)*COS(X) 01380379
           IVTNUM = 3                                                   01390379
        BVS = 8.5                                                       01400379
        CVS = BVS * (-0.5)                                              01410379
        AVS = (SIN(-4.25) * COS(CVS)) * 2.0 - SIN(-8.5)                 01420379
           IF (AVS + 0.50000E-04) 20030, 10030, 40030                   01430379
40030      IF (AVS - 0.50000E-04) 10030, 10030, 20030                   01440379
10030      IVPASS = IVPASS + 1                                          01450379
           WRITE (NUVI, 80002) IVTNUM                                   01460379
           GO TO 0031                                                   01470379
20030      IVFAIL = IVFAIL + 1                                          01480379
           RVCORR = 0.0000                                              01490379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01500379
 0031      CONTINUE                                                     01510379
CT004*  TEST 4                               ARCSIN(X) = ARCCOS(1-X**2) 01520379
           IVTNUM = 4                                                   01530379
        AVS = ASIN(-0.875) + ACOS(SQRT(1.0 - (0.875)  ** 2))            01540379
           IF (AVS + 0.50000E-04) 20040, 10040, 40040                   01550379
40040      IF (AVS - 0.50000E-04) 10040, 10040, 20040                   01560379
10040      IVPASS = IVPASS + 1                                          01570379
           WRITE (NUVI, 80002) IVTNUM                                   01580379
           GO TO 0041                                                   01590379
20040      IVFAIL = IVFAIL + 1                                          01600379
           RVCORR = 0.0000                                              01610379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01620379
 0041      CONTINUE                                                     01630379
CT005*  TEST 5                       TAN(X)**2 - 1 = -COS(2X)/COS(X)**2 01640379
           IVTNUM = 5                                                   01650379
        BVS = 7.0                                                       01660379
        AVS = COS(1.75) / COS(BVS / 8.0) ** 2 + TAN(0.875) ** 2 -       01670379
     1            1                                                     01680379
           IF (AVS + 0.50000E-04) 20050, 10050, 40050                   01690379
40050      IF (AVS - 0.50000E-04) 10050, 10050, 20050                   01700379
10050      IVPASS = IVPASS + 1                                          01710379
           WRITE (NUVI, 80002) IVTNUM                                   01720379
           GO TO 0051                                                   01730379
20050      IVFAIL = IVFAIL + 1                                          01740379
           RVCORR = 0.0000                                              01750379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01760379
 0051      CONTINUE                                                     01770379
CT006*  TEST 6                           ATAN(X/Y) = ATAN2(X,Y),  Y > 0 01780379
           IVTNUM = 6                                                   01790379
        BVS = 12.0                                                      01800379
        CVS = ATAN2(BVS / 4.0, BVS / 3.0)                               01810379
        AVS = CVS - ATAN(0.75)                                          01820379
           IF (AVS + 0.50000E-04) 20060, 10060, 40060                   01830379
40060      IF (AVS - 0.50000E-04) 10060, 10060, 20060                   01840379
10060      IVPASS = IVPASS + 1                                          01850379
           WRITE (NUVI, 80002) IVTNUM                                   01860379
           GO TO 0061                                                   01870379
20060      IVFAIL = IVFAIL + 1                                          01880379
           RVCORR = 0.0000                                              01890379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01900379
 0061      CONTINUE                                                     01910379
CT007*  TEST 7                                           SQRT(X)**2 = X 01920379
           IVTNUM = 7                                                   01930379
        AVS = SQRT(9.125) ** 2 - 9.125                                  01940379
           IF (AVS + 0.50000E-04) 20070, 10070, 40070                   01950379
40070      IF (AVS - 0.50000E-04) 10070, 10070, 20070                   01960379
10070      IVPASS = IVPASS + 1                                          01970379
           WRITE (NUVI, 80002) IVTNUM                                   01980379
           GO TO 0071                                                   01990379
20070      IVFAIL = IVFAIL + 1                                          02000379
           RVCORR = 0.0000                                              02010379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02020379
 0071      CONTINUE                                                     02030379
CT008*  TEST 8                                LN(X) = LN(10) * LOG10(X) 02040379
           IVTNUM = 8                                                   02050379
        BVS = 62.5 / 1000.0                                             02060379
        AVS = ALOG10(BVS) * ALOG(10.0) - ALOG(0.0625)                   02070379
           IF (AVS + 0.50000E-04) 20080, 10080, 40080                   02080379
40080      IF (AVS - 0.50000E-04) 10080, 10080, 20080                   02090379
10080      IVPASS = IVPASS + 1                                          02100379
           WRITE (NUVI, 80002) IVTNUM                                   02110379
           GO TO 0081                                                   02120379
20080      IVFAIL = IVFAIL + 1                                          02130379
           RVCORR = 0.0000                                              02140379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02150379
 0081      CONTINUE                                                     02160379
CT009*  TEST 9                                    COSH**2 - SINH**2 = 1 02170379
           IVTNUM = 9                                                   02180379
        BVS = 0.125                                                     02190379
        CVS = SINH(2.125)                                               02200379
        DVS = COSH(2.0 + BVS)                                           02210379
        AVS = DVS  ** 2 - CVS ** 2 - COSH(0.0)                          02220379
           IF (AVS + 0.50000E-04) 20090, 10090, 40090                   02230379
40090      IF (AVS - 0.50000E-04) 10090, 10090, 20090                   02240379
10090      IVPASS = IVPASS + 1                                          02250379
           WRITE (NUVI, 80002) IVTNUM                                   02260379
           GO TO 0091                                                   02270379
20090      IVFAIL = IVFAIL + 1                                          02280379
           RVCORR = 0.0000                                              02290379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02300379
 0091      CONTINUE                                                     02310379
CT010*  TEST 10                             TANH(X) = 1 - 2/(EXP(2X)+1) 02320379
           IVTNUM = 10                                                  02330379
        BVS = 5.0                                                       02340379
        CVS = 2.0                                                       02350379
        DVS = ALOG10(BVS * CVS) - SQRT(4.0) /                           02360379
     1  (EXP(2.0 * (BVS - CVS)) + COS(0.0))                             02370379
        AVS = DVS - TANH(3.0)                                           02380379
           IF (AVS + 0.50000E-04) 20100, 10100, 40100                   02390379
40100      IF (AVS - 0.50000E-04) 10100, 10100, 20100                   02400379
10100      IVPASS = IVPASS + 1                                          02410379
           WRITE (NUVI, 80002) IVTNUM                                   02420379
           GO TO 0101                                                   02430379
20100      IVFAIL = IVFAIL + 1                                          02440379
           RVCORR = 0.0000                                              02450379
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02460379
 0101      CONTINUE                                                     02470379
C*****                                                                  02480379
CBB** ********************** BBCSUM0  **********************************02490379
C**** WRITE OUT TEST SUMMARY                                            02500379
C****                                                                   02510379
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02520379
      WRITE (I02, 90004)                                                02530379
      WRITE (I02, 90014)                                                02540379
      WRITE (I02, 90004)                                                02550379
      WRITE (I02, 90020) IVPASS                                         02560379
      WRITE (I02, 90022) IVFAIL                                         02570379
      WRITE (I02, 90024) IVDELE                                         02580379
      WRITE (I02, 90026) IVINSP                                         02590379
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02600379
CBE** ********************** BBCSUM0  **********************************02610379
CBB** ********************** BBCFOOT0 **********************************02620379
C**** WRITE OUT REPORT FOOTINGS                                         02630379
C****                                                                   02640379
      WRITE (I02,90016) ZPROG, ZPROG                                    02650379
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02660379
      WRITE (I02,90019)                                                 02670379
CBE** ********************** BBCFOOT0 **********************************02680379
CBB** ********************** BBCFMT0A **********************************02690379
C**** FORMATS FOR TEST DETAIL LINES                                     02700379
C****                                                                   02710379
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02720379
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02730379
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02740379
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02750379
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02760379
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02770379
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02780379
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02790379
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02800379
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02810379
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02820379
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02830379
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02840379
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02850379
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02860379
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02870379
80050 FORMAT (1H ,48X,A31)                                              02880379
CBE** ********************** BBCFMT0A **********************************02890379
CBB** ********************** BBCFMT0B **********************************02900379
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02910379
C****                                                                   02920379
90002 FORMAT (1H1)                                                      02930379
90004 FORMAT (1H )                                                      02940379
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02950379
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02960379
90008 FORMAT (1H ,21X,A13,A17)                                          02970379
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02980379
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02990379
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03000379
     1       7X,7HREMARKS,24X)                                          03010379
90014 FORMAT (1H ,46H----------------------------------------------,    03020379
     1        33H---------------------------------)                     03030379
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03040379
C****                                                                   03050379
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03060379
C****                                                                   03070379
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03080379
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03090379
     1        A13)                                                      03100379
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03110379
C****                                                                   03120379
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03130379
C****                                                                   03140379
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03150379
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03160379
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03170379
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03180379
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03190379
CBE** ********************** BBCFMT0B **********************************03200379
C*****                                                                  03210379
C*****    END OF TEST SEGMENT 201                                       03220379
      STOP                                                              03230379
      END                                                               03240379
                                                                        03250379
