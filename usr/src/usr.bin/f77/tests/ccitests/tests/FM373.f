C***********************************************************************00010373
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020373
C*****   FM373                                                          00030373
C*****                       XCOS - (189)                               00040373
C*****                                                                  00050373
C***********************************************************************00060373
C*****  GENERAL PURPOSE                                      SUBSET REF 00070373
C*****    TEST INTRINSIC FUNCTION COS                          15.3     00080373
C*****                                                        TABLE 5   00090373
C*****                                                                  00100373
CBB** ********************** BBCCOMNT **********************************00110373
C****                                                                   00120373
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130373
C****                          VERSION 2.0                              00140373
C****                                                                   00150373
C****                                                                   00160373
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170373
C****                   GENERAL SERVICES ADMINISTRATION                 00180373
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190373
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200373
C****                      FALLS CHURCH, VA. 22041                      00210373
C****                                                                   00220373
C****                          (703) 756-6153                           00230373
C****                                                                   00240373
CBE** ********************** BBCCOMNT **********************************00250373
CBB** ********************** BBCINITA **********************************00260373
C**** SPECIFICATION STATEMENTS                                          00270373
C****                                                                   00280373
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290373
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300373
CBE** ********************** BBCINITA **********************************00310373
CBB** ********************** BBCINITB **********************************00320373
C**** INITIALIZE SECTION                                                00330373
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340373
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350373
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360373
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370373
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380373
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390373
      DATA   REMRKS /'                               '/                 00400373
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410373
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420373
C****                                                                   00430373
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440373
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450373
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460373
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530373
      IVPASS = 0                                                        00540373
      IVFAIL = 0                                                        00550373
      IVDELE = 0                                                        00560373
      IVINSP = 0                                                        00570373
      IVTOTL = 0                                                        00580373
      IVTOTN = 0                                                        00590373
      ICZERO = 0                                                        00600373
C                                                                       00610373
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620373
      I01 = 05                                                          00630373
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640373
      I02 = 06                                                          00650373
C                                                                       00660373
      I01 = 5                                                           00670373
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680373
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690373
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700373
C                                                                       00710373
      I02 = 6                                                           00720373
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730373
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740373
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750373
C                                                                       00760373
CBE** ********************** BBCINITB **********************************00770373
      NUVI = I02                                                        00780373
      IVTOTL = 19                                                       00790373
      ZPROG = 'FM373'                                                   00800373
CBB** ********************** BBCHED0A **********************************00810373
C****                                                                   00820373
C**** WRITE REPORT TITLE                                                00830373
C****                                                                   00840373
      WRITE (I02, 90002)                                                00850373
      WRITE (I02, 90006)                                                00860373
      WRITE (I02, 90007)                                                00870373
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880373
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890373
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900373
CBE** ********************** BBCHED0A **********************************00910373
C*****                                                                  00920373
C*****    HEADER FOR SEGMENT 189                                        00930373
        WRITE(NUVI,18900)                                               00940373
18900   FORMAT(1H /33H XCOS - (189) INTRINSIC FUNCTIONS//               00950373
     1         14H  COS (COSINE)//                                      00960373
     2         20H  SUBSET REF. - 15.3)                                 00970373
CBB** ********************** BBCHED0B **********************************00980373
C**** WRITE DETAIL REPORT HEADERS                                       00990373
C****                                                                   01000373
      WRITE (I02,90004)                                                 01010373
      WRITE (I02,90004)                                                 01020373
      WRITE (I02,90013)                                                 01030373
      WRITE (I02,90014)                                                 01040373
      WRITE (I02,90015) IVTOTL                                          01050373
CBE** ********************** BBCHED0B **********************************01060373
C*****                                                                  01070373
        PIVS = 3.1415926535897932384626434                              01080373
C*****                                                                  01090373
CT001*  TEST 1                               ZERO (0.0), SINCE COS(0)=1 01100373
           IVTNUM = 1                                                   01110373
        BVS = 0.0                                                       01120373
        AVS = COS(BVS)                                                  01130373
           IF (AVS - 0.99995E+00) 20010, 10010, 40010                   01140373
40010      IF (AVS - 0.10001E+01) 10010, 10010, 20010                   01150373
10010      IVPASS = IVPASS + 1                                          01160373
           WRITE (NUVI, 80002) IVTNUM                                   01170373
           GO TO 0011                                                   01180373
20010      IVFAIL = IVFAIL + 1                                          01190373
           RVCORR = 1.00000000000000                                    01200373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01210373
 0011      CONTINUE                                                     01220373
CT002*  TEST 2                                           VALUES NEAR PI 01230373
           IVTNUM = 2                                                   01240373
        AVS = COS(PIVS)                                                 01250373
           IF (AVS + 0.10001E+01) 20020, 10020, 40020                   01260373
40020      IF (AVS + 0.99995E+00) 10020, 10020, 20020                   01270373
10020      IVPASS = IVPASS + 1                                          01280373
           WRITE (NUVI, 80002) IVTNUM                                   01290373
           GO TO 0021                                                   01300373
20020      IVFAIL = IVFAIL + 1                                          01310373
           RVCORR = -1.00000000000000                                   01320373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01330373
 0021      CONTINUE                                                     01340373
CT003*  TEST 3                                                PI - 1/16 01350373
           IVTNUM = 3                                                   01360373
        BVS = 3.0790926536                                              01370373
        AVS = COS(BVS)                                                  01380373
           IF (AVS + 0.99810E+00) 20030, 10030, 40030                   01390373
40030      IF (AVS + 0.99799E+00) 10030, 10030, 20030                   01400373
10030      IVPASS = IVPASS + 1                                          01410373
           WRITE (NUVI, 80002) IVTNUM                                   01420373
           GO TO 0031                                                   01430373
20030      IVFAIL = IVFAIL + 1                                          01440373
           RVCORR = -0.99804751070010                                   01450373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01460373
 0031      CONTINUE                                                     01470373
CT004*  TEST 4                                                PI + 1/32 01480373
           IVTNUM = 4                                                   01490373
        AVS = COS(3.1728426535)                                         01500373
           IF (AVS + 0.99957E+00) 20040, 10040, 40040                   01510373
40040      IF (AVS + 0.99946E+00) 10040, 10040, 20040                   01520373
10040      IVPASS = IVPASS + 1                                          01530373
           WRITE (NUVI, 80002) IVTNUM                                   01540373
           GO TO 0041                                                   01550373
20040      IVFAIL = IVFAIL + 1                                          01560373
           RVCORR = -0.99951175848514                                   01570373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01580373
 0041      CONTINUE                                                     01590373
CT005*  TEST 5                                         VALUES NEAR 2*PI 01600373
           IVTNUM = 5                                                   01610373
        BVS = PIVS * 2.0                                                01620373
        AVS = COS(BVS)                                                  01630373
           IF (AVS - 0.99995E+00) 20050, 10050, 40050                   01640373
40050      IF (AVS - 0.10001E+01) 10050, 10050, 20050                   01650373
10050      IVPASS = IVPASS + 1                                          01660373
           WRITE (NUVI, 80002) IVTNUM                                   01670373
           GO TO 0051                                                   01680373
20050      IVFAIL = IVFAIL + 1                                          01690373
           RVCORR = 1.00000000000000                                    01700373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01710373
 0051      CONTINUE                                                     01720373
CT006*  TEST 6                                         VALUES NEAR 2*PI 01730373
           IVTNUM = 6                                                   01740373
        BVS = (2.0 * PIVS) - 1.0 / 64.0                                 01750373
        AVS = COS(BVS)                                                  01760373
           IF (AVS - 0.99982E+00) 20060, 10060, 40060                   01770373
40060      IF (AVS - 0.99993E+00) 10060, 10060, 20060                   01780373
10060      IVPASS = IVPASS + 1                                          01790373
           WRITE (NUVI, 80002) IVTNUM                                   01800373
           GO TO 0061                                                   01810373
20060      IVFAIL = IVFAIL + 1                                          01820373
           RVCORR = 0.99987793217101                                    01830373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01840373
 0061      CONTINUE                                                     01850373
CT007*  TEST 7                                         VALUES NEAR 2*PI 01860373
           IVTNUM = 7                                                   01870373
        BVS = (2.0 * PIVS) + 1.0 / 128.0                                01880373
        AVS = COS(BVS)                                                  01890373
           IF (AVS - 0.99992E+00) 20070, 10070, 40070                   01900373
40070      IF (AVS - 0.10001E+01) 10070, 10070, 20070                   01910373
10070      IVPASS = IVPASS + 1                                          01920373
           WRITE (NUVI, 80002) IVTNUM                                   01930373
           GO TO 0071                                                   01940373
20070      IVFAIL = IVFAIL + 1                                          01950373
           RVCORR = 0.99996948257710                                    01960373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01970373
 0071      CONTINUE                                                     01980373
CT008*  TEST 8                           AN EXPRESSION PRESENTED TO COS 01990373
           IVTNUM = 8                                                   02000373
        BVS = 350.0                                                     02010373
        AVS = COS(BVS / 100.0)                                          02020373
           IF (AVS + 0.93651E+00) 20080, 10080, 40080                   02030373
40080      IF (AVS + 0.93641E+00) 10080, 10080, 20080                   02040373
10080      IVPASS = IVPASS + 1                                          02050373
           WRITE (NUVI, 80002) IVTNUM                                   02060373
           GO TO 0081                                                   02070373
20080      IVFAIL = IVFAIL + 1                                          02080373
           RVCORR = -0.93645668729080                                   02090373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02100373
 0081      CONTINUE                                                     02110373
CT009*  TEST 9                                      A NEGATIVE ARGUMENT 02120373
           IVTNUM = 9                                                   02130373
        BVS = -1.5                                                      02140373
        AVS = COS(BVS)                                                  02150373
           IF (AVS - 0.70733E-01) 20090, 10090, 40090                   02160373
40090      IF (AVS - 0.70741E-01) 10090, 10090, 20090                   02170373
10090      IVPASS = IVPASS + 1                                          02180373
           WRITE (NUVI, 80002) IVTNUM                                   02190373
           GO TO 0091                                                   02200373
20090      IVFAIL = IVFAIL + 1                                          02210373
           RVCORR = 0.07073720166770                                    02220373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02230373
 0091      CONTINUE                                                     02240373
CT010*  TEST 10                TEST LARGE VALUES FOR ARGUMENT REDUCTION 02250373
           IVTNUM = 10                                                  02260373
        AVS = COS(200.0)                                                02270373
           IF (AVS - 0.48716E+00) 20100, 10100, 40100                   02280373
40100      IF (AVS - 0.48722E+00) 10100, 10100, 20100                   02290373
10100      IVPASS = IVPASS + 1                                          02300373
           WRITE (NUVI, 80002) IVTNUM                                   02310373
           GO TO 0101                                                   02320373
20100      IVFAIL = IVFAIL + 1                                          02330373
           RVCORR = 0.48718767500701                                    02340373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02350373
 0101      CONTINUE                                                     02360373
CT011*  TEST 11                TEST LARGE VALUES FOR ARGUMENT REDUCTION 02370373
           IVTNUM = 11                                                  02380373
        AVS = COS(-31416.0)                                             02390373
           IF (AVS - 0.99725E+00) 20110, 10110, 40110                   02400373
40110      IF (AVS - 0.99736E+00) 10110, 10110, 20110                   02410373
10110      IVPASS = IVPASS + 1                                          02420373
           WRITE (NUVI, 80002) IVTNUM                                   02430373
           GO TO 0111                                                   02440373
20110      IVFAIL = IVFAIL + 1                                          02450373
           RVCORR = 0.99730272627420                                    02460373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02470373
 0111      CONTINUE                                                     02480373
CT012*  TEST 12                                   TEST VALUES NEAR PI/2 02490373
           IVTNUM = 12                                                  02500373
        AVS = COS(1.5707963268)                                         02510373
           IF (AVS + 0.50000E-04) 20120, 10120, 40120                   02520373
40120      IF (AVS - 0.50000E-04) 10120, 10120, 20120                   02530373
10120      IVPASS = IVPASS + 1                                          02540373
           WRITE (NUVI, 80002) IVTNUM                                   02550373
           GO TO 0121                                                   02560373
20120      IVFAIL = IVFAIL + 1                                          02570373
           RVCORR = 0.00000000000000                                    02580373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02590373
 0121      CONTINUE                                                     02600373
CT013*  TEST 13                                         (PI / 2) - 1/32 02610373
           IVTNUM = 13                                                  02620373
        BVS = (1.5395463267)                                            02630373
        AVS = COS(BVS)                                                  02640373
           IF (AVS - 0.31243E-01) 20130, 10130, 40130                   02650373
40130      IF (AVS - 0.31247E-01) 10130, 10130, 20130                   02660373
10130      IVPASS = IVPASS + 1                                          02670373
           WRITE (NUVI, 80002) IVTNUM                                   02680373
           GO TO 0131                                                   02690373
20130      IVFAIL = IVFAIL + 1                                          02700373
           RVCORR = 0.03124491398533                                    02710373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02720373
 0131      CONTINUE                                                     02730373
CT014*  TEST 14                                         (PI / 2) + 1/16 02740373
           IVTNUM = 14                                                  02750373
        AVS = COS(1.6332963267)                                         02760373
           IF (AVS + 0.62463E-01) 20140, 10140, 40140                   02770373
40140      IF (AVS + 0.62456E-01) 10140, 10140, 20140                   02780373
10140      IVPASS = IVPASS + 1                                          02790373
           WRITE (NUVI, 80002) IVTNUM                                   02800373
           GO TO 0141                                                   02810373
20140      IVFAIL = IVFAIL + 1                                          02820373
           RVCORR = -0.06245931784238                                   02830373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02840373
 0141      CONTINUE                                                     02850373
CT015*  TEST 15                                 TEST VALUES NEAR 3*PI/2 02860373
           IVTNUM = 15                                                  02870373
        BVS = 3.0 * PIVS / 2.0                                          02880373
        AVS = COS(BVS)                                                  02890373
           IF (AVS + 0.50000E-04) 20150, 10150, 40150                   02900373
40150      IF (AVS - 0.50000E-04) 10150, 10150, 20150                   02910373
10150      IVPASS = IVPASS + 1                                          02920373
           WRITE (NUVI, 80002) IVTNUM                                   02930373
           GO TO 0151                                                   02940373
20150      IVFAIL = IVFAIL + 1                                          02950373
           RVCORR = 0.00000000000000                                    02960373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02970373
 0151      CONTINUE                                                     02980373
CT016*  TEST 16                                 TEST VALUES NEAR 3*PI/2 02990373
           IVTNUM = 16                                                  03000373
        BVS = (3.0 * PIVS / 2.0) - 1.0 / 16.0                           03010373
        AVS = COS(BVS)                                                  03020373
           IF (AVS + 0.62463E-01) 20160, 10160, 40160                   03030373
40160      IF (AVS + 0.62456E-01) 10160, 10160, 20160                   03040373
10160      IVPASS = IVPASS + 1                                          03050373
           WRITE (NUVI, 80002) IVTNUM                                   03060373
           GO TO 0161                                                   03070373
20160      IVFAIL = IVFAIL + 1                                          03080373
           RVCORR = -0.06245931784238                                   03090373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03100373
 0161      CONTINUE                                                     03110373
CT017*  TEST 17                                 TEST VALUES NEAR 3*PI/2 03120373
           IVTNUM = 17                                                  03130373
        BVS = (3.0 * PIVS / 2.0) + 1.0 / 512.0                          03140373
        AVS = COS(BVS)                                                  03150373
           IF (AVS - 0.19530E-02) 20170, 10170, 40170                   03160373
40170      IF (AVS - 0.19533E-02) 10170, 10170, 20170                   03170373
10170      IVPASS = IVPASS + 1                                          03180373
           WRITE (NUVI, 80002) IVTNUM                                   03190373
           GO TO 0171                                                   03200373
20170      IVFAIL = IVFAIL + 1                                          03210373
           RVCORR = 0.00195312375824                                    03220373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03230373
70171      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       03230*TI
           WRITE (NUVI,70171)                                           03230*TI
 0171      CONTINUE                                                     03240373
CT018*  TEST 18                               ARGUMENT OF LOW MAGNITUDE 03250373
           IVTNUM = 18                                                  03260373
        BVS = -3.141593E-35                                             03270373
        AVS = COS(BVS)                                                  03280373
           IF (AVS - 0.99995E+00) 20180, 10180, 40180                   03290373
40180      IF (AVS - 0.10001E+01) 10180, 10180, 20180                   03300373
10180      IVPASS = IVPASS + 1                                          03310373
           WRITE (NUVI, 80002) IVTNUM                                   03320373
           GO TO 0181                                                   03330373
20180      IVFAIL = IVFAIL + 1                                          03340373
           RVCORR = 1.00000000000000                                    03350373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03360373
 0181      CONTINUE                                                     03370373
CT019*  TEST 19                              THE FUNCTION APPLIED TWICE 03380373
           IVTNUM = 19                                                  03390373
        AVS = COS(PIVS / 4.0) * COS(3.0 * PIVS / 4.0)                   03400373
           IF (AVS + 0.50003E+00) 20190, 10190, 40190                   03410373
40190      IF (AVS + 0.49997E+00) 10190, 10190, 20190                   03420373
10190      IVPASS = IVPASS + 1                                          03430373
           WRITE (NUVI, 80002) IVTNUM                                   03440373
           GO TO 0191                                                   03450373
20190      IVFAIL = IVFAIL + 1                                          03460373
           RVCORR = -0.50000000000000                                   03470373
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03480373
 0191      CONTINUE                                                     03490373
C*****                                                                  03500373
CBB** ********************** BBCSUM0  **********************************03510373
C**** WRITE OUT TEST SUMMARY                                            03520373
C****                                                                   03530373
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03540373
      WRITE (I02, 90004)                                                03550373
      WRITE (I02, 90014)                                                03560373
      WRITE (I02, 90004)                                                03570373
      WRITE (I02, 90020) IVPASS                                         03580373
      WRITE (I02, 90022) IVFAIL                                         03590373
      WRITE (I02, 90024) IVDELE                                         03600373
      WRITE (I02, 90026) IVINSP                                         03610373
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03620373
CBE** ********************** BBCSUM0  **********************************03630373
CBB** ********************** BBCFOOT0 **********************************03640373
C**** WRITE OUT REPORT FOOTINGS                                         03650373
C****                                                                   03660373
      WRITE (I02,90016) ZPROG, ZPROG                                    03670373
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03680373
      WRITE (I02,90019)                                                 03690373
CBE** ********************** BBCFOOT0 **********************************03700373
CBB** ********************** BBCFMT0A **********************************03710373
C**** FORMATS FOR TEST DETAIL LINES                                     03720373
C****                                                                   03730373
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03740373
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03750373
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03760373
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03770373
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03780373
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03790373
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03800373
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03810373
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03820373
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03830373
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03840373
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03850373
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03860373
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03870373
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03880373
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03890373
80050 FORMAT (1H ,48X,A31)                                              03900373
CBE** ********************** BBCFMT0A **********************************03910373
CBB** ********************** BBCFMT0B **********************************03920373
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03930373
C****                                                                   03940373
90002 FORMAT (1H1)                                                      03950373
90004 FORMAT (1H )                                                      03960373
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03970373
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03980373
90008 FORMAT (1H ,21X,A13,A17)                                          03990373
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04000373
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04010373
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04020373
     1       7X,7HREMARKS,24X)                                          04030373
90014 FORMAT (1H ,46H----------------------------------------------,    04040373
     1        33H---------------------------------)                     04050373
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04060373
C****                                                                   04070373
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04080373
C****                                                                   04090373
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04100373
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04110373
     1        A13)                                                      04120373
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04130373
C****                                                                   04140373
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04150373
C****                                                                   04160373
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04170373
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04180373
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04190373
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04200373
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04210373
CBE** ********************** BBCFMT0B **********************************04220373
C*****                                                                  04230373
C*****    END OF TEST SEGMENT 189                                       04240373
      STOP                                                              04250373
      END                                                               04260373
                                                                        04270373
