C***********************************************************************00010376
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020376
C*****   FM376                                                          00030376
C*****                       XATAN - (195)                              00040376
C*****                                                                  00050376
C***********************************************************************00060376
C*****  GENERAL PURPOSE                                     SUBSET REF  00070376
C*****    TEST INTRINSIC FUNCTION ATAN, ATAN2                 15.3      00080376
C*****    INTRINSIC FUNCTION SQRT ASSUMED WORKING            TABLE 5    00090376
C*****                                                                  00100376
CBB** ********************** BBCCOMNT **********************************00110376
C****                                                                   00120376
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130376
C****                          VERSION 2.0                              00140376
C****                                                                   00150376
C****                                                                   00160376
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170376
C****                   GENERAL SERVICES ADMINISTRATION                 00180376
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190376
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200376
C****                      FALLS CHURCH, VA. 22041                      00210376
C****                                                                   00220376
C****                          (703) 756-6153                           00230376
C****                                                                   00240376
CBE** ********************** BBCCOMNT **********************************00250376
CBB** ********************** BBCINITA **********************************00260376
C**** SPECIFICATION STATEMENTS                                          00270376
C****                                                                   00280376
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290376
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300376
CBE** ********************** BBCINITA **********************************00310376
CBB** ********************** BBCINITB **********************************00320376
C**** INITIALIZE SECTION                                                00330376
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340376
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350376
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360376
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370376
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380376
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390376
      DATA   REMRKS /'                               '/                 00400376
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410376
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420376
C****                                                                   00430376
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440376
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450376
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460376
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530376
      IVPASS = 0                                                        00540376
      IVFAIL = 0                                                        00550376
      IVDELE = 0                                                        00560376
      IVINSP = 0                                                        00570376
      IVTOTL = 0                                                        00580376
      IVTOTN = 0                                                        00590376
      ICZERO = 0                                                        00600376
C                                                                       00610376
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620376
      I01 = 05                                                          00630376
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640376
      I02 = 06                                                          00650376
C                                                                       00660376
      I01 = 5                                                           00670376
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680376
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690376
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700376
C                                                                       00710376
      I02 = 6                                                           00720376
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730376
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740376
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750376
C                                                                       00760376
CBE** ********************** BBCINITB **********************************00770376
      NUVI = I02                                                        00780376
      IVTOTL = 13                                                       00790376
      ZPROG = 'FM376'                                                   00800376
CBB** ********************** BBCHED0A **********************************00810376
C****                                                                   00820376
C**** WRITE REPORT TITLE                                                00830376
C****                                                                   00840376
      WRITE (I02, 90002)                                                00850376
      WRITE (I02, 90006)                                                00860376
      WRITE (I02, 90007)                                                00870376
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880376
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890376
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900376
CBE** ********************** BBCHED0A **********************************00910376
C*****                                                                  00920376
C*****    HEADER FOR SEGMENT 195                                        00930376
        WRITE(NUVI,19500)                                               00940376
19500   FORMAT(1H , / 35H  XATAN - (195) INTRINSIC FUNCTIONS//          00950376
     1         28H  ATAN, ATAN2   (ARCTANGENT)//                        00960376
     2         20H  SUBSET REF. - 15.3)                                 00970376
CBB** ********************** BBCHED0B **********************************00980376
C**** WRITE DETAIL REPORT HEADERS                                       00990376
C****                                                                   01000376
      WRITE (I02,90004)                                                 01010376
      WRITE (I02,90004)                                                 01020376
      WRITE (I02,90013)                                                 01030376
      WRITE (I02,90014)                                                 01040376
      WRITE (I02,90015) IVTOTL                                          01050376
CBE** ********************** BBCHED0B **********************************01060376
C*****                                                                  01070376
        WRITE(NUVI,19501)                                               01080376
C*****                                                                  01090376
19501   FORMAT(/ 8X, 12HTEST OF ATAN)                                   01100376
C*****                                                                  01110376
CT001*  TEST 1                    TEST LARGE VALUES TO TEST SINGULARITY 01120376
           IVTNUM = 1                                                   01130376
        BVS = 500.0                                                     01140376
        AVS = ATAN(BVS)                                                 01150376
           IF (AVS - 0.15687E+01) 20010, 10010, 40010                   01160376
40010      IF (AVS - 0.15689E+01) 10010, 10010, 20010                   01170376
10010      IVPASS = IVPASS + 1                                          01180376
           WRITE (NUVI, 80002) IVTNUM                                   01190376
           GO TO 0011                                                   01200376
20010      IVFAIL = IVFAIL + 1                                          01210376
           RVCORR = 1.56879632946156                                    01220376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01230376
 0011      CONTINUE                                                     01240376
CT002*  TEST 2                    TEST LARGE VALUES TO TEST SINGULARITY 01250376
           IVTNUM = 2                                                   01260376
        AVS = ATAN(-1000.0)                                             01270376
           IF (AVS + 0.15699E+01) 20020, 10020, 40020                   01280376
40020      IF (AVS + 0.15697E+01) 10020, 10020, 20020                   01290376
10020      IVPASS = IVPASS + 1                                          01300376
           WRITE (NUVI, 80002) IVTNUM                                   01310376
           GO TO 0021                                                   01320376
20020      IVFAIL = IVFAIL + 1                                          01330376
           RVCORR = -1.56979632712823                                   01340376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01350376
 0021      CONTINUE                                                     01360376
CT003*  TEST 3                          AN EXPRESSION PRESENTED TO ATAN 01370376
           IVTNUM = 3                                                   01380376
        AVS = ATAN(100.0 / 100.0)                                       01390376
           IF (AVS - 0.78535E+00) 20030, 10030, 40030                   01400376
40030      IF (AVS - 0.78544E+00) 10030, 10030, 20030                   01410376
10030      IVPASS = IVPASS + 1                                          01420376
           WRITE (NUVI, 80002) IVTNUM                                   01430376
           GO TO 0031                                                   01440376
20030      IVFAIL = IVFAIL + 1                                          01450376
           RVCORR = 0.78539816339745                                    01460376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01470376
 0031      CONTINUE                                                     01480376
CT004*  TEST 4                             A VARIABLE PRESENTED TO ATAN 01490376
           IVTNUM = 4                                                   01500376
        BVS = -SQRT(3.0)                                                01510376
        AVS = ATAN(BVS)                                                 01520376
           IF (AVS + 0.10473E+01) 20040, 10040, 40040                   01530376
40040      IF (AVS + 0.10471E+01) 10040, 10040, 20040                   01540376
10040      IVPASS = IVPASS + 1                                          01550376
           WRITE (NUVI, 80002) IVTNUM                                   01560376
           GO TO 0041                                                   01570376
20040      IVFAIL = IVFAIL + 1                                          01580376
           RVCORR = -1.04719755119660                                   01590376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01600376
 0041      CONTINUE                                                     01610376
CT005*  TEST 5                             AN ARGUMENT OF LOW MAGNITUDE 01620376
           IVTNUM = 5                                                   01630376
        AVS = ATAN(1.0E-16)                                             01640376
           IF (AVS - 0.99995E-16) 20050, 10050, 40050                   01650376
40050      IF (AVS - 0.10001E-15) 10050, 10050, 20050                   01660376
10050      IVPASS = IVPASS + 1                                          01670376
           WRITE (NUVI, 80002) IVTNUM                                   01680376
           GO TO 0051                                                   01690376
20050      IVFAIL = IVFAIL + 1                                          01700376
           RVCORR = 1.00000000000000E-16                                01710376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01720376
 0051      CONTINUE                                                     01730376
CT006*  TEST 6                            AN ARGUMENT OF HIGH MAGNITUDE 01740376
           IVTNUM = 6                                                   01750376
        AVS = ATAN(-2.0E+34)                                            01760376
           IF (AVS + 0.15709E+01) 20060, 10060, 40060                   01770376
40060      IF (AVS + 0.15707E+01) 10060, 10060, 20060                   01780376
10060      IVPASS = IVPASS + 1                                          01790376
           WRITE (NUVI, 80002) IVTNUM                                   01800376
           GO TO 0061                                                   01810376
20060      IVFAIL = IVFAIL + 1                                          01820376
           RVCORR = -1.57079632679490                                   01830376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01840376
 0061      CONTINUE                                                     01850376
C*****                                                                  01860376
        WRITE(NUVI,19508)                                               01870376
19508   FORMAT(/ 08X, 13HTEST OF ATAN2)                                 01880376
CT007*  TEST 7                              TEST ATAN2 FOR (0,POSITIVE) 01890376
           IVTNUM = 7                                                   01900376
        BVS = 10.0 / 10.0                                               01910376
        CVS = 0.0                                                       01920376
        AVS = ATAN2(CVS, BVS)                                           01930376
           IF (AVS + 0.50000E-04) 20070, 10070, 40070                   01940376
40070      IF (AVS - 0.50000E-04) 10070, 10070, 20070                   01950376
10070      IVPASS = IVPASS + 1                                          01960376
           WRITE (NUVI, 80002) IVTNUM                                   01970376
           GO TO 0071                                                   01980376
20070      IVFAIL = IVFAIL + 1                                          01990376
           RVCORR = 0.00000000000000                                    02000376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02010376
 0071      CONTINUE                                                     02020376
CT008*  TEST 8                             TEST ATAN2 FOR (0, NEGATIVE) 02030376
           IVTNUM = 8                                                   02040376
        BVS = 0.0                                                       02050376
        CVS = -25.0 / 2.0                                               02060376
        AVS = ATAN2(BVS, CVS)                                           02070376
           IF (AVS - 0.31414E+01) 20080, 10080, 40080                   02080376
40080      IF (AVS - 0.31418E+01) 10080, 10080, 20080                   02090376
10080      IVPASS = IVPASS + 1                                          02100376
           WRITE (NUVI, 80002) IVTNUM                                   02110376
           GO TO 0081                                                   02120376
20080      IVFAIL = IVFAIL + 1                                          02130376
           RVCORR = 3.14159265358979                                    02140376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02150376
 0081      CONTINUE                                                     02160376
CT009*  TEST 9                         AN EXPRESSION PRESENTED TO ATAN2 02170376
           IVTNUM = 9                                                   02180376
        BVS = 1.0                                                       02190376
        CVS = BVS + BVS                                                 02200376
        AVS = ATAN2(BVS * 2.0, CVS)                                     02210376
           IF (AVS - 0.78535E+00) 20090, 10090, 40090                   02220376
40090      IF (AVS - 0.78544E+00) 10090, 10090, 20090                   02230376
10090      IVPASS = IVPASS + 1                                          02240376
           WRITE (NUVI, 80002) IVTNUM                                   02250376
           GO TO 0091                                                   02260376
20090      IVFAIL = IVFAIL + 1                                          02270376
           RVCORR = 0.78539816339745                                    02280376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02290376
 0091      CONTINUE                                                     02300376
CT010*  TEST 10                         TEST ATAN2(X,Y) FOR X NEAR ZERO 02310376
           IVTNUM = 10                                                  02320376
        BVS = ASIN(0.6)                                                 02330376
        CVS = ACOS(0.8)                                                 02340376
        AVS = ATAN2(BVS, CVS)                                           02350376
           IF (AVS - 0.78535E+00) 20100, 10100, 40100                   02360376
40100      IF (AVS - 0.78544E+00) 10100, 10100, 20100                   02370376
10100      IVPASS = IVPASS + 1                                          02380376
           WRITE (NUVI, 80002) IVTNUM                                   02390376
           GO TO 0101                                                   02400376
20100      IVFAIL = IVFAIL + 1                                          02410376
           RVCORR = 0.78539816339745                                    02420376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02430376
 0101      CONTINUE                                                     02440376
CT011*  TEST 11                          WHERE ATAN2(X,Y) IS ZERO FOR Y 02450376
           IVTNUM = 11                                                  02460376
        AVS = ATAN2(1.2, 0.0)                                           02470376
           IF (AVS - 0.15707E+01) 20110, 10110, 40110                   02480376
40110      IF (AVS - 0.15709E+01) 10110, 10110, 20110                   02490376
10110      IVPASS = IVPASS + 1                                          02500376
           WRITE (NUVI, 80002) IVTNUM                                   02510376
           GO TO 0111                                                   02520376
20110      IVFAIL = IVFAIL + 1                                          02530376
           RVCORR = 1.57079632679490                                    02540376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02550376
 0111      CONTINUE                                                     02560376
CT012*  TEST 12                          WHERE ATAN2(X,Y) IS ZERO FOR Y 02570376
           IVTNUM = 12                                                  02580376
        BVS = -2.5                                                      02590376
        CVS = 0.0                                                       02600376
        AVS = ATAN2(BVS, CVS)                                           02610376
           IF (AVS + 0.15709E+01) 20120, 10120, 40120                   02620376
40120      IF (AVS + 0.15707E+01) 10120, 10120, 20120                   02630376
10120      IVPASS = IVPASS + 1                                          02640376
           WRITE (NUVI, 80002) IVTNUM                                   02650376
           GO TO 0121                                                   02660376
20120      IVFAIL = IVFAIL + 1                                          02670376
           RVCORR = -1.57079632679490                                   02680376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02690376
 0121      CONTINUE                                                     02700376
CT013*  TEST 13                           COMPARISON OF ATAN WITH ATAN2 02710376
           IVTNUM = 13                                                  02720376
        AVS = (ATAN(SQRT(3.0) / 3.0) * 2.0)                             02730376
     1             + ATAN2(-SQRT(3.0) / 2.0, 1.0 / 2.0)                 02740376
           IF (AVS + 0.50000E-04) 20130, 10130, 40130                   02750376
40130      IF (AVS - 0.50000E-04) 10130, 10130, 20130                   02760376
10130      IVPASS = IVPASS + 1                                          02770376
           WRITE (NUVI, 80002) IVTNUM                                   02780376
           GO TO 0131                                                   02790376
20130      IVFAIL = IVFAIL + 1                                          02800376
           RVCORR = 0.00000000000000                                    02810376
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02820376
 0131      CONTINUE                                                     02830376
C*****                                                                  02840376
CBB** ********************** BBCSUM0  **********************************02850376
C**** WRITE OUT TEST SUMMARY                                            02860376
C****                                                                   02870376
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02880376
      WRITE (I02, 90004)                                                02890376
      WRITE (I02, 90014)                                                02900376
      WRITE (I02, 90004)                                                02910376
      WRITE (I02, 90020) IVPASS                                         02920376
      WRITE (I02, 90022) IVFAIL                                         02930376
      WRITE (I02, 90024) IVDELE                                         02940376
      WRITE (I02, 90026) IVINSP                                         02950376
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02960376
CBE** ********************** BBCSUM0  **********************************02970376
CBB** ********************** BBCFOOT0 **********************************02980376
C**** WRITE OUT REPORT FOOTINGS                                         02990376
C****                                                                   03000376
      WRITE (I02,90016) ZPROG, ZPROG                                    03010376
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03020376
      WRITE (I02,90019)                                                 03030376
CBE** ********************** BBCFOOT0 **********************************03040376
CBB** ********************** BBCFMT0A **********************************03050376
C**** FORMATS FOR TEST DETAIL LINES                                     03060376
C****                                                                   03070376
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03080376
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03090376
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03100376
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03110376
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03120376
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03130376
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03140376
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03150376
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03160376
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03170376
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03180376
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03190376
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03200376
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03210376
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03220376
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03230376
80050 FORMAT (1H ,48X,A31)                                              03240376
CBE** ********************** BBCFMT0A **********************************03250376
CBB** ********************** BBCFMT0B **********************************03260376
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03270376
C****                                                                   03280376
90002 FORMAT (1H1)                                                      03290376
90004 FORMAT (1H )                                                      03300376
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03310376
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03320376
90008 FORMAT (1H ,21X,A13,A17)                                          03330376
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03340376
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03350376
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03360376
     1       7X,7HREMARKS,24X)                                          03370376
90014 FORMAT (1H ,46H----------------------------------------------,    03380376
     1        33H---------------------------------)                     03390376
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03400376
C****                                                                   03410376
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03420376
C****                                                                   03430376
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03440376
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03450376
     1        A13)                                                      03460376
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03470376
C****                                                                   03480376
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03490376
C****                                                                   03500376
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03510376
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03520376
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03530376
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03540376
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03550376
CBE** ********************** BBCFMT0B **********************************03560376
C*****                                                                  03570376
C*****    END OF TEST SEGMENT 195                                       03580376
      STOP                                                              03590376
      END                                                               03600376
                                                                        03610376
