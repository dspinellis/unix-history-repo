C***********************************************************************00010374
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020374
C*****   FM374                                                          00030374
C*****                       XTAN - (191)                               00040374
C*****                                                                  00050374
C***********************************************************************00060374
C*****  GENERAL PURPOSE                                      SUBSET REF 00070374
C*****    TEST INTRINSIC FUNCTION TAN                          15.3     00080374
C*****                                                        TABLE 5   00090374
C*****                                                                  00100374
CBB** ********************** BBCCOMNT **********************************00110374
C****                                                                   00120374
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130374
C****                          VERSION 2.0                              00140374
C****                                                                   00150374
C****                                                                   00160374
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170374
C****                   GENERAL SERVICES ADMINISTRATION                 00180374
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190374
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200374
C****                      FALLS CHURCH, VA. 22041                      00210374
C****                                                                   00220374
C****                          (703) 756-6153                           00230374
C****                                                                   00240374
CBE** ********************** BBCCOMNT **********************************00250374
CBB** ********************** BBCINITA **********************************00260374
C**** SPECIFICATION STATEMENTS                                          00270374
C****                                                                   00280374
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290374
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300374
CBE** ********************** BBCINITA **********************************00310374
CBB** ********************** BBCINITB **********************************00320374
C**** INITIALIZE SECTION                                                00330374
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340374
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350374
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360374
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370374
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380374
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390374
      DATA   REMRKS /'                               '/                 00400374
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410374
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420374
C****                                                                   00430374
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440374
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450374
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460374
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530374
      IVPASS = 0                                                        00540374
      IVFAIL = 0                                                        00550374
      IVDELE = 0                                                        00560374
      IVINSP = 0                                                        00570374
      IVTOTL = 0                                                        00580374
      IVTOTN = 0                                                        00590374
      ICZERO = 0                                                        00600374
C                                                                       00610374
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620374
      I01 = 05                                                          00630374
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640374
      I02 = 06                                                          00650374
C                                                                       00660374
      I01 = 5                                                           00670374
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680374
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690374
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700374
C                                                                       00710374
      I02 = 6                                                           00720374
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730374
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740374
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750374
C                                                                       00760374
CBE** ********************** BBCINITB **********************************00770374
      NUVI = I02                                                        00780374
      IVTOTL = 14                                                       00790374
      ZPROG = 'FM374'                                                   00800374
CBB** ********************** BBCHED0A **********************************00810374
C****                                                                   00820374
C**** WRITE REPORT TITLE                                                00830374
C****                                                                   00840374
      WRITE (I02, 90002)                                                00850374
      WRITE (I02, 90006)                                                00860374
      WRITE (I02, 90007)                                                00870374
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880374
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890374
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900374
CBE** ********************** BBCHED0A **********************************00910374
C*****                                                                  00920374
C*****    HEADER FOR SEGMENT 191                                        00930374
        WRITE(NUVI,19100)                                               00940374
19100   FORMAT(1H , / 34H  XTAN - (191) INTRINSIC FUNCTIONS//           00950374
     1         17H  TAN   (TANGENT)//                                   00960374
     2         20H  SUBSET REF. - 15.3)                                 00970374
CBB** ********************** BBCHED0B **********************************00980374
C**** WRITE DETAIL REPORT HEADERS                                       00990374
C****                                                                   01000374
      WRITE (I02,90004)                                                 01010374
      WRITE (I02,90004)                                                 01020374
      WRITE (I02,90013)                                                 01030374
      WRITE (I02,90014)                                                 01040374
      WRITE (I02,90015) IVTOTL                                          01050374
CBE** ********************** BBCHED0B **********************************01060374
C*****                                                                  01070374
        PIVS = 3.1415926535897932384626434                              01080374
C*****                                                                  01090374
CT001*  TEST 1                             ZERO (0.0), SINCE TAN(0) = 0 01100374
           IVTNUM = 1                                                   01110374
        BVS = 0.0                                                       01120374
        AVS = TAN(BVS)                                                  01130374
           IF (AVS + 0.00005) 20010, 10010, 40010                       01140374
40010      IF (AVS - 0.00005) 10010, 10010, 20010                       01150374
10010      IVPASS = IVPASS + 1                                          01160374
           WRITE (NUVI, 80002) IVTNUM                                   01170374
           GO TO 0011                                                   01180374
20010      IVFAIL = IVFAIL + 1                                          01190374
           RVCORR = 0.0                                                 01200374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01210374
 0011      CONTINUE                                                     01220374
CT002*  TEST 2                                                     2*PI 01230374
           IVTNUM = 2                                                   01240374
        BVS = 6.2831853071                                              01250374
        AVS = TAN(BVS)                                                  01260374
           IF (AVS + 0.00005) 20020, 10020, 40020                       01270374
40020      IF (AVS - 0.00005) 10020, 10020, 20020                       01280374
10020      IVPASS = IVPASS + 1                                          01290374
           WRITE (NUVI, 80002) IVTNUM                                   01300374
           GO TO 0021                                                   01310374
20020      IVFAIL = IVFAIL + 1                                          01320374
           RVCORR = 0.0                                                 01330374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01340374
 0021      CONTINUE                                                     01350374
CT003*  TEST 3                                                     3*PI 01360374
           IVTNUM = 3                                                   01370374
        BVS = 9.424777960                                               01380374
        AVS = TAN(BVS)                                                  01390374
           IF (AVS + 0.00005) 20030, 10030, 40030                       01400374
40030      IF (AVS - 0.00005) 10030, 10030, 20030                       01410374
10030      IVPASS = IVPASS + 1                                          01420374
           WRITE (NUVI, 80002) IVTNUM                                   01430374
           GO TO 0031                                                   01440374
20030      IVFAIL = IVFAIL + 1                                          01450374
           RVCORR = 0.0                                                 01460374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01470374
 0031      CONTINUE                                                     01480374
CT004*  TEST 4                                                     PI/4 01490374
           IVTNUM = 4                                                   01500374
        AVS = TAN(PIVS / 4.0)                                           01510374
           IF (AVS - 0.99995) 20040, 10040, 40040                       01520374
40040      IF (AVS - 1.0001) 10040, 10040, 20040                        01530374
10040      IVPASS = IVPASS + 1                                          01540374
           WRITE (NUVI, 80002) IVTNUM                                   01550374
           GO TO 0041                                                   01560374
20040      IVFAIL = IVFAIL + 1                                          01570374
           RVCORR = 1.0                                                 01580374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01590374
 0041      CONTINUE                                                     01600374
CT005*  TEST 5                                                   5*PI/4 01610374
           IVTNUM = 5                                                   01620374
        BVS = 5.0 * PIVS / 4.0                                          01630374
        AVS = TAN(BVS)                                                  01640374
           IF (AVS - 0.99995) 20050, 10050, 40050                       01650374
40050      IF (AVS - 1.0001) 10050, 10050, 20050                        01660374
10050      IVPASS = IVPASS + 1                                          01670374
           WRITE (NUVI, 80002) IVTNUM                                   01680374
           GO TO 0051                                                   01690374
20050      IVFAIL = IVFAIL + 1                                          01700374
           RVCORR = 1.0                                                 01710374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01720374
 0051      CONTINUE                                                     01730374
CT006*  TEST 6                                         A NEGATIVE VALUE 01740374
           IVTNUM = 6                                                   01750374
        BVS = -2.0 / 1.0                                                01760374
        AVS = TAN(BVS)                                                  01770374
           IF (AVS - 2.1849) 20060, 10060, 40060                        01780374
40060      IF (AVS - 2.1852) 10060, 10060, 20060                        01790374
10060      IVPASS = IVPASS + 1                                          01800374
           WRITE (NUVI, 80002) IVTNUM                                   01810374
           GO TO 0061                                                   01820374
20060      IVFAIL = IVFAIL + 1                                          01830374
           RVCORR = 2.18503986326151                                    01840374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01850374
 0061      CONTINUE                                                     01860374
CT007*  TEST 7                                         A POSITIVE VALUE 01870374
           IVTNUM = 7                                                   01880374
        BVS = 350.0 / 100.0                                             01890374
        AVS = TAN(BVS)                                                  01900374
           IF (AVS - 0.37456) 20070, 10070, 40070                       01910374
40070      IF (AVS - 0.37461) 10070, 10070, 20070                       01920374
10070      IVPASS = IVPASS + 1                                          01930374
           WRITE (NUVI, 80002) IVTNUM                                   01940374
           GO TO 0071                                                   01950374
20070      IVFAIL = IVFAIL + 1                                          01960374
           RVCORR = 0.37458564015859                                    01970374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01980374
 0071      CONTINUE                                                     01990374
CT008*  TEST 8                                           (PI / 2) - 1/8 02000374
           IVTNUM = 8                                                   02010374
        BVS = 1.4457963267                                              02020374
        AVS = TAN(BVS)                                                  02030374
           IF (AVS - 7.9578) 20080, 10080, 40080                        02040374
40080      IF (AVS - 7.9587) 10080, 10080, 20080                        02050374
10080      IVPASS = IVPASS + 1                                          02060374
           WRITE (NUVI, 80002) IVTNUM                                   02070374
           GO TO 0081                                                   02080374
20080      IVFAIL = IVFAIL + 1                                          02090374
           RVCORR = 7.95828986586701                                    02100374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02110374
 0081      CONTINUE                                                     02120374
CT009*  TEST 9                                         (PI / 2) + 1/256 02130374
           IVTNUM = 9                                                   02140374
        BVS = 1.5747025767                                              02150374
        AVS = TAN(BVS)                                                  02160374
           IF (AVS + 256.02) 20090, 10090, 40090                        02170374
40090      IF (AVS + 255.98) 10090, 10090, 20090                        02180374
10090      IVPASS = IVPASS + 1                                          02190374
           WRITE (NUVI, 80002) IVTNUM                                   02200374
           GO TO 0091                                                   02210374
20090      IVFAIL = IVFAIL + 1                                          02220374
           RVCORR = -255.99869791534212                                 02230374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02240374
 0091      CONTINUE                                                     02250374
CT010*  TEST 10                                         3*PI/2 - 1/1024 02260374
           IVTNUM = 10                                                  02270374
        AVS = TAN((3.0 * PIVS / 2.0) - 1.0 / 1024.0)                    02280374
           IF (AVS - 1023.9) 20100, 10100, 40100                        02290374
40100      IF (AVS - 1024.1) 10100, 10100, 20100                        02300374
10100      IVPASS = IVPASS + 1                                          02310374
           WRITE (NUVI, 80002) IVTNUM                                   02320374
           GO TO 0101                                                   02330374
20100      IVFAIL = IVFAIL + 1                                          02340374
           RVCORR = 1023.99967447914597                                 02350374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02360374
70101      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       02360*TI
           WRITE (NUVI,70101)                                           02360*TI
 0101      CONTINUE                                                     02370374
CT011*  TEST 11                                           3*PI/2 + 1/64 02380374
           IVTNUM = 11                                                  02390374
        BVS = (3.0 * PIVS / 2.0) + 1.0 / 64.0                           02400374
        AVS = TAN(BVS)                                                  02410374
           IF (AVS + 63.998) 20110, 10110, 40110                        02420374
40110      IF (AVS + 63.991) 10110, 10110, 20110                        02430374
10110      IVPASS = IVPASS + 1                                          02440374
           WRITE (NUVI, 80002) IVTNUM                                   02450374
           GO TO 0111                                                   02460374
20110      IVFAIL = IVFAIL + 1                                          02470374
           RVCORR = -63.99479158189365                                  02480374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02490374
70111      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       02490*TI
           WRITE (NUVI,70111)                                           02490*TI
 0111      CONTINUE                                                     02500374
CT012*  TEST 12               LARGE ARGUMENT TO TEST ARGUMENT REDUCTION 02510374
           IVTNUM = 12                                                  02520374
        AVS = TAN(2000.0)                                               02530374
           IF (AVS + 2.5312) 20120, 10120, 40120                        02540374
40120      IF (AVS + 2.5308) 10120, 10120, 20120                        02550374
10120      IVPASS = IVPASS + 1                                          02560374
           WRITE (NUVI, 80002) IVTNUM                                   02570374
           GO TO 0121                                                   02580374
20120      IVFAIL = IVFAIL + 1                                          02590374
           RVCORR = -2.53099832809334                                   02600374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02610374
 0121      CONTINUE                                                     02620374
CT013*  TEST 13                               ARGUMENT OF LOW MAGNITUDE 02630374
           IVTNUM = 13                                                  02640374
        BVS = PIVS * 1.0E-35                                            02650374
        AVS = TAN(BVS)                                                  02660374
           IF (AVS - 3.1414E-35) 20130, 10130, 40130                    02670374
40130      IF (AVS - 3.1418E-35) 10130, 10130, 20130                    02680374
10130      IVPASS = IVPASS + 1                                          02690374
           WRITE (NUVI, 80002) IVTNUM                                   02700374
           GO TO 0131                                                   02710374
20130      IVFAIL = IVFAIL + 1                                          02720374
           RVCORR = 3.14159265358979E-35                                02730374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02740374
 0131      CONTINUE                                                     02750374
CT014*  TEST 14                              THE FUNCTION APPLIED TWICE 02760374
           IVTNUM = 14                                                  02770374
        AVS = TAN(PIVS / 6.0) * TAN(PIVS / 6.0)                         02780374
           IF (AVS - 0.33331) 20140, 10140, 40140                       02790374
40140      IF (AVS - 0.33335) 10140, 10140, 20140                       02800374
10140      IVPASS = IVPASS + 1                                          02810374
           WRITE (NUVI, 80002) IVTNUM                                   02820374
           GO TO 0141                                                   02830374
20140      IVFAIL = IVFAIL + 1                                          02840374
           RVCORR = 0.33333333333333                                    02850374
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02860374
 0141      CONTINUE                                                     02870374
C*****                                                                  02880374
CBB** ********************** BBCSUM0  **********************************02890374
C**** WRITE OUT TEST SUMMARY                                            02900374
C****                                                                   02910374
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02920374
      WRITE (I02, 90004)                                                02930374
      WRITE (I02, 90014)                                                02940374
      WRITE (I02, 90004)                                                02950374
      WRITE (I02, 90020) IVPASS                                         02960374
      WRITE (I02, 90022) IVFAIL                                         02970374
      WRITE (I02, 90024) IVDELE                                         02980374
      WRITE (I02, 90026) IVINSP                                         02990374
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03000374
CBE** ********************** BBCSUM0  **********************************03010374
CBB** ********************** BBCFOOT0 **********************************03020374
C**** WRITE OUT REPORT FOOTINGS                                         03030374
C****                                                                   03040374
      WRITE (I02,90016) ZPROG, ZPROG                                    03050374
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03060374
      WRITE (I02,90019)                                                 03070374
CBE** ********************** BBCFOOT0 **********************************03080374
CBB** ********************** BBCFMT0A **********************************03090374
C**** FORMATS FOR TEST DETAIL LINES                                     03100374
C****                                                                   03110374
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03120374
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03130374
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03140374
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03150374
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03160374
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03170374
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03180374
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03190374
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03200374
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03210374
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03220374
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03230374
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03240374
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03250374
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03260374
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03270374
80050 FORMAT (1H ,48X,A31)                                              03280374
CBE** ********************** BBCFMT0A **********************************03290374
CBB** ********************** BBCFMT0B **********************************03300374
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03310374
C****                                                                   03320374
90002 FORMAT (1H1)                                                      03330374
90004 FORMAT (1H )                                                      03340374
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03350374
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03360374
90008 FORMAT (1H ,21X,A13,A17)                                          03370374
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03380374
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03390374
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03400374
     1       7X,7HREMARKS,24X)                                          03410374
90014 FORMAT (1H ,46H----------------------------------------------,    03420374
     1        33H---------------------------------)                     03430374
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03440374
C****                                                                   03450374
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03460374
C****                                                                   03470374
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03480374
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03490374
     1        A13)                                                      03500374
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03510374
C****                                                                   03520374
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03530374
C****                                                                   03540374
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03550374
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03560374
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03570374
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03580374
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03590374
CBE** ********************** BBCFMT0B **********************************03600374
C*****                                                                  03610374
C*****    END OF TEST SEGMENT 191                                       03620374
      STOP                                                              03630374
      END                                                               03640374
                                                                        03650374
