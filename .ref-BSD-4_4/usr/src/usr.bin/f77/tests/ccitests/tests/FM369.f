C***********************************************************************00010369
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020369
C*****   FM369                                                          00030369
C*****                       XEXP - (178)                               00040369
C*****                                                                  00050369
C***********************************************************************00060369
C*****  GENERAL PURPOSE                                      SUBSET REF 00070369
C*****    TEST INTRINSIC FUNCTION EXP                          15.3     00080369
C*****                                                        TABLE 5   00090369
C*****                                                                  00100369
CBB** ********************** BBCCOMNT **********************************00110369
C****                                                                   00120369
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130369
C****                          VERSION 2.0                              00140369
C****                                                                   00150369
C****                                                                   00160369
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170369
C****                   GENERAL SERVICES ADMINISTRATION                 00180369
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190369
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200369
C****                      FALLS CHURCH, VA. 22041                      00210369
C****                                                                   00220369
C****                          (703) 756-6153                           00230369
C****                                                                   00240369
CBE** ********************** BBCCOMNT **********************************00250369
CBB** ********************** BBCINITA **********************************00260369
C**** SPECIFICATION STATEMENTS                                          00270369
C****                                                                   00280369
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290369
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300369
CBE** ********************** BBCINITA **********************************00310369
CBB** ********************** BBCINITB **********************************00320369
C**** INITIALIZE SECTION                                                00330369
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340369
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350369
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360369
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370369
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380369
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390369
      DATA   REMRKS /'                               '/                 00400369
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410369
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420369
C****                                                                   00430369
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440369
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450369
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460369
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530369
      IVPASS = 0                                                        00540369
      IVFAIL = 0                                                        00550369
      IVDELE = 0                                                        00560369
      IVINSP = 0                                                        00570369
      IVTOTL = 0                                                        00580369
      IVTOTN = 0                                                        00590369
      ICZERO = 0                                                        00600369
C                                                                       00610369
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620369
      I01 = 05                                                          00630369
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640369
      I02 = 06                                                          00650369
C                                                                       00660369
      I01 = 5                                                           00670369
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680369
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690369
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700369
C                                                                       00710369
      I02 = 6                                                           00720369
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730369
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740369
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750369
C                                                                       00760369
CBE** ********************** BBCINITB **********************************00770369
      NUVI = I02                                                        00780369
      IVTOTL = 19                                                       00790369
      ZPROG = 'FM369'                                                   00800369
CBB** ********************** BBCHED0A **********************************00810369
C****                                                                   00820369
C**** WRITE REPORT TITLE                                                00830369
C****                                                                   00840369
      WRITE (I02, 90002)                                                00850369
      WRITE (I02, 90006)                                                00860369
      WRITE (I02, 90007)                                                00870369
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880369
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890369
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900369
CBE** ********************** BBCHED0A **********************************00910369
C*****                                                                  00920369
C*****    HEADER FOR SEGMENT 178                                        00930369
        WRITE(NUVI,17800)                                               00940369
17800   FORMAT(1H , / 34H  XEXP - (178) INTRINSIC FUNCTIONS//           00950369
     1         19H  EXP (EXPONENTIAL)//                                 00960369
     2         20H  SUBSET REF. - 15.3)                                 00970369
CBB** ********************** BBCHED0B **********************************00980369
C**** WRITE DETAIL REPORT HEADERS                                       00990369
C****                                                                   01000369
      WRITE (I02,90004)                                                 01010369
      WRITE (I02,90004)                                                 01020369
      WRITE (I02,90013)                                                 01030369
      WRITE (I02,90014)                                                 01040369
      WRITE (I02,90015) IVTOTL                                          01050369
CBE** ********************** BBCHED0B **********************************01060369
C*****                                                                  01070369
CT001*  TEST 1                                  ZERO SINCE EXP(0.0) = 1 01080369
           IVTNUM = 1                                                   01090369
        BVS = 0.0                                                       01100369
        AVS = EXP(BVS)                                                  01110369
           IF (AVS - 0.99995E+00) 20010, 10010, 40010                   01120369
40010      IF (AVS - 0.10001E+01) 10010, 10010, 20010                   01130369
10010      IVPASS = IVPASS + 1                                          01140369
           WRITE (NUVI, 80002) IVTNUM                                   01150369
           GO TO 0011                                                   01160369
20010      IVFAIL = IVFAIL + 1                                          01170369
           RVCORR = 0.10000000000000E+01                                01180369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01190369
 0011      CONTINUE                                                     01200369
CT002*  TEST 2                                   ONE SINCE EXP(1.0) = E 01210369
           IVTNUM = 2                                                   01220369
        AVS = EXP(1.0)                                                  01230369
           IF (AVS - 0.27181E+01) 20020, 10020, 40020                   01240369
40020      IF (AVS - 0.27185E+01) 10020, 10020, 20020                   01250369
10020      IVPASS = IVPASS + 1                                          01260369
           WRITE (NUVI, 80002) IVTNUM                                   01270369
           GO TO 0021                                                   01280369
20020      IVFAIL = IVFAIL + 1                                          01290369
           RVCORR = 0.27182818284590E+01                                01300369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01310369
 0021      CONTINUE                                                     01320369
C*****  TESTS 3 THRU 5 - POSITIVE VALUES                                01330369
CT003*  TEST 3                                                          01340369
           IVTNUM = 3                                                   01350369
        AVS = EXP(2.0)                                                  01360369
           IF (AVS - 0.73886E+01) 20030, 10030, 40030                   01370369
40030      IF (AVS - 0.73895E+01) 10030, 10030, 20030                   01380369
10030      IVPASS = IVPASS + 1                                          01390369
           WRITE (NUVI, 80002) IVTNUM                                   01400369
           GO TO 0031                                                   01410369
20030      IVFAIL = IVFAIL + 1                                          01420369
           RVCORR = 0.73890560989307E+01                                01430369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01440369
 0031      CONTINUE                                                     01450369
CT004*  TEST 4                                                          01460369
           IVTNUM = 4                                                   01470369
        AVS = EXP(5.125)                                                01480369
           IF (AVS - 0.16816E+03) 20040, 10040, 40040                   01490369
40040      IF (AVS - 0.16819E+03) 10040, 10040, 20040                   01500369
10040      IVPASS = IVPASS + 1                                          01510369
           WRITE (NUVI, 80002) IVTNUM                                   01520369
           GO TO 0041                                                   01530369
20040      IVFAIL = IVFAIL + 1                                          01540369
           RVCORR = 0.16817414165185E+03                                01550369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01560369
 0041      CONTINUE                                                     01570369
CT005*  TEST 5                                                          01580369
           IVTNUM = 5                                                   01590369
        AVS = EXP(15.0)                                                 01600369
           IF (AVS - 0.32688E+07) 20050, 10050, 40050                   01610369
40050      IF (AVS - 0.32692E+07) 10050, 10050, 20050                   01620369
10050      IVPASS = IVPASS + 1                                          01630369
           WRITE (NUVI, 80002) IVTNUM                                   01640369
           GO TO 0051                                                   01650369
20050      IVFAIL = IVFAIL + 1                                          01660369
           RVCORR = 0.32690173724721E+07                                01670369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01680369
 0051      CONTINUE                                                     01690369
CT006*  TEST 6                                                          01700369
           IVTNUM = 6                                                   01710369
        BVS = 20.5                                                      01720369
        AVS = EXP(BVS)                                                  01730369
           IF (AVS - 0.79986E+09) 20060, 10060, 40060                   01740369
40060      IF (AVS - 0.79995E+09) 10060, 10060, 20060                   01750369
10060      IVPASS = IVPASS + 1                                          01760369
           WRITE (NUVI, 80002) IVTNUM                                   01770369
           GO TO 0061                                                   01780369
20060      IVFAIL = IVFAIL + 1                                          01790369
           RVCORR = 0.79990217747551E+09                                01800369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01810369
 0061      CONTINUE                                                     01820369
C***** TESTS 7 THRU 10 - EXPRESSION PRESENTED TO EXP                    01830369
CT007*  TEST 7                                                          01840369
           IVTNUM = 7                                                   01850369
        BVS = 4.5                                                       01860369
        AVS = EXP(BVS - 7.5)                                            01870369
           IF (AVS - 0.49784E-01) 20070, 10070, 40070                   01880369
40070      IF (AVS - 0.49790E-01) 10070, 10070, 20070                   01890369
10070      IVPASS = IVPASS + 1                                          01900369
           WRITE (NUVI, 80002) IVTNUM                                   01910369
           GO TO 0071                                                   01920369
20070      IVFAIL = IVFAIL + 1                                          01930369
           RVCORR = 0.49787068367864E-01                                01940369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01950369
 0071      CONTINUE                                                     01960369
CT008*  TEST 8                                                          01970369
           IVTNUM = 8                                                   01980369
        BVS = 0.25                                                      01990369
        AVS = EXP(BVS - 5.0)                                            02000369
           IF (AVS - 0.86512E-02) 20080, 10080, 40080                   02010369
40080      IF (AVS - 0.86522E-02) 10080, 10080, 20080                   02020369
10080      IVPASS = IVPASS + 1                                          02030369
           WRITE (NUVI, 80002) IVTNUM                                   02040369
           GO TO 0081                                                   02050369
20080      IVFAIL = IVFAIL + 1                                          02060369
           RVCORR = 0.86516952031206E-02                                02070369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02080369
 0081      CONTINUE                                                     02090369
CT009*  TEST 9                                                          02100369
           IVTNUM = 9                                                   02110369
        AVS = EXP(0.5 * (-20.0))                                        02120369
           IF (AVS - 0.45397E-04) 20090, 10090, 40090                   02130369
40090      IF (AVS - 0.45403E-04) 10090, 10090, 20090                   02140369
10090      IVPASS = IVPASS + 1                                          02150369
           WRITE (NUVI, 80002) IVTNUM                                   02160369
           GO TO 0091                                                   02170369
20090      IVFAIL = IVFAIL + 1                                          02180369
           RVCORR = 0.45399929762485E-04                                02190369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02200369
 0091      CONTINUE                                                     02210369
CT010*  TEST 10                                                         02220369
           IVTNUM = 10                                                  02230369
        BVS = 30.5                                                      02240369
        AVS = EXP(BVS * (-0.5))                                         02250369
           IF (AVS - 0.23822E-06) 20100, 10100, 40100                   02260369
40100      IF (AVS - 0.23825E-06) 10100, 10100, 20100                   02270369
10100      IVPASS = IVPASS + 1                                          02280369
           WRITE (NUVI, 80002) IVTNUM                                   02290369
           GO TO 0101                                                   02300369
20100      IVFAIL = IVFAIL + 1                                          02310369
           RVCORR = 0.23823696675018E-06                                02320369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02330369
 0101      CONTINUE                                                     02340369
C*****  TESTS 11 THRU 14 - VALUES CLOSE TO ONE                          02350369
CT011*  TEST 11                                                         02360369
           IVTNUM = 11                                                  02370369
        AVS = EXP(0.9921875)                                            02380369
           IF (AVS - 0.26970E+01) 20110, 10110, 40110                   02390369
40110      IF (AVS - 0.26973E+01) 10110, 10110, 20110                   02400369
10110      IVPASS = IVPASS + 1                                          02410369
           WRITE (NUVI, 80002) IVTNUM                                   02420369
           GO TO 0111                                                   02430369
20110      IVFAIL = IVFAIL + 1                                          02440369
           RVCORR = 0.26971279914439E+01                                02450369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02460369
 0111      CONTINUE                                                     02470369
CT012*  TEST 12                                                         02480369
           IVTNUM = 12                                                  02490369
        BVS = 0.9990234375                                              02500369
        AVS = EXP(BVS)                                                  02510369
           IF (AVS - 0.27155E+01) 20120, 10120, 40120                   02520369
40120      IF (AVS - 0.27158E+01) 10120, 10120, 20120                   02530369
10120      IVPASS = IVPASS + 1                                          02540369
           WRITE (NUVI, 80002) IVTNUM                                   02550369
           GO TO 0121                                                   02560369
20120      IVFAIL = IVFAIL + 1                                          02570369
           RVCORR = 0.27156285521169E+01                                02580369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02590369
 0121      CONTINUE                                                     02600369
C*****                                                                  02610369
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                        02620369
        WRITE (NUVI, 90002)                                             02630369
        WRITE (NUVI, 90013)                                             02640369
        WRITE (NUVI, 90014)                                             02650369
C*****                                                                  02660369
CT013*  TEST 13                                                         02670369
           IVTNUM = 13                                                  02680369
        AVS = EXP(1.00390625)                                           02690369
           IF (AVS - 0.27287E+01) 20130, 10130, 40130                   02700369
40130      IF (AVS - 0.27291E+01) 10130, 10130, 20130                   02710369
10130      IVPASS = IVPASS + 1                                          02720369
           WRITE (NUVI, 80002) IVTNUM                                   02730369
           GO TO 0131                                                   02740369
20130      IVFAIL = IVFAIL + 1                                          02750369
           RVCORR = 0.27289208827261E+01                                02760369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02770369
 0131      CONTINUE                                                     02780369
CT014*  TEST 14                                                         02790369
           IVTNUM = 14                                                  02800369
        BVS = 1.001953125                                               02810369
        AVS = EXP(BVS)                                                  02820369
           IF (AVS - 0.27234E+01) 20140, 10140, 40140                   02830369
40140      IF (AVS - 0.27238E+01) 10140, 10140, 20140                   02840369
10140      IVPASS = IVPASS + 1                                          02850369
           WRITE (NUVI, 80002) IVTNUM                                   02860369
           GO TO 0141                                                   02870369
20140      IVFAIL = IVFAIL + 1                                          02880369
           RVCORR = 0.27235961607435E+01                                02890369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02900369
 0141      CONTINUE                                                     02910369
C*****  TESTS 15 THRU 19 - VALUES CLOSE TO 1/E                          02920369
CT015*  TEST 15                                                         02930369
           IVTNUM = 15                                                  02940369
        BVS = 128.0                                                     02950369
        AVS = EXP(44. / BVS)                                            02960369
           IF (AVS - 0.14101E+01) 20150, 10150, 40150                   02970369
40150      IF (AVS - 0.14103E+01) 10150, 10150, 20150                   02980369
10150      IVPASS = IVPASS + 1                                          02990369
           WRITE (NUVI, 80002) IVTNUM                                   03000369
           GO TO 0151                                                   03010369
20150      IVFAIL = IVFAIL + 1                                          03020369
           RVCORR = 0.14102260349257E+01                                03030369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03040369
 0151      CONTINUE                                                     03050369
CT016*  TEST 16                                                         03060369
           IVTNUM = 16                                                  03070369
        BVS = 128.                                                      03080369
        AVS = EXP(45. / BVS)                                            03090369
           IF (AVS - 0.14212E+01) 20160, 10160, 40160                   03100369
40160      IF (AVS - 0.14214E+01) 10160, 10160, 20160                   03110369
10160      IVPASS = IVPASS + 1                                          03120369
           WRITE (NUVI, 80002) IVTNUM                                   03130369
           GO TO 0161                                                   03140369
20160      IVFAIL = IVFAIL + 1                                          03150369
           RVCORR = 0.14212865748007E+01                                03160369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03170369
 0161      CONTINUE                                                     03180369
CT017*  TEST 17                                                         03190369
           IVTNUM = 17                                                  03200369
        BVS = 128.                                                      03210369
        AVS = EXP(46. / BVS)                                            03220369
           IF (AVS - 0.14323E+01) 20170, 10170, 40170                   03230369
40170      IF (AVS - 0.14325E+01) 10170, 10170, 20170                   03240369
10170      IVPASS = IVPASS + 1                                          03250369
           WRITE (NUVI, 80002) IVTNUM                                   03260369
           GO TO 0171                                                   03270369
20170      IVFAIL = IVFAIL + 1                                          03280369
           RVCORR = 0.14324338635651E+01                                03290369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03300369
 0171      CONTINUE                                                     03310369
CT018*  TEST 18                                                         03320369
           IVTNUM = 18                                                  03330369
        BVS = 128.                                                      03340369
        AVS = EXP(47. / BVS)                                            03350369
           IF (AVS - 0.14436E+01) 20180, 10180, 40180                   03360369
40180      IF (AVS - 0.14438E+01) 10180, 10180, 20180                   03370369
10180      IVPASS = IVPASS + 1                                          03380369
           WRITE (NUVI, 80002) IVTNUM                                   03390369
           GO TO 0181                                                   03400369
20180      IVFAIL = IVFAIL + 1                                          03410369
           RVCORR = 0.14436685815988E+01                                03420369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03430369
 0181      CONTINUE                                                     03440369
CT019*  TEST 19                                                         03450369
           IVTNUM = 19                                                  03460369
        BVS = 128.                                                      03470369
        AVS = EXP(48. / BVS)                                            03480369
           IF (AVS - 0.14549E+01) 20190, 10190, 40190                   03490369
40190      IF (AVS - 0.14551E+01) 10190, 10190, 20190                   03500369
10190      IVPASS = IVPASS + 1                                          03510369
           WRITE (NUVI, 80002) IVTNUM                                   03520369
           GO TO 0191                                                   03530369
20190      IVFAIL = IVFAIL + 1                                          03540369
           RVCORR = 0.14549914146182E+01                                03550369
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03560369
 0191      CONTINUE                                                     03570369
CBB** ********************** BBCSUM0  **********************************03580369
C**** WRITE OUT TEST SUMMARY                                            03590369
C****                                                                   03600369
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03610369
      WRITE (I02, 90004)                                                03620369
      WRITE (I02, 90014)                                                03630369
      WRITE (I02, 90004)                                                03640369
      WRITE (I02, 90020) IVPASS                                         03650369
      WRITE (I02, 90022) IVFAIL                                         03660369
      WRITE (I02, 90024) IVDELE                                         03670369
      WRITE (I02, 90026) IVINSP                                         03680369
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03690369
CBE** ********************** BBCSUM0  **********************************03700369
CBB** ********************** BBCFOOT0 **********************************03710369
C**** WRITE OUT REPORT FOOTINGS                                         03720369
C****                                                                   03730369
      WRITE (I02,90016) ZPROG, ZPROG                                    03740369
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03750369
      WRITE (I02,90019)                                                 03760369
CBE** ********************** BBCFOOT0 **********************************03770369
CBB** ********************** BBCFMT0A **********************************03780369
C**** FORMATS FOR TEST DETAIL LINES                                     03790369
C****                                                                   03800369
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03810369
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03820369
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03830369
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03840369
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03850369
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03860369
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03870369
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03880369
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03890369
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03900369
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03910369
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03920369
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03930369
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03940369
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03950369
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03960369
80050 FORMAT (1H ,48X,A31)                                              03970369
CBE** ********************** BBCFMT0A **********************************03980369
CBB** ********************** BBCFMT0B **********************************03990369
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04000369
C****                                                                   04010369
90002 FORMAT (1H1)                                                      04020369
90004 FORMAT (1H )                                                      04030369
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04040369
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04050369
90008 FORMAT (1H ,21X,A13,A17)                                          04060369
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04070369
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04080369
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04090369
     1       7X,7HREMARKS,24X)                                          04100369
90014 FORMAT (1H ,46H----------------------------------------------,    04110369
     1        33H---------------------------------)                     04120369
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04130369
C****                                                                   04140369
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04150369
C****                                                                   04160369
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04170369
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04180369
     1        A13)                                                      04190369
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04200369
C****                                                                   04210369
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04220369
C****                                                                   04230369
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04240369
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04250369
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04260369
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04270369
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04280369
CBE** ********************** BBCFMT0B **********************************04290369
C*****                                                                  04300369
C*****    END OF TEST SEGMENT 178                                       04310369
      STOP                                                              04320369
      END                                                               04330369
