C***********************************************************************00010814
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020814
C*****   FM814                                                          00030814
C*****                       YDEXP - (179)                              00040814
C*****                                                                  00050814
C***********************************************************************00060814
C*****  GENERAL PURPOSE                                         ANS REF 00070814
C*****    TEST INTRINSIC FUNCTION DEXP                           15.3   00080814
C*****                                                          TABLE 5 00090814
C*****                                                                  00100814
CBB** ********************** BBCCOMNT **********************************00110814
C****                                                                   00120814
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130814
C****                          VERSION 2.0                              00140814
C****                                                                   00150814
C****                                                                   00160814
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170814
C****                   GENERAL SERVICES ADMINISTRATION                 00180814
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190814
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200814
C****                      FALLS CHURCH, VA. 22041                      00210814
C****                                                                   00220814
C****                          (703) 756-6153                           00230814
C****                                                                   00240814
CBE** ********************** BBCCOMNT **********************************00250814
C*****                                                                  00260814
C*****  S P E C I F I C A T I O N S  SEGMENT 179                        00270814
        DOUBLE PRECISION AVD, BVD, DVCORR                               00280814
C*****                                                                  00290814
CBB** ********************** BBCINITA **********************************00300814
C**** SPECIFICATION STATEMENTS                                          00310814
C****                                                                   00320814
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330814
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340814
CBE** ********************** BBCINITA **********************************00350814
CBB** ********************** BBCINITB **********************************00360814
C**** INITIALIZE SECTION                                                00370814
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380814
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390814
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400814
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410814
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420814
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430814
      DATA   REMRKS /'                               '/                 00440814
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450814
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460814
C****                                                                   00470814
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480814
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490814
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500814
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570814
      IVPASS = 0                                                        00580814
      IVFAIL = 0                                                        00590814
      IVDELE = 0                                                        00600814
      IVINSP = 0                                                        00610814
      IVTOTL = 0                                                        00620814
      IVTOTN = 0                                                        00630814
      ICZERO = 0                                                        00640814
C                                                                       00650814
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660814
      I01 = 05                                                          00670814
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680814
      I02 = 06                                                          00690814
C                                                                       00700814
      I01 = 5                                                           00710814
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720814
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730814
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740814
C                                                                       00750814
      I02 = 6                                                           00760814
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770814
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780814
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790814
C                                                                       00800814
CBE** ********************** BBCINITB **********************************00810814
      NUVI = I02                                                        00820814
      IVTOTL = 19                                                       00830814
      ZPROG = 'FM814'                                                   00840814
CBB** ********************** BBCHED0A **********************************00850814
C****                                                                   00860814
C**** WRITE REPORT TITLE                                                00870814
C****                                                                   00880814
      WRITE (I02, 90002)                                                00890814
      WRITE (I02, 90006)                                                00900814
      WRITE (I02, 90007)                                                00910814
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920814
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930814
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940814
CBE** ********************** BBCHED0A **********************************00950814
C*****                                                                  00960814
C*****    HEADER FOR SEGMENT 179                                        00970814
        WRITE(NUVI,17900)                                               00980814
17900   FORMAT(1H , / 35H  YDEXP - (179) INTRINSIC FUNCTIONS//          00990814
     1         37H  DEXP (DOUBLE PRECISION EXPONENTIAL)//               01000814
     2         17H  ANS REF. - 15.3)                                    01010814
CBB** ********************** BBCHED0B **********************************01020814
C**** WRITE DETAIL REPORT HEADERS                                       01030814
C****                                                                   01040814
      WRITE (I02,90004)                                                 01050814
      WRITE (I02,90004)                                                 01060814
      WRITE (I02,90013)                                                 01070814
      WRITE (I02,90014)                                                 01080814
      WRITE (I02,90015) IVTOTL                                          01090814
CBE** ********************** BBCHED0B **********************************01100814
C*****                                                                  01110814
CT001*  TEST 1                                 ZERO, SINCE EXP(0) = 1   01120814
           IVTNUM = 1                                                   01130814
        BVD = 0.0D0                                                     01140814
        AVD = DEXP(BVD)                                                 01150814
           IF (AVD - 0.9999999995D+00) 20010, 10010, 40010              01160814
40010      IF (AVD - 0.1000000001D+01) 10010, 10010, 20010              01170814
10010      IVPASS = IVPASS + 1                                          01180814
           WRITE (NUVI, 80002) IVTNUM                                   01190814
           GO TO 0011                                                   01200814
20010      IVFAIL = IVFAIL + 1                                          01210814
           DVCORR = 0.10000000000000000000D+01                          01220814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01230814
 0011      CONTINUE                                                     01240814
CT002*  TEST 2                                  ONE, SINCE EXP(1) = E   01250814
           IVTNUM = 2                                                   01260814
        AVD = DEXP(1.0D0)                                               01270814
           IF (AVD - 0.2718281827D+01) 20020, 10020, 40020              01280814
40020      IF (AVD - 0.2718281830D+01) 10020, 10020, 20020              01290814
10020      IVPASS = IVPASS + 1                                          01300814
           WRITE (NUVI, 80002) IVTNUM                                   01310814
           GO TO 0021                                                   01320814
20020      IVFAIL = IVFAIL + 1                                          01330814
           DVCORR = 0.27182818284590452354D+01                          01340814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01350814
 0021      CONTINUE                                                     01360814
CT003*  TEST 3                                                          01370814
           IVTNUM = 3                                                   01380814
        AVD = DEXP(2.0D0)                                               01390814
           IF (AVD - 0.7389056095D+01) 20030, 10030, 40030              01400814
40030      IF (AVD - 0.7389056103D+01) 10030, 10030, 20030              01410814
10030      IVPASS = IVPASS + 1                                          01420814
           WRITE (NUVI, 80002) IVTNUM                                   01430814
           GO TO 0031                                                   01440814
20030      IVFAIL = IVFAIL + 1                                          01450814
           DVCORR = 0.73890560989306502272D+01                          01460814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01470814
 0031      CONTINUE                                                     01480814
CT004*  TEST 4                                                          01490814
           IVTNUM = 4                                                   01500814
        AVD = DEXP(5.125D0)                                             01510814
           IF (AVD - 0.1681741415D+03) 20040, 10040, 40040              01520814
40040      IF (AVD - 0.1681741418D+03) 10040, 10040, 20040              01530814
10040      IVPASS = IVPASS + 1                                          01540814
           WRITE (NUVI, 80002) IVTNUM                                   01550814
           GO TO 0041                                                   01560814
20040      IVFAIL = IVFAIL + 1                                          01570814
           DVCORR = 0.16817414165184545127D+03                          01580814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01590814
 0041      CONTINUE                                                     01600814
CT005*  TEST 5                                                          01610814
           IVTNUM = 5                                                   01620814
        AVD = DEXP(15.0D0)                                              01630814
           IF (AVD - 0.3269017370D+07) 20050, 10050, 40050              01640814
40050      IF (AVD - 0.3269017374D+07) 10050, 10050, 20050              01650814
10050      IVPASS = IVPASS + 1                                          01660814
           WRITE (NUVI, 80002) IVTNUM                                   01670814
           GO TO 0051                                                   01680814
20050      IVFAIL = IVFAIL + 1                                          01690814
           DVCORR = 0.32690173724721106393D+07                          01700814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01710814
 0051      CONTINUE                                                     01720814
CT006*  TEST 6                                                          01730814
           IVTNUM = 6                                                   01740814
        BVD = 20.5D0                                                    01750814
        AVD = DEXP(BVD)                                                 01760814
           IF (AVD - 0.7999021770D+09) 20060, 10060, 40060              01770814
40060      IF (AVD - 0.7999021779D+09) 10060, 10060, 20060              01780814
10060      IVPASS = IVPASS + 1                                          01790814
           WRITE (NUVI, 80002) IVTNUM                                   01800814
           GO TO 0061                                                   01810814
20060      IVFAIL = IVFAIL + 1                                          01820814
           DVCORR = 0.79990217747550540670D+09                          01830814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01840814
 0061      CONTINUE                                                     01850814
CT007*  TEST 7                                                          01860814
           IVTNUM = 7                                                   01870814
        BVD = 4.5D0                                                     01880814
        AVD = DEXP(BVD - 7.5D0)                                         01890814
           IF (AVD - 0.4978706834D-01) 20070, 10070, 40070              01900814
40070      IF (AVD - 0.4978706840D-01) 10070, 10070, 20070              01910814
10070      IVPASS = IVPASS + 1                                          01920814
           WRITE (NUVI, 80002) IVTNUM                                   01930814
           GO TO 0071                                                   01940814
20070      IVFAIL = IVFAIL + 1                                          01950814
           DVCORR = 0.49787068367863942979D-01                          01960814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01970814
 0071      CONTINUE                                                     01980814
CT008*  TEST 8                                                          01990814
           IVTNUM = 8                                                   02000814
        BVD = 0.25D0                                                    02010814
        AVD = DEXP(BVD - 5.0D0)                                         02020814
           IF (AVD - 0.8651695198D-02) 20080, 10080, 40080              02030814
40080      IF (AVD - 0.8651695208D-02) 10080, 10080, 20080              02040814
10080      IVPASS = IVPASS + 1                                          02050814
           WRITE (NUVI, 80002) IVTNUM                                   02060814
           GO TO 0081                                                   02070814
20080      IVFAIL = IVFAIL + 1                                          02080814
           DVCORR = 0.86516952031206341771D-02                          02090814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02100814
 0081      CONTINUE                                                     02110814
CT009*  TEST 9                                                          02120814
           IVTNUM = 9                                                   02130814
        AVD = DEXP(0.5D0 * (-20.0D0))                                   02140814
           IF (AVD - 0.4539992974D-04) 20090, 10090, 40090              02150814
40090      IF (AVD - 0.4539992979D-04) 10090, 10090, 20090              02160814
10090      IVPASS = IVPASS + 1                                          02170814
           WRITE (NUVI, 80002) IVTNUM                                   02180814
           GO TO 0091                                                   02190814
20090      IVFAIL = IVFAIL + 1                                          02200814
           DVCORR = 0.45399929762484851536D-04                          02210814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02220814
 0091      CONTINUE                                                     02230814
CT010*  TEST 10                                                         02240814
           IVTNUM = 10                                                  02250814
        BVD = 30.5D0                                                    02260814
        AVD = DEXP(BVD / (-2.0D0))                                      02270814
           IF (AVD - 0.2382369666D-06) 20100, 10100, 40100              02280814
40100      IF (AVD - 0.2382369669D-06) 10100, 10100, 20100              02290814
10100      IVPASS = IVPASS + 1                                          02300814
           WRITE (NUVI, 80002) IVTNUM                                   02310814
           GO TO 0101                                                   02320814
20100      IVFAIL = IVFAIL + 1                                          02330814
           DVCORR = 0.23823696675018179180D-06                          02340814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02350814
 0101      CONTINUE                                                     02360814
CT011*  TEST 11                                   VALUES CLOSE TO 1.0   02370814
           IVTNUM = 11                                                  02380814
        AVD = DEXP(0.9921875D0)                                         02390814
           IF (AVD - 0.2697127990D+01) 20110, 10110, 40110              02400814
40110      IF (AVD - 0.2697127993D+01) 10110, 10110, 20110              02410814
10110      IVPASS = IVPASS + 1                                          02420814
           WRITE (NUVI, 80002) IVTNUM                                   02430814
           GO TO 0111                                                   02440814
20110      IVFAIL = IVFAIL + 1                                          02450814
           DVCORR = 0.26971279914439187908D+01                          02460814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02470814
 0111      CONTINUE                                                     02480814
CT012*  TEST 12                                                         02490814
           IVTNUM = 12                                                  02500814
        BVD = 0.9990234375D0                                            02510814
        AVD = DEXP(BVD)                                                 02520814
           IF (AVD - 0.2715628550D+01) 20120, 10120, 40120              02530814
40120      IF (AVD - 0.2715628554D+01) 10120, 10120, 20120              02540814
10120      IVPASS = IVPASS + 1                                          02550814
           WRITE (NUVI, 80002) IVTNUM                                   02560814
           GO TO 0121                                                   02570814
20120      IVFAIL = IVFAIL + 1                                          02580814
           DVCORR = 0.27156285521168930956D+01                          02590814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02600814
 0121      CONTINUE                                                     02610814
CT013*  TEST 13                                                         02620814
           IVTNUM = 13                                                  02630814
        AVD = DEXP(1.00390625D0)                                        02640814
           IF (AVD - 0.2728920881D+01) 20130, 10130, 40130              02650814
40130      IF (AVD - 0.2728920884D+01) 10130, 10130, 20130              02660814
10130      IVPASS = IVPASS + 1                                          02670814
           WRITE (NUVI, 80002) IVTNUM                                   02680814
           GO TO 0131                                                   02690814
20130      IVFAIL = IVFAIL + 1                                          02700814
           DVCORR = 0.27289208827260750401D+01                          02710814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02720814
 0131      CONTINUE                                                     02730814
CT014*  TEST 14                                                         02740814
           IVTNUM = 14                                                  02750814
        BVD = 1.001953125D0                                             02760814
        AVD = DEXP(BVD)                                                 02770814
           IF (AVD - 0.2723596159D+01) 20140, 10140, 40140              02780814
40140      IF (AVD - 0.2723596162D+01) 10140, 10140, 20140              02790814
10140      IVPASS = IVPASS + 1                                          02800814
           WRITE (NUVI, 80002) IVTNUM                                   02810814
           GO TO 0141                                                   02820814
20140      IVFAIL = IVFAIL + 1                                          02830814
           DVCORR = 0.27235961607434952125D+01                          02840814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02850814
 0141      CONTINUE                                                     02860814
CT015*  TEST 15                                   VALUES CLOSE TO 1/E   02870814
           IVTNUM = 15                                                  02880814
        BVD = 128.0D0                                                   02890814
        AVD = DEXP(44.0D0 / BVD)                                        02900814
           IF (AVD - 0.1410226034D+01) 20150, 10150, 40150              02910814
40150      IF (AVD - 0.1410226036D+01) 10150, 10150, 20150              02920814
10150      IVPASS = IVPASS + 1                                          02930814
           WRITE (NUVI, 80002) IVTNUM                                   02940814
           GO TO 0151                                                   02950814
20150      IVFAIL = IVFAIL + 1                                          02960814
           DVCORR = 0.14102260349257107057D+01                          02970814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02980814
 0151      CONTINUE                                                     02990814
CT016*  TEST 16                                                         03000814
           IVTNUM = 16                                                  03010814
        BVD = 128.0D0                                                   03020814
        AVD = DEXP(45.0D0 / BVD)                                        03030814
           IF (AVD - 0.1421286574D+01) 20160, 10160, 40160              03040814
40160      IF (AVD - 0.1421286576D+01) 10160, 10160, 20160              03050814
10160      IVPASS = IVPASS + 1                                          03060814
           WRITE (NUVI, 80002) IVTNUM                                   03070814
           GO TO 0161                                                   03080814
20160      IVFAIL = IVFAIL + 1                                          03090814
           DVCORR = 0.14212865748006967556D+01                          03100814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03110814
 0161      CONTINUE                                                     03120814
CT017*  TEST 17                                                         03130814
           IVTNUM = 17                                                  03140814
        BVD = 128.0D0                                                   03150814
        AVD = DEXP(46.0D0 / BVD)                                        03160814
           IF (AVD - 0.1432433862D+01) 20170, 10170, 40170              03170814
40170      IF (AVD - 0.1432433865D+01) 10170, 10170, 20170              03180814
10170      IVPASS = IVPASS + 1                                          03190814
           WRITE (NUVI, 80002) IVTNUM                                   03200814
           GO TO 0171                                                   03210814
20170      IVFAIL = IVFAIL + 1                                          03220814
           DVCORR = 0.14324338635650781150D+01                          03230814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03240814
 0171      CONTINUE                                                     03250814
CT018*  TEST 18                                                         03260814
           IVTNUM = 18                                                  03270814
        BVD = 128.0D0                                                   03280814
        AVD = DEXP(47.0D0 / BVD)                                        03290814
           IF (AVD - 0.1443668580D+01) 20180, 10180, 40180              03300814
40180      IF (AVD - 0.1443668583D+01) 10180, 10180, 20180              03310814
10180      IVPASS = IVPASS + 1                                          03320814
           WRITE (NUVI, 80002) IVTNUM                                   03330814
           GO TO 0181                                                   03340814
20180      IVFAIL = IVFAIL + 1                                          03350814
           DVCORR = 0.14436685815988268628D+01                          03360814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03370814
 0181      CONTINUE                                                     03380814
CT019*  TEST 19                                                         03390814
           IVTNUM = 19                                                  03400814
        BVD = 128.0D0                                                   03410814
        AVD = DEXP(48.0D0 / BVD)                                        03420814
           IF (AVD - 0.1454991413D+01) 20190, 10190, 40190              03430814
40190      IF (AVD - 0.1454991416D+01) 10190, 10190, 20190              03440814
10190      IVPASS = IVPASS + 1                                          03450814
           WRITE (NUVI, 80002) IVTNUM                                   03460814
           GO TO 0191                                                   03470814
20190      IVFAIL = IVFAIL + 1                                          03480814
           DVCORR = 0.14549914146182013361D+01                          03490814
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03500814
 0191      CONTINUE                                                     03510814
C*****                                                                  03520814
CBB** ********************** BBCSUM0  **********************************03530814
C**** WRITE OUT TEST SUMMARY                                            03540814
C****                                                                   03550814
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03560814
      WRITE (I02, 90004)                                                03570814
      WRITE (I02, 90014)                                                03580814
      WRITE (I02, 90004)                                                03590814
      WRITE (I02, 90020) IVPASS                                         03600814
      WRITE (I02, 90022) IVFAIL                                         03610814
      WRITE (I02, 90024) IVDELE                                         03620814
      WRITE (I02, 90026) IVINSP                                         03630814
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03640814
CBE** ********************** BBCSUM0  **********************************03650814
CBB** ********************** BBCFOOT0 **********************************03660814
C**** WRITE OUT REPORT FOOTINGS                                         03670814
C****                                                                   03680814
      WRITE (I02,90016) ZPROG, ZPROG                                    03690814
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03700814
      WRITE (I02,90019)                                                 03710814
CBE** ********************** BBCFOOT0 **********************************03720814
CBB** ********************** BBCFMT0A **********************************03730814
C**** FORMATS FOR TEST DETAIL LINES                                     03740814
C****                                                                   03750814
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03760814
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03770814
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03780814
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03790814
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03800814
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03810814
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03820814
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03830814
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03840814
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03850814
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03860814
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03870814
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03880814
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03890814
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03900814
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03910814
80050 FORMAT (1H ,48X,A31)                                              03920814
CBE** ********************** BBCFMT0A **********************************03930814
CBB** ********************** BBCFMAT1 **********************************03940814
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03950814
C****                                                                   03960814
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03970814
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03980814
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03990814
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04000814
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04010814
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04020814
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04030814
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04040814
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04050814
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04060814
     21H(,F12.5,2H, ,F12.5,1H))                                         04070814
CBE** ********************** BBCFMAT1 **********************************04080814
CBB** ********************** BBCFMT0B **********************************04090814
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04100814
C****                                                                   04110814
90002 FORMAT (1H1)                                                      04120814
90004 FORMAT (1H )                                                      04130814
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04140814
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04150814
90008 FORMAT (1H ,21X,A13,A17)                                          04160814
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04170814
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04180814
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04190814
     1       7X,7HREMARKS,24X)                                          04200814
90014 FORMAT (1H ,46H----------------------------------------------,    04210814
     1        33H---------------------------------)                     04220814
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04230814
C****                                                                   04240814
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04250814
C****                                                                   04260814
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04270814
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04280814
     1        A13)                                                      04290814
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04300814
C****                                                                   04310814
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04320814
C****                                                                   04330814
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04340814
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04350814
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04360814
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04370814
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04380814
CBE** ********************** BBCFMT0B **********************************04390814
C*****                                                                  04400814
C*****    END OF TEST SEGMENT 179                                       04410814
      STOP                                                              04420814
      END                                                               04430814
                                                                        04440814
