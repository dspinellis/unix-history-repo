C***********************************************************************00010357
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020357
C*****   FM357               XAMOD - (159)                              00030357
C*****                                                                  00040357
C***********************************************************************00050357
C*****  GENERAL PURPOSE                                         ANS REF 00060357
C*****    TEST INTRINSIC FUNCTIONS AMOD AND MOD - REMAINDERING,  15.3   00070357
C*****    WHICH IS DEFINED AS A1-(A1/A2)A2 WHERE (X) IS AN     (TABLE 5)00080357
C*****    INTEGER WHOSE MAGNITUDE IS LE ABS(X) AND WHOSE SIGN           00090357
C*****    IS THE SAME AS X.                                             00100357
C*****                                                                  00110357
CBB** ********************** BBCCOMNT **********************************00120357
C****                                                                   00130357
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140357
C****                          VERSION 2.0                              00150357
C****                                                                   00160357
C****                                                                   00170357
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180357
C****                   GENERAL SERVICES ADMINISTRATION                 00190357
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200357
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210357
C****                      FALLS CHURCH, VA. 22041                      00220357
C****                                                                   00230357
C****                          (703) 756-6153                           00240357
C****                                                                   00250357
CBE** ********************** BBCCOMNT **********************************00260357
CBB** ********************** BBCINITA **********************************00270357
C**** SPECIFICATION STATEMENTS                                          00280357
C****                                                                   00290357
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00300357
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00310357
CBE** ********************** BBCINITA **********************************00320357
CBB** ********************** BBCINITB **********************************00330357
C**** INITIALIZE SECTION                                                00340357
      DATA  ZVERS,                  ZVERSD,             ZDATE           00350357
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00360357
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00370357
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00380357
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00390357
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00400357
      DATA   REMRKS /'                               '/                 00410357
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00420357
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00430357
C****                                                                   00440357
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00450357
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00460357
CZ03  ZPROG  = 'PROGRAM NAME'                                           00470357
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00540357
      IVPASS = 0                                                        00550357
      IVFAIL = 0                                                        00560357
      IVDELE = 0                                                        00570357
      IVINSP = 0                                                        00580357
      IVTOTL = 0                                                        00590357
      IVTOTN = 0                                                        00600357
      ICZERO = 0                                                        00610357
C                                                                       00620357
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00630357
      I01 = 05                                                          00640357
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00650357
      I02 = 06                                                          00660357
C                                                                       00670357
      I01 = 5                                                           00680357
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690357
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00700357
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00710357
C                                                                       00720357
      I02 = 6                                                           00730357
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00740357
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00750357
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00760357
C                                                                       00770357
CBE** ********************** BBCINITB **********************************00780357
      NUVI = I02                                                        00790357
      IVTOTL = 22                                                       00800357
      ZPROG = 'FM357'                                                   00810357
CBB** ********************** BBCHED0A **********************************00820357
C****                                                                   00830357
C**** WRITE REPORT TITLE                                                00840357
C****                                                                   00850357
      WRITE (I02, 90002)                                                00860357
      WRITE (I02, 90006)                                                00870357
      WRITE (I02, 90007)                                                00880357
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00890357
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00900357
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00910357
CBE** ********************** BBCHED0A **********************************00920357
C*****                                                                  00930357
C*****    HEADER FOR SEGMENT 159 WRITTEN                                00940357
        WRITE (NUVI,15901)                                              00950357
15901   FORMAT (1H , //, 2X,35HXAMOD - (159) INTRINSIC FUNCTION-- //16X,00960357
     1          24HAMOD, MOD (REMAINDERING)//19H SUBSET REF. - 15.3)    00970357
CBB** ********************** BBCHED0B **********************************00980357
C**** WRITE DETAIL REPORT HEADERS                                       00990357
C****                                                                   01000357
      WRITE (I02,90004)                                                 01010357
      WRITE (I02,90004)                                                 01020357
      WRITE (I02,90013)                                                 01030357
      WRITE (I02,90014)                                                 01040357
      WRITE (I02,90015) IVTOTL                                          01050357
CBE** ********************** BBCHED0B **********************************01060357
C*****                                                                  01070357
C*****    TEST OF AMOD                                                  01080357
C*****                                                                  01090357
        WRITE(NUVI, 15902)                                              01100357
15902   FORMAT (/ 8X, 12HTEST OF AMOD)                                  01110357
C*****                                                                  01120357
CT001*  TEST 1                      FIRST VALUE ZERO, SECOND NON-ZERO   01130357
           IVTNUM = 1                                                   01140357
        REBVS = 0.0                                                     01150357
        REDVS = 4.5                                                     01160357
        REAVS = AMOD(REBVS, REDVS)                                      01170357
           IF (REAVS + 0.00005) 20010, 10010, 40010                     01180357
40010      IF (REAVS - 0.00005) 10010, 10010, 20010                     01190357
10010      IVPASS = IVPASS + 1                                          01200357
           WRITE (NUVI, 80002) IVTNUM                                   01210357
           GO TO 0011                                                   01220357
20010      IVFAIL = IVFAIL + 1                                          01230357
           RVCORR = 0.0                                                 01240357
           WRITE (NUVI,80012) IVTNUM, REAVS, RVCORR                     01250357
 0011      CONTINUE                                                     01260357
CT002*  TEST 2                                      BOTH VALUES EQUAL   01270357
           IVTNUM = 2                                                   01280357
        REBVS = 3.5                                                     01290357
        REDVS = 3.5                                                     01300357
        REAVS = AMOD(REBVS, REDVS)                                      01310357
           IF (REAVS + 0.00005) 20020, 10020, 40020                     01320357
40020      IF (REAVS - 0.00005) 10020, 10020, 20020                     01330357
10020      IVPASS = IVPASS + 1                                          01340357
           WRITE (NUVI, 80002) IVTNUM                                   01350357
           GO TO 0021                                                   01360357
20020      IVFAIL = IVFAIL + 1                                          01370357
           RVCORR = 0.0                                                 01380357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     01390357
 0021      CONTINUE                                                     01400357
CT003*  TEST 3         FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   01410357
           IVTNUM = 3                                                   01420357
        REBVS = -10.9                                                   01430357
        REDVS = -3.3                                                    01440357
        REAVS = AMOD(REBVS, REDVS)                                      01450357
           IF (REAVS + 1.0001) 20030, 10030, 40030                      01460357
40030      IF (REAVS + 0.99995) 10030, 10030, 20030                     01470357
10030      IVPASS = IVPASS + 1                                          01480357
           WRITE (NUVI, 80002) IVTNUM                                   01490357
           GO TO 0031                                                   01500357
20030      IVFAIL = IVFAIL + 1                                          01510357
           RVCORR = -1.0                                                01520357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     01530357
 0031      CONTINUE                                                     01540357
CT004*  TEST 4             FIRST MAGNITUDE LARGER, MULTIPLE OF SECOND   01550357
           IVTNUM = 4                                                   01560357
        REDVS = 1.5                                                     01570357
        REBVS = 1.5 + REDVS + 1.5                                       01580357
        REAVS = AMOD(REBVS, REDVS)                                      01590357
           IF (REAVS + 0.00005) 20040, 10040, 40040                     01600357
40040      IF (REAVS - 0.00005) 10040, 10040, 20040                     01610357
10040      IVPASS = IVPASS + 1                                          01620357
           WRITE (NUVI, 80002) IVTNUM                                   01630357
           GO TO 0041                                                   01640357
20040      IVFAIL = IVFAIL + 1                                          01650357
           RVCORR = 0.0                                                 01660357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     01670357
 0041      CONTINUE                                                     01680357
CT005*  TEST 5         FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   01690357
           IVTNUM = 5                                                   01700357
        REBVS = 7.625                                                   01710357
        REDVS = 2.125                                                   01720357
        REAVS = AMOD(REBVS, REDVS)                                      01730357
           IF (REAVS - 1.2499) 20050, 10050, 40050                      01740357
40050      IF (REAVS - 1.2501) 10050, 10050, 20050                      01750357
10050      IVPASS = IVPASS + 1                                          01760357
           WRITE (NUVI, 80002) IVTNUM                                   01770357
           GO TO 0051                                                   01780357
20050      IVFAIL = IVFAIL + 1                                          01790357
           RVCORR = 1.25                                                01800357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     01810357
 0051      CONTINUE                                                     01820357
CT006*  TEST 6                      FIRST VALUE ZERO, SECOND NEGATIVE   01830357
           IVTNUM = 6                                                   01840357
        REBVS = 0.0                                                     01850357
        REDVS = -4.5                                                    01860357
        REAVS = AMOD(REBVS, REDVS)                                      01870357
           IF (REAVS + 0.00005) 20060, 10060, 40060                     01880357
40060      IF (REAVS - 0.00005) 10060, 10060, 20060                     01890357
10060      IVPASS = IVPASS + 1                                          01900357
           WRITE (NUVI, 80002) IVTNUM                                   01910357
           GO TO 0061                                                   01920357
20060      IVFAIL = IVFAIL + 1                                          01930357
           RVCORR = 0.0                                                 01940357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     01950357
 0061      CONTINUE                                                     01960357
CT007*  TEST 7                       BOTH VALUES EQUAL, BOTH NEGATIVE   01970357
           IVTNUM = 7                                                   01980357
        REBVS = -3.5                                                    01990357
        REDVS = -3.5                                                    02000357
        REAVS = AMOD(REBVS, REDVS)                                      02010357
           IF (REAVS + 0.00005) 20070, 10070, 40070                     02020357
40070      IF (REAVS - 0.00005) 10070, 10070, 20070                     02030357
10070      IVPASS = IVPASS + 1                                          02040357
           WRITE (NUVI, 80002) IVTNUM                                   02050357
           GO TO 0071                                                   02060357
20070      IVFAIL = IVFAIL + 1                                          02070357
           RVCORR = 0.0                                                 02080357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     02090357
 0071      CONTINUE                                                     02100357
CT008*  TEST 8        FIRST VALUE NEGATIVE, SECOND POSITIVE, MULTIPLE   02110357
           IVTNUM = 8                                                   02120357
        REBVS = 1.5                                                     02130357
        REDVS = -(1.5 + REDVS + 1.5)                                    02140357
        REAVS = AMOD(-REBVS, -REDVS)                                    02150357
           IF (REAVS + 0.00005) 20080, 10080, 40080                     02160357
40080      IF (REAVS - 0.00005) 10080, 10080, 20080                     02170357
10080      IVPASS = IVPASS + 1                                          02180357
           WRITE (NUVI, 80002) IVTNUM                                   02190357
           GO TO 0081                                                   02200357
20080      IVFAIL = IVFAIL + 1                                          02210357
           RVCORR = 0.0                                                 02220357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     02230357
 0081      CONTINUE                                                     02240357
CT009*  TEST 9         FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   02250357
           IVTNUM = 9                                                   02260357
        REBVS = 10.5                                                    02270357
        REDVS = -3.3                                                    02280357
        REAVS = AMOD(REBVS, REDVS)                                      02290357
           IF (REAVS - 0.59997) 20090, 10090, 40090                     02300357
40090      IF (REAVS - 0.60003) 10090, 10090, 20090                     02310357
10090      IVPASS = IVPASS + 1                                          02320357
           WRITE (NUVI, 80002) IVTNUM                                   02330357
           GO TO 0091                                                   02340357
20090      IVFAIL = IVFAIL + 1                                          02350357
           RVCORR = 0.6                                                 02360357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     02370357
 0091      CONTINUE                                                     02380357
CT010*  TEST 10       PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT   02390357
           IVTNUM = 10                                                  02400357
        RECVS = 7.625                                                   02410357
        REDVS = 2.125                                                   02420357
        REFVS = 2.0                                                     02430357
        REAVS = AMOD(RECVS - REFVS, REDVS + REFVS)                      02440357
           IF (REAVS - 1.4999) 20100, 10100, 40100                      02450357
40100      IF (REAVS - 1.5001) 10100, 10100, 20100                      02460357
10100      IVPASS = IVPASS + 1                                          02470357
           WRITE (NUVI, 80002) IVTNUM                                   02480357
           GO TO 0101                                                   02490357
20100      IVFAIL = IVFAIL + 1                                          02500357
           RVCORR = 1.5                                                 02510357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     02520357
 0101      CONTINUE                                                     02530357
CT011*  TEST 11                TEST LOW AND HIGH MAGNITUDE ARGUMENTS    02540357
           IVTNUM = 11                                                  02550357
        RECVS = 1.0E-16                                                 02560357
        REDVS = 1.0E+16                                                 02570357
        REAVS = AMOD(RECVS, REDVS)                                      02580357
           IF (REAVS - 0.99995E-16) 20110, 10110, 40110                 02590357
40110      IF (REAVS - 1.0001E-16) 10110, 10110, 20110                  02600357
10110      IVPASS = IVPASS + 1                                          02610357
           WRITE (NUVI, 80002) IVTNUM                                   02620357
           GO TO 0111                                                   02630357
20110      IVFAIL = IVFAIL + 1                                          02640357
           RVCORR = 1.0E-16                                             02650357
           WRITE(NUVI, 80012) IVTNUM, REAVS, RVCORR                     02660357
 0111      CONTINUE                                                     02670357
C*****                                                                  02680357
C*****    TEST OF MOD                                                   02690357
C*****                                                                  02700357
        WRITE(NUVI, 15904)                                              02710357
15904   FORMAT (/ 8X, 11HTEST OF MOD)                                   02720357
C*****                                                                  02730357
CT012*  TEST 12                     FIRST VALUE ZERO, SECOND NON-ZERO   02740357
           IVTNUM = 12                                                  02750357
        IEBVI = 0                                                       02760357
        IEDVI = 4                                                       02770357
        IEAVI = MOD(IEBVI, IEDVI)                                       02780357
           IF (IEAVI - 0) 20120, 10120, 20120                           02790357
10120      IVPASS = IVPASS + 1                                          02800357
           WRITE (NUVI, 80002) IVTNUM                                   02810357
           GO TO 0121                                                   02820357
20120      IVFAIL = IVFAIL + 1                                          02830357
           IVCORR = 0                                                   02840357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    02850357
 0121      CONTINUE                                                     02860357
CT013*  TEST 13                                     BOTH VALUES EQUAL   02870357
           IVTNUM = 13                                                  02880357
        IEBVI = 3                                                       02890357
        IEDVI = 3                                                       02900357
        IEAVI = MOD(IEBVI, IEDVI)                                       02910357
           IF (IEAVI - 0) 20130, 10130, 20130                           02920357
10130      IVPASS = IVPASS + 1                                          02930357
           WRITE (NUVI, 80002) IVTNUM                                   02940357
           GO TO 0131                                                   02950357
20130      IVFAIL = IVFAIL + 1                                          02960357
           IVCORR = 0                                                   02970357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    02980357
 0131      CONTINUE                                                     02990357
CT014*  TEST 14        FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   03000357
           IVTNUM = 14                                                  03010357
        IEBVI = -10                                                     03020357
        IEDVI = -3                                                      03030357
        IEAVI = MOD(IEBVI, IEDVI)                                       03040357
           IF (IEAVI + 1) 20140, 10140, 20140                           03050357
10140      IVPASS = IVPASS + 1                                          03060357
           WRITE (NUVI, 80002) IVTNUM                                   03070357
           GO TO 0141                                                   03080357
20140      IVFAIL = IVFAIL + 1                                          03090357
           IVCORR = -1                                                  03100357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    03110357
 0141      CONTINUE                                                     03120357
CT015*  TEST 15            FIRST MAGNITUDE LARGER, MULTIPLE OF SECOND   03130357
           IVTNUM = 15                                                  03140357
        IEBVI = 9                                                       03150357
        IEDVI = 3                                                       03160357
        IEAVI = MOD(IEBVI, IEDVI)                                       03170357
           IF (IEAVI - 0) 20150, 10150, 20150                           03180357
10150      IVPASS = IVPASS + 1                                          03190357
           WRITE (NUVI, 80002) IVTNUM                                   03200357
           GO TO 0151                                                   03210357
20150      IVFAIL = IVFAIL + 1                                          03220357
           IVCORR = 0                                                   03230357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    03240357
 0151      CONTINUE                                                     03250357
CT016*  TEST 16        FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   03260357
           IVTNUM = 16                                                  03270357
        IEBVI = 7                                                       03280357
        IEDVI = 2                                                       03290357
        IEAVI = MOD(IEBVI, IEDVI)                                       03300357
           IF (IEAVI - 1) 20160, 10160, 20160                           03310357
10160      IVPASS = IVPASS + 1                                          03320357
           WRITE (NUVI, 80002) IVTNUM                                   03330357
           GO TO 0161                                                   03340357
20160      IVFAIL = IVFAIL + 1                                          03350357
           IVCORR = 1                                                   03360357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    03370357
 0161      CONTINUE                                                     03380357
CT017*  TEST 17                     FIRST VALUE ZERO, SECOND NEGATIVE   03390357
           IVTNUM = 17                                                  03400357
        IEBVI = 0                                                       03410357
        IEDVI = -4                                                      03420357
        IEAVI = MOD(IEBVI, IEDVI)                                       03430357
           IF (IEAVI - 0) 20170, 10170, 20170                           03440357
10170      IVPASS = IVPASS + 1                                          03450357
           WRITE (NUVI, 80002) IVTNUM                                   03460357
           GO TO 0171                                                   03470357
20170      IVFAIL = IVFAIL + 1                                          03480357
           IVCORR = 0                                                   03490357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    03500357
 0171      CONTINUE                                                     03510357
CT018*  TEST 18                      BOTH VALUES EQUAL, BOTH NEGATIVE   03520357
           IVTNUM = 18                                                  03530357
        IEBVI = -3                                                      03540357
        IEDVI = -3                                                      03550357
        IEAVI = MOD(IEBVI, IEDVI)                                       03560357
           IF (IEAVI - 0) 20180, 10180, 20180                           03570357
10180      IVPASS = IVPASS + 1                                          03580357
           WRITE (NUVI, 80002) IVTNUM                                   03590357
           GO TO 0181                                                   03600357
20180      IVFAIL = IVFAIL + 1                                          03610357
           IVCORR = 0                                                   03620357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    03630357
 0181      CONTINUE                                                     03640357
CT019*  TEST 19       FIRST MAGNITUDE LARGER, MULTIPLE, BOTH NEGATIVE   03650357
           IVTNUM = 19                                                  03660357
        IEBVI = -9                                                      03670357
        IEDVI = -3                                                      03680357
        IEAVI = MOD(IEBVI, IEDVI)                                       03690357
           IF (IEAVI - 0) 20190, 10190, 20190                           03700357
10190      IVPASS = IVPASS + 1                                          03710357
           WRITE (NUVI, 80002) IVTNUM                                   03720357
           GO TO 0191                                                   03730357
20190      IVFAIL = IVFAIL + 1                                          03740357
           IVCORR = 0                                                   03750357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    03760357
 0191      CONTINUE                                                     03770357
CT020*  TEST 20      FIRST NUMBER NEGATIVE, SECOND POSITIVE, MULTIPLE   03780357
           IVTNUM = 20                                                  03790357
        IEBVI = -9                                                      03800357
        IEDVI = 3                                                       03810357
        IEAVI = MOD(IEBVI, IEDVI)                                       03820357
           IF (IEAVI - 0) 20200, 10200, 20200                           03830357
10200      IVPASS = IVPASS + 1                                          03840357
           WRITE (NUVI, 80002) IVTNUM                                   03850357
           GO TO 0201                                                   03860357
20200      IVFAIL = IVFAIL + 1                                          03870357
           IVCORR = 0                                                   03880357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    03890357
 0201      CONTINUE                                                     03900357
CT021*  TEST 21               FIRST VALUE ZERO PRECEDED BY MINUS SIGN   03910357
           IVTNUM = 21                                                  03920357
        IEBVI = 0                                                       03930357
        IEDVI = 4                                                       03940357
        IEAVI = MOD(-IEBVI, IEDVI)                                      03950357
           IF (IEAVI - 0) 20210, 10210, 20210                           03960357
10210      IVPASS = IVPASS + 1                                          03970357
           WRITE (NUVI, 80002) IVTNUM                                   03980357
           GO TO 0211                                                   03990357
20210      IVFAIL = IVFAIL + 1                                          04000357
           IVCORR = 0                                                   04010357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    04020357
 0211      CONTINUE                                                     04030357
CT022*  TEST 22       PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT   04040357
           IVTNUM = 22                                                  04050357
        IEDVI = 10                                                      04060357
        IEEVI = 3                                                       04070357
        IEFVI = 2                                                       04080357
        IEAVI = MOD(IEDVI - IEFVI, IEEVI + IEFVI)                       04090357
           IF (IEAVI - 3) 20220, 10220, 20220                           04100357
10220      IVPASS = IVPASS + 1                                          04110357
           WRITE (NUVI, 80002) IVTNUM                                   04120357
           GO TO 0221                                                   04130357
20220      IVFAIL = IVFAIL + 1                                          04140357
           IVCORR = 3                                                   04150357
           WRITE (NUVI, 80010) IVTNUM, IEAVI, IVCORR                    04160357
 0221      CONTINUE                                                     04170357
C*****                                                                  04180357
CBB** ********************** BBCSUM0  **********************************04190357
C**** WRITE OUT TEST SUMMARY                                            04200357
C****                                                                   04210357
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04220357
      WRITE (I02, 90004)                                                04230357
      WRITE (I02, 90014)                                                04240357
      WRITE (I02, 90004)                                                04250357
      WRITE (I02, 90020) IVPASS                                         04260357
      WRITE (I02, 90022) IVFAIL                                         04270357
      WRITE (I02, 90024) IVDELE                                         04280357
      WRITE (I02, 90026) IVINSP                                         04290357
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04300357
CBE** ********************** BBCSUM0  **********************************04310357
CBB** ********************** BBCFOOT0 **********************************04320357
C**** WRITE OUT REPORT FOOTINGS                                         04330357
C****                                                                   04340357
      WRITE (I02,90016) ZPROG, ZPROG                                    04350357
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04360357
      WRITE (I02,90019)                                                 04370357
CBE** ********************** BBCFOOT0 **********************************04380357
CBB** ********************** BBCFMT0A **********************************04390357
C**** FORMATS FOR TEST DETAIL LINES                                     04400357
C****                                                                   04410357
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04420357
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04430357
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04440357
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04450357
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04460357
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04470357
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04480357
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04490357
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04500357
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04510357
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04520357
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04530357
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04540357
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04550357
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04560357
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04570357
80050 FORMAT (1H ,48X,A31)                                              04580357
CBE** ********************** BBCFMT0A **********************************04590357
CBB** ********************** BBCFMT0B **********************************04600357
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04610357
C****                                                                   04620357
90002 FORMAT (1H1)                                                      04630357
90004 FORMAT (1H )                                                      04640357
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04650357
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04660357
90008 FORMAT (1H ,21X,A13,A17)                                          04670357
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04680357
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04690357
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04700357
     1       7X,7HREMARKS,24X)                                          04710357
90014 FORMAT (1H ,46H----------------------------------------------,    04720357
     1        33H---------------------------------)                     04730357
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04740357
C****                                                                   04750357
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04760357
C****                                                                   04770357
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04780357
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04790357
     1        A13)                                                      04800357
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04810357
C****                                                                   04820357
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04830357
C****                                                                   04840357
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04850357
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04860357
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04870357
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04880357
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04890357
CBE** ********************** BBCFMT0B **********************************04900357
C*****                                                                  04910357
C*****    END OF TEST SEGMENT 159                                       04920357
      STOP                                                              04930357
      END                                                               04940357
                                                                        04950357
