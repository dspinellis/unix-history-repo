C***********************************************************************00010362
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020362
C*****   FM362               XMIN - (167)                               00030362
C*****                                                                  00040362
C***********************************************************************00050362
C*****  GENERAL PURPOSE                                       SUBSET REF00060362
C*****    TEST INTRINSIC FUNCTIONS AMIN0,AMIN1,MIN0,MIN1         15.3   00070362
C*****    CHOOSING SMALLEST VALUE.                             (TABLE 5)00080362
C*****                                                                  00090362
CBB** ********************** BBCCOMNT **********************************00100362
C****                                                                   00110362
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120362
C****                          VERSION 2.0                              00130362
C****                                                                   00140362
C****                                                                   00150362
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160362
C****                   GENERAL SERVICES ADMINISTRATION                 00170362
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180362
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190362
C****                      FALLS CHURCH, VA. 22041                      00200362
C****                                                                   00210362
C****                          (703) 756-6153                           00220362
C****                                                                   00230362
CBE** ********************** BBCCOMNT **********************************00240362
CBB** ********************** BBCINITA **********************************00250362
C**** SPECIFICATION STATEMENTS                                          00260362
C****                                                                   00270362
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00280362
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00290362
CBE** ********************** BBCINITA **********************************00300362
CBB** ********************** BBCINITB **********************************00310362
C**** INITIALIZE SECTION                                                00320362
      DATA  ZVERS,                  ZVERSD,             ZDATE           00330362
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00340362
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00350362
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00360362
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00370362
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00380362
      DATA   REMRKS /'                               '/                 00390362
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00400362
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00410362
C****                                                                   00420362
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00430362
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00440362
CZ03  ZPROG  = 'PROGRAM NAME'                                           00450362
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00520362
      IVPASS = 0                                                        00530362
      IVFAIL = 0                                                        00540362
      IVDELE = 0                                                        00550362
      IVINSP = 0                                                        00560362
      IVTOTL = 0                                                        00570362
      IVTOTN = 0                                                        00580362
      ICZERO = 0                                                        00590362
C                                                                       00600362
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610362
      I01 = 05                                                          00620362
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630362
      I02 = 06                                                          00640362
C                                                                       00650362
      I01 = 5                                                           00660362
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670362
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00680362
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00690362
C                                                                       00700362
      I02 = 6                                                           00710362
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00720362
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00730362
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00740362
C                                                                       00750362
CBE** ********************** BBCINITB **********************************00760362
      NUVI = I02                                                        00770362
      IVTOTL = 47                                                       00780362
      ZPROG = 'FM362'                                                   00790362
CBB** ********************** BBCHED0A **********************************00800362
C****                                                                   00810362
C**** WRITE REPORT TITLE                                                00820362
C****                                                                   00830362
      WRITE (I02, 90002)                                                00840362
      WRITE (I02, 90006)                                                00850362
      WRITE (I02, 90007)                                                00860362
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00870362
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00880362
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00890362
CBE** ********************** BBCHED0A **********************************00900362
C*****                                                                  00910362
C*****    HEADER FOR SEGMENT 167                                        00920362
        WRITE (NUVI,16700)                                              00930362
16700   FORMAT (1H , // 2X,36HXMIN - (167) INTRINSIC FUNCTIONS--  //13X,00940362
     1          24HAMIN0, AMIN1, MIN0, MIN1/ 13X,                       00950362
     2          25H(CHOOSING SMALLEST VALUE)//2X,                       00960362
     3          18HSUBSET REF. - 15.3)                                  00970362
CBB** ********************** BBCHED0B **********************************00980362
C**** WRITE DETAIL REPORT HEADERS                                       00990362
C****                                                                   01000362
      WRITE (I02,90004)                                                 01010362
      WRITE (I02,90004)                                                 01020362
      WRITE (I02,90013)                                                 01030362
      WRITE (I02,90014)                                                 01040362
      WRITE (I02,90015) IVTOTL                                          01050362
CBE** ********************** BBCHED0B **********************************01060362
C*****                                                                  01070362
C*****    TEST OF AMIN0                                                 01080362
C*****                                                                  01090362
        WRITE(NUVI, 16702)                                              01100362
16702   FORMAT (/ 8X, 13HTEST OF AMIN0)                                 01110362
CT001*  TEST 1                                       BOTH VALUES ZERO   01120362
           IVTNUM = 1                                                   01130362
        IIBVI = 0                                                       01140362
        IIDVI = 0                                                       01150362
        RIAVS = AMIN0(IIBVI, IIDVI)                                     01160362
           IF (RIAVS + 0.00005) 20010, 10010, 40010                     01170362
40010      IF (RIAVS - 0.00005) 10010, 10010, 20010                     01180362
10010      IVPASS = IVPASS + 1                                          01190362
           WRITE (NUVI, 80002) IVTNUM                                   01200362
           GO TO 0011                                                   01210362
20010      IVFAIL = IVFAIL + 1                                          01220362
           RVCORR = 0.0                                                 01230362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    01240362
 0011      CONTINUE                                                     01250362
CT002*  TEST 2                      FIRST VALUE NON-ZERO, SECOND ZERO   01260362
           IVTNUM = 2                                                   01270362
        IIBVI = 6                                                       01280362
        IIDVI = 0                                                       01290362
        RIAVS = AMIN0(IIBVI, IIDVI)                                     01300362
           IF (RIAVS + 0.00005) 20020, 10020, 40020                     01310362
40020      IF (RIAVS - 0.00005) 10020, 10020, 20020                     01320362
10020      IVPASS = IVPASS + 1                                          01330362
           WRITE (NUVI, 80002) IVTNUM                                   01340362
           GO TO 0021                                                   01350362
20020      IVFAIL = IVFAIL + 1                                          01360362
           RVCORR = 0.0                                                 01370362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    01380362
 0021      CONTINUE                                                     01390362
CT003*  TEST 3                                      BOTH VALUES EQUAL   01400362
           IVTNUM = 3                                                   01410362
        IIBVI = 7                                                       01420362
        IIDVI = 7                                                       01430362
        RIAVS = AMIN0(IIBVI, IIDVI)                                     01440362
           IF (RIAVS - 6.9996) 20030, 10030, 40030                      01450362
40030      IF (RIAVS - 7.0004) 10030, 10030, 20030                      01460362
10030      IVPASS = IVPASS + 1                                          01470362
           WRITE (NUVI, 80002) IVTNUM                                   01480362
           GO TO 0031                                                   01490362
20030      IVFAIL = IVFAIL + 1                                          01500362
           RVCORR = 7.0                                                 01510362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    01520362
 0031      CONTINUE                                                     01530362
CT004*  TEST 4                                       VALUES NOT EQUAL   01540362
           IVTNUM = 4                                                   01550362
        IIBVI = 7                                                       01560362
        IIDVI = 5                                                       01570362
        RIAVS = AMIN0(IIBVI, IIDVI)                                     01580362
           IF (RIAVS - 4.9997) 20040, 10040, 40040                      01590362
40040      IF (RIAVS - 5.0003) 10040, 10040, 20040                      01600362
10040      IVPASS = IVPASS + 1                                          01610362
           WRITE (NUVI, 80002) IVTNUM                                   01620362
           GO TO 0041                                                   01630362
20040      IVFAIL = IVFAIL + 1                                          01640362
           RVCORR = 5.0                                                 01650362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    01660362
 0041      CONTINUE                                                     01670362
CT005*  TEST 5                      FIRST VALUE NEGATIVE, SECOND ZERO   01680362
           IVTNUM = 5                                                   01690362
        IIBVI = -6                                                      01700362
        IIDVI = 0                                                       01710362
        RIAVS = AMIN0(IIBVI, IIDVI)                                     01720362
           IF (RIAVS + 6.0003) 20050, 10050, 40050                      01730362
40050      IF (RIAVS + 5.9997) 10050, 10050, 20050                      01740362
10050      IVPASS = IVPASS + 1                                          01750362
           WRITE (NUVI, 80002) IVTNUM                                   01760362
           GO TO 0051                                                   01770362
20050      IVFAIL = IVFAIL + 1                                          01780362
           RVCORR = -6.0                                                01790362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    01800362
 0051      CONTINUE                                                     01810362
CT006*  TEST 6                       BOTH VALUES EQUAL, BOTH NEGATIVE   01820362
           IVTNUM = 6                                                   01830362
        IIBVI = -7                                                      01840362
        IIDVI = -7                                                      01850362
        RIAVS = AMIN0(IIBVI, IIDVI)                                     01860362
           IF (RIAVS + 7.0004) 20060, 10060, 40060                      01870362
40060      IF (RIAVS + 6.9996) 10060, 10060, 20060                      01880362
10060      IVPASS = IVPASS + 1                                          01890362
           WRITE (NUVI, 80002) IVTNUM                                   01900362
           GO TO 0061                                                   01910362
20060      IVFAIL = IVFAIL + 1                                          01920362
           RVCORR = -7.0                                                01930362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    01940362
 0061      CONTINUE                                                     01950362
CT007*  TEST 7                        VALUES NOT EQUAL, BOTH NEGATIVE   01960362
           IVTNUM = 7                                                   01970362
        IIBVI = -7                                                      01980362
        IIDVI = -5                                                      01990362
        RIAVS = AMIN0(IIBVI, IIDVI)                                     02000362
           IF (RIAVS + 7.0004) 20070, 10070, 40070                      02010362
40070      IF (RIAVS + 6.9996) 10070, 10070, 20070                      02020362
10070      IVPASS = IVPASS + 1                                          02030362
           WRITE (NUVI, 80002) IVTNUM                                   02040362
           GO TO 0071                                                   02050362
20070      IVFAIL = IVFAIL + 1                                          02060362
           RVCORR = -7.0                                                02070362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    02080362
 0071      CONTINUE                                                     02090362
CT008*  TEST 8  FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   02100362
           IVTNUM = 8                                                   02110362
        IIDVI = 6                                                       02120362
        IIEVI = 0                                                       02130362
        RIAVS = AMIN0(IIDVI, -IIEVI)                                    02140362
           IF (RIAVS + 0.00005) 20080, 10080, 40080                     02150362
40080      IF (RIAVS - 0.00005) 10080, 10080, 20080                     02160362
10080      IVPASS = IVPASS + 1                                          02170362
           WRITE (NUVI, 80002) IVTNUM                                   02180362
           GO TO 0081                                                   02190362
20080      IVFAIL = IVFAIL + 1                                          02200362
           RVCORR = 0.0                                                 02210362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    02220362
 0081      CONTINUE                                                     02230362
CT009*  TEST 9                                            3 ARGUMENTS   02240362
           IVTNUM = 9                                                   02250362
        IIBVI = 0                                                       02260362
        IICVI = 9                                                       02270362
        IIDVI = 8                                                       02280362
        RIAVS = AMIN0(IIBVI, IICVI, IIDVI)                              02290362
           IF (RIAVS + 0.00005) 20090, 10090, 40090                     02300362
40090      IF (RIAVS - 0.00005) 10090, 10090, 20090                     02310362
10090      IVPASS = IVPASS + 1                                          02320362
           WRITE (NUVI, 80002) IVTNUM                                   02330362
           GO TO 0091                                                   02340362
20090      IVFAIL = IVFAIL + 1                                          02350362
           RVCORR = 0.0                                                 02360362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    02370362
 0091      CONTINUE                                                     02380362
CT010*  TEST 10                                           4 ARGUMENTS   02390362
           IVTNUM = 10                                                  02400362
        IIBVI = 34                                                      02410362
        IICVI = 8                                                       02420362
        IIDVI = 4                                                       02430362
        RIAVS = AMIN0(IIDVI, IIBVI, IICVI, IIDVI)                       02440362
           IF (RIAVS - 3.9998) 20100, 10100, 40100                      02450362
40100      IF (RIAVS - 4.0002) 10100, 10100, 20100                      02460362
10100      IVPASS = IVPASS + 1                                          02470362
           WRITE (NUVI, 80002) IVTNUM                                   02480362
           GO TO 0101                                                   02490362
20100      IVFAIL = IVFAIL + 1                                          02500362
           RVCORR = 4.0                                                 02510362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    02520362
 0101      CONTINUE                                                     02530362
CT011*  TEST 11                                           5 ARGUMENTS   02540362
           IVTNUM = 11                                                  02550362
        IIDVI = 4.0                                                     02560362
        IIEVI = 5.0                                                     02570362
        RIAVS = AMIN0(IIDVI, -IIDVI, -IIEVI, +IIDVI, IIEVI)             02580362
           IF (RIAVS + 5.0003) 20110, 10110, 40110                      02590362
40110      IF (RIAVS + 4.9997) 10110, 10110, 20110                      02600362
10110      IVPASS = IVPASS + 1                                          02610362
           WRITE (NUVI, 80002) IVTNUM                                   02620362
           GO TO 0111                                                   02630362
20110      IVFAIL = IVFAIL + 1                                          02640362
           RVCORR = -5.0                                                02650362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    02660362
 0111      CONTINUE                                                     02670362
C*****                                                                  02680362
        WRITE (NUVI, 90002)                                             02690362
        WRITE (NUVI, 90013)                                             02700362
        WRITE (NUVI, 90014)                                             02710362
C*****    TEST OF AMIN1                                                 02720362
C*****                                                                  02730362
        WRITE(NUVI, 16704)                                              02740362
16704   FORMAT (/ 8X, 13HTEST OF AMIN1)                                 02750362
CT012*  TEST 12                                      BOTH VALUES ZERO   02760362
           IVTNUM = 12                                                  02770362
        RIBVS = 0.0                                                     02780362
        RIDVS = 0.0                                                     02790362
        RIAVS = AMIN1(RIBVS, RIDVS)                                     02800362
           IF (RIAVS + 0.00005) 20120, 10120, 40120                     02810362
40120      IF (RIAVS - 0.00005) 10120, 10120, 20120                     02820362
10120      IVPASS = IVPASS + 1                                          02830362
           WRITE (NUVI, 80002) IVTNUM                                   02840362
           GO TO 0121                                                   02850362
20120      IVFAIL = IVFAIL + 1                                          02860362
           RVCORR = 0.0                                                 02870362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    02880362
 0121      CONTINUE                                                     02890362
CT013*  TEST 13                     FIRST VALUE NON-ZERO, SECOND ZERO   02900362
           IVTNUM = 13                                                  02910362
        RIBVS = 5.625                                                   02920362
        RIDVS = 0.0                                                     02930362
        RIAVS = AMIN1(RIBVS, RIDVS)                                     02940362
           IF (RIAVS + 0.00005) 20130, 10130, 40130                     02950362
40130      IF (RIAVS - 0.00005) 10130, 10130, 20130                     02960362
10130      IVPASS = IVPASS + 1                                          02970362
           WRITE (NUVI, 80002) IVTNUM                                   02980362
           GO TO 0131                                                   02990362
20130      IVFAIL = IVFAIL + 1                                          03000362
           RVCORR = 0.0                                                 03010362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    03020362
 0131      CONTINUE                                                     03030362
CT014*  TEST 14                                     BOTH VALUES EQUAL   03040362
           IVTNUM = 14                                                  03050362
        RIBVS = 6.5                                                     03060362
        RIDVS = 6.5                                                     03070362
        RIAVS = AMIN1(RIBVS, RIDVS)                                     03080362
           IF (RIAVS - 6.4996) 20140, 10140, 40140                      03090362
40140      IF (RIAVS - 6.5004) 10140, 10140, 20140                      03100362
10140      IVPASS = IVPASS + 1                                          03110362
           WRITE (NUVI, 80002) IVTNUM                                   03120362
           GO TO 0141                                                   03130362
20140      IVFAIL = IVFAIL + 1                                          03140362
           RVCORR = 6.5                                                 03150362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    03160362
 0141      CONTINUE                                                     03170362
CT015*  TEST 15                                      VALUES NOT EQUAL   03180362
           IVTNUM = 15                                                  03190362
        RIBVS = 7.125                                                   03200362
        RIDVS = 5.125                                                   03210362
        RIAVS = AMIN1(RIBVS, RIDVS)                                     03220362
           IF (RIAVS - 5.1247) 20150, 10150, 40150                      03230362
40150      IF (RIAVS - 5.1253) 10150, 10150, 20150                      03240362
10150      IVPASS = IVPASS + 1                                          03250362
           WRITE (NUVI, 80002) IVTNUM                                   03260362
           GO TO 0151                                                   03270362
20150      IVFAIL = IVFAIL + 1                                          03280362
           RVCORR = 5.125                                               03290362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    03300362
 0151      CONTINUE                                                     03310362
CT016*  TEST 16                     FIRST VALUE NEGATIVE, SECOND ZERO   03320362
           IVTNUM = 16                                                  03330362
        RIBVS = -5.625                                                  03340362
        RIDVS = 0.0                                                     03350362
        RIAVS = AMIN1(RIBVS, RIDVS)                                     03360362
           IF (RIAVS + 5.6253) 20160, 10160, 40160                      03370362
40160      IF (RIAVS + 5.6247) 10160, 10160, 20160                      03380362
10160      IVPASS = IVPASS + 1                                          03390362
           WRITE (NUVI, 80002) IVTNUM                                   03400362
           GO TO 0161                                                   03410362
20160      IVFAIL = IVFAIL + 1                                          03420362
           RVCORR = -5.625                                              03430362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    03440362
 0161      CONTINUE                                                     03450362
CT017*  TEST 17                      BOTH VALUES EQUAL, BOTH NEGATIVE   03460362
           IVTNUM = 17                                                  03470362
        RIBVS = -6.5                                                    03480362
        RIDVS = -6.5                                                    03490362
        RIAVS = AMIN1(RIBVS, RIDVS)                                     03500362
           IF (RIAVS + 6.5004) 20170, 10170, 40170                      03510362
40170      IF (RIAVS + 6.4996) 10170, 10170, 20170                      03520362
10170      IVPASS = IVPASS + 1                                          03530362
           WRITE (NUVI, 80002) IVTNUM                                   03540362
           GO TO 0171                                                   03550362
20170      IVFAIL = IVFAIL + 1                                          03560362
           RVCORR = -6.5                                                03570362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    03580362
 0171      CONTINUE                                                     03590362
CT018*  TEST 18                       VALUES NOT EQUAL, BOTH NEGATIVE   03600362
           IVTNUM = 18                                                  03610362
        RIBVS = -7.125                                                  03620362
        RIDVS = -5.125                                                  03630362
        RIAVS = AMIN1(RIBVS, RIDVS)                                     03640362
           IF (RIAVS + 7.1254) 20180, 10180, 40180                      03650362
40180      IF (RIAVS + 7.1246) 10180, 10180, 20180                      03660362
10180      IVPASS = IVPASS + 1                                          03670362
           WRITE (NUVI, 80002) IVTNUM                                   03680362
           GO TO 0181                                                   03690362
20180      IVFAIL = IVFAIL + 1                                          03700362
           RVCORR = -7.125                                              03710362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    03720362
 0181      CONTINUE                                                     03730362
CT019*  TEST 19 FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   03740362
           IVTNUM = 19                                                  03750362
        RIDVS = 5.625                                                   03760362
        RIEVS = 0.0                                                     03770362
        RIAVS = AMIN1(RIDVS, -RIEVS)                                    03780362
           IF (RIAVS + 0.00005) 20190, 10190, 40190                     03790362
40190      IF (RIAVS - 0.00005) 10190, 10190, 20190                     03800362
10190      IVPASS = IVPASS + 1                                          03810362
           WRITE (NUVI, 80002) IVTNUM                                   03820362
           GO TO 0191                                                   03830362
20190      IVFAIL = IVFAIL + 1                                          03840362
           RVCORR = 0.0                                                 03850362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    03860362
 0191      CONTINUE                                                     03870362
CT020*  TEST 20                                EXPRESSION AS ARGUMENT   03880362
           IVTNUM = 20                                                  03890362
        RIDVS = 3.5                                                     03900362
        RIEVS = 4.0                                                     03910362
        RIAVS = AMIN1(RIDVS + RIEVS, -RIEVS - RIDVS)                    03920362
           IF (RIAVS + 7.5004) 20200, 10200, 40200                      03930362
40200      IF (RIAVS + 7.4996) 10200, 10200, 20200                      03940362
10200      IVPASS = IVPASS + 1                                          03950362
           WRITE (NUVI, 80002) IVTNUM                                   03960362
           GO TO 0201                                                   03970362
20200      IVFAIL = IVFAIL + 1                                          03980362
           RVCORR = -7.5                                                03990362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    04000362
 0201      CONTINUE                                                     04010362
CT021*  TEST 21                                           3 ARGUMENTS   04020362
           IVTNUM = 21                                                  04030362
        RIBVS = 0.0                                                     04040362
        RICVS = 1.0                                                     04050362
        RIDVS = 10.9                                                    04060362
        RIAVS = AMIN1(RIDVS, RICVS, RIBVS)                              04070362
           IF (RIAVS + 0.00005) 20210, 10210, 40210                     04080362
40210      IF (RIAVS - 0.00005) 10210, 10210, 20210                     04090362
10210      IVPASS = IVPASS + 1                                          04100362
           WRITE (NUVI, 80002) IVTNUM                                   04110362
           GO TO 0211                                                   04120362
20210      IVFAIL = IVFAIL + 1                                          04130362
           RVCORR = 0.0                                                 04140362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    04150362
 0211      CONTINUE                                                     04160362
CT022*  TEST 22                                           4 ARGUMENTS   04170362
           IVTNUM = 22                                                  04180362
        RIBVS = -9.0                                                    04190362
        RICVS = 10.0                                                    04200362
        RIDVS = 3.5                                                     04210362
        RIAVS = AMIN1(RIDVS, RICVS, -RIBVS, RIDVS)                      04220362
           IF (RIAVS - 3.4998) 20220, 10220, 40220                      04230362
40220      IF (RIAVS - 3.5002) 10220, 10220, 20220                      04240362
10220      IVPASS = IVPASS + 1                                          04250362
           WRITE (NUVI, 80002) IVTNUM                                   04260362
           GO TO 0221                                                   04270362
20220      IVFAIL = IVFAIL + 1                                          04280362
           RVCORR = 3.5                                                 04290362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    04300362
 0221      CONTINUE                                                     04310362
CT023*  TEST 23                                           5 ARGUMENTS   04320362
           IVTNUM = 23                                                  04330362
        RIDVS = 3.5                                                     04340362
        RIEVS = 4.5                                                     04350362
        RIAVS = AMIN1(RIDVS, -RIDVS, -RIEVS, +RIDVS, RIEVS)             04360362
           IF (RIAVS + 4.5003) 20230, 10230, 40230                      04370362
40230      IF (RIAVS + 4.4997) 10230, 10230, 20230                      04380362
10230      IVPASS = IVPASS + 1                                          04390362
           WRITE (NUVI, 80002) IVTNUM                                   04400362
           GO TO 0231                                                   04410362
20230      IVFAIL = IVFAIL + 1                                          04420362
           RVCORR = -4.5                                                04430362
           WRITE (NUVI, 80012) IVTNUM, RIAVS, RVCORR                    04440362
 0231      CONTINUE                                                     04450362
C*****                                                                  04460362
        WRITE (NUVI, 90002)                                             04470362
        WRITE (NUVI, 90013)                                             04480362
        WRITE (NUVI, 90014)                                             04490362
C*****    TEST OF MIN0                                                  04500362
C*****                                                                  04510362
        WRITE(NUVI, 16705)                                              04520362
16705   FORMAT (/ 8X, 12HTEST OF MIN0)                                  04530362
CT024*  TEST 24                                      BOTH VALUES ZERO   04540362
           IVTNUM = 24                                                  04550362
        IIBVI = 0                                                       04560362
        IIDVI = 0                                                       04570362
        IIAVI = MIN0(IIBVI, IIDVI)                                      04580362
           IF (IIAVI - 0) 20240, 10240, 20240                           04590362
10240      IVPASS = IVPASS + 1                                          04600362
           WRITE (NUVI, 80002) IVTNUM                                   04610362
           GO TO 0241                                                   04620362
20240      IVFAIL = IVFAIL + 1                                          04630362
           IVCORR = 0                                                   04640362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    04650362
 0241      CONTINUE                                                     04660362
CT025*  TEST 25                     FIRST VALUE NON-ZERO, SECOND ZERO   04670362
           IVTNUM = 25                                                  04680362
        IIBVI = 6                                                       04690362
        IIDVI = 0                                                       04700362
        IIAVI = MIN0(IIBVI, IIDVI)                                      04710362
           IF (IIAVI - 0) 20250, 10250, 20250                           04720362
10250      IVPASS = IVPASS + 1                                          04730362
           WRITE (NUVI, 80002) IVTNUM                                   04740362
           GO TO 0251                                                   04750362
20250      IVFAIL = IVFAIL + 1                                          04760362
           IVCORR = 0                                                   04770362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    04780362
 0251      CONTINUE                                                     04790362
CT026*  TEST 26                                     BOTH VALUES EQUAL   04800362
           IVTNUM = 26                                                  04810362
        IIBVI = 7                                                       04820362
        IIDVI = 7                                                       04830362
        IIAVI = MIN0(IIBVI, IIDVI)                                      04840362
           IF (IIAVI - 7) 20260, 10260, 20260                           04850362
10260      IVPASS = IVPASS + 1                                          04860362
           WRITE (NUVI, 80002) IVTNUM                                   04870362
           GO TO 0261                                                   04880362
20260      IVFAIL = IVFAIL + 1                                          04890362
           IVCORR = 7                                                   04900362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    04910362
 0261      CONTINUE                                                     04920362
CT027*  TEST 27                                      VALUES NOT EQUAL   04930362
           IVTNUM = 27                                                  04940362
        IIBVI = 7                                                       04950362
        IIDVI = 5                                                       04960362
        IIAVI = MIN0(IIBVI, IIDVI)                                      04970362
           IF (IIAVI - 5) 20270, 10270, 20270                           04980362
10270      IVPASS = IVPASS + 1                                          04990362
           WRITE (NUVI, 80002) IVTNUM                                   05000362
           GO TO 0271                                                   05010362
20270      IVFAIL = IVFAIL + 1                                          05020362
           IVCORR = 5                                                   05030362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05040362
 0271      CONTINUE                                                     05050362
CT028*  TEST 28                     FIRST VALUE NEGATIVE, SECOND ZERO   05060362
           IVTNUM = 28                                                  05070362
        IIBVI = -6                                                      05080362
        IIDVI = 0                                                       05090362
        IIAVI = MIN0(IIBVI, IIDVI)                                      05100362
           IF (IIAVI + 6) 20280, 10280, 20280                           05110362
10280      IVPASS = IVPASS + 1                                          05120362
           WRITE (NUVI, 80002) IVTNUM                                   05130362
           GO TO 0281                                                   05140362
20280      IVFAIL = IVFAIL + 1                                          05150362
           IVCORR = -6                                                  05160362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05170362
 0281      CONTINUE                                                     05180362
CT029*  TEST 29                      BOTH VALUES EQUAL, BOTH NEGATIVE   05190362
           IVTNUM = 29                                                  05200362
        IIBVI = -7                                                      05210362
        IIDVI = -7                                                      05220362
        IIAVI = MIN0(IIBVI, IIDVI)                                      05230362
           IF (IIAVI + 7) 20290, 10290, 20290                           05240362
10290      IVPASS = IVPASS + 1                                          05250362
           WRITE (NUVI, 80002) IVTNUM                                   05260362
           GO TO 0291                                                   05270362
20290      IVFAIL = IVFAIL + 1                                          05280362
           IVCORR = -7                                                  05290362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05300362
 0291      CONTINUE                                                     05310362
CT030*  TEST 30                       VALUES NOT EQUAL, BOTH NEGATIVE   05320362
           IVTNUM = 30                                                  05330362
        IIBVI = -7                                                      05340362
        IIDVI = -5                                                      05350362
        IIAVI = MIN0(IIBVI, IIDVI)                                      05360362
           IF (IIAVI + 7) 20300, 10300, 20300                           05370362
10300      IVPASS = IVPASS + 1                                          05380362
           WRITE (NUVI, 80002) IVTNUM                                   05390362
           GO TO 0301                                                   05400362
20300      IVFAIL = IVFAIL + 1                                          05410362
           IVCORR = -7                                                  05420362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05430362
 0301      CONTINUE                                                     05440362
CT031*  TEST 31 FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   05450362
           IVTNUM = 31                                                  05460362
        IIDVI = 6                                                       05470362
        IIEVI = 0                                                       05480362
        IIAVI = MIN0(IIDVI, -IIEVI)                                     05490362
           IF (IIAVI - 0) 20310, 10310, 20310                           05500362
10310      IVPASS = IVPASS + 1                                          05510362
           WRITE (NUVI, 80002) IVTNUM                                   05520362
           GO TO 0311                                                   05530362
20310      IVFAIL = IVFAIL + 1                                          05540362
           IVCORR = 0                                                   05550362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05560362
 0311      CONTINUE                                                     05570362
CT032*  TEST 32                      EXPRESSION PRESENTED TO FUNCTION   05580362
           IVTNUM = 32                                                  05590362
        IIDVI = 3                                                       05600362
        IIEVI = 4                                                       05610362
        IIAVI = MIN0(IIDVI + IIEVI, -IIEVI - IIDVI)                     05620362
           IF (IIAVI + 7) 20320, 10320, 20320                           05630362
10320      IVPASS = IVPASS + 1                                          05640362
           WRITE (NUVI, 80002) IVTNUM                                   05650362
           GO TO 0321                                                   05660362
20320      IVFAIL = IVFAIL + 1                                          05670362
           IVCORR = -7                                                  05680362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05690362
 0321      CONTINUE                                                     05700362
CT033*  TEST 33                                           3 ARGUMENTS   05710362
           IVTNUM = 33                                                  05720362
        IIBVI = 0                                                       05730362
        IICVI = 10                                                      05740362
        IIDVI = -11                                                     05750362
        IIAVI = MIN0(IICVI, IIBVI, -IIDVI)                              05760362
           IF (IIAVI - 0) 20330, 10330, 20330                           05770362
10330      IVPASS = IVPASS + 1                                          05780362
           WRITE (NUVI, 80002) IVTNUM                                   05790362
           GO TO 0331                                                   05800362
20330      IVFAIL = IVFAIL + 1                                          05810362
           IVCORR = 0                                                   05820362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05830362
 0331      CONTINUE                                                     05840362
CT034*  TEST 34                                           4 ARGUMENTS   05850362
           IVTNUM = 34                                                  05860362
        IIAVI = 10                                                      05870362
        IIBVI = -4                                                      05880362
        IICVI = 8                                                       05890362
        IIDVI = 4                                                       05900362
        IIAVI = MIN0(IIAVI, -IIBVI, IICVI, IIDVI)                       05910362
           IF (IIAVI - 4) 20340, 10340, 20340                           05920362
10340      IVPASS = IVPASS + 1                                          05930362
           WRITE (NUVI, 80002) IVTNUM                                   05940362
           GO TO 0341                                                   05950362
20340      IVFAIL = IVFAIL + 1                                          05960362
           IVCORR = 4                                                   05970362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    05980362
 0341      CONTINUE                                                     05990362
CT035*  TEST 35                                           5 ARGUMENTS   06000362
           IVTNUM = 35                                                  06010362
        IIDVI = 4                                                       06020362
        IIEVI = 5                                                       06030362
        IIAVI = MIN0(IIDVI, -IIDVI, -IIEVI, +IIDVI, IIEVI)              06040362
           IF (IIAVI + 5) 20350, 10350, 20350                           06050362
10350      IVPASS = IVPASS + 1                                          06060362
           WRITE (NUVI, 80002) IVTNUM                                   06070362
           GO TO 0351                                                   06080362
20350      IVFAIL = IVFAIL + 1                                          06090362
           IVCORR = -5                                                  06100362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    06110362
 0351      CONTINUE                                                     06120362
C*****                                                                  06130362
        WRITE (NUVI, 90002)                                             06140362
        WRITE (NUVI, 90013)                                             06150362
        WRITE (NUVI, 90014)                                             06160362
C*****    TEST OF MIN1                                                  06170362
C*****                                                                  06180362
        WRITE(NUVI, 16707)                                              06190362
16707   FORMAT (/ 8X, 12HTEST OF MIN1)                                  06200362
CT036*  TEST 36                                      BOTH VALUES ZERO   06210362
           IVTNUM = 36                                                  06220362
        RIBVS = 0.0                                                     06230362
        RIDVS = 0.0                                                     06240362
        IIAVI = MIN1(RIBVS, RIDVS)                                      06250362
           IF (IIAVI - 0) 20360, 10360, 20360                           06260362
10360      IVPASS = IVPASS + 1                                          06270362
           WRITE (NUVI, 80002) IVTNUM                                   06280362
           GO TO 0361                                                   06290362
20360      IVFAIL = IVFAIL + 1                                          06300362
           IVCORR = 0                                                   06310362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    06320362
 0361      CONTINUE                                                     06330362
CT037*  TEST 37                     FIRST VALUE NON-ZERO, SECOND ZERO   06340362
           IVTNUM = 37                                                  06350362
        RIBVS = 5.625                                                   06360362
        RIDVS = 0.0                                                     06370362
        IIAVI = MIN1(RIBVS, RIDVS)                                      06380362
           IF (IIAVI - 0) 20370, 10370, 20370                           06390362
10370      IVPASS = IVPASS + 1                                          06400362
           WRITE (NUVI, 80002) IVTNUM                                   06410362
           GO TO 0371                                                   06420362
20370      IVFAIL = IVFAIL + 1                                          06430362
           IVCORR = 0                                                   06440362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    06450362
 0371      CONTINUE                                                     06460362
CT038*  TEST 38                                     BOTH VALUES EQUAL   06470362
           IVTNUM = 38                                                  06480362
        RIBVS = 6.5                                                     06490362
        RIDVS = 6.5                                                     06500362
        IIAVI = MIN1(RIBVS, RIDVS)                                      06510362
           IF (IIAVI - 6) 20380, 10380, 20380                           06520362
10380      IVPASS = IVPASS + 1                                          06530362
           WRITE (NUVI, 80002) IVTNUM                                   06540362
           GO TO 0381                                                   06550362
20380      IVFAIL = IVFAIL + 1                                          06560362
           IVCORR = 6                                                   06570362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    06580362
 0381      CONTINUE                                                     06590362
CT039*  TEST 39                                      VALUES NOT EQUAL   06600362
           IVTNUM = 39                                                  06610362
        RIBVS = 7.125                                                   06620362
        RIDVS = 5.125                                                   06630362
        IIAVI = MIN1(RIBVS, RIDVS)                                      06640362
           IF (IIAVI - 5) 20390, 10390, 20390                           06650362
10390      IVPASS = IVPASS + 1                                          06660362
           WRITE (NUVI, 80002) IVTNUM                                   06670362
           GO TO 0391                                                   06680362
20390      IVFAIL = IVFAIL + 1                                          06690362
           IVCORR = 5                                                   06700362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    06710362
 0391      CONTINUE                                                     06720362
CT040*  TEST 40                     FIRST VALUE NEGATIVE, SECOND ZERO   06730362
           IVTNUM = 40                                                  06740362
        RIBVS = -5.625                                                  06750362
        RIDVS = 0.0                                                     06760362
        IIAVI = MIN1(RIBVS, RIDVS)                                      06770362
           IF (IIAVI + 5) 20400, 10400, 20400                           06780362
10400      IVPASS = IVPASS + 1                                          06790362
           WRITE (NUVI, 80002) IVTNUM                                   06800362
           GO TO 0401                                                   06810362
20400      IVFAIL = IVFAIL + 1                                          06820362
           IVCORR = -5                                                  06830362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    06840362
 0401      CONTINUE                                                     06850362
CT041*  TEST 41                      BOTH VALUES EQUAL, BOTH NEGATIVE   06860362
           IVTNUM = 41                                                  06870362
        RIBVS = -6.5                                                    06880362
        RIDVS = -6.5                                                    06890362
        IIAVI = MIN1(RIBVS, RIDVS)                                      06900362
           IF (IIAVI + 6) 20410, 10410, 20410                           06910362
10410      IVPASS = IVPASS + 1                                          06920362
           WRITE (NUVI, 80002) IVTNUM                                   06930362
           GO TO 0411                                                   06940362
20410      IVFAIL = IVFAIL + 1                                          06950362
           IVCORR = -6                                                  06960362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    06970362
 0411      CONTINUE                                                     06980362
CT042*  TEST 42                       VALUES NOT EQUAL, BOTH NEGATIVE   06990362
           IVTNUM = 42                                                  07000362
        RIBVS = -7.125                                                  07010362
        RIDVS = -5.125                                                  07020362
        IIAVI = MIN1(RIBVS, RIDVS)                                      07030362
           IF (IIAVI + 7) 20420, 10420, 20420                           07040362
10420      IVPASS = IVPASS + 1                                          07050362
           WRITE (NUVI, 80002) IVTNUM                                   07060362
           GO TO 0421                                                   07070362
20420      IVFAIL = IVFAIL + 1                                          07080362
           IVCORR = -7                                                  07090362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    07100362
 0421      CONTINUE                                                     07110362
CT043*  TEST 43 FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   07120362
           IVTNUM = 43                                                  07130362
        RIDVS = 5.625                                                   07140362
        RIEVS = 0.0                                                     07150362
        IIAVI = MIN1(RIDVS, -RIEVS)                                     07160362
           IF (IIAVI - 0) 20430, 10430, 20430                           07170362
10430      IVPASS = IVPASS + 1                                          07180362
           WRITE (NUVI, 80002) IVTNUM                                   07190362
           GO TO 0431                                                   07200362
20430      IVFAIL = IVFAIL + 1                                          07210362
           IVCORR = 0                                                   07220362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    07230362
 0431      CONTINUE                                                     07240362
CT044*  TEST 44                      EXPRESSION PRESENTED TO FUNCTION   07250362
           IVTNUM = 44                                                  07260362
        RIDVS = 3.5                                                     07270362
        RIEVS = 4.0                                                     07280362
        IIAVI = MIN1(RIDVS + RIEVS, -RIEVS - RIDVS)                     07290362
           IF (IIAVI + 7) 20440, 10440, 20440                           07300362
10440      IVPASS = IVPASS + 1                                          07310362
           WRITE (NUVI, 80002) IVTNUM                                   07320362
           GO TO 0441                                                   07330362
20440      IVFAIL = IVFAIL + 1                                          07340362
           IVCORR = -7                                                  07350362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    07360362
 0441      CONTINUE                                                     07370362
CT045*  TEST 45                                           3 ARGUMENTS   07380362
           IVTNUM = 45                                                  07390362
        RIBVS = 0.0                                                     07400362
        RICVS = 1.0                                                     07410362
        RIDVS = 2.0                                                     07420362
        IIAVI = MIN1(RIBVS, RICVS, RIDVS)                               07430362
           IF (IIAVI - 0) 20450, 10450, 20450                           07440362
10450      IVPASS = IVPASS + 1                                          07450362
           WRITE (NUVI, 80002) IVTNUM                                   07460362
           GO TO 0451                                                   07470362
20450      IVFAIL = IVFAIL + 1                                          07480362
           IVCORR = 0                                                   07490362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    07500362
 0451      CONTINUE                                                     07510362
CT046*  TEST 46                                           4 ARGUMENTS   07520362
           IVTNUM = 46                                                  07530362
        RIAVS = -3.5                                                    07540362
        RIBVS = 12.0                                                    07550362
        RICVS = 3.6                                                     07560362
        RIDVS = 3.5                                                     07570362
        IIAVI = MIN1(-RIAVS, RIBVS, RICVS, RIDVS)                       07580362
           IF (IIAVI - 3) 20460, 10460, 20460                           07590362
10460      IVPASS = IVPASS + 1                                          07600362
           WRITE (NUVI, 80002) IVTNUM                                   07610362
           GO TO 0461                                                   07620362
20460      IVFAIL = IVFAIL + 1                                          07630362
           IVCORR = 3                                                   07640362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    07650362
 0461      CONTINUE                                                     07660362
CT047*  TEST 47                                           5 ARGUMENTS   07670362
           IVTNUM = 47                                                  07680362
        RIDVS = 3.5                                                     07690362
        RIEVS = 4.5                                                     07700362
        IIAVI = MIN1(RIDVS, -RIDVS, -RIEVS, +RIDVS, RIEVS)              07710362
           IF (IIAVI + 4) 20470, 10470, 20470                           07720362
10470      IVPASS = IVPASS + 1                                          07730362
           WRITE (NUVI, 80002) IVTNUM                                   07740362
           GO TO 0471                                                   07750362
20470      IVFAIL = IVFAIL + 1                                          07760362
           IVCORR = -4                                                  07770362
           WRITE (NUVI, 80010) IVTNUM, IIAVI, IVCORR                    07780362
 0471      CONTINUE                                                     07790362
C*****                                                                  07800362
CBB** ********************** BBCSUM0  **********************************07810362
C**** WRITE OUT TEST SUMMARY                                            07820362
C****                                                                   07830362
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07840362
      WRITE (I02, 90004)                                                07850362
      WRITE (I02, 90014)                                                07860362
      WRITE (I02, 90004)                                                07870362
      WRITE (I02, 90020) IVPASS                                         07880362
      WRITE (I02, 90022) IVFAIL                                         07890362
      WRITE (I02, 90024) IVDELE                                         07900362
      WRITE (I02, 90026) IVINSP                                         07910362
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 07920362
CBE** ********************** BBCSUM0  **********************************07930362
CBB** ********************** BBCFOOT0 **********************************07940362
C**** WRITE OUT REPORT FOOTINGS                                         07950362
C****                                                                   07960362
      WRITE (I02,90016) ZPROG, ZPROG                                    07970362
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     07980362
      WRITE (I02,90019)                                                 07990362
CBE** ********************** BBCFOOT0 **********************************08000362
CBB** ********************** BBCFMT0A **********************************08010362
C**** FORMATS FOR TEST DETAIL LINES                                     08020362
C****                                                                   08030362
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           08040362
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           08050362
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           08060362
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           08070362
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           08080362
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    08090362
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08100362
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              08110362
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08120362
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  08130362
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         08140362
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         08150362
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         08160362
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         08170362
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      08180362
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      08190362
80050 FORMAT (1H ,48X,A31)                                              08200362
CBE** ********************** BBCFMT0A **********************************08210362
CBB** ********************** BBCFMT0B **********************************08220362
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                08230362
C****                                                                   08240362
90002 FORMAT (1H1)                                                      08250362
90004 FORMAT (1H )                                                      08260362
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               08270362
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08280362
90008 FORMAT (1H ,21X,A13,A17)                                          08290362
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       08300362
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    08310362
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     08320362
     1       7X,7HREMARKS,24X)                                          08330362
90014 FORMAT (1H ,46H----------------------------------------------,    08340362
     1        33H---------------------------------)                     08350362
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               08360362
C****                                                                   08370362
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             08380362
C****                                                                   08390362
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          08400362
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        08410362
     1        A13)                                                      08420362
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 08430362
C****                                                                   08440362
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 08450362
C****                                                                   08460362
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              08470362
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08480362
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08490362
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08500362
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08510362
CBE** ********************** BBCFMT0B **********************************08520362
C*****                                                                  08530362
C*****    END OF TEST SEGMENT 167                                       08540362
        STOP                                                            08550362
        END                                                             08560362
                                                                        08570362
