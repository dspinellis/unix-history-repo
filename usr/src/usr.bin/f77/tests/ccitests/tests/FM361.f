C***********************************************************************00010361
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020361
C*****   FM361               XMAX - (165)                               00030361
C*****                                                                  00040361
C***********************************************************************00050361
C*****  GENERAL PURPOSE                                       SUBSET REF00060361
C*****    TEST OF INTRINSIC FUNCTIONS AMAX0,AMAX1,MAX0,MAX1      15.3   00070361
C*****    CHOOSING LARGEST VALUE                               (TABLE 5)00080361
C*****                                                                  00090361
CBB** ********************** BBCCOMNT **********************************00100361
C****                                                                   00110361
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120361
C****                          VERSION 2.0                              00130361
C****                                                                   00140361
C****                                                                   00150361
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160361
C****                   GENERAL SERVICES ADMINISTRATION                 00170361
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180361
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190361
C****                      FALLS CHURCH, VA. 22041                      00200361
C****                                                                   00210361
C****                          (703) 756-6153                           00220361
C****                                                                   00230361
CBE** ********************** BBCCOMNT **********************************00240361
CBB** ********************** BBCINITA **********************************00250361
C**** SPECIFICATION STATEMENTS                                          00260361
C****                                                                   00270361
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00280361
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00290361
CBE** ********************** BBCINITA **********************************00300361
CBB** ********************** BBCINITB **********************************00310361
C**** INITIALIZE SECTION                                                00320361
      DATA  ZVERS,                  ZVERSD,             ZDATE           00330361
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00340361
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00350361
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00360361
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00370361
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00380361
      DATA   REMRKS /'                               '/                 00390361
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00400361
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00410361
C****                                                                   00420361
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00430361
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00440361
CZ03  ZPROG  = 'PROGRAM NAME'                                           00450361
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00520361
      IVPASS = 0                                                        00530361
      IVFAIL = 0                                                        00540361
      IVDELE = 0                                                        00550361
      IVINSP = 0                                                        00560361
      IVTOTL = 0                                                        00570361
      IVTOTN = 0                                                        00580361
      ICZERO = 0                                                        00590361
C                                                                       00600361
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610361
      I01 = 05                                                          00620361
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630361
      I02 = 06                                                          00640361
C                                                                       00650361
      I01 = 5                                                           00660361
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670361
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00680361
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00690361
C                                                                       00700361
      I02 = 6                                                           00710361
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00720361
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00730361
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00740361
C                                                                       00750361
CBE** ********************** BBCINITB **********************************00760361
      NUVI = I02                                                        00770361
      IVTOTL = 48                                                       00780361
      ZPROG = 'FM361'                                                   00790361
CBB** ********************** BBCHED0A **********************************00800361
C****                                                                   00810361
C**** WRITE REPORT TITLE                                                00820361
C****                                                                   00830361
      WRITE (I02, 90002)                                                00840361
      WRITE (I02, 90006)                                                00850361
      WRITE (I02, 90007)                                                00860361
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00870361
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00880361
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00890361
CBE** ********************** BBCHED0A **********************************00900361
C*****                                                                  00910361
C*****    HEADER FOR SEGMENT 165                                        00920361
        WRITE (NUVI,16501)                                              00930361
16501   FORMAT (1H , // 2X,36HXMAX - (165) INTRINSIC FUNCTIONS--  //13X,00940361
     1          26HAMAX0, AMAX1, MAX0, MAX1     /13X,                   00950361
     2          24H(CHOOSING LARGEST VALUE)//2X,                        00960361
     3          18HSUBSET REF. - 15.3)                                  00970361
CBB** ********************** BBCHED0B **********************************00980361
C**** WRITE DETAIL REPORT HEADERS                                       00990361
C****                                                                   01000361
      WRITE (I02,90004)                                                 01010361
      WRITE (I02,90004)                                                 01020361
      WRITE (I02,90013)                                                 01030361
      WRITE (I02,90014)                                                 01040361
      WRITE (I02,90015) IVTOTL                                          01050361
CBE** ********************** BBCHED0B **********************************01060361
C*****                                                                  01070361
C*****    TEST OF AMAX0                                                 01080361
C*****                                                                  01090361
        WRITE(NUVI, 16502)                                              01100361
16502   FORMAT (/ 8X, 13HTEST OF AMAX0)                                 01110361
CT001*  TEST 1                                            BOTH ZEROES   01120361
           IVTNUM = 1                                                   01130361
        IHBVI = 0                                                       01140361
        IHDVI = 0                                                       01150361
        RHAVS = AMAX0(IHBVI,IHDVI)                                      01160361
           IF (RHAVS + 0.00005) 20010, 10010, 40010                     01170361
40010      IF (RHAVS - 0.00005) 10010, 10010, 20010                     01180361
10010      IVPASS = IVPASS + 1                                          01190361
           WRITE (NUVI, 80002) IVTNUM                                   01200361
           GO TO 0011                                                   01210361
20010      IVFAIL = IVFAIL + 1                                          01220361
           RVCORR = 0.0                                                 01230361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    01240361
 0011      CONTINUE                                                     01250361
CT002*  TEST 2                                 ONE NON-ZERO, ONE ZERO   01260361
           IVTNUM = 2                                                   01270361
        IHBVI = 6                                                       01280361
        IHDVI = 0                                                       01290361
        RHAVS = AMAX0(IHBVI,IHDVI)                                      01300361
           IF (RHAVS - 5.9997) 20020, 10020, 40020                      01310361
40020      IF (RHAVS - 6.0003) 10020, 10020, 20020                      01320361
10020      IVPASS = IVPASS + 1                                          01330361
           WRITE (NUVI, 80002) IVTNUM                                   01340361
           GO TO 0021                                                   01350361
20020      IVFAIL = IVFAIL + 1                                          01360361
           RVCORR = 6.0                                                 01370361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    01380361
 0021      CONTINUE                                                     01390361
CT003*  TEST 3                                      BOTH VALUES EQUAL   01400361
           IVTNUM = 3                                                   01410361
        IHBVI = 7                                                       01420361
        IHDVI = 7                                                       01430361
        RHAVS = AMAX0(IHBVI,IHDVI)                                      01440361
           IF (RHAVS - 6.9996) 20030, 10030, 40030                      01450361
40030      IF (RHAVS - 7.0004) 10030, 10030, 20030                      01460361
10030      IVPASS = IVPASS + 1                                          01470361
           WRITE (NUVI, 80002) IVTNUM                                   01480361
           GO TO 0031                                                   01490361
20030      IVFAIL = IVFAIL + 1                                          01500361
           RVCORR = 7.0                                                 01510361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    01520361
 0031      CONTINUE                                                     01530361
CT004*  TEST 4                          UNEQUAL VALUES, BOTH POSITIVE   01540361
           IVTNUM = 4                                                   01550361
        IHBVI = 7                                                       01560361
        IHDVI = 5                                                       01570361
        RHAVS = AMAX0(IHBVI,IHDVI)                                      01580361
           IF (RHAVS - 6.9996) 20040, 10040, 40040                      01590361
40040      IF (RHAVS - 7.0004) 10040, 10040, 20040                      01600361
10040      IVPASS = IVPASS + 1                                          01610361
           WRITE (NUVI, 80002) IVTNUM                                   01620361
           GO TO 0041                                                   01630361
20040      IVFAIL = IVFAIL + 1                                          01640361
           RVCORR = 7.0                                                 01650361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    01660361
 0041      CONTINUE                                                     01670361
CT005*  TEST 5                                 ONE NEGATIVE, ONE ZERO   01680361
           IVTNUM = 5                                                   01690361
        IHBVI = -6                                                      01700361
        IHDVI = 0                                                       01710361
        RHAVS = AMAX0(IHBVI,IHDVI)                                      01720361
           IF (RHAVS + 0.00005) 20050, 10050, 40050                     01730361
40050      IF (RHAVS - 0.00005) 10050, 10050, 20050                     01740361
10050      IVPASS = IVPASS + 1                                          01750361
           WRITE (NUVI, 80002) IVTNUM                                   01760361
           GO TO 0051                                                   01770361
20050      IVFAIL = IVFAIL + 1                                          01780361
           RVCORR = 0.0                                                 01790361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    01800361
 0051      CONTINUE                                                     01810361
CT006*  TEST 6                       BOTH VALUES EQUAL, BOTH NEGATIVE   01820361
           IVTNUM = 6                                                   01830361
        IHBVI = -7                                                      01840361
        IHDVI = -7                                                      01850361
        RHAVS = AMAX0(IHBVI,IHDVI)                                      01860361
           IF (RHAVS + 7.0004) 20060, 10060, 40060                      01870361
40060      IF (RHAVS + 6.9996) 10060, 10060, 20060                      01880361
10060      IVPASS = IVPASS + 1                                          01890361
           WRITE (NUVI, 80002) IVTNUM                                   01900361
           GO TO 0061                                                   01910361
20060      IVFAIL = IVFAIL + 1                                          01920361
           RVCORR = -7.0                                                01930361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    01940361
 0061      CONTINUE                                                     01950361
CT007*  TEST 7                   BOTH VALUES NOT EQUAL, BOTH NEGATIVE   01960361
           IVTNUM = 7                                                   01970361
        IHBVI = -7                                                      01980361
        IHDVI = -5                                                      01990361
        RHAVS = AMAX0(IHBVI,IHDVI)                                      02000361
           IF (RHAVS + 5.0003) 20070, 10070, 40070                      02010361
40070      IF (RHAVS + 4.9997) 10070, 10070, 20070                      02020361
10070      IVPASS = IVPASS + 1                                          02030361
           WRITE (NUVI, 80002) IVTNUM                                   02040361
           GO TO 0071                                                   02050361
20070      IVFAIL = IVFAIL + 1                                          02060361
           RVCORR = -5.0                                                02070361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    02080361
 0071      CONTINUE                                                     02090361
CT008*  TEST 8  1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY A MINUS SIGN   02100361
           IVTNUM = 8                                                   02110361
        IHDVI = 6                                                       02120361
        IHEVI = 0                                                       02130361
        RHAVS = AMAX0(IHDVI, -IHEVI)                                    02140361
           IF (RHAVS - 5.9997) 20080, 10080, 40080                      02150361
40080      IF (RHAVS - 6.0003) 10080, 10080, 20080                      02160361
10080      IVPASS = IVPASS + 1                                          02170361
           WRITE (NUVI, 80002) IVTNUM                                   02180361
           GO TO 0081                                                   02190361
20080      IVFAIL = IVFAIL + 1                                          02200361
           RVCORR = 6.0                                                 02210361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    02220361
 0081      CONTINUE                                                     02230361
CT009*  TEST 9                      EXPRESSIONS PRESENTED TO FUNCTION   02240361
           IVTNUM = 9                                                   02250361
        IHDVI = 3                                                       02260361
        IHEVI = 4                                                       02270361
        RHAVS = AMAX0(IHDVI + IHEVI, -IHEVI - IHDVI)                    02280361
           IF (RHAVS - 6.9996) 20090, 10090, 40090                      02290361
40090      IF (RHAVS - 7.0004) 10090, 10090, 20090                      02300361
10090      IVPASS = IVPASS + 1                                          02310361
           WRITE (NUVI, 80002) IVTNUM                                   02320361
           GO TO 0091                                                   02330361
20090      IVFAIL = IVFAIL + 1                                          02340361
           RVCORR = 7.0                                                 02350361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    02360361
 0091      CONTINUE                                                     02370361
CT010*  TEST 10                                           3 ARGUMENTS   02380361
           IVTNUM = 10                                                  02390361
        IHBVI = 0                                                       02400361
        IHCVI = 1                                                       02410361
        IHDVI = 3                                                       02420361
        RHAVS = AMAX0(IHBVI, IHCVI, IHDVI)                              02430361
           IF (RHAVS - 2.9998) 20100, 10100, 40100                      02440361
40100      IF (RHAVS - 3.0002) 10100, 10100, 20100                      02450361
10100      IVPASS = IVPASS + 1                                          02460361
           WRITE (NUVI, 80002) IVTNUM                                   02470361
           GO TO 0101                                                   02480361
20100      IVFAIL = IVFAIL + 1                                          02490361
           RVCORR = 3.0                                                 02500361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    02510361
 0101      CONTINUE                                                     02520361
CT011*  TEST 11                                           4 ARGUMENTS   02530361
           IVTNUM = 11                                                  02540361
        IHBVI = 0                                                       02550361
        IHCVI = 1                                                       02560361
        IHDVI = 4                                                       02570361
        RHAVS = AMAX0(IHDVI, -IHBVI, IHCVI, IHBVI)                      02580361
           IF (RHAVS - 3.9998) 20110, 10110, 40110                      02590361
40110      IF (RHAVS - 4.0002) 10110, 10110, 20110                      02600361
10110      IVPASS = IVPASS + 1                                          02610361
           WRITE (NUVI, 80002) IVTNUM                                   02620361
           GO TO 0111                                                   02630361
20110      IVFAIL = IVFAIL + 1                                          02640361
           RVCORR = 4.0                                                 02650361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    02660361
 0111      CONTINUE                                                     02670361
CT012*  TEST 12                                           5 ARGUMENTS   02680361
           IVTNUM = 12                                                  02690361
        IHDVI = 4.0                                                     02700361
        IHEVI = 5.0                                                     02710361
        RHAVS = AMAX0(IHDVI, -IHDVI, -IHEVI, +IHDVI, IHEVI)             02720361
           IF (RHAVS - 4.9997) 20120, 10120, 40120                      02730361
40120      IF (RHAVS - 5.0003) 10120, 10120, 20120                      02740361
10120      IVPASS = IVPASS + 1                                          02750361
           WRITE (NUVI, 80002) IVTNUM                                   02760361
           GO TO 0121                                                   02770361
20120      IVFAIL = IVFAIL + 1                                          02780361
           RVCORR = 5.0                                                 02790361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    02800361
 0121      CONTINUE                                                     02810361
C*****                                                                  02820361
        WRITE (NUVI, 90002)                                             02830361
        WRITE (NUVI, 90013)                                             02840361
        WRITE (NUVI, 90014)                                             02850361
C*****    TEST OF AMAX1                                                 02860361
C*****                                                                  02870361
        WRITE(NUVI, 16504)                                              02880361
16504   FORMAT (/ 8X, 13HTEST OF AMAX1)                                 02890361
CT013*  TEST 13                                      BOTH VALUES ZERO   02900361
           IVTNUM = 13                                                  02910361
        RHBVS = 0.0                                                     02920361
        RHDVS = 0.0                                                     02930361
        RHAVS = AMAX1(RHBVS, RHDVS)                                     02940361
           IF (RHAVS + 0.00005) 20130, 10130, 40130                     02950361
40130      IF (RHAVS - 0.00005) 10130, 10130, 20130                     02960361
10130      IVPASS = IVPASS + 1                                          02970361
           WRITE (NUVI, 80002) IVTNUM                                   02980361
           GO TO 0131                                                   02990361
20130      IVFAIL = IVFAIL + 1                                          03000361
           RVCORR = 0.0                                                 03010361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    03020361
 0131      CONTINUE                                                     03030361
CT014*  TEST 14                     FIRST VALUE NON-ZERO, SECOND ZERO   03040361
           IVTNUM = 14                                                  03050361
        RHBVS = 5.625                                                   03060361
        RHDVS = 0.0                                                     03070361
        RHAVS = AMAX1(RHBVS, RHDVS)                                     03080361
           IF (RHAVS - 5.6247) 20140, 10140, 40140                      03090361
40140      IF (RHAVS - 5.6253) 10140, 10140, 20140                      03100361
10140      IVPASS = IVPASS + 1                                          03110361
           WRITE (NUVI, 80002) IVTNUM                                   03120361
           GO TO 0141                                                   03130361
20140      IVFAIL = IVFAIL + 1                                          03140361
           RVCORR = 5.625                                               03150361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    03160361
 0141      CONTINUE                                                     03170361
CT015*  TEST 15                                     BOTH VALUES EQUAL   03180361
           IVTNUM = 15                                                  03190361
        RHBVS = 6.5                                                     03200361
        RHDVS = 6.5                                                     03210361
        RHAVS = AMAX1(RHBVS, RHDVS)                                     03220361
           IF (RHAVS - 6.4996) 20150, 10150, 40150                      03230361
40150      IF (RHAVS - 6.5004) 10150, 10150, 20150                      03240361
10150      IVPASS = IVPASS + 1                                          03250361
           WRITE (NUVI, 80002) IVTNUM                                   03260361
           GO TO 0151                                                   03270361
20150      IVFAIL = IVFAIL + 1                                          03280361
           RVCORR = 6.5                                                 03290361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    03300361
 0151      CONTINUE                                                     03310361
CT016*  TEST 16                                      VALUES NOT EQUAL   03320361
           IVTNUM = 16                                                  03330361
        RHBVS = 7.125                                                   03340361
        RHDVS = 5.125                                                   03350361
        RHAVS = AMAX1(RHBVS, RHDVS)                                     03360361
           IF (RHAVS - 7.1246) 20160, 10160, 40160                      03370361
40160      IF (RHAVS - 7.1254) 10160, 10160, 20160                      03380361
10160      IVPASS = IVPASS + 1                                          03390361
           WRITE (NUVI, 80002) IVTNUM                                   03400361
           GO TO 0161                                                   03410361
20160      IVFAIL = IVFAIL + 1                                          03420361
           RVCORR = 7.125                                               03430361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    03440361
 0161      CONTINUE                                                     03450361
CT017*  TEST 17                     FIRST VALUE NEGATIVE, SECOND ZERO   03460361
           IVTNUM = 17                                                  03470361
        RHBVS = -5.625                                                  03480361
        RHDVS = 0.0                                                     03490361
        RHAVS = AMAX1(RHBVS, RHDVS)                                     03500361
           IF (RHAVS + 0.00005) 20170, 10170, 40170                     03510361
40170      IF (RHAVS - 0.00005) 10170, 10170, 20170                     03520361
10170      IVPASS = IVPASS + 1                                          03530361
           WRITE (NUVI, 80002) IVTNUM                                   03540361
           GO TO 0171                                                   03550361
20170      IVFAIL = IVFAIL + 1                                          03560361
           RVCORR = 0.0                                                 03570361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    03580361
 0171      CONTINUE                                                     03590361
CT018*  TEST 18                      BOTH VALUES EQUAL, BOTH NEGATIVE   03600361
           IVTNUM = 18                                                  03610361
        RHBVS = -6.5                                                    03620361
        RHDVS = -6.5                                                    03630361
        RHAVS = AMAX1(RHBVS, RHDVS)                                     03640361
           IF (RHAVS + 6.5004) 20180, 10180, 40180                      03650361
40180      IF (RHAVS + 6.4996) 10180, 10180, 20180                      03660361
10180      IVPASS = IVPASS + 1                                          03670361
           WRITE (NUVI, 80002) IVTNUM                                   03680361
           GO TO 0181                                                   03690361
20180      IVFAIL = IVFAIL + 1                                          03700361
           RVCORR = -6.5                                                03710361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    03720361
 0181      CONTINUE                                                     03730361
CT019*  TEST 19                       VALUES NOT EQUAL, BOTH NEGATIVE   03740361
           IVTNUM = 19                                                  03750361
        RHBVS = -7.125                                                  03760361
        RHDVS = -5.125                                                  03770361
        RHAVS = AMAX1(RHBVS, RHDVS)                                     03780361
           IF (RHAVS + 5.1253) 20190, 10190, 40190                      03790361
40190      IF (RHAVS + 5.1247) 10190, 10190, 20190                      03800361
10190      IVPASS = IVPASS + 1                                          03810361
           WRITE (NUVI, 80002) IVTNUM                                   03820361
           GO TO 0191                                                   03830361
20190      IVFAIL = IVFAIL + 1                                          03840361
           RVCORR = -5.125                                              03850361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    03860361
 0191      CONTINUE                                                     03870361
CT020*  TEST 20   1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   03880361
           IVTNUM = 20                                                  03890361
        RHDVS = 5.625                                                   03900361
        RHEVS = 0.0                                                     03910361
        RHAVS = AMAX1(RHDVS, -RHEVS)                                    03920361
           IF (RHAVS - 5.6247) 20200, 10200, 40200                      03930361
40200      IF (RHAVS - 5.6253) 10200, 10200, 20200                      03940361
10200      IVPASS = IVPASS + 1                                          03950361
           WRITE (NUVI, 80002) IVTNUM                                   03960361
           GO TO 0201                                                   03970361
20200      IVFAIL = IVFAIL + 1                                          03980361
           RVCORR = 5.625                                               03990361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    04000361
 0201      CONTINUE                                                     04010361
CT021*  TEST 21                     EXPRESSIONS PRESENTED TO FUNCTION   04020361
           IVTNUM = 21                                                  04030361
        RHDVS = 3.5                                                     04040361
        RHEVS = 4.0                                                     04050361
        RHAVS = AMAX1(RHDVS + RHEVS, -RHEVS - RHDVS)                    04060361
           IF (RHAVS - 7.4996) 20210, 10210, 40210                      04070361
40210      IF (RHAVS - 7.5004) 10210, 10210, 20210                      04080361
10210      IVPASS = IVPASS + 1                                          04090361
           WRITE (NUVI, 80002) IVTNUM                                   04100361
           GO TO 0211                                                   04110361
20210      IVFAIL = IVFAIL + 1                                          04120361
           RVCORR = 7.5                                                 04130361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    04140361
 0211      CONTINUE                                                     04150361
CT022*  TEST 22                                           3 ARGUMENTS   04160361
           IVTNUM = 22                                                  04170361
        RHBVS = 0.0                                                     04180361
        RHCVS = 1.0                                                     04190361
        RHDVS = 0.5                                                     04200361
        RHAVS = AMAX1(RHBVS, RHCVS, RHDVS)                              04210361
           IF (RHAVS - 0.99995) 20220, 10220, 40220                     04220361
40220      IF (RHAVS - 1.0001) 10220, 10220, 20220                      04230361
10220      IVPASS = IVPASS + 1                                          04240361
           WRITE (NUVI, 80002) IVTNUM                                   04250361
           GO TO 0221                                                   04260361
20220      IVFAIL = IVFAIL + 1                                          04270361
           RVCORR = 1.0                                                 04280361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    04290361
 0221      CONTINUE                                                     04300361
CT023*  TEST 23                                           4 ARGUMENTS   04310361
           IVTNUM = 23                                                  04320361
        RHBVS = 1.5                                                     04330361
        RHCVS = 3.4                                                     04340361
        RHDVS = 3.5                                                     04350361
        RHAVS = AMAX1(-RHDVS, RHCVS, RHBVS, RHDVS)                      04360361
           IF (RHAVS - 3.4998) 20230, 10230, 40230                      04370361
40230      IF (RHAVS - 3.5002) 10230, 10230, 20230                      04380361
10230      IVPASS = IVPASS + 1                                          04390361
           WRITE (NUVI, 80002) IVTNUM                                   04400361
           GO TO 0231                                                   04410361
20230      IVFAIL = IVFAIL + 1                                          04420361
           RVCORR = 3.5                                                 04430361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    04440361
 0231      CONTINUE                                                     04450361
CT024*  TEST 24                                           5 ARGUMENTS   04460361
           IVTNUM = 24                                                  04470361
        RHDVS = 3.5                                                     04480361
        RHEVS = 4.5                                                     04490361
        RHAVS = AMAX1(RHDVS, -RHDVS, -RHEVS, +RHDVS, RHEVS)             04500361
           IF (RHAVS - 4.4997) 20240, 10240, 40240                      04510361
40240      IF (RHAVS - 4.5003) 10240, 10240, 20240                      04520361
10240      IVPASS = IVPASS + 1                                          04530361
           WRITE (NUVI, 80002) IVTNUM                                   04540361
           GO TO 0241                                                   04550361
20240      IVFAIL = IVFAIL + 1                                          04560361
           RVCORR = 4.5                                                 04570361
           WRITE (NUVI, 80012) IVTNUM, RHAVS, RVCORR                    04580361
 0241      CONTINUE                                                     04590361
C*****                                                                  04600361
        WRITE (NUVI, 90002)                                             04610361
        WRITE (NUVI, 90013)                                             04620361
        WRITE (NUVI, 90014)                                             04630361
C*****    TEST OF MAX0                                                  04640361
C*****                                                                  04650361
        WRITE(NUVI, 16505)                                              04660361
16505   FORMAT (/ 8X, 12HTEST OF MAX0)                                  04670361
C*****                                                                  04680361
CT025*  TEST 25                                      BOTH VALUES ZERO   04690361
           IVTNUM = 25                                                  04700361
        IHBVI = 0                                                       04710361
        IHDVI = 0                                                       04720361
        IHAVI = MAX0(IHBVI, IHDVI)                                      04730361
           IF (IHAVI - 0) 20250, 10250, 20250                           04740361
10250      IVPASS = IVPASS + 1                                          04750361
           WRITE (NUVI, 80002) IVTNUM                                   04760361
           GO TO 0251                                                   04770361
20250      IVFAIL = IVFAIL + 1                                          04780361
           IVCORR = 0                                                   04790361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    04800361
 0251      CONTINUE                                                     04810361
CT026*  TEST 26                     FIRST VALUE NON-ZERO, SECOND ZERO   04820361
           IVTNUM = 26                                                  04830361
        IHBVI = 6                                                       04840361
        IHDVI = 0                                                       04850361
        IHAVI = MAX0(IHBVI, IHDVI)                                      04860361
           IF (IHAVI - 6) 20260, 10260, 20260                           04870361
10260      IVPASS = IVPASS + 1                                          04880361
           WRITE (NUVI, 80002) IVTNUM                                   04890361
           GO TO 0261                                                   04900361
20260      IVFAIL = IVFAIL + 1                                          04910361
           IVCORR = 6                                                   04920361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    04930361
 0261      CONTINUE                                                     04940361
CT027*  TEST 27                                     BOTH VALUES EQUAL   04950361
           IVTNUM = 27                                                  04960361
        IHBVI = 7                                                       04970361
        IHDVI = 7                                                       04980361
        IHAVI = MAX0(IHBVI, IHDVI)                                      04990361
           IF (IHAVI - 7) 20270, 10270, 20270                           05000361
10270      IVPASS = IVPASS + 1                                          05010361
           WRITE (NUVI, 80002) IVTNUM                                   05020361
           GO TO 0271                                                   05030361
20270      IVFAIL = IVFAIL + 1                                          05040361
           IVCORR = 7                                                   05050361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05060361
 0271      CONTINUE                                                     05070361
CT028*  TEST 28                                      VALUES NOT EQUAL   05080361
           IVTNUM = 28                                                  05090361
        IHBVI = 7                                                       05100361
        IHDVI = 5                                                       05110361
        IHAVI = MAX0(IHBVI, IHDVI)                                      05120361
           IF (IHAVI - 7) 20280, 10280, 20280                           05130361
10280      IVPASS = IVPASS + 1                                          05140361
           WRITE (NUVI, 80002) IVTNUM                                   05150361
           GO TO 0281                                                   05160361
20280      IVFAIL = IVFAIL + 1                                          05170361
           IVCORR = 7                                                   05180361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05190361
 0281      CONTINUE                                                     05200361
CT029*  TEST 29                     FIRST VALUE NEGATIVE, SECOND ZERO   05210361
           IVTNUM = 29                                                  05220361
        IHBVI = -6                                                      05230361
        IHDVI = 0                                                       05240361
        IHAVI = MAX0(IHBVI, IHDVI)                                      05250361
           IF (IHAVI - 0) 20290, 10290, 20290                           05260361
10290      IVPASS = IVPASS + 1                                          05270361
           WRITE (NUVI, 80002) IVTNUM                                   05280361
           GO TO 0291                                                   05290361
20290      IVFAIL = IVFAIL + 1                                          05300361
           IVCORR = 0                                                   05310361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05320361
 0291      CONTINUE                                                     05330361
CT030*  TEST 30                      BOTH VALUES EQUAL, BOTH NEGATIVE   05340361
           IVTNUM = 30                                                  05350361
        IHBVI = -7                                                      05360361
        IHDVI = -7                                                      05370361
        IHAVI = MAX0(IHBVI, IHDVI)                                      05380361
           IF (IHAVI + 7) 20300, 10300, 20300                           05390361
10300      IVPASS = IVPASS + 1                                          05400361
           WRITE (NUVI, 80002) IVTNUM                                   05410361
           GO TO 0301                                                   05420361
20300      IVFAIL = IVFAIL + 1                                          05430361
           IVCORR = -7                                                  05440361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05450361
 0301      CONTINUE                                                     05460361
CT031*  TEST 31                       VALUES NOT EQUAL, BOTH NEGATIVE   05470361
           IVTNUM = 31                                                  05480361
        IHBVI = -7                                                      05490361
        IHDVI = -5                                                      05500361
        IHAVI = MAX0(IHBVI, IHDVI)                                      05510361
           IF (IHAVI + 5) 20310, 10310, 20310                           05520361
10310      IVPASS = IVPASS + 1                                          05530361
           WRITE (NUVI, 80002) IVTNUM                                   05540361
           GO TO 0311                                                   05550361
20310      IVFAIL = IVFAIL + 1                                          05560361
           IVCORR = -5                                                  05570361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05580361
 0311      CONTINUE                                                     05590361
CT032*  TEST 32   1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   05600361
           IVTNUM = 32                                                  05610361
        IHDVI = 6                                                       05620361
        IHEVI = 0                                                       05630361
        IHAVI = MAX0(IHDVI, -IHEVI)                                     05640361
           IF (IHAVI - 6) 20320, 10320, 20320                           05650361
10320      IVPASS = IVPASS + 1                                          05660361
           WRITE (NUVI, 80002) IVTNUM                                   05670361
           GO TO 0321                                                   05680361
20320      IVFAIL = IVFAIL + 1                                          05690361
           IVCORR = 6                                                   05700361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05710361
 0321      CONTINUE                                                     05720361
CT033*  TEST 33                     EXPRESSIONS PRESENTED TO FUNCTION   05730361
           IVTNUM = 33                                                  05740361
        IHDVI = 3                                                       05750361
        IHEVI = 4                                                       05760361
        IHAVI = MAX0(IHDVI + IHEVI, -IHEVI - IHDVI)                     05770361
           IF (IHAVI - 7) 20330, 10330, 20330                           05780361
10330      IVPASS = IVPASS + 1                                          05790361
           WRITE (NUVI, 80002) IVTNUM                                   05800361
           GO TO 0331                                                   05810361
20330      IVFAIL = IVFAIL + 1                                          05820361
           IVCORR = 7                                                   05830361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05840361
 0331      CONTINUE                                                     05850361
CT034*  TEST 34                                           3 ARGUMENTS   05860361
           IVTNUM = 34                                                  05870361
        IHBVI = 0                                                       05880361
        IHCVI = 3                                                       05890361
        IHDVI = -4                                                      05900361
        IHAVI = MAX0(IHDVI, IHBVI, IHCVI)                               05910361
           IF (IHAVI - 3) 20340, 10340, 20340                           05920361
10340      IVPASS = IVPASS + 1                                          05930361
           WRITE (NUVI, 80002) IVTNUM                                   05940361
           GO TO 0341                                                   05950361
20340      IVFAIL = IVFAIL + 1                                          05960361
           IVCORR = 3                                                   05970361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    05980361
 0341      CONTINUE                                                     05990361
CT035*  TEST 35                                           4 ARGUMENTS   06000361
           IVTNUM = 35                                                  06010361
        IHBVI = -1                                                      06020361
        IHCVI = 0                                                       06030361
        IHDVI = 4                                                       06040361
        IHAVI = MAX0(IHDVI, IHCVI, IHBVI, IHDVI)                        06050361
           IF (IHAVI - 4) 20350, 10350, 20350                           06060361
10350      IVPASS = IVPASS + 1                                          06070361
           WRITE (NUVI, 80002) IVTNUM                                   06080361
           GO TO 0351                                                   06090361
20350      IVFAIL = IVFAIL + 1                                          06100361
           IVCORR = 4                                                   06110361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    06120361
 0351      CONTINUE                                                     06130361
CT036*  TEST 36                                           5 ARGUMENTS   06140361
           IVTNUM = 36                                                  06150361
        IHDVI = 4                                                       06160361
        IHEVI = 5                                                       06170361
        IHAVI = MAX0(IHDVI, -IHDVI, -IHEVI, +IHDVI, IHEVI)              06180361
           IF (IHAVI - 5) 20360, 10360, 20360                           06190361
10360      IVPASS = IVPASS + 1                                          06200361
           WRITE (NUVI, 80002) IVTNUM                                   06210361
           GO TO 0361                                                   06220361
20360      IVFAIL = IVFAIL + 1                                          06230361
           IVCORR = 5                                                   06240361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    06250361
 0361      CONTINUE                                                     06260361
C*****                                                                  06270361
        WRITE (NUVI, 90002)                                             06280361
        WRITE (NUVI, 90013)                                             06290361
        WRITE (NUVI, 90014)                                             06300361
C*****    TEST OF MAX1                                                  06310361
C*****                                                                  06320361
        WRITE(NUVI, 16507)                                              06330361
16507   FORMAT (/ 8X, 12HTEST OF MAX1)                                  06340361
CT037*  TEST 37                                     BOTH VALUES EQUAL   06350361
           IVTNUM = 37                                                  06360361
        RHBVS = 0.0                                                     06370361
        RHDVS = 0.0                                                     06380361
        IHAVI = MAX1(RHBVS, RHDVS)                                      06390361
           IF (IHAVI - 0) 20370, 10370, 20370                           06400361
10370      IVPASS = IVPASS + 1                                          06410361
           WRITE (NUVI, 80002) IVTNUM                                   06420361
           GO TO 0371                                                   06430361
20370      IVFAIL = IVFAIL + 1                                          06440361
           IVCORR = 0                                                   06450361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    06460361
 0371      CONTINUE                                                     06470361
CT038*  TEST 38                     FIRST VALUE NON-ZERO, SECOND ZERO   06480361
           IVTNUM = 38                                                  06490361
        RHBVS = 5.625                                                   06500361
        RHDVS = 0.0                                                     06510361
        IHAVI = MAX1(RHBVS, RHDVS)                                      06520361
           IF (IHAVI - 5) 20380, 10380, 20380                           06530361
10380      IVPASS = IVPASS + 1                                          06540361
           WRITE (NUVI, 80002) IVTNUM                                   06550361
           GO TO 0381                                                   06560361
20380      IVFAIL = IVFAIL + 1                                          06570361
           IVCORR = 5                                                   06580361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    06590361
 0381      CONTINUE                                                     06600361
CT039*  TEST 39                                     BOTH VALUES EQUAL   06610361
           IVTNUM = 39                                                  06620361
        RHBVS = 6.5                                                     06630361
        RHDVS = 6.5                                                     06640361
        IHAVI = MAX1(RHBVS, RHDVS)                                      06650361
           IF (IHAVI - 6) 20390, 10390, 20390                           06660361
10390      IVPASS = IVPASS + 1                                          06670361
           WRITE (NUVI, 80002) IVTNUM                                   06680361
           GO TO 0391                                                   06690361
20390      IVFAIL = IVFAIL + 1                                          06700361
           IVCORR = 6                                                   06710361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    06720361
 0391      CONTINUE                                                     06730361
CT040*  TEST 40                                      VALUES NOT EQUAL   06740361
           IVTNUM = 40                                                  06750361
        RHBVS = 7.125                                                   06760361
        RHDVS = 5.125                                                   06770361
        IHAVI = MAX1(RHBVS, RHDVS)                                      06780361
           IF (IHAVI - 7) 20400, 10400, 20400                           06790361
10400      IVPASS = IVPASS + 1                                          06800361
           WRITE (NUVI, 80002) IVTNUM                                   06810361
           GO TO 0401                                                   06820361
20400      IVFAIL = IVFAIL + 1                                          06830361
           IVCORR = 7                                                   06840361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    06850361
 0401      CONTINUE                                                     06860361
CT041*  TEST 41                     FIRST VALUE NEGATIVE, SECOND ZERO   06870361
           IVTNUM = 41                                                  06880361
        RHBVS = -5.625                                                  06890361
        RHDVS = 0.0                                                     06900361
        IHAVI = MAX1(RHBVS, RHDVS)                                      06910361
           IF (IHAVI - 0) 20410, 10410, 20410                           06920361
10410      IVPASS = IVPASS + 1                                          06930361
           WRITE (NUVI, 80002) IVTNUM                                   06940361
           GO TO 0411                                                   06950361
20410      IVFAIL = IVFAIL + 1                                          06960361
           IVCORR = 0                                                   06970361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    06980361
 0411      CONTINUE                                                     06990361
CT042*  TEST 42                      BOTH VALUES EQUAL, BOTH NEGATIVE   07000361
           IVTNUM = 42                                                  07010361
        RHBVS = - 6.5                                                   07020361
        RHDVS = - 6.5                                                   07030361
        IHAVI = MAX1(RHBVS, RHDVS)                                      07040361
           IF (IHAVI + 6) 20420, 10420, 20420                           07050361
10420      IVPASS = IVPASS + 1                                          07060361
           WRITE (NUVI, 80002) IVTNUM                                   07070361
           GO TO 0421                                                   07080361
20420      IVFAIL = IVFAIL + 1                                          07090361
           IVCORR = -6                                                  07100361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    07110361
 0421      CONTINUE                                                     07120361
CT043*  TEST 43                      VALUES NOT EQUAL,  BOTH NEGATIVE   07130361
           IVTNUM = 43                                                  07140361
        RHBVS = -7.125                                                  07150361
        RHDVS = -5.125                                                  07160361
        IHAVI = MAX1(RHBVS, RHDVS)                                      07170361
           IF (IHAVI + 5) 20430, 10430, 20430                           07180361
10430      IVPASS = IVPASS + 1                                          07190361
           WRITE (NUVI, 80002) IVTNUM                                   07200361
           GO TO 0431                                                   07210361
20430      IVFAIL = IVFAIL + 1                                          07220361
           IVCORR = -5                                                  07230361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    07240361
 0431      CONTINUE                                                     07250361
CT044*  TEST 44 1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY A MINUS SIGN   07260361
           IVTNUM = 44                                                  07270361
        RHDVS = 5.625                                                   07280361
        RHEVS = 0.0                                                     07290361
        IHAVI = MAX1(RHDVS, -RHEVS)                                     07300361
           IF (IHAVI - 5) 20440, 10440, 20440                           07310361
10440      IVPASS = IVPASS + 1                                          07320361
           WRITE (NUVI, 80002) IVTNUM                                   07330361
           GO TO 0441                                                   07340361
20440      IVFAIL = IVFAIL + 1                                          07350361
           IVCORR = 5                                                   07360361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    07370361
 0441      CONTINUE                                                     07380361
CT045*  TEST 45                     EXPRESSIONS PRESENTED TO FUNCTION   07390361
           IVTNUM = 45                                                  07400361
        RHDVS = 3.5                                                     07410361
        RHEVS = 4.0                                                     07420361
        IHAVI = MAX1(RHDVS + RHEVS, -RHEVS - RHDVS)                     07430361
           IF (IHAVI - 7) 20450, 10450, 20450                           07440361
10450      IVPASS = IVPASS + 1                                          07450361
           WRITE (NUVI, 80002) IVTNUM                                   07460361
           GO TO 0451                                                   07470361
20450      IVFAIL = IVFAIL + 1                                          07480361
           IVCORR = 7                                                   07490361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    07500361
 0451      CONTINUE                                                     07510361
CT046*  TEST 46                                           3 ARGUMENTS   07520361
           IVTNUM = 46                                                  07530361
        RHBVS = 0.0                                                     07540361
        RHCVS = 4.0                                                     07550361
        RHDVS = 0.0                                                     07560361
        IHAVI = MAX1(RHBVS, -RHCVS, RHDVS)                              07570361
           IF (IHAVI - 0) 20460, 10460, 20460                           07580361
10460      IVPASS = IVPASS + 1                                          07590361
           WRITE (NUVI, 80002) IVTNUM                                   07600361
           GO TO 0461                                                   07610361
20460      IVFAIL = IVFAIL + 1                                          07620361
           IVCORR = 0                                                   07630361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    07640361
 0461      CONTINUE                                                     07650361
CT047*  TEST 47                                           4 ARGUMENTS   07660361
           IVTNUM = 47                                                  07670361
        RHBVS = 3.49                                                    07680361
        RHCVS = 0.0                                                     07690361
        RHDVS = 3.5                                                     07700361
        IHAVI = MAX1(RHDVS, RHBVS, -RHBVS, RHCVS)                       07710361
           IF (IHAVI - 3) 20470, 10470, 20470                           07720361
10470      IVPASS = IVPASS + 1                                          07730361
           WRITE (NUVI, 80002) IVTNUM                                   07740361
           GO TO 0471                                                   07750361
20470      IVFAIL = IVFAIL + 1                                          07760361
           IVCORR = 3                                                   07770361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    07780361
 0471      CONTINUE                                                     07790361
CT048*  TEST 48                                           5 ARGUMENTS   07800361
           IVTNUM = 48                                                  07810361
        RHDVS = 3.5                                                     07820361
        RHEVS = 4.5                                                     07830361
        IHAVI = MAX1(RHDVS, -RHDVS, -RHEVS, +RHDVS, RHEVS)              07840361
           IF (IHAVI - 4) 20480, 10480, 20480                           07850361
10480      IVPASS = IVPASS + 1                                          07860361
           WRITE (NUVI, 80002) IVTNUM                                   07870361
           GO TO 0481                                                   07880361
20480      IVFAIL = IVFAIL + 1                                          07890361
           IVCORR = 4                                                   07900361
           WRITE (NUVI, 80010) IVTNUM, IHAVI, IVCORR                    07910361
 0481      CONTINUE                                                     07920361
C*****                                                                  07930361
CBB** ********************** BBCSUM0  **********************************07940361
C**** WRITE OUT TEST SUMMARY                                            07950361
C****                                                                   07960361
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07970361
      WRITE (I02, 90004)                                                07980361
      WRITE (I02, 90014)                                                07990361
      WRITE (I02, 90004)                                                08000361
      WRITE (I02, 90020) IVPASS                                         08010361
      WRITE (I02, 90022) IVFAIL                                         08020361
      WRITE (I02, 90024) IVDELE                                         08030361
      WRITE (I02, 90026) IVINSP                                         08040361
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 08050361
CBE** ********************** BBCSUM0  **********************************08060361
CBB** ********************** BBCFOOT0 **********************************08070361
C**** WRITE OUT REPORT FOOTINGS                                         08080361
C****                                                                   08090361
      WRITE (I02,90016) ZPROG, ZPROG                                    08100361
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     08110361
      WRITE (I02,90019)                                                 08120361
CBE** ********************** BBCFOOT0 **********************************08130361
CBB** ********************** BBCFMT0A **********************************08140361
C**** FORMATS FOR TEST DETAIL LINES                                     08150361
C****                                                                   08160361
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           08170361
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           08180361
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           08190361
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           08200361
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           08210361
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    08220361
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08230361
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              08240361
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08250361
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  08260361
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         08270361
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         08280361
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         08290361
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         08300361
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      08310361
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      08320361
80050 FORMAT (1H ,48X,A31)                                              08330361
CBE** ********************** BBCFMT0A **********************************08340361
CBB** ********************** BBCFMT0B **********************************08350361
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                08360361
C****                                                                   08370361
90002 FORMAT (1H1)                                                      08380361
90004 FORMAT (1H )                                                      08390361
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               08400361
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08410361
90008 FORMAT (1H ,21X,A13,A17)                                          08420361
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       08430361
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    08440361
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     08450361
     1       7X,7HREMARKS,24X)                                          08460361
90014 FORMAT (1H ,46H----------------------------------------------,    08470361
     1        33H---------------------------------)                     08480361
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               08490361
C****                                                                   08500361
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             08510361
C****                                                                   08520361
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          08530361
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        08540361
     1        A13)                                                      08550361
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 08560361
C****                                                                   08570361
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 08580361
C****                                                                   08590361
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              08600361
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08610361
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08620361
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08630361
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08640361
CBE** ********************** BBCFMT0B **********************************08650361
C*****                                                                  08660361
C*****    END OF TEST SEGMENT 165                                       08670361
        STOP                                                            08680361
        END                                                             08690361
                                                                        08700361
