C***********************************************************************00010906
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020906
C*****   FM906                                                          00030906
C*****                       LSTDI2 - (372)                             00040906
C*****                                                                  00050906
C***********************************************************************00060906
C*****  GENERAL PURPOSE                                         ANS REF 00070906
C*****    TEST LIST DIRECTED INPUT                              13.6    00080906
C*****    DOUBLE PRECISION, COMPLEX DATA TYPES INCLUDED         12.4    00090906
C*****                                                                  00100906
CBB** ********************** BBCCOMNT **********************************00110906
C****                                                                   00120906
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130906
C****                          VERSION 2.0                              00140906
C****                                                                   00150906
C****                                                                   00160906
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170906
C****                   GENERAL SERVICES ADMINISTRATION                 00180906
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190906
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200906
C****                      FALLS CHURCH, VA. 22041                      00210906
C****                                                                   00220906
C****                          (703) 756-6153                           00230906
C****                                                                   00240906
CBE** ********************** BBCCOMNT **********************************00250906
C*****                                                                  00260906
C  INPUT DATA TO THIS SEGMENT CONSISTS OF 12 CARD IMAGES IN COL. 1-44   00270906
COL.      1-----------------------------------------44                  00280906
CARD 1    2.5D0                                                         00290906
CARD 2    1.5  2.5D0  3.5E0                                             00300906
CARD 3    (3.0,4.0)                                                     00310906
CARD 4    (1.0,0.0)  (0.0,0.0)  (0.0,3.0)                               00320906
CARD 5    2, 2.5D0, 2.5D0, T, (3.0,4.0), 'TEST'                         00330906
CARD 6    ( 2.5 , 3.5 )                                                 00340906
CARD 7    (1.0        ,                                                 00350906
CARD 8       2.0)                                                       00360906
CARD 9    , (2.0, 3.0),,6.0D0, 2*,                                      00370906
CARD 10   1.0D0  (2.0, 2.0)  3.0D0  (4.0, 4.0)  5.0D0                   00380906
CARD 11   6.0D0  (7.0, 7.0) / 8.0D0  (9.0, 9.0) 10.0D0                  00390906
CARD 12   2.0D0 4.0D0 / 6.0D0 8.0D0 10.0D0                              00400906
C*****                                                                  00410906
C*****  S P E C I F I C A T I O N S  SEGMENT 372                        00420906
        LOGICAL AVB                                                     00430906
        CHARACTER A4VK*4,CVCORR*4                                       00440906
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00450906
        DOUBLE PRECISION A1D(4)                                         00460906
        COMPLEX AVC, BVC, CVC, ZVCORR                                   00470906
        REAL R2E(6)                                                     00480906
        EQUIVALENCE (AVC,R2E(1)),(BVC,R2E(3)),(CVC,R2E(5))              00490906
C*****                                                                  00500906
CBB** ********************** BBCINITA **********************************00510906
C**** SPECIFICATION STATEMENTS                                          00520906
C****                                                                   00530906
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00540906
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00550906
CBE** ********************** BBCINITA **********************************00560906
CBB** ********************** BBCINITB **********************************00570906
C**** INITIALIZE SECTION                                                00580906
      DATA  ZVERS,                  ZVERSD,             ZDATE           00590906
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00600906
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00610906
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00620906
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00630906
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00640906
      DATA   REMRKS /'                               '/                 00650906
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00660906
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00670906
C****                                                                   00680906
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00690906
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00700906
CZ03  ZPROG  = 'PROGRAM NAME'                                           00710906
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00780906
      IVPASS = 0                                                        00790906
      IVFAIL = 0                                                        00800906
      IVDELE = 0                                                        00810906
      IVINSP = 0                                                        00820906
      IVTOTL = 0                                                        00830906
      IVTOTN = 0                                                        00840906
      ICZERO = 0                                                        00850906
C                                                                       00860906
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00870906
      I01 = 05                                                          00880906
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00890906
      I02 = 06                                                          00900906
C                                                                       00910906
      I01 = 5                                                           00920906
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00930906
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00940906
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00950906
C                                                                       00960906
      I02 = 6                                                           00970906
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00980906
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00990906
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01000906
C                                                                       01010906
CBE** ********************** BBCINITB **********************************01020906
      IRVI = I01                                                        01030906
      NUVI = I02                                                        01040906
      IVTOTL = 28                                                       01050906
      ZPROG = 'FM906'                                                   01060906
CBB** ********************** BBCHED0A **********************************01070906
C****                                                                   01080906
C**** WRITE REPORT TITLE                                                01090906
C****                                                                   01100906
      WRITE (I02, 90002)                                                01110906
      WRITE (I02, 90006)                                                01120906
      WRITE (I02, 90007)                                                01130906
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01140906
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01150906
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01160906
CBE** ********************** BBCHED0A **********************************01170906
C*****                                                                  01180906
C*****    HEADING FOR SEGMENT 372                                       01190906
        WRITE(NUVI,37200)                                               01200906
37200   FORMAT(1H , /16H LSTDI2 - (372) ,                               01210906
     1         20H LIST DIRECTED INPUT,                                 01220906
     2         32H FOR D.P. AND COMPLEX DATA TYPES//                    01230906
     3         22H ANS REF. - 13.6  12.4)                               01240906
CBB** ********************** BBCHED0B **********************************01250906
C**** WRITE DETAIL REPORT HEADERS                                       01260906
C****                                                                   01270906
      WRITE (I02,90004)                                                 01280906
      WRITE (I02,90004)                                                 01290906
      WRITE (I02,90013)                                                 01300906
      WRITE (I02,90014)                                                 01310906
      WRITE (I02,90015) IVTOTL                                          01320906
CBE** ********************** BBCHED0B **********************************01330906
CT001*  TEST 1 - CARD 1    DOUBLE PRECISION                             01340906
           IVTNUM = 1                                                   01350906
        READ(IRVI, *) AVD                                               01360906
           IF (AVD - 0.2499999998D+01) 20010, 10010, 40010              01370906
40010      IF (AVD - 0.2500000002D+01) 10010, 10010, 20010              01380906
10010      IVPASS = IVPASS + 1                                          01390906
           WRITE (NUVI, 80002) IVTNUM                                   01400906
           GO TO 0011                                                   01410906
20010      IVFAIL = IVFAIL + 1                                          01420906
           DVCORR = 2.5D0                                               01430906
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01440906
 0011      CONTINUE                                                     01450906
C*****  TESTS 2 THRU 4 - CARD 2    SEVERAL DOUBLE PRECISION             01460906
CT002*  TEST 2                                                          01470906
           IVTNUM = 2                                                   01480906
        READ(IRVI, *) AVD, BVD, CVD                                     01490906
           IF (AVD - 0.1499999999D+01) 20020, 10020, 40020              01500906
40020      IF (AVD - 0.1500000001D+01) 10020, 10020, 20020              01510906
10020      IVPASS = IVPASS + 1                                          01520906
           WRITE (NUVI, 80002) IVTNUM                                   01530906
           GO TO 0021                                                   01540906
20020      IVFAIL = IVFAIL + 1                                          01550906
           DVCORR = 1.5D0                                               01560906
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01570906
 0021      CONTINUE                                                     01580906
CT003*  TEST 3                                                          01590906
           IVTNUM = 3                                                   01600906
           IF (BVD - 0.2499999998D+01) 20030, 10030, 40030              01610906
40030      IF (BVD - 0.2500000002D+01) 10030, 10030, 20030              01620906
10030      IVPASS = IVPASS + 1                                          01630906
           WRITE (NUVI, 80002) IVTNUM                                   01640906
           GO TO 0031                                                   01650906
20030      IVFAIL = IVFAIL + 1                                          01660906
           DVCORR = 2.5D0                                               01670906
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      01680906
 0031      CONTINUE                                                     01690906
CT004*  TEST 4                                                          01700906
           IVTNUM = 4                                                   01710906
           IF (CVD - 0.3499999998D+01) 20040, 10040, 40040              01720906
40040      IF (CVD - 0.3500000002D+01) 10040, 10040, 20040              01730906
10040      IVPASS = IVPASS + 1                                          01740906
           WRITE (NUVI, 80002) IVTNUM                                   01750906
           GO TO 0041                                                   01760906
20040      IVFAIL = IVFAIL + 1                                          01770906
           DVCORR = 3.5D0                                               01780906
           WRITE (NUVI, 80031) IVTNUM, CVD, DVCORR                      01790906
 0041      CONTINUE                                                     01800906
CT005*  TEST 5 - CARD 3    COMPLEX                                      01810906
           IVTNUM = 5                                                   01820906
        READ(IRVI, *) AVC                                               01830906
           IF (R2E(1) - 0.29998E+01) 20050, 40052, 40051                01840906
40051      IF (R2E(1) - 0.30002E+01) 40052, 40052, 20050                01850906
40052      IF (R2E(2) - 0.39998E+01) 20050, 10050, 40050                01860906
40050      IF (R2E(2) - 0.40002E+01) 10050, 10050, 20050                01870906
10050      IVPASS = IVPASS + 1                                          01880906
           WRITE (NUVI, 80002) IVTNUM                                   01890906
           GO TO 0051                                                   01900906
20050      IVFAIL = IVFAIL + 1                                          01910906
           ZVCORR = (3.0, 4.0)                                          01920906
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01930906
 0051      CONTINUE                                                     01940906
C*****  TESTS 6 THRU 8 - CARD 4    SEVERAL COMPLEX                      01950906
CT006*  TEST 6                                                          01960906
           IVTNUM = 6                                                   01970906
        READ(IRVI, *) AVC, BVC, CVC                                     01980906
           IF (R2E(1) - 0.99995E+00) 20060, 40062, 40061                01990906
40061      IF (R2E(1) - 0.10001E+01) 40062, 40062, 20060                02000906
40062      IF (R2E(2) + 0.50000E-04) 20060, 10060, 40060                02010906
40060      IF (R2E(2) - 0.50000E-04) 10060, 10060, 20060                02020906
10060      IVPASS = IVPASS + 1                                          02030906
           WRITE (NUVI, 80002) IVTNUM                                   02040906
           GO TO 0061                                                   02050906
20060      IVFAIL = IVFAIL + 1                                          02060906
           ZVCORR = (1.0, 0.0)                                          02070906
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02080906
 0061      CONTINUE                                                     02090906
CT007*  TEST 7                                                          02100906
           IVTNUM = 7                                                   02110906
           IF (R2E(3) + 0.50000E-04) 20070, 40072, 40071                02120906
40071      IF (R2E(3) - 0.50000E-04) 40072, 40072, 20070                02130906
40072      IF (R2E(4) + 0.50000E-04) 20070, 10070, 40070                02140906
40070      IF (R2E(4) - 0.50000E-04) 10070, 10070, 20070                02150906
10070      IVPASS = IVPASS + 1                                          02160906
           WRITE (NUVI, 80002) IVTNUM                                   02170906
           GO TO 0071                                                   02180906
20070      IVFAIL = IVFAIL + 1                                          02190906
           ZVCORR = (0.0, 0.0)                                          02200906
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      02210906
 0071      CONTINUE                                                     02220906
CT008*  TEST 8                                                          02230906
           IVTNUM = 8                                                   02240906
           IF (R2E(5) + 0.50000E-04) 20080, 40082, 40081                02250906
40081      IF (R2E(5) - 0.50000E-04) 40082, 40082, 20080                02260906
40082      IF (R2E(6) - 0.29998E+01) 20080, 10080, 40080                02270906
40080      IF (R2E(6) - 0.30002E+01) 10080, 10080, 20080                02280906
10080      IVPASS = IVPASS + 1                                          02290906
           WRITE (NUVI, 80002) IVTNUM                                   02300906
           GO TO 0081                                                   02310906
20080      IVFAIL = IVFAIL + 1                                          02320906
           ZVCORR = (0.0, 3.0)                                          02330906
           WRITE (NUVI, 80045) IVTNUM, CVC, ZVCORR                      02340906
 0081      CONTINUE                                                     02350906
C*****  TESTS 9 THRU 14 - CARD 5    MIXED LIST                          02360906
CT009*  TEST 9                                                          02370906
           IVTNUM = 9                                                   02380906
        READ(IRVI, *) IVI, AVD, AVS, AVB, AVC, A4VK                     02390906
           IF (IVI - 2) 20090, 10090, 20090                             02400906
10090      IVPASS = IVPASS + 1                                          02410906
           WRITE (NUVI, 80002) IVTNUM                                   02420906
           GO TO 0091                                                   02430906
20090      IVFAIL = IVFAIL + 1                                          02440906
           IVCORR = 2                                                   02450906
           WRITE (NUVI, 80010) IVTNUM, IVI, IVCORR                      02460906
 0091      CONTINUE                                                     02470906
CT010*  TEST 10                                                         02480906
           IVTNUM = 10                                                  02490906
           IF (AVD - 0.2499999998D+01) 20100, 10100, 40100              02500906
40100      IF (AVD - 0.2500000002D+01) 10100, 10100, 20100              02510906
10100      IVPASS = IVPASS + 1                                          02520906
           WRITE (NUVI, 80002) IVTNUM                                   02530906
           GO TO 0101                                                   02540906
20100      IVFAIL = IVFAIL + 1                                          02550906
           DVCORR = 2.5D0                                               02560906
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02570906
 0101      CONTINUE                                                     02580906
CT011*  TEST 11                                                         02590906
           IVTNUM = 11                                                  02600906
           IF (AVS - 0.24998E+01) 20110, 10110, 40110                   02610906
40110      IF (AVS - 0.25002E+01) 10110, 10110, 20110                   02620906
10110      IVPASS = IVPASS + 1                                          02630906
           WRITE (NUVI, 80002) IVTNUM                                   02640906
           GO TO 0111                                                   02650906
20110      IVFAIL = IVFAIL + 1                                          02660906
           RVCORR = 2.5                                                 02670906
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02680906
 0111      CONTINUE                                                     02690906
CT012*  TEST 12                                                         02700906
           IVTNUM = 12                                                  02710906
           IVCOMP = 0                                                   02720906
           IF (AVB) IVCOMP = 1                                          02730906
           IF (IVCOMP - 1) 20120, 10120, 20120                          02740906
10120      IVPASS = IVPASS + 1                                          02750906
           WRITE (NUVI, 80002) IVTNUM                                   02760906
           GO TO 0121                                                   02770906
20120      IVFAIL = IVFAIL + 1                                          02780906
           LVCORR = 1                                                   02790906
           REMRKS = '1 = TRUE ;  0 = FALSE'                             02800906
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           02810906
           WRITE (NUVI, 80024) IVCOMP                                   02820906
           WRITE (NUVI, 80026) LVCORR                                   02830906
 0121      CONTINUE                                                     02840906
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                        02850906
        WRITE (NUVI, 90002)                                             02860906
        WRITE (NUVI, 90013)                                             02870906
        WRITE (NUVI, 90014)                                             02880906
CT013*  TEST 13                                                         02890906
           IVTNUM = 13                                                  02900906
           IF (R2E(1) - 0.29998E+01) 20130, 40132, 40131                02910906
40131      IF (R2E(1) - 0.30002E+01) 40132, 40132, 20130                02920906
40132      IF (R2E(2) - 0.39998E+01) 20130, 10130, 40130                02930906
40130      IF (R2E(2) - 0.40002E+01) 10130, 10130, 20130                02940906
10130      IVPASS = IVPASS + 1                                          02950906
           WRITE (NUVI, 80002) IVTNUM                                   02960906
           GO TO 0131                                                   02970906
20130      IVFAIL = IVFAIL + 1                                          02980906
           ZVCORR = (3.0, 4.0)                                          02990906
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03000906
 0131      CONTINUE                                                     03010906
CT014*  TEST 14                                                         03020906
           IVTNUM = 14                                                  03030906
           IVCOMP = 0                                                   03040906
           IF (A4VK.EQ.'TEST') IVCOMP = 1                               03050906
           IF (IVCOMP - 1) 20140, 10140, 20140                          03060906
10140      IVPASS = IVPASS + 1                                          03070906
           WRITE (NUVI, 80002) IVTNUM                                   03080906
           GO TO 0141                                                   03090906
20140      IVFAIL = IVFAIL + 1                                          03100906
           CVCORR = 'TEST'                                              03110906
           WRITE (NUVI, 80018) IVTNUM, A4VK, CVCORR                     03120906
 0141      CONTINUE                                                     03130906
CT015*  TEST 15 - CARD 6    COMPLEX CONSTANT W/EMBEDDED BLANKS          03140906
           IVTNUM = 15                                                  03150906
        READ(IRVI, *) AVC                                               03160906
           IF (R2E(1) - 0.24998E+01) 20150, 40152, 40151                03170906
40151      IF (R2E(1) - 0.25002E+01) 40152, 40152, 20150                03180906
40152      IF (R2E(2) - 0.34998E+01) 20150, 10150, 40150                03190906
40150      IF (R2E(2) - 0.35002E+01) 10150, 10150, 20150                03200906
10150      IVPASS = IVPASS + 1                                          03210906
           WRITE (NUVI, 80002) IVTNUM                                   03220906
           GO TO 0151                                                   03230906
20150      IVFAIL = IVFAIL + 1                                          03240906
           ZVCORR = (2.5, 3.5)                                          03250906
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03260906
 0151      CONTINUE                                                     03270906
CT016*  TEST 16 - CARDS 7-8   COMPLEX WITH EMBEDDED END-OF-RECORD       03280906
           IVTNUM = 16                                                  03290906
        READ(IRVI, *) AVC                                               03300906
           IF (R2E(1) - 0.99995E+00) 20160, 40162, 40161                03310906
40161      IF (R2E(1) - 0.10001E+01) 40162, 40162, 20160                03320906
40162      IF (R2E(2) - 0.19999E+01) 20160, 10160, 40160                03330906
40160      IF (R2E(2) - 0.20001E+01) 10160, 10160, 20160                03340906
10160      IVPASS = IVPASS + 1                                          03350906
           WRITE (NUVI, 80002) IVTNUM                                   03360906
           GO TO 0161                                                   03370906
20160      IVFAIL = IVFAIL + 1                                          03380906
           ZVCORR = (1.0, 2.0)                                          03390906
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03400906
 0161      CONTINUE                                                     03410906
C*****  TESTS 17 THRU 22 - CARD 9    NULL VALUES                        03420906
CT017*  TEST 17                                                         03430906
           IVTNUM = 17                                                  03440906
        AVD = 1.0D0                                                     03450906
        BVC = (4.0, 5.0)                                                03460906
        CVC = (7.0, 8.0)                                                03470906
        CVD = 9.0D0                                                     03480906
        READ(IRVI, *) AVD, AVC, BVC, BVD, CVC, CVD                      03490906
           IF (AVD - 0.9999999995D+00) 20170, 10170, 40170              03500906
40170      IF (AVD - 0.1000000001D+01) 10170, 10170, 20170              03510906
10170      IVPASS = IVPASS + 1                                          03520906
           WRITE (NUVI, 80002) IVTNUM                                   03530906
           GO TO 0171                                                   03540906
20170      IVFAIL = IVFAIL + 1                                          03550906
           DVCORR = 1.0D0                                               03560906
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03570906
 0171      CONTINUE                                                     03580906
CT018*  TEST 18                                                         03590906
           IVTNUM = 18                                                  03600906
           IF (R2E(1) - 0.19999E+01) 20180, 40182, 40181                03610906
40181      IF (R2E(1) - 0.20001E+01) 40182, 40182, 20180                03620906
40182      IF (R2E(2) - 0.29998E+01) 20180, 10180, 40180                03630906
40180      IF (R2E(2) - 0.30002E+01) 10180, 10180, 20180                03640906
10180      IVPASS = IVPASS + 1                                          03650906
           WRITE (NUVI, 80002) IVTNUM                                   03660906
           GO TO 0181                                                   03670906
20180      IVFAIL = IVFAIL + 1                                          03680906
           ZVCORR = (2.0, 3.0)                                          03690906
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03700906
 0181      CONTINUE                                                     03710906
CT019*  TEST 19                                                         03720906
           IVTNUM = 19                                                  03730906
           IF (R2E(3) - 0.39998E+01) 20190, 40192, 40191                03740906
40191      IF (R2E(3) - 0.40002E+01) 40192, 40192, 20190                03750906
40192      IF (R2E(4) - 0.49997E+01) 20190, 10190, 40190                03760906
40190      IF (R2E(4) - 0.50003E+01) 10190, 10190, 20190                03770906
10190      IVPASS = IVPASS + 1                                          03780906
           WRITE (NUVI, 80002) IVTNUM                                   03790906
           GO TO 0191                                                   03800906
20190      IVFAIL = IVFAIL + 1                                          03810906
           ZVCORR = (4.0, 5.0)                                          03820906
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      03830906
 0191      CONTINUE                                                     03840906
CT020*  TEST 20                                                         03850906
           IVTNUM = 20                                                  03860906
           IF (BVD - 0.5999999997D+01) 20200, 10200, 40200              03870906
40200      IF (BVD - 0.6000000003D+01) 10200, 10200, 20200              03880906
10200      IVPASS = IVPASS + 1                                          03890906
           WRITE (NUVI, 80002) IVTNUM                                   03900906
           GO TO 0201                                                   03910906
20200      IVFAIL = IVFAIL + 1                                          03920906
           DVCORR = 6.0D0                                               03930906
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03940906
 0201      CONTINUE                                                     03950906
CT021*  TEST 21                                                         03960906
           IVTNUM = 21                                                  03970906
           IF (R2E(5) - 0.69996E+01) 20210, 40212, 40211                03980906
40211      IF (R2E(5) - 0.70004E+01) 40212, 40212, 20210                03990906
40212      IF (R2E(6) - 0.79996E+01) 20210, 10210, 40210                04000906
40210      IF (R2E(6) - 0.80004E+01) 10210, 10210, 20210                04010906
10210      IVPASS = IVPASS + 1                                          04020906
           WRITE (NUVI, 80002) IVTNUM                                   04030906
           GO TO 0211                                                   04040906
20210      IVFAIL = IVFAIL + 1                                          04050906
           ZVCORR = (7.0, 8.0)                                          04060906
           WRITE (NUVI, 80045) IVTNUM, CVC, ZVCORR                      04070906
 0211      CONTINUE                                                     04080906
CT022*  TEST 22                                                         04090906
           IVTNUM = 22                                                  04100906
           IF (CVD - 0.8999999995D+01) 20220, 10220, 40220              04110906
40220      IF (CVD - 0.9000000005D+01) 10220, 10220, 20220              04120906
10220      IVPASS = IVPASS + 1                                          04130906
           WRITE (NUVI, 80002) IVTNUM                                   04140906
           GO TO 0221                                                   04150906
20220      IVFAIL = IVFAIL + 1                                          04160906
           DVCORR = 9.0D0                                               04170906
           WRITE (NUVI, 80031) IVTNUM, CVD, DVCORR                      04180906
 0221      CONTINUE                                                     04190906
C*****  TESTS 23 THRU 27 - CARDS 10-11    SLASH TERMINATOR              04200906
CT023*  TEST 23                                                         04210906
           IVTNUM = 23                                                  04220906
        READ(IRVI, *) AVD, AVC, BVD, BVC, CVD                           04230906
        READ(IRVI, *) AVD, AVC, BVD, BVC, CVD                           04240906
           IF (AVD - 0.5999999997D+01) 20230, 10230, 40230              04250906
40230      IF (AVD - 0.6000000003D+01) 10230, 10230, 20230              04260906
10230      IVPASS = IVPASS + 1                                          04270906
           WRITE (NUVI, 80002) IVTNUM                                   04280906
           GO TO 0231                                                   04290906
20230      IVFAIL = IVFAIL + 1                                          04300906
           DVCORR = 6.0D0                                               04310906
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      04320906
 0231      CONTINUE                                                     04330906
CT024*  TEST 24                                                         04340906
           IVTNUM = 24                                                  04350906
           IF (R2E(1) - 0.69996E+01) 20240, 40242, 40241                04360906
40241      IF (R2E(1) - 0.70004E+01) 40242, 40242, 20240                04370906
40242      IF (R2E(2) - 0.69996E+01) 20240, 10240, 40240                04380906
40240      IF (R2E(2) - 0.70004E+01) 10240, 10240, 20240                04390906
10240      IVPASS = IVPASS + 1                                          04400906
           WRITE (NUVI, 80002) IVTNUM                                   04410906
           GO TO 0241                                                   04420906
20240      IVFAIL = IVFAIL + 1                                          04430906
           ZVCORR = (7.0, 7.0)                                          04440906
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      04450906
 0241      CONTINUE                                                     04460906
CT025*  TEST 25                                                         04470906
           IVTNUM = 25                                                  04480906
           IF (BVD - 0.2999999998D+01) 20250, 10250, 40250              04490906
40250      IF (BVD - 0.3000000002D+01) 10250, 10250, 20250              04500906
10250      IVPASS = IVPASS + 1                                          04510906
           WRITE (NUVI, 80002) IVTNUM                                   04520906
           GO TO 0251                                                   04530906
20250      IVFAIL = IVFAIL + 1                                          04540906
           DVCORR = 3.0D0                                               04550906
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      04560906
 0251      CONTINUE                                                     04570906
CT026*  TEST 26                                                         04580906
           IVTNUM = 26                                                  04590906
           IF (R2E(3) - 0.39998E+01) 20260, 40262, 40261                04600906
40261      IF (R2E(3) - 0.40002E+01) 40262, 40262, 20260                04610906
40262      IF (R2E(4) - 0.39998E+01) 20260, 10260, 40260                04620906
40260      IF (R2E(4) - 0.40002E+01) 10260, 10260, 20260                04630906
10260      IVPASS = IVPASS + 1                                          04640906
           WRITE (NUVI, 80002) IVTNUM                                   04650906
           GO TO 0261                                                   04660906
20260      IVFAIL = IVFAIL + 1                                          04670906
           ZVCORR = (4.0, 4.0)                                          04680906
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      04690906
 0261      CONTINUE                                                     04700906
CT027*  TEST 27                                                         04710906
           IVTNUM = 27                                                  04720906
           IF (CVD - 0.4999999997D+01) 20270, 10270, 40270              04730906
40270      IF (CVD - 0.5000000003D+01) 10270, 10270, 20270              04740906
10270      IVPASS = IVPASS + 1                                          04750906
           WRITE (NUVI, 80002) IVTNUM                                   04760906
           GO TO 0271                                                   04770906
20270      IVFAIL = IVFAIL + 1                                          04780906
           DVCORR = 5.0D0                                               04790906
           WRITE (NUVI, 80031) IVTNUM, CVD, DVCORR                      04800906
 0271      CONTINUE                                                     04810906
CT028*  TEST 28                                                         04820906
           IVTNUM = 28                                                  04830906
        A1D(3) = 3.0D0                                                  04840906
        READ(IRVI, *) (A1D(IVI), IVI=1,4)                               04850906
           IF (A1D(3) - 0.2999999998D+01) 20280, 10280, 40280           04860906
40280      IF (A1D(3) - 0.3000000002D+01) 10280, 10280, 20280           04870906
10280      IVPASS = IVPASS + 1                                          04880906
           WRITE (NUVI, 80002) IVTNUM                                   04890906
           GO TO 0281                                                   04900906
20280      IVFAIL = IVFAIL + 1                                          04910906
           DVCORR = 3.0D0                                               04920906
           WRITE (NUVI, 80031) IVTNUM, A1D(3), DVCORR                   04930906
 0281      CONTINUE                                                     04940906
CBB** ********************** BBCSUM0  **********************************04950906
C**** WRITE OUT TEST SUMMARY                                            04960906
C****                                                                   04970906
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04980906
      WRITE (I02, 90004)                                                04990906
      WRITE (I02, 90014)                                                05000906
      WRITE (I02, 90004)                                                05010906
      WRITE (I02, 90020) IVPASS                                         05020906
      WRITE (I02, 90022) IVFAIL                                         05030906
      WRITE (I02, 90024) IVDELE                                         05040906
      WRITE (I02, 90026) IVINSP                                         05050906
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 05060906
CBE** ********************** BBCSUM0  **********************************05070906
CBB** ********************** BBCFOOT0 **********************************05080906
C**** WRITE OUT REPORT FOOTINGS                                         05090906
C****                                                                   05100906
      WRITE (I02,90016) ZPROG, ZPROG                                    05110906
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     05120906
      WRITE (I02,90019)                                                 05130906
CBE** ********************** BBCFOOT0 **********************************05140906
CBB** ********************** BBCFMT0A **********************************05150906
C**** FORMATS FOR TEST DETAIL LINES                                     05160906
C****                                                                   05170906
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           05180906
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           05190906
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           05200906
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           05210906
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           05220906
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    05230906
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05240906
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              05250906
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05260906
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  05270906
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         05280906
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         05290906
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         05300906
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         05310906
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      05320906
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      05330906
80050 FORMAT (1H ,48X,A31)                                              05340906
CBE** ********************** BBCFMT0A **********************************05350906
CBB** ********************** BBCFMAT1 **********************************05360906
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     05370906
C****                                                                   05380906
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05390906
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            05400906
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     05410906
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     05420906
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05430906
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05440906
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05450906
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05460906
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05470906
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  05480906
     21H(,F12.5,2H, ,F12.5,1H))                                         05490906
CBE** ********************** BBCFMAT1 **********************************05500906
CBB** ********************** BBCFMT0B **********************************05510906
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                05520906
C****                                                                   05530906
90002 FORMAT (1H1)                                                      05540906
90004 FORMAT (1H )                                                      05550906
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               05560906
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05570906
90008 FORMAT (1H ,21X,A13,A17)                                          05580906
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       05590906
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    05600906
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     05610906
     1       7X,7HREMARKS,24X)                                          05620906
90014 FORMAT (1H ,46H----------------------------------------------,    05630906
     1        33H---------------------------------)                     05640906
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               05650906
C****                                                                   05660906
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             05670906
C****                                                                   05680906
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          05690906
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        05700906
     1        A13)                                                      05710906
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 05720906
C****                                                                   05730906
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 05740906
C****                                                                   05750906
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              05760906
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              05770906
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             05780906
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  05790906
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  05800906
CBE** ********************** BBCFMT0B **********************************05810906
C*****                                                                  05820906
C*****    END OF TEST SEGMENT 372                                       05830906
        STOP                                                            05840906
        END                                                             05850906
