C***********************************************************************00010820
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020820
C*****   FM820                                                          00030820
C*****                       YCSIN - (188)                              00040820
C*****                                                                  00050820
C***********************************************************************00060820
C*****  GENERAL PURPOSE                                         ANS REF 00070820
C*****    TEST INTRINSIC FUNCTION CSIN                           15.3   00080820
C*****    INTRINSIC FUNCTION CABS ASSUMED WORKING               TABLE 5 00090820
C*****                                                                  00100820
CBB** ********************** BBCCOMNT **********************************00110820
C****                                                                   00120820
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130820
C****                          VERSION 2.0                              00140820
C****                                                                   00150820
C****                                                                   00160820
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170820
C****                   GENERAL SERVICES ADMINISTRATION                 00180820
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190820
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200820
C****                      FALLS CHURCH, VA. 22041                      00210820
C****                                                                   00220820
C****                          (703) 756-6153                           00230820
C****                                                                   00240820
CBE** ********************** BBCCOMNT **********************************00250820
C*****                                                                  00260820
C*****    S P E C I F I C A T I O N S SEGMENT 188                       00270820
        COMPLEX AVC, BVC, ZVCORR                                        00280820
        REAL R2E(2)                                                     00290820
        EQUIVALENCE (AVC, R2E)                                          00300820
C*****                                                                  00310820
CBB** ********************** BBCINITA **********************************00320820
C**** SPECIFICATION STATEMENTS                                          00330820
C****                                                                   00340820
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350820
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360820
CBE** ********************** BBCINITA **********************************00370820
CBB** ********************** BBCINITB **********************************00380820
C**** INITIALIZE SECTION                                                00390820
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400820
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410820
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420820
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430820
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440820
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450820
      DATA   REMRKS /'                               '/                 00460820
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470820
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480820
C****                                                                   00490820
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500820
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510820
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520820
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590820
      IVPASS = 0                                                        00600820
      IVFAIL = 0                                                        00610820
      IVDELE = 0                                                        00620820
      IVINSP = 0                                                        00630820
      IVTOTL = 0                                                        00640820
      IVTOTN = 0                                                        00650820
      ICZERO = 0                                                        00660820
C                                                                       00670820
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680820
      I01 = 05                                                          00690820
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700820
      I02 = 06                                                          00710820
C                                                                       00720820
      I01 = 5                                                           00730820
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740820
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750820
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760820
C                                                                       00770820
      I02 = 6                                                           00780820
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790820
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800820
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810820
C                                                                       00820820
CBE** ********************** BBCINITB **********************************00830820
      NUVI = I02                                                        00840820
      IVTOTL = 19                                                       00850820
      ZPROG = 'FM820'                                                   00860820
CBB** ********************** BBCHED0A **********************************00870820
C****                                                                   00880820
C**** WRITE REPORT TITLE                                                00890820
C****                                                                   00900820
      WRITE (I02, 90002)                                                00910820
      WRITE (I02, 90006)                                                00920820
      WRITE (I02, 90007)                                                00930820
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940820
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950820
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960820
CBE** ********************** BBCHED0A **********************************00970820
C*****                                                                  00980820
C*****    HEADER FOR SEGMENT 188                                        00990820
        WRITE(NUVI,18800)                                               01000820
18800   FORMAT(1H /35H  YCSIN - (188) INTRINSIC FUNCTIONS//             01010820
     1         36H  CSIN, CCOS  (COMPLEX SINE, COSINE)//                01020820
     2         17H  ANS REF. - 15.3)                                    01030820
CBB** ********************** BBCHED0B **********************************01040820
C**** WRITE DETAIL REPORT HEADERS                                       01050820
C****                                                                   01060820
      WRITE (I02,90004)                                                 01070820
      WRITE (I02,90004)                                                 01080820
      WRITE (I02,90013)                                                 01090820
      WRITE (I02,90014)                                                 01100820
      WRITE (I02,90015) IVTOTL                                          01110820
CBE** ********************** BBCHED0B **********************************01120820
C*****                                                                  01130820
        WRITE(NUVI, 18801)                                              01140820
18801   FORMAT(/ 8X, 12HTEST OF CSIN)                                   01150820
C*****                                                                  01160820
CT001*  TEST 1                                TEST AT ZERO (0.0, 0.0)   01170820
           IVTNUM = 1                                                   01180820
        AVC = CSIN(( 0.0, 0.0))                                         01190820
           IF (R2E(1) + 0.50000E-04) 20010, 40012, 40011                01200820
40011      IF (R2E(1) - 0.50000E-04) 40012, 40012, 20010                01210820
40012      IF (R2E(2) + 0.50000E-04) 20010, 10010, 40010                01220820
40010      IF (R2E(2) - 0.50000E-04) 10010, 10010, 20010                01230820
10010      IVPASS = IVPASS + 1                                          01240820
           WRITE (NUVI, 80002) IVTNUM                                   01250820
           GO TO 0011                                                   01260820
20010      IVFAIL = IVFAIL + 1                                          01270820
           ZVCORR = (0.00000000000000, 0.00000000000000)                01280820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01290820
 0011      CONTINUE                                                     01300820
CT002*  TEST 2            TEST SIN ON THE REAL LINE, CSIN SAME AS SIN   01310820
           IVTNUM = 2                                                   01320820
        AVC = CSIN(( 2.0, 0.0))                                         01330820
           IF (R2E(1) - 0.90925E+00) 20020, 40022, 40021                01340820
40021      IF (R2E(1) - 0.90935E+00) 40022, 40022, 20020                01350820
40022      IF (R2E(2) + 0.50000E-04) 20020, 10020, 40020                01360820
40020      IF (R2E(2) - 0.50000E-04) 10020, 10020, 20020                01370820
10020      IVPASS = IVPASS + 1                                          01380820
           WRITE (NUVI, 80002) IVTNUM                                   01390820
           GO TO 0021                                                   01400820
20020      IVFAIL = IVFAIL + 1                                          01410820
           ZVCORR = (0.90929742682568, 0.00000000000000)                01420820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01430820
 0021      CONTINUE                                                     01440820
CT003*  TEST 3            TEST SIN ON THE REAL LINE, CSIN SAME AS SIN   01450820
           IVTNUM = 3                                                   01460820
        AVC = CSIN(( -1000.0, 0.0))                                     01470820
           IF (R2E(1) + 0.82692E+00) 20030, 40032, 40031                01480820
40031      IF (R2E(1) + 0.82683E+00) 40032, 40032, 20030                01490820
40032      IF (R2E(2) + 0.50000E-04) 20030, 10030, 40030                01500820
40030      IF (R2E(2) - 0.50000E-04) 10030, 10030, 20030                01510820
10030      IVPASS = IVPASS + 1                                          01520820
           WRITE (NUVI, 80002) IVTNUM                                   01530820
           GO TO 0031                                                   01540820
20030      IVFAIL = IVFAIL + 1                                          01550820
           ZVCORR = (-0.82687954053200, 0.00000000000000)               01560820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01570820
 0031      CONTINUE                                                     01580820
CT004*  TEST 4                           EXPRESSION PRESENTED TO CSIN   01590820
           IVTNUM = 4                                                   01600820
        AVC = CSIN(( 150.0, 350.0) / (100.0, 0.0))                      01610820
           IF (R2E(1) - 0.16530E+02) 20040, 40042, 40041                01620820
40041      IF (R2E(1) - 0.16533E+02) 40042, 40042, 20040                01630820
40042      IF (R2E(2) - 0.11701E+01) 20040, 10040, 40040                01640820
40040      IF (R2E(2) - 0.11703E+01) 10040, 10040, 20040                01650820
10040      IVPASS = IVPASS + 1                                          01660820
           WRITE (NUVI, 80002) IVTNUM                                   01670820
           GO TO 0041                                                   01680820
20040      IVFAIL = IVFAIL + 1                                          01690820
           ZVCORR = (16.531309523248, 1.1701791625591)                  01700820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01710820
 0041      CONTINUE                                                     01720820
CT005*  TEST 5                             VARIABLE PRESENTED TO CSIN   01730820
           IVTNUM = 5                                                   01740820
        BVC = ( 4.75, 2.50) - (9.50, 1.25)                              01750820
        AVC = CSIN(BVC)                                                 01760820
           IF (R2E(1) - 0.18870E+01) 20050, 40052, 40051                01770820
40051      IF (R2E(1) - 0.18872E+01) 40052, 40052, 20050                01780820
40052      IF (R2E(2) - 0.60232E-01) 20050, 10050, 40050                01790820
40050      IF (R2E(2) - 0.60239E-01) 10050, 10050, 20050                01800820
10050      IVPASS = IVPASS + 1                                          01810820
           WRITE (NUVI, 80002) IVTNUM                                   01820820
           GO TO 0051                                                   01830820
20050      IVFAIL = IVFAIL + 1                                          01840820
           ZVCORR = (1.8870883629759, 0.060235606171638)                01850820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01860820
 0051      CONTINUE                                                     01870820
CT006*  TEST 6                             VARIABLE PRESENTED TO CSIN   01880820
           IVTNUM = 6                                                   01890820
        BVC = ( 0.125, 2.0) * (10.0, 0.0)                               01900820
        AVC = CSIN(BVC)                                                 01910820
           IF (R2E(1) - 0.23019E+09) 20060, 40062, 40061                01920820
40061      IF (R2E(1) - 0.23022E+09) 40062, 40062, 20060                01930820
40062      IF (R2E(2) - 0.76487E+08) 20060, 10060, 40060                01940820
40060      IF (R2E(2) - 0.76496E+08) 10060, 10060, 20060                01950820
10060      IVPASS = IVPASS + 1                                          01960820
           WRITE (NUVI, 80002) IVTNUM                                   01970820
           GO TO 0061                                                   01980820
20060      IVFAIL = IVFAIL + 1                                          01990820
           ZVCORR = (230207154.14527, 76491717.784289)                  02000820
           WRITE (NUVI, 80145) IVTNUM, AVC, ZVCORR                      02010820
80145 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED: ,           02020820
     1        1H(,E12.5,2H, ,E12.5,1H)/,1H ,16X,10HCORRECT:  ,          02030820
     2        1H(,E12.5,2H, ,E12.5,1H))                                 02040820
 0061      CONTINUE                                                     02050820
CT007*  TEST 7                                TEST WHERE REAL IS ZERO   02060820
           IVTNUM = 7                                                   02070820
        BVC = ( 0.0, 1.0)                                               02080820
        AVC = CSIN(BVC)                                                 02090820
           IF (R2E(1) + 0.50000E-04) 20070, 40072, 40071                02100820
40071      IF (R2E(1) - 0.50000E-04) 40072, 40072, 20070                02110820
40072      IF (R2E(2) - 0.11751E+01) 20070, 10070, 40070                02120820
40070      IF (R2E(2) - 0.11753E+01) 10070, 10070, 20070                02130820
10070      IVPASS = IVPASS + 1                                          02140820
           WRITE (NUVI, 80002) IVTNUM                                   02150820
           GO TO 0071                                                   02160820
20070      IVFAIL = IVFAIL + 1                                          02170820
           ZVCORR = (0.00000000000000, 1.1752011936438)                 02180820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02190820
 0071      CONTINUE                                                     02200820
CT008*  TEST 8                                TEST WHERE REAL IS ZERO   02210820
           IVTNUM = 8                                                   02220820
        BVC = ( 0.0, -4.75)                                             02230820
        AVC = CSIN(BVC)                                                 02240820
           IF (R2E(1) + 0.50000E-04) 20080, 40082, 40081                02250820
40081      IF (R2E(1) - 0.50000E-04) 40082, 40082, 20080                02260820
40082      IF (R2E(2) + 0.57791E+02) 20080, 10080, 40080                02270820
40080      IF (R2E(2) + 0.57785E+02) 10080, 10080, 20080                02280820
10080      IVPASS = IVPASS + 1                                          02290820
           WRITE (NUVI, 80002) IVTNUM                                   02300820
           GO TO 0081                                                   02310820
20080      IVFAIL = IVFAIL + 1                                          02320820
           ZVCORR = (0.00000000000000, -57.787816415992)                02330820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02340820
 0081      CONTINUE                                                     02350820
CT009*  TEST 9                                TEST WHERE REAL IS ZERO   02360820
           IVTNUM = 9                                                   02370820
        AVC = CSIN(( 0.0, -10.0))                                       02380820
           IF (R2E(1) + 0.50000E-04) 20090, 40092, 40091                02390820
40091      IF (R2E(1) - 0.50000E-04) 40092, 40092, 20090                02400820
40092      IF (R2E(2) + 0.11014E+05) 20090, 10090, 40090                02410820
40090      IF (R2E(2) + 0.11012E+05) 10090, 10090, 20090                02420820
10090      IVPASS = IVPASS + 1                                          02430820
           WRITE (NUVI, 80002) IVTNUM                                   02440820
           GO TO 0091                                                   02450820
20090      IVFAIL = IVFAIL + 1                                          02460820
           ZVCORR = (0.00000000000000, -11013.232874703)                02470820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02480820
 0091      CONTINUE                                                     02490820
C*****                                                                  02500820
        WRITE (NUVI, 90002)                                             02510820
        WRITE (NUVI, 90013)                                             02520820
        WRITE (NUVI, 90014)                                             02530820
C*****                                                                  02540820
        WRITE(NUVI, 18811)                                              02550820
18811   FORMAT(/ 08X, 12HTEST OF CCOS)                                  02560820
CT010*  TEST 10                              TEST FOR ZERO (0.0, 0.0)   02570820
           IVTNUM = 10                                                  02580820
        AVC = CCOS(( 0.0, 0.0))                                         02590820
           IF (R2E(1) - 0.99995E+00) 20100, 40102, 40101                02600820
40101      IF (R2E(1) - 0.10001E+01) 40102, 40102, 20100                02610820
40102      IF (R2E(2) + 0.50000E-04) 20100, 10100, 40100                02620820
40100      IF (R2E(2) - 0.50000E-04) 10100, 10100, 20100                02630820
10100      IVPASS = IVPASS + 1                                          02640820
           WRITE (NUVI, 80002) IVTNUM                                   02650820
           GO TO 0101                                                   02660820
20100      IVFAIL = IVFAIL + 1                                          02670820
           ZVCORR = (1.00000000000000, 0.00000000000000)                02680820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02690820
 0101      CONTINUE                                                     02700820
CT011*  TEST 11                 TEST WITH ZERO IMAGINARY,  CCOS = COS   02710820
           IVTNUM = 11                                                  02720820
        AVC = CCOS((3.5, 1.0) - (0.0, 1.0))                             02730820
           IF (R2E(1) + 0.93651E+00) 20110, 40112, 40111                02740820
40111      IF (R2E(1) + 0.93641E+00) 40112, 40112, 20110                02750820
40112      IF (R2E(2) + 0.50000E-04) 20110, 10110, 40110                02760820
40110      IF (R2E(2) - 0.50000E-04) 10110, 10110, 20110                02770820
10110      IVPASS = IVPASS + 1                                          02780820
           WRITE (NUVI, 80002) IVTNUM                                   02790820
           GO TO 0111                                                   02800820
20110      IVFAIL = IVFAIL + 1                                          02810820
           ZVCORR = (-0.93645668729080, 0.00000000000000)               02820820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02830820
 0111      CONTINUE                                                     02840820
CT012*  TEST 12                          EXPRESSION PRESENTED TO CCOS   02850820
           IVTNUM = 12                                                  02860820
        AVC = CCOS(( 3.1416, 0.0) * (-10000.0, 0.0))                    02870820
           IF (R2E(1) - 0.99725E+00) 20120, 40122, 40121                02880820
40121      IF (R2E(1) - 0.99736E+00) 40122, 40122, 20120                02890820
40122      IF (R2E(2) + 0.50000E-04) 20120, 10120, 40120                02900820
40120      IF (R2E(2) - 0.50000E-04) 10120, 10120, 20120                02910820
10120      IVPASS = IVPASS + 1                                          02920820
           WRITE (NUVI, 80002) IVTNUM                                   02930820
           GO TO 0121                                                   02940820
20120      IVFAIL = IVFAIL + 1                                          02950820
           ZVCORR = (0.99730272627420, 0.00000000000000)                02960820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02970820
70121      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       02970*TI
           WRITE (NUVI,70121)                                           02970*TI
 0121      CONTINUE                                                     02980820
CT013*  TEST 13                          EXPRESSION PRESENTED TO CCOS   02990820
           IVTNUM = 13                                                  03000820
        AVC = CCOS(( 3.5, 5.5) - (2.0, 2.0))                            03010820
           IF (R2E(1) - 0.11722E+01) 20130, 40132, 40131                03020820
40131      IF (R2E(1) - 0.11724E+01) 40132, 40132, 20130                03030820
40132      IF (R2E(2) + 0.16502E+02) 20130, 10130, 40130                03040820
40130      IF (R2E(2) + 0.16500E+02) 10130, 10130, 20130                03050820
10130      IVPASS = IVPASS + 1                                          03060820
           WRITE (NUVI, 80002) IVTNUM                                   03070820
           GO TO 0131                                                   03080820
20130      IVFAIL = IVFAIL + 1                                          03090820
           ZVCORR = (1.1723152409601, -16.501187784675)                 03100820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03110820
 0131      CONTINUE                                                     03120820
CT014*  TEST 14                         VARIABLE WITHIN AN EXPRESSION   03130820
           IVTNUM = 14                                                  03140820
        BVC = ( 4.75, 1.25)                                             03150820
        AVC = CCOS(BVC - (9.50, 0.0))                                   03160820
           IF (R2E(1) - 0.71005E-01) 20140, 40142, 40141                03170820
40141      IF (R2E(1) - 0.71013E-01) 40142, 40142, 20140                03180820
40142      IF (R2E(2) + 0.16009E+01) 20140, 10140, 40140                03190820
40140      IF (R2E(2) + 0.16007E+01) 10140, 10140, 20140                03200820
10140      IVPASS = IVPASS + 1                                          03210820
           WRITE (NUVI, 80002) IVTNUM                                   03220820
           GO TO 0141                                                   03230820
20140      IVFAIL = IVFAIL + 1                                          03240820
           ZVCORR = (0.071008803346314, -1.6007861854666)               03250820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03260820
 0141      CONTINUE                                                     03270820
CT015*  TEST 15                         VARIABLE WITHIN AN EXPRESSION   03280820
           IVTNUM = 15                                                  03290820
        BVC = ( 1.00, 10.0)                                             03300820
        AVC = CCOS(BVC + ( 0.25, 10.0))                                 03310820
           IF (R2E(1) - 0.76487E+08) 20150, 40152, 40151                03320820
40151      IF (R2E(1) - 0.76496E+08) 40152, 40152, 20150                03330820
40152      IF (R2E(2) + 0.23022E+09) 20150, 10150, 40150                03340820
40150      IF (R2E(2) + 0.23019E+09) 10150, 10150, 20150                03350820
10150      IVPASS = IVPASS + 1                                          03360820
           WRITE (NUVI, 80002) IVTNUM                                   03370820
           GO TO 0151                                                   03380820
20150      IVFAIL = IVFAIL + 1                                          03390820
           ZVCORR = (76491717.784289, -230207154.14527)                 03400820
           WRITE (NUVI, 80145) IVTNUM, AVC, ZVCORR                      03410820
 0151      CONTINUE                                                     03420820
CT016*  TEST 16                              TEST WITH ZERO REAL PART   03430820
           IVTNUM = 16                                                  03440820
        BVC = ( 0.0, 1.0)                                               03450820
        AVC = CCOS(BVC)                                                 03460820
           IF (R2E(1) - 0.15430E+01) 20160, 40162, 40161                03470820
40161      IF (R2E(1) - 0.15432E+01) 40162, 40162, 20160                03480820
40162      IF (R2E(2) + 0.50000E-04) 20160, 10160, 40160                03490820
40160      IF (R2E(2) - 0.50000E-04) 10160, 10160, 20160                03500820
10160      IVPASS = IVPASS + 1                                          03510820
           WRITE (NUVI, 80002) IVTNUM                                   03520820
           GO TO 0161                                                   03530820
20160      IVFAIL = IVFAIL + 1                                          03540820
           ZVCORR = (1.5430806348152, 0.00000000000000)                 03550820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03560820
 0161      CONTINUE                                                     03570820
CT017*  TEST 17                              TEST WITH ZERO REAL PART   03580820
           IVTNUM = 17                                                  03590820
        BVC = ( 0.0, -4.75)                                             03600820
        AVC = CCOS(BVC)                                                 03610820
           IF (R2E(1) - 0.57793E+02) 20170, 40172, 40171                03620820
40171      IF (R2E(1) - 0.57800E+02) 40172, 40172, 20170                03630820
40172      IF (R2E(2) + 0.50000E-04) 20170, 10170, 40170                03640820
40170      IF (R2E(2) - 0.50000E-04) 10170, 10170, 20170                03650820
10170      IVPASS = IVPASS + 1                                          03660820
           WRITE (NUVI, 80002) IVTNUM                                   03670820
           GO TO 0171                                                   03680820
20170      IVFAIL = IVFAIL + 1                                          03690820
           ZVCORR = (57.796468111195, 0.00000000000000)                 03700820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03710820
 0171      CONTINUE                                                     03720820
CT018*  TEST 18                              TEST WITH ZERO REAL PART   03730820
           IVTNUM = 18                                                  03740820
        AVC = CCOS(( 0.0, -10.0))                                       03750820
           IF (R2E(1) - 0.11012E+05) 20180, 40182, 40181                03760820
40181      IF (R2E(1) - 0.11014E+05) 40182, 40182, 20180                03770820
40182      IF (R2E(2) + 0.50000E-04) 20180, 10180, 40180                03780820
40180      IF (R2E(2) - 0.50000E-04) 10180, 10180, 20180                03790820
10180      IVPASS = IVPASS + 1                                          03800820
           WRITE (NUVI, 80002) IVTNUM                                   03810820
           GO TO 0181                                                   03820820
20180      IVFAIL = IVFAIL + 1                                          03830820
           ZVCORR = (11013.232920103, 0.00000000000000)                 03840820
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03850820
 0181      CONTINUE                                                     03860820
CT019*  TEST 19              THE FUNCTION TOGETHER WITH CSIN AND CABS   03870820
           IVTNUM = 19                                                  03880820
        DVS = (CABS(CCOS((-2.25, 0.0))) ** 2) +                         03890820
     1        (CABS(CSIN((-2.25, 0.0))) ** 2)                           03900820
           IF (DVS - 0.99995E+00) 20190, 10190, 40190                   03910820
40190      IF (DVS - 0.10001E+01) 10190, 10190, 20190                   03920820
10190      IVPASS = IVPASS + 1                                          03930820
           WRITE (NUVI, 80002) IVTNUM                                   03940820
           GO TO 0191                                                   03950820
20190      IVFAIL = IVFAIL + 1                                          03960820
           RVCORR = 1.00000000000000                                    03970820
           WRITE (NUVI, 80012) IVTNUM, DVS, RVCORR                      03980820
 0191      CONTINUE                                                     03990820
C*****                                                                  04000820
CBB** ********************** BBCSUM0  **********************************04010820
C**** WRITE OUT TEST SUMMARY                                            04020820
C****                                                                   04030820
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04040820
      WRITE (I02, 90004)                                                04050820
      WRITE (I02, 90014)                                                04060820
      WRITE (I02, 90004)                                                04070820
      WRITE (I02, 90020) IVPASS                                         04080820
      WRITE (I02, 90022) IVFAIL                                         04090820
      WRITE (I02, 90024) IVDELE                                         04100820
      WRITE (I02, 90026) IVINSP                                         04110820
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04120820
CBE** ********************** BBCSUM0  **********************************04130820
CBB** ********************** BBCFOOT0 **********************************04140820
C**** WRITE OUT REPORT FOOTINGS                                         04150820
C****                                                                   04160820
      WRITE (I02,90016) ZPROG, ZPROG                                    04170820
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04180820
      WRITE (I02,90019)                                                 04190820
CBE** ********************** BBCFOOT0 **********************************04200820
CBB** ********************** BBCFMT0A **********************************04210820
C**** FORMATS FOR TEST DETAIL LINES                                     04220820
C****                                                                   04230820
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04240820
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04250820
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04260820
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04270820
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04280820
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04290820
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04300820
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04310820
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04320820
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04330820
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04340820
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04350820
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04360820
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04370820
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04380820
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04390820
80050 FORMAT (1H ,48X,A31)                                              04400820
CBE** ********************** BBCFMT0A **********************************04410820
CBB** ********************** BBCFMAT1 **********************************04420820
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04430820
C****                                                                   04440820
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04450820
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04460820
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04470820
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04480820
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04490820
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04500820
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04510820
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04520820
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04530820
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04540820
     21H(,F12.5,2H, ,F12.5,1H))                                         04550820
CBE** ********************** BBCFMAT1 **********************************04560820
CBB** ********************** BBCFMT0B **********************************04570820
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04580820
C****                                                                   04590820
90002 FORMAT (1H1)                                                      04600820
90004 FORMAT (1H )                                                      04610820
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04620820
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04630820
90008 FORMAT (1H ,21X,A13,A17)                                          04640820
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04650820
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04660820
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04670820
     1       7X,7HREMARKS,24X)                                          04680820
90014 FORMAT (1H ,46H----------------------------------------------,    04690820
     1        33H---------------------------------)                     04700820
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04710820
C****                                                                   04720820
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04730820
C****                                                                   04740820
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04750820
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04760820
     1        A13)                                                      04770820
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04780820
C****                                                                   04790820
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04800820
C****                                                                   04810820
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04820820
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04830820
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04840820
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04850820
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04860820
CBE** ********************** BBCFMT0B **********************************04870820
C*****                                                                  04880820
C*****  END OF TEST SEGMENT 188                                         04890820
      STOP                                                              04900820
      END                                                               04910820
                                                                        04920820
