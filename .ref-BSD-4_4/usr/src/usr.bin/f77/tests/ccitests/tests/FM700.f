      PROGRAM FM700                                                     00010700
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020700
C     THIS PROGRAM TESTS THE DATA STATEMENT WITH           ANS REF.     00030700
C          VARIABLE NAMES, ARRAY NAMES, ARRAY ELEMENT      9.1          00040700
C          NAMES, SUBSTRING NAMES, AND IMPLIED-DO LISTS.   9.2          00050700
C                                                          9.3          00060700
C     SYMBOLIC NAMES OF CONSTANTS ARE PERMITTED IN THE                  00070700
C          CLIST OF THE DATA STATEMENT.   IF NECESSARY,                 00080700
C          THE CLIST CONSTANT IS CONVERTED TO THE TYPE                  00090700
C          OF THE NLIST ENTITY ACCORDING TO THE RULES                   00100700
C          FOR ARITHMETIC CONVERSION.                                   00110700
C                                                                       00120700
C                                                                       00130700
CBB** ********************** BBCCOMNT **********************************00140700
C****                                                                   00150700
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00160700
C****                          VERSION 2.0                              00170700
C****                                                                   00180700
C****                                                                   00190700
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00200700
C****                   GENERAL SERVICES ADMINISTRATION                 00210700
C****                   FEDERAL SOFTWARE TESTING CENTER                 00220700
C****                   5203 LEESBURG PIKE, SUITE 1100                  00230700
C****                      FALLS CHURCH, VA. 22041                      00240700
C****                                                                   00250700
C****                          (703) 756-6153                           00260700
C****                                                                   00270700
CBE** ********************** BBCCOMNT **********************************00280700
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00290700
           IMPLICIT CHARACTER*27 (C)                                    00300700
CBB** ********************** BBCINITA **********************************00310700
C**** SPECIFICATION STATEMENTS                                          00320700
C****                                                                   00330700
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00340700
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00350700
CBE** ********************** BBCINITA **********************************00360700
C                                                                       00370700
      INTEGER I2N001(2,3), I2N002(7), I2N003(3,7)                       00380700
      INTEGER I2N004(3,10), I2N005(4,5), I2N006(6,8)                    00390700
      CHARACTER CVCOMP*25, CVCORR*25, CPN001*5                          00400700
      CHARACTER CVN001*25, CVN002*5, C1N001(3)*5                        00410700
      CHARACTER C2N001(3,4)*4, CVN003*17                                00420700
      REAL R2E001(2), R2N001(5,3)                                       00430700
      DOUBLE PRECISION DVCOMP, DVCORR, DVN001, D1N001(9), DPN001        00440700
      COMPLEX ZVCOMP, ZVCORR, ZVN001, Z1N001(10)                        00450700
      PARAMETER (IPN001=-14, CPN001='SEVEN', IPN002=5, DPN001=0.1948D+3)00460700
      EQUIVALENCE (ZVCOMP, R2E001)                                      00470700
      DATA IVN001, C1N001, I2N001(2,1), CVN001(11:22)                   00480700
     1     /-137, 'FIRST', 'SECND', 'THIRD', 65, 'ELEVENTWELVE'/        00490700
      DATA (I2N001(1,I), I=1,3) /-47, 198, -217/                        00500700
      DATA IVN002, CVN002 /IPN001, CPN001/                              00510700
      DATA I2N002, (I2N003(I,7), I=1,3), C2N001, CVN003(13:16)          00520700
     1     /3*19, 7*-4, 13*'SAME'/                                      00530700
      DATA IVN003, IVN004, RVN001, ZVN001, DVN001, DVN002               00540700
     1     /-0.473E+3, 239.2D-1, 71, (71, -27), 6, 9.1534E-2/           00550700
      DATA (I2N004(2,J), J=1,10) /9,8,7,6,5,4,3,2,1,0/                  00560700
      DATA ((R2N001(I,J), J=1,3), I=3,5)                                00570700
     1     /3.1, 3.2, 3.3, 4.1, 4.2, 4.3, 5.1, 5.2, 5.3/                00580700
      DATA (Z1N001(I), I=3,7) /IPN002*(7.3, -2.28)/                     00590700
      DATA (D1N001(I), I=1,9,2) /IPN002*DPN001/                         00600700
      DATA (I2N005(I,I+1),I=1,4) / 91, -82, 73, -64/                    00610700
      DATA ((I2N006(2*I,I*J-1), I=2,3), J=1,3,2) /41, 62, 45, 68/       00620700
C                                                                       00630700
C                                                                       00640700
CBB** ********************** BBCINITB **********************************00650700
C**** INITIALIZE SECTION                                                00660700
      DATA  ZVERS,                  ZVERSD,             ZDATE           00670700
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00680700
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00690700
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00700700
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00710700
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00720700
      DATA   REMRKS /'                               '/                 00730700
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00740700
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00750700
C****                                                                   00760700
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00770700
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00780700
CZ03  ZPROG  = 'PROGRAM NAME'                                           00790700
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00860700
      IVPASS = 0                                                        00870700
      IVFAIL = 0                                                        00880700
      IVDELE = 0                                                        00890700
      IVINSP = 0                                                        00900700
      IVTOTL = 0                                                        00910700
      IVTOTN = 0                                                        00920700
      ICZERO = 0                                                        00930700
C                                                                       00940700
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00950700
      I01 = 05                                                          00960700
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00970700
      I02 = 06                                                          00980700
C                                                                       00990700
      I01 = 5                                                           01000700
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01010700
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01020700
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01030700
C                                                                       01040700
      I02 = 6                                                           01050700
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01060700
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01070700
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01080700
C                                                                       01090700
CBE** ********************** BBCINITB **********************************01100700
           ZPROG = 'FM700'                                              01110700
           IVTOTL = 23                                                  01120700
CBB** ********************** BBCHED0A **********************************01130700
C****                                                                   01140700
C**** WRITE REPORT TITLE                                                01150700
C****                                                                   01160700
      WRITE (I02, 90002)                                                01170700
      WRITE (I02, 90006)                                                01180700
      WRITE (I02, 90007)                                                01190700
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01200700
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01210700
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01220700
CBE** ********************** BBCHED0A **********************************01230700
CBB** ********************** BBCHED0B **********************************01240700
C**** WRITE DETAIL REPORT HEADERS                                       01250700
C****                                                                   01260700
      WRITE (I02,90004)                                                 01270700
      WRITE (I02,90004)                                                 01280700
      WRITE (I02,90013)                                                 01290700
      WRITE (I02,90014)                                                 01300700
      WRITE (I02,90015) IVTOTL                                          01310700
CBE** ********************** BBCHED0B **********************************01320700
C                                                                       01330700
C                                                                       01340700
C          TESTS 1 THRU 5 TEST DATA STATEMENT WITH VARIABLE NAMES,      01350700
C     ARRAY NAMES, ARRAY ELEMENT NAMES, SUBSTRING NAMES, AND IMPLIED-   01360700
C     DO LISTS.                                                         01370700
C                                                                       01380700
CT001*  TEST 001   ****  FCVS PROGRAM 700  *****                        01390700
C     VARIABLE NAME                                                     01400700
C                                                                       01410700
           IVTNUM = 1                                                   01420700
           IVCOMP = 0                                                   01430700
           IVCORR = -137                                                01440700
      IVCOMP = IVN001                                                   01450700
40010      IF (IVCOMP + 137) 20010, 10010, 20010                        01460700
10010      IVPASS = IVPASS + 1                                          01470700
           WRITE (I02, 80002) IVTNUM                                    01480700
           GO TO 0011                                                   01490700
20010      IVFAIL = IVFAIL + 1                                          01500700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    01510700
 0011      CONTINUE                                                     01520700
C                                                                       01530700
CT002*  TEST 002   ****  FCVS PROGRAM 700  *****                        01540700
C     ARRAY NAME                                                        01550700
C                                                                       01560700
           IVTNUM = 2                                                   01570700
           CVCOMP = ' '                                                 01580700
           CVCORR = 'SECND'                                             01590700
      CVCOMP = C1N001(2)                                                01600700
           IVCOMP = 0                                                   01610700
           IF (CVCOMP.EQ.'SECND') IVCOMP = 1                            01620700
40020      IF (IVCOMP - 1) 20020, 10020, 20020                          01630700
10020      IVPASS = IVPASS + 1                                          01640700
           WRITE (I02, 80002) IVTNUM                                    01650700
           GO TO 0021                                                   01660700
20020      IVFAIL = IVFAIL + 1                                          01670700
           WRITE (I02, 80018) IVTNUM, CVCOMP, CVCORR                    01680700
 0021      CONTINUE                                                     01690700
C                                                                       01700700
CT003*  TEST 003   ****  FCVS PROGRAM 700  *****                        01710700
C     ARRAY ELEMENT NAME                                                01720700
C                                                                       01730700
           IVTNUM = 3                                                   01740700
           IVCOMP = 0                                                   01750700
           IVCORR = 65                                                  01760700
      IVCOMP = I2N001(2,1)                                              01770700
40030      IF (IVCOMP - 65) 20030, 10030, 20030                         01780700
10030      IVPASS = IVPASS + 1                                          01790700
           WRITE (I02, 80002) IVTNUM                                    01800700
           GO TO 0031                                                   01810700
20030      IVFAIL = IVFAIL + 1                                          01820700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    01830700
 0031      CONTINUE                                                     01840700
C                                                                       01850700
CT004*  TEST 004   ****  FCVS PROGRAM 700  *****                        01860700
C     SUBSTRING NAME                                                    01870700
C                                                                       01880700
           IVTNUM = 4                                                   01890700
           CVCOMP = ' '                                                 01900700
           CVCORR = 'ELEVENTWELVE'                                      01910700
      CVCOMP = CVN001(11:22)                                            01920700
           IVCOMP = 0                                                   01930700
           IF (CVCOMP.EQ.'ELEVENTWELVE') IVCOMP = 1                     01940700
40040      IF (IVCOMP - 1) 20040, 10040, 20040                          01950700
10040      IVPASS = IVPASS + 1                                          01960700
           WRITE (I02, 80002) IVTNUM                                    01970700
           GO TO 0041                                                   01980700
20040      IVFAIL = IVFAIL + 1                                          01990700
           WRITE (I02, 80018) IVTNUM, CVCOMP, CVCORR                    02000700
 0041      CONTINUE                                                     02010700
C                                                                       02020700
CT005*  TEST 005   ****  FCVS PROGRAM 700  *****                        02030700
C     IMPLIED-DO LIST                                                   02040700
C                                                                       02050700
           IVTNUM = 5                                                   02060700
           IVCOMP = 0                                                   02070700
           IVCORR = -217                                                02080700
      IVCOMP = I2N001(1,3)                                              02090700
40050      IF (IVCOMP + 217) 20050, 10050, 20050                        02100700
10050      IVPASS = IVPASS + 1                                          02110700
           WRITE (I02, 80002) IVTNUM                                    02120700
           GO TO 0051                                                   02130700
20050      IVFAIL = IVFAIL + 1                                          02140700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    02150700
 0051      CONTINUE                                                     02160700
C                                                                       02170700
CT006*  TEST 006   ****  FCVS PROGRAM 700  *****                        02180700
C     CLIST CONTAINS A SYMBOLIC NAME OF AN INTEGER CONSTANT             02190700
C                                                                       02200700
           IVTNUM = 6                                                   02210700
           IVCOMP = 0                                                   02220700
           IVCORR = -14                                                 02230700
      IVCOMP = IVN002                                                   02240700
40060      IF (IVCOMP + 14) 20060, 10060, 20060                         02250700
10060      IVPASS = IVPASS + 1                                          02260700
           WRITE (I02, 80002) IVTNUM                                    02270700
           GO TO 0061                                                   02280700
20060      IVFAIL = IVFAIL + 1                                          02290700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    02300700
 0061      CONTINUE                                                     02310700
C                                                                       02320700
CT007*  TEST 007   ****  FCVS PROGRAM 700  *****                        02330700
C     CLIST CONTAINS A SYMBOLIC NAME OF A CHARACTER CONSTANT            02340700
C                                                                       02350700
           IVTNUM = 7                                                   02360700
           CVCOMP = ' '                                                 02370700
           CVCORR = 'SEVEN'                                             02380700
      CVCOMP = CVN002                                                   02390700
           IVCOMP = 0                                                   02400700
           IF (CVCOMP.EQ.'SEVEN') IVCOMP = 1                            02410700
40070      IF (IVCOMP - 1) 20070, 10070, 20070                          02420700
10070      IVPASS = IVPASS + 1                                          02430700
           WRITE (I02, 80002) IVTNUM                                    02440700
           GO TO 0071                                                   02450700
20070      IVFAIL = IVFAIL + 1                                          02460700
           WRITE (I02, 80018) IVTNUM, CVCOMP, CVCORR                    02470700
 0071      CONTINUE                                                     02480700
C                                                                       02490700
C          TESTS 8 THRU 11 TEST COMBINATIONS OF SUBSTRING NAMES AND     02500700
C     ARRAY NAMES AND THE R*C FORMAT OF THE CLIST                       02510700
C                                                                       02520700
CT008*  TEST 008   ****  FCVS PROGRAM 700  *****                        02530700
C                                                                       02540700
           IVTNUM = 8                                                   02550700
           IVCOMP = 0                                                   02560700
           IVCORR = 23                                                  02570700
      IVCOMP = I2N002(3) - I2N002(4)                                    02580700
40080      IF (IVCOMP - 23) 20080, 10080, 20080                         02590700
10080      IVPASS = IVPASS + 1                                          02600700
           WRITE (I02, 80002) IVTNUM                                    02610700
           GO TO 0081                                                   02620700
20080      IVFAIL = IVFAIL + 1                                          02630700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    02640700
 0081      CONTINUE                                                     02650700
C                                                                       02660700
CT009*  TEST 009   ****  FCVS PROGRAM 700  *****                        02670700
C                                                                       02680700
           IVTNUM = 9                                                   02690700
           IVCOMP = 0                                                   02700700
           IVCORR = -4                                                  02710700
      DO 0092 I = 1, 3                                                  02720700
      IF (I2N003(I,7) + 4) 0093, 0092, 0093                             02730700
0092  CONTINUE                                                          02740700
0093  IVCOMP = I2N003(3,7)                                              02750700
40090      IF (IVCOMP + 4) 20090, 10090, 20090                          02760700
10090      IVPASS = IVPASS + 1                                          02770700
           WRITE (I02, 80002) IVTNUM                                    02780700
           GO TO 0091                                                   02790700
20090      IVFAIL = IVFAIL + 1                                          02800700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    02810700
 0091      CONTINUE                                                     02820700
C                                                                       02830700
CT010*  TEST 010   ****  FCVS PROGRAM 700  *****                        02840700
C                                                                       02850700
           IVTNUM = 10                                                  02860700
           CVCOMP = ' '                                                 02870700
           CVCORR = 'SAME'                                              02880700
      DO 0102 I = 1, 3                                                  02890700
      DO 0102 J = 1, 4                                                  02900700
      IF (C2N001(I,J).NE.'SAME') GO TO 0103                             02910700
0102  CONTINUE                                                          02920700
0103  CVCOMP = C2N001(3,4)                                              02930700
           IVCOMP = 0                                                   02940700
           IF (CVCOMP.EQ.'SAME') IVCOMP = 1                             02950700
40100      IF (IVCOMP - 1) 20100, 10100, 20100                          02960700
10100      IVPASS = IVPASS + 1                                          02970700
           WRITE (I02, 80002) IVTNUM                                    02980700
           GO TO 0101                                                   02990700
20100      IVFAIL = IVFAIL + 1                                          03000700
           WRITE (I02, 80018) IVTNUM, CVCOMP, CVCORR                    03010700
 0101      CONTINUE                                                     03020700
C                                                                       03030700
CT011*  TEST 011   ****  FCVS PROGRAM 700  *****                        03040700
C                                                                       03050700
           IVTNUM = 11                                                  03060700
           CVCOMP = ' '                                                 03070700
           CVCORR = 'SAME'                                              03080700
      CVCOMP = CVN003(13:16)                                            03090700
           IVCOMP = 0                                                   03100700
           IF (CVCOMP.EQ.'SAME') IVCOMP = 1                             03110700
40110      IF (IVCOMP - 1) 20110, 10110, 20110                          03120700
10110      IVPASS = IVPASS + 1                                          03130700
           WRITE (I02, 80002) IVTNUM                                    03140700
           GO TO 0111                                                   03150700
20110      IVFAIL = IVFAIL + 1                                          03160700
           WRITE (I02, 80018) IVTNUM, CVCOMP, CVCORR                    03170700
 0111      CONTINUE                                                     03180700
C                                                                       03190700
C          TESTS 12 THRU 17 TEST ARITHMETIC CONVERSION OF CLIST         03200700
C     CONSTANTS TO THE TYPE OF THE CORRESPONDING NLIST ENTITIES         03210700
C                                                                       03220700
CT012*  TEST 012   ****  FCVS PROGRAM 700  *****                        03230700
C     REAL TO INTEGER                                                   03240700
C                                                                       03250700
           IVTNUM = 12                                                  03260700
           IVCOMP = 0                                                   03270700
           IVCORR =  -473                                               03280700
      IVCOMP = IVN003                                                   03290700
40120      IF (IVCOMP + 473) 20120, 10120, 20120                        03300700
10120      IVPASS = IVPASS + 1                                          03310700
           WRITE (I02, 80002) IVTNUM                                    03320700
           GO TO 0121                                                   03330700
20120      IVFAIL = IVFAIL + 1                                          03340700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    03350700
 0121      CONTINUE                                                     03360700
C                                                                       03370700
CT013*  TEST 013   ****  FCVS PROGRAM 700  *****                        03380700
C     DOUBLE PRECISION TO INTEGER                                       03390700
C                                                                       03400700
           IVTNUM = 13                                                  03410700
           IVCOMP = 0                                                   03420700
           IVCORR = 23                                                  03430700
      IVCOMP = IVN004                                                   03440700
40130      IF (IVCOMP - 23) 20130, 10130, 20130                         03450700
10130      IVPASS = IVPASS + 1                                          03460700
           WRITE (I02, 80002) IVTNUM                                    03470700
           GO TO 0131                                                   03480700
20130      IVFAIL = IVFAIL + 1                                          03490700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    03500700
 0131      CONTINUE                                                     03510700
C                                                                       03520700
CT014*  TEST 014   ****  FCVS PROGRAM 700  *****                        03530700
C     INTEGER TO REAL                                                   03540700
C                                                                       03550700
           IVTNUM = 14                                                  03560700
           RVCOMP = 0.0                                                 03570700
           RVCORR = 71.0                                                03580700
      RVCOMP = RVN001                                                   03590700
           IF (RVCOMP - 0.70996E+02) 20140, 10140, 40140                03600700
40140      IF (RVCOMP - 0.71004E+02) 10140, 10140, 20140                03610700
10140      IVPASS = IVPASS + 1                                          03620700
           WRITE (I02, 80002) IVTNUM                                    03630700
           GO TO 0141                                                   03640700
20140      IVFAIL = IVFAIL + 1                                          03650700
           WRITE (I02, 80012) IVTNUM, RVCOMP, RVCORR                    03660700
 0141      CONTINUE                                                     03670700
C                                                                       03680700
CT015*  TEST 015   ****  FCVS PROGRAM 700  *****                        03690700
C     COMPLEX                                                           03700700
C                                                                       03710700
           IVTNUM = 15                                                  03720700
           ZVCOMP = (0.0, 0.0)                                          03730700
           ZVCORR = (71.0, -27.0)                                       03740700
      ZVCOMP = ZVN001                                                   03750700
           IF (R2E001(1) - 0.70996E+02) 20150, 40152, 40151             03760700
40151      IF (R2E001(1) - 0.71004E+02) 40152, 40152, 20150             03770700
40152      IF (R2E001(2) + 0.27002E+02) 20150, 10150, 40150             03780700
40150      IF (R2E001(2) + 0.26998E+02) 10150, 10150, 20150             03790700
10150      IVPASS = IVPASS + 1                                          03800700
           WRITE (I02, 80002) IVTNUM                                    03810700
           GO TO 0151                                                   03820700
20150      IVFAIL = IVFAIL + 1                                          03830700
           WRITE (I02, 80045) IVTNUM, ZVCOMP, ZVCORR                    03840700
 0151      CONTINUE                                                     03850700
C                                                                       03860700
CT016*  TEST 016   ****  FCVS PROGRAM 700  *****                        03870700
C     INTEGER TO DOUBLE PRECISION                                       03880700
C                                                                       03890700
           IVTNUM = 16                                                  03900700
           DVCOMP = 0.0D0                                               03910700
           DVCORR = 6.0D0                                               03920700
      DVCOMP = DVN001                                                   03930700
           IF (DVCOMP - 0.5999999997D+01) 20160, 10160, 40160           03940700
40160      IF (DVCOMP - 0.6000000003D+01) 10160, 10160, 20160           03950700
10160      IVPASS = IVPASS + 1                                          03960700
           WRITE (I02, 80002) IVTNUM                                    03970700
           GO TO 0161                                                   03980700
20160      IVFAIL = IVFAIL + 1                                          03990700
           WRITE (I02, 80031) IVTNUM, DVCOMP, DVCORR                    04000700
 0161      CONTINUE                                                     04010700
C                                                                       04020700
CT017*  TEST 017   ****  FCVS PROGRAM 700  *****                        04030700
C     REAL TO DOUBLE PRECISION                                          04040700
C                                                                       04050700
           IVTNUM = 17                                                  04060700
           DVCOMP = 0.0D0                                               04070700
           DVCORR = 9.1534D-2                                           04080700
      DVCOMP = DVN002                                                   04090700
           IF (DVCOMP - 0.91529D-01) 20170, 10170, 40170                04100700
40170      IF (DVCOMP - 0.91539D-01) 10170, 10170, 20170                04110700
10170      IVPASS = IVPASS + 1                                          04120700
           WRITE (I02, 80002) IVTNUM                                    04130700
           GO TO 0171                                                   04140700
20170      IVFAIL = IVFAIL + 1                                          04150700
           WRITE (I02, 80031) IVTNUM, DVCOMP, DVCORR                    04160700
 0171      CONTINUE                                                     04170700
C                                                                       04180700
C     TESTS 18 THRU 21 TEST DIFFERENT DATA TYPES USING THE IMPLIED-DO   04190700
C                                                                       04200700
CT018*  TEST 018   ****  FCVS PROGRAM 700  *****                        04210700
C     INTEGER                                                           04220700
C                                                                       04230700
           IVTNUM = 18                                                  04240700
           IVCOMP = 0                                                   04250700
           IVCORR = 3                                                   04260700
      IVCOMP = I2N004(2,7)                                              04270700
40180      IF (IVCOMP - 3) 20180, 10180, 20180                          04280700
10180      IVPASS = IVPASS + 1                                          04290700
           WRITE (I02, 80002) IVTNUM                                    04300700
           GO TO 0181                                                   04310700
20180      IVFAIL = IVFAIL + 1                                          04320700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    04330700
 0181      CONTINUE                                                     04340700
C                                                                       04350700
CT019*  TEST 019   ****  FCVS PROGRAM 700  *****                        04360700
C     REAL                                                              04370700
C                                                                       04380700
           IVTNUM = 19                                                  04390700
           RVCOMP = 0.0                                                 04400700
           RVCORR = 4.1                                                 04410700
      RVCOMP = R2N001(4,1)                                              04420700
           IF (RVCOMP - 0.40998E+01) 20190, 10190, 40190                04430700
40190      IF (RVCOMP - 0.41002E+01) 10190, 10190, 20190                04440700
10190      IVPASS = IVPASS + 1                                          04450700
           WRITE (I02, 80002) IVTNUM                                    04460700
           GO TO 0191                                                   04470700
20190      IVFAIL = IVFAIL + 1                                          04480700
           WRITE (I02, 80012) IVTNUM, RVCOMP, RVCORR                    04490700
 0191      CONTINUE                                                     04500700
C                                                                       04510700
CT020*  TEST 020   ****  FCVS PROGRAM 700  *****                        04520700
C     COMPLEX                                                           04530700
C                                                                       04540700
           IVTNUM = 20                                                  04550700
           ZVCOMP = (0.0, 0.0)                                          04560700
           ZVCORR = (7.3, -2.28)                                        04570700
      ZVCOMP = Z1N001(7)                                                04580700
           IF (R2E001(1) - 0.72996E+01) 20200, 40202, 40201             04590700
40201      IF (R2E001(1) - 0.73004E+01) 40202, 40202, 20200             04600700
40202      IF (R2E001(2) + 0.22802E+01) 20200, 10200, 40200             04610700
40200      IF (R2E001(2) + 0.22798E+01) 10200, 10200, 20200             04620700
10200      IVPASS = IVPASS + 1                                          04630700
           WRITE (I02, 80002) IVTNUM                                    04640700
           GO TO 0201                                                   04650700
20200      IVFAIL = IVFAIL + 1                                          04660700
           WRITE (I02, 80045) IVTNUM, ZVCOMP, ZVCORR                    04670700
 0201      CONTINUE                                                     04680700
C                                                                       04690700
CT021*  TEST 021   ****  FCVS PROGRAM 700  *****                        04700700
C     DOUBLE PRECISION                                                  04710700
C                                                                       04720700
           IVTNUM = 21                                                  04730700
           DVCOMP = 0.0D0                                               04740700
           DVCORR = 0.1948D+3                                           04750700
      DVCOMP = D1N001(9)                                                04760700
           IF (DVCOMP - 0.1947999999D+03) 20210, 10210, 40210           04770700
40210      IF (DVCOMP - 0.1948000001D+03) 10210, 10210, 20210           04780700
10210      IVPASS = IVPASS + 1                                          04790700
           WRITE (I02, 80002) IVTNUM                                    04800700
           GO TO 0211                                                   04810700
20210      IVFAIL = IVFAIL + 1                                          04820700
           WRITE (I02, 80031) IVTNUM, DVCOMP, DVCORR                    04830700
 0211      CONTINUE                                                     04840700
C                                                                       04850700
C          TESTS 22 AND 23 TEST THAT EACH SUBSCRIPT EXPRESSION          04860700
C     IN AN IMPLIED-DO LIST MAY CONTAIN IMPLIED-DO-VARIABLES OF         04870700
C     THE LIST THAT HAS THE SUBSCRIPT EXPRESSION WITHIN ITS RANGE.      04880700
C                                                                       04890700
CT022*  TEST 022   ****  FCVS PROGRAM 700  *****                        04900700
C                                                                       04910700
           IVTNUM = 22                                                  04920700
           IVCOMP = 0                                                   04930700
           IVCORR = 155                                                 04940700
      IVCOMP = I2N005(3,4) - I2N005(2,3)                                04950700
40220      IF (IVCOMP - 155) 20220, 10220, 20220                        04960700
10220      IVPASS = IVPASS + 1                                          04970700
           WRITE (I02, 80002) IVTNUM                                    04980700
           GO TO 0221                                                   04990700
20220      IVFAIL = IVFAIL + 1                                          05000700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    05010700
 0221      CONTINUE                                                     05020700
C                                                                       05030700
CT023*  TEST 023   ****  FCVS PROGRAM 700  *****                        05040700
C                                                                       05050700
           IVTNUM = 23                                                  05060700
           IVCOMP = 0                                                   05070700
           IVCORR = 130                                                 05080700
      IVCOMP = I2N006(6,2) + I2N006(6,8)                                05090700
40230      IF (IVCOMP - 130) 20230, 10230, 20230                        05100700
10230      IVPASS = IVPASS + 1                                          05110700
           WRITE (I02, 80002) IVTNUM                                    05120700
           GO TO 0231                                                   05130700
20230      IVFAIL = IVFAIL + 1                                          05140700
           WRITE (I02, 80010) IVTNUM, IVCOMP, IVCORR                    05150700
 0231      CONTINUE                                                     05160700
C                                                                       05170700
CBB** ********************** BBCSUM0  **********************************05180700
C**** WRITE OUT TEST SUMMARY                                            05190700
C****                                                                   05200700
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        05210700
      WRITE (I02, 90004)                                                05220700
      WRITE (I02, 90014)                                                05230700
      WRITE (I02, 90004)                                                05240700
      WRITE (I02, 90020) IVPASS                                         05250700
      WRITE (I02, 90022) IVFAIL                                         05260700
      WRITE (I02, 90024) IVDELE                                         05270700
      WRITE (I02, 90026) IVINSP                                         05280700
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 05290700
CBE** ********************** BBCSUM0  **********************************05300700
CBB** ********************** BBCFOOT0 **********************************05310700
C**** WRITE OUT REPORT FOOTINGS                                         05320700
C****                                                                   05330700
      WRITE (I02,90016) ZPROG, ZPROG                                    05340700
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     05350700
      WRITE (I02,90019)                                                 05360700
CBE** ********************** BBCFOOT0 **********************************05370700
90001 FORMAT (1H ,56X,5HFM700)                                          05380700
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM700)                          05390700
CBB** ********************** BBCFMT0A **********************************05400700
C**** FORMATS FOR TEST DETAIL LINES                                     05410700
C****                                                                   05420700
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           05430700
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           05440700
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           05450700
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           05460700
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           05470700
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    05480700
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05490700
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              05500700
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05510700
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  05520700
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         05530700
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         05540700
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         05550700
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         05560700
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      05570700
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      05580700
80050 FORMAT (1H ,48X,A31)                                              05590700
CBE** ********************** BBCFMT0A **********************************05600700
CBB** ********************** BBCFMAT1 **********************************05610700
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     05620700
C****                                                                   05630700
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05640700
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            05650700
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     05660700
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     05670700
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05680700
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05690700
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05700700
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05710700
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05720700
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  05730700
     21H(,F12.5,2H, ,F12.5,1H))                                         05740700
CBE** ********************** BBCFMAT1 **********************************05750700
CBB** ********************** BBCFMT0B **********************************05760700
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                05770700
C****                                                                   05780700
90002 FORMAT (1H1)                                                      05790700
90004 FORMAT (1H )                                                      05800700
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               05810700
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05820700
90008 FORMAT (1H ,21X,A13,A17)                                          05830700
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       05840700
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    05850700
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     05860700
     1       7X,7HREMARKS,24X)                                          05870700
90014 FORMAT (1H ,46H----------------------------------------------,    05880700
     1        33H---------------------------------)                     05890700
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               05900700
C****                                                                   05910700
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             05920700
C****                                                                   05930700
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          05940700
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        05950700
     1        A13)                                                      05960700
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 05970700
C****                                                                   05980700
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 05990700
C****                                                                   06000700
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              06010700
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              06020700
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             06030700
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  06040700
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  06050700
CBE** ********************** BBCFMT0B **********************************06060700
      STOP                                                              06070700
      END                                                               06080700
