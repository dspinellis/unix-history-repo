C***********************************************************************00010829
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020829
C*****   FM829                                                          00030829
C*****                       YGEN1 - (206)                              00040829
C*****                                                                  00050829
C***********************************************************************00060829
C*****  TESTING OF GENERIC FUNCTIONS                            ANS REF 00070829
C*****          INT, REAL, DBLE, CMPLX                           15.3   00080829
C*****                                                          TABLE 5 00090829
CBB** ********************** BBCCOMNT **********************************00100829
C****                                                                   00110829
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120829
C****                          VERSION 2.0                              00130829
C****                                                                   00140829
C****                                                                   00150829
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160829
C****                   GENERAL SERVICES ADMINISTRATION                 00170829
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180829
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190829
C****                      FALLS CHURCH, VA. 22041                      00200829
C****                                                                   00210829
C****                          (703) 756-6153                           00220829
C****                                                                   00230829
CBE** ********************** BBCCOMNT **********************************00240829
C*****                                                                  00250829
C*****  S P E C I F I C A T I O N S  SEGMENT 206                        00260829
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00270829
        COMPLEX AVC, BVC, CVC, ZVCORR                                   00280829
        REAL R2E(2)                                                     00290829
        EQUIVALENCE (BVC, R2E)                                          00300829
C*****                                                                  00310829
CBB** ********************** BBCINITA **********************************00320829
C**** SPECIFICATION STATEMENTS                                          00330829
C****                                                                   00340829
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350829
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360829
CBE** ********************** BBCINITA **********************************00370829
CBB** ********************** BBCINITB **********************************00380829
C**** INITIALIZE SECTION                                                00390829
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400829
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410829
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420829
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430829
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440829
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450829
      DATA   REMRKS /'                               '/                 00460829
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470829
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480829
C****                                                                   00490829
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500829
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510829
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520829
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590829
      IVPASS = 0                                                        00600829
      IVFAIL = 0                                                        00610829
      IVDELE = 0                                                        00620829
      IVINSP = 0                                                        00630829
      IVTOTL = 0                                                        00640829
      IVTOTN = 0                                                        00650829
      ICZERO = 0                                                        00660829
C                                                                       00670829
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680829
      I01 = 05                                                          00690829
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700829
      I02 = 06                                                          00710829
C                                                                       00720829
      I01 = 5                                                           00730829
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740829
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750829
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760829
C                                                                       00770829
      I02 = 6                                                           00780829
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790829
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800829
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810829
C                                                                       00820829
CBE** ********************** BBCINITB **********************************00830829
      NUVI = I02                                                        00840829
      IVTOTL = 35                                                       00850829
      ZPROG = 'FM829'                                                   00860829
CBB** ********************** BBCHED0A **********************************00870829
C****                                                                   00880829
C**** WRITE REPORT TITLE                                                00890829
C****                                                                   00900829
      WRITE (I02, 90002)                                                00910829
      WRITE (I02, 90006)                                                00920829
      WRITE (I02, 90007)                                                00930829
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940829
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950829
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960829
CBE** ********************** BBCHED0A **********************************00970829
C*****                                                                  00980829
C*****    HEADER FOR SEGMENT 206                                        00990829
        WRITE(NUVI,20600)                                               01000829
20600   FORMAT( 1H , /  35H YGEN1 - (206) GENERIC FUNCTIONS --//        01010829
     1          24H  INT, REAL, DBLE, CMPLX//                           01020829
     2          17H  ANS REF. - 15.3)                                   01030829
CBB** ********************** BBCHED0B **********************************01040829
C**** WRITE DETAIL REPORT HEADERS                                       01050829
C****                                                                   01060829
      WRITE (I02,90004)                                                 01070829
      WRITE (I02,90004)                                                 01080829
      WRITE (I02,90013)                                                 01090829
      WRITE (I02,90014)                                                 01100829
      WRITE (I02,90015) IVTOTL                                          01110829
CBE** ********************** BBCHED0B **********************************01120829
C*****                                                                  01130829
CT001*  TEST 1                          TEST OF INT                     01140829
C*****                                          WITH INTEGER ARG        01150829
           IVTNUM = 1                                                   01160829
        LVI = INT(485)                                                  01170829
           IF (LVI -   485) 20010, 10010, 20010                         01180829
10010      IVPASS = IVPASS + 1                                          01190829
           WRITE (NUVI, 80002) IVTNUM                                   01200829
           GO TO 0011                                                   01210829
20010      IVFAIL = IVFAIL + 1                                          01220829
           IVCORR =   485                                               01230829
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01240829
 0011      CONTINUE                                                     01250829
CT002*  TEST 2                                  WITH DOUBLE PREC ARG    01260829
           IVTNUM = 2                                                   01270829
        LVI = INT(1.375D0)                                              01280829
           IF (LVI -     1) 20020, 10020, 20020                         01290829
10020      IVPASS = IVPASS + 1                                          01300829
           WRITE (NUVI, 80002) IVTNUM                                   01310829
           GO TO 0021                                                   01320829
20020      IVFAIL = IVFAIL + 1                                          01330829
           IVCORR =     1                                               01340829
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01350829
 0021      CONTINUE                                                     01360829
CT003*  TEST 3                                  WITH COMPLEX ARG        01370829
           IVTNUM = 3                                                   01380829
        LVI = INT((1.24, 5.67))                                         01390829
           IF (LVI -     1) 20030, 10030, 20030                         01400829
10030      IVPASS = IVPASS + 1                                          01410829
           WRITE (NUVI, 80002) IVTNUM                                   01420829
           GO TO 0031                                                   01430829
20030      IVFAIL = IVFAIL + 1                                          01440829
           IVCORR =     1                                               01450829
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01460829
 0031      CONTINUE                                                     01470829
CT004*  TEST 4                          TEST OF INT AND IFIX            01480829
C*****                                          WITH REAL ARGS          01490829
           IVTNUM = 4                                                   01500829
        LVI = INT(6.0001) + IFIX(-1.750)                                01510829
           IF (LVI -     5) 20040, 10040, 20040                         01520829
10040      IVPASS = IVPASS + 1                                          01530829
           WRITE (NUVI, 80002) IVTNUM                                   01540829
           GO TO 0041                                                   01550829
20040      IVFAIL = IVFAIL + 1                                          01560829
           IVCORR =     5                                               01570829
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01580829
 0041      CONTINUE                                                     01590829
CT005*  TEST 5                          TEST OF INT AND IDINT           01600829
C*****                                          WITH DOUBLE PREC ARGS   01610829
           IVTNUM = 5                                                   01620829
        AVD = -1.11D1                                                   01630829
        LVI = INT(AVD) * IDINT(3.5D0)                                   01640829
           IF (LVI +    33) 20050, 10050, 20050                         01650829
10050      IVPASS = IVPASS + 1                                          01660829
           WRITE (NUVI, 80002) IVTNUM                                   01670829
           GO TO 0051                                                   01680829
20050      IVFAIL = IVFAIL + 1                                          01690829
           IVCORR =   -33                                               01700829
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01710829
 0051      CONTINUE                                                     01720829
CT006*  TEST 6             INTEGER, REAL, DOUBLE PRECISION, AND COMPLEX 01730829
C*****                                                        ARGUMENTS 01740829
           IVTNUM = 6                                                   01750829
        LVI = INT(-327) + INT(6.75) * INT(123) - INT(6.0001D0)          01760829
     1        / IFIX(13.3) + INT((2.4, 3.5)) + IDINT(-3.375D0)          01770829
           IF (LVI -   410) 20060, 10060, 20060                         01780829
10060      IVPASS = IVPASS + 1                                          01790829
           WRITE (NUVI, 80002) IVTNUM                                   01800829
           GO TO 0061                                                   01810829
20060      IVFAIL = IVFAIL + 1                                          01820829
           IVCORR =   410                                               01830829
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01840829
 0061      CONTINUE                                                     01850829
CT007*  TEST 7                          TEST OF REAL                    01860829
C*****                                          WITH REAL ARG           01870829
           IVTNUM = 7                                                   01880829
        AVS = -3.0                                                      01890829
        BVS = REAL(AVS)                                                 01900829
           IF (BVS +  0.30002E+01) 20070, 10070, 40070                  01910829
40070      IF (BVS +  0.29998E+01) 10070, 10070, 20070                  01920829
10070      IVPASS = IVPASS + 1                                          01930829
           WRITE (NUVI, 80002) IVTNUM                                   01940829
           GO TO 0071                                                   01950829
20070      IVFAIL = IVFAIL + 1                                          01960829
           RVCORR = -3.0                                                01970829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      01980829
 0071      CONTINUE                                                     01990829
CT008*  TEST 8                                  WITH DOUBLE PRECISION   02000829
           IVTNUM = 8                                                   02010829
        AVD = 0.96875D0                                                 02020829
        BVS = REAL(AVD)                                                 02030829
           IF (BVS -  0.96870E+00) 20080, 10080, 40080                  02040829
40080      IF (BVS -  0.96880E+00) 10080, 10080, 20080                  02050829
10080      IVPASS = IVPASS + 1                                          02060829
           WRITE (NUVI, 80002) IVTNUM                                   02070829
           GO TO 0081                                                   02080829
20080      IVFAIL = IVFAIL + 1                                          02090829
           RVCORR = 0.96875                                             02100829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      02110829
 0081      CONTINUE                                                     02120829
CT009*  TEST 9                                  WITH COMPLEX            02130829
           IVTNUM = 9                                                   02140829
        BVS = REAL((2.5, -3.0))                                         02150829
           IF (BVS -  0.24998E+01) 20090, 10090, 40090                  02160829
40090      IF (BVS -  0.25002E+01) 10090, 10090, 20090                  02170829
10090      IVPASS = IVPASS + 1                                          02180829
           WRITE (NUVI, 80002) IVTNUM                                   02190829
           GO TO 0091                                                   02200829
20090      IVFAIL = IVFAIL + 1                                          02210829
           RVCORR = 2.5                                                 02220829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      02230829
 0091      CONTINUE                                                     02240829
CT010*  TEST 10                         TEST OF REAL AND FLOAT          02250829
           IVTNUM = 10                                                  02260829
        BVS = REAL(6) + FLOAT(8)                                        02270829
           IF (BVS -  0.13999E+02) 20100, 10100, 40100                  02280829
40100      IF (BVS -  0.14001E+02) 10100, 10100, 20100                  02290829
10100      IVPASS = IVPASS + 1                                          02300829
           WRITE (NUVI, 80002) IVTNUM                                   02310829
           GO TO 0101                                                   02320829
20100      IVFAIL = IVFAIL + 1                                          02330829
           RVCORR = 14.0                                                02340829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      02350829
 0101      CONTINUE                                                     02360829
CT011*  TEST 11                         TEST OF REAL AND SNGL           02370829
           IVTNUM = 11                                                  02380829
        AVD = 2.5D0                                                     02390829
        BVS = REAL(AVD) + SNGL(0.35875D2)                               02400829
           IF (BVS -  0.38373E+02) 20110, 10110, 40110                  02410829
40110      IF (BVS -  0.38377E+02) 10110, 10110, 20110                  02420829
10110      IVPASS = IVPASS + 1                                          02430829
           WRITE (NUVI, 80002) IVTNUM                                   02440829
           GO TO 0111                                                   02450829
20110      IVFAIL = IVFAIL + 1                                          02460829
           RVCORR = 38.375                                              02470829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      02480829
 0111      CONTINUE                                                     02490829
CT012*  TEST 12                         TEST OF REAL, FLOAT, AND SNGL   02500829
           IVTNUM = 12                                                  02510829
        BVS = REAL(13) + FLOAT(9) * SNGL(0.7625D1) - REAL(2.625D0) +    02520829
     1          REAL(3.5) / REAL((2.0, 4.0))                            02530829
           IF (BVS -  0.80746E+02) 20120, 10120, 40120                  02540829
40120      IF (BVS -  0.80754E+02) 10120, 10120, 20120                  02550829
10120      IVPASS = IVPASS + 1                                          02560829
           WRITE (NUVI, 80002) IVTNUM                                   02570829
           GO TO 0121                                                   02580829
20120      IVFAIL = IVFAIL + 1                                          02590829
           RVCORR = 80.75                                               02600829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      02610829
 0121      CONTINUE                                                     02620829
CT013*  TEST 13                         TEST OF DBLE                    02630829
C*****                                          WITH INTEGER ARG        02640829
           IVTNUM = 13                                                  02650829
        LVI = 9                                                         02660829
        BVD = DBLE(LVI)                                                 02670829
           IF (BVD -  0.89995D+01) 20130, 10130, 40130                  02680829
40130      IF (BVD -  0.90005D+01) 10130, 10130, 20130                  02690829
10130      IVPASS = IVPASS + 1                                          02700829
           WRITE (NUVI, 80002) IVTNUM                                   02710829
           GO TO 0131                                                   02720829
20130      IVFAIL = IVFAIL + 1                                          02730829
           DVCORR = 9.0D0                                               02740829
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      02750829
 0131      CONTINUE                                                     02760829
CT014*  TEST 14                                 WITH REAL ARG           02770829
           IVTNUM = 14                                                  02780829
        AVS = 10.5                                                      02790829
        BVD = DBLE(AVS)                                                 02800829
           IF (BVD -  0.10499D+02) 20140, 10140, 40140                  02810829
40140      IF (BVD -  0.10501D+02) 10140, 10140, 20140                  02820829
10140      IVPASS = IVPASS + 1                                          02830829
           WRITE (NUVI, 80002) IVTNUM                                   02840829
           GO TO 0141                                                   02850829
20140      IVFAIL = IVFAIL + 1                                          02860829
           DVCORR = 10.5D0                                              02870829
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      02880829
 0141      CONTINUE                                                     02890829
CT015*  TEST 15                                 WITH DOUBLE PREC ARG    02900829
           IVTNUM = 15                                                  02910829
        AVD = 9.9D0                                                     02920829
        BVD = DBLE(AVD)                                                 02930829
           IF (BVD -  0.9899999995D+01) 20150, 10150, 40150             02940829
40150      IF (BVD -  0.9900000005D+01) 10150, 10150, 20150             02950829
10150      IVPASS = IVPASS + 1                                          02960829
           WRITE (NUVI, 80002) IVTNUM                                   02970829
           GO TO 0151                                                   02980829
20150      IVFAIL = IVFAIL + 1                                          02990829
           DVCORR = 9.9D0                                               03000829
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03010829
 0151      CONTINUE                                                     03020829
CT016*  TEST 16                                 WITH COMPLEX ARG        03030829
           IVTNUM = 16                                                  03040829
        AVC = (2.5, 5.5)                                                03050829
        BVD = DBLE(AVC)                                                 03060829
           IF (BVD -  0.24998D+01) 20160, 10160, 40160                  03070829
40160      IF (BVD -  0.25002D+01) 10160, 10160, 20160                  03080829
10160      IVPASS = IVPASS + 1                                          03090829
           WRITE (NUVI, 80002) IVTNUM                                   03100829
           GO TO 0161                                                   03110829
20160      IVFAIL = IVFAIL + 1                                          03120829
           DVCORR = 2.5D0                                               03130829
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03140829
 0161      CONTINUE                                                     03150829
CT017*  TEST 17                         TEST OF CMPLX WITH ONE ARG      03160829
C*****                                          WITH INTEGER ARG        03170829
           IVTNUM = 17                                                  03180829
        BVC = CMPLX(9)                                                  03190829
           IF (R2E(1) -  0.89995E+01) 20170, 40172, 40171               03200829
40171      IF (R2E(1) -  0.90005E+01) 40172, 40172, 20170               03210829
40172      IF (R2E(2) +  0.50000E-04) 20170, 10170, 40170               03220829
40170      IF (R2E(2) -  0.50000E-04) 10170, 10170, 20170               03230829
10170      IVPASS = IVPASS + 1                                          03240829
           WRITE (NUVI, 80002) IVTNUM                                   03250829
           GO TO 0171                                                   03260829
20170      IVFAIL = IVFAIL + 1                                          03270829
           ZVCORR = (9,0)                                               03280829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      03290829
 0171      CONTINUE                                                     03300829
CT018*  TEST 18                                 WITH REAL               03310829
           IVTNUM = 18                                                  03320829
        BVC = CMPLX(4.093)                                              03330829
           IF (R2E(1) -  0.40928E+01) 20180, 40182, 40181               03340829
40181      IF (R2E(1) -  0.40932E+01) 40182, 40182, 20180               03350829
40182      IF (R2E(2) +  0.50000E-04) 20180, 10180, 40180               03360829
40180      IF (R2E(2) -  0.50000E-04) 10180, 10180, 20180               03370829
10180      IVPASS = IVPASS + 1                                          03380829
           WRITE (NUVI, 80002) IVTNUM                                   03390829
           GO TO 0181                                                   03400829
20180      IVFAIL = IVFAIL + 1                                          03410829
           ZVCORR = (4.093,0.0)                                         03420829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      03430829
 0181      CONTINUE                                                     03440829
CT019*  TEST 19                                 WITH DOUBLE PREC ARG    03450829
           IVTNUM = 19                                                  03460829
        AVD = 0.375D-3                                                  03470829
        BVC = CMPLX(AVD)                                                03480829
           IF (R2E(1) -  0.37498E-03) 20190, 40192, 40191               03490829
40191      IF (R2E(1) -  0.37502E-03) 40192, 40192, 20190               03500829
40192      IF (R2E(2) +  0.50000E-04) 20190, 10190, 40190               03510829
40190      IF (R2E(2) -  0.50000E-04) 10190, 10190, 20190               03520829
10190      IVPASS = IVPASS + 1                                          03530829
           WRITE (NUVI, 80002) IVTNUM                                   03540829
           GO TO 0191                                                   03550829
20190      IVFAIL = IVFAIL + 1                                          03560829
           ZVCORR = (0.375E-3, 0.0E0)                                   03570829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      03580829
 0191      CONTINUE                                                     03590829
CT020*  TEST 20                                 WITH COMPLEX            03600829
           IVTNUM = 20                                                  03610829
        AVC = (4.5, 1.2)                                                03620829
        BVC = CMPLX(AVC)                                                03630829
           IF (R2E(1) -  0.44997E+01) 20200, 40202, 40201               03640829
40201      IF (R2E(1) -  0.45003E+01) 40202, 40202, 20200               03650829
40202      IF (R2E(2) -  0.11999E+01) 20200, 10200, 40200               03660829
40200      IF (R2E(2) -  0.12001E+01) 10200, 10200, 20200               03670829
10200      IVPASS = IVPASS + 1                                          03680829
           WRITE (NUVI, 80002) IVTNUM                                   03690829
           GO TO 0201                                                   03700829
20200      IVFAIL = IVFAIL + 1                                          03710829
           ZVCORR = (4.5, 1.2)                                          03720829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      03730829
 0201      CONTINUE                                                     03740829
CT021*  TEST 21                         TEST OF CMPLX WITH TWO ARGS     03750829
C*****                                          WITH INTEGER ARGS       03760829
           IVTNUM = 21                                                  03770829
        BVC = CMPLX(3, 1)                                               03780829
           IF (R2E(1) -  0.29998E+01) 20210, 40212, 40211               03790829
40211      IF (R2E(1) -  0.30002E+01) 40212, 40212, 20210               03800829
40212      IF (R2E(2) -  0.99995E+00) 20210, 10210, 40210               03810829
40210      IF (R2E(2) -  0.10001E+01) 10210, 10210, 20210               03820829
10210      IVPASS = IVPASS + 1                                          03830829
           WRITE (NUVI, 80002) IVTNUM                                   03840829
           GO TO 0211                                                   03850829
20210      IVFAIL = IVFAIL + 1                                          03860829
           ZVCORR = (3.0, 1.0)                                          03870829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      03880829
 0211      CONTINUE                                                     03890829
CT022*  TEST 22                                 WITH REAL ARGS          03900829
           IVTNUM = 22                                                  03910829
        BVC = CMPLX(8.34, 634.3)                                        03920829
           IF (R2E(1) -  0.83395E+01) 20220, 40222, 40221               03930829
40221      IF (R2E(1) -  0.83405E+01) 40222, 40222, 20220               03940829
40222      IF (R2E(2) -  0.63426E+03) 20220, 10220, 40220               03950829
40220      IF (R2E(2) -  0.63434E+03) 10220, 10220, 20220               03960829
10220      IVPASS = IVPASS + 1                                          03970829
           WRITE (NUVI, 80002) IVTNUM                                   03980829
           GO TO 0221                                                   03990829
20220      IVFAIL = IVFAIL + 1                                          04000829
           ZVCORR = (8.34, 634.3)                                       04010829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      04020829
 0221      CONTINUE                                                     04030829
CT023*  TEST 23                                 WITH DOUBLE PREC ARGS   04040829
           IVTNUM = 23                                                  04050829
        AVD = 0.96875D0                                                 04060829
        BVD = 3.5D-1                                                    04070829
        BVC = CMPLX(AVD, BVD)                                           04080829
           IF (R2E(1) -  0.96870E+00) 20230, 40232, 40231               04090829
40231      IF (R2E(1) -  0.96880E+00) 40232, 40232, 20230               04100829
40232      IF (R2E(2) -  0.34998E+00) 20230, 10230, 40230               04110829
40230      IF (R2E(2) -  0.35002E+00) 10230, 10230, 20230               04120829
10230      IVPASS = IVPASS + 1                                          04130829
           WRITE (NUVI, 80002) IVTNUM                                   04140829
           GO TO 0231                                                   04150829
20230      IVFAIL = IVFAIL + 1                                          04160829
           ZVCORR = (0.96875, 0.35)                                     04170829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      04180829
 0231      CONTINUE                                                     04190829
CT024*  TEST 24                         TEST OF INT AND =               04200829
C*****                                          WITH REAL EXPR          04210829
           IVTNUM = 24                                                  04220829
        CVS = 0.0                                                       04230829
        CVD = 0.0D0                                                     04240829
        CVC = (0.0,0.0)                                                 04250829
        LVI = 0                                                         04260829
        AVS = 5.0                                                       04270829
        IVI = 1.0 * 5.0 + 6.0                                           04280829
        KVI = LVI + INT(1.0 * AVS + 6.0)                                04290829
           IF (KVI -    11) 20240, 10240, 20240                         04300829
10240      IVPASS = IVPASS + 1                                          04310829
           WRITE (NUVI, 80002) IVTNUM                                   04320829
           GO TO 0241                                                   04330829
20240      IVFAIL = IVFAIL + 1                                          04340829
           IVCORR =    11                                               04350829
           WRITE (NUVI, 80010) IVTNUM, KVI, IVCORR                      04360829
 0241      CONTINUE                                                     04370829
CT025*  TEST 25                                 WITH DOUBLE PREC EXPR   04380829
           IVTNUM = 25                                                  04390829
        AVD = 3.48D0                                                    04400829
        IVI = 3.48D0 * 47.98D0                                          04410829
        KVI = LVI + INT(AVD * 47.98D0)                                  04420829
           IF (KVI -   166) 20250, 10250, 20250                         04430829
10250      IVPASS = IVPASS + 1                                          04440829
           WRITE (NUVI, 80002) IVTNUM                                   04450829
           GO TO 0251                                                   04460829
20250      IVFAIL = IVFAIL + 1                                          04470829
           IVCORR =   166                                               04480829
           WRITE (NUVI, 80010) IVTNUM, KVI, IVCORR                      04490829
 0251      CONTINUE                                                     04500829
CT026*  TEST 26                                 WITH COMPLEX EXPR       04510829
           IVTNUM = 26                                                  04520829
        AVC = (3.9, 5.0)                                                04530829
        IVI = (3.4, 4.5) + (3.9, 5.0)                                   04540829
        KVI = LVI + INT((3.4, 4.5) + AVC)                               04550829
           IF (KVI -     7) 20260, 10260, 20260                         04560829
10260      IVPASS = IVPASS + 1                                          04570829
           WRITE (NUVI, 80002) IVTNUM                                   04580829
           GO TO 0261                                                   04590829
20260      IVFAIL = IVFAIL + 1                                          04600829
           IVCORR =     7                                               04610829
           WRITE (NUVI, 80010) IVTNUM, KVI, IVCORR                      04620829
 0261      CONTINUE                                                     04630829
CT027*  TEST 27                         TEST OF REAL AND =              04640829
C*****                                          WITH INT EXPR           04650829
           IVTNUM = 27                                                  04660829
        IVI = 20                                                        04670829
        AVS = 20 + 34 / 20                                              04680829
        BVS = CVS + REAL(IVI + 34 / IVI)                                04690829
           IF (BVS -  0.20999E+02) 20270, 10270, 40270                  04700829
40270      IF (BVS -  0.21001E+02) 10270, 10270, 20270                  04710829
10270      IVPASS = IVPASS + 1                                          04720829
           WRITE (NUVI, 80002) IVTNUM                                   04730829
           GO TO 0271                                                   04740829
20270      IVFAIL = IVFAIL + 1                                          04750829
           RVCORR = 21.0                                                04760829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      04770829
 0271      CONTINUE                                                     04780829
CT028*  TEST 28                                 WITH DOUBLE PREC EXPR   04790829
           IVTNUM = 28                                                  04800829
        JVI = 28                                                        04810829
        AVD = 0.9834D0                                                  04820829
        AVS = 3.0748D0 / 0.9834D0                                       04830829
        BVS = CVS + REAL(3.0748D0 / AVD)                                04840829
           IF (BVS -  0.31265E+01) 20280, 10280, 40280                  04850829
40280      IF (BVS -  0.31269E+01) 10280, 10280, 20280                  04860829
10280      IVPASS = IVPASS + 1                                          04870829
           WRITE (NUVI, 80002) IVTNUM                                   04880829
           GO TO 0281                                                   04890829
20280      IVFAIL = IVFAIL + 1                                          04900829
           RVCORR = 3.1267033                                           04910829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      04920829
 0281      CONTINUE                                                     04930829
CT029*  TEST 29                                 WITH COMPLEX            04940829
           IVTNUM = 29                                                  04950829
        JVI = 29                                                        04960829
        AVC = (1.0, 384.9)                                              04970829
        AVS = (3.495, 98.734) * (1.0, 384.9)                            04980829
        BVS = CVS + REAL((3.495, 98.734) * AVC)                         04990829
           IF (BVS +  0.38001E+05) 20290, 10290, 40290                  05000829
40290      IF (BVS +  0.37997E+05) 10290, 10290, 20290                  05010829
10290      IVPASS = IVPASS + 1                                          05020829
           WRITE (NUVI, 80002) IVTNUM                                   05030829
           GO TO 0291                                                   05040829
20290      IVFAIL = IVFAIL + 1                                          05050829
           RVCORR = -37999.222                                          05060829
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      05070829
 0291      CONTINUE                                                     05080829
CT030*  TEST 30                         TEST OF DBLE AND =              05090829
C*****                                          WITH INTEGER EXPR       05100829
           IVTNUM = 30                                                  05110829
        JVI = 30                                                        05120829
        IVI = 5                                                         05130829
        AVD = 1 * 5 + 6                                                 05140829
        BVD = CVD + DBLE(1 * IVI + 6)                                   05150829
           IF (BVD -  0.10999D+02) 20300, 10300, 40300                  05160829
40300      IF (BVD -  0.11001D+02) 10300, 10300, 20300                  05170829
10300      IVPASS = IVPASS + 1                                          05180829
           WRITE (NUVI, 80002) IVTNUM                                   05190829
           GO TO 0301                                                   05200829
20300      IVFAIL = IVFAIL + 1                                          05210829
           DVCORR = .11000000D+02                                       05220829
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      05230829
 0301      CONTINUE                                                     05240829
CT031*  TEST 31                                 WITH REAL EXPR          05250829
           IVTNUM = 31                                                  05260829
        JVI = 31                                                        05270829
        AVS = -4.5                                                      05280829
        AVD = 1.3 / (-4.5)                                              05290829
        BVD = CVD + DBLE(1.3 / AVS)                                     05300829
           IF (BVD +  0.28891D+00) 20310, 10310, 40310                  05310829
40310      IF (BVD +  0.28887D+00) 10310, 10310, 20310                  05320829
10310      IVPASS = IVPASS + 1                                          05330829
           WRITE (NUVI, 80002) IVTNUM                                   05340829
           GO TO 0311                                                   05350829
20310      IVFAIL = IVFAIL + 1                                          05360829
           DVCORR = -0.288888888888888889D+00                           05370829
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      05380829
 0311      CONTINUE                                                     05390829
CT032*  TEST 32                                 WITH COMPLEX EXPR       05400829
           IVTNUM = 32                                                  05410829
        JVI = 32                                                        05420829
        AVC = (3.9, 5.0)                                                05430829
        AVD = (3.4, 4.5) + (3.9, 5.0)                                   05440829
        BVD = CVD + DBLE((3.4, 4.5) + AVC)                              05450829
           IF (BVD -  0.72996D+01) 20320, 10320, 40320                  05460829
40320      IF (BVD -  0.73004D+01) 10320, 10320, 20320                  05470829
10320      IVPASS = IVPASS + 1                                          05480829
           WRITE (NUVI, 80002) IVTNUM                                   05490829
           GO TO 0321                                                   05500829
20320      IVFAIL = IVFAIL + 1                                          05510829
           DVCORR = .73000000D+01                                       05520829
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      05530829
 0321      CONTINUE                                                     05540829
CT033*  TEST 33                         TEST OF CMPLX AND =             05550829
C*****                                          WITH INTEGER EXPR       05560829
           IVTNUM = 33                                                  05570829
        JVI = 33                                                        05580829
        IVI = 673                                                       05590829
        AVC = 394 - 673                                                 05600829
        BVC = CVC + CMPLX(394 - IVI)                                    05610829
           IF (R2E(1) +  0.27902E+03) 20330, 40332, 40331               05620829
40331      IF (R2E(1) +  0.27898E+03) 40332, 40332, 20330               05630829
40332      IF (R2E(2) +  0.50000E-04) 20330, 10330, 40330               05640829
40330      IF (R2E(2) -  0.50000E-04) 10330, 10330, 20330               05650829
10330      IVPASS = IVPASS + 1                                          05660829
           WRITE (NUVI, 80002) IVTNUM                                   05670829
           GO TO 0331                                                   05680829
20330      IVFAIL = IVFAIL + 1                                          05690829
           ZVCORR = (-279.00000, .00000000)                             05700829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      05710829
 0331      CONTINUE                                                     05720829
CT034*  TEST 34                                 WITH REAL EXPR          05730829
           IVTNUM = 34                                                  05740829
        JVI = 34                                                        05750829
        AVS = 3.48                                                      05760829
        AVC = 3.48 * 47.98                                              05770829
        BVC = CVC + CMPLX(AVS * 47.98)                                  05780829
           IF (R2E(1) -  0.16696E+03) 20340, 40342, 40341               05790829
40341      IF (R2E(1) -  0.16698E+03) 40342, 40342, 20340               05800829
40342      IF (R2E(2) +  0.50000E-04) 20340, 10340, 40340               05810829
40340      IF (R2E(2) -  0.50000E-04) 10340, 10340, 20340               05820829
10340      IVPASS = IVPASS + 1                                          05830829
           WRITE (NUVI, 80002) IVTNUM                                   05840829
           GO TO 0341                                                   05850829
20340      IVFAIL = IVFAIL + 1                                          05860829
           ZVCORR = (166.97040, .00000000)                              05870829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      05880829
 0341      CONTINUE                                                     05890829
CT035*  TEST 35                                                         05900829
           IVTNUM = 35                                                  05910829
        JVI = 35                                                        05920829
        AVD = 0.94D1                                                    05930829
        AVC = 3.0283D3 / 0.94D1                                         05940829
        BVC = CVC + CMPLX(3.0283D3 / AVD)                               05950829
           IF (R2E(1) -  0.32214E+03) 20350, 40352, 40351               05960829
40351      IF (R2E(1) -  0.32218E+03) 40352, 40352, 20350               05970829
40352      IF (R2E(2) +  0.50000E-04) 20350, 10350, 40350               05980829
40350      IF (R2E(2) -  0.50000E-04) 10350, 10350, 20350               05990829
10350      IVPASS = IVPASS + 1                                          06000829
           WRITE (NUVI, 80002) IVTNUM                                   06010829
           GO TO 0351                                                   06020829
20350      IVFAIL = IVFAIL + 1                                          06030829
           ZVCORR = (322.15957, .000000000)                             06040829
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      06050829
 0351      CONTINUE                                                     06060829
C*****                                                                  06070829
CBB** ********************** BBCSUM0  **********************************06080829
C**** WRITE OUT TEST SUMMARY                                            06090829
C****                                                                   06100829
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        06110829
      WRITE (I02, 90004)                                                06120829
      WRITE (I02, 90014)                                                06130829
      WRITE (I02, 90004)                                                06140829
      WRITE (I02, 90020) IVPASS                                         06150829
      WRITE (I02, 90022) IVFAIL                                         06160829
      WRITE (I02, 90024) IVDELE                                         06170829
      WRITE (I02, 90026) IVINSP                                         06180829
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 06190829
CBE** ********************** BBCSUM0  **********************************06200829
CBB** ********************** BBCFOOT0 **********************************06210829
C**** WRITE OUT REPORT FOOTINGS                                         06220829
C****                                                                   06230829
      WRITE (I02,90016) ZPROG, ZPROG                                    06240829
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     06250829
      WRITE (I02,90019)                                                 06260829
CBE** ********************** BBCFOOT0 **********************************06270829
CBB** ********************** BBCFMT0A **********************************06280829
C**** FORMATS FOR TEST DETAIL LINES                                     06290829
C****                                                                   06300829
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           06310829
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           06320829
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           06330829
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           06340829
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           06350829
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    06360829
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06370829
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              06380829
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06390829
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  06400829
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         06410829
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         06420829
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         06430829
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         06440829
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      06450829
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      06460829
80050 FORMAT (1H ,48X,A31)                                              06470829
CBE** ********************** BBCFMT0A **********************************06480829
CBB** ********************** BBCFMAT1 **********************************06490829
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     06500829
C****                                                                   06510829
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06520829
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            06530829
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     06540829
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     06550829
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06560829
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06570829
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06580829
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06590829
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06600829
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  06610829
     21H(,F12.5,2H, ,F12.5,1H))                                         06620829
CBE** ********************** BBCFMAT1 **********************************06630829
CBB** ********************** BBCFMT0B **********************************06640829
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                06650829
C****                                                                   06660829
90002 FORMAT (1H1)                                                      06670829
90004 FORMAT (1H )                                                      06680829
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               06690829
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06700829
90008 FORMAT (1H ,21X,A13,A17)                                          06710829
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       06720829
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    06730829
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     06740829
     1       7X,7HREMARKS,24X)                                          06750829
90014 FORMAT (1H ,46H----------------------------------------------,    06760829
     1        33H---------------------------------)                     06770829
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               06780829
C****                                                                   06790829
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             06800829
C****                                                                   06810829
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          06820829
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        06830829
     1        A13)                                                      06840829
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 06850829
C****                                                                   06860829
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 06870829
C****                                                                   06880829
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              06890829
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              06900829
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             06910829
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  06920829
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  06930829
CBE** ********************** BBCFMT0B **********************************06940829
C*****                                                                  06950829
C*****    END OF TEST SEGMENT 206                                       06960829
      STOP                                                              06970829
      END                                                               06980829
