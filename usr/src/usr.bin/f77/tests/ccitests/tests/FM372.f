C***********************************************************************00010372
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020372
C*****   FM372                                                          00030372
C*****                       XSIN - (186)                               00040372
C*****                                                                  00050372
C***********************************************************************00060372
C*****  GENERAL PURPOSE                                      SUBSET REF 00070372
C*****    TEST INTRINSIC FUNCTION SIN                          15.3     00080372
C*****                                                        TABLE 5   00090372
C*****                                                                  00100372
CBB** ********************** BBCCOMNT **********************************00110372
C****                                                                   00120372
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130372
C****                          VERSION 2.0                              00140372
C****                                                                   00150372
C****                                                                   00160372
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170372
C****                   GENERAL SERVICES ADMINISTRATION                 00180372
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190372
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200372
C****                      FALLS CHURCH, VA. 22041                      00210372
C****                                                                   00220372
C****                          (703) 756-6153                           00230372
C****                                                                   00240372
CBE** ********************** BBCCOMNT **********************************00250372
CBB** ********************** BBCINITA **********************************00260372
C**** SPECIFICATION STATEMENTS                                          00270372
C****                                                                   00280372
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290372
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300372
CBE** ********************** BBCINITA **********************************00310372
CBB** ********************** BBCINITB **********************************00320372
C**** INITIALIZE SECTION                                                00330372
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340372
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350372
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360372
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370372
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380372
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390372
      DATA   REMRKS /'                               '/                 00400372
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410372
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420372
C****                                                                   00430372
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440372
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450372
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460372
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530372
      IVPASS = 0                                                        00540372
      IVFAIL = 0                                                        00550372
      IVDELE = 0                                                        00560372
      IVINSP = 0                                                        00570372
      IVTOTL = 0                                                        00580372
      IVTOTN = 0                                                        00590372
      ICZERO = 0                                                        00600372
C                                                                       00610372
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620372
      I01 = 05                                                          00630372
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640372
      I02 = 06                                                          00650372
C                                                                       00660372
      I01 = 5                                                           00670372
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680372
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690372
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700372
C                                                                       00710372
      I02 = 6                                                           00720372
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730372
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740372
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750372
C                                                                       00760372
CBE** ********************** BBCINITB **********************************00770372
      NUVI = I02                                                        00780372
      IVTOTL = 18                                                       00790372
      ZPROG = 'FM372'                                                   00800372
CBB** ********************** BBCHED0A **********************************00810372
C****                                                                   00820372
C**** WRITE REPORT TITLE                                                00830372
C****                                                                   00840372
      WRITE (I02, 90002)                                                00850372
      WRITE (I02, 90006)                                                00860372
      WRITE (I02, 90007)                                                00870372
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880372
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890372
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900372
CBE** ********************** BBCHED0A **********************************00910372
C*****                                                                  00920372
C*****    HEADER FOR SEGMENT 186                                        00930372
        WRITE(NUVI,18600)                                               00940372
18600   FORMAT(1H ,33H XSIN - (186) INTRINSIC FUNCTIONS//               00950372
     1         12H  SIN (SINE)//                                        00960372
     2         20H  SUBSET REF. - 15.3)                                 00970372
CBB** ********************** BBCHED0B **********************************00980372
C**** WRITE DETAIL REPORT HEADERS                                       00990372
C****                                                                   01000372
      WRITE (I02,90004)                                                 01010372
      WRITE (I02,90004)                                                 01020372
      WRITE (I02,90013)                                                 01030372
      WRITE (I02,90014)                                                 01040372
      WRITE (I02,90015) IVTOTL                                          01050372
CBE** ********************** BBCHED0B **********************************01060372
C*****                                                                  01070372
        PIVS = 3.1415926535897932384626434                              01080372
C*****                                                                  01090372
CT001*  TEST 1                               ZERO (0.0), SINCE SIN(0)=0 01100372
           IVTNUM = 1                                                   01110372
        BVS = 0.0                                                       01120372
        AVS = SIN(BVS)                                                  01130372
           IF (AVS + 0.50000E-04) 20010, 10010, 40010                   01140372
40010      IF (AVS - 0.50000E-04) 10010, 10010, 20010                   01150372
10010      IVPASS = IVPASS + 1                                          01160372
           WRITE (NUVI, 80002) IVTNUM                                   01170372
           GO TO 0011                                                   01180372
20010      IVFAIL = IVFAIL + 1                                          01190372
           RVCORR = 0.00000000000000                                    01200372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01210372
 0011      CONTINUE                                                     01220372
CT002*  TEST 2                                                       PI 01230372
           IVTNUM = 2                                                   01240372
        AVS = SIN(PIVS)                                                 01250372
           IF (AVS + 0.50000E-04) 20020, 10020, 40020                   01260372
40020      IF (AVS - 0.50000E-04) 10020, 10020, 20020                   01270372
10020      IVPASS = IVPASS + 1                                          01280372
           WRITE (NUVI, 80002) IVTNUM                                   01290372
           GO TO 0021                                                   01300372
20020      IVFAIL = IVFAIL + 1                                          01310372
           RVCORR = 0.00000000000000                                    01320372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01330372
 0021      CONTINUE                                                     01340372
CT003*  TEST 3                                                 PI - 1/8 01350372
           IVTNUM = 3                                                   01360372
        BVS = 3.0165926535                                              01370372
        AVS = SIN(BVS)                                                  01380372
           IF (AVS - 0.12466E+00) 20030, 10030, 40030                   01390372
40030      IF (AVS - 0.12468E+00) 10030, 10030, 20030                   01400372
10030      IVPASS = IVPASS + 1                                          01410372
           WRITE (NUVI, 80002) IVTNUM                                   01420372
           GO TO 0031                                                   01430372
20030      IVFAIL = IVFAIL + 1                                          01440372
           RVCORR = 0.12467473338523                                    01450372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01460372
 0031      CONTINUE                                                     01470372
CT004*  TEST 4                                                PI - 1/16 01480372
           IVTNUM = 4                                                   01490372
        AVS = SIN(3.2040926535)                                         01500372
           IF (AVS + 0.62463E-01) 20040, 10040, 40040                   01510372
40040      IF (AVS + 0.62456E-01) 10040, 10040, 20040                   01520372
10040      IVPASS = IVPASS + 1                                          01530372
           WRITE (NUVI, 80002) IVTNUM                                   01540372
           GO TO 0041                                                   01550372
20040      IVFAIL = IVFAIL + 1                                          01560372
           RVCORR = -0.06245931784238                                   01570372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01580372
 0041      CONTINUE                                                     01590372
CT005*  TEST 5                                                     2*PI 01600372
           IVTNUM = 5                                                   01610372
        BVS = PIVS * 2.0                                                01620372
        AVS = SIN(BVS)                                                  01630372
           IF (AVS + 0.50000E-04) 20050, 10050, 40050                   01640372
40050      IF (AVS - 0.50000E-04) 10050, 10050, 20050                   01650372
10050      IVPASS = IVPASS + 1                                          01660372
           WRITE (NUVI, 80002) IVTNUM                                   01670372
           GO TO 0051                                                   01680372
20050      IVFAIL = IVFAIL + 1                                          01690372
           RVCORR = 0.00000000000000                                    01700372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01710372
 0051      CONTINUE                                                     01720372
CT006*  TEST 6                                             2*PI - 1/128 01730372
           IVTNUM = 6                                                   01740372
        BVS = (2.0 * PIVS) - 1.0 / 128.0                                01750372
        AVS = SIN(BVS)                                                  01760372
           IF (AVS + 0.78129E-02) 20060, 10060, 40060                   01770372
40060      IF (AVS + 0.78120E-02) 10060, 10060, 20060                   01780372
10060      IVPASS = IVPASS + 1                                          01790372
           WRITE (NUVI, 80002) IVTNUM                                   01800372
           GO TO 0061                                                   01810372
20060      IVFAIL = IVFAIL + 1                                          01820372
           RVCORR = -0.00781242052738                                   01830372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01840372
70061      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       01840*TI
           WRITE (NUVI,70061)                                           01840*TI
 0061      CONTINUE                                                     01850372
CT007*  TEST 7                                            THE VALUE 2.0 01860372
           IVTNUM = 7                                                   01870372
        BVS = 2.0                                                       01880372
        AVS = SIN(BVS)                                                  01890372
           IF (AVS - 0.90925E+00) 20070, 10070, 40070                   01900372
40070      IF (AVS - 0.90935E+00) 10070, 10070, 20070                   01910372
10070      IVPASS = IVPASS + 1                                          01920372
           WRITE (NUVI, 80002) IVTNUM                                   01930372
           GO TO 0071                                                   01940372
20070      IVFAIL = IVFAIL + 1                                          01950372
           RVCORR = 0.90929742682568                                    01960372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01970372
 0071      CONTINUE                                                     01980372
CT008*  TEST 8                                           THE VALUE -2.0 01990372
           IVTNUM = 8                                                   02000372
        BVS = -2.0                                                      02010372
        AVS = SIN(BVS)                                                  02020372
           IF (AVS + 0.90935E+00) 20080, 10080, 40080                   02030372
40080      IF (AVS + 0.90925E+00) 10080, 10080, 20080                   02040372
10080      IVPASS = IVPASS + 1                                          02050372
           WRITE (NUVI, 80002) IVTNUM                                   02060372
           GO TO 0081                                                   02070372
20080      IVFAIL = IVFAIL + 1                                          02080372
           RVCORR = -0.90929742682568                                   02090372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02100372
 0081      CONTINUE                                                     02110372
CT009*  TEST 09                A LARGE VALUE TO TEST ARGUMENT REDUCTION 02120372
           IVTNUM = 09                                                  02130372
        AVS = SIN(100.0)                                                02140372
           IF (AVS + 0.50639E+00) 20090, 10090, 40090                   02150372
40090      IF (AVS + 0.50634E+00) 10090, 10090, 20090                   02160372
10090      IVPASS = IVPASS + 1                                          02170372
           WRITE (NUVI, 80002) IVTNUM                                   02180372
           GO TO 0091                                                   02190372
20090      IVFAIL = IVFAIL + 1                                          02200372
           RVCORR = -0.50636564110976                                   02210372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02220372
 0091      CONTINUE                                                     02230372
CT010*  TEST 10                                      A VERY LARGE VALUE 02240372
           IVTNUM = 10                                                  02250372
        AVS = SIN(-1000.0)                                              02260372
           IF (AVS + 0.82692E+00) 20100, 10100, 40100                   02270372
40100      IF (AVS + 0.82683E+00) 10100, 10100, 20100                   02280372
10100      IVPASS = IVPASS + 1                                          02290372
           WRITE (NUVI, 80002) IVTNUM                                   02300372
           GO TO 0101                                                   02310372
20100      IVFAIL = IVFAIL + 1                                          02320372
           RVCORR = -0.82687954053200                                   02330372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02340372
 0101      CONTINUE                                                     02350372
CT011*  TEST 11                                                    PI/2 02360372
           IVTNUM = 11                                                  02370372
        AVS = SIN(1.5707963268)                                         02380372
           IF (AVS - 0.99995E+00) 20110, 10110, 40110                   02390372
40110      IF (AVS - 0.10001E+01) 10110, 10110, 20110                   02400372
10110      IVPASS = IVPASS + 1                                          02410372
           WRITE (NUVI, 80002) IVTNUM                                   02420372
           GO TO 0111                                                   02430372
20110      IVFAIL = IVFAIL + 1                                          02440372
           RVCORR = 1.00000000000000                                    02450372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02460372
 0111      CONTINUE                                                     02470372
CT012*  TEST 12                                             PI/2 - 1/32 02480372
           IVTNUM = 12                                                  02490372
        BVS = 1.5395463268                                              02500372
        AVS = SIN(BVS)                                                  02510372
           IF (AVS - 0.99946E+00) 20120, 10120, 40120                   02520372
40120      IF (AVS - 0.99957E+00) 10120, 10120, 20120                   02530372
10120      IVPASS = IVPASS + 1                                          02540372
           WRITE (NUVI, 80002) IVTNUM                                   02550372
           GO TO 0121                                                   02560372
20120      IVFAIL = IVFAIL + 1                                          02570372
           RVCORR = 0.99951175848514                                    02580372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02590372
 0121      CONTINUE                                                     02600372
CT013*  TEST 13                                             PI/2 - 1/64 02610372
           IVTNUM = 13                                                  02620372
        AVS = SIN(1.5864213268)                                         02630372
           IF (AVS - 0.99982E+00) 20130, 10130, 40130                   02640372
40130      IF (AVS - 0.99993E+00) 10130, 10130, 20130                   02650372
10130      IVPASS = IVPASS + 1                                          02660372
           WRITE (NUVI, 80002) IVTNUM                                   02670372
           GO TO 0131                                                   02680372
20130      IVFAIL = IVFAIL + 1                                          02690372
           RVCORR =  0.99987793217101                                   02700372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02710372
 0131      CONTINUE                                                     02720372
CT014*  TEST 14                                                  3*PI/2 02730372
           IVTNUM = 14                                                  02740372
        BVS = 3.0 * PIVS / 2.0                                          02750372
        AVS = SIN(BVS)                                                  02760372
           IF (AVS + 0.10001E+01) 20140, 10140, 40140                   02770372
40140      IF (AVS + 0.99995E+00) 10140, 10140, 20140                   02780372
10140      IVPASS = IVPASS + 1                                          02790372
           WRITE (NUVI, 80002) IVTNUM                                   02800372
           GO TO 0141                                                   02810372
20140      IVFAIL = IVFAIL + 1                                          02820372
           RVCORR = -1.00000000000000                                   02830372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02840372
 0141      CONTINUE                                                     02850372
CT015*  TEST 15                                           3*PI/2 - 1/16 02860372
           IVTNUM = 15                                                  02870372
        BVS = (3.0 * PIVS / 2.0) - 1.0 / 16.0                           02880372
        AVS = SIN(BVS)                                                  02890372
           IF (AVS + 0.99810E+00) 20150, 10150, 40150                   02900372
40150      IF (AVS + 0.99799E+00) 10150, 10150, 20150                   02910372
10150      IVPASS = IVPASS + 1                                          02920372
           WRITE (NUVI, 80002) IVTNUM                                   02930372
           GO TO 0151                                                   02940372
20150      IVFAIL = IVFAIL + 1                                          02950372
           RVCORR = -0.99804751070010                                   02960372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02970372
 0151      CONTINUE                                                     02980372
CT016*  TEST 16                                          3*PI/2 - 1/512 02990372
           IVTNUM = 16                                                  03000372
        BVS = (3.0 * PIVS / 2.0) + 1.0 / 512.0                          03010372
        AVS = SIN(BVS)                                                  03020372
           IF (AVS + 0.10001E+01) 20160, 10160, 40160                   03030372
40160      IF (AVS + 0.99994E+00) 10160, 10160, 20160                   03040372
10160      IVPASS = IVPASS + 1                                          03050372
           WRITE (NUVI, 80002) IVTNUM                                   03060372
           GO TO 0161                                                   03070372
20160      IVFAIL = IVFAIL + 1                                          03080372
           RVCORR = -0.99999809265197                                   03090372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03100372
 0161      CONTINUE                                                     03110372
CT017*  TEST 17                               ARGUMENT OF LOW MAGNITUDE 03120372
           IVTNUM = 17                                                  03130372
        BVS = PIVS * 1.0E-37                                            03140372
        AVS = SIN(BVS)                                                  03150372
           IF (AVS + 0.50000E-04) 20170, 10170, 40170                   03160372
40170      IF (AVS - 0.50000E-04) 10170, 10170, 20170                   03170372
10170      IVPASS = IVPASS + 1                                          03180372
           WRITE (NUVI, 80002) IVTNUM                                   03190372
           GO TO 0171                                                   03200372
20170      IVFAIL = IVFAIL + 1                                          03210372
           RVCORR = 3.14159265358979E-37                                03220372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03230372
 0171      CONTINUE                                                     03240372
CT018*  TEST 18                              THE FUNCTION APPLIED TWICE 03250372
           IVTNUM = 18                                                  03260372
        AVS = SIN(PIVS / 4.0) * SIN(3.0 * PIVS / 4.0)                   03270372
           IF (AVS - 0.49997E+00) 20180, 10180, 40180                   03280372
40180      IF (AVS - 0.50003E+00) 10180, 10180, 20180                   03290372
10180      IVPASS = IVPASS + 1                                          03300372
           WRITE (NUVI, 80002) IVTNUM                                   03310372
           GO TO 0181                                                   03320372
20180      IVFAIL = IVFAIL + 1                                          03330372
           RVCORR = 0.50000000000000                                    03340372
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03350372
 0181      CONTINUE                                                     03360372
C*****                                                                  03370372
CBB** ********************** BBCSUM0  **********************************03380372
C**** WRITE OUT TEST SUMMARY                                            03390372
C****                                                                   03400372
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03410372
      WRITE (I02, 90004)                                                03420372
      WRITE (I02, 90014)                                                03430372
      WRITE (I02, 90004)                                                03440372
      WRITE (I02, 90020) IVPASS                                         03450372
      WRITE (I02, 90022) IVFAIL                                         03460372
      WRITE (I02, 90024) IVDELE                                         03470372
      WRITE (I02, 90026) IVINSP                                         03480372
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03490372
CBE** ********************** BBCSUM0  **********************************03500372
CBB** ********************** BBCFOOT0 **********************************03510372
C**** WRITE OUT REPORT FOOTINGS                                         03520372
C****                                                                   03530372
      WRITE (I02,90016) ZPROG, ZPROG                                    03540372
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03550372
      WRITE (I02,90019)                                                 03560372
CBE** ********************** BBCFOOT0 **********************************03570372
CBB** ********************** BBCFMT0A **********************************03580372
C**** FORMATS FOR TEST DETAIL LINES                                     03590372
C****                                                                   03600372
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03610372
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03620372
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03630372
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03640372
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03650372
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03660372
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03670372
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03680372
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03690372
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03700372
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03710372
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03720372
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03730372
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03740372
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03750372
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03760372
80050 FORMAT (1H ,48X,A31)                                              03770372
CBE** ********************** BBCFMT0A **********************************03780372
CBB** ********************** BBCFMT0B **********************************03790372
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03800372
C****                                                                   03810372
90002 FORMAT (1H1)                                                      03820372
90004 FORMAT (1H )                                                      03830372
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03840372
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03850372
90008 FORMAT (1H ,21X,A13,A17)                                          03860372
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03870372
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03880372
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03890372
     1       7X,7HREMARKS,24X)                                          03900372
90014 FORMAT (1H ,46H----------------------------------------------,    03910372
     1        33H---------------------------------)                     03920372
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03930372
C****                                                                   03940372
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03950372
C****                                                                   03960372
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03970372
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03980372
     1        A13)                                                      03990372
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04000372
C****                                                                   04010372
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04020372
C****                                                                   04030372
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04040372
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04050372
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04060372
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04070372
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04080372
CBE** ********************** BBCFMT0B **********************************04090372
C*****                                                                  04100372
C*****    END OF TEST SEGMENT 186                                       04110372
      STOP                                                              04120372
      END                                                               04130372
                                                                        04140372
