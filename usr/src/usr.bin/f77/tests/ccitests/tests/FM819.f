C***********************************************************************00010819
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020819
C*****   FM819                                                          00030819
C*****                       YDSIN - (187)                              00040819
C*****                                                                  00050819
C***********************************************************************00060819
C*****  GENERAL PURPOSE                                         ANS REF 00070819
C*****    TEST INTRINSIC FUNCTION DSIN                           15.3   00080819
C*****                                                          TABLE 5 00090819
C*****                                                                  00100819
CBB** ********************** BBCCOMNT **********************************00110819
C****                                                                   00120819
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130819
C****                          VERSION 2.0                              00140819
C****                                                                   00150819
C****                                                                   00160819
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170819
C****                   GENERAL SERVICES ADMINISTRATION                 00180819
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190819
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200819
C****                      FALLS CHURCH, VA. 22041                      00210819
C****                                                                   00220819
C****                          (703) 756-6153                           00230819
C****                                                                   00240819
CBE** ********************** BBCCOMNT **********************************00250819
C*****    S P E C I F I C A T I O N S SEGMENT 187                       00260819
        DOUBLE PRECISION AVD, BVD, PIVD, DVCORR                         00270819
C*****                                                                  00280819
CBB** ********************** BBCINITA **********************************00290819
C**** SPECIFICATION STATEMENTS                                          00300819
C****                                                                   00310819
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320819
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330819
CBE** ********************** BBCINITA **********************************00340819
CBB** ********************** BBCINITB **********************************00350819
C**** INITIALIZE SECTION                                                00360819
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370819
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380819
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390819
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400819
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410819
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420819
      DATA   REMRKS /'                               '/                 00430819
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440819
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450819
C****                                                                   00460819
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470819
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480819
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490819
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560819
      IVPASS = 0                                                        00570819
      IVFAIL = 0                                                        00580819
      IVDELE = 0                                                        00590819
      IVINSP = 0                                                        00600819
      IVTOTL = 0                                                        00610819
      IVTOTN = 0                                                        00620819
      ICZERO = 0                                                        00630819
C                                                                       00640819
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650819
      I01 = 05                                                          00660819
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670819
      I02 = 06                                                          00680819
C                                                                       00690819
      I01 = 5                                                           00700819
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710819
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720819
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730819
C                                                                       00740819
      I02 = 6                                                           00750819
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760819
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770819
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780819
C                                                                       00790819
CBE** ********************** BBCINITB **********************************00800819
      NUVI = I02                                                        00810819
      IVTOTL = 19                                                       00820819
      ZPROG = 'FM819'                                                   00830819
CBB** ********************** BBCHED0A **********************************00840819
C****                                                                   00850819
C**** WRITE REPORT TITLE                                                00860819
C****                                                                   00870819
      WRITE (I02, 90002)                                                00880819
      WRITE (I02, 90006)                                                00890819
      WRITE (I02, 90007)                                                00900819
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910819
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920819
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930819
CBE** ********************** BBCHED0A **********************************00940819
C*****                                                                  00950819
C*****    HEADER FOR SEGMENT 187                                        00960819
        WRITE(NUVI,18700)                                               00970819
18700   FORMAT(1H /34H YDSIN - (187) INTRINSIC FUNCTIONS//              00980819
     1         32H  DSIN - (DOUBLE PRECISION SINE)//                    00990819
     2         17H  ANS REF. - 15.3)                                    01000819
CBB** ********************** BBCHED0B **********************************01010819
C**** WRITE DETAIL REPORT HEADERS                                       01020819
C****                                                                   01030819
      WRITE (I02,90004)                                                 01040819
      WRITE (I02,90004)                                                 01050819
      WRITE (I02,90013)                                                 01060819
      WRITE (I02,90014)                                                 01070819
      WRITE (I02,90015) IVTOTL                                          01080819
CBE** ********************** BBCHED0B **********************************01090819
C*****                                                                  01100819
        PIVD = 3.1415926535897932384626434D0                            01110819
C*****                                                                  01120819
CT001*  TEST 1                                ZERO (0.0) SINCE SIN(0)=0 01130819
           IVTNUM = 1                                                   01140819
        BVD = 0.0D0                                                     01150819
        AVD = DSIN(BVD)                                                 01160819
           IF (AVD + 0.5000000000D-09) 20010, 10010, 40010              01170819
40010      IF (AVD - 0.5000000000D-09) 10010, 10010, 20010              01180819
10010      IVPASS = IVPASS + 1                                          01190819
           WRITE (NUVI, 80002) IVTNUM                                   01200819
           GO TO 0011                                                   01210819
20010      IVFAIL = IVFAIL + 1                                          01220819
           DVCORR = 0.00000000000000000000D+00                          01230819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01240819
 0011      CONTINUE                                                     01250819
CT002*  TEST 2                                                       PI 01260819
           IVTNUM = 2                                                   01270819
        AVD = DSIN(PIVD)                                                01280819
           IF (AVD + 0.5000000000D-09) 20020, 10020, 40020              01290819
40020      IF (AVD - 0.5000000000D-09) 10020, 10020, 20020              01300819
10020      IVPASS = IVPASS + 1                                          01310819
           WRITE (NUVI, 80002) IVTNUM                                   01320819
           GO TO 0021                                                   01330819
20020      IVFAIL = IVFAIL + 1                                          01340819
           DVCORR = 0.00000000000000000000D+00                          01350819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01360819
 0021      CONTINUE                                                     01370819
CT003*  TEST 3                                                 PI - 1/8 01380819
           IVTNUM = 3                                                   01390819
        BVD = 3.01659265358979323846D0                                  01400819
        AVD = DSIN(BVD)                                                 01410819
           IF (AVD - 0.1246747333D+00) 20030, 10030, 40030              01420819
40030      IF (AVD - 0.1246747335D+00) 10030, 10030, 20030              01430819
10030      IVPASS = IVPASS + 1                                          01440819
           WRITE (NUVI, 80002) IVTNUM                                   01450819
           GO TO 0031                                                   01460819
20030      IVFAIL = IVFAIL + 1                                          01470819
           DVCORR = 0.12467473338522768996D+00                          01480819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01490819
 0031      CONTINUE                                                     01500819
CT004*  TEST 4                                                PI - 1/16 01510819
           IVTNUM = 4                                                   01520819
        AVD = DSIN(3.204092653589793238D0)                              01530819
           IF (AVD + 0.6245931788D-01) 20040, 10040, 40040              01540819
40040      IF (AVD + 0.6245931781D-01) 10040, 10040, 20040              01550819
10040      IVPASS = IVPASS + 1                                          01560819
           WRITE (NUVI, 80002) IVTNUM                                   01570819
           GO TO 0041                                                   01580819
20040      IVFAIL = IVFAIL + 1                                          01590819
           DVCORR = -0.062459317842380198585D+00                        01600819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01610819
 0041      CONTINUE                                                     01620819
CT005*  TEST 5                                                     2*PI 01630819
           IVTNUM = 5                                                   01640819
        BVD = PIVD * 2.0D0                                              01650819
        AVD = DSIN(BVD)                                                 01660819
           IF (AVD + 0.5000000000D-09) 20050, 10050, 40050              01670819
40050      IF (AVD - 0.5000000000D-09) 10050, 10050, 20050              01680819
10050      IVPASS = IVPASS + 1                                          01690819
           WRITE (NUVI, 80002) IVTNUM                                   01700819
           GO TO 0051                                                   01710819
20050      IVFAIL = IVFAIL + 1                                          01720819
           DVCORR = 0.00000000000000000000D+00                          01730819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01740819
 0051      CONTINUE                                                     01750819
CT006*  TEST 6                                             2*PI - 1/128 01760819
           IVTNUM = 6                                                   01770819
        BVD = (2.0D0 * PIVD) - 1.0D0 / 128.0D0                          01780819
        AVD = DSIN(BVD)                                                 01790819
           IF (AVD + 0.7812420532D-02) 20060, 10060, 40060              01800819
40060      IF (AVD + 0.7812420523D-02) 10060, 10060, 20060              01810819
10060      IVPASS = IVPASS + 1                                          01820819
           WRITE (NUVI, 80002) IVTNUM                                   01830819
           GO TO 0061                                                   01840819
20060      IVFAIL = IVFAIL + 1                                          01850819
           DVCORR = -0.0078124205273828310472D+00                       01860819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01870819
 0061      CONTINUE                                                     01880819
CT007*  TEST 7                                             2*PI - 1/256 01890819
           IVTNUM = 7                                                   01900819
        BVD = (2.0D0 * PIVD) + 1.0D0 / 256.0D0                          01910819
        AVD = DSIN(BVD)                                                 01920819
           IF (AVD - 0.3906240064D-02) 20070, 10070, 40070              01930819
40070      IF (AVD - 0.3906240068D-02) 10070, 10070, 20070              01940819
10070      IVPASS = IVPASS + 1                                          01950819
           WRITE (NUVI, 80002) IVTNUM                                   01960819
           GO TO 0071                                                   01970819
20070      IVFAIL = IVFAIL + 1                                          01980819
           DVCORR = 0.0039062400659001165547D+00                        01990819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02000819
 0071      CONTINUE                                                     02010819
CT008*  TEST 8                           AN EXPRESSION SUPPLIED TO DSIN 02020819
           IVTNUM = 8                                                   02030819
        BVD = 2000.0D0                                                  02040819
        AVD = DSIN(BVD / 10.0D2)                                        02050819
           IF (AVD - 0.9092974263D+00) 20080, 10080, 40080              02060819
40080      IF (AVD - 0.9092974273D+00) 10080, 10080, 20080              02070819
10080      IVPASS = IVPASS + 1                                          02080819
           WRITE (NUVI, 80002) IVTNUM                                   02090819
           GO TO 0081                                                   02100819
20080      IVFAIL = IVFAIL + 1                                          02110819
           DVCORR = 0.90929742682568169540D+00                          02120819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02130819
 0081      CONTINUE                                                     02140819
CT009*  TEST 9                                         THE VALUE -2.0D0 02150819
           IVTNUM = 9                                                   02160819
        BVD = -2.0D0                                                    02170819
        AVD = DSIN(BVD)                                                 02180819
           IF (AVD + 0.9092974273D+00) 20090, 10090, 40090              02190819
40090      IF (AVD + 0.9092974263D+00) 10090, 10090, 20090              02200819
10090      IVPASS = IVPASS + 1                                          02210819
           WRITE (NUVI, 80002) IVTNUM                                   02220819
           GO TO 0091                                                   02230819
20090      IVFAIL = IVFAIL + 1                                          02240819
           DVCORR = -0.90929742682568169540D+00                         02250819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02260819
 0091      CONTINUE                                                     02270819
CT010*  TEST 10                A LARGE VALUE TO TEST ARGUMENT REDUCTION 02280819
           IVTNUM = 10                                                  02290819
        AVD = DSIN(100.0D0)                                             02300819
           IF (AVD + 0.5063656414D+00) 20100, 10100, 40100              02310819
40100      IF (AVD + 0.5063656408D+00) 10100, 10100, 20100              02320819
10100      IVPASS = IVPASS + 1                                          02330819
           WRITE (NUVI, 80002) IVTNUM                                   02340819
           GO TO 0101                                                   02350819
20100      IVFAIL = IVFAIL + 1                                          02360819
           DVCORR = -0.50636564110975879366D+00                         02370819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02380819
 0101      CONTINUE                                                     02390819
CT011*  TEST 11                                      A VERY LARGE VALUE 02400819
           IVTNUM = 11                                                  02410819
        AVD = DSIN(-1000.0D0)                                           02420819
           IF (AVD + 0.8268795410D+00) 20110, 10110, 40110              02430819
40110      IF (AVD + 0.8268795401D+00) 10110, 10110, 20110              02440819
10110      IVPASS = IVPASS + 1                                          02450819
           WRITE (NUVI, 80002) IVTNUM                                   02460819
           GO TO 0111                                                   02470819
20110      IVFAIL = IVFAIL + 1                                          02480819
           DVCORR = -0.82687954053200256026D+00                         02490819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02500819
 0111      CONTINUE                                                     02510819
CT012*  TEST 12                                                    PI/2 02520819
           IVTNUM = 12                                                  02530819
        AVD = DSIN(1.57079632679489661923D0)                            02540819
           IF (AVD - 0.9999999995D+00) 20120, 10120, 40120              02550819
40120      IF (AVD - 0.1000000001D+01) 10120, 10120, 20120              02560819
10120      IVPASS = IVPASS + 1                                          02570819
           WRITE (NUVI, 80002) IVTNUM                                   02580819
           GO TO 0121                                                   02590819
20120      IVFAIL = IVFAIL + 1                                          02600819
           DVCORR = 1.0000000000000000000D+00                           02610819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02620819
 0121      CONTINUE                                                     02630819
CT013*  TEST 13                                         (PI / 2) - 1/32 02640819
           IVTNUM = 13                                                  02650819
        BVD = 1.53954632679489661923D0                                  02660819
        AVD = DSIN(BVD)                                                 02670819
           IF (AVD - 0.9995117579D+00) 20130, 10130, 40130              02680819
40130      IF (AVD - 0.9995117590D+00) 10130, 10130, 20130              02690819
10130      IVPASS = IVPASS + 1                                          02700819
           WRITE (NUVI, 80002) IVTNUM                                   02710819
           GO TO 0131                                                   02720819
20130      IVFAIL = IVFAIL + 1                                          02730819
           DVCORR = 0.99951175848513636924D+00                          02740819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02750819
 0131      CONTINUE                                                     02760819
CT014*  TEST 14                                         (PI / 2) + 1/64 02770819
           IVTNUM = 14                                                  02780819
        BVD = 1.58642132679489661923D0                                  02790819
        AVD = DSIN(BVD)                                                 02800819
           IF (AVD - 0.9998779316D+00) 20140, 10140, 40140              02810819
40140      IF (AVD - 0.9998779327D+00) 10140, 10140, 20140              02820819
10140      IVPASS = IVPASS + 1                                          02830819
           WRITE (NUVI, 80002) IVTNUM                                   02840819
           GO TO 0141                                                   02850819
20140      IVFAIL = IVFAIL + 1                                          02860819
           DVCORR = 0.99987793217100665474D+00                          02870819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02880819
 0141      CONTINUE                                                     02890819
CT015*  TEST 15                                                  3*PI/2 02900819
           IVTNUM = 15                                                  02910819
        BVD = 3.0D0 * PIVD / 2.0D0                                      02920819
        AVD = DSIN(BVD)                                                 02930819
           IF (AVD + 0.1000000001D+01) 20150, 10150, 40150              02940819
40150      IF (AVD + 0.9999999995D+00) 10150, 10150, 20150              02950819
10150      IVPASS = IVPASS + 1                                          02960819
           WRITE (NUVI, 80002) IVTNUM                                   02970819
           GO TO 0151                                                   02980819
20150      IVFAIL = IVFAIL + 1                                          02990819
           DVCORR = -1.000000000000000000D+00                           03000819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03010819
 0151      CONTINUE                                                     03020819
CT016*  TEST 16                                           3*PI/2 - 1/16 03030819
           IVTNUM = 16                                                  03040819
        BVD = (3.0D0 * PIVD / 2.0D0) - 1.0D0 / 16.0D0                   03050819
        AVD = DSIN(BVD)                                                 03060819
           IF (AVD + 0.9980475112D+00) 20160, 10160, 40160              03070819
40160      IF (AVD + 0.9980475102D+00) 10160, 10160, 20160              03080819
10160      IVPASS = IVPASS + 1                                          03090819
           WRITE (NUVI, 80002) IVTNUM                                   03100819
           GO TO 0161                                                   03110819
20160      IVFAIL = IVFAIL + 1                                          03120819
           DVCORR = -0.99804751070009914963D+00                         03130819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03140819
 0161      CONTINUE                                                     03150819
CT017*  TEST 17                                            3*PI - 1/512 03160819
           IVTNUM = 17                                                  03170819
        BVD = (3.0D0 * PIVD / 2.0D0) + 1.0D0 / 512.0D0                  03180819
        AVD = DSIN(BVD)                                                 03190819
           IF (AVD + 0.9999980932D+00) 20170, 10170, 40170              03200819
40170      IF (AVD + 0.9999980921D+00) 10170, 10170, 20170              03210819
10170      IVPASS = IVPASS + 1                                          03220819
           WRITE (NUVI, 80002) IVTNUM                                   03230819
           GO TO 0171                                                   03240819
20170      IVFAIL = IVFAIL + 1                                          03250819
           DVCORR = -0.99999809265197351722D+00                         03260819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03270819
 0171      CONTINUE                                                     03280819
CT018*  TEST 18                               ARGUMENT OF LOW MAGNITUDE 03290819
           IVTNUM = 18                                                  03300819
        BVD = PIVD * 1.0D-17                                            03310819
        AVD = DSIN(BVD)                                                 03320819
           IF (AVD - 0.3141592652D-16) 20180, 10180, 40180              03330819
40180      IF (AVD - 0.3141592655D-16) 10180, 10180, 20180              03340819
10180      IVPASS = IVPASS + 1                                          03350819
           WRITE (NUVI, 80002) IVTNUM                                   03360819
           GO TO 0181                                                   03370819
20180      IVFAIL = IVFAIL + 1                                          03380819
           DVCORR = 3.1415926535897932385D-17                           03390819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03400819
 0181      CONTINUE                                                     03410819
CT019*  TEST 19                              THE FUNCTION APPLIED TWICE 03420819
           IVTNUM = 19                                                  03430819
        AVD = DSIN(PIVD / 4.0D0) * DSIN(3.0D0 * PIVD / 4.0D0)           03440819
           IF (AVD - 0.4999999997D+00) 20190, 10190, 40190              03450819
40190      IF (AVD - 0.5000000003D+00) 10190, 10190, 20190              03460819
10190      IVPASS = IVPASS + 1                                          03470819
           WRITE (NUVI, 80002) IVTNUM                                   03480819
           GO TO 0191                                                   03490819
20190      IVFAIL = IVFAIL + 1                                          03500819
           DVCORR = 0.50000000000000000000D+00                          03510819
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03520819
 0191      CONTINUE                                                     03530819
C*****                                                                  03540819
CBB** ********************** BBCSUM0  **********************************03550819
C**** WRITE OUT TEST SUMMARY                                            03560819
C****                                                                   03570819
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03580819
      WRITE (I02, 90004)                                                03590819
      WRITE (I02, 90014)                                                03600819
      WRITE (I02, 90004)                                                03610819
      WRITE (I02, 90020) IVPASS                                         03620819
      WRITE (I02, 90022) IVFAIL                                         03630819
      WRITE (I02, 90024) IVDELE                                         03640819
      WRITE (I02, 90026) IVINSP                                         03650819
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03660819
CBE** ********************** BBCSUM0  **********************************03670819
CBB** ********************** BBCFOOT0 **********************************03680819
C**** WRITE OUT REPORT FOOTINGS                                         03690819
C****                                                                   03700819
      WRITE (I02,90016) ZPROG, ZPROG                                    03710819
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03720819
      WRITE (I02,90019)                                                 03730819
CBE** ********************** BBCFOOT0 **********************************03740819
CBB** ********************** BBCFMT0A **********************************03750819
C**** FORMATS FOR TEST DETAIL LINES                                     03760819
C****                                                                   03770819
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03780819
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03790819
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03800819
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03810819
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03820819
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03830819
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03840819
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03850819
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03860819
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03870819
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03880819
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03890819
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03900819
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03910819
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03920819
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03930819
80050 FORMAT (1H ,48X,A31)                                              03940819
CBE** ********************** BBCFMT0A **********************************03950819
CBB** ********************** BBCFMAT1 **********************************03960819
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03970819
C****                                                                   03980819
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03990819
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04000819
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04010819
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04020819
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04030819
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04040819
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04050819
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04060819
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04070819
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04080819
     21H(,F12.5,2H, ,F12.5,1H))                                         04090819
CBE** ********************** BBCFMAT1 **********************************04100819
CBB** ********************** BBCFMT0B **********************************04110819
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04120819
C****                                                                   04130819
90002 FORMAT (1H1)                                                      04140819
90004 FORMAT (1H )                                                      04150819
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04160819
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04170819
90008 FORMAT (1H ,21X,A13,A17)                                          04180819
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04190819
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04200819
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04210819
     1       7X,7HREMARKS,24X)                                          04220819
90014 FORMAT (1H ,46H----------------------------------------------,    04230819
     1        33H---------------------------------)                     04240819
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04250819
C****                                                                   04260819
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04270819
C****                                                                   04280819
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04290819
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04300819
     1        A13)                                                      04310819
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04320819
C****                                                                   04330819
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04340819
C****                                                                   04350819
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04360819
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04370819
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04380819
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04390819
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04400819
CBE** ********************** BBCFMT0B **********************************04410819
C*****                                                                  04420819
C*****    END OF TEST SEGMENT 187                                       04430819
      STOP                                                              04440819
      END                                                               04450819
                                                                        04460819
