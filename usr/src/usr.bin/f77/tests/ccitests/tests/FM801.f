C***********************************************************************00010801
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020801
C*****   FM801               YDINT - (155)                              00030801
C*****                                                                  00040801
C***********************************************************************00050801
C*****  GENERAL PURPOSE                                         ANS REF 00060801
C*****    TEST INTRINSIC FUNCTIONS DINT, DNINT, IDNINT           15.3   00070801
C*****    TRUNCATION (SIGN OF A * LARGEST INTEGER LE ABS(A) )  (TABLE 5)00080801
C*****                                                                  00090801
C*****  GENERAL COMMENTS                                                00100801
C*****          FLOAT FUNCTION ASSUMED WORKING                          00110801
C*****                                                                  00120801
CBB** ********************** BBCCOMNT **********************************00130801
C****                                                                   00140801
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150801
C****                          VERSION 2.0                              00160801
C****                                                                   00170801
C****                                                                   00180801
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190801
C****                   GENERAL SERVICES ADMINISTRATION                 00200801
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210801
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220801
C****                      FALLS CHURCH, VA. 22041                      00230801
C****                                                                   00240801
C****                          (703) 756-6153                           00250801
C****                                                                   00260801
CBE** ********************** BBCCOMNT **********************************00270801
C*****                                                                  00280801
C*****    S P E C I F I C A T I O N S  SEGMENT 155                      00290801
        DOUBLE PRECISION DNAVD, DNBVD, DNDVD                            00300801
C*****                                                                  00310801
CBB** ********************** BBCINITA **********************************00320801
C**** SPECIFICATION STATEMENTS                                          00330801
C****                                                                   00340801
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350801
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360801
CBE** ********************** BBCINITA **********************************00370801
CBB** ********************** BBCINITB **********************************00380801
C**** INITIALIZE SECTION                                                00390801
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400801
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410801
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420801
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430801
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440801
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450801
      DATA   REMRKS /'                               '/                 00460801
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470801
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480801
C****                                                                   00490801
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500801
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510801
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520801
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590801
      IVPASS = 0                                                        00600801
      IVFAIL = 0                                                        00610801
      IVDELE = 0                                                        00620801
      IVINSP = 0                                                        00630801
      IVTOTL = 0                                                        00640801
      IVTOTN = 0                                                        00650801
      ICZERO = 0                                                        00660801
C                                                                       00670801
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680801
      I01 = 05                                                          00690801
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700801
      I02 = 06                                                          00710801
C                                                                       00720801
      I01 = 5                                                           00730801
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740801
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750801
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760801
C                                                                       00770801
      I02 = 6                                                           00780801
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790801
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800801
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810801
C                                                                       00820801
CBE** ********************** BBCINITB **********************************00830801
      NUVI = I02                                                        00840801
      IVTOTL = 45                                                       00850801
      ZPROG = 'FM801'                                                   00860801
CBB** ********************** BBCHED0A **********************************00870801
C****                                                                   00880801
C**** WRITE REPORT TITLE                                                00890801
C****                                                                   00900801
      WRITE (I02, 90002)                                                00910801
      WRITE (I02, 90006)                                                00920801
      WRITE (I02, 90007)                                                00930801
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940801
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950801
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960801
CBE** ********************** BBCHED0A **********************************00970801
C*****                                                                  00980801
C*****    HEADER FOR SEGMENT 155                                        00990801
        WRITE (NUVI,15501)                                              01000801
15501   FORMAT (1H , // 1X,35HYDINT - (155) INTRINSIC FUNCTIONS--//16X, 01010801
     1          38HDINT, DNINT, IDNINT (TYPE CONVERSION)  //            01020801
     2          17H  ANS REF. - 15.3)                                   01030801
CBB** ********************** BBCHED0B **********************************01040801
C**** WRITE DETAIL REPORT HEADERS                                       01050801
C****                                                                   01060801
      WRITE (I02,90004)                                                 01070801
      WRITE (I02,90004)                                                 01080801
      WRITE (I02,90013)                                                 01090801
      WRITE (I02,90014)                                                 01100801
      WRITE (I02,90015) IVTOTL                                          01110801
CBE** ********************** BBCHED0B **********************************01120801
C*****                                                                  01130801
C*****    TEST OF DINT                                                  01140801
C*****                                                                  01150801
        WRITE(NUVI, 15502)                                              01160801
15502   FORMAT(// 8X, 12HTEST OF DINT)                                  01170801
CT001*  TEST 1                                         THE VALUE ZERO   01180801
           IVTNUM = 1                                                   01190801
        DNBVD = 0.0D0                                                   01200801
        DNAVD = DINT(DNBVD)                                             01210801
           IF (DNAVD + 5.0D-10) 20010, 10010, 40010                     01220801
40010      IF (DNAVD - 5.0D-10) 10010, 10010, 20010                     01230801
10010      IVPASS = IVPASS + 1                                          01240801
           WRITE (NUVI, 80002) IVTNUM                                   01250801
           GO TO 0011                                                   01260801
20010      IVFAIL = IVFAIL + 1                                          01270801
           DVCORR = 0.0D0                                               01280801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    01290801
 0011      CONTINUE                                                     01300801
CT002*  TEST 2                                       A VALUE IN (0,1)   01310801
           IVTNUM = 2                                                   01320801
        DNBVD = 0.375D0                                                 01330801
        DNAVD = DINT(DNBVD)                                             01340801
           IF (DNAVD + 5.0D-10) 20020, 10020, 40020                     01350801
40020      IF (DNAVD - 5.0D-10) 10020, 10020, 20020                     01360801
10020      IVPASS = IVPASS + 1                                          01370801
           WRITE (NUVI, 80002) IVTNUM                                   01380801
           GO TO 0021                                                   01390801
20020      IVFAIL = IVFAIL + 1                                          01400801
           DVCORR = 0.0D0                                               01410801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    01420801
 0021      CONTINUE                                                     01430801
CT003*  TEST 3                                            THE VALUE 1   01440801
           IVTNUM = 3                                                   01450801
        DNBVD = FLOAT(1)                                                01460801
        DNAVD = DINT(DNBVD)                                             01470801
           IF (DNAVD - 0.9999999995D0) 20030, 10030, 40030              01480801
40030      IF (DNAVD - 1.000000001D0) 10030, 10030, 20030               01490801
10030      IVPASS = IVPASS + 1                                          01500801
           WRITE (NUVI, 80002) IVTNUM                                   01510801
           GO TO 0031                                                   01520801
20030      IVFAIL = IVFAIL + 1                                          01530801
           DVCORR = 1.0D0                                               01540801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    01550801
 0031      CONTINUE                                                     01560801
CT004*  TEST 4                      AN INTEGRAL VALUE OTHER THAN 0, 1   01570801
           IVTNUM = 4                                                   01580801
        DNBVD = FLOAT(6)                                                01590801
        DNAVD = DINT(DNBVD)                                             01600801
           IF (DNAVD - 5.999999997D0) 20040, 10040, 40040               01610801
40040      IF (DNAVD - 6.000000003D0) 10040, 10040, 20040               01620801
10040      IVPASS = IVPASS + 1                                          01630801
           WRITE (NUVI, 80002) IVTNUM                                   01640801
           GO TO 0041                                                   01650801
20040      IVFAIL = IVFAIL + 1                                          01660801
           DVCORR = 6.0D0                                               01670801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    01680801
 0041      CONTINUE                                                     01690801
CT005*  TEST 5                                     A VALUE IN (X,X+1)   01700801
           IVTNUM = 5                                                   01710801
        DNBVD = 0.375D1                                                 01720801
        DNAVD = DINT(DNBVD)                                             01730801
           IF (DNAVD - 2.999999998D0) 20050, 10050, 40050               01740801
40050      IF (DNAVD - 3.000000002D0) 10050, 10050, 20050               01750801
10050      IVPASS = IVPASS + 1                                          01760801
           WRITE (NUVI, 80002) IVTNUM                                   01770801
           GO TO 0051                                                   01780801
20050      IVFAIL = IVFAIL + 1                                          01790801
           DVCORR = 0.3D1                                               01800801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    01810801
 0051      CONTINUE                                                     01820801
CT006*  TEST 6               A NEGATIVE VALUE WITH MAGNITUDE IN (0,1)   01830801
           IVTNUM = 6                                                   01840801
        DNBVD = -0.375D0                                                01850801
        DNAVD = DINT(DNBVD)                                             01860801
           IF (DNAVD + 5.0D-10) 20060, 10060, 40060                     01870801
40060      IF (DNAVD - 5.0D-10) 10060, 10060, 20060                     01880801
10060      IVPASS = IVPASS + 1                                          01890801
           WRITE (NUVI, 80002) IVTNUM                                   01900801
           GO TO 0061                                                   01910801
20060      IVFAIL = IVFAIL + 1                                          01920801
           DVCORR = 0.0D0                                               01930801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    01940801
 0061      CONTINUE                                                     01950801
CT007*  TEST 7                                           THE VALUE -1   01960801
           IVTNUM = 7                                                   01970801
        DNBVD = FLOAT(-1)                                               01980801
        DNAVD = DINT(DNBVD)                                             01990801
           IF (DNAVD + 1.000000001D0) 20070, 10070, 40070               02000801
40070      IF (DNAVD + 0.9999999995D0) 10070, 10070, 20070              02010801
10070      IVPASS = IVPASS + 1                                          02020801
           WRITE (NUVI, 80002) IVTNUM                                   02030801
           GO TO 0071                                                   02040801
20070      IVFAIL = IVFAIL + 1                                          02050801
           DVCORR = -1.0D0                                              02060801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    02070801
 0071      CONTINUE                                                     02080801
CT008*  TEST 8                              A NEGATIVE INTEGRAL VALUE   02090801
           IVTNUM = 8                                                   02100801
        DNBVD = FLOAT(-6)                                               02110801
        DNAVD = DINT(DNBVD)                                             02120801
           IF (DNAVD + 6.000000003D0) 20080, 10080, 40080               02130801
40080      IF (DNAVD + 5.999999997D0) 10080, 10080, 20080               02140801
10080      IVPASS = IVPASS + 1                                          02150801
           WRITE (NUVI, 80002) IVTNUM                                   02160801
           GO TO 0081                                                   02170801
20080      IVFAIL = IVFAIL + 1                                          02180801
           DVCORR = -6.0D0                                              02190801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    02200801
 0081      CONTINUE                                                     02210801
CT009*  TEST 9             A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+1)   02220801
           IVTNUM = 9                                                   02230801
        DNBVD = -0.375D1                                                02240801
        DNAVD = DINT(DNBVD)                                             02250801
           IF (DNAVD + 3.000000002D0) 20090, 10090, 40090               02260801
40090      IF (DNAVD + 2.999999998D0) 10090, 10090, 20090               02270801
10090      IVPASS = IVPASS + 1                                          02280801
           WRITE (NUVI, 80002) IVTNUM                                   02290801
           GO TO 0091                                                   02300801
20090      IVFAIL = IVFAIL + 1                                          02310801
           DVCORR = -0.3D1                                              02320801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    02330801
 0091      CONTINUE                                                     02340801
CT010*  TEST 10                       ZERO PREFIXED WITH A MINUS SIGN   02350801
           IVTNUM = 10                                                  02360801
        DNBVD = 0.0D0                                                   02370801
        DNAVD = DINT(-DNBVD)                                            02380801
           IF (DNAVD + 5.0D-10) 20100, 10100, 40100                     02390801
40100      IF (DNAVD - 5.0D-10) 10100, 10100, 20100                     02400801
10100      IVPASS = IVPASS + 1                                          02410801
           WRITE (NUVI, 80002) IVTNUM                                   02420801
           GO TO 0101                                                   02430801
20100      IVFAIL = IVFAIL + 1                                          02440801
           DVCORR = 0.0D0                                               02450801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    02460801
 0101      CONTINUE                                                     02470801
CT011*  TEST 11        AN ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   02480801
           IVTNUM = 11                                                  02490801
        DNBVD = 0.375D1                                                 02500801
        DNAVD = DINT(DNBVD/0.375D0)                                     02510801
           IF (DNAVD - 0.9000000000D1) 20110, 10110, 40110              02520*TI
40110      IF (DNAVD - 1.000000001D1) 10110, 10110, 20110               02530801
10110      IVPASS = IVPASS + 1                                          02540801
           WRITE (NUVI, 80002) IVTNUM                                   02550801
           GO TO 0111                                                   02560801
20110      IVFAIL = IVFAIL + 1                                          02570801
           DVCORR = 1.0D1                                               02580801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    02590801
 0111      CONTINUE                                                     02600801
C*****                                                                  02610801
        WRITE (NUVI, 90002)                                             02620801
        WRITE (NUVI, 90013)                                             02630801
        WRITE (NUVI, 90014)                                             02640801
C*****                                                                  02650801
C*****    TEST OF DNINT                                                 02660801
C*****                                                                  02670801
        WRITE(NUVI, 15504)                                              02680801
15504   FORMAT( // 8X, 13HTEST OF DNINT)                                02690801
CT012*  TEST 12                                        THE VALUE ZERO   02700801
           IVTNUM = 12                                                  02710801
        DNBVD = 0.0D0                                                   02720801
        DNAVD = DNINT(DNBVD)                                            02730801
           IF (DNAVD + 5.0D-10) 20120, 10120, 40120                     02740801
40120      IF (DNAVD - 5.0D-10) 10120, 10120, 20120                     02750801
10120      IVPASS = IVPASS + 1                                          02760801
           WRITE (NUVI, 80002) IVTNUM                                   02770801
           GO TO 0121                                                   02780801
20120      IVFAIL = IVFAIL + 1                                          02790801
           DVCORR = 0.0D0                                               02800801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    02810801
 0121      CONTINUE                                                     02820801
CT013*  TEST 13                                     A VALUE IN (0,.5)   02830801
           IVTNUM = 13                                                  02840801
        DNBVD = 0.25D0                                                  02850801
        DNAVD = DNINT(DNBVD)                                            02860801
           IF (DNAVD + 5.0D-10) 20130, 10130, 40130                     02870801
40130      IF (DNAVD - 5.0D-10) 10130, 10130, 20130                     02880801
10130      IVPASS = IVPASS + 1                                          02890801
           WRITE (NUVI, 80002) IVTNUM                                   02900801
           GO TO 0131                                                   02910801
20130      IVFAIL = IVFAIL + 1                                          02920801
           DVCORR = 0.0D0                                               02930801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    02940801
 0131      CONTINUE                                                     02950801
CT014*  TEST 14                                         THE VALUE 0.5   02960801
           IVTNUM = 14                                                  02970801
        DNBVD = FLOAT(1) / FLOAT(2)                                     02980801
        DNAVD = DNINT(DNBVD)                                            02990801
           IF (DNAVD - 0.9999999995D0) 20140, 10140, 40140              03000801
40140      IF (DNAVD - 1.000000001D0) 10140, 10140, 20140               03010801
10140      IVPASS = IVPASS + 1                                          03020801
           WRITE (NUVI, 80002) IVTNUM                                   03030801
           GO TO 0141                                                   03040801
20140      IVFAIL = IVFAIL + 1                                          03050801
           DVCORR = 1.0D0                                               03060801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03070801
 0141      CONTINUE                                                     03080801
CT015*  TEST 15                                     A VALUE IN (.5,1)   03090801
           IVTNUM = 15                                                  03100801
        DNBVD = 0.75D0                                                  03110801
        DNAVD = DNINT(DNBVD)                                            03120801
           IF (DNAVD - 0.9999999995D0) 20150, 10150, 40150              03130801
40150      IF (DNAVD - 1.000000001D0) 10150, 10150, 20150               03140801
10150      IVPASS = IVPASS + 1                                          03150801
           WRITE (NUVI, 80002) IVTNUM                                   03160801
           GO TO 0151                                                   03170801
20150      IVFAIL = IVFAIL + 1                                          03180801
           DVCORR = 1.0D0                                               03190801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03200801
 0151      CONTINUE                                                     03210801
CT016*  TEST 16                     AN INTEGRAL VALUE OTHER THAN 0, 1   03220801
           IVTNUM = 16                                                  03230801
        DNBVD = FLOAT(5)                                                03240801
        DNAVD = DNINT(DNBVD)                                            03250801
           IF (DNAVD - 4.999999997D0) 20160, 10160, 40160               03260801
40160      IF (DNAVD - 5.000000003D0) 10160, 10160, 20160               03270801
10160      IVPASS = IVPASS + 1                                          03280801
           WRITE (NUVI, 80002) IVTNUM                                   03290801
           GO TO 0161                                                   03300801
20160      IVFAIL = IVFAIL + 1                                          03310801
           DVCORR = 5.0D0                                               03320801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03330801
 0161      CONTINUE                                                     03340801
CT017*  TEST 17                                   A VALUE IN (X,X+.5)   03350801
           IVTNUM = 17                                                  03360801
        DNBVD = 10.46875D0                                              03370801
        DNAVD = DNINT(DNBVD)                                            03380801
           IF (DNAVD - 9.999999995D0) 20170, 10170, 40170               03390801
40170      IF (DNAVD - 10.00000001D0) 10170, 10170, 20170               03400801
10170      IVPASS = IVPASS + 1                                          03410801
           WRITE (NUVI, 80002) IVTNUM                                   03420801
           GO TO 0171                                                   03430801
20170      IVFAIL = IVFAIL + 1                                          03440801
           DVCORR = 10.0D0                                              03450801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03460801
 0171      CONTINUE                                                     03470801
CT018*  TEST 18                 A VALUE WITH FRACTIONAL COMPONENT 0.5   03480801
           IVTNUM = 18                                                  03490801
        DNBVD = FLOAT(15) + FLOAT(1) / FLOAT(2)                         03500801
        DNAVD = DNINT(DNBVD)                                            03510801
           IF (DNAVD - 15.99999999D0) 20180, 10180, 40180               03520801
40180      IF (DNAVD - 16.00000001D0) 10180, 10180, 20180               03530801
10180      IVPASS = IVPASS + 1                                          03540801
           WRITE (NUVI, 80002) IVTNUM                                   03550801
           GO TO 0181                                                   03560801
20180      IVFAIL = IVFAIL + 1                                          03570801
           DVCORR = 16.0D0                                              03580801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03590801
 0181      CONTINUE                                                     03600801
CT019*  TEST 19                                 A VALUE IN (X+.5,X+1)   03610801
           IVTNUM = 19                                                  03620801
        DNBVD = 27.96875D0                                              03630801
        DNAVD = DNINT(DNBVD)                                            03640801
           IF (DNAVD - 27.99999998D0) 20190, 10190, 40190               03650801
40190      IF (DNAVD - 28.00000002D0) 10190, 10190, 20190               03660801
10190      IVPASS = IVPASS + 1                                          03670801
           WRITE (NUVI, 80002) IVTNUM                                   03680801
           GO TO 0191                                                   03690801
20190      IVFAIL = IVFAIL + 1                                          03700801
           DVCORR = 28.0D0                                              03710801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03720801
 0191      CONTINUE                                                     03730801
CT020*  TEST 20             A NEGATIVE VALUE WITH MAGNITUDE IN (0,.5)   03740801
           IVTNUM = 20                                                  03750801
        DNBVD = -0.25D0                                                 03760801
        DNAVD = DNINT(DNBVD)                                            03770801
           IF (DNAVD + 5.0D-10) 20200, 10200, 40200                     03780801
40200      IF (DNAVD - 5.0D-10) 10200, 10200, 20200                     03790801
10200      IVPASS = IVPASS + 1                                          03800801
           WRITE (NUVI, 80002) IVTNUM                                   03810801
           GO TO 0201                                                   03820801
20200      IVFAIL = IVFAIL + 1                                          03830801
           DVCORR = 0.0D0                                               03840801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03850801
 0201      CONTINUE                                                     03860801
CT021*  TEST 21                                        THE VALUE -0.5   03870801
           IVTNUM = 21                                                  03880801
        DNBVD = -FLOAT(1) / FLOAT(2)                                    03890801
        DNAVD = DNINT(DNBVD)                                            03900801
           IF (DNAVD + 1.000000001D0) 20210, 10210, 40210               03910801
40210      IF (DNAVD + 0.9999999995D0) 10210, 10210, 20210              03920801
10210      IVPASS = IVPASS + 1                                          03930801
           WRITE (NUVI, 80002) IVTNUM                                   03940801
           GO TO 0211                                                   03950801
20210      IVFAIL = IVFAIL + 1                                          03960801
           DVCORR = -1.0D0                                              03970801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    03980801
 0211      CONTINUE                                                     03990801
CT022*  TEST 22             A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1)   04000801
           IVTNUM = 22                                                  04010801
        DNBVD = -0.75D0                                                 04020801
        DNAVD = DNINT(DNBVD)                                            04030801
           IF (DNAVD + 1.000000001D0) 20220, 10220, 40220               04040801
40220      IF (DNAVD + 0.9999999995D0) 10220, 10220, 20220              04050801
10220      IVPASS = IVPASS + 1                                          04060801
           WRITE (NUVI, 80002) IVTNUM                                   04070801
           GO TO 0221                                                   04080801
20220      IVFAIL = IVFAIL + 1                                          04090801
           DVCORR = -1.0D0                                              04100801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    04110801
 0221      CONTINUE                                                     04120801
CT023*  TEST 23                             A NEGATIVE INTEGRAL VALUE   04130801
           IVTNUM = 23                                                  04140801
        DNBVD = -FLOAT(5)                                               04150801
        DNAVD = DNINT(DNBVD)                                            04160801
           IF (DNAVD + 5.000000003D0) 20230, 10230, 40230               04170801
40230      IF (DNAVD + 4.999999997D0) 10230, 10230, 20230               04180801
10230      IVPASS = IVPASS + 1                                          04190801
           WRITE (NUVI, 80002) IVTNUM                                   04200801
           GO TO 0231                                                   04210801
20230      IVFAIL = IVFAIL + 1                                          04220801
           DVCORR = -5.0D0                                              04230801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    04240801
 0231      CONTINUE                                                     04250801
CT024*  TEST 24           A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5)   04260801
           IVTNUM = 24                                                  04270801
        DNBVD = -10.46875D0                                             04280801
        DNAVD = DNINT(DNBVD)                                            04290801
           IF (DNAVD + 10.00000001D0) 20240, 10240, 40240               04300801
40240      IF (DNAVD + 9.999999995D0) 10240, 10240, 20240               04310801
10240      IVPASS = IVPASS + 1                                          04320801
           WRITE (NUVI, 80002) IVTNUM                                   04330801
           GO TO 0241                                                   04340801
20240      IVFAIL = IVFAIL + 1                                          04350801
           DVCORR = -10.0D0                                             04360801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    04370801
 0241      CONTINUE                                                     04380801
CT025*  TEST 25        A NEGATIVE VALUE WITH FRACTIONAL COMPONENT 0.5   04390801
           IVTNUM = 25                                                  04400801
        DNBVD = FLOAT(-15) - FLOAT(1) / FLOAT(2)                        04410801
        DNAVD = DNINT(DNBVD)                                            04420801
           IF (DNAVD + 16.00000001D0) 20250, 10250, 40250               04430801
40250      IF (DNAVD + 15.99999999D0) 10250, 10250, 20250               04440801
10250      IVPASS = IVPASS + 1                                          04450801
           WRITE (NUVI, 80002) IVTNUM                                   04460801
           GO TO 0251                                                   04470801
20250      IVFAIL = IVFAIL + 1                                          04480801
           DVCORR = -16.0D0                                             04490801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    04500801
 0251      CONTINUE                                                     04510801
CT026*  TEST 26         A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1)   04520801
           IVTNUM = 26                                                  04530801
        DNBVD = -27.96875D0                                             04540801
        DNAVD = DNINT(DNBVD)                                            04550801
           IF (DNAVD + 28.00000002D0) 20260, 10260, 40260               04560801
40260      IF (DNAVD + 27.99999998D0) 10260, 10260, 20260               04570801
10260      IVPASS = IVPASS + 1                                          04580801
           WRITE (NUVI, 80002) IVTNUM                                   04590801
           GO TO 0261                                                   04600801
20260      IVFAIL = IVFAIL + 1                                          04610801
           DVCORR = -28.0D0                                             04620801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    04630801
 0261      CONTINUE                                                     04640801
CT027*  TEST 27                       ZERO PREFIXED WITH A MINUS SIGN   04650801
           IVTNUM = 27                                                  04660801
        DNBVD = 0.0D0                                                   04670801
        DNAVD = DNINT(-DNBVD)                                           04680801
           IF (DNAVD + 5.0D-10) 20270, 10270, 40270                     04690801
40270      IF (DNAVD - 5.0D-10) 10270, 10270, 20270                     04700801
10270      IVPASS = IVPASS + 1                                          04710801
           WRITE (NUVI, 80002) IVTNUM                                   04720801
           GO TO 0271                                                   04730801
20270      IVFAIL = IVFAIL + 1                                          04740801
           DVCORR = 0.0D0                                               04750801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    04760801
 0271      CONTINUE                                                     04770801
CT028*  TEST 28        AN ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   04780801
           IVTNUM = 28                                                  04790801
        DNBVD = 8.00D0                                                  04800801
        DNDVD = 7.25D0                                                  04810801
        DNAVD = DNINT(DNBVD - DNDVD)                                    04820801
           IF (DNAVD - 0.9999999995D0) 20280, 10280, 40280              04830801
40280      IF (DNAVD - 1.000000001D0) 10280, 10280, 20280               04840801
10280      IVPASS = IVPASS + 1                                          04850801
           WRITE (NUVI, 80002) IVTNUM                                   04860801
           GO TO 0281                                                   04870801
20280      IVFAIL = IVFAIL + 1                                          04880801
           DVCORR = 1.0D0                                               04890801
           WRITE (NUVI, 80031) IVTNUM, DNAVD, DVCORR                    04900801
 0281      CONTINUE                                                     04910801
C*****                                                                  04920801
        WRITE (NUVI, 90002)                                             04930801
        WRITE (NUVI, 90013)                                             04940801
        WRITE (NUVI, 90014)                                             04950801
C*****                                                                  04960801
C*****    TEST OF IDNINT                                                04970801
C*****                                                                  04980801
C*****                                                                  04990801
        WRITE(NUVI, 15506)                                              05000801
15506   FORMAT( // 8X, 14HTEST OF IDNINT)                               05010801
CT029*  TEST 29                                      THE VALUE ZERO     05020801
           IVTNUM = 29                                                  05030801
        DNBVD = 0.0D0                                                   05040801
        INAVI = IDNINT(DNBVD)                                           05050801
           IF (INAVI - 0) 20290, 10290, 20290                           05060801
10290      IVPASS = IVPASS + 1                                          05070801
           WRITE (NUVI, 80002) IVTNUM                                   05080801
           GO TO 0291                                                   05090801
20290      IVFAIL = IVFAIL + 1                                          05100801
           IVCORR = 0                                                   05110801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05120801
 0291      CONTINUE                                                     05130801
CT030*  TEST 30                                     A VALUE IN (0.,5)   05140801
           IVTNUM = 30                                                  05150801
        DNBVD = 0.25D0                                                  05160801
        INAVI = IDNINT(DNBVD)                                           05170801
           IF (INAVI - 0) 20300, 10300, 20300                           05180801
10300      IVPASS = IVPASS + 1                                          05190801
           WRITE (NUVI, 80002) IVTNUM                                   05200801
           GO TO 0301                                                   05210801
20300      IVFAIL = IVFAIL + 1                                          05220801
           IVCORR = 0                                                   05230801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05240801
 0301      CONTINUE                                                     05250801
CT031*  TEST 31                                          THE VALUE 0.5  05260801
           IVTNUM = 31                                                  05270801
        DNBVD = FLOAT(1) / FLOAT(2)                                     05280801
        INAVI = IDNINT(DNBVD)                                           05290801
           IF (INAVI - 1) 20310, 10310, 20310                           05300801
10310      IVPASS = IVPASS + 1                                          05310801
           WRITE (NUVI, 80002) IVTNUM                                   05320801
           GO TO 0311                                                   05330801
20310      IVFAIL = IVFAIL + 1                                          05340801
           IVCORR = 1                                                   05350801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05360801
 0311      CONTINUE                                                     05370801
CT032*  TEST 32                                     A VALUE IN (.5,1)   05380801
           IVTNUM = 32                                                  05390801
        DNBVD = 0.75D0                                                  05400801
        INAVI = IDNINT(DNBVD)                                           05410801
           IF (INAVI - 1) 20320, 10320, 20320                           05420801
10320      IVPASS = IVPASS + 1                                          05430801
           WRITE (NUVI, 80002) IVTNUM                                   05440801
           GO TO 0321                                                   05450801
20320      IVFAIL = IVFAIL + 1                                          05460801
           IVCORR = 1                                                   05470801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05480801
 0321      CONTINUE                                                     05490801
CT033*  TEST 33                     AN INTEGRAL VALUE OTHER THAN 0, 1   05500801
           IVTNUM = 33                                                  05510801
        DNBVD = FLOAT(5)                                                05520801
        INAVI = IDNINT(DNBVD)                                           05530801
           IF (INAVI - 5) 20330, 10330, 20330                           05540801
10330      IVPASS = IVPASS + 1                                          05550801
           WRITE (NUVI, 80002) IVTNUM                                   05560801
           GO TO 0331                                                   05570801
20330      IVFAIL = IVFAIL + 1                                          05580801
           IVCORR = 5                                                   05590801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05600801
 0331      CONTINUE                                                     05610801
CT034*  TEST 34                                     A VALUE IN (X,X+.5) 05620801
           IVTNUM = 34                                                  05630801
        DNBVD = 10.46875D0                                              05640801
        INAVI = IDNINT(DNBVD)                                           05650801
           IF (INAVI - 10) 20340, 10340, 20340                          05660801
10340      IVPASS = IVPASS + 1                                          05670801
           WRITE (NUVI, 80002) IVTNUM                                   05680801
           GO TO 0341                                                   05690801
20340      IVFAIL = IVFAIL + 1                                          05700801
           IVCORR = 10                                                  05710801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05720801
 0341      CONTINUE                                                     05730801
CT035*  TEST 35                 A VALUE WITH FRACTIONAL COMPONENT 0.5   05740801
           IVTNUM = 35                                                  05750801
        DNBVD = FLOAT(15) + FLOAT(1) / FLOAT(2)                         05760801
        INAVI = IDNINT(DNBVD)                                           05770801
           IF (INAVI - 16) 20350, 10350, 20350                          05780801
10350      IVPASS = IVPASS + 1                                          05790801
           WRITE (NUVI, 80002) IVTNUM                                   05800801
           GO TO 0351                                                   05810801
20350      IVFAIL = IVFAIL + 1                                          05820801
           IVCORR = 16                                                  05830801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05840801
 0351      CONTINUE                                                     05850801
CT036*  TEST 36                                 A VALUE IN (X+.5,X+1)   05860801
           IVTNUM = 36                                                  05870801
        DNBVD = 27.96875D0                                              05880801
        INAVI = IDNINT(DNBVD)                                           05890801
           IF (INAVI - 28) 20360, 10360, 20360                          05900801
10360      IVPASS = IVPASS + 1                                          05910801
           WRITE (NUVI, 80002) IVTNUM                                   05920801
           GO TO 0361                                                   05930801
20360      IVFAIL = IVFAIL + 1                                          05940801
           IVCORR = 28                                                  05950801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    05960801
 0361      CONTINUE                                                     05970801
CT037*  TEST 37             A NEGATIVE VALUE WITH MAGNITUDE IN (0,.5)   05980801
           IVTNUM = 37                                                  05990801
        DNBVD = -0.25D0                                                 06000801
        INAVI = IDNINT(DNBVD)                                           06010801
           IF (INAVI - 0) 20370, 10370, 20370                           06020801
10370      IVPASS = IVPASS + 1                                          06030801
           WRITE (NUVI, 80002) IVTNUM                                   06040801
           GO TO 0371                                                   06050801
20370      IVFAIL = IVFAIL + 1                                          06060801
           IVCORR = 0                                                   06070801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06080801
 0371      CONTINUE                                                     06090801
CT038*  TEST 38                                        THE VALUE -0.5   06100801
           IVTNUM = 38                                                  06110801
        DNBVD = -FLOAT(1) / FLOAT(2)                                    06120801
        INAVI = IDNINT(DNBVD)                                           06130801
           IF (INAVI + 1) 20380, 10380, 20380                           06140801
10380      IVPASS = IVPASS + 1                                          06150801
           WRITE (NUVI, 80002) IVTNUM                                   06160801
           GO TO 0381                                                   06170801
20380      IVFAIL = IVFAIL + 1                                          06180801
           IVCORR = -1                                                  06190801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06200801
 0381      CONTINUE                                                     06210801
CT039*  TEST 39             A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1)   06220801
           IVTNUM = 39                                                  06230801
        DNBVD = -0.75D0                                                 06240801
        INAVI = IDNINT(DNBVD)                                           06250801
           IF (INAVI + 1) 20390, 10390, 20390                           06260801
10390      IVPASS = IVPASS + 1                                          06270801
           WRITE (NUVI, 80002) IVTNUM                                   06280801
           GO TO 0391                                                   06290801
20390      IVFAIL = IVFAIL + 1                                          06300801
           IVCORR = -1                                                  06310801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06320801
 0391      CONTINUE                                                     06330801
CT040*  TEST 40                             A NEGATIVE INTEGRAL VALUE   06340801
           IVTNUM = 40                                                  06350801
        DNBVD = -FLOAT(5)                                               06360801
        INAVI = IDNINT(DNBVD)                                           06370801
           IF (INAVI + 5) 20400, 10400, 20400                           06380801
10400      IVPASS = IVPASS + 1                                          06390801
           WRITE (NUVI, 80002) IVTNUM                                   06400801
           GO TO 0401                                                   06410801
20400      IVFAIL = IVFAIL + 1                                          06420801
           IVCORR = -5                                                  06430801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06440801
 0401      CONTINUE                                                     06450801
CT041*  TEST 41           A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5)   06460801
           IVTNUM = 41                                                  06470801
        DNBVD = -10.46875D0                                             06480801
        INAVI = IDNINT(DNBVD)                                           06490801
           IF (INAVI + 10) 20410, 10410, 20410                          06500801
10410      IVPASS = IVPASS + 1                                          06510801
           WRITE (NUVI, 80002) IVTNUM                                   06520801
           GO TO 0411                                                   06530801
20410      IVFAIL = IVFAIL + 1                                          06540801
           IVCORR = -10                                                 06550801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06560801
 0411      CONTINUE                                                     06570801
CT042*  TEST 42        A NEGATIVE VALUE WITH FRACTIONAL COMPONENT 0.5   06580801
           IVTNUM = 42                                                  06590801
        DNBVD = FLOAT(-15) - FLOAT(1) /FLOAT(2)                         06600801
        INAVI = IDNINT(DNBVD)                                           06610801
           IF (INAVI + 16) 20420, 10420, 20420                          06620801
10420      IVPASS = IVPASS + 1                                          06630801
           WRITE (NUVI, 80002) IVTNUM                                   06640801
           GO TO 0421                                                   06650801
20420      IVFAIL = IVFAIL + 1                                          06660801
           IVCORR = -16                                                 06670801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06680801
 0421      CONTINUE                                                     06690801
CT043*  TEST 43         A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1)   06700801
           IVTNUM = 43                                                  06710801
        DNBVD = -27.96875D0                                             06720801
        INAVI = IDNINT(DNBVD)                                           06730801
           IF (INAVI + 28) 20430, 10430, 20430                          06740801
10430      IVPASS = IVPASS + 1                                          06750801
           WRITE (NUVI, 80002) IVTNUM                                   06760801
           GO TO 0431                                                   06770801
20430      IVFAIL = IVFAIL + 1                                          06780801
           IVCORR = -28                                                 06790801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06800801
 0431      CONTINUE                                                     06810801
CT044*  TEST 44                       ZERO PREFIXED WITH A MINUS SIGN   06820801
           IVTNUM = 44                                                  06830801
        DNBVD = 0.0D0                                                   06840801
        INAVI = IDNINT(-DNBVD)                                          06850801
           IF (INAVI - 0) 20440, 10440, 20440                           06860801
10440      IVPASS = IVPASS + 1                                          06870801
           WRITE (NUVI, 80002) IVTNUM                                   06880801
           GO TO 0441                                                   06890801
20440      IVFAIL = IVFAIL + 1                                          06900801
           IVCORR = 0                                                   06910801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    06920801
 0441      CONTINUE                                                     06930801
CT045*  TEST 45        AN ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   06940801
           IVTNUM = 45                                                  06950801
        DNBVD = 8.00D0                                                  06960801
        DNDVD = 7.25D0                                                  06970801
        INAVI = IDNINT(DNBVD - DNDVD)                                   06980801
           IF (INAVI - 1) 20450, 10450, 20450                           06990801
10450      IVPASS = IVPASS + 1                                          07000801
           WRITE (NUVI, 80002) IVTNUM                                   07010801
           GO TO 0451                                                   07020801
20450      IVFAIL = IVFAIL + 1                                          07030801
           IVCORR = 1                                                   07040801
           WRITE (NUVI, 80010) IVTNUM, INAVI, IVCORR                    07050801
 0451      CONTINUE                                                     07060801
C*****                                                                  07070801
CBB** ********************** BBCSUM0  **********************************07080801
C**** WRITE OUT TEST SUMMARY                                            07090801
C****                                                                   07100801
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07110801
      WRITE (I02, 90004)                                                07120801
      WRITE (I02, 90014)                                                07130801
      WRITE (I02, 90004)                                                07140801
      WRITE (I02, 90020) IVPASS                                         07150801
      WRITE (I02, 90022) IVFAIL                                         07160801
      WRITE (I02, 90024) IVDELE                                         07170801
      WRITE (I02, 90026) IVINSP                                         07180801
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 07190801
CBE** ********************** BBCSUM0  **********************************07200801
CBB** ********************** BBCFOOT0 **********************************07210801
C**** WRITE OUT REPORT FOOTINGS                                         07220801
C****                                                                   07230801
      WRITE (I02,90016) ZPROG, ZPROG                                    07240801
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     07250801
      WRITE (I02,90019)                                                 07260801
CBE** ********************** BBCFOOT0 **********************************07270801
CBB** ********************** BBCFMT0A **********************************07280801
C**** FORMATS FOR TEST DETAIL LINES                                     07290801
C****                                                                   07300801
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           07310801
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           07320801
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           07330801
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           07340801
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           07350801
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    07360801
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07370801
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              07380801
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07390801
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  07400801
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         07410801
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         07420801
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         07430801
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         07440801
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      07450801
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      07460801
80050 FORMAT (1H ,48X,A31)                                              07470801
CBE** ********************** BBCFMT0A **********************************07480801
CBB** ********************** BBCFMAT1 **********************************07490801
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     07500801
C****                                                                   07510801
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07520801
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            07530801
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     07540801
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     07550801
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    07560801
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    07570801
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    07580801
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    07590801
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07600801
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  07610801
     21H(,F12.5,2H, ,F12.5,1H))                                         07620801
CBE** ********************** BBCFMAT1 **********************************07630801
CBB** ********************** BBCFMT0B **********************************07640801
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                07650801
C****                                                                   07660801
90002 FORMAT (1H1)                                                      07670801
90004 FORMAT (1H )                                                      07680801
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               07690801
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07700801
90008 FORMAT (1H ,21X,A13,A17)                                          07710801
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       07720801
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    07730801
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     07740801
     1       7X,7HREMARKS,24X)                                          07750801
90014 FORMAT (1H ,46H----------------------------------------------,    07760801
     1        33H---------------------------------)                     07770801
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               07780801
C****                                                                   07790801
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             07800801
C****                                                                   07810801
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          07820801
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        07830801
     1        A13)                                                      07840801
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 07850801
C****                                                                   07860801
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 07870801
C****                                                                   07880801
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              07890801
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              07900801
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             07910801
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  07920801
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  07930801
CBE** ********************** BBCFMT0B **********************************07940801
C*****                                                                  07950801
C*****    END OF TEST SEGMENT 155                                       07960801
        STOP                                                            07970801
        END                                                             07980801
                                                                        07990801
