C***********************************************************************00010355
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020355
C*****   FM355               XAINT - (154)                              00030355
C*****                                                                  00040355
C***********************************************************************00050355
C*****  GENERAL PURPOSE                                       SUBSET REF00060355
C*****    TEST INTRINSIC FUNCTIONS AINT, ANINT, NINT             15.3   00070355
C*****    TRUNCATION (SIGN OF A * LARGEST INTEGER LE ABS(A) )  (TABLE 5)00080355
C*****                                                                  00090355
C*****   GENERAL COMMENTS                                               00100355
C*****         FLOAT FUNCTION ASSUMED WORKING                           00110355
C*****                                                                  00120355
CBB** ********************** BBCCOMNT **********************************00130355
C****                                                                   00140355
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150355
C****                          VERSION 2.0                              00160355
C****                                                                   00170355
C****                                                                   00180355
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190355
C****                   GENERAL SERVICES ADMINISTRATION                 00200355
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210355
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220355
C****                      FALLS CHURCH, VA. 22041                      00230355
C****                                                                   00240355
C****                          (703) 756-6153                           00250355
C****                                                                   00260355
CBE** ********************** BBCCOMNT **********************************00270355
CBB** ********************** BBCINITA **********************************00280355
C**** SPECIFICATION STATEMENTS                                          00290355
C****                                                                   00300355
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00310355
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00320355
CBE** ********************** BBCINITA **********************************00330355
CBB** ********************** BBCINITB **********************************00340355
C**** INITIALIZE SECTION                                                00350355
      DATA  ZVERS,                  ZVERSD,             ZDATE           00360355
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00370355
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00380355
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00390355
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00400355
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00410355
      DATA   REMRKS /'                               '/                 00420355
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00430355
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00440355
C****                                                                   00450355
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00460355
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00470355
CZ03  ZPROG  = 'PROGRAM NAME'                                           00480355
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00550355
      IVPASS = 0                                                        00560355
      IVFAIL = 0                                                        00570355
      IVDELE = 0                                                        00580355
      IVINSP = 0                                                        00590355
      IVTOTL = 0                                                        00600355
      IVTOTN = 0                                                        00610355
      ICZERO = 0                                                        00620355
C                                                                       00630355
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640355
      I01 = 05                                                          00650355
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660355
      I02 = 06                                                          00670355
C                                                                       00680355
      I01 = 5                                                           00690355
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700355
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00710355
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00720355
C                                                                       00730355
      I02 = 6                                                           00740355
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00750355
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00760355
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00770355
C                                                                       00780355
CBE** ********************** BBCINITB **********************************00790355
      NUVI = I02                                                        00800355
      IVTOTL = 48                                                       00810355
      ZPROG = 'FM355'                                                   00820355
CBB** ********************** BBCHED0A **********************************00830355
C****                                                                   00840355
C**** WRITE REPORT TITLE                                                00850355
C****                                                                   00860355
      WRITE (I02, 90002)                                                00870355
      WRITE (I02, 90006)                                                00880355
      WRITE (I02, 90007)                                                00890355
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00900355
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00910355
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00920355
CBE** ********************** BBCHED0A **********************************00930355
C*****                                                                  00940355
C*****    HEADER FOR SEGMENT 154                                        00950355
        WRITE (NUVI,15401)                                              00960355
15401   FORMAT (1H , //  2X,35HXAINT - (154) INTRINSIC FUNCTIONS--//10X,00970355
     1          36HAINT, ANINT, NINT (TYPE CONVERSION)  //              00980355
     2          20H  SUBSET REF. - 15.3)                                00990355
CBB** ********************** BBCHED0B **********************************01000355
C**** WRITE DETAIL REPORT HEADERS                                       01010355
C****                                                                   01020355
      WRITE (I02,90004)                                                 01030355
      WRITE (I02,90004)                                                 01040355
      WRITE (I02,90013)                                                 01050355
      WRITE (I02,90014)                                                 01060355
      WRITE (I02,90015) IVTOTL                                          01070355
CBE** ********************** BBCHED0B **********************************01080355
C*****                                                                  01090355
C*****    TEST OF AINT                                                  01100355
C*****                                                                  01110355
        WRITE(NUVI, 15402)                                              01120355
15402   FORMAT (/ 8X, 12HTEST OF AINT)                                  01130355
CT001*  TEST 1                                           THE VALUE ZERO 01140355
           IVTNUM = 1                                                   01150355
        RCBVS = 0.0                                                     01160355
        RCAVS = AINT(RCBVS)                                             01170355
           IF (RCAVS + 0.00005) 20010, 10010, 40010                     01180355
40010      IF (RCAVS - 0.00005) 10010, 10010, 20010                     01190355
10010      IVPASS = IVPASS + 1                                          01200355
           WRITE (NUVI, 80002) IVTNUM                                   01210355
           GO TO 0011                                                   01220355
20010      IVFAIL = IVFAIL + 1                                          01230355
           RVCORR = 0.0                                                 01240355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     01250355
 0011      CONTINUE                                                     01260355
CT002*  TEST 2                          ZERO PREFIXED WITH A MINUS SIGN 01270355
           IVTNUM = 2                                                   01280355
        RCDVS = -0.0                                                    01290355
        RCAVS = AINT(RCBVS)                                             01300355
           IF (RCAVS + 0.00005) 20020, 10020, 40020                     01310355
40020      IF (RCAVS - 0.00005) 10020, 10020, 20020                     01320355
10020      IVPASS = IVPASS + 1                                          01330355
           WRITE (NUVI, 80002) IVTNUM                                   01340355
           GO TO 0021                                                   01350355
20020      IVFAIL = IVFAIL + 1                                          01360355
           RVCORR = -0.0                                                01370355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     01380355
 0021      CONTINUE                                                     01390355
CT003*  TEST 3                                         A VALUE IN (0,1) 01400355
           IVTNUM = 3                                                   01410355
        RCDVS = 0.375                                                   01420355
        RCAVS = AINT(RCBVS)                                             01430355
           IF (RCAVS + 0.00005) 20030, 10030, 40030                     01440355
40030      IF (RCAVS - 0.00005) 10030, 10030, 20030                     01450355
10030      IVPASS = IVPASS + 1                                          01460355
           WRITE (NUVI, 80002) IVTNUM                                   01470355
           GO TO 0031                                                   01480355
20030      IVFAIL = IVFAIL + 1                                          01490355
           RVCORR = 0.0                                                 01500355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     01510355
 0031      CONTINUE                                                     01520355
CT004*  TEST 4                                              THE VALUE 1 01530355
           IVTNUM = 4                                                   01540355
        RCBVS = FLOAT(1)                                                01550355
        RCAVS = AINT(RCBVS)                                             01560355
           IF (RCAVS - 0.99995) 20040, 10040, 40040                     01570355
40040      IF (RCAVS - 1.0001) 10040, 10040, 20040                      01580355
10040      IVPASS = IVPASS + 1                                          01590355
           WRITE (NUVI, 80002) IVTNUM                                   01600355
           GO TO 0041                                                   01610355
20040      IVFAIL = IVFAIL + 1                                          01620355
           RVCORR = 1.0                                                 01630355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     01640355
 0041      CONTINUE                                                     01650355
CT005*  TEST 5                        AN INTEGRAL VALUE OTHER THAN 0, 1 01660355
           IVTNUM = 5                                                   01670355
        RCBVS = FLOAT(6)                                                01680355
        RCAVS = AINT(RCBVS)                                             01690355
           IF (RCAVS - 5.9997) 20050, 10050, 40050                      01700355
40050      IF (RCAVS - 6.0003) 10050, 10050, 20050                      01710355
10050      IVPASS = IVPASS + 1                                          01720355
           WRITE (NUVI, 80002) IVTNUM                                   01730355
           GO TO 0051                                                   01740355
20050      IVFAIL = IVFAIL + 1                                          01750355
           RVCORR = 6.0                                                 01760355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     01770355
 0051      CONTINUE                                                     01780355
CT006*  TEST 6                                       A VALUE IN (X,X+1) 01790355
           IVTNUM = 6                                                   01800355
        RCBVS = 3.75                                                    01810355
        RCAVS = AINT(RCBVS)                                             01820355
           IF (RCAVS - 2.9998) 20060, 10060, 40060                      01830355
40060      IF (RCAVS - 3.0002) 10060, 10060, 20060                      01840355
10060      IVPASS = IVPASS + 1                                          01850355
           WRITE (NUVI, 80002) IVTNUM                                   01860355
           GO TO 0061                                                   01870355
20060      IVFAIL = IVFAIL + 1                                          01880355
           RVCORR = 3.0                                                 01890355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     01900355
 0061      CONTINUE                                                     01910355
CT007*  TEST 7                 A NEGATIVE VALUE WITH MAGNITUDE IN (0,1) 01920355
           IVTNUM = 7                                                   01930355
        RCBVS = -0.375                                                  01940355
        RCAVS = AINT(RCBVS)                                             01950355
           IF (RCAVS + 0.00005) 20070, 10070, 40070                     01960355
40070      IF (RCAVS - 0.00005) 10070, 10070, 20070                     01970355
10070      IVPASS = IVPASS + 1                                          01980355
           WRITE (NUVI, 80002) IVTNUM                                   01990355
           GO TO 0071                                                   02000355
20070      IVFAIL = IVFAIL + 1                                          02010355
           RVCORR = 0.0                                                 02020355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     02030355
 0071      CONTINUE                                                     02040355
CT008*  TEST 8                                             THE VALUE -1 02050355
           IVTNUM = 8                                                   02060355
        RCBVS = FLOAT(-1)                                               02070355
        RCAVS = AINT(RCBVS)                                             02080355
           IF (RCAVS + 1.0001) 20080, 10080, 40080                      02090355
40080      IF (RCAVS + 0.99995) 10080, 10080, 20080                     02100355
10080      IVPASS = IVPASS + 1                                          02110355
           WRITE (NUVI, 80002) IVTNUM                                   02120355
           GO TO 0081                                                   02130355
20080      IVFAIL = IVFAIL + 1                                          02140355
           RVCORR = -1.0                                                02150355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     02160355
 0081      CONTINUE                                                     02170355
CT009*  TEST 9                                A NEGATIVE INTEGRAL VALUE 02180355
           IVTNUM = 9                                                   02190355
        RCBVS = FLOAT(-6)                                               02200355
        RCAVS = AINT(RCBVS)                                             02210355
           IF (RCAVS + 6.0003) 20090, 10090, 40090                      02220355
40090      IF (RCAVS + 5.9997) 10090, 10090, 20090                      02230355
10090      IVPASS = IVPASS + 1                                          02240355
           WRITE (NUVI, 80002) IVTNUM                                   02250355
           GO TO 0091                                                   02260355
20090      IVFAIL = IVFAIL + 1                                          02270355
           RVCORR = -6.0                                                02280355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     02290355
 0091      CONTINUE                                                     02300355
CT010*  TEST 10              A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+1) 02310355
           IVTNUM = 10                                                  02320355
        RCBVS = -3.75                                                   02330355
        RCAVS = AINT(RCBVS)                                             02340355
           IF (RCAVS + 3.0002) 20100, 10100, 40100                      02350355
40100      IF (RCAVS + 2.9998) 10100, 10100, 20100                      02360355
10100      IVPASS = IVPASS + 1                                          02370355
           WRITE (NUVI, 80002) IVTNUM                                   02380355
           GO TO 0101                                                   02390355
20100      IVFAIL = IVFAIL + 1                                          02400355
           RVCORR = -3.0                                                02410355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     02420355
 0101      CONTINUE                                                     02430355
CT011*  TEST 11              AN ARITHMETIC EXPRESSION PRESENTED TO AINT 02440355
           IVTNUM = 11                                                  02450355
        RCBVS = 3.25                                                    02460355
        RCDVS = 3.0                                                     02470355
        RCAVS = AINT(FLOAT(25) + RCDVS * RCBVS)                         02480355
           IF (RCAVS - 33.998) 20110, 10110, 40110                      02490355
40110      IF (RCAVS - 34.002) 10110, 10110, 20110                      02500355
10110      IVPASS = IVPASS + 1                                          02510355
           WRITE (NUVI, 80002) IVTNUM                                   02520355
           GO TO 0111                                                   02530355
20110      IVFAIL = IVFAIL + 1                                          02540355
           RVCORR = 34.0                                                02550355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     02560355
 0111      CONTINUE                                                     02570355
CT012*  TEST 12                            AN ARGUMENT OF LOW MAGNITUDE 02580355
           IVTNUM = 12                                                  02590355
        RCBVS = 3.7521E-36                                              02600355
        RCAVS = AINT(RCBVS)                                             02610355
           IF (RCAVS + 0.00005) 20120, 10120, 40120                     02620355
40120      IF (RCAVS - 0.00005) 10120, 10120, 20120                     02630355
10120      IVPASS = IVPASS + 1                                          02640355
           WRITE (NUVI, 80002) IVTNUM                                   02650355
           GO TO 0121                                                   02660355
20120      IVFAIL = IVFAIL + 1                                          02670355
           RVCORR = 0.0                                                 02680355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     02690355
 0121      CONTINUE                                                     02700355
C*****                                                                  02710355
        WRITE(NUVI, 90002)                                              02720355
        WRITE(NUVI, 90013)                                              02730355
        WRITE(NUVI, 90014)                                              02740355
C*****                                                                  02750355
C*****    TEST OF ANINT                                                 02760355
C*****                                                                  02770355
        WRITE(NUVI, 15404)                                              02780355
15404   FORMAT (/ 08X, 13HTEST OF ANINT)                                02790355
C*****                                                                  02800355
CT013*  TEST 13                                          THE VALUE ZERO 02810355
           IVTNUM = 13                                                  02820355
        RCBVS = 0.0                                                     02830355
        RCAVS = ANINT(RCBVS)                                            02840355
           IF (RCAVS + 0.00005) 20130, 10130, 40130                     02850355
40130      IF (RCAVS - 0.00005) 10130, 10130, 20130                     02860355
10130      IVPASS = IVPASS + 1                                          02870355
           WRITE (NUVI, 80002) IVTNUM                                   02880355
           GO TO 0131                                                   02890355
20130      IVFAIL = IVFAIL + 1                                          02900355
           RVCORR = 0.0                                                 02910355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     02920355
 0131      CONTINUE                                                     02930355
CT014*  TEST 14               THE VALUE ZERO PREFIXED WITH A MINUS SIGN 02940355
           IVTNUM = 14                                                  02950355
        RCDVS = 0.0                                                     02960355
        RCAVS = ANINT(-RCBVS)                                           02970355
           IF (RCAVS + 0.00005) 20140, 10140, 40140                     02980355
40140      IF (RCAVS - 0.00005) 10140, 10140, 20140                     02990355
10140      IVPASS = IVPASS + 1                                          03000355
           WRITE (NUVI, 80002) IVTNUM                                   03010355
           GO TO 0141                                                   03020355
20140      IVFAIL = IVFAIL + 1                                          03030355
           RVCORR = 0.0                                                 03040355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03050355
 0141      CONTINUE                                                     03060355
CT015*  TEST 15                                       A VALUE IN (0,.5) 03070355
           IVTNUM = 15                                                  03080355
        RCBVS = 0.25                                                    03090355
        RCAVS = ANINT(RCBVS)                                            03100355
           IF (RCAVS + 0.00005) 20150, 10150, 40150                     03110355
40150      IF (RCAVS - 0.00005) 10150, 10150, 20150                     03120355
10150      IVPASS = IVPASS + 1                                          03130355
           WRITE (NUVI, 80002) IVTNUM                                   03140355
           GO TO 0151                                                   03150355
20150      IVFAIL = IVFAIL + 1                                          03160355
           RVCORR = 0.0                                                 03170355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03180355
 0151      CONTINUE                                                     03190355
CT016*  TEST 16                                           THE VALUE 0.5 03200355
           IVTNUM = 16                                                  03210355
        RCBVS = FLOAT(1) / FLOAT(2)                                     03220355
        RCAVS = ANINT(RCBVS)                                            03230355
           IF (RCAVS - 0.99995) 20160, 10160, 40160                     03240355
40160      IF (RCAVS - 1.0001) 10160, 10160, 20160                      03250355
10160      IVPASS = IVPASS + 1                                          03260355
           WRITE (NUVI, 80002) IVTNUM                                   03270355
           GO TO 0161                                                   03280355
20160      IVFAIL = IVFAIL + 1                                          03290355
           RVCORR = 1.0                                                 03300355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03310355
 0161      CONTINUE                                                     03320355
CT017*  TEST 17                                       A VALUE IN (.5,1) 03330355
           IVTNUM = 17                                                  03340355
        RCBVS = 0.75                                                    03350355
        RCAVS = ANINT(RCBVS)                                            03360355
           IF (RCAVS - 0.99995) 20170, 10170, 40170                     03370355
40170      IF (RCAVS - 1.0001) 10170, 10170, 20170                      03380355
10170      IVPASS = IVPASS + 1                                          03390355
           WRITE (NUVI, 80002) IVTNUM                                   03400355
           GO TO 0171                                                   03410355
20170      IVFAIL = IVFAIL + 1                                          03420355
           RVCORR = 1.0                                                 03430355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03440355
 0171      CONTINUE                                                     03450355
CT018*  TEST 18                        AN INTEGRAL VALUE OTHER THAN 0,1 03460355
           IVTNUM = 18                                                  03470355
        RCBVS = FLOAT(5)                                                03480355
        RCAVS = ANINT(RCBVS)                                            03490355
           IF (RCAVS - 4.9997) 20180, 10180, 40180                      03500355
40180      IF (RCAVS - 5.0003) 10180, 10180, 20180                      03510355
10180      IVPASS = IVPASS + 1                                          03520355
           WRITE (NUVI, 80002) IVTNUM                                   03530355
           GO TO 0181                                                   03540355
20180      IVFAIL = IVFAIL + 1                                          03550355
           RVCORR = 5.0                                                 03560355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03570355
 0181      CONTINUE                                                     03580355
CT019*  TEST 19                                     A VALUE IN (X,X+.5) 03590355
           IVTNUM = 19                                                  03600355
        RCBVS = 10.46875                                                03610355
        RCAVS = ANINT(RCBVS)                                            03620355
           IF (RCAVS - 9.9995) 20190, 10190, 40190                      03630355
40190      IF (RCAVS - 10.001) 10190, 10190, 20190                      03640355
10190      IVPASS = IVPASS + 1                                          03650355
           WRITE (NUVI, 80002) IVTNUM                                   03660355
           GO TO 0191                                                   03670355
20190      IVFAIL = IVFAIL + 1                                          03680355
           RVCORR = 10.0                                                03690355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03700355
 0191      CONTINUE                                                     03710355
CT020*  TEST 20                     A VALUE WITH FRACTIONAL PART OF 0.5 03720355
           IVTNUM = 20                                                  03730355
        RCBVS = FLOAT(16) - FLOAT(1) / FLOAT(2)                         03740355
        RCAVS = ANINT(RCBVS)                                            03750355
           IF (RCAVS - 15.999) 20200, 10200, 40200                      03760355
40200      IF (RCAVS - 16.001) 10200, 10200, 20200                      03770355
10200      IVPASS = IVPASS + 1                                          03780355
           WRITE (NUVI, 80002) IVTNUM                                   03790355
           GO TO 0201                                                   03800355
20200      IVFAIL = IVFAIL + 1                                          03810355
           RVCORR = 16.0                                                03820355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03830355
 0201      CONTINUE                                                     03840355
CT021*  TEST 21                                   A VALUE IN (X+.5,X+1) 03850355
           IVTNUM = 21                                                  03860355
        RCBVS = 27.96875                                                03870355
        RCAVS = ANINT(RCBVS)                                            03880355
           IF (RCAVS - 27.998) 20210, 10210, 40210                      03890355
40210      IF (RCAVS - 28.002) 10210, 10210, 20210                      03900355
10210      IVPASS = IVPASS + 1                                          03910355
           WRITE (NUVI, 80002) IVTNUM                                   03920355
           GO TO 0211                                                   03930355
20210      IVFAIL = IVFAIL + 1                                          03940355
           RVCORR = 28.0                                                03950355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     03960355
 0211      CONTINUE                                                     03970355
CT022*  TEST 22               A NEGATIVE VALUE WITH MAGNITUDE IN (0,.5) 03980355
           IVTNUM = 22                                                  03990355
        RCBVS = -0.25                                                   04000355
        RCAVS = ANINT(RCBVS)                                            04010355
           IF (RCAVS + 0.00005) 20220, 10220, 40220                     04020355
40220      IF (RCAVS - 0.00005) 10220, 10220, 20220                     04030355
10220      IVPASS = IVPASS + 1                                          04040355
           WRITE (NUVI, 80002) IVTNUM                                   04050355
           GO TO 0221                                                   04060355
20220      IVFAIL = IVFAIL + 1                                          04070355
           RVCORR = -0.0                                                04080355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     04090355
 0221      CONTINUE                                                     04100355
CT023*  TEST 23                                          THE VALUE -0.5 04110355
           IVTNUM = 23                                                  04120355
        RCBVS = FLOAT(-1) / FLOAT(2)                                    04130355
        RCAVS = ANINT(RCBVS)                                            04140355
           IF (RCAVS + 1.0001) 20230, 10230, 40230                      04150355
40230      IF (RCAVS + 0.99995) 10230, 10230, 20230                     04160355
10230      IVPASS = IVPASS + 1                                          04170355
           WRITE (NUVI, 80002) IVTNUM                                   04180355
           GO TO 0231                                                   04190355
20230      IVFAIL = IVFAIL + 1                                          04200355
           RVCORR = -1.0                                                04210355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     04220355
 0231      CONTINUE                                                     04230355
CT024*  TEST 24               A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1) 04240355
           IVTNUM = 24                                                  04250355
        RCBVS = -0.75                                                   04260355
        RCAVS = ANINT(RCBVS)                                            04270355
           IF (RCAVS + 1.0001) 20240, 10240, 40240                      04280355
40240      IF (RCAVS + 0.99995) 10240, 10240, 20240                     04290355
10240      IVPASS = IVPASS + 1                                          04300355
           WRITE (NUVI, 80002) IVTNUM                                   04310355
           GO TO 0241                                                   04320355
20240      IVFAIL = IVFAIL + 1                                          04330355
           RVCORR = -1.0                                                04340355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     04350355
 0241      CONTINUE                                                     04360355
CT025*  TEST 25                               A NEGATIVE INTEGRAL VALUE 04370355
           IVTNUM = 25                                                  04380355
        RCBVS = FLOAT(-5)                                               04390355
        RCAVS = ANINT(RCBVS)                                            04400355
           IF (RCAVS + 5.0003) 20250, 10250, 40250                      04410355
40250      IF (RCAVS + 4.9997) 10250, 10250, 20250                      04420355
10250      IVPASS = IVPASS + 1                                          04430355
           WRITE (NUVI, 80002) IVTNUM                                   04440355
           GO TO 0251                                                   04450355
20250      IVFAIL = IVFAIL + 1                                          04460355
           RVCORR = -5.0                                                04470355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     04480355
 0251      CONTINUE                                                     04490355
CT026*  TEST 26             A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5) 04500355
           IVTNUM = 26                                                  04510355
        RCBVS = -10.46875                                               04520355
        RCAVS = ANINT(RCBVS)                                            04530355
           IF (RCAVS + 10.001) 20260, 10260, 40260                      04540355
40260      IF (RCAVS + 9.9995) 10260, 10260, 20260                      04550355
10260      IVPASS = IVPASS + 1                                          04560355
           WRITE (NUVI, 80002) IVTNUM                                   04570355
           GO TO 0261                                                   04580355
20260      IVFAIL = IVFAIL + 1                                          04590355
           RVCORR = -10.0                                               04600355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     04610355
 0261      CONTINUE                                                     04620355
CT027*  TEST 27           A NEGATIVE VALUE WITH FRACTIONAL COMPONENT .5 04630355
           IVTNUM = 27                                                  04640355
        RCBVS = FLOAT(-15) - FLOAT(1) / FLOAT(2)                        04650355
        RCAVS = ANINT(RCBVS)                                            04660355
           IF (RCAVS + 16.001) 20270, 10270, 40270                      04670355
40270      IF (RCAVS + 15.999) 10270, 10270, 20270                      04680355
10270      IVPASS = IVPASS + 1                                          04690355
           WRITE (NUVI, 80002) IVTNUM                                   04700355
           GO TO 0271                                                   04710355
20270      IVFAIL = IVFAIL + 1                                          04720355
           RVCORR = -16.0                                               04730355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     04740355
 0271      CONTINUE                                                     04750355
CT028*  TEST 28           A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1) 04760355
           IVTNUM = 28                                                  04770355
        RCBVS = -27.96875                                               04780355
        RCAVS = ANINT(RCBVS)                                            04790355
           IF (RCAVS + 28.002) 20280, 10280, 40280                      04800355
40280      IF (RCAVS + 27.998) 10280, 10280, 20280                      04810355
10280      IVPASS = IVPASS + 1                                          04820355
           WRITE (NUVI, 80002) IVTNUM                                   04830355
           GO TO 0281                                                   04840355
20280      IVFAIL = IVFAIL + 1                                          04850355
           RVCORR = -28.0                                               04860355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     04870355
 0281      CONTINUE                                                     04880355
CT029*  TEST 29             AN ARITHMETIC EXPRESSION PRESENTED TO ANINT 04890355
           IVTNUM = 29                                                  04900355
        RCDVS = 8.00                                                    04910355
        RCBVS = 7.25                                                    04920355
        RCAVS = ANINT(RCDVS - RCBVS)                                    04930355
           IF (RCAVS - 0.99995) 20290, 10290, 40290                     04940355
40290      IF (RCAVS - 1.0001) 10290, 10290, 20290                      04950355
10290      IVPASS = IVPASS + 1                                          04960355
           WRITE (NUVI, 80002) IVTNUM                                   04970355
           GO TO 0291                                                   04980355
20290      IVFAIL = IVFAIL + 1                                          04990355
           RVCORR = 1.0                                                 05000355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     05010355
 0291      CONTINUE                                                     05020355
CT030*  TEST 30                            AN ARGUMENT OF LOW MAGNITUDE 05030355
           IVTNUM = 30                                                  05040355
        RCBVS = -5.9876E-35                                             05050355
        RCAVS = ANINT(RCBVS)                                            05060355
           IF (RCAVS + 0.00005) 20300, 10300, 40300                     05070355
40300      IF (RCAVS - 0.00005) 10300, 10300, 20300                     05080355
10300      IVPASS = IVPASS + 1                                          05090355
           WRITE (NUVI, 80002) IVTNUM                                   05100355
           GO TO 0301                                                   05110355
20300      IVFAIL = IVFAIL + 1                                          05120355
           RVCORR = 0.0                                                 05130355
           WRITE(NUVI, 80012) IVTNUM, RCAVS, RVCORR                     05140355
 0301      CONTINUE                                                     05150355
C*****                                                                  05160355
        WRITE(NUVI, 90002)                                              05170355
        WRITE(NUVI, 90013)                                              05180355
        WRITE(NUVI, 90014)                                              05190355
C*****                                                                  05200355
C*****    TEST OF NINT                                                  05210355
C*****                                                                  05220355
        WRITE(NUVI, 15405)                                              05230355
15405   FORMAT (/ 8X, 12HTEST OF NINT)                                  05240355
C*****                                                                  05250355
CT031*  TEST 31                                          THE VALUE ZERO 05260355
           IVTNUM = 31                                                  05270355
        RCBVS = 0.0                                                     05280355
        ICAVI = NINT(RCBVS)                                             05290355
           IF (ICAVI - 0) 20310, 10310, 20310                           05300355
10310      IVPASS = IVPASS + 1                                          05310355
           WRITE (NUVI, 80002) IVTNUM                                   05320355
           GO TO 0311                                                   05330355
20310      IVFAIL = IVFAIL + 1                                          05340355
           IVCORR = 0                                                   05350355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    05360355
 0311      CONTINUE                                                     05370355
CT032*  TEST 32                         ZERO PREFIXED WITH A MINUS SIGN 05380355
           IVTNUM = 32                                                  05390355
        RCDVS = 0.0                                                     05400355
        ICAVI = NINT(-RCDVS)                                            05410355
           IF (ICAVI - 0) 20320, 10320, 20320                           05420355
10320      IVPASS = IVPASS + 1                                          05430355
           WRITE (NUVI, 80002) IVTNUM                                   05440355
           GO TO 0321                                                   05450355
20320      IVFAIL = IVFAIL + 1                                          05460355
           IVCORR = 0                                                   05470355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    05480355
 0321      CONTINUE                                                     05490355
CT033*  TEST 33                                       A VALUE IN (0,.5) 05500355
           IVTNUM = 33                                                  05510355
        RCBVS = 0.25                                                    05520355
        ICAVI = NINT(RCBVS)                                             05530355
           IF (ICAVI - 0) 20330, 10330, 20330                           05540355
10330      IVPASS = IVPASS + 1                                          05550355
           WRITE (NUVI, 80002) IVTNUM                                   05560355
           GO TO 0331                                                   05570355
20330      IVFAIL = IVFAIL + 1                                          05580355
           IVCORR = 0                                                   05590355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    05600355
 0331      CONTINUE                                                     05610355
CT034*  TEST 34                                           THE VALUE 0.5 05620355
           IVTNUM = 34                                                  05630355
        RCBVS = FLOAT(1) / FLOAT(2)                                     05640355
        ICAVI = NINT(RCBVS)                                             05650355
           IF (ICAVI - 1) 20340, 10340, 20340                           05660355
10340      IVPASS = IVPASS + 1                                          05670355
           WRITE (NUVI, 80002) IVTNUM                                   05680355
           GO TO 0341                                                   05690355
20340      IVFAIL = IVFAIL + 1                                          05700355
           IVCORR = 1                                                   05710355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    05720355
 0341      CONTINUE                                                     05730355
CT035*  TEST 35                                       A VALUE IN (.5,1) 05740355
           IVTNUM = 35                                                  05750355
        RCBVS = 0.75                                                    05760355
        ICAVI = NINT(RCBVS)                                             05770355
           IF (ICAVI - 1) 20350, 10350, 20350                           05780355
10350      IVPASS = IVPASS + 1                                          05790355
           WRITE (NUVI, 80002) IVTNUM                                   05800355
           GO TO 0351                                                   05810355
20350      IVFAIL = IVFAIL + 1                                          05820355
           IVCORR = 1                                                   05830355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    05840355
 0351      CONTINUE                                                     05850355
CT036*  TEST 36                       AN INTEGRAL VALUE OTHER THAN 0, 1 05860355
           IVTNUM = 36                                                  05870355
        RCBVS = FLOAT(5)                                                05880355
        ICAVI = NINT(RCBVS)                                             05890355
           IF (ICAVI - 5) 20360, 10360, 20360                           05900355
10360      IVPASS = IVPASS + 1                                          05910355
           WRITE (NUVI, 80002) IVTNUM                                   05920355
           GO TO 0361                                                   05930355
20360      IVFAIL = IVFAIL + 1                                          05940355
           IVCORR = 5                                                   05950355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    05960355
 0361      CONTINUE                                                     05970355
CT037*  TEST 37                                     A VALUE IN (X,X+.5) 05980355
           IVTNUM = 37                                                  05990355
        RCBVS = 10.46875                                                06000355
        ICAVI = NINT(RCBVS)                                             06010355
           IF (ICAVI - 10) 20370, 10370, 20370                          06020355
10370      IVPASS = IVPASS + 1                                          06030355
           WRITE (NUVI, 80002) IVTNUM                                   06040355
           GO TO 0371                                                   06050355
20370      IVFAIL = IVFAIL + 1                                          06060355
           IVCORR = 10                                                  06070355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06080355
 0371      CONTINUE                                                     06090355
CT038*  TEST 38                     A VALUE WITH FRACTIONAL PART OF 0.5 06100355
           IVTNUM = 38                                                  06110355
        RCBVS = FLOAT(15) + FLOAT(1) / FLOAT(2)                         06120355
        ICAVI = NINT(RCBVS)                                             06130355
           IF (ICAVI - 16) 20380, 10380, 20380                          06140355
10380      IVPASS = IVPASS + 1                                          06150355
           WRITE (NUVI, 80002) IVTNUM                                   06160355
           GO TO 0381                                                   06170355
20380      IVFAIL = IVFAIL + 1                                          06180355
           IVCORR = 16                                                  06190355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06200355
 0381      CONTINUE                                                     06210355
CT039*  TEST 39                                   A VALUE IN (X+.5,X+1) 06220355
           IVTNUM = 39                                                  06230355
        RCBVS = 27.96875                                                06240355
        ICAVI = NINT(RCBVS)                                             06250355
           IF (ICAVI - 28) 20390, 10390, 20390                          06260355
10390      IVPASS = IVPASS + 1                                          06270355
           WRITE (NUVI, 80002) IVTNUM                                   06280355
           GO TO 0391                                                   06290355
20390      IVFAIL = IVFAIL + 1                                          06300355
           IVCORR = 28                                                  06310355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06320355
 0391      CONTINUE                                                     06330355
CT040*  TEST 40               A NEGATIVE VALUE WITH MAGNITUDE IN (0.,5) 06340355
           IVTNUM = 40                                                  06350355
        RCBVS = -0.25                                                   06360355
        ICAVI = NINT(RCBVS)                                             06370355
           IF (ICAVI - 0) 20400, 10400, 20400                           06380355
10400      IVPASS = IVPASS + 1                                          06390355
           WRITE (NUVI, 80002) IVTNUM                                   06400355
           GO TO 0401                                                   06410355
20400      IVFAIL = IVFAIL + 1                                          06420355
           IVCORR = 0                                                   06430355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06440355
 0401      CONTINUE                                                     06450355
CT041*  TEST 41                                          THE VALUE -0.5 06460355
           IVTNUM = 41                                                  06470355
        RCBVS = FLOAT(-1) / FLOAT(2)                                    06480355
        ICAVI = NINT(RCBVS)                                             06490355
           IF (ICAVI + 1) 20410, 10410, 20410                           06500355
10410      IVPASS = IVPASS + 1                                          06510355
           WRITE (NUVI, 80002) IVTNUM                                   06520355
           GO TO 0411                                                   06530355
20410      IVFAIL = IVFAIL + 1                                          06540355
           IVCORR = -1                                                  06550355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06560355
 0411      CONTINUE                                                     06570355
CT042*  TEST 42               A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1) 06580355
           IVTNUM = 42                                                  06590355
        RCBVS = -0.75                                                   06600355
        ICAVI = NINT(RCBVS)                                             06610355
           IF (ICAVI + 1) 20420, 10420, 20420                           06620355
10420      IVPASS = IVPASS + 1                                          06630355
           WRITE (NUVI, 80002) IVTNUM                                   06640355
           GO TO 0421                                                   06650355
20420      IVFAIL = IVFAIL + 1                                          06660355
           IVCORR = -1                                                  06670355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06680355
 0421      CONTINUE                                                     06690355
CT043*  TEST 43                               A NEGATIVE INTEGRAL VALUE 06700355
           IVTNUM = 43                                                  06710355
        RCBVS = FLOAT(-5)                                               06720355
        ICAVI = NINT(RCBVS)                                             06730355
           IF (ICAVI + 5) 20430, 10430, 20430                           06740355
10430      IVPASS = IVPASS + 1                                          06750355
           WRITE (NUVI, 80002) IVTNUM                                   06760355
           GO TO 0431                                                   06770355
20430      IVFAIL = IVFAIL + 1                                          06780355
           IVCORR = -5                                                  06790355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06800355
 0431      CONTINUE                                                     06810355
CT044*  TEST 44             A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5) 06820355
           IVTNUM = 44                                                  06830355
        RCBVS = -10.46875                                               06840355
        ICAVI = NINT(RCBVS)                                             06850355
           IF (ICAVI + 10) 20440, 10440, 20440                          06860355
10440      IVPASS = IVPASS + 1                                          06870355
           WRITE (NUVI, 80002) IVTNUM                                   06880355
           GO TO 0441                                                   06890355
20440      IVFAIL = IVFAIL + 1                                          06900355
           IVCORR = -10                                                 06910355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    06920355
 0441      CONTINUE                                                     06930355
CT045*  TEST 45        A NEGATIVE VALUE WITH FRACTIONAL COMPONENT 0.5 S106940355
           IVTNUM = 45                                                  06950355
        RCBVS = FLOAT(-15) - FLOAT(1) / FLOAT(2)                        06960355
        ICAVI = NINT(RCBVS)                                             06970355
           IF (ICAVI + 16) 20450, 10450, 20450                          06980355
10450      IVPASS = IVPASS + 1                                          06990355
           WRITE (NUVI, 80002) IVTNUM                                   07000355
           GO TO 0451                                                   07010355
20450      IVFAIL = IVFAIL + 1                                          07020355
           IVCORR = -16                                                 07030355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    07040355
 0451      CONTINUE                                                     07050355
CT046*  TEST 46         A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1) S107060355
           IVTNUM = 46                                                  07070355
        RCBVS = -27.96875                                               07080355
        ICAVI = NINT(RCBVS)                                             07090355
           IF (ICAVI + 28) 20460, 10460, 20460                          07100355
10460      IVPASS = IVPASS + 1                                          07110355
           WRITE (NUVI, 80002) IVTNUM                                   07120355
           GO TO 0461                                                   07130355
20460      IVFAIL = IVFAIL + 1                                          07140355
           IVCORR = -28                                                 07150355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    07160355
 0461      CONTINUE                                                     07170355
CT047*  TEST 47            AN ARITHMETIC EXPRESSION PRESENTED TO NINT S107180355
           IVTNUM = 47                                                  07190355
        RCDVS = 8.00                                                    07200355
        RCEVS = 7.25                                                    07210355
        ICAVI = NINT(RCDVS - RCEVS)                                     07220355
           IF (ICAVI - 1) 20470, 10470, 20470                           07230355
10470      IVPASS = IVPASS + 1                                          07240355
           WRITE (NUVI, 80002) IVTNUM                                   07250355
           GO TO 0471                                                   07260355
20470      IVFAIL = IVFAIL + 1                                          07270355
           IVCORR = 1                                                   07280355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    07290355
 0471      CONTINUE                                                     07300355
CT048*  TEST 48                            AN ARGUMENT OF LOW MAGNITUDE 07310355
           IVTNUM = 48                                                  07320355
        RCBVS = -5.9876E-33                                             07330355
        ICAVI = NINT(RCBVS)                                             07340355
           IF (ICAVI - 0) 20480, 10480, 20480                           07350355
10480      IVPASS = IVPASS + 1                                          07360355
           WRITE (NUVI, 80002) IVTNUM                                   07370355
           GO TO 0481                                                   07380355
20480      IVFAIL = IVFAIL + 1                                          07390355
           IVCORR = 0                                                   07400355
           WRITE (NUVI, 80010) IVTNUM, ICAVI, IVCORR                    07410355
 0481      CONTINUE                                                     07420355
C*****                                                                  07430355
CBB** ********************** BBCSUM0  **********************************07440355
C**** WRITE OUT TEST SUMMARY                                            07450355
C****                                                                   07460355
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07470355
      WRITE (I02, 90004)                                                07480355
      WRITE (I02, 90014)                                                07490355
      WRITE (I02, 90004)                                                07500355
      WRITE (I02, 90020) IVPASS                                         07510355
      WRITE (I02, 90022) IVFAIL                                         07520355
      WRITE (I02, 90024) IVDELE                                         07530355
      WRITE (I02, 90026) IVINSP                                         07540355
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 07550355
CBE** ********************** BBCSUM0  **********************************07560355
CBB** ********************** BBCFOOT0 **********************************07570355
C**** WRITE OUT REPORT FOOTINGS                                         07580355
C****                                                                   07590355
      WRITE (I02,90016) ZPROG, ZPROG                                    07600355
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     07610355
      WRITE (I02,90019)                                                 07620355
CBE** ********************** BBCFOOT0 **********************************07630355
CBB** ********************** BBCFMT0A **********************************07640355
C**** FORMATS FOR TEST DETAIL LINES                                     07650355
C****                                                                   07660355
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           07670355
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           07680355
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           07690355
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           07700355
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           07710355
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    07720355
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07730355
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              07740355
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07750355
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  07760355
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         07770355
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         07780355
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         07790355
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         07800355
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      07810355
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      07820355
80050 FORMAT (1H ,48X,A31)                                              07830355
CBE** ********************** BBCFMT0A **********************************07840355
CBB** ********************** BBCFMT0B **********************************07850355
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                07860355
C****                                                                   07870355
90002 FORMAT (1H1)                                                      07880355
90004 FORMAT (1H )                                                      07890355
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               07900355
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07910355
90008 FORMAT (1H ,21X,A13,A17)                                          07920355
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       07930355
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    07940355
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     07950355
     1       7X,7HREMARKS,24X)                                          07960355
90014 FORMAT (1H ,46H----------------------------------------------,    07970355
     1        33H---------------------------------)                     07980355
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               07990355
C****                                                                   08000355
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             08010355
C****                                                                   08020355
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          08030355
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        08040355
     1        A13)                                                      08050355
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 08060355
C****                                                                   08070355
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 08080355
C****                                                                   08090355
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              08100355
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08110355
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08120355
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08130355
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08140355
CBE** ********************** BBCFMT0B **********************************08150355
C*****                                                                  08160355
C*****    END OF TEST SEGMENT 154                                       08170355
        STOP                                                            08180355
        END                                                             08190355
                                                                        08200355
