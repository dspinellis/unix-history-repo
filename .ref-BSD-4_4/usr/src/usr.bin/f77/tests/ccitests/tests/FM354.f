C***********************************************************************00010354
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020354
C*****   FM354              XREAL - (152)                               00030354
C*****                                                                  00040354
C***********************************************************************00050354
C*****  GENERAL PURPOSE                                       SUBSET REF00060354
C*****    TEST INTRINSIC FUNCTIONS FLOAT AND REAL                15.3   00070354
C*****    (CONVERSION FROM INTEGER TO REAL)                    (TABLE 5)00080354
C*****                                                                  00090354
CBB** ********************** BBCCOMNT **********************************00100354
C****                                                                   00110354
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120354
C****                          VERSION 2.0                              00130354
C****                                                                   00140354
C****                                                                   00150354
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160354
C****                   GENERAL SERVICES ADMINISTRATION                 00170354
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180354
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190354
C****                      FALLS CHURCH, VA. 22041                      00200354
C****                                                                   00210354
C****                          (703) 756-6153                           00220354
C****                                                                   00230354
CBE** ********************** BBCCOMNT **********************************00240354
CBB** ********************** BBCINITA **********************************00250354
C**** SPECIFICATION STATEMENTS                                          00260354
C****                                                                   00270354
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00280354
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00290354
CBE** ********************** BBCINITA **********************************00300354
CBB** ********************** BBCINITB **********************************00310354
C**** INITIALIZE SECTION                                                00320354
      DATA  ZVERS,                  ZVERSD,             ZDATE           00330354
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00340354
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00350354
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00360354
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00370354
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00380354
      DATA   REMRKS /'                               '/                 00390354
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00400354
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00410354
C****                                                                   00420354
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00430354
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00440354
CZ03  ZPROG  = 'PROGRAM NAME'                                           00450354
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00520354
      IVPASS = 0                                                        00530354
      IVFAIL = 0                                                        00540354
      IVDELE = 0                                                        00550354
      IVINSP = 0                                                        00560354
      IVTOTL = 0                                                        00570354
      IVTOTN = 0                                                        00580354
      ICZERO = 0                                                        00590354
C                                                                       00600354
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610354
      I01 = 05                                                          00620354
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630354
      I02 = 06                                                          00640354
C                                                                       00650354
      I01 = 5                                                           00660354
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670354
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00680354
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00690354
C                                                                       00700354
      I02 = 6                                                           00710354
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00720354
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00730354
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00740354
C                                                                       00750354
CBE** ********************** BBCINITB **********************************00760354
      NUVI = I02                                                        00770354
      IVTOTL = 14                                                       00780354
      ZPROG = 'FM354'                                                   00790354
CBB** ********************** BBCHED0A **********************************00800354
C****                                                                   00810354
C**** WRITE REPORT TITLE                                                00820354
C****                                                                   00830354
      WRITE (I02, 90002)                                                00840354
      WRITE (I02, 90006)                                                00850354
      WRITE (I02, 90007)                                                00860354
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00870354
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00880354
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00890354
CBE** ********************** BBCHED0A **********************************00900354
C*****                                                                  00910354
C*****    HEADER FOR SEGMENT 152                                        00920354
        WRITE (NUVI,15201)                                              00930354
15201   FORMAT (1H , // 2X,35HXREAL - (152) INTRINSIC FUNCTIONS--//17X, 00940354
     1      29HFLOAT, REAL (TYPE CONVERSION)// 2X,                      00950354
     2      18HSUBSET REF. - 15.3)                                      00960354
CBB** ********************** BBCHED0B **********************************00970354
C**** WRITE DETAIL REPORT HEADERS                                       00980354
C****                                                                   00990354
      WRITE (I02,90004)                                                 01000354
      WRITE (I02,90004)                                                 01010354
      WRITE (I02,90013)                                                 01020354
      WRITE (I02,90014)                                                 01030354
      WRITE (I02,90015) IVTOTL                                          01040354
CBE** ********************** BBCHED0B **********************************01050354
C*****                                                                  01060354
C*****    TEST OF FLOAT                                                 01070354
C*****                                                                  01080354
        WRITE(NUVI, 15204)                                              01090354
15204   FORMAT (/ 8X, 13HTEST OF FLOAT)                                 01100354
CT001*  TEST 1                                           THE VALUE ZERO 01110354
           IVTNUM = 1                                                   01120354
        IBCVI = 0                                                       01130354
        RBAVS = FLOAT(IBCVI)                                            01140354
           IF (RBAVS + 0.00005) 20010, 10010, 40010                     01150354
40010      IF (RBAVS - 0.00005) 10010, 10010, 20010                     01160354
10010      IVPASS = IVPASS + 1                                          01170354
           WRITE (NUVI, 80002) IVTNUM                                   01180354
           GO TO 0011                                                   01190354
20010      IVFAIL = IVFAIL + 1                                          01200354
           RVCORR = 0.0                                                 01210354
           WRITE(NUVI, 80012) IVTNUM, RBAVS, RVCORR                     01220354
 0011      CONTINUE                                                     01230354
CT002*  TEST 2                                       A POSITIVE INTEGER 01240354
           IVTNUM = 2                                                   01250354
        IBCVI = 3                                                       01260354
        RBAVS = FLOAT(IBCVI)                                            01270354
           IF (RBAVS - 2.9998) 20020, 10020, 40020                      01280354
40020      IF (RBAVS - 3.0002) 10020, 10020, 20020                      01290354
10020      IVPASS = IVPASS + 1                                          01300354
           WRITE (NUVI, 80002) IVTNUM                                   01310354
           GO TO 0021                                                   01320354
20020      IVFAIL = IVFAIL + 1                                          01330354
           RVCORR = 3.0                                                 01340354
           WRITE(NUVI, 80012) IVTNUM, RBAVS, RVCORR                     01350354
 0021      CONTINUE                                                     01360354
CT003*  TEST 3                                       A NEGATIVE INTEGER 01370354
           IVTNUM = 3                                                   01380354
        IBCVI = -3                                                      01390354
        RBAVS = FLOAT(IBCVI)                                            01400354
           IF (RBAVS + 3.0002) 20030, 10030, 40030                      01410354
40030      IF (RBAVS + 2.9998) 10030, 10030, 20030                      01420354
10030      IVPASS = IVPASS + 1                                          01430354
           WRITE (NUVI, 80002) IVTNUM                                   01440354
           GO TO 0031                                                   01450354
20030      IVFAIL = IVFAIL + 1                                          01460354
           RVCORR = -3.0                                                01470354
           WRITE(NUVI, 80012) IVTNUM, RBAVS, RVCORR                     01480354
 0031      CONTINUE                                                     01490354
CT004*  TEST 4                        A ZERO PREFIXED WITH A MINUS SIGN 01500354
           IVTNUM = 4                                                   01510354
        IBCVI = 0                                                       01520354
        RBAVS = FLOAT(-IBCVI)                                           01530354
           IF (RBAVS + 0.00005) 20040, 10040, 40040                     01540354
40040      IF (RBAVS - 0.00005) 10040, 10040, 20040                     01550354
10040      IVPASS = IVPASS + 1                                          01560354
           WRITE (NUVI, 80002) IVTNUM                                   01570354
           GO TO 0041                                                   01580354
20040      IVFAIL = IVFAIL + 1                                          01590354
           RVCORR = 0.0                                                 01600354
           WRITE(NUVI, 80012) IVTNUM, RBAVS, RVCORR                     01610354
 0041      CONTINUE                                                     01620354
CT005*  TEST 5                   FLOAT USED IN AN ARITHMETIC EXPRESSION 01630354
           IVTNUM = 5                                                   01640354
        RBFVS = -3.0                                                    01650354
        IBCVI = 3                                                       01660354
        RBAVS = 16.1875 + RBFVS/FLOAT(IBCVI)                            01670354
           IF (RBAVS - 15.186) 20050, 10050, 40050                      01680354
40050      IF (RBAVS - 15.189) 10050, 10050, 20050                      01690354
10050      IVPASS = IVPASS + 1                                          01700354
           WRITE (NUVI, 80002) IVTNUM                                   01710354
           GO TO 0051                                                   01720354
20050      IVFAIL = IVFAIL + 1                                          01730354
           RVCORR = 15.1875                                             01740354
           WRITE(NUVI, 80012) IVTNUM, RBAVS, RVCORR                     01750354
 0051      CONTINUE                                                     01760354
CT006*  TEST 6              AN ARITHMETIC EXPRESSION PRESENTED TO FLOAT 01770354
           IVTNUM = 6                                                   01780354
        IBAVI = -7                                                      01790354
        IBBVI = 27                                                      01800354
        RBAVS = FLOAT(IBAVI - IBBVI * 2)                                01810354
           IF (RBAVS + 61.003) 20060, 10060, 40060                      01820354
40060      IF (RBAVS + 60.997) 10060, 10060, 20060                      01830354
10060      IVPASS = IVPASS + 1                                          01840354
           WRITE (NUVI, 80002) IVTNUM                                   01850354
           GO TO 0061                                                   01860354
20060      IVFAIL = IVFAIL + 1                                          01870354
           RVCORR = -61.0                                               01880354
           WRITE(NUVI, 80012) IVTNUM, RBAVS, RVCORR                     01890354
 0061      CONTINUE                                                     01900354
CT007*  TEST 7            COMPARE AUTOMATIC TYPE CONVERSION TO EXPLICIT 01910354
           IVTNUM = 7                                                   01920354
        IBAVI = 2                                                       01930354
        IBBVI = 10                                                      01940354
        RBAVS = FLOAT(IBBVI ** IBAVI)                                   01950354
           IF (RBAVS - 99.995) 20070, 10070, 40070                      01960354
40070      IF (RBAVS - 100.01) 10070, 10070, 20070                      01970354
10070      IVPASS = IVPASS + 1                                          01980354
           WRITE (NUVI, 80002) IVTNUM                                   01990354
           GO TO 0071                                                   02000354
20070      IVFAIL = IVFAIL + 1                                          02010354
           RVCORR = 100.0                                               02020354
           WRITE(NUVI, 80012) IVTNUM, RBAVS, RVCORR                     02030354
 0071      CONTINUE                                                     02040354
C*****                                                                  02050354
C*****    TEST OF REAL                                                  02060354
C*****                                                                  02070354
        WRITE(NUVI, 15202)                                              02080354
15202   FORMAT (/ 08X, 12HTEST OF REAL)                                 02090354
CT008*  TEST 8                                           THE VALUE ZERO 02100354
           IVTNUM = 8                                                   02110354
        IBCVI = 0                                                       02120354
        RBBVS = REAL(IBCVI)                                             02130354
           IF (RBBVS + 0.00005) 20080, 10080, 40080                     02140354
40080      IF (RBBVS - 0.00005) 10080, 10080, 20080                     02150354
10080      IVPASS = IVPASS + 1                                          02160354
           WRITE (NUVI, 80002) IVTNUM                                   02170354
           GO TO 0081                                                   02180354
20080      IVFAIL = IVFAIL + 1                                          02190354
           RVCORR = 0.0                                                 02200354
           WRITE(NUVI, 80012) IVTNUM, RBBVS, RVCORR                     02210354
 0081      CONTINUE                                                     02220354
CT009*  TEST 9                                       A POSITIVE INTEGER 02230354
           IVTNUM = 9                                                   02240354
        IBCVI = 3                                                       02250354
        RBBVS = REAL(IBCVI)                                             02260354
           IF (RBBVS - 2.9998) 20090, 10090, 40090                      02270354
40090      IF (RBBVS - 3.0002) 10090, 10090, 20090                      02280354
10090      IVPASS = IVPASS + 1                                          02290354
           WRITE (NUVI, 80002) IVTNUM                                   02300354
           GO TO 0091                                                   02310354
20090      IVFAIL = IVFAIL + 1                                          02320354
           RVCORR = 3.0                                                 02330354
           WRITE(NUVI, 80012) IVTNUM, RBBVS, RVCORR                     02340354
 0091      CONTINUE                                                     02350354
CT010*  TEST 10                                      A NEGATIVE INTEGER 02360354
           IVTNUM = 10                                                  02370354
        IBCVI = -3                                                      02380354
        RBBVS = REAL(IBCVI)                                             02390354
           IF (RBBVS + 3.0002) 20100, 10100, 40100                      02400354
40100      IF (RBBVS + 2.9998) 10100, 10100, 20100                      02410354
10100      IVPASS = IVPASS + 1                                          02420354
           WRITE (NUVI, 80002) IVTNUM                                   02430354
           GO TO 0101                                                   02440354
20100      IVFAIL = IVFAIL + 1                                          02450354
           RVCORR = -3.0                                                02460354
           WRITE(NUVI, 80012) IVTNUM, RBBVS, RVCORR                     02470354
 0101      CONTINUE                                                     02480354
CT011*  TEST 11                       A ZERO PREFIXED WITH A MINUS SIGN 02490354
           IVTNUM = 11                                                  02500354
        IBCVI = 0                                                       02510354
        RBBVS = REAL(-IBCVI)                                            02520354
           IF (RBBVS + 0.00005) 20110, 10110, 40110                     02530354
40110      IF (RBBVS - 0.00005) 10110, 10110, 20110                     02540354
10110      IVPASS = IVPASS + 1                                          02550354
           WRITE (NUVI, 80002) IVTNUM                                   02560354
           GO TO 0111                                                   02570354
20110      IVFAIL = IVFAIL + 1                                          02580354
           RVCORR = 0.0                                                 02590354
           WRITE(NUVI, 80012) IVTNUM, RBBVS, RVCORR                     02600354
 0111      CONTINUE                                                     02610354
CT012*  TEST 12                   REAL USED IN AN ARITHMETIC EXPRESSION 02620354
           IVTNUM = 12                                                  02630354
        RBFVS = -3.0                                                    02640354
        IBCVI = 3                                                       02650354
        RBBVS = 16.1875 + RBFVS/REAL(IBCVI)                             02660354
           IF (RBBVS - 15.186) 20120, 10120, 40120                      02670354
40120      IF (RBBVS - 15.189) 10120, 10120, 20120                      02680354
10120      IVPASS = IVPASS + 1                                          02690354
           WRITE (NUVI, 80002) IVTNUM                                   02700354
           GO TO 0121                                                   02710354
20120      IVFAIL = IVFAIL + 1                                          02720354
           RVCORR = 15.1875                                             02730354
           WRITE(NUVI, 80012) IVTNUM, RBBVS, RVCORR                     02740354
 0121      CONTINUE                                                     02750354
CT013*  TEST 13              AN ARITHMETIC EXPRESSION PRESENTED TO REAL 02760354
           IVTNUM = 13                                                  02770354
        IBAVI = -7                                                      02780354
        IBBVI = 27                                                      02790354
        RBBVS = REAL(IBAVI - IBBVI * 2)                                 02800354
           IF (RBBVS + 61.003) 20130, 10130, 40130                      02810354
40130      IF (RBBVS + 60.997) 10130, 10130, 20130                      02820354
10130      IVPASS = IVPASS + 1                                          02830354
           WRITE (NUVI, 80002) IVTNUM                                   02840354
           GO TO 0131                                                   02850354
20130      IVFAIL = IVFAIL + 1                                          02860354
           RVCORR = 61.0                                                02870354
           WRITE(NUVI, 80012) IVTNUM, RBBVS, RVCORR                     02880354
 0131      CONTINUE                                                     02890354
CT014*  TEST 14           COMPARE AUTOMATIC TYPE CONVERSION TO EXPLICIT 02900354
           IVTNUM = 14                                                  02910354
        IBAVI = 2                                                       02920354
        IBBVI = 10                                                      02930354
        RBBVS = REAL(IBBVI ** IBAVI)                                    02940354
           IF (RBBVS - 99.995) 20140, 10140, 40140                      02950354
40140      IF (RBBVS - 100.01) 10140, 10140, 20140                      02960354
10140      IVPASS = IVPASS + 1                                          02970354
           WRITE (NUVI, 80002) IVTNUM                                   02980354
           GO TO 0141                                                   02990354
20140      IVFAIL = IVFAIL + 1                                          03000354
           RVCORR = 100.0                                               03010354
           WRITE(NUVI, 80012) IVTNUM, RBBVS, RVCORR                     03020354
 0141      CONTINUE                                                     03030354
CBB** ********************** BBCSUM0  **********************************03040354
C**** WRITE OUT TEST SUMMARY                                            03050354
C****                                                                   03060354
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03070354
      WRITE (I02, 90004)                                                03080354
      WRITE (I02, 90014)                                                03090354
      WRITE (I02, 90004)                                                03100354
      WRITE (I02, 90020) IVPASS                                         03110354
      WRITE (I02, 90022) IVFAIL                                         03120354
      WRITE (I02, 90024) IVDELE                                         03130354
      WRITE (I02, 90026) IVINSP                                         03140354
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03150354
CBE** ********************** BBCSUM0  **********************************03160354
CBB** ********************** BBCFOOT0 **********************************03170354
C**** WRITE OUT REPORT FOOTINGS                                         03180354
C****                                                                   03190354
      WRITE (I02,90016) ZPROG, ZPROG                                    03200354
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03210354
      WRITE (I02,90019)                                                 03220354
CBE** ********************** BBCFOOT0 **********************************03230354
CBB** ********************** BBCFMT0A **********************************03240354
C**** FORMATS FOR TEST DETAIL LINES                                     03250354
C****                                                                   03260354
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03270354
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03280354
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03290354
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03300354
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03310354
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03320354
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03330354
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03340354
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03350354
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03360354
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03370354
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03380354
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03390354
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03400354
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03410354
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03420354
80050 FORMAT (1H ,48X,A31)                                              03430354
CBE** ********************** BBCFMT0A **********************************03440354
CBB** ********************** BBCFMT0B **********************************03450354
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03460354
C****                                                                   03470354
90002 FORMAT (1H1)                                                      03480354
90004 FORMAT (1H )                                                      03490354
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03500354
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03510354
90008 FORMAT (1H ,21X,A13,A17)                                          03520354
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03530354
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03540354
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03550354
     1       7X,7HREMARKS,24X)                                          03560354
90014 FORMAT (1H ,46H----------------------------------------------,    03570354
     1        33H---------------------------------)                     03580354
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03590354
C****                                                                   03600354
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03610354
C****                                                                   03620354
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03630354
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03640354
     1        A13)                                                      03650354
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03660354
C****                                                                   03670354
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03680354
C****                                                                   03690354
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03700354
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03710354
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03720354
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03730354
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03740354
CBE** ********************** BBCFMT0B **********************************03750354
C*****                                                                  03760354
C*****    END OF TEST SEGMENT 152                                       03770354
        STOP                                                            03780354
        END                                                             03790354
                                                                        03800354
