C***********************************************************************00010805
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020805
C*****   FM805               YDDIM - (164)                              00030805
C*****                                                                  00040805
C***********************************************************************00050805
C*****  GENERAL PURPOSE                                         ANS REF 00060805
C*****    TEST INTRINSIC FUNCTION DDIM AND PROD--POSITIVE        15.3   00070805
C*****    DIFFERENCE AND DOUBLE PRECISION PRODUCT, RESP.       (TABLE 5)00080805
C*****                                                                  00090805
CBB** ********************** BBCCOMNT **********************************00100805
C****                                                                   00110805
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120805
C****                          VERSION 2.0                              00130805
C****                                                                   00140805
C****                                                                   00150805
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160805
C****                   GENERAL SERVICES ADMINISTRATION                 00170805
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180805
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190805
C****                      FALLS CHURCH, VA. 22041                      00200805
C****                                                                   00210805
C****                          (703) 756-6153                           00220805
C****                                                                   00230805
CBE** ********************** BBCCOMNT **********************************00240805
C*****                                                                  00250805
C*****     S P E C I F I C A T I O N S  SEGMENT 164                     00260805
        DOUBLE PRECISION DSAVD, DSBVD, DSDVD, DSEVD, DVCORR             00270805
C*****                                                                  00280805
CBB** ********************** BBCINITA **********************************00290805
C**** SPECIFICATION STATEMENTS                                          00300805
C****                                                                   00310805
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320805
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330805
CBE** ********************** BBCINITA **********************************00340805
CBB** ********************** BBCINITB **********************************00350805
C**** INITIALIZE SECTION                                                00360805
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370805
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380805
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390805
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400805
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410805
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420805
      DATA   REMRKS /'                               '/                 00430805
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440805
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450805
C****                                                                   00460805
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470805
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480805
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490805
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560805
      IVPASS = 0                                                        00570805
      IVFAIL = 0                                                        00580805
      IVDELE = 0                                                        00590805
      IVINSP = 0                                                        00600805
      IVTOTL = 0                                                        00610805
      IVTOTN = 0                                                        00620805
      ICZERO = 0                                                        00630805
C                                                                       00640805
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650805
      I01 = 05                                                          00660805
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670805
      I02 = 06                                                          00680805
C                                                                       00690805
      I01 = 5                                                           00700805
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710805
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720805
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730805
C                                                                       00740805
      I02 = 6                                                           00750805
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760805
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770805
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780805
C                                                                       00790805
CBE** ********************** BBCINITB **********************************00800805
      NUVI = I02                                                        00810805
      IVTOTL = 18                                                       00820805
      ZPROG = 'FM805'                                                   00830805
CBB** ********************** BBCHED0A **********************************00840805
C****                                                                   00850805
C**** WRITE REPORT TITLE                                                00860805
C****                                                                   00870805
      WRITE (I02, 90002)                                                00880805
      WRITE (I02, 90006)                                                00890805
      WRITE (I02, 90007)                                                00900805
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910805
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920805
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930805
CBE** ********************** BBCHED0A **********************************00940805
C*****                                                                  00950805
C*****    HEADER FOR SEGMENT 164                                        00960805
        WRITE (NUVI,16401)                                              00970805
16401   FORMAT (1H ,// 1X,36HYDDIM - (164) INTRINSIC FUNCTIONS-- //16X, 00980805
     1          26HDDIM (POSITIVE DIFFERENCE)/,16X,                     00990805
     2          20HDPROD (D.P. PRODUCT)//                               01000805
     3          2X,15HANS REF. - 15.3)                                  01010805
CBB** ********************** BBCHED0B **********************************01020805
C**** WRITE DETAIL REPORT HEADERS                                       01030805
C****                                                                   01040805
      WRITE (I02,90004)                                                 01050805
      WRITE (I02,90004)                                                 01060805
      WRITE (I02,90013)                                                 01070805
      WRITE (I02,90014)                                                 01080805
      WRITE (I02,90015) IVTOTL                                          01090805
CBE** ********************** BBCHED0B **********************************01100805
C*****                                                                  01110805
C*****    TEST OF DDIM                                                  01120805
C*****                                                                  01130805
        WRITE(NUVI, 16402)                                              01140805
16402   FORMAT(/ 8X, 12HTEST OF DDIM)                                   01150805
CT001*  TEST 1                                        BOTH VALUES EQUAL 01160805
           IVTNUM = 1                                                   01170805
        DSBVD = 0.25D0                                                  01180805
        DSDVD = 0.25D0                                                  01190805
        DSAVD = DDIM(DSBVD, DSDVD)                                      01200805
           IF (DSAVD + 5.0D-10) 20010, 10010, 40010                     01210805
40010      IF (DSAVD - 5.0D-10) 10010, 10010, 20010                     01220805
10010      IVPASS = IVPASS + 1                                          01230805
           WRITE (NUVI, 80002) IVTNUM                                   01240805
           GO TO 0011                                                   01250805
20010      IVFAIL = IVFAIL + 1                                          01260805
           DVCORR = 0.0D0                                               01270805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    01280805
 0011      CONTINUE                                                     01290805
CT002*  TEST 2                       BOTH VALUES EQUAL, INTEGRAL VALUES 01300805
           IVTNUM = 2                                                   01310805
        DSBVD = 2.0D0                                                   01320805
        DSDVD = 2.0D0                                                   01330805
        DSAVD = DDIM(DSBVD, DSDVD)                                      01340805
           IF (DSAVD + 5.0D-10) 20020, 10020, 40020                     01350805
40020      IF (DSAVD - 5.0D-10) 10020, 10020, 20020                     01360805
10020      IVPASS = IVPASS + 1                                          01370805
           WRITE (NUVI, 80002) IVTNUM                                   01380805
           GO TO 0021                                                   01390805
20020      IVFAIL = IVFAIL + 1                                          01400805
           DVCORR = 0.0D0                                               01410805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    01420805
 0021      CONTINUE                                                     01430805
CT003*  TEST 3                             FIRST VALUE LESS THAN SECOND 01440805
           IVTNUM = 3                                                   01450805
        DSBVD = 0.25D1                                                  01460805
        DSDVD = 0.55D1                                                  01470805
        DSAVD = DDIM(DSBVD, DSDVD)                                      01480805
           IF (DSAVD + 5.0D-10) 20030, 10030, 40030                     01490805
40030      IF (DSAVD - 5.0D-10) 10030, 10030, 20030                     01500805
10030      IVPASS = IVPASS + 1                                          01510805
           WRITE (NUVI, 80002) IVTNUM                                   01520805
           GO TO 0031                                                   01530805
20030      IVFAIL = IVFAIL + 1                                          01540805
           DVCORR = 0.0D0                                               01550805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    01560805
 0031      CONTINUE                                                     01570805
CT004*  TEST 4                          FIRST VALUE GREATER THAN SECOND 01580805
           IVTNUM = 4                                                   01590805
        DSBVD = 0.55D1                                                  01600805
        DSDVD = 0.25D1                                                  01610805
        DSAVD = DDIM(DSBVD, DSDVD)                                      01620805
           IF (DSAVD - 2.999999998D0) 20040, 10040, 40040               01630805
40040      IF (DSAVD - 3.000000002D0) 10040, 10040, 20040               01640805
10040      IVPASS = IVPASS + 1                                          01650805
           WRITE (NUVI, 80002) IVTNUM                                   01660805
           GO TO 0041                                                   01670805
20040      IVFAIL = IVFAIL + 1                                          01680805
           DVCORR = 3.0D0                                               01690805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    01700805
 0041      CONTINUE                                                     01710805
CT005*  TEST 5                         BOTH VALUES EQUAL, BOTH NEGATIVE 01720805
           IVTNUM = 5                                                   01730805
        DSBVD = -0.25D1                                                 01740805
        DSDVD = -0.25D1                                                 01750805
        DSAVD = DDIM(DSBVD, DSDVD)                                      01760805
           IF (DSAVD + 5.0D-10) 20050, 10050, 40050                     01770805
40050      IF (DSAVD - 5.0D-10) 10050, 10050, 20050                     01780805
10050      IVPASS = IVPASS + 1                                          01790805
           WRITE (NUVI, 80002) IVTNUM                                   01800805
           GO TO 0051                                                   01810805
20050      IVFAIL = IVFAIL + 1                                          01820805
           DVCORR = 0.0D0                                               01830805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    01840805
 0051      CONTINUE                                                     01850805
CT006*  TEST 6           FIRST VALUE GREATER THAN SECOND, BOTH NEGATIVE 01860805
           IVTNUM = 6                                                   01870805
        DSBVD = -0.25D1                                                 01880805
        DSDVD = -0.55D1                                                 01890805
        DSAVD = DDIM(DSBVD, DSDVD)                                      01900805
           IF (DSAVD - 2.999999998D0) 20060, 10060, 40060               01910805
40060      IF (DSAVD - 3.000000002D0) 10060, 10060, 20060               01920805
10060      IVPASS = IVPASS + 1                                          01930805
           WRITE (NUVI, 80002) IVTNUM                                   01940805
           GO TO 0061                                                   01950805
20060      IVFAIL = IVFAIL + 1                                          01960805
           DVCORR = 3.0D0                                               01970805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    01980805
 0061      CONTINUE                                                     01990805
CT007*  TEST 7                    FIRST VALUE POSITIVE, SECOND NEGATIVE 02000805
           IVTNUM = 7                                                   02010805
        DSBVD = 0.55D1                                                  02020805
        DSDVD = -0.25D1                                                 02030805
        DSAVD = DDIM(DSBVD, DSDVD)                                      02040805
           IF (DSAVD - 7.999999996D0) 20070, 10070, 40070               02050805
40070      IF (DSAVD - 8.000000004D0) 10070, 10070, 20070               02060805
10070      IVPASS = IVPASS + 1                                          02070805
           WRITE (NUVI, 80002) IVTNUM                                   02080805
           GO TO 0071                                                   02090805
20070      IVFAIL = IVFAIL + 1                                          02100805
           DVCORR = 8.0D0                                               02110805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    02120805
 0071      CONTINUE                                                     02130805
CT008*  TEST 8                  ARITHMETIC EXPRESSION PRESENTED TO DDIM 02140805
           IVTNUM = 8                                                   02150805
        DSDVD = 0.25D1                                                  02160805
        DSEVD = 0.125D1                                                 02170805
        DSAVD = DDIM(DSDVD / DSEVD, DSDVD * DSEVD)                      02180805
           IF (DSAVD + 5.0D-10) 20080, 10080, 40080                     02190805
40080      IF (DSAVD - 5.0D-10) 10080, 10080, 20080                     02200805
10080      IVPASS = IVPASS + 1                                          02210805
           WRITE (NUVI, 80002) IVTNUM                                   02220805
           GO TO 0081                                                   02230805
20080      IVFAIL = IVFAIL + 1                                          02240805
           DVCORR = 0.0D0                                               02250805
           WRITE (NUVI, 80031) IVTNUM, DSAVD, DVCORR                    02260805
 0081      CONTINUE                                                     02270805
C*****                                                                  02280805
C*****    TEST OF DPROD                                                 02290805
C*****                                                                  02300805
        WRITE(NUVI, 16404)                                              02310805
        REMRKS = '+ OR - 0.00005'                                       02320805
16404   FORMAT(// 8X, 13HTEST OF DPROD)                                 02330805
CT009*  TEST 9                     PAIR OF VALUES, ONE OF WHICH IS ZERO 02340805
           IVTNUM = 9                                                   02350805
        RSAVS = 0.0                                                     02360805
        RSBVS = 2.0                                                     02370805
        DSAVD = DPROD(RSAVS, RSBVS)                                     02380805
           IF (DSAVD + 5.0D-5) 20090, 10090, 40090                      02390805
40090      IF (DSAVD - 5.0D-5) 10090, 10090, 20090                      02400805
10090      IVPASS = IVPASS + 1                                          02410805
           WRITE (NUVI, 80002) IVTNUM                                   02420805
           GO TO 0091                                                   02430805
20090      IVFAIL = IVFAIL + 1                                          02440805
           DVCORR = 0.0D0                                               02450805
           WRITE (NUVI, 80008) IVTNUM                                   02460805
           WRITE (NUVI, 80033) DSAVD                                    02470805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           02480805
 0091      CONTINUE                                                     02490805
CT010*  TEST 10                     PAIR OF VALUES, ONE OF WHICH IS ONE 02500805
           IVTNUM = 10                                                  02510805
        RSAVS = 1.0                                                     02520805
        RSBVS = 2.0                                                     02530805
        DSAVD = DPROD(RSAVS, RSBVS)                                     02540805
           IF (DSAVD - 1.9999D0) 20100, 10100, 40100                    02550805
40100      IF (DSAVD - 2.0001D0) 10100, 10100, 20100                    02560805
10100      IVPASS = IVPASS + 1                                          02570805
           WRITE (NUVI, 80002) IVTNUM                                   02580805
           GO TO 0101                                                   02590805
20100      IVFAIL = IVFAIL + 1                                          02600805
           DVCORR = 2.0D0                                               02610805
           WRITE (NUVI, 80008) IVTNUM                                   02620805
           WRITE (NUVI, 80033) DSAVD                                    02630805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           02640805
 0101      CONTINUE                                                     02650805
CT011*  TEST 11                                 PAIR OF NON-ZERO VALUES 02660805
           IVTNUM = 11                                                  02670805
        RSAVS = 3.333333                                                02680805
        RSBVS = 2.3948094                                               02690805
        DSAVD = DPROD(RSAVS, RSBVS)                                     02700805
           IF (DSAVD - 7.9823D0) 20110, 10110, 40110                    02710805
40110      IF (DSAVD - 7.9831D0) 10110, 10110, 20110                    02720805
10110      IVPASS = IVPASS + 1                                          02730805
           WRITE (NUVI, 80002) IVTNUM                                   02740805
           GO TO 0111                                                   02750805
20110      IVFAIL = IVFAIL + 1                                          02760805
           DVCORR = 7.982697202D0                                       02770805
           WRITE (NUVI, 80008) IVTNUM                                   02780805
           WRITE (NUVI, 80033) DSAVD                                    02790805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           02800805
 0111      CONTINUE                                                     02810805
CT012*  TEST 12                            ONE POSITIVE, ONE NEGATIVE   02820805
           IVTNUM = 12                                                  02830805
        RSAVS = 0.123456                                                02840805
        RSBVS = -2.98765                                                02850805
        DSAVD = DPROD(RSAVS, RSBVS)                                     02860805
           IF (DSAVD + 3.6887D-1) 20120, 10120, 40120                   02870805
40120      IF (DSAVD + 3.6882D-1) 10120, 10120, 20120                   02880805
10120      IVPASS = IVPASS + 1                                          02890805
           WRITE (NUVI, 80002) IVTNUM                                   02900805
           GO TO 0121                                                   02910805
20120      IVFAIL = IVFAIL + 1                                          02920805
           DVCORR = -3.688433184D-1                                     02930805
           WRITE (NUVI, 80008) IVTNUM                                   02940805
           WRITE (NUVI, 80033) DSAVD                                    02950805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           02960805
 0121      CONTINUE                                                     02970805
CT013*  TEST 13                          ONE VALUE ONE(1), ONE NEGATIVE 02980805
           IVTNUM = 13                                                  02990805
        RSAVS = 1.0834001                                               03000805
        RSBVS = -2.034985                                               03010805
        DSAVD = DPROD(RSAVS, RSBVS)                                     03020805
           IF (DSAVD + 2.2049D0) 20130, 10130, 40130                    03030805
40130      IF (DSAVD + 2.2045D0) 10130, 10130, 20130                    03040805
10130      IVPASS = IVPASS + 1                                          03050805
           WRITE (NUVI, 80002) IVTNUM                                   03060805
           GO TO 0131                                                   03070805
20130      IVFAIL = IVFAIL + 1                                          03080805
           DVCORR = -2.204702953D0                                      03090805
           WRITE (NUVI, 80008) IVTNUM                                   03100805
           WRITE (NUVI, 80033) DSAVD                                    03110805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           03120805
 0131      CONTINUE                                                     03130805
CT014*  TEST 14                                 PAIR OF NEGATIVE VALUES 03140805
           IVTNUM = 14                                                  03150805
        RSAVS = -3.077734                                               03160805
        RSBVS = -2.348343                                               03170805
        DSAVD = DPROD(RSAVS, RSBVS)                                     03180805
           IF (DSAVD - 7.2272D0) 20140, 10140, 40140                    03190805
40140      IF (DSAVD - 7.2280D0) 10140, 10140, 20140                    03200805
10140      IVPASS = IVPASS + 1                                          03210805
           WRITE (NUVI, 80002) IVTNUM                                   03220805
           GO TO 0141                                                   03230805
20140      IVFAIL = IVFAIL + 1                                          03240805
           DVCORR = 7.227575095D0                                       03250805
           WRITE (NUVI, 80008) IVTNUM                                   03260805
           WRITE (NUVI, 80033) DSAVD                                    03270805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           03280805
 0141      CONTINUE                                                     03290805
CT015*  TEST 15                  ONE POSITIVE VALUE, ONE NEGATIVE VALUE 03300805
           IVTNUM = 15                                                  03310805
        RSAVS = 3.3333324                                               03320805
        RSBVS = -2.343953                                               03330805
        DSAVD = DPROD(RSAVS, RSBVS)                                     03340805
           IF (DSAVD + 7.8136D0) 20150, 10150, 40150                    03350805
40150      IF (DSAVD + 7.8127D0) 10150, 10150, 20150                    03360805
10150      IVPASS = IVPASS + 1                                          03370805
           WRITE (NUVI, 80002) IVTNUM                                   03380805
           GO TO 0151                                                   03390805
20150      IVFAIL = IVFAIL + 1                                          03400805
           DVCORR = -7.813174479D0                                      03410805
           WRITE (NUVI, 80008) IVTNUM                                   03420805
           WRITE (NUVI, 80033) DSAVD                                    03430805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           03440805
 0151      CONTINUE                                                     03450805
CT016*  TEST 16                ARITHMETIC EXPRESSION PRESENTED TO DPROD 03460805
           IVTNUM = 16                                                  03470805
        RSAVS = 1.555674                                                03480805
        RSBVS = 2.00012                                                 03490805
        DSAVD = DPROD(RSAVS - RSBVS, RSAVS + RSBVS)                     03500805
           IF (DSAVD + 1.5805D0) 20160, 10160, 40160                    03510805
40160      IF (DSAVD + 1.5802D0) 10160, 10160, 20160                    03520805
10160      IVPASS = IVPASS + 1                                          03530805
           WRITE (NUVI, 80002) IVTNUM                                   03540805
           GO TO 0161                                                   03550805
20160      IVFAIL = IVFAIL + 1                                          03560805
           DVCORR = -1.580358420D0                                      03570805
           WRITE (NUVI, 80008) IVTNUM                                   03580805
           WRITE (NUVI, 80033) DSAVD                                    03590805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           03600805
 0161      CONTINUE                                                     03610805
CT017*  TEST 17                       DPROD FORMS THE ARGUMENTS TO DDIM 03620805
           IVTNUM = 17                                                  03630805
        DSAVD = DDIM(DPROD(0.4, 2.0), DPROD(3.0, 0.1))                  03640805
           IF (DSAVD - 0.49997D0) 20170, 10170, 40170                   03650805
40170      IF (DSAVD - 0.50003D0) 10170, 10170, 20170                   03660805
10170      IVPASS = IVPASS + 1                                          03670805
           WRITE (NUVI, 80002) IVTNUM                                   03680805
           GO TO 0171                                                   03690805
20170      IVFAIL = IVFAIL + 1                                          03700805
           DVCORR = 0.5D0                                               03710805
           WRITE (NUVI, 80008) IVTNUM                                   03720805
           WRITE (NUVI, 80033) DSAVD                                    03730805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           03740805
 0171      CONTINUE                                                     03750805
CT018*  TEST 18                  ARGUMENTS WITH HIGH AND LOW MAGNITUDES 03760805
           IVTNUM = 18                                                  03770805
        RSAVS = 1.23456E-33                                             03780805
        RSBVS = 1.23456E+34                                             03790805
        DSAVD = DPROD(RSAVS, RSBVS)                                     03800805
           IF (DSAVD - 1.5240D1) 20180, 10180, 40180                    03810805
40180      IF (DSAVD - 1.5242D1) 10180, 10180, 20180                    03820805
10180      IVPASS = IVPASS + 1                                          03830805
           WRITE (NUVI, 80002) IVTNUM                                   03840805
           GO TO 0181                                                   03850805
20180      IVFAIL = IVFAIL + 1                                          03860805
           DVCORR = 1.524138394D1                                       03870805
           WRITE (NUVI, 80008) IVTNUM                                   03880805
           WRITE (NUVI, 80033) DSAVD                                    03890805
           WRITE (NUVI, 80035) DVCORR, REMRKS                           03900805
 0181      CONTINUE                                                     03910805
C*****                                                                  03920805
CBB** ********************** BBCSUM0  **********************************03930805
C**** WRITE OUT TEST SUMMARY                                            03940805
C****                                                                   03950805
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03960805
      WRITE (I02, 90004)                                                03970805
      WRITE (I02, 90014)                                                03980805
      WRITE (I02, 90004)                                                03990805
      WRITE (I02, 90020) IVPASS                                         04000805
      WRITE (I02, 90022) IVFAIL                                         04010805
      WRITE (I02, 90024) IVDELE                                         04020805
      WRITE (I02, 90026) IVINSP                                         04030805
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04040805
CBE** ********************** BBCSUM0  **********************************04050805
CBB** ********************** BBCFOOT0 **********************************04060805
C**** WRITE OUT REPORT FOOTINGS                                         04070805
C****                                                                   04080805
      WRITE (I02,90016) ZPROG, ZPROG                                    04090805
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04100805
      WRITE (I02,90019)                                                 04110805
CBE** ********************** BBCFOOT0 **********************************04120805
CBB** ********************** BBCFMT0A **********************************04130805
C**** FORMATS FOR TEST DETAIL LINES                                     04140805
C****                                                                   04150805
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04160805
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04170805
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04180805
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04190805
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04200805
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04210805
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04220805
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04230805
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04240805
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04250805
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04260805
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04270805
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04280805
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04290805
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04300805
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04310805
80050 FORMAT (1H ,48X,A31)                                              04320805
CBE** ********************** BBCFMT0A **********************************04330805
CBB** ********************** BBCFMAT1 **********************************04340805
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04350805
C****                                                                   04360805
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04370805
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04380805
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04390805
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04400805
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04410805
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04420805
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04430805
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04440805
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04450805
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04460805
     21H(,F12.5,2H, ,F12.5,1H))                                         04470805
CBE** ********************** BBCFMAT1 **********************************04480805
CBB** ********************** BBCFMT0B **********************************04490805
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04500805
C****                                                                   04510805
90002 FORMAT (1H1)                                                      04520805
90004 FORMAT (1H )                                                      04530805
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04540805
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04550805
90008 FORMAT (1H ,21X,A13,A17)                                          04560805
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04570805
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04580805
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04590805
     1       7X,7HREMARKS,24X)                                          04600805
90014 FORMAT (1H ,46H----------------------------------------------,    04610805
     1        33H---------------------------------)                     04620805
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04630805
C****                                                                   04640805
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04650805
C****                                                                   04660805
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04670805
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04680805
     1        A13)                                                      04690805
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04700805
C****                                                                   04710805
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04720805
C****                                                                   04730805
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04740805
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04750805
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04760805
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04770805
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04780805
CBE** ********************** BBCFMT0B **********************************04790805
C*****                                                                  04800805
C*****    END OF TEST SEGMENT 164                                       04810805
        STOP                                                            04820805
        END                                                             04830805
                                                                        04840805
