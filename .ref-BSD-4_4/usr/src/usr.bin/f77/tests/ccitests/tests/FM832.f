C***********************************************************************00010832
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020832
C*****   FM832                                                          00030832
C*****                       YGEN5 - (210)                              00040832
C*****                                                                  00050832
C***********************************************************************00060832
C*****  GENERAL PURPOSE                                         ANS REF 00070832
C*****      TEST GENERIC FUNCTIONS                               15.3   00080832
C*****       SQRT,EXP,LOG,LOG10,COS,SINH,TANH,ASIN,ATAN,ATAN2   TABLE 5 00090832
C*****          EACH FUNCTION IS FIRST CALLED WITH A REAL VALUE         00100832
C*****          AND THEN WITH A DOUBLE PRECISION VALUE                  00110832
C*****                                                                  00120832
CBB** ********************** BBCCOMNT **********************************00130832
C****                                                                   00140832
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150832
C****                          VERSION 2.0                              00160832
C****                                                                   00170832
C****                                                                   00180832
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190832
C****                   GENERAL SERVICES ADMINISTRATION                 00200832
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210832
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220832
C****                      FALLS CHURCH, VA. 22041                      00230832
C****                                                                   00240832
C****                          (703) 756-6153                           00250832
C****                                                                   00260832
CBE** ********************** BBCCOMNT **********************************00270832
C*****                                                                  00280832
C*****  S P E C I F I C A T I O N S  SEGMENT 210                        00290832
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00300832
C*****                                                                  00310832
CBB** ********************** BBCINITA **********************************00320832
C**** SPECIFICATION STATEMENTS                                          00330832
C****                                                                   00340832
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350832
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360832
CBE** ********************** BBCINITA **********************************00370832
CBB** ********************** BBCINITB **********************************00380832
C**** INITIALIZE SECTION                                                00390832
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400832
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410832
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420832
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430832
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440832
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450832
      DATA   REMRKS /'                               '/                 00460832
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470832
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480832
C****                                                                   00490832
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500832
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510832
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520832
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590832
      IVPASS = 0                                                        00600832
      IVFAIL = 0                                                        00610832
      IVDELE = 0                                                        00620832
      IVINSP = 0                                                        00630832
      IVTOTL = 0                                                        00640832
      IVTOTN = 0                                                        00650832
      ICZERO = 0                                                        00660832
C                                                                       00670832
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680832
      I01 = 05                                                          00690832
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700832
      I02 = 06                                                          00710832
C                                                                       00720832
      I01 = 5                                                           00730832
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740832
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750832
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760832
C                                                                       00770832
      I02 = 6                                                           00780832
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790832
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800832
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810832
C                                                                       00820832
CBE** ********************** BBCINITB **********************************00830832
      NUVI = I02                                                        00840832
      IVTOTL = 20                                                       00850832
      ZPROG = 'FM832'                                                   00860832
CBB** ********************** BBCHED0A **********************************00870832
C****                                                                   00880832
C**** WRITE REPORT TITLE                                                00890832
C****                                                                   00900832
      WRITE (I02, 90002)                                                00910832
      WRITE (I02, 90006)                                                00920832
      WRITE (I02, 90007)                                                00930832
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940832
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950832
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960832
CBE** ********************** BBCHED0A **********************************00970832
C*****                                                                  00980832
C*****    HEADER FOR SEGMENT 210                                        00990832
        WRITE(NUVI,21000)                                               01000832
21000   FORMAT( 1H , /  35H YGEN5 - (210) GENERIC FUNCTIONS --//        01010832
     1          50H  SQRT,EXP,LOG,LOG10,COS,SINH,TANH,ASIN,ATAN,ATAN2// 01020832
     2          17H  ANS REF. - 15.3)                                   01030832
CBB** ********************** BBCHED0B **********************************01040832
C**** WRITE DETAIL REPORT HEADERS                                       01050832
C****                                                                   01060832
      WRITE (I02,90004)                                                 01070832
      WRITE (I02,90004)                                                 01080832
      WRITE (I02,90013)                                                 01090832
      WRITE (I02,90014)                                                 01100832
      WRITE (I02,90015) IVTOTL                                          01110832
CBE** ********************** BBCHED0B **********************************01120832
C*****                                                                  01130832
C*****    TEST WITH REAL ARGUMENTS                                      01140832
C*****                                                                  01150832
        WRITE(NUVI, 21001)                                              01160832
21001   FORMAT (/ 8X, 24HTEST WITH REAL ARGUMENTS)                      01170832
CT001*  TEST 1                                             TEST OF SQRT 01180832
           IVTNUM = 1                                                   01190832
        AVS = 2.0                                                       01200832
        BVS = 1.0                                                       01210832
        AVD = SQRT(AVS*BVS)                                             01220832
           IF (AVD -  0.14141E+01) 20010, 10010, 40010                  01230832
40010      IF (AVD -  0.14143E+01) 10010, 10010, 20010                  01240832
10010      IVPASS = IVPASS + 1                                          01250832
           WRITE (NUVI, 80002) IVTNUM                                   01260832
           GO TO 0011                                                   01270832
20010      IVFAIL = IVFAIL + 1                                          01280832
           RVCORR =          0.14142135381699E+01                       01290832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      01300832
 0011      CONTINUE                                                     01310832
CT002*  TEST 2                                              TEST OF EXP 01320832
           IVTNUM = 2                                                   01330832
        AVS = 10.0                                                      01340832
        AVD = EXP(AVS / 10.0)                                           01350832
           IF (AVD -  0.27181E+01) 20020, 10020, 40020                  01360832
40020      IF (AVD -  0.27185E+01) 10020, 10020, 20020                  01370832
10020      IVPASS = IVPASS + 1                                          01380832
           WRITE (NUVI, 80002) IVTNUM                                   01390832
           GO TO 0021                                                   01400832
20020      IVFAIL = IVFAIL + 1                                          01410832
           RVCORR =          0.27182817459106E+01                       01420832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      01430832
 0021      CONTINUE                                                     01440832
CT003*  TEST 3                                              TEST OF LOG 01450832
           IVTNUM = 3                                                   01460832
        AVS = 0.1234                                                    01470832
        BVS = .0000567                                                  01480832
        AVD = LOG(AVS + BVS)                                            01490832
           IF (AVD +  0.20920E+01) 20030, 10030, 40030                  01500832
40030      IF (AVD +  0.20917E+01) 10030, 10030, 20030                  01510832
10030      IVPASS = IVPASS + 1                                          01520832
           WRITE (NUVI, 80002) IVTNUM                                   01530832
           GO TO 0031                                                   01540832
20030      IVFAIL = IVFAIL + 1                                          01550832
           RVCORR =         -0.20918648242950E+01                       01560832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      01570832
 0031      CONTINUE                                                     01580832
CT004*  TEST 4                                            TEST OF LOG10 01590832
           IVTNUM = 4                                                   01600832
        AVS = 0.375                                                     01610832
        BVD = 3.75D0                                                    01620832
        AVD = LOG10(AVS)                                                01630832
           IF (AVD +  0.42599E+00) 20040, 10040, 40040                  01640832
40040      IF (AVD +  0.42594E+00) 10040, 10040, 20040                  01650832
10040      IVPASS = IVPASS + 1                                          01660832
           WRITE (NUVI, 80002) IVTNUM                                   01670832
           GO TO 0041                                                   01680832
20040      IVFAIL = IVFAIL + 1                                          01690832
           RVCORR =         -0.42596873641014E+00                       01700832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      01710832
 0041      CONTINUE                                                     01720832
CT005*  TEST 5                                              TEST OF COS 01730832
           IVTNUM = 5                                                   01740832
        AVS = .25                                                       01750832
        AVD = COS(AVS*2)                                                01760832
           IF (AVD -  0.87753E+00) 20050, 10050, 40050                  01770832
40050      IF (AVD -  0.87763E+00) 10050, 10050, 20050                  01780832
10050      IVPASS = IVPASS + 1                                          01790832
           WRITE (NUVI, 80002) IVTNUM                                   01800832
           GO TO 0051                                                   01810832
20050      IVFAIL = IVFAIL + 1                                          01820832
           RVCORR =          0.87758255004883E+00                       01830832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      01840832
 0051      CONTINUE                                                     01850832
CT006*  TEST 6                                             TEST OF SINH 01860832
           IVTNUM = 6                                                   01870832
        AVD = SINH(AVS+3.0)                                             01880832
           IF (AVD -  0.12875E+02) 20060, 10060, 40060                  01890832
40060      IF (AVD -  0.12877E+02) 10060, 10060, 20060                  01900832
10060      IVPASS = IVPASS + 1                                          01910832
           WRITE (NUVI, 80002) IVTNUM                                   01920832
           GO TO 0061                                                   01930832
20060      IVFAIL = IVFAIL + 1                                          01940832
           RVCORR =          0.12875782966614E+02                       01950832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      01960832
 0061      CONTINUE                                                     01970832
CT007*  TEST 7                                             TEST OF TANH 01980832
           IVTNUM = 7                                                   01990832
        CVD = 0.5D1                                                     02000832
        AVD = TANH(AVS*20.0)                                            02010832
           IF (AVD -  0.99986E+00) 20070, 10070, 40070                  02020832
40070      IF (AVD -  0.99996E+00) 10070, 10070, 20070                  02030832
10070      IVPASS = IVPASS + 1                                          02040832
           WRITE (NUVI, 80002) IVTNUM                                   02050832
           GO TO 0071                                                   02060832
20070      IVFAIL = IVFAIL + 1                                          02070832
           RVCORR =          0.99990922212601E+00                       02080832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      02090832
 0071      CONTINUE                                                     02100832
CT008*  TEST 8                                             TEST OF ASIN 02110832
           IVTNUM = 8                                                   02120832
        AVD = ASIN(AVS*4.0)                                             02130832
           IF (AVD -  0.15707E+01) 20080, 10080, 40080                  02140832
40080      IF (AVD -  0.15709E+01) 10080, 10080, 20080                  02150832
10080      IVPASS = IVPASS + 1                                          02160832
           WRITE (NUVI, 80002) IVTNUM                                   02170832
           GO TO 0081                                                   02180832
20080      IVFAIL = IVFAIL + 1                                          02190832
           RVCORR =          0.15707963705063E+01                       02200832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      02210832
 0081      CONTINUE                                                     02220832
CT009*  TEST 9                                             TEST OF ATAN 02230832
           IVTNUM = 9                                                   02240832
        AVS = 500.0                                                     02250832
        AVD = ATAN(-2.0*AVS)                                            02260832
           IF (AVD +  0.15699E+01) 20090, 10090, 40090                  02270832
40090      IF (AVD +  0.15697E+01) 10090, 10090, 20090                  02280832
10090      IVPASS = IVPASS + 1                                          02290832
           WRITE (NUVI, 80002) IVTNUM                                   02300832
           GO TO 0091                                                   02310832
20090      IVFAIL = IVFAIL + 1                                          02320832
           RVCORR =         -0.15697963237762E+01                       02330832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      02340832
 0091      CONTINUE                                                     02350832
CT010*  TEST 10                                           TEST OF ATAN2 02360832
           IVTNUM = 10                                                  02370832
        AVS = 0.0                                                       02380832
        BVS = -5.0                                                      02390832
        AVD = ATAN2(AVS, BVS)                                           02400832
           IF (AVD -  0.31414E+01) 20100, 10100, 40100                  02410832
40100      IF (AVD -  0.31418E+01) 10100, 10100, 20100                  02420832
10100      IVPASS = IVPASS + 1                                          02430832
           WRITE (NUVI, 80002) IVTNUM                                   02440832
           GO TO 0101                                                   02450832
20100      IVFAIL = IVFAIL + 1                                          02460832
           RVCORR =          0.31415927410126E+01                       02470832
           WRITE (NUVI, 80012) IVTNUM, AVD, RVCORR                      02480832
 0101      CONTINUE                                                     02490832
C*****                                                                  02500832
           WRITE (NUVI, 90002)                                          02510832
           WRITE (NUVI, 90013)                                          02520832
           WRITE (NUVI, 90014)                                          02530832
C*****                                                                  02540832
C*****    TEST WITH DOUBLE PRECISION ARGUMENTS                          02550832
C*****                                                                  02560832
        WRITE (NUVI, 21002)                                             02570832
21002   FORMAT (/ 08X, 36HTEST WITH DOUBLE PRECISION ARGUMENTS)         02580832
CT011*  TEST 11                                            TEST OF SQRT 02590832
           IVTNUM = 11                                                  02600832
        AVS = 2.0                                                       02610832
        BVS = 1.0                                                       02620832
        BVD = SQRT(DBLE(AVS))                                           02630832
           IF (BVD -  0.1414213561D+01) 20110, 10110, 40110             02640832
40110      IF (BVD -  0.1414213563D+01) 10110, 10110, 20110             02650832
10110      IVPASS = IVPASS + 1                                          02660832
           WRITE (NUVI, 80002) IVTNUM                                   02670832
           GO TO 0111                                                   02680832
20110      IVFAIL = IVFAIL + 1                                          02690832
           DVCORR =          0.14142135623731D+01                       02700832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      02710832
 0111      CONTINUE                                                     02720832
CT012*  TEST 12                                             TEST OF EXP 02730832
           IVTNUM = 12                                                  02740832
        AVS = 10.0                                                      02750832
        BVD = EXP(1.0D0)                                                02760832
           IF (BVD -  0.2718281827D+01) 20120, 10120, 40120             02770832
40120      IF (BVD -  0.2718281830D+01) 10120, 10120, 20120             02780832
10120      IVPASS = IVPASS + 1                                          02790832
           WRITE (NUVI, 80002) IVTNUM                                   02800832
           GO TO 0121                                                   02810832
20120      IVFAIL = IVFAIL + 1                                          02820832
           DVCORR =          0.27182818284590D+01                       02830832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      02840832
 0121      CONTINUE                                                     02850832
CT013*  TEST 13                                             TEST OF LOG 02860832
           IVTNUM = 13                                                  02870832
        AVS = 0.1234                                                    02880832
        BVS = .0000567                                                  02890832
        BVD = LOG(0.1234567D0)                                          02900832
           IF (BVD +  0.2091864793D+01) 20130, 10130, 40130             02910832
40130      IF (BVD +  0.2091864790D+01) 10130, 10130, 20130             02920832
10130      IVPASS = IVPASS + 1                                          02930832
           WRITE (NUVI, 80002) IVTNUM                                   02940832
           GO TO 0131                                                   02950832
20130      IVFAIL = IVFAIL + 1                                          02960832
           DVCORR =         -0.20918647916786D+01                       02970832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      02980832
 0131      CONTINUE                                                     02990832
CT014*  TEST 14                                           TEST OF LOG10 03000832
           IVTNUM = 14                                                  03010832
        AVS = 0.375                                                     03020832
        BVD = 3.75D0                                                    03030832
        BVD = LOG10(BVD / 1.0D1)                                        03040832
           IF (BVD +  0.4259687325D+00) 20140, 10140, 40140             03050832
40140      IF (BVD +  0.4259687320D+00) 10140, 10140, 20140             03060832
10140      IVPASS = IVPASS + 1                                          03070832
           WRITE (NUVI, 80002) IVTNUM                                   03080832
           GO TO 0141                                                   03090832
20140      IVFAIL = IVFAIL + 1                                          03100832
           DVCORR =         -0.42596873227228D+00                       03110832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03120832
 0141      CONTINUE                                                     03130832
CT015*  TEST 15                                             TEST OF COS 03140832
           IVTNUM = 15                                                  03150832
        AVS = .25                                                       03160832
        BVD = COS(0.5D0)                                                03170832
           IF (BVD -  0.8775825614D+00) 20150, 10150, 40150             03180832
40150      IF (BVD -  0.8775825624D+00) 10150, 10150, 20150             03190832
10150      IVPASS = IVPASS + 1                                          03200832
           WRITE (NUVI, 80002) IVTNUM                                   03210832
           GO TO 0151                                                   03220832
20150      IVFAIL = IVFAIL + 1                                          03230832
           DVCORR =          0.87758256189037D+00                       03240832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03250832
 0151      CONTINUE                                                     03260832
CT016*  TEST 16                                            TEST OF SINH 03270832
           IVTNUM = 16                                                  03280832
        BVD = SINH(3.25D0)                                              03290832
           IF (BVD -  0.1287578284D+02) 20160, 10160, 40160             03300832
40160      IF (BVD -  0.1287578286D+02) 10160, 10160, 20160             03310832
10160      IVPASS = IVPASS + 1                                          03320832
           WRITE (NUVI, 80002) IVTNUM                                   03330832
           GO TO 0161                                                   03340832
20160      IVFAIL = IVFAIL + 1                                          03350832
           DVCORR =          0.12875782854681D+02                       03360832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03370832
 0161      CONTINUE                                                     03380832
CT017*  TEST 17                                            TEST OF TANH 03390832
           IVTNUM = 17                                                  03400832
        CVD = 0.5D1                                                     03410832
        BVD = TANH(CVD)                                                 03420832
           IF (BVD -  0.9999092037D+00) 20170, 10170, 40170             03430832
40170      IF (BVD -  0.9999092048D+00) 10170, 10170, 20170             03440832
10170      IVPASS = IVPASS + 1                                          03450832
           WRITE (NUVI, 80002) IVTNUM                                   03460832
           GO TO 0171                                                   03470832
20170      IVFAIL = IVFAIL + 1                                          03480832
           DVCORR =          0.99990920426260D+00                       03490832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03500832
 0171      CONTINUE                                                     03510832
CT018*  TEST 18                                            TEST OF ASIN 03520832
           IVTNUM = 18                                                  03530832
        BVD = ASIN(100.0D0 / 1.0D2)                                     03540832
           IF (BVD -  0.1570796326D+01) 20180, 10180, 40180             03550832
40180      IF (BVD -  0.1570796328D+01) 10180, 10180, 20180             03560832
10180      IVPASS = IVPASS + 1                                          03570832
           WRITE (NUVI, 80002) IVTNUM                                   03580832
           GO TO 0181                                                   03590832
20180      IVFAIL = IVFAIL + 1                                          03600832
           DVCORR =          0.15707963267949D+01                       03610832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03620832
 0181      CONTINUE                                                     03630832
CT019*  TEST 19                                            TEST OF ATAN 03640832
           IVTNUM = 19                                                  03650832
        AVS = 500.0                                                     03660832
        BVD = ATAN(-1.0D3)                                              03670832
           IF (BVD +  0.1569796328D+01) 20190, 10190, 40190             03680832
40190      IF (BVD +  0.1569796326D+01) 10190, 10190, 20190             03690832
10190      IVPASS = IVPASS + 1                                          03700832
           WRITE (NUVI, 80002) IVTNUM                                   03710832
           GO TO 0191                                                   03720832
20190      IVFAIL = IVFAIL + 1                                          03730832
           DVCORR =         -0.15697963271282D+01                       03740832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03750832
 0191      CONTINUE                                                     03760832
CT020*  TEST 20                                           TEST OF ATAN2 03770832
           IVTNUM = 20                                                  03780832
        AVS = 0.0                                                       03790832
        BVS = -5.0                                                      03800832
        BVD = ATAN2(0.0D0, -5.0D0)                                      03810832
           IF (BVD -  0.3141592652D+01) 20200, 10200, 40200             03820832
40200      IF (BVD -  0.3141592655D+01) 10200, 10200, 20200             03830832
10200      IVPASS = IVPASS + 1                                          03840832
           WRITE (NUVI, 80002) IVTNUM                                   03850832
           GO TO 0201                                                   03860832
20200      IVFAIL = IVFAIL + 1                                          03870832
           DVCORR =          0.31415926535898D+01                       03880832
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      03890832
 0201      CONTINUE                                                     03900832
C*****                                                                  03910832
CBB** ********************** BBCSUM0  **********************************03920832
C**** WRITE OUT TEST SUMMARY                                            03930832
C****                                                                   03940832
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03950832
      WRITE (I02, 90004)                                                03960832
      WRITE (I02, 90014)                                                03970832
      WRITE (I02, 90004)                                                03980832
      WRITE (I02, 90020) IVPASS                                         03990832
      WRITE (I02, 90022) IVFAIL                                         04000832
      WRITE (I02, 90024) IVDELE                                         04010832
      WRITE (I02, 90026) IVINSP                                         04020832
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04030832
CBE** ********************** BBCSUM0  **********************************04040832
CBB** ********************** BBCFOOT0 **********************************04050832
C**** WRITE OUT REPORT FOOTINGS                                         04060832
C****                                                                   04070832
      WRITE (I02,90016) ZPROG, ZPROG                                    04080832
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04090832
      WRITE (I02,90019)                                                 04100832
CBE** ********************** BBCFOOT0 **********************************04110832
CBB** ********************** BBCFMT0A **********************************04120832
C**** FORMATS FOR TEST DETAIL LINES                                     04130832
C****                                                                   04140832
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04150832
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04160832
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04170832
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04180832
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04190832
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04200832
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04210832
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04220832
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04230832
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04240832
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04250832
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04260832
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04270832
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04280832
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04290832
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04300832
80050 FORMAT (1H ,48X,A31)                                              04310832
CBE** ********************** BBCFMT0A **********************************04320832
CBB** ********************** BBCFMAT1 **********************************04330832
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04340832
C****                                                                   04350832
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04360832
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04370832
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04380832
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04390832
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04400832
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04410832
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04420832
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04430832
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04440832
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04450832
     21H(,F12.5,2H, ,F12.5,1H))                                         04460832
CBE** ********************** BBCFMAT1 **********************************04470832
CBB** ********************** BBCFMT0B **********************************04480832
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04490832
C****                                                                   04500832
90002 FORMAT (1H1)                                                      04510832
90004 FORMAT (1H )                                                      04520832
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04530832
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04540832
90008 FORMAT (1H ,21X,A13,A17)                                          04550832
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04560832
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04570832
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04580832
     1       7X,7HREMARKS,24X)                                          04590832
90014 FORMAT (1H ,46H----------------------------------------------,    04600832
     1        33H---------------------------------)                     04610832
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04620832
C****                                                                   04630832
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04640832
C****                                                                   04650832
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04660832
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04670832
     1        A13)                                                      04680832
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04690832
C****                                                                   04700832
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04710832
C****                                                                   04720832
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04730832
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04740832
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04750832
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04760832
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04770832
CBE** ********************** BBCFMT0B **********************************04780832
C*****                                                                  04790832
C*****    END OF TEST SEGMENT 210                                       04800832
      STOP                                                              04810832
      END                                                               04820832
