C***********************************************************************00010831
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020831
C*****   FM831                                                          00030831
C*****                       YGEN3 - (208)                              00040831
C*****                                                                  00050831
C***********************************************************************00060831
C*****  GENERAL PURPOSE                                         ANS REF 00070831
C*****      TEST GENERIC FUNCTIONS                               15.3   00080831
C*****       ABS, MOD, SIGN, SIN, COS, TAN, SINH, COSH, TANH    TABLE 5 00090831
C*****                                                                  00100831
CBB** ********************** BBCCOMNT **********************************00110831
C****                                                                   00120831
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130831
C****                          VERSION 2.0                              00140831
C****                                                                   00150831
C****                                                                   00160831
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170831
C****                   GENERAL SERVICES ADMINISTRATION                 00180831
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190831
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200831
C****                      FALLS CHURCH, VA. 22041                      00210831
C****                                                                   00220831
C****                          (703) 756-6153                           00230831
C****                                                                   00240831
CBE** ********************** BBCCOMNT **********************************00250831
C*****                                                                  00260831
C*****  S P E C I F I C A T I O N S  SEGMENT 208                        00270831
        DOUBLE PRECISION AVD, CVD, DVD, DVCORR                          00280831
        COMPLEX AVC, CVC, ZVCORR                                        00290831
        REAL R2E(2)                                                     00300831
        EQUIVALENCE (AVC, R2E)                                          00310831
C*****                                                                  00320831
CBB** ********************** BBCINITA **********************************00330831
C**** SPECIFICATION STATEMENTS                                          00340831
C****                                                                   00350831
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00360831
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00370831
CBE** ********************** BBCINITA **********************************00380831
CBB** ********************** BBCINITB **********************************00390831
C**** INITIALIZE SECTION                                                00400831
      DATA  ZVERS,                  ZVERSD,             ZDATE           00410831
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00420831
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00430831
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00440831
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00450831
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00460831
      DATA   REMRKS /'                               '/                 00470831
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00480831
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00490831
C****                                                                   00500831
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00510831
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00520831
CZ03  ZPROG  = 'PROGRAM NAME'                                           00530831
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00600831
      IVPASS = 0                                                        00610831
      IVFAIL = 0                                                        00620831
      IVDELE = 0                                                        00630831
      IVINSP = 0                                                        00640831
      IVTOTL = 0                                                        00650831
      IVTOTN = 0                                                        00660831
      ICZERO = 0                                                        00670831
C                                                                       00680831
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00690831
      I01 = 05                                                          00700831
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00710831
      I02 = 06                                                          00720831
C                                                                       00730831
      I01 = 5                                                           00740831
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750831
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00760831
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00770831
C                                                                       00780831
      I02 = 6                                                           00790831
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00800831
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00810831
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00820831
C                                                                       00830831
CBE** ********************** BBCINITB **********************************00840831
      NUVI = I02                                                        00850831
      IVTOTL = 12                                                       00860831
      ZPROG = 'FM831'                                                   00870831
CBB** ********************** BBCHED0A **********************************00880831
C****                                                                   00890831
C**** WRITE REPORT TITLE                                                00900831
C****                                                                   00910831
      WRITE (I02, 90002)                                                00920831
      WRITE (I02, 90006)                                                00930831
      WRITE (I02, 90007)                                                00940831
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00950831
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00960831
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00970831
CBE** ********************** BBCHED0A **********************************00980831
C*****                                                                  00990831
C*****    HEADER FOR SEGMENT 208                                        01000831
        WRITE(NUVI,20800)                                               01010831
20800   FORMAT( 1H , /  35H YGEN3 - (208) GENERIC FUNCTIONS --//        01020831
     1          49H  ABS, MOD, SIGN, SIN, COS, TAN, SINH, COSH, TANH//  01030831
     2          17H  ANS REF. - 15.3)                                   01040831
CBB** ********************** BBCHED0B **********************************01050831
C**** WRITE DETAIL REPORT HEADERS                                       01060831
C****                                                                   01070831
      WRITE (I02,90004)                                                 01080831
      WRITE (I02,90004)                                                 01090831
      WRITE (I02,90013)                                                 01100831
      WRITE (I02,90014)                                                 01110831
      WRITE (I02,90015) IVTOTL                                          01120831
CBE** ********************** BBCHED0B **********************************01130831
C*****                                                                  01140831
CT001*  TEST 1                       TEST OF ABS AND SIGN WITH INTEGERS 01150831
           IVTNUM = 1                                                   01160831
        LVI = ABS(-25) - SIGN(2, -15)                                   01170831
           IF (LVI - 27) 20010, 10010, 20010                            01180831
10010      IVPASS = IVPASS + 1                                          01190831
           WRITE (NUVI, 80002) IVTNUM                                   01200831
           GO TO 0011                                                   01210831
20010      IVFAIL = IVFAIL + 1                                          01220831
           IVCORR = 27                                                  01230831
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01240831
 0011      CONTINUE                                                     01250831
CT002*  TEST 2                     TEST OF MOD, SIGN AND ABS WITH REALS 01260831
           IVTNUM = 2                                                   01270831
        AVS = MOD(24.5, 2.5) + SIGN(-1.50, -5.125) - ABS(-63.5)         01280831
           IF (AVS +  0.63004E+02) 20020, 10020, 40020                  01290831
40020      IF (AVS +  0.62996E+02) 10020, 10020, 20020                  01300831
10020      IVPASS = IVPASS + 1                                          01310831
           WRITE (NUVI, 80002) IVTNUM                                   01320831
           GO TO 0021                                                   01330831
20020      IVFAIL = IVFAIL + 1                                          01340831
           RVCORR = -63.0                                               01350831
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01360831
 0021      CONTINUE                                                     01370831
CT003*  TEST 3                     TEST OF SIN AND COS WITH DOUBLE PREC 01380831
           IVTNUM = 3                                                   01390831
        CVD = 1.125D0                                                   01400831
        AVD = (SIN(CVD)) ** 2 + (COS(CVD)) ** 2                         01410831
           IF (AVD -  0.9999999995D+00) 20030, 10030, 40030             01420831
40030      IF (AVD -  0.1000000001D+01) 10030, 10030, 20030             01430831
10030      IVPASS = IVPASS + 1                                          01440831
           WRITE (NUVI, 80002) IVTNUM                                   01450831
           GO TO 0031                                                   01460831
20030      IVFAIL = IVFAIL + 1                                          01470831
           DVCORR = 1.0D0                                               01480831
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01490831
 0031      CONTINUE                                                     01500831
CT004*  TEST 4                     TEST OF TAN AND MOD WITH DOUBLE PREC 01510831
           IVTNUM = 4                                                   01520831
        AVD = TAN(3.5D0) * MOD(32.5D0, 5.0D0)                           01530831
           IF (AVD -  0.9364640999D+00) 20040, 10040, 40040             01540831
40040      IF (AVD -  0.9364641009D+00) 10040, 10040, 20040             01550831
10040      IVPASS = IVPASS + 1                                          01560831
           WRITE (NUVI, 80002) IVTNUM                                   01570831
           GO TO 0041                                                   01580831
20040      IVFAIL = IVFAIL + 1                                          01590831
           DVCORR = 0.9364641003965D0                                   01600831
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01610831
 0041      CONTINUE                                                     01620831
CT005*  TEST 5                   TEST OF SINH AND COSH WITH DOUBLE PREC 01630831
           IVTNUM = 5                                                   01640831
        CVD = 3.25D0                                                    01650831
        AVD = (SINH(CVD)) ** 2 - (COSH(CVD)) ** 2                       01660831
           IF (AVD +  0.1000000001D+01) 20050, 10050, 40050             01670831
40050      IF (AVD +  0.9999999995D+00) 10050, 10050, 20050             01680831
10050      IVPASS = IVPASS + 1                                          01690831
           WRITE (NUVI, 80002) IVTNUM                                   01700831
           GO TO 0051                                                   01710831
20050      IVFAIL = IVFAIL + 1                                          01720831
           DVCORR = -1.0D0                                              01730831
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01740831
 0051      CONTINUE                                                     01750831
CT006*  TEST 6                            TEST OF TANH WITH DOUBLE PREC 01760831
           IVTNUM = 6                                                   01770831
        AVD = TANH(0.5D0) * TANH(0.75D0)                                01780831
           IF (AVD -  0.2935132281D+00) 20060, 10060, 40060             01790831
40060      IF (AVD -  0.2935132285D+00) 10060, 10060, 20060             01800831
10060      IVPASS = IVPASS + 1                                          01810831
           WRITE (NUVI, 80002) IVTNUM                                   01820831
           GO TO 0061                                                   01830831
20060      IVFAIL = IVFAIL + 1                                          01840831
           DVCORR = 0.29351322831389D0                                  01850831
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01860831
 0061      CONTINUE                                                     01870831
CT007*  TEST 7                     TEST OF ABS AND SIN WITH DOUBLE PREC 01880831
           IVTNUM = 7                                                   01890831
        AVD = ABS(4.57812500D0) * SIN(1.125D0)                          01900831
           IF (AVD -  0.4130693827D+01) 20070, 10070, 40070             01910831
40070      IF (AVD -  0.4130693832D+01) 10070, 10070, 20070             01920831
10070      IVPASS = IVPASS + 1                                          01930831
           WRITE (NUVI, 80002) IVTNUM                                   01940831
           GO TO 0071                                                   01950831
20070      IVFAIL = IVFAIL + 1                                          01960831
           DVCORR = 4.130693829235D0                                    01970831
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01980831
 0071      CONTINUE                                                     01990831
CT008*  TEST 8                     TEST OF ABS, MOD AND SIGN            02000831
C*****                               WITH INTEGER, REAL AND DOUBLE PREC 02010831
           IVTNUM = 8                                                   02020831
        LVI = -25                                                       02030831
        AVS = 32.750                                                    02040831
        BVS = 1.375                                                     02050831
        CVD = 0.75D0                                                    02060831
        DVD = 1.125D0                                                   02070831
        AVD = ABS(LVI) - (MOD(AVS, BVS) * SIGN(CVD, DVD))               02080831
           IF (AVD -  0.2415624998D+02) 20080, 10080, 40080             02090831
40080      IF (AVD -  0.2415625002D+02) 10080, 10080, 20080             02100831
10080      IVPASS = IVPASS + 1                                          02110831
           WRITE (NUVI, 80002) IVTNUM                                   02120831
           GO TO 0081                                                   02130831
20080      IVFAIL = IVFAIL + 1                                          02140831
           DVCORR = 24.15625D0                                          02150831
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02160831
 0081      CONTINUE                                                     02170831
CT009*  TEST 9                                 TEST OF ABS WITH COMPLEX 02180831
           IVTNUM = 9                                                   02190831
        AVS = ABS((-2.125, 5.0))                                        02200831
           IF (AVS -  0.54325E+01) 20090, 10090, 40090                  02210831
40090      IF (AVS -  0.54331E+01) 10090, 10090, 20090                  02220831
10090      IVPASS = IVPASS + 1                                          02230831
           WRITE (NUVI, 80002) IVTNUM                                   02240831
           GO TO 0091                                                   02250831
20090      IVFAIL = IVFAIL + 1                                          02260831
           RVCORR = 5.4328279                                           02270831
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02280831
 0091      CONTINUE                                                     02290831
CT010*  TEST 10                        TEST OF SIN AND COS WITH COMPLEX 02300831
           IVTNUM = 10                                                  02310831
        AVC = SIN((2.5, 3.5)) * COS((-4.75, 1.25))                      02320831
           IF (R2E(1) +  0.20512E+02) 20100, 40102, 40101               02330831
40101      IF (R2E(1) +  0.20510E+02) 40102, 40102, 20100               02340831
40102      IF (R2E(2) +  0.16820E+02) 20100, 10100, 40100               02350831
40100      IF (R2E(2) +  0.16817E+02) 10100, 10100, 20100               02360831
10100      IVPASS = IVPASS + 1                                          02370831
           WRITE (NUVI, 80002) IVTNUM                                   02380831
           GO TO 0101                                                   02390831
20100      IVFAIL = IVFAIL + 1                                          02400831
           ZVCORR = (-20.5109598, -16.8182771)                          02410831
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02420831
 0101      CONTINUE                                                     02430831
CT011*  TEST 11                     TEST OF SIN, COS AND TAN            02440831
C*****                                            WITH REAL AND COMPLEX 02450831
           IVTNUM = 11                                                  02460831
        AVS = 2.0                                                       02470831
        CVC = (3.125, 1.5)                                              02480831
        BVS = 3.5                                                       02490831
        AVC = SIN(AVS) + COS(CVC) + TAN(BVS)                            02500831
           IF (R2E(1) +  0.10683E+01) 20110, 40112, 40111               02510831
40111      IF (R2E(1) +  0.10681E+01) 40112, 40112, 20110               02520831
40112      IF (R2E(2) +  0.35331E-01) 20110, 10110, 40110               02530831
40110      IF (R2E(2) +  0.35327E-01) 10110, 10110, 20110               02540831
10110      IVPASS = IVPASS + 1                                          02550831
           WRITE (NUVI, 80002) IVTNUM                                   02560831
           GO TO 0111                                                   02570831
20110      IVFAIL = IVFAIL + 1                                          02580831
           ZVCORR = (-1.068203, -0.0353288)                             02590831
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02600831
 0111      CONTINUE                                                     02610831
CT012*  TEST 12                     TEST OF ABS, MOD, SIN AND COS       02620831
C*****                                   WITH INTEGER, REAL AND COMPLEX 02630831
           IVTNUM = 12                                                  02640831
        AVC = ABS(-2) * MOD(17.250, 3.125) + SIN(3.125) -               02650831
     1          COS((-0.375, 1.625))                                    02660831
           IF (R2E(1) -  0.81218E+00) 20120, 40122, 40121               02670831
40121      IF (R2E(1) -  0.81227E+00) 40122, 40122, 20120               02680831
40122      IF (R2E(2) +  0.89403E+00) 20120, 10120, 40120               02690831
40120      IF (R2E(2) +  0.89393E+00) 10120, 10120, 20120               02700831
10120      IVPASS = IVPASS + 1                                          02710831
           WRITE (NUVI, 80002) IVTNUM                                   02720831
           GO TO 0121                                                   02730831
20120      IVFAIL = IVFAIL + 1                                          02740831
           ZVCORR = (0.8122242, -0.893981)                              02750831
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02760831
 0121      CONTINUE                                                     02770831
C*****                                                                  02780831
CBB** ********************** BBCSUM0  **********************************02790831
C**** WRITE OUT TEST SUMMARY                                            02800831
C****                                                                   02810831
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02820831
      WRITE (I02, 90004)                                                02830831
      WRITE (I02, 90014)                                                02840831
      WRITE (I02, 90004)                                                02850831
      WRITE (I02, 90020) IVPASS                                         02860831
      WRITE (I02, 90022) IVFAIL                                         02870831
      WRITE (I02, 90024) IVDELE                                         02880831
      WRITE (I02, 90026) IVINSP                                         02890831
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02900831
CBE** ********************** BBCSUM0  **********************************02910831
CBB** ********************** BBCFOOT0 **********************************02920831
C**** WRITE OUT REPORT FOOTINGS                                         02930831
C****                                                                   02940831
      WRITE (I02,90016) ZPROG, ZPROG                                    02950831
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02960831
      WRITE (I02,90019)                                                 02970831
CBE** ********************** BBCFOOT0 **********************************02980831
CBB** ********************** BBCFMT0A **********************************02990831
C**** FORMATS FOR TEST DETAIL LINES                                     03000831
C****                                                                   03010831
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03020831
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03030831
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03040831
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03050831
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03060831
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03070831
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03080831
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03090831
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03100831
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03110831
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03120831
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03130831
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03140831
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03150831
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03160831
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03170831
80050 FORMAT (1H ,48X,A31)                                              03180831
CBE** ********************** BBCFMT0A **********************************03190831
CBB** ********************** BBCFMAT1 **********************************03200831
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03210831
C****                                                                   03220831
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03230831
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03240831
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03250831
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03260831
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03270831
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03280831
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03290831
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03300831
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03310831
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03320831
     21H(,F12.5,2H, ,F12.5,1H))                                         03330831
CBE** ********************** BBCFMAT1 **********************************03340831
CBB** ********************** BBCFMT0B **********************************03350831
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03360831
C****                                                                   03370831
90002 FORMAT (1H1)                                                      03380831
90004 FORMAT (1H )                                                      03390831
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03400831
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03410831
90008 FORMAT (1H ,21X,A13,A17)                                          03420831
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03430831
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03440831
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03450831
     1       7X,7HREMARKS,24X)                                          03460831
90014 FORMAT (1H ,46H----------------------------------------------,    03470831
     1        33H---------------------------------)                     03480831
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03490831
C****                                                                   03500831
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03510831
C****                                                                   03520831
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03530831
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03540831
     1        A13)                                                      03550831
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03560831
C****                                                                   03570831
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03580831
C****                                                                   03590831
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03600831
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03610831
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03620831
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03630831
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03640831
CBE** ********************** BBCFMT0B **********************************03650831
C*****                                                                  03660831
C*****    END OF TEST SEGMENT 208                                       03670831
      STOP                                                              03680831
      END                                                               03690831
