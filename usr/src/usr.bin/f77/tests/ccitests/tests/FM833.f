C***********************************************************************00010833
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020833
C*****   FM833                                                          00030833
C*****                       YGEN6 - (211)                              00040833
C*****                                                                  00050833
C***********************************************************************00060833
C*****  GENERAL PURPOSE                                         ANS REF 00070833
C*****      TEST GENERIC FUNCTIONS                               15.3   00080833
C*****       SPECIFIC AND GENERIC NAME OF SAME FUNCTION WITH    TABLE 5 00090833
C*****       SAME TYPE OF ARGUMENT IN A STATEMENT                       00100833
C*****                                                                  00110833
CBB** ********************** BBCCOMNT **********************************00120833
C****                                                                   00130833
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140833
C****                          VERSION 2.0                              00150833
C****                                                                   00160833
C****                                                                   00170833
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180833
C****                   GENERAL SERVICES ADMINISTRATION                 00190833
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200833
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210833
C****                      FALLS CHURCH, VA. 22041                      00220833
C****                                                                   00230833
C****                          (703) 756-6153                           00240833
C****                                                                   00250833
CBE** ********************** BBCCOMNT **********************************00260833
C*****                                                                  00270833
C*****  S P E C I F I C A T I O N S  SEGMENT 211                        00280833
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00290833
        COMPLEX AVC, BVC, ZVCORR                                        00300833
        REAL R2E(2)                                                     00310833
        EQUIVALENCE (AVC, R2E)                                          00320833
C*****                                                                  00330833
CBB** ********************** BBCINITA **********************************00340833
C**** SPECIFICATION STATEMENTS                                          00350833
C****                                                                   00360833
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00370833
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00380833
CBE** ********************** BBCINITA **********************************00390833
CBB** ********************** BBCINITB **********************************00400833
C**** INITIALIZE SECTION                                                00410833
      DATA  ZVERS,                  ZVERSD,             ZDATE           00420833
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00430833
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00440833
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00450833
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00460833
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00470833
      DATA   REMRKS /'                               '/                 00480833
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00490833
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00500833
C****                                                                   00510833
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00520833
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00530833
CZ03  ZPROG  = 'PROGRAM NAME'                                           00540833
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00610833
      IVPASS = 0                                                        00620833
      IVFAIL = 0                                                        00630833
      IVDELE = 0                                                        00640833
      IVINSP = 0                                                        00650833
      IVTOTL = 0                                                        00660833
      IVTOTN = 0                                                        00670833
      ICZERO = 0                                                        00680833
C                                                                       00690833
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00700833
      I01 = 05                                                          00710833
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00720833
      I02 = 06                                                          00730833
C                                                                       00740833
      I01 = 5                                                           00750833
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00760833
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00770833
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00780833
C                                                                       00790833
      I02 = 6                                                           00800833
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00810833
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00820833
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00830833
C                                                                       00840833
CBE** ********************** BBCINITB **********************************00850833
      NUVI = I02                                                        00860833
      IVTOTL = 11                                                       00870833
      ZPROG = 'FM833'                                                   00880833
CBB** ********************** BBCHED0A **********************************00890833
C****                                                                   00900833
C**** WRITE REPORT TITLE                                                00910833
C****                                                                   00920833
      WRITE (I02, 90002)                                                00930833
      WRITE (I02, 90006)                                                00940833
      WRITE (I02, 90007)                                                00950833
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00960833
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00970833
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00980833
CBE** ********************** BBCHED0A **********************************00990833
C*****                                                                  01000833
C*****    HEADER FOR SEGMENT 211                                        01010833
        WRITE(NUVI,21100)                                               01020833
21100   FORMAT( 1H , /  35H YGEN6 - (211) GENERIC FUNCTIONS --//        01030833
     1  59H  SPECIFIC AND GENERIC NAME OF SAME FUNCTION IN A STATEMENT//01040833
     2  17H  ANS REF. - 15.3)                                           01050833
CBB** ********************** BBCHED0B **********************************01060833
C**** WRITE DETAIL REPORT HEADERS                                       01070833
C****                                                                   01080833
      WRITE (I02,90004)                                                 01090833
      WRITE (I02,90004)                                                 01100833
      WRITE (I02,90013)                                                 01110833
      WRITE (I02,90014)                                                 01120833
      WRITE (I02,90015) IVTOTL                                          01130833
CBE** ********************** BBCHED0B **********************************01140833
C*****                                                                  01150833
CT001*  TEST 1                      TEST OF ISIGN AND SIGN WITH INTEGER 01160833
           IVTNUM = 1                                                   01170833
        KVI = 5                                                         01180833
        JVI = -3                                                        01190833
        LVI = ISIGN(KVI, JVI) - SIGN(KVI, JVI)                          01200833
           IF (LVI -  0) 20010, 10010, 20010                            01210833
10010      IVPASS = IVPASS + 1                                          01220833
           WRITE (NUVI, 80002) IVTNUM                                   01230833
           GO TO 0011                                                   01240833
20010      IVFAIL = IVFAIL + 1                                          01250833
           IVCORR =     0                                               01260833
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01270833
 0011      CONTINUE                                                     01280833
CT002*  TEST 2                         TEST OF AMAX1 AND MAX WITH REALS 01290833
           IVTNUM = 2                                                   01300833
        BVS = 2.5                                                       01310833
        CVS = 3.5                                                       01320833
        AVS = AMAX1(BVS, CVS) - MAX(BVS, CVS)                           01330833
           IF (AVS + 0.50000E-04) 20020, 10020, 40020                   01340833
40020      IF (AVS - 0.50000E-04) 10020, 10020, 20020                   01350833
10020      IVPASS = IVPASS + 1                                          01360833
           WRITE (NUVI, 80002) IVTNUM                                   01370833
           GO TO 0021                                                   01380833
20020      IVFAIL = IVFAIL + 1                                          01390833
           RVCORR = 0.0000                                              01400833
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01410833
 0021      CONTINUE                                                     01420833
CT003*  TEST 3                    TEST OF DEXP AND EXP WITH DOUBLE PREC 01430833
           IVTNUM = 3                                                   01440833
        BVD = 1.0D0                                                     01450833
        AVD = DEXP(BVD) - EXP(BVD)                                      01460833
           IF (AVD + 0.5000000000D-09) 20030, 10030, 40030              01470833
40030      IF (AVD - 0.5000000000D-09) 10030, 10030, 20030              01480833
10030      IVPASS = IVPASS + 1                                          01490833
           WRITE (NUVI, 80002) IVTNUM                                   01500833
           GO TO 0031                                                   01510833
20030      IVFAIL = IVFAIL + 1                                          01520833
           DVCORR = 0.00000000D+00                                      01530833
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01540833
 0031      CONTINUE                                                     01550833
CT004*  TEST 4                  TEST OF DTANH AND TANH WITH DOUBLE PREC 01560833
           IVTNUM = 4                                                   01570833
        BVD = 0.5D0                                                     01580833
        AVD = DTANH(BVD) - TANH(BVD)                                    01590833
           IF (AVD + 0.5000000000D-09) 20040, 10040, 40040              01600833
40040      IF (AVD - 0.5000000000D-09) 10040, 10040, 20040              01610833
10040      IVPASS = IVPASS + 1                                          01620833
           WRITE (NUVI, 80002) IVTNUM                                   01630833
           GO TO 0041                                                   01640833
20040      IVFAIL = IVFAIL + 1                                          01650833
           DVCORR = 0.00000000D+00                                      01660833
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01670833
 0041      CONTINUE                                                     01680833
CT005*  TEST 5                  TEST OF DASIN AND ASIN WITH DOUBLE PREC 01690833
           IVTNUM = 5                                                   01700833
        BVD = -1.0D0                                                    01710833
        AVD = DASIN(BVD) - ASIN(BVD)                                    01720833
           IF (AVD + 0.5000000000D-09) 20050, 10050, 40050              01730833
40050      IF (AVD - 0.5000000000D-09) 10050, 10050, 20050              01740833
10050      IVPASS = IVPASS + 1                                          01750833
           WRITE (NUVI, 80002) IVTNUM                                   01760833
           GO TO 0051                                                   01770833
20050      IVFAIL = IVFAIL + 1                                          01780833
           DVCORR = 0.00000000D+00                                      01790833
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01800833
 0051      CONTINUE                                                     01810833
CT006*  TEST 6                 TEST OF DNINT AND ANINT WITH DOUBLE PREC 01820833
           IVTNUM = 6                                                   01830833
        BVD = 2.75D0                                                    01840833
        AVD = DNINT(BVD) - ANINT(BVD)                                   01850833
           IF (AVD + 0.5000000000D-09) 20060, 10060, 40060              01860833
40060      IF (AVD - 0.5000000000D-09) 10060, 10060, 20060              01870833
10060      IVPASS = IVPASS + 1                                          01880833
           WRITE (NUVI, 80002) IVTNUM                                   01890833
           GO TO 0061                                                   01900833
20060      IVFAIL = IVFAIL + 1                                          01910833
           DVCORR = 0.00000000D+00                                      01920833
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01930833
 0061      CONTINUE                                                     01940833
CT007*  TEST 7                    TEST OF DMOD AND MOD WITH DOUBLE PREC 01950833
           IVTNUM = 7                                                   01960833
        BVD = 6.0D0                                                     01970833
        CVD = 3.0D0                                                     01980833
        AVD = DMOD(BVD, CVD) - MOD(BVD, CVD)                            01990833
           IF (AVD + 0.5000000000D-09) 20070, 10070, 40070              02000833
40070      IF (AVD - 0.5000000000D-09) 10070, 10070, 20070              02010833
10070      IVPASS = IVPASS + 1                                          02020833
           WRITE (NUVI, 80002) IVTNUM                                   02030833
           GO TO 0071                                                   02040833
20070      IVFAIL = IVFAIL + 1                                          02050833
           DVCORR = 0.00000000D+00                                      02060833
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02070833
 0071      CONTINUE                                                     02080833
CT008*  TEST 8                        TEST OF CABS AND ABS WITH COMPLEX 02090833
           IVTNUM = 8                                                   02100833
        BVC = (4.0, 3.0)                                                02110833
        AVC = CABS(BVC) - ABS(BVC)                                      02120833
           IF (R2E(1) + 0.50000E-04) 20080, 40082, 40081                02130833
40081      IF (R2E(1) - 0.50000E-04) 40082, 40082, 20080                02140833
40082      IF (R2E(2) + 0.50000E-04) 20080, 10080, 40080                02150833
40080      IF (R2E(2) - 0.50000E-04) 10080, 10080, 20080                02160833
10080      IVPASS = IVPASS + 1                                          02170833
           WRITE (NUVI, 80002) IVTNUM                                   02180833
           GO TO 0081                                                   02190833
20080      IVFAIL = IVFAIL + 1                                          02200833
           ZVCORR = ( 0.0000,  0.0000)                                  02210833
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02220833
 0081      CONTINUE                                                     02230833
CT009*  TEST 9                      TEST OF CSQRT AND SQRT WITH COMPLEX 02240833
           IVTNUM = 9                                                   02250833
        BVC = (3.0, 4.0)                                                02260833
        AVC = CSQRT(BVC) - SQRT(BVC)                                    02270833
           IF (R2E(1) + 0.50000E-04) 20090, 40092, 40091                02280833
40091      IF (R2E(1) - 0.50000E-04) 40092, 40092, 20090                02290833
40092      IF (R2E(2) + 0.50000E-04) 20090, 10090, 40090                02300833
40090      IF (R2E(2) - 0.50000E-04) 10090, 10090, 20090                02310833
10090      IVPASS = IVPASS + 1                                          02320833
           WRITE (NUVI, 80002) IVTNUM                                   02330833
           GO TO 0091                                                   02340833
20090      IVFAIL = IVFAIL + 1                                          02350833
           ZVCORR = ( 0.0000,  0.0000)                                  02360833
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02370833
 0091      CONTINUE                                                     02380833
CT010*  TEST 10                       TEST OF CLOG AND LOG WITH COMPLEX 02390833
           IVTNUM = 10                                                  02400833
        BVC = (1.0, 0.0)                                                02410833
        AVC = CLOG(BVC) - LOG(BVC)                                      02420833
           IF (R2E(1) + 0.50000E-04) 20100, 40102, 40101                02430833
40101      IF (R2E(1) - 0.50000E-04) 40102, 40102, 20100                02440833
40102      IF (R2E(2) + 0.50000E-04) 20100, 10100, 40100                02450833
40100      IF (R2E(2) - 0.50000E-04) 10100, 10100, 20100                02460833
10100      IVPASS = IVPASS + 1                                          02470833
           WRITE (NUVI, 80002) IVTNUM                                   02480833
           GO TO 0101                                                   02490833
20100      IVFAIL = IVFAIL + 1                                          02500833
           ZVCORR = ( 0.0000,  0.0000)                                  02510833
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02520833
 0101      CONTINUE                                                     02530833
CT011*  TEST 11                       TEST OF CSIN AND SIN WITH COMPLEX 02540833
           IVTNUM = 11                                                  02550833
         BVC = (1.5, 3.5)                                               02560833
         AVC = CSIN(BVC) - SIN(BVC)                                     02570833
            IF (R2E(1) + 0.50000E-04) 20110, 40112, 40111               02580833
40111       IF (R2E(1) - 0.50000E-04) 40112, 40112, 20110               02590833
40112       IF (R2E(2) + 0.50000E-04) 20110, 10110, 40110               02600833
40110       IF (R2E(2) - 0.50000E-04) 10110, 10110, 20110               02610833
10110       IVPASS = IVPASS + 1                                         02620833
            WRITE (NUVI, 80002) IVTNUM                                  02630833
            GO TO 0111                                                  02640833
20110       IVFAIL = IVFAIL + 1                                         02650833
            ZVCORR = ( 0.0000,  0.0000)                                 02660833
            WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                     02670833
 0111       CONTINUE                                                    02680833
C*****                                                                  02690833
CBB** ********************** BBCSUM0  **********************************02700833
C**** WRITE OUT TEST SUMMARY                                            02710833
C****                                                                   02720833
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02730833
      WRITE (I02, 90004)                                                02740833
      WRITE (I02, 90014)                                                02750833
      WRITE (I02, 90004)                                                02760833
      WRITE (I02, 90020) IVPASS                                         02770833
      WRITE (I02, 90022) IVFAIL                                         02780833
      WRITE (I02, 90024) IVDELE                                         02790833
      WRITE (I02, 90026) IVINSP                                         02800833
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02810833
CBE** ********************** BBCSUM0  **********************************02820833
CBB** ********************** BBCFOOT0 **********************************02830833
C**** WRITE OUT REPORT FOOTINGS                                         02840833
C****                                                                   02850833
      WRITE (I02,90016) ZPROG, ZPROG                                    02860833
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02870833
      WRITE (I02,90019)                                                 02880833
CBE** ********************** BBCFOOT0 **********************************02890833
CBB** ********************** BBCFMT0A **********************************02900833
C**** FORMATS FOR TEST DETAIL LINES                                     02910833
C****                                                                   02920833
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02930833
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02940833
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02950833
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02960833
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02970833
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02980833
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02990833
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03000833
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03010833
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03020833
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03030833
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03040833
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03050833
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03060833
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03070833
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03080833
80050 FORMAT (1H ,48X,A31)                                              03090833
CBE** ********************** BBCFMT0A **********************************03100833
CBB** ********************** BBCFMAT1 **********************************03110833
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03120833
C****                                                                   03130833
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03140833
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03150833
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03160833
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03170833
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03180833
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03190833
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03200833
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03210833
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03220833
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03230833
     21H(,F12.5,2H, ,F12.5,1H))                                         03240833
CBE** ********************** BBCFMAT1 **********************************03250833
CBB** ********************** BBCFMT0B **********************************03260833
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03270833
C****                                                                   03280833
90002 FORMAT (1H1)                                                      03290833
90004 FORMAT (1H )                                                      03300833
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03310833
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03320833
90008 FORMAT (1H ,21X,A13,A17)                                          03330833
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03340833
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03350833
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03360833
     1       7X,7HREMARKS,24X)                                          03370833
90014 FORMAT (1H ,46H----------------------------------------------,    03380833
     1        33H---------------------------------)                     03390833
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03400833
C****                                                                   03410833
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03420833
C****                                                                   03430833
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03440833
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03450833
     1        A13)                                                      03460833
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03470833
C****                                                                   03480833
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03490833
C****                                                                   03500833
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03510833
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03520833
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03530833
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03540833
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03550833
CBE** ********************** BBCFMT0B **********************************03560833
C*****                                                                  03570833
C*****    END OF TEST SEGMENT 211                                       03580833
      STOP                                                              03590833
      END                                                               03600833
