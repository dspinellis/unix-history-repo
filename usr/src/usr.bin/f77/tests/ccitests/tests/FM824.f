C***********************************************************************00010824
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020824
C*****   FM824                                                          00030824
C*****                       YDATAN - (196)                             00040824
C*****                                                                  00050824
C***********************************************************************00060824
C*****  GENERAL PURPOSE                                         ANS REF 00070824
C*****    TEST INTRINSIC FUNCTION DATAN, DATAN2                  15.3   00080824
C*****    INTRINSIC FUNCTION DSQRT ASSUMED WORKING              TABLE 5 00090824
C*****                                                                  00100824
CBB** ********************** BBCCOMNT **********************************00110824
C****                                                                   00120824
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130824
C****                          VERSION 2.0                              00140824
C****                                                                   00150824
C****                                                                   00160824
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170824
C****                   GENERAL SERVICES ADMINISTRATION                 00180824
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190824
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200824
C****                      FALLS CHURCH, VA. 22041                      00210824
C****                                                                   00220824
C****                          (703) 756-6153                           00230824
C****                                                                   00240824
CBE** ********************** BBCCOMNT **********************************00250824
C*****                                                                  00260824
C*****    S P E C I F I C A T I O N S SEGMENT 196                       00270824
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00280824
C*****                                                                  00290824
CBB** ********************** BBCINITA **********************************00300824
C**** SPECIFICATION STATEMENTS                                          00310824
C****                                                                   00320824
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330824
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340824
CBE** ********************** BBCINITA **********************************00350824
CBB** ********************** BBCINITB **********************************00360824
C**** INITIALIZE SECTION                                                00370824
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380824
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390824
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400824
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410824
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420824
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430824
      DATA   REMRKS /'                               '/                 00440824
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450824
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460824
C****                                                                   00470824
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480824
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490824
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500824
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570824
      IVPASS = 0                                                        00580824
      IVFAIL = 0                                                        00590824
      IVDELE = 0                                                        00600824
      IVINSP = 0                                                        00610824
      IVTOTL = 0                                                        00620824
      IVTOTN = 0                                                        00630824
      ICZERO = 0                                                        00640824
C                                                                       00650824
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660824
      I01 = 05                                                          00670824
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680824
      I02 = 06                                                          00690824
C                                                                       00700824
      I01 = 5                                                           00710824
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720824
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730824
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740824
C                                                                       00750824
      I02 = 6                                                           00760824
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770824
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780824
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790824
C                                                                       00800824
CBE** ********************** BBCINITB **********************************00810824
      NUVI = I02                                                        00820824
      IVTOTL = 13                                                       00830824
      ZPROG = 'FM824'                                                   00840824
CBB** ********************** BBCHED0A **********************************00850824
C****                                                                   00860824
C**** WRITE REPORT TITLE                                                00870824
C****                                                                   00880824
      WRITE (I02, 90002)                                                00890824
      WRITE (I02, 90006)                                                00900824
      WRITE (I02, 90007)                                                00910824
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920824
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930824
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940824
CBE** ********************** BBCHED0A **********************************00950824
C*****                                                                  00960824
C*****    HEADER FOR SEGMENT 196                                        00970824
        WRITE(NUVI,19600)                                               00980824
19600   FORMAT(1H , / 36H  YDATAN - (196) INTRINSIC FUNCTIONS//         00990824
     1         45H  DATAN, DATAN2 (DOUBLE PRECISION ARCTANGENT)//       01000824
     2         17H  ANS REF. - 15.3)                                    01010824
CBB** ********************** BBCHED0B **********************************01020824
C**** WRITE DETAIL REPORT HEADERS                                       01030824
C****                                                                   01040824
      WRITE (I02,90004)                                                 01050824
      WRITE (I02,90004)                                                 01060824
      WRITE (I02,90013)                                                 01070824
      WRITE (I02,90014)                                                 01080824
      WRITE (I02,90015) IVTOTL                                          01090824
CBE** ********************** BBCHED0B **********************************01100824
C*****                                                                  01110824
        WRITE(NUVI,19601)                                               01120824
19601   FORMAT(/ 8X, 13HTEST OF DATAN)                                  01130824
C*****                                                                  01140824
CT001*  TEST 1                LARGE ARGUMENT VALUES TO TEST SINGULARITY 01150824
           IVTNUM = 1                                                   01160824
        BVD = 500.0D0                                                   01170824
        AVD = DATAN(BVD)                                                01180824
           IF (AVD - 0.1568796328D+01) 20010, 10010, 40010              01190824
40010      IF (AVD - 0.1568796331D+01) 10010, 10010, 20010              01200824
10010      IVPASS = IVPASS + 1                                          01210824
           WRITE (NUVI, 80002) IVTNUM                                   01220824
           GO TO 0011                                                   01230824
20010      IVFAIL = IVFAIL + 1                                          01240824
           DVCORR = 1.5687963294632946155D+00                           01250824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01260824
 0011      CONTINUE                                                     01270824
CT002*  TEST 2                LARGE ARGUMENT VALUES TO TEST SINGULARITY 01280824
           IVTNUM = 2                                                   01290824
        AVD = DATAN(-1000.0D0)                                          01300824
           IF (AVD + 0.1569796328D+01) 20020, 10020, 40020              01310824
40020      IF (AVD + 0.1569796326D+01) 10020, 10020, 20020              01320824
10020      IVPASS = IVPASS + 1                                          01330824
           WRITE (NUVI, 80002) IVTNUM                                   01340824
           GO TO 0021                                                   01350824
20020      IVFAIL = IVFAIL + 1                                          01360824
           DVCORR = -1.5697963271282297525D+00                          01370824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01380824
 0021      CONTINUE                                                     01390824
CT003*  TEST 3                         AN EXPRESSION PRESENTED TO DATAN 01400824
           IVTNUM = 3                                                   01410824
        AVD = DATAN(100.0D0 / 100.0D0)                                  01420824
           IF (AVD - 0.7853981630D+00) 20030, 10030, 40030              01430824
40030      IF (AVD - 0.7853981638D+00) 10030, 10030, 20030              01440824
10030      IVPASS = IVPASS + 1                                          01450824
           WRITE (NUVI, 80002) IVTNUM                                   01460824
           GO TO 0031                                                   01470824
20030      IVFAIL = IVFAIL + 1                                          01480824
           DVCORR = 0.78539816339744830962D+00                          01490824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01500824
 0031      CONTINUE                                                     01510824
CT004*  TEST 4               THE FUNCTION DSQRT EVALUATED AND PRESENTED 01520824
C*****                       AS AN ARGUMENT                             01530824
           IVTNUM = 4                                                   01540824
        BVD = -DSQRT(3.0D0)                                             01550824
        AVD = DATAN(BVD)                                                01560824
           IF (AVD + 0.1047197552D+01) 20040, 10040, 40040              01570824
40040      IF (AVD + 0.1047197550D+01) 10040, 10040, 20040              01580824
10040      IVPASS = IVPASS + 1                                          01590824
           WRITE (NUVI, 80002) IVTNUM                                   01600824
           GO TO 0041                                                   01610824
20040      IVFAIL = IVFAIL + 1                                          01620824
           DVCORR = -1.0471975511965977461D+00                          01630824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01640824
 0041      CONTINUE                                                     01650824
CT005*  TEST 5                             AN ARGUMENT OF LOW MAGNITUDE 01660824
           IVTNUM = 5                                                   01670824
        AVD = DATAN(1.0D-16)                                            01680824
           IF (AVD - 0.9999999995D-16) 20050, 10050, 40050              01690824
40050      IF (AVD - 0.1000000001D-15) 10050, 10050, 20050              01700824
10050      IVPASS = IVPASS + 1                                          01710824
           WRITE (NUVI, 80002) IVTNUM                                   01720824
           GO TO 0051                                                   01730824
20050      IVFAIL = IVFAIL + 1                                          01740824
           DVCORR = 1.0000000000000000000D-16                           01750824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01760824
 0051      CONTINUE                                                     01770824
CT006*  TEST 6                            AN ARGUMENT OF HIGH MAGNITUDE 01780824
           IVTNUM = 6                                                   01790824
        AVD = DATAN(-2.0D+34)                                           01800824
           IF (AVD + 0.1570796328D+01) 20060, 10060, 40060              01810824
40060      IF (AVD + 0.1570796326D+01) 10060, 10060, 20060              01820824
10060      IVPASS = IVPASS + 1                                          01830824
           WRITE (NUVI, 80002) IVTNUM                                   01840824
           GO TO 0061                                                   01850824
20060      IVFAIL = IVFAIL + 1                                          01860824
           DVCORR = -1.5707963267948966192D+00                          01870824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01880824
 0061      CONTINUE                                                     01890824
C*****                                                                  01900824
        WRITE(NUVI,19608)                                               01910824
19608   FORMAT(/ 08X, 14HTEST OF DATAN2)                                01920824
CT007*  TEST 7                  TEST (0,POSITIVE) TO TEST DISCONTINUITY 01930824
           IVTNUM = 7                                                   01940824
        BVD = 10.0D0 / 10.0D0                                           01950824
        CVD = 0.0D0                                                     01960824
        AVD = DATAN2(CVD, BVD)                                          01970824
           IF (AVD + 0.5000000000D-09) 20070, 10070, 40070              01980824
40070      IF (AVD - 0.5000000000D-09) 10070, 10070, 20070              01990824
10070      IVPASS = IVPASS + 1                                          02000824
           WRITE (NUVI, 80002) IVTNUM                                   02010824
           GO TO 0071                                                   02020824
20070      IVFAIL = IVFAIL + 1                                          02030824
           DVCORR = 0.00000000000000000000D+00                          02040824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02050824
 0071      CONTINUE                                                     02060824
CT008*  TEST 8                  TEST (0,NEGATIVE) TO TEST DISCONTINUITY 02070824
           IVTNUM = 8                                                   02080824
        BVD = 0.0D0                                                     02090824
        CVD = -25.0D0 / 2.0D0                                           02100824
        AVD = DATAN2(BVD, CVD)                                          02110824
           IF (AVD - 0.3141592652D+01) 20080, 10080, 40080              02120824
40080      IF (AVD - 0.3141592655D+01) 10080, 10080, 20080              02130824
10080      IVPASS = IVPASS + 1                                          02140824
           WRITE (NUVI, 80002) IVTNUM                                   02150824
           GO TO 0081                                                   02160824
20080      IVFAIL = IVFAIL + 1                                          02170824
           DVCORR = 3.1415926535897932384D+00                           02180824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02190824
 0081      CONTINUE                                                     02200824
CT009*  TEST 9                        AN EXPRESSION PRESENTED TO DATAN2 02210824
           IVTNUM = 9                                                   02220824
        BVD = 1.0D0                                                     02230824
        CVD = BVD + BVD                                                 02240824
        AVD = DATAN2(BVD * 2.0D0, CVD)                                  02250824
           IF (AVD - 0.7853981630D+00) 20090, 10090, 40090              02260824
40090      IF (AVD - 0.7853981638D+00) 10090, 10090, 20090              02270824
10090      IVPASS = IVPASS + 1                                          02280824
           WRITE (NUVI, 80002) IVTNUM                                   02290824
           GO TO 0091                                                   02300824
20090      IVFAIL = IVFAIL + 1                                          02310824
           DVCORR = 0.78539816339744830962D+00                          02320824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02330824
 0091      CONTINUE                                                     02340824
CT010*  TEST 10                    ARGUMENTS WHERE (X,Y) X IS NEAR ZERO 02350824
           IVTNUM = 10                                                  02360824
        BVD = DASIN(0.6D0)                                              02370824
        CVD = DACOS(0.8D0)                                              02380824
        AVD = DATAN2(BVD, CVD)                                          02390824
           IF (AVD - 0.7853981630D+00) 20100, 10100, 40100              02400824
40100      IF (AVD - 0.7853981638D+00) 10100, 10100, 20100              02410824
10100      IVPASS = IVPASS + 1                                          02420824
           WRITE (NUVI, 80002) IVTNUM                                   02430824
           GO TO 0101                                                   02440824
20100      IVFAIL = IVFAIL + 1                                          02450824
           DVCORR = 0.78539816339744830962D+00                          02460824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02470824
 0101      CONTINUE                                                     02480824
CT011*  TEST 11                     WHERE ARGUMENT (X,Y) Y IS NEAR ZERO 02490824
           IVTNUM = 11                                                  02500824
        AVD = DATAN2(1.2D0, 0.0D0)                                      02510824
           IF (AVD - 0.1570796326D+01) 20110, 10110, 40110              02520824
40110      IF (AVD - 0.1570796328D+01) 10110, 10110, 20110              02530824
10110      IVPASS = IVPASS + 1                                          02540824
           WRITE (NUVI, 80002) IVTNUM                                   02550824
           GO TO 0111                                                   02560824
20110      IVFAIL = IVFAIL + 1                                          02570824
           DVCORR = 1.5707963267948966192D+00                           02580824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02590824
 0111      CONTINUE                                                     02600824
CT012*  TEST 12                     WHERE ARGUMENT (X,Y) Y IS NEAR ZERO 02610824
           IVTNUM = 12                                                  02620824
        BVD = -2.5D0                                                    02630824
        CVD = 0.0D0                                                     02640824
        AVD = DATAN2(BVD, CVD)                                          02650824
           IF (AVD + 0.1570796328D+01) 20120, 10120, 40120              02660824
40120      IF (AVD + 0.1570796326D+01) 10120, 10120, 20120              02670824
10120      IVPASS = IVPASS + 1                                          02680824
           WRITE (NUVI, 80002) IVTNUM                                   02690824
           GO TO 0121                                                   02700824
20120      IVFAIL = IVFAIL + 1                                          02710824
           DVCORR = -1.5707963267948966192D+00                          02720824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02730824
 0121      CONTINUE                                                     02740824
CT013*  TEST 13                          COMPARISON OF DATAN AND DATAN2 02750824
           IVTNUM = 13                                                  02760824
        AVD = (DATAN(DSQRT(3.0D0) / 3.0D0) * 2.0D0) +                   02770824
     1    DATAN2(-DSQRT(3.0D0) / 2.0D0, 1.0D0 / 2.0D0)                  02780824
           IF (AVD + 0.5000000000D-09) 20130, 10130, 40130              02790824
40130      IF (AVD - 0.5000000000D-09) 10130, 10130, 20130              02800824
10130      IVPASS = IVPASS + 1                                          02810824
           WRITE (NUVI, 80002) IVTNUM                                   02820824
           GO TO 0131                                                   02830824
20130      IVFAIL = IVFAIL + 1                                          02840824
           DVCORR = 0.00000000000000000000D+00                          02850824
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02860824
 0131      CONTINUE                                                     02870824
C*****                                                                  02880824
CBB** ********************** BBCSUM0  **********************************02890824
C**** WRITE OUT TEST SUMMARY                                            02900824
C****                                                                   02910824
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02920824
      WRITE (I02, 90004)                                                02930824
      WRITE (I02, 90014)                                                02940824
      WRITE (I02, 90004)                                                02950824
      WRITE (I02, 90020) IVPASS                                         02960824
      WRITE (I02, 90022) IVFAIL                                         02970824
      WRITE (I02, 90024) IVDELE                                         02980824
      WRITE (I02, 90026) IVINSP                                         02990824
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03000824
CBE** ********************** BBCSUM0  **********************************03010824
CBB** ********************** BBCFOOT0 **********************************03020824
C**** WRITE OUT REPORT FOOTINGS                                         03030824
C****                                                                   03040824
      WRITE (I02,90016) ZPROG, ZPROG                                    03050824
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03060824
      WRITE (I02,90019)                                                 03070824
CBE** ********************** BBCFOOT0 **********************************03080824
CBB** ********************** BBCFMT0A **********************************03090824
C**** FORMATS FOR TEST DETAIL LINES                                     03100824
C****                                                                   03110824
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03120824
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03130824
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03140824
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03150824
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03160824
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03170824
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03180824
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03190824
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03200824
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03210824
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03220824
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03230824
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03240824
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03250824
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03260824
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03270824
80050 FORMAT (1H ,48X,A31)                                              03280824
CBE** ********************** BBCFMT0A **********************************03290824
CBB** ********************** BBCFMAT1 **********************************03300824
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03310824
C****                                                                   03320824
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03330824
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03340824
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03350824
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03360824
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03370824
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03380824
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03390824
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03400824
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03410824
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03420824
     21H(,F12.5,2H, ,F12.5,1H))                                         03430824
CBE** ********************** BBCFMAT1 **********************************03440824
CBB** ********************** BBCFMT0B **********************************03450824
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03460824
C****                                                                   03470824
90002 FORMAT (1H1)                                                      03480824
90004 FORMAT (1H )                                                      03490824
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03500824
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03510824
90008 FORMAT (1H ,21X,A13,A17)                                          03520824
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03530824
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03540824
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03550824
     1       7X,7HREMARKS,24X)                                          03560824
90014 FORMAT (1H ,46H----------------------------------------------,    03570824
     1        33H---------------------------------)                     03580824
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03590824
C****                                                                   03600824
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03610824
C****                                                                   03620824
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03630824
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03640824
     1        A13)                                                      03650824
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03660824
C****                                                                   03670824
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03680824
C****                                                                   03690824
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03700824
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03710824
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03720824
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03730824
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03740824
CBE** ********************** BBCFMT0B **********************************03750824
C*****                                                                  03760824
C*****    END OF TEST SEGMENT 196                                       03770824
      STOP                                                              03780824
      END                                                               03790824
                                                                        03800824
