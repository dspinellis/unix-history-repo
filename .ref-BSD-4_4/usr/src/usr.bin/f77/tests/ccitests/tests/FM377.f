C***********************************************************************00010377
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020377
C*****   FM377                                                          00030377
C*****                       XSINH - (197)                              00040377
C*****                                                                  00050377
C***********************************************************************00060377
C*****  GENERAL PURPOSE                                      SUBSET REF 00070377
C*****    TEST INTRINSIC FUNCTION SINH, COSH                   15.3     00080377
C*****                                                        TABLE 5   00090377
C*****                                                                  00100377
CBB** ********************** BBCCOMNT **********************************00110377
C****                                                                   00120377
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130377
C****                          VERSION 2.0                              00140377
C****                                                                   00150377
C****                                                                   00160377
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170377
C****                   GENERAL SERVICES ADMINISTRATION                 00180377
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190377
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200377
C****                      FALLS CHURCH, VA. 22041                      00210377
C****                                                                   00220377
C****                          (703) 756-6153                           00230377
C****                                                                   00240377
CBE** ********************** BBCCOMNT **********************************00250377
CBB** ********************** BBCINITA **********************************00260377
C**** SPECIFICATION STATEMENTS                                          00270377
C****                                                                   00280377
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290377
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300377
CBE** ********************** BBCINITA **********************************00310377
CBB** ********************** BBCINITB **********************************00320377
C**** INITIALIZE SECTION                                                00330377
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340377
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350377
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360377
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370377
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380377
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390377
      DATA   REMRKS /'                               '/                 00400377
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410377
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420377
C****                                                                   00430377
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440377
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450377
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460377
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530377
      IVPASS = 0                                                        00540377
      IVFAIL = 0                                                        00550377
      IVDELE = 0                                                        00560377
      IVINSP = 0                                                        00570377
      IVTOTL = 0                                                        00580377
      IVTOTN = 0                                                        00590377
      ICZERO = 0                                                        00600377
C                                                                       00610377
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620377
      I01 = 05                                                          00630377
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640377
      I02 = 06                                                          00650377
C                                                                       00660377
      I01 = 5                                                           00670377
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680377
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690377
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700377
C                                                                       00710377
      I02 = 6                                                           00720377
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730377
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740377
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750377
C                                                                       00760377
CBE** ********************** BBCINITB **********************************00770377
      NUVI = I02                                                        00780377
      IVTOTL = 15                                                       00790377
      ZPROG = 'FM377'                                                   00800377
CBB** ********************** BBCHED0A **********************************00810377
C****                                                                   00820377
C**** WRITE REPORT TITLE                                                00830377
C****                                                                   00840377
      WRITE (I02, 90002)                                                00850377
      WRITE (I02, 90006)                                                00860377
      WRITE (I02, 90007)                                                00870377
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880377
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890377
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900377
CBE** ********************** BBCHED0A **********************************00910377
C*****                                                                  00920377
C*****    HEADER FOR SEGMENT 197                                        00930377
        WRITE(NUVI,19700)                                               00940377
19700   FORMAT(1H , / 35H  XSINH - (197) INTRINSIC FUNCTIONS//          00950377
     1         41H  SINH, COSH    (HYPERBOLIC SINE, COSINE)//           00960377
     2         20H  SUBSET REF. - 15.3)                                 00970377
CBB** ********************** BBCHED0B **********************************00980377
C**** WRITE DETAIL REPORT HEADERS                                       00990377
C****                                                                   01000377
      WRITE (I02,90004)                                                 01010377
      WRITE (I02,90004)                                                 01020377
      WRITE (I02,90013)                                                 01030377
      WRITE (I02,90014)                                                 01040377
      WRITE (I02,90015) IVTOTL                                          01050377
CBE** ********************** BBCHED0B **********************************01060377
C*****                                                                  01070377
        WRITE(NUVI,19701)                                               01080377
19701   FORMAT(/ 8X, 12HTEST OF SINH)                                   01090377
C*****                                                                  01100377
CT001*  TEST 1                                       TEST AT ZERO (0.0) 01110377
           IVTNUM = 1                                                   01120377
        BVS = 0.0                                                       01130377
        AVS = SINH(BVS)                                                 01140377
           IF (AVS +  0.50000E-04) 20010, 10010, 40010                  01150377
40010      IF (AVS -  0.50000E-04) 10010, 10010, 20010                  01160377
10010      IVPASS = IVPASS + 1                                          01170377
           WRITE (NUVI, 80002) IVTNUM                                   01180377
           GO TO 0011                                                   01190377
20010      IVFAIL = IVFAIL + 1                                          01200377
           RVCORR =  0.00000000000000                                   01210377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01220377
 0011      CONTINUE                                                     01230377
CT002*  TEST 2                              TEST ARGUMENTS CLOSE TO 1.0 01240377
           IVTNUM = 2                                                   01250377
        AVS = SINH(15.0 / 16.0)                                         01260377
           IF (AVS -  0.10809E+01) 20020, 10020, 40020                  01270377
40020      IF (AVS -  0.10811E+01) 10020, 10020, 20020                  01280377
10020      IVPASS = IVPASS + 1                                          01290377
           WRITE (NUVI, 80002) IVTNUM                                   01300377
           GO TO 0021                                                   01310377
20020      IVFAIL = IVFAIL + 1                                          01320377
           RVCORR =  1.08099191569306                                   01330377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01340377
 0021      CONTINUE                                                     01350377
CT003*  TEST 3                                              TEST AT 1.0 01360377
           IVTNUM = 3                                                   01370377
        BVS = 1.0                                                       01380377
        AVS = SINH(BVS)                                                 01390377
           IF (AVS -  0.11751E+01) 20030, 10030, 40030                  01400377
40030      IF (AVS -  0.11753E+01) 10030, 10030, 20030                  01410377
10030      IVPASS = IVPASS + 1                                          01420377
           WRITE (NUVI, 80002) IVTNUM                                   01430377
           GO TO 0031                                                   01440377
20030      IVFAIL = IVFAIL + 1                                          01450377
           RVCORR =  1.17520119364380                                   01460377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01470377
 0031      CONTINUE                                                     01480377
CT004*  TEST 4                              TEST ARGUMENTS CLOSE TO 1.0 01490377
           IVTNUM = 4                                                   01500377
        AVS = SINH(33.0 / 32.0)                                         01510377
           IF (AVS -  0.12239E+01) 20040, 10040, 40040                  01520377
40040      IF (AVS -  0.12241E+01) 10040, 10040, 20040                  01530377
10040      IVPASS = IVPASS + 1                                          01540377
           WRITE (NUVI, 80002) IVTNUM                                   01550377
           GO TO 0041                                                   01560377
20040      IVFAIL = IVFAIL + 1                                          01570377
           RVCORR =  1.22400418778664                                   01580377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01590377
 0041      CONTINUE                                                     01600377
CT005*  TEST 5                                              TEST AT 2.0 01610377
           IVTNUM = 5                                                   01620377
        BVS = 2.0                                                       01630377
        AVS = SINH(BVS)                                                 01640377
           IF (AVS -  0.36266E+01) 20050, 10050, 40050                  01650377
40050      IF (AVS -  0.36271E+01) 10050, 10050, 20050                  01660377
10050      IVPASS = IVPASS + 1                                          01670377
           WRITE (NUVI, 80002) IVTNUM                                   01680377
           GO TO 0051                                                   01690377
20050      IVFAIL = IVFAIL + 1                                          01700377
           RVCORR =  3.62686040784702                                   01710377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01720377
 0051      CONTINUE                                                     01730377
CT006*  TEST 6                                      A NEGATIVE ARGUMENT 01740377
           IVTNUM = 6                                                   01750377
        AVS = SINH(-2.0)                                                01760377
           IF (AVS +  0.36271E+01) 20060, 10060, 40060                  01770377
40060      IF (AVS +  0.36266E+01) 10060, 10060, 20060                  01780377
10060      IVPASS = IVPASS + 1                                          01790377
           WRITE (NUVI, 80002) IVTNUM                                   01800377
           GO TO 0061                                                   01810377
20060      IVFAIL = IVFAIL + 1                                          01820377
           RVCORR = -3.62686040784702                                   01830377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01840377
 0061      CONTINUE                                                     01850377
CT007*  TEST 7                            AN ARGUMENT OF LOW MAGNITUDE  01860377
           IVTNUM = 7                                                   01870377
        AVS = SINH(1.0E-34)                                             01880377
           IF (AVS -  0.99995E-34) 20070, 10070, 40070                  01890377
40070      IF (AVS -  0.10001E-33) 10070, 10070, 20070                  01900377
10070      IVPASS = IVPASS + 1                                          01910377
           WRITE (NUVI, 80002) IVTNUM                                   01920377
           GO TO 0071                                                   01930377
20070      IVFAIL = IVFAIL + 1                                          01940377
           RVCORR =  1.00000000000000E-34                               01950377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01960377
 0071      CONTINUE                                                     01970377
C*****                                                                  01980377
        WRITE (NUVI, 90002)                                             01990377
        WRITE (NUVI, 90013)                                             02000377
        WRITE (NUVI, 90014)                                             02010377
C*****                                                                  02020377
        WRITE(NUVI,19709)                                               02030377
19709   FORMAT(/ 8X, 12HTEST OF COSH)                                   02040377
C*****                                                                  02050377
CT008*  TEST 8                                               ZERO (0.0) 02060377
           IVTNUM = 8                                                   02070377
        BVS = 0.0                                                       02080377
        AVS = COSH(BVS)                                                 02090377
           IF (AVS -  0.99995E+00) 20080, 10080, 40080                  02100377
40080      IF (AVS -  0.10001E+01) 10080, 10080, 20080                  02110377
10080      IVPASS = IVPASS + 1                                          02120377
           WRITE (NUVI, 80002) IVTNUM                                   02130377
           GO TO 0081                                                   02140377
20080      IVFAIL = IVFAIL + 1                                          02150377
           RVCORR =  1.00000000000000                                   02160377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02170377
 0081      CONTINUE                                                     02180377
CT009*  TEST 9                                      VALUES CLOSE TO 1.0 02190377
           IVTNUM = 9                                                   02200377
        AVS = COSH(15.0 / 16.0)                                         02210377
           IF (AVS -  0.14725E+01) 20090, 10090, 40090                  02220377
40090      IF (AVS -  0.14727E+01) 10090, 10090, 20090                  02230377
10090      IVPASS = IVPASS + 1                                          02240377
           WRITE (NUVI, 80002) IVTNUM                                   02250377
           GO TO 0091                                                   02260377
20090      IVFAIL = IVFAIL + 1                                          02270377
           RVCORR =  1.47259754236986                                   02280377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02290377
 0091      CONTINUE                                                     02300377
CT010*  TEST 10                                             TEST AT 1.0 02310377
           IVTNUM = 10                                                  02320377
        BVS = 1.0                                                       02330377
        AVS = COSH(BVS)                                                 02340377
           IF (AVS -  0.15430E+01) 20100, 10100, 40100                  02350377
40100      IF (AVS -  0.15432E+01) 10100, 10100, 20100                  02360377
10100      IVPASS = IVPASS + 1                                          02370377
           WRITE (NUVI, 80002) IVTNUM                                   02380377
           GO TO 0101                                                   02390377
20100      IVFAIL = IVFAIL + 1                                          02400377
           RVCORR =  1.54308063481524                                   02410377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02420377
 0101      CONTINUE                                                     02430377
CT011*  TEST 11                             TEST ARGUMENTS CLOSE TO 1.0 02440377
           IVTNUM = 11                                                  02450377
        AVS = COSH(33.0 / 32.0)                                         02460377
           IF (AVS -  0.15804E+01) 20110, 10110, 40110                  02470377
40110      IF (AVS -  0.15807E+01) 10110, 10110, 20110                  02480377
10110      IVPASS = IVPASS + 1                                          02490377
           WRITE (NUVI, 80002) IVTNUM                                   02500377
           GO TO 0111                                                   02510377
20110      IVFAIL = IVFAIL + 1                                          02520377
           RVCORR =  1.58056516845059                                   02530377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02540377
 0111      CONTINUE                                                     02550377
CT012*  TEST 12                                             TEST AT 2.0 02560377
           IVTNUM = 12                                                  02570377
        BVS = 2.0                                                       02580377
        AVS = COSH(BVS)                                                 02590377
           IF (AVS -  0.37620E+01) 20120, 10120, 40120                  02600377
40120      IF (AVS -  0.37624E+01) 10120, 10120, 20120                  02610377
10120      IVPASS = IVPASS + 1                                          02620377
           WRITE (NUVI, 80002) IVTNUM                                   02630377
           GO TO 0121                                                   02640377
20120      IVFAIL = IVFAIL + 1                                          02650377
           RVCORR =  3.76219569108363                                   02660377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02670377
 0121      CONTINUE                                                     02680377
CT013*  TEST 13                                     A NEGATIVE ARGUMENT 02690377
           IVTNUM = 13                                                  02700377
        AVS = COSH(-2.0)                                                02710377
           IF (AVS -  0.37620E+01) 20130, 10130, 40130                  02720377
40130      IF (AVS -  0.37624E+01) 10130, 10130, 20130                  02730377
10130      IVPASS = IVPASS + 1                                          02740377
           WRITE (NUVI, 80002) IVTNUM                                   02750377
           GO TO 0131                                                   02760377
20130      IVFAIL = IVFAIL + 1                                          02770377
           RVCORR =  3.76219569108363                                   02780377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02790377
 0131      CONTINUE                                                     02800377
CT014*  TEST 14                            AN ARGUMENT OF LOW MAGNITUDE 02810377
           IVTNUM = 14                                                  02820377
        AVS = COSH(-1.0E-34)                                            02830377
           IF (AVS -  0.99995E+00) 20140, 10140, 40140                  02840377
40140      IF (AVS -  0.10001E+01) 10140, 10140, 20140                  02850377
10140      IVPASS = IVPASS + 1                                          02860377
           WRITE (NUVI, 80002) IVTNUM                                   02870377
           GO TO 0141                                                   02880377
20140      IVFAIL = IVFAIL + 1                                          02890377
           RVCORR =  1.00000000000000E+00                               02900377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02910377
 0141      CONTINUE                                                     02920377
CT015*  TEST 15                   POSITIVE VALUES SUPPLIED AS ARGUMENTS 02930377
C*****                               TO BOTH FUNCTIONS IN AN EXPRESSION 02940377
           IVTNUM = 15                                                  02950377
        AVS = SINH(3.25) + COSH(3.25)                                   02960377
           IF (AVS -  0.25789E+02) 20150, 10150, 40150                  02970377
40150      IF (AVS -  0.25792E+02) 10150, 10150, 20150                  02980377
10150      IVPASS = IVPASS + 1                                          02990377
           WRITE (NUVI, 80002) IVTNUM                                   03000377
           GO TO 0151                                                   03010377
20150      IVFAIL = IVFAIL + 1                                          03020377
           RVCORR =  25.79033991719306                                  03030377
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03040377
 0151      CONTINUE                                                     03050377
C*****                                                                  03060377
CBB** ********************** BBCSUM0  **********************************03070377
C**** WRITE OUT TEST SUMMARY                                            03080377
C****                                                                   03090377
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03100377
      WRITE (I02, 90004)                                                03110377
      WRITE (I02, 90014)                                                03120377
      WRITE (I02, 90004)                                                03130377
      WRITE (I02, 90020) IVPASS                                         03140377
      WRITE (I02, 90022) IVFAIL                                         03150377
      WRITE (I02, 90024) IVDELE                                         03160377
      WRITE (I02, 90026) IVINSP                                         03170377
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03180377
CBE** ********************** BBCSUM0  **********************************03190377
CBB** ********************** BBCFOOT0 **********************************03200377
C**** WRITE OUT REPORT FOOTINGS                                         03210377
C****                                                                   03220377
      WRITE (I02,90016) ZPROG, ZPROG                                    03230377
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03240377
      WRITE (I02,90019)                                                 03250377
CBE** ********************** BBCFOOT0 **********************************03260377
CBB** ********************** BBCFMT0A **********************************03270377
C**** FORMATS FOR TEST DETAIL LINES                                     03280377
C****                                                                   03290377
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03300377
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03310377
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03320377
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03330377
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03340377
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03350377
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03360377
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03370377
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03380377
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03390377
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03400377
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03410377
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03420377
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03430377
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03440377
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03450377
80050 FORMAT (1H ,48X,A31)                                              03460377
CBE** ********************** BBCFMT0A **********************************03470377
CBB** ********************** BBCFMT0B **********************************03480377
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03490377
C****                                                                   03500377
90002 FORMAT (1H1)                                                      03510377
90004 FORMAT (1H )                                                      03520377
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03530377
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03540377
90008 FORMAT (1H ,21X,A13,A17)                                          03550377
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03560377
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03570377
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03580377
     1       7X,7HREMARKS,24X)                                          03590377
90014 FORMAT (1H ,46H----------------------------------------------,    03600377
     1        33H---------------------------------)                     03610377
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03620377
C****                                                                   03630377
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03640377
C****                                                                   03650377
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03660377
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03670377
     1        A13)                                                      03680377
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03690377
C****                                                                   03700377
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03710377
C****                                                                   03720377
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03730377
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03740377
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03750377
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03760377
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03770377
CBE** ********************** BBCFMT0B **********************************03780377
C*****                                                                  03790377
C*****    END OF TEST SEGMENT 197                                       03800377
      STOP                                                              03810377
      END                                                               03820377
