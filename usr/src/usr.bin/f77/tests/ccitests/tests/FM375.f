C***********************************************************************00010375
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020375
C*****   FM375                                                          00030375
C*****                       XASIN - (193)                              00040375
C*****                                                                  00050375
C***********************************************************************00060375
C*****  GENERAL PURPOSE                                      SUBSET REF 00070375
C*****    TEST INTRINSIC FUNCTION ASIN, ACOS                    15.3    00080375
C*****                                                         TABLE 5  00090375
C*****                                                                  00100375
CBB** ********************** BBCCOMNT **********************************00110375
C****                                                                   00120375
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130375
C****                          VERSION 2.0                              00140375
C****                                                                   00150375
C****                                                                   00160375
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170375
C****                   GENERAL SERVICES ADMINISTRATION                 00180375
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190375
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200375
C****                      FALLS CHURCH, VA. 22041                      00210375
C****                                                                   00220375
C****                          (703) 756-6153                           00230375
C****                                                                   00240375
CBE** ********************** BBCCOMNT **********************************00250375
CBB** ********************** BBCINITA **********************************00260375
C**** SPECIFICATION STATEMENTS                                          00270375
C****                                                                   00280375
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290375
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300375
CBE** ********************** BBCINITA **********************************00310375
CBB** ********************** BBCINITB **********************************00320375
C**** INITIALIZE SECTION                                                00330375
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340375
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350375
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360375
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370375
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380375
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390375
      DATA   REMRKS /'                               '/                 00400375
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410375
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420375
C****                                                                   00430375
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440375
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450375
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460375
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530375
      IVPASS = 0                                                        00540375
      IVFAIL = 0                                                        00550375
      IVDELE = 0                                                        00560375
      IVINSP = 0                                                        00570375
      IVTOTL = 0                                                        00580375
      IVTOTN = 0                                                        00590375
      ICZERO = 0                                                        00600375
C                                                                       00610375
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620375
      I01 = 05                                                          00630375
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640375
      I02 = 06                                                          00650375
C                                                                       00660375
      I01 = 5                                                           00670375
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680375
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690375
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700375
C                                                                       00710375
      I02 = 6                                                           00720375
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730375
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740375
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750375
C                                                                       00760375
CBE** ********************** BBCINITB **********************************00770375
      NUVI = I02                                                        00780375
      IVTOTL = 12                                                       00790375
      ZPROG = 'FM375'                                                   00800375
CBB** ********************** BBCHED0A **********************************00810375
C****                                                                   00820375
C**** WRITE REPORT TITLE                                                00830375
C****                                                                   00840375
      WRITE (I02, 90002)                                                00850375
      WRITE (I02, 90006)                                                00860375
      WRITE (I02, 90007)                                                00870375
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880375
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890375
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900375
CBE** ********************** BBCHED0A **********************************00910375
C*****                                                                  00920375
C*****    HEADER FOR SEGMENT 193                                        00930375
        WRITE(NUVI,19300)                                               00940375
19300   FORMAT(1H , / 35H  XASIN - (193) INTRINSIC FUNCTIONS//          00950375
     1         34H  ASIN, ACOS  (ARCSIN, ARCCOSINE) //                  00960375
     2         20H  SUBSET REF. - 15.3)                                 00970375
CBB** ********************** BBCHED0B **********************************00980375
C**** WRITE DETAIL REPORT HEADERS                                       00990375
C****                                                                   01000375
      WRITE (I02,90004)                                                 01010375
      WRITE (I02,90004)                                                 01020375
      WRITE (I02,90013)                                                 01030375
      WRITE (I02,90014)                                                 01040375
      WRITE (I02,90015) IVTOTL                                          01050375
CBE** ********************** BBCHED0B **********************************01060375
C*****                                                                  01070375
        WRITE(NUVI,19301)                                               01080375
19301   FORMAT(1H0,8X,12HTEST OF ASIN)                                  01090375
C*****                                                                  01100375
CT001*  TEST 1                 -1 TO CHECK PRINCIPAL VALUE AT ENDPOINTS 01110375
           IVTNUM = 1                                                   01120375
        BVS = -1.0                                                      01130375
        AVS = ASIN(BVS)                                                 01140375
           IF (AVS +  0.15709E+01) 20010, 10010, 40010                  01150375
40010      IF (AVS +  0.15707E+01) 10010, 10010, 20010                  01160375
10010      IVPASS = IVPASS + 1                                          01170375
           WRITE (NUVI, 80002) IVTNUM                                   01180375
           GO TO 0011                                                   01190375
20010      IVFAIL = IVFAIL + 1                                          01200375
           RVCORR = -1.57079632679490                                   01210375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01220375
 0011      CONTINUE                                                     01230375
CT002*  TEST 2                 +1 TO CHECK PRINCIPAL VALUE AT ENDPOINTS 01240375
           IVTNUM = 2                                                   01250375
        AVS = ASIN(1.0)                                                 01260375
           IF (AVS -  0.15707E+01) 20020, 10020, 40020                  01270375
40020      IF (AVS -  0.15709E+01) 10020, 10020, 20020                  01280375
10020      IVPASS = IVPASS + 1                                          01290375
           WRITE (NUVI, 80002) IVTNUM                                   01300375
           GO TO 0021                                                   01310375
20020      IVFAIL = IVFAIL + 1                                          01320375
           RVCORR = 1.57079632679490                                    01330375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01340375
 0021      CONTINUE                                                     01350375
CT003*  TEST 3                                     THE VALUE -SQRT(0.5) 01360375
           IVTNUM = 3                                                   01370375
        BVS = -SQRT(2.0) / 2.0                                          01380375
        AVS = ASIN(BVS)                                                 01390375
           IF (AVS +  0.78544E+00) 20030, 10030, 40030                  01400375
40030      IF (AVS +  0.78535E+00) 10030, 10030, 20030                  01410375
10030      IVPASS = IVPASS + 1                                          01420375
           WRITE (NUVI, 80002) IVTNUM                                   01430375
           GO TO 0031                                                   01440375
20030      IVFAIL = IVFAIL + 1                                          01450375
           RVCORR = -0.78539816339745                                   01460375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01470375
 0031      CONTINUE                                                     01480375
CT004*  TEST 4                                            THE VALUE 0.5 01490375
           IVTNUM = 4                                                   01500375
        AVS = ASIN(1.0 / 2.0)                                           01510375
           IF (AVS -  0.52357E+00) 20040, 10040, 40040                  01520375
40040      IF (AVS -  0.52363E+00) 10040, 10040, 20040                  01530375
10040      IVPASS = IVPASS + 1                                          01540375
           WRITE (NUVI, 80002) IVTNUM                                   01550375
           GO TO 0041                                                   01560375
20040      IVFAIL = IVFAIL + 1                                          01570375
           RVCORR = 0.52359877559830                                    01580375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01590375
 0041      CONTINUE                                                     01600375
CT005*  TEST 5                             AN ARGUMENT OF LOW MAGNITUDE 01610375
           IVTNUM = 5                                                   01620375
        AVS = ASIN(-1.0E-33)                                            01630375
           IF (AVS +  0.10001E-32) 20050, 10050, 40050                  01640375
40050      IF (AVS +  0.99995E-33) 10050, 10050, 20050                  01650375
10050      IVPASS = IVPASS + 1                                          01660375
           WRITE (NUVI, 80002) IVTNUM                                   01670375
           GO TO 0051                                                   01680375
20050      IVFAIL = IVFAIL + 1                                          01690375
           RVCORR = -1.00000000000000E-33                               01700375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01710375
 0051      CONTINUE                                                     01720375
C*****                                                                  01730375
        WRITE(NUVI,19307)                                               01740375
19307   FORMAT(1H0,8X,12HTEST OF ACOS)                                  01750375
C*****                                                                  01760375
CT006*  TEST 6                  -1 TO TEST PRINCIPAL VALUE AT ENDPOINTS 01770375
           IVTNUM = 6                                                   01780375
        BVS = -1.0                                                      01790375
        AVS = ACOS(BVS)                                                 01800375
           IF (AVS -  0.31414E+01) 20060, 10060, 40060                  01810375
40060      IF (AVS -  0.31418E+01) 10060, 10060, 20060                  01820375
10060      IVPASS = IVPASS + 1                                          01830375
           WRITE (NUVI, 80002) IVTNUM                                   01840375
           GO TO 0061                                                   01850375
20060      IVFAIL = IVFAIL + 1                                          01860375
           RVCORR = 3.14159265358980                                    01870375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01880375
 0061      CONTINUE                                                     01890375
CT007*  TEST 7                                                          01900375
           IVTNUM = 7                                                   01910375
        AVS = ACOS(1.0)                                                 01920375
           IF (AVS +  0.50000E-04) 20070, 10070, 40070                  01930375
40070      IF (AVS -  0.50000E-04) 10070, 10070, 20070                  01940375
10070      IVPASS = IVPASS + 1                                          01950375
           WRITE (NUVI, 80002) IVTNUM                                   01960375
           GO TO 0071                                                   01970375
20070      IVFAIL = IVFAIL + 1                                          01980375
           RVCORR = 0.00000000000000                                    01990375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02000375
 0071      CONTINUE                                                     02010375
CT008*  TEST 8                                                          02020375
           IVTNUM = 8                                                   02030375
        BVS = -SQRT(2.0) / 2.0                                          02040375
        AVS = ACOS(BVS)                                                 02050375
           IF (AVS -  0.23560E+01) 20080, 10080, 40080                  02060375
40080      IF (AVS -  0.23564E+01) 10080, 10080, 20080                  02070375
10080      IVPASS = IVPASS + 1                                          02080375
           WRITE (NUVI, 80002) IVTNUM                                   02090375
           GO TO 0081                                                   02100375
20080      IVFAIL = IVFAIL + 1                                          02110375
           RVCORR = 2.35619449019234                                    02120375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02130375
 0081      CONTINUE                                                     02140375
CT009*  TEST 9                                                          02150375
           IVTNUM = 9                                                   02160375
        AVS = ACOS(1.0 / 2.0)                                           02170375
           IF (AVS -  0.10471E+01) 20090, 10090, 40090                  02180375
40090      IF (AVS -  0.10473E+01) 10090, 10090, 20090                  02190375
10090      IVPASS = IVPASS + 1                                          02200375
           WRITE (NUVI, 80002) IVTNUM                                   02210375
           GO TO 0091                                                   02220375
20090      IVFAIL = IVFAIL + 1                                          02230375
           RVCORR = 1.04719755119660                                    02240375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02250375
 0091      CONTINUE                                                     02260375
CT010*  TEST 10                            AN ARGUMENT OF LOW MAGNITUDE 02270375
           IVTNUM = 10                                                  02280375
        AVS = ACOS(-1.0E-33)                                            02290375
           IF (AVS -  0.15707E+01) 20100, 10100, 40100                  02300375
40100      IF (AVS -  0.15709E+01) 10100, 10100, 20100                  02310375
10100      IVPASS = IVPASS + 1                                          02320375
           WRITE (NUVI, 80002) IVTNUM                                   02330375
           GO TO 0101                                                   02340375
20100      IVFAIL = IVFAIL + 1                                          02350375
           RVCORR = 1.57079632679490                                    02360375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02370375
 0101      CONTINUE                                                     02380375
CT011*  TEST 11        COMPARISON OF ASIN AND ACOS TO TEST RELATIONSHIP 02390375
           IVTNUM = 11                                                  02400375
        BVS = ASIN(SQRT(3.0) / 3.0)                                     02410375
        CVS = ACOS(SQRT(3.0) / 3.0)                                     02420375
        AVS = (BVS + CVS) * 2.0                                         02430375
           IF (AVS -  0.31414E+01) 20110, 10110, 40110                  02440375
40110      IF (AVS -  0.31418E+01) 10110, 10110, 20110                  02450375
10110      IVPASS = IVPASS + 1                                          02460375
           WRITE (NUVI, 80002) IVTNUM                                   02470375
           GO TO 0111                                                   02480375
20110      IVFAIL = IVFAIL + 1                                          02490375
           RVCORR = 3.14159265358979                                    02500375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02510375
 0111      CONTINUE                                                     02520375
CT012*  TEST 12        COMPARISON OF ASIN AND ACOS TO TEST RELATIONSHIP 02530375
           IVTNUM = 12                                                  02540375
        AVS = (ASIN(+0.25) + ACOS(+0.25)) * 2.0                         02550375
           IF (AVS -  0.31414E+01) 20120, 10120, 40120                  02560375
40120      IF (AVS -  0.31418E+01) 10120, 10120, 20120                  02570375
10120      IVPASS = IVPASS + 1                                          02580375
           WRITE (NUVI, 80002) IVTNUM                                   02590375
           GO TO 0121                                                   02600375
20120      IVFAIL = IVFAIL + 1                                          02610375
           RVCORR = 3.14159265358979                                    02620375
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02630375
 0121      CONTINUE                                                     02640375
C*****                                                                  02650375
CBB** ********************** BBCSUM0  **********************************02660375
C**** WRITE OUT TEST SUMMARY                                            02670375
C****                                                                   02680375
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02690375
      WRITE (I02, 90004)                                                02700375
      WRITE (I02, 90014)                                                02710375
      WRITE (I02, 90004)                                                02720375
      WRITE (I02, 90020) IVPASS                                         02730375
      WRITE (I02, 90022) IVFAIL                                         02740375
      WRITE (I02, 90024) IVDELE                                         02750375
      WRITE (I02, 90026) IVINSP                                         02760375
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02770375
CBE** ********************** BBCSUM0  **********************************02780375
CBB** ********************** BBCFOOT0 **********************************02790375
C**** WRITE OUT REPORT FOOTINGS                                         02800375
C****                                                                   02810375
      WRITE (I02,90016) ZPROG, ZPROG                                    02820375
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02830375
      WRITE (I02,90019)                                                 02840375
CBE** ********************** BBCFOOT0 **********************************02850375
CBB** ********************** BBCFMT0A **********************************02860375
C**** FORMATS FOR TEST DETAIL LINES                                     02870375
C****                                                                   02880375
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02890375
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02900375
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02910375
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02920375
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02930375
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02940375
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02950375
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02960375
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02970375
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02980375
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02990375
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03000375
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03010375
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03020375
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03030375
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03040375
80050 FORMAT (1H ,48X,A31)                                              03050375
CBE** ********************** BBCFMT0A **********************************03060375
CBB** ********************** BBCFMT0B **********************************03070375
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03080375
C****                                                                   03090375
90002 FORMAT (1H1)                                                      03100375
90004 FORMAT (1H )                                                      03110375
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03120375
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03130375
90008 FORMAT (1H ,21X,A13,A17)                                          03140375
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03150375
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03160375
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03170375
     1       7X,7HREMARKS,24X)                                          03180375
90014 FORMAT (1H ,46H----------------------------------------------,    03190375
     1        33H---------------------------------)                     03200375
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03210375
C****                                                                   03220375
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03230375
C****                                                                   03240375
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03250375
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03260375
     1        A13)                                                      03270375
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03280375
C****                                                                   03290375
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03300375
C****                                                                   03310375
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03320375
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03330375
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03340375
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03350375
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03360375
CBE** ********************** BBCFMT0B **********************************03370375
C*****                                                                  03380375
C*****    END OF TEST SEGMENT 193                                       03390375
      STOP                                                              03400375
      END                                                               03410375
