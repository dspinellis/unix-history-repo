C***********************************************************************00010804
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020804
C*****   FM804               YDMOD - (160)                              00030804
C*****                                                                  00040804
C***********************************************************************00050804
C*****  GENERAL PURPOSE                                         ANS REF 00060804
C*****     TO TEST INTRINSIC FUNCTION - DMOD -                   15.3   00070804
C*****     (REMAINDERING -TYPE DOUBLE PRECISION)               (TABLE 5)00080804
CBB** ********************** BBCCOMNT **********************************00090804
C****                                                                   00100804
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00110804
C****                          VERSION 2.0                              00120804
C****                                                                   00130804
C****                                                                   00140804
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00150804
C****                   GENERAL SERVICES ADMINISTRATION                 00160804
C****                   FEDERAL SOFTWARE TESTING CENTER                 00170804
C****                   5203 LEESBURG PIKE, SUITE 1100                  00180804
C****                      FALLS CHURCH, VA. 22041                      00190804
C****                                                                   00200804
C****                          (703) 756-6153                           00210804
C****                                                                   00220804
CBE** ********************** BBCCOMNT **********************************00230804
C*****                                                                  00240804
C*****  S P E C I F I C A T I O N S  SEGMENT 160                        00250804
C*****                                                                  00260804
        DOUBLE PRECISION DQAVD, DQBVD, DQDVD, DQEVD, DQFVD              00270804
C*****                                                                  00280804
CBB** ********************** BBCINITA **********************************00290804
C**** SPECIFICATION STATEMENTS                                          00300804
C****                                                                   00310804
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320804
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330804
CBE** ********************** BBCINITA **********************************00340804
CBB** ********************** BBCINITB **********************************00350804
C**** INITIALIZE SECTION                                                00360804
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370804
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380804
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390804
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400804
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410804
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420804
      DATA   REMRKS /'                               '/                 00430804
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440804
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450804
C****                                                                   00460804
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470804
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480804
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490804
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560804
      IVPASS = 0                                                        00570804
      IVFAIL = 0                                                        00580804
      IVDELE = 0                                                        00590804
      IVINSP = 0                                                        00600804
      IVTOTL = 0                                                        00610804
      IVTOTN = 0                                                        00620804
      ICZERO = 0                                                        00630804
C                                                                       00640804
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650804
      I01 = 05                                                          00660804
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670804
      I02 = 06                                                          00680804
C                                                                       00690804
      I01 = 5                                                           00700804
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710804
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720804
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730804
C                                                                       00740804
      I02 = 6                                                           00750804
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760804
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770804
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780804
C                                                                       00790804
CBE** ********************** BBCINITB **********************************00800804
      NUVI = I02                                                        00810804
      IVTOTL = 11                                                       00820804
      ZPROG = 'FM804'                                                   00830804
CBB** ********************** BBCHED0A **********************************00840804
C****                                                                   00850804
C**** WRITE REPORT TITLE                                                00860804
C****                                                                   00870804
      WRITE (I02, 90002)                                                00880804
      WRITE (I02, 90006)                                                00890804
      WRITE (I02, 90007)                                                00900804
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910804
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920804
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930804
CBE** ********************** BBCHED0A **********************************00940804
C*****                                                                  00950804
C*****    HEADER FOR SEGMENT 160                                        00960804
        WRITE (NUVI, 16001)                                             00970804
16001   FORMAT( 1H , //35H YDMOD - (160) INTRINSIC FUNCTION--//         00980804
     1          16X,19HDMOD (REMAINDERING) //                           00990804
     2          19H  ANS REF. - 15.3  )                                 01000804
CBB** ********************** BBCHED0B **********************************01010804
C**** WRITE DETAIL REPORT HEADERS                                       01020804
C****                                                                   01030804
      WRITE (I02,90004)                                                 01040804
      WRITE (I02,90004)                                                 01050804
      WRITE (I02,90013)                                                 01060804
      WRITE (I02,90014)                                                 01070804
      WRITE (I02,90015) IVTOTL                                          01080804
CBE** ********************** BBCHED0B **********************************01090804
C*****                                                                  01100804
CT001*  TEST 1                        FIRST VALUE ZERO, SECOND NON-ZERO 01110804
           IVTNUM = 1                                                   01120804
        DQBVD = 0.0D0                                                   01130804
        DQDVD = 4.5D0                                                   01140804
        DQAVD = DMOD(DQBVD, DQDVD)                                      01150804
           IF (DQAVD + 5.0D-10) 20010, 10010, 40010                     01160804
40010      IF (DQAVD - 5.0D-10) 10010, 10010, 20010                     01170804
10010      IVPASS = IVPASS + 1                                          01180804
           WRITE (NUVI, 80002) IVTNUM                                   01190804
           GO TO 0011                                                   01200804
20010      IVFAIL = IVFAIL + 1                                          01210804
           DVCORR = 0.0D0                                               01220804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    01230804
 0011      CONTINUE                                                     01240804
CT002*  TEST 2                                        BOTH VALUES EQUAL 01250804
           IVTNUM = 2                                                   01260804
        DQBVD = 0.35D1                                                  01270804
        DQDVD = 0.35D1                                                  01280804
        DQAVD = DMOD(DQBVD, DQDVD)                                      01290804
           IF (DQAVD + 5.0D-10) 20020, 10020, 40020                     01300804
40020      IF (DQAVD - 5.0D-10) 10020, 10020, 20020                     01310804
10020      IVPASS = IVPASS + 1                                          01320804
           WRITE (NUVI, 80002) IVTNUM                                   01330804
           GO TO 0021                                                   01340804
20020      IVFAIL = IVFAIL + 1                                          01350804
           DVCORR = 0.0D0                                               01360804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    01370804
 0021      CONTINUE                                                     01380804
CT003*  TEST 3           FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND 01390804
           IVTNUM = 3                                                   01400804
        DQBVD = -0.10D2                                                 01410804
        DQDVD = -0.3D1                                                  01420804
        DQAVD = DMOD(DQBVD, DQDVD)                                      01430804
           IF (DQAVD + 1.000000001D0) 20030, 10030, 40030               01440804
40030      IF (DQAVD + 0.9999999995D0) 10030, 10030, 20030              01450804
10030      IVPASS = IVPASS + 1                                          01460804
           WRITE (NUVI, 80002) IVTNUM                                   01470804
           GO TO 0031                                                   01480804
20030      IVFAIL = IVFAIL + 1                                          01490804
           DVCORR = -1.0D0                                              01500804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    01510804
 0031      CONTINUE                                                     01520804
CT004*  TEST 4               FIRST MAGNITUDE LARGER, MULTIPLE OF SECOND 01530804
           IVTNUM = 4                                                   01540804
        DQDVD = 1.5D0                                                   01550804
        DQBVD = 1.5D0 + DQDVD + 1.5D0                                   01560804
        DQAVD = DMOD(DQBVD, DQDVD)                                      01570804
           IF (DQAVD + 5.0D-10) 20040, 10040, 40040                     01580804
40040      IF (DQAVD - 5.0D-10) 10040, 10040, 20040                     01590804
10040      IVPASS = IVPASS + 1                                          01600804
           WRITE (NUVI, 80002) IVTNUM                                   01610804
           GO TO 0041                                                   01620804
20040      IVFAIL = IVFAIL + 1                                          01630804
           DVCORR = 0.0D0                                               01640804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    01650804
 0041      CONTINUE                                                     01660804
CT005*  TEST 5           FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND 01670804
           IVTNUM = 5                                                   01680804
        DQBVD = 7.625D0                                                 01690804
        DQDVD = 2.125D0                                                 01700804
        DQAVD = DMOD(DQBVD, DQDVD)                                      01710804
           IF (DQAVD - 1.249999999D0) 20050, 10050, 40050               01720804
40050      IF (DQAVD - 1.250000001D0) 10050, 10050, 20050               01730804
10050      IVPASS = IVPASS + 1                                          01740804
           WRITE (NUVI, 80002) IVTNUM                                   01750804
           GO TO 0051                                                   01760804
20050      IVFAIL = IVFAIL + 1                                          01770804
           DVCORR = 1.25D0                                              01780804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    01790804
 0051      CONTINUE                                                     01800804
CT006*  TEST 6                        FIRST VALUE ZERO, SECOND NEGATIVE 01810804
           IVTNUM = 6                                                   01820804
        DQBVD = 0.0D0                                                   01830804
        DQDVD = -0.45D1                                                 01840804
        DQAVD = DMOD(DQBVD, DQDVD)                                      01850804
           IF (DQAVD + 5.0D-10) 20060, 10060, 40060                     01860804
40060      IF (DQAVD - 5.0D-10) 10060, 10060, 20060                     01870804
10060      IVPASS = IVPASS + 1                                          01880804
           WRITE (NUVI, 80002) IVTNUM                                   01890804
           GO TO 0061                                                   01900804
20060      IVFAIL = IVFAIL + 1                                          01910804
           DVCORR = 0.0D0                                               01920804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    01930804
 0061      CONTINUE                                                     01940804
CT007*  TEST 7                         BOTH VALUES EQUAL, BOTH NEGATIVE 01950804
           IVTNUM = 7                                                   01960804
        DQBVD = -3.5D1                                                  01970804
        DQDVD = -3.5D1                                                  01980804
        DQAVD = DMOD(DQBVD, DQDVD)                                      01990804
           IF (DQAVD + 5.0D-10) 20070, 10070, 40070                     02000804
40070      IF (DQAVD - 5.0D-10) 10070, 10070, 20070                     02010804
10070      IVPASS = IVPASS + 1                                          02020804
           WRITE (NUVI, 80002) IVTNUM                                   02030804
           GO TO 0071                                                   02040804
20070      IVFAIL = IVFAIL + 1                                          02050804
           DVCORR = 0.0D0                                               02060804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    02070804
 0071      CONTINUE                                                     02080804
CT008*  TEST 8          FIRST MAGNITUDE LARGER, MULIPLES, BOTH NEGATIVE 02090804
           IVTNUM = 8                                                   02100804
        DQDVD = 3.5D0                                                   02110804
        DQBVD = -(3.5D0 + DQDVD + 3.5D0)                                02120804
        DQAVD = DMOD(DQBVD, -DQDVD)                                     02130804
           IF (DQAVD + 5.0D-10) 20080, 10080, 40080                     02140804
40080      IF (DQAVD - 5.0D-10) 10080, 10080, 20080                     02150804
10080      IVPASS = IVPASS + 1                                          02160804
           WRITE (NUVI, 80002) IVTNUM                                   02170804
           GO TO 0081                                                   02180804
20080      IVFAIL = IVFAIL + 1                                          02190804
           DVCORR = 0.0D0                                               02200804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    02210804
 0081      CONTINUE                                                     02220804
CT009*  TEST 9          FIRST VALUE POSITIVE, SECOND NEGATIVE, MULTIPLE 02230804
           IVTNUM = 9                                                   02240804
        DQBVD = 10.5D0                                                  02250804
        DQDVD = -3.5D0                                                  02260804
        DQAVD = DMOD(DQBVD, DQDVD)                                      02270804
           IF (DQAVD + 5.0D-10) 20090, 10090, 40090                     02280804
40090      IF (DQAVD - 5.0D-10) 10090, 10090, 20090                     02290804
10090      IVPASS = IVPASS + 1                                          02300804
           WRITE (NUVI, 80002) IVTNUM                                   02310804
           GO TO 0091                                                   02320804
20090      IVFAIL = IVFAIL + 1                                          02330804
           DVCORR = 0.0D0                                               02340804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    02350804
 0091      CONTINUE                                                     02360804
CT010*  TEST 10                 FIRST VALUE ZERO PRECEDED BY MINUS SIGN 02370804
           IVTNUM = 10                                                  02380804
        DQDVD = 0.0D0                                                   02390804
        DQEVD = 4.5D0                                                   02400804
        DQAVD = DMOD(-DQDVD, DQEVD)                                     02410804
           IF (DQAVD + 5.0D-10) 20100, 10100, 40100                     02420804
40100      IF (DQAVD - 5.0D-10) 10100, 10100, 20100                     02430804
10100      IVPASS = IVPASS + 1                                          02440804
           WRITE (NUVI, 80002) IVTNUM                                   02450804
           GO TO 0101                                                   02460804
20100      IVFAIL = IVFAIL + 1                                          02470804
           DVCORR = 0.0D0                                               02480804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    02490804
 0101      CONTINUE                                                     02500804
CT011*  TEST 11         PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT 02510804
           IVTNUM = 11                                                  02520804
        DQDVD = 0.7625D1                                                02530804
        DQEVD = 0.2125D1                                                02540804
        DQFVD = 0.2D1                                                   02550804
        DQAVD = DMOD(DQDVD - DQFVD, DQEVD + DQFVD)                      02560804
           IF (DQAVD - 0.1499999999D1) 20110, 10110, 40110              02570804
40110      IF (DQAVD - 0.1500000001D1) 10110, 10110, 20110              02580804
10110      IVPASS = IVPASS + 1                                          02590804
           WRITE (NUVI, 80002) IVTNUM                                   02600804
           GO TO 0111                                                   02610804
20110      IVFAIL = IVFAIL + 1                                          02620804
           DVCORR = 0.15D1                                              02630804
           WRITE (NUVI, 80031) IVTNUM, DQAVD, DVCORR                    02640804
 0111      CONTINUE                                                     02650804
C*****                                                                  02660804
CBB** ********************** BBCSUM0  **********************************02670804
C**** WRITE OUT TEST SUMMARY                                            02680804
C****                                                                   02690804
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02700804
      WRITE (I02, 90004)                                                02710804
      WRITE (I02, 90014)                                                02720804
      WRITE (I02, 90004)                                                02730804
      WRITE (I02, 90020) IVPASS                                         02740804
      WRITE (I02, 90022) IVFAIL                                         02750804
      WRITE (I02, 90024) IVDELE                                         02760804
      WRITE (I02, 90026) IVINSP                                         02770804
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02780804
CBE** ********************** BBCSUM0  **********************************02790804
CBB** ********************** BBCFOOT0 **********************************02800804
C**** WRITE OUT REPORT FOOTINGS                                         02810804
C****                                                                   02820804
      WRITE (I02,90016) ZPROG, ZPROG                                    02830804
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02840804
      WRITE (I02,90019)                                                 02850804
CBE** ********************** BBCFOOT0 **********************************02860804
CBB** ********************** BBCFMT0A **********************************02870804
C**** FORMATS FOR TEST DETAIL LINES                                     02880804
C****                                                                   02890804
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02900804
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02910804
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02920804
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02930804
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02940804
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02950804
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02960804
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02970804
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02980804
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02990804
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03000804
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03010804
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03020804
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03030804
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03040804
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03050804
80050 FORMAT (1H ,48X,A31)                                              03060804
CBE** ********************** BBCFMT0A **********************************03070804
CBB** ********************** BBCFMAT1 **********************************03080804
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03090804
C****                                                                   03100804
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03110804
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03120804
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03130804
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03140804
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03150804
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03160804
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03170804
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03180804
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03190804
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03200804
     21H(,F12.5,2H, ,F12.5,1H))                                         03210804
CBE** ********************** BBCFMAT1 **********************************03220804
CBB** ********************** BBCFMT0B **********************************03230804
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03240804
C****                                                                   03250804
90002 FORMAT (1H1)                                                      03260804
90004 FORMAT (1H )                                                      03270804
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03280804
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03290804
90008 FORMAT (1H ,21X,A13,A17)                                          03300804
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03310804
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03320804
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03330804
     1       7X,7HREMARKS,24X)                                          03340804
90014 FORMAT (1H ,46H----------------------------------------------,    03350804
     1        33H---------------------------------)                     03360804
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03370804
C****                                                                   03380804
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03390804
C****                                                                   03400804
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03410804
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03420804
     1        A13)                                                      03430804
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03440804
C****                                                                   03450804
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03460804
C****                                                                   03470804
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03480804
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03490804
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03500804
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03510804
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03520804
CBE** ********************** BBCFMT0B **********************************03530804
C*****                                                                  03540804
C*****    END OF TEST SEGMENT 160                                       03550804
        STOP                                                            03560804
        END                                                             03570804
                                                                        03580804
