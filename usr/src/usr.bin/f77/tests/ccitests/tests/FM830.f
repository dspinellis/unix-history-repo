C***********************************************************************00010830
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020830
C*****   FM830                                                          00030830
C*****                       YGEN2 - (207)                              00040830
C*****                                                                  00050830
C***********************************************************************00060830
C*****  GENERAL PURPOSE                                         ANS REF 00070830
C*****      TEST GENERIC FUNCTIONS                               15.3   00080830
C*****         AINT, ANINT, NINT, SQRT, EXP, LOG, LOG10         TABLE 5 00090830
C*****                                                                  00100830
CBB** ********************** BBCCOMNT **********************************00110830
C****                                                                   00120830
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130830
C****                          VERSION 2.0                              00140830
C****                                                                   00150830
C****                                                                   00160830
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170830
C****                   GENERAL SERVICES ADMINISTRATION                 00180830
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190830
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200830
C****                      FALLS CHURCH, VA. 22041                      00210830
C****                                                                   00220830
C****                          (703) 756-6153                           00230830
C****                                                                   00240830
CBE** ********************** BBCCOMNT **********************************00250830
C*****                                                                  00260830
C*****  S P E C I F I C A T I O N S  SEGMENT 207                        00270830
        DOUBLE PRECISION AVD, DVCORR                                    00280830
        COMPLEX AVC, ZVCORR                                             00290830
        REAL R2E(2)                                                     00300830
        EQUIVALENCE (AVC, R2E)                                          00310830
C*****                                                                  00320830
CBB** ********************** BBCINITA **********************************00330830
C**** SPECIFICATION STATEMENTS                                          00340830
C****                                                                   00350830
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00360830
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00370830
CBE** ********************** BBCINITA **********************************00380830
CBB** ********************** BBCINITB **********************************00390830
C**** INITIALIZE SECTION                                                00400830
      DATA  ZVERS,                  ZVERSD,             ZDATE           00410830
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00420830
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00430830
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00440830
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00450830
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00460830
      DATA   REMRKS /'                               '/                 00470830
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00480830
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00490830
C****                                                                   00500830
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00510830
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00520830
CZ03  ZPROG  = 'PROGRAM NAME'                                           00530830
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00600830
      IVPASS = 0                                                        00610830
      IVFAIL = 0                                                        00620830
      IVDELE = 0                                                        00630830
      IVINSP = 0                                                        00640830
      IVTOTL = 0                                                        00650830
      IVTOTN = 0                                                        00660830
      ICZERO = 0                                                        00670830
C                                                                       00680830
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00690830
      I01 = 05                                                          00700830
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00710830
      I02 = 06                                                          00720830
C                                                                       00730830
      I01 = 5                                                           00740830
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750830
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00760830
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00770830
C                                                                       00780830
      I02 = 6                                                           00790830
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00800830
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00810830
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00820830
C                                                                       00830830
CBE** ********************** BBCINITB **********************************00840830
      NUVI = I02                                                        00850830
      IVTOTL = 9                                                        00860830
      ZPROG = 'FM830'                                                   00870830
CBB** ********************** BBCHED0A **********************************00880830
C****                                                                   00890830
C**** WRITE REPORT TITLE                                                00900830
C****                                                                   00910830
      WRITE (I02, 90002)                                                00920830
      WRITE (I02, 90006)                                                00930830
      WRITE (I02, 90007)                                                00940830
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00950830
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00960830
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00970830
CBE** ********************** BBCHED0A **********************************00980830
C*****                                                                  00990830
C*****    HEADER FOR SEGMENT 207                                        01000830
        WRITE(NUVI,20700)                                               01010830
20700   FORMAT( 1H , /  35H YGEN2 - (207) GENERIC FUNCTIONS --//        01020830
     1          42H  AINT, ANINT, NINT, SQRT, EXP, LOG, LOG10//         01030830
     2          17H  ANS REF. - 15.3)                                   01040830
CBB** ********************** BBCHED0B **********************************01050830
C**** WRITE DETAIL REPORT HEADERS                                       01060830
C****                                                                   01070830
      WRITE (I02,90004)                                                 01080830
      WRITE (I02,90004)                                                 01090830
      WRITE (I02,90013)                                                 01100830
      WRITE (I02,90014)                                                 01110830
      WRITE (I02,90015) IVTOTL                                          01120830
CBE** ********************** BBCHED0B **********************************01130830
C*****                                                                  01140830
CT001*  TEST 1                            TEST OF NINT WITH DOUBLE PREC 01150830
           IVTNUM = 1                                                   01160830
        LVI = NINT(27.96875D0)                                          01170830
           IF (LVI -    28) 20010, 10010, 20010                         01180830
10010      IVPASS = IVPASS + 1                                          01190830
           WRITE (NUVI, 80002) IVTNUM                                   01200830
           GO TO 0011                                                   01210830
20010      IVFAIL = IVFAIL + 1                                          01220830
           IVCORR =    28                                               01230830
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01240830
 0011      CONTINUE                                                     01250830
CT002*  TEST 2                  TEST OF AINT AND ANINT WITH DOUBLE PREC 01260830
           IVTNUM = 2                                                   01270830
        AVD = AINT(-1.375D0) + ANINT(-27.96875D0)                       01280830
           IF (AVD +  0.2900000002D+02) 20020, 10020, 40020             01290830
40020      IF (AVD +  0.2899999998D+02) 10020, 10020, 20020             01300830
10020      IVPASS = IVPASS + 1                                          01310830
           WRITE (NUVI, 80002) IVTNUM                                   01320830
           GO TO 0021                                                   01330830
20020      IVFAIL = IVFAIL + 1                                          01340830
           DVCORR = -29.0D0                                             01350830
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01360830
 0021      CONTINUE                                                     01370830
CT003*  TEST 3                    TEST OF SQRT AND EXP WITH DOUBLE PREC 01380830
           IVTNUM = 3                                                   01390830
        AVD = SQRT(16.0D0) - EXP(5.125D0)                               01400830
           IF (AVD +  0.1641741418D+03) 20030, 10030, 40030             01410830
40030      IF (AVD +  0.1641741415D+03) 10030, 10030, 20030             01420830
10030      IVPASS = IVPASS + 1                                          01430830
           WRITE (NUVI, 80002) IVTNUM                                   01440830
           GO TO 0031                                                   01450830
20030      IVFAIL = IVFAIL + 1                                          01460830
           DVCORR = -0.16417414165D+03                                  01470830
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01480830
 0031      CONTINUE                                                     01490830
CT004*  TEST 4                   TEST OF LOG AND LOG10 WITH DOUBLE PREC 01500830
           IVTNUM = 4                                                   01510830
        AVD = LOG(9.5D0) * LOG10(25.25D0)                               01520830
           IF (AVD -  0.3156899548D+01) 20040, 10040, 40040             01530830
40040      IF (AVD -  0.3156899552D+01) 10040, 10040, 20040             01540830
10040      IVPASS = IVPASS + 1                                          01550830
           WRITE (NUVI, 80002) IVTNUM                                   01560830
           GO TO 0041                                                   01570830
20040      IVFAIL = IVFAIL + 1                                          01580830
           DVCORR = 0.31568995498D+01                                   01590830
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01600830
 0041      CONTINUE                                                     01610830
CT005*  TEST 5                            TEST OF AINT, SQRT AND  LOG10 01620830
           IVTNUM = 5                                                   01630830
        AVD = (AINT(2.75D0) + SQRT(17.125D0)) * LOG10(10.0D0)           01640830
           IF (AVD -  0.6138236337D+01) 20050, 10050, 40050             01650830
40050      IF (AVD -  0.6138236343D+01) 10050, 10050, 20050             01660830
10050      IVPASS = IVPASS + 1                                          01670830
           WRITE (NUVI, 80002) IVTNUM                                   01680830
           GO TO 0051                                                   01690830
20050      IVFAIL = IVFAIL + 1                                          01700830
           DVCORR = 0.613823634D+01                                     01710830
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01720830
 0051      CONTINUE                                                     01730830
CT006*  TEST 6                   TEST OF AINT AND NINT WITH DOUBLE PREC 01740830
           IVTNUM = 6                                                   01750830
        AVD = AINT(72.375D0) * NINT(-4.25D0)                            01760830
           IF (AVD +  0.2880000002D+03) 20060, 10060, 40060             01770830
40060      IF (AVD +  0.2879999998D+03) 10060, 10060, 20060             01780830
10060      IVPASS = IVPASS + 1                                          01790830
           WRITE (NUVI, 80002) IVTNUM                                   01800830
           GO TO 0061                                                   01810830
20060      IVFAIL = IVFAIL + 1                                          01820830
           DVCORR = -288.0D0                                            01830830
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01840830
 0061      CONTINUE                                                     01850830
CT007*  TEST 7                   TEST OF SQRT, EXP AND LOG WITH COMPLEX 01860830
           IVTNUM = 7                                                   01870830
        AVC = SQRT((-4.0,2.0)) + EXP((2.125,6.75)) * LOG((17.375,2.5))  01880830
           IF (R2E(1) -  0.21370E+02) 20070, 40072, 40071               01890830
40071      IF (R2E(1) -  0.21373E+02) 40072, 40072, 20070               01900830
40072      IF (R2E(2) -  0.13922E+02) 20070, 10070, 40070               01910830
40070      IF (R2E(2) -  0.13925E+02) 10070, 10070, 20070               01920830
10070      IVPASS = IVPASS + 1                                          01930830
           WRITE (NUVI, 80002) IVTNUM                                   01940830
           GO TO 0071                                                   01950830
20070      IVFAIL = IVFAIL + 1                                          01960830
           ZVCORR = (21.3712104, 13.9235362)                            01970830
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01980830
 0071      CONTINUE                                                     01990830
CT008*  TEST 8                       TEST OF SQRT WITH REAL AND COMPLEX 02000830
           IVTNUM = 8                                                   02010830
        AVC = SQRT(77.76953) - SQRT((-22.125, 7.0))                     02020830
           IF (R2E(1) -  0.80831E+01) 20080, 40082, 40081               02030830
40081      IF (R2E(1) -  0.80840E+01) 40082, 40082, 20080               02040830
40082      IF (R2E(2) +  0.47611E+01) 20080, 10080, 40080               02050830
40080      IF (R2E(2) +  0.47605E+01) 10080, 10080, 20080               02060830
10080      IVPASS = IVPASS + 1                                          02070830
           WRITE (NUVI, 80002) IVTNUM                                   02080830
           GO TO 0081                                                   02090830
20080      IVFAIL = IVFAIL + 1                                          02100830
           ZVCORR = (8.0835370, -4.7608266)                             02110830
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02120830
 0081      CONTINUE                                                     02130830
CT009*  TEST 9                          TEST OF AINT, NINT, EXP AND LOG 02140830
C*****                                            WITH REAL AND COMPLEX 02150830
           IVTNUM = 9                                                   02160830
        AVC = AINT(2.25) * NINT(1.50) + EXP((1.0, 2.0)) - LOG(5.125)    02170830
           IF (R2E(1) -  0.12346E+01) 20090, 40092, 40091               02180830
40091      IF (R2E(1) -  0.12348E+01) 40092, 40092, 20090               02190830
40092      IF (R2E(2) -  0.24716E+01) 20090, 10090, 40090               02200830
40090      IF (R2E(2) -  0.24719E+01) 10090, 10090, 20090               02210830
10090      IVPASS = IVPASS + 1                                          02220830
           WRITE (NUVI, 80002) IVTNUM                                   02230830
           GO TO 0091                                                   02240830
20090      IVFAIL = IVFAIL + 1                                          02250830
           ZVCORR = (1.234665192, 2.471726672)                          02260830
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02270830
 0091      CONTINUE                                                     02280830
C*****                                                                  02290830
CBB** ********************** BBCSUM0  **********************************02300830
C**** WRITE OUT TEST SUMMARY                                            02310830
C****                                                                   02320830
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02330830
      WRITE (I02, 90004)                                                02340830
      WRITE (I02, 90014)                                                02350830
      WRITE (I02, 90004)                                                02360830
      WRITE (I02, 90020) IVPASS                                         02370830
      WRITE (I02, 90022) IVFAIL                                         02380830
      WRITE (I02, 90024) IVDELE                                         02390830
      WRITE (I02, 90026) IVINSP                                         02400830
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02410830
CBE** ********************** BBCSUM0  **********************************02420830
CBB** ********************** BBCFOOT0 **********************************02430830
C**** WRITE OUT REPORT FOOTINGS                                         02440830
C****                                                                   02450830
      WRITE (I02,90016) ZPROG, ZPROG                                    02460830
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02470830
      WRITE (I02,90019)                                                 02480830
CBE** ********************** BBCFOOT0 **********************************02490830
CBB** ********************** BBCFMT0A **********************************02500830
C**** FORMATS FOR TEST DETAIL LINES                                     02510830
C****                                                                   02520830
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02530830
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02540830
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02550830
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02560830
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02570830
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02580830
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02590830
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02600830
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02610830
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02620830
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02630830
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02640830
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02650830
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02660830
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02670830
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02680830
80050 FORMAT (1H ,48X,A31)                                              02690830
CBE** ********************** BBCFMT0A **********************************02700830
CBB** ********************** BBCFMAT1 **********************************02710830
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02720830
C****                                                                   02730830
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02740830
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02750830
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02760830
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02770830
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02780830
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02790830
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02800830
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02810830
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02820830
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02830830
     21H(,F12.5,2H, ,F12.5,1H))                                         02840830
CBE** ********************** BBCFMAT1 **********************************02850830
CBB** ********************** BBCFMT0B **********************************02860830
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02870830
C****                                                                   02880830
90002 FORMAT (1H1)                                                      02890830
90004 FORMAT (1H )                                                      02900830
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02910830
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02920830
90008 FORMAT (1H ,21X,A13,A17)                                          02930830
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02940830
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02950830
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02960830
     1       7X,7HREMARKS,24X)                                          02970830
90014 FORMAT (1H ,46H----------------------------------------------,    02980830
     1        33H---------------------------------)                     02990830
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03000830
C****                                                                   03010830
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03020830
C****                                                                   03030830
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03040830
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03050830
     1        A13)                                                      03060830
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03070830
C****                                                                   03080830
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03090830
C****                                                                   03100830
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03110830
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03120830
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03130830
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03140830
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03150830
CBE** ********************** BBCFMT0B **********************************03160830
C*****                                                                  03170830
C*****    END OF TEST SEGMENT 207                                       03180830
      STOP                                                              03190830
      END                                                               03200830
