C***********************************************************************00010826
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020826
C*****   FM826                                                          00030826
C*****                       YDTANH - (200)                             00040826
C*****                                                                  00050826
C***********************************************************************00060826
C*****  GENERAL PURPOSE                                         ANS REF 00070826
C*****    TEST INTRINSIC FUNCTION DTANH                           15.3  00080826
C*****                                                          TABLE 5 00090826
C*****                                                                  00100826
CBB** ********************** BBCCOMNT **********************************00110826
C****                                                                   00120826
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130826
C****                          VERSION 2.0                              00140826
C****                                                                   00150826
C****                                                                   00160826
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170826
C****                   GENERAL SERVICES ADMINISTRATION                 00180826
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190826
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200826
C****                      FALLS CHURCH, VA. 22041                      00210826
C****                                                                   00220826
C****                          (703) 756-6153                           00230826
C****                                                                   00240826
CBE** ********************** BBCCOMNT **********************************00250826
C*****                                                                  00260826
C*****    S P E C I F I C A T I O N S SEGMENT 200                       00270826
        DOUBLE PRECISION AVD, BVD, DVCORR                               00280826
C*****                                                                  00290826
CBB** ********************** BBCINITA **********************************00300826
C**** SPECIFICATION STATEMENTS                                          00310826
C****                                                                   00320826
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330826
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340826
CBE** ********************** BBCINITA **********************************00350826
CBB** ********************** BBCINITB **********************************00360826
C**** INITIALIZE SECTION                                                00370826
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380826
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390826
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400826
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410826
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420826
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430826
      DATA   REMRKS /'                               '/                 00440826
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450826
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460826
C****                                                                   00470826
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480826
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490826
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500826
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570826
      IVPASS = 0                                                        00580826
      IVFAIL = 0                                                        00590826
      IVDELE = 0                                                        00600826
      IVINSP = 0                                                        00610826
      IVTOTL = 0                                                        00620826
      IVTOTN = 0                                                        00630826
      ICZERO = 0                                                        00640826
C                                                                       00650826
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660826
      I01 = 05                                                          00670826
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680826
      I02 = 06                                                          00690826
C                                                                       00700826
      I01 = 5                                                           00710826
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720826
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730826
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740826
C                                                                       00750826
      I02 = 6                                                           00760826
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770826
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780826
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790826
C                                                                       00800826
CBE** ********************** BBCINITB **********************************00810826
      NUVI = I02                                                        00820826
      IVTOTL = 9                                                        00830826
      ZPROG = 'FM826'                                                   00840826
CBB** ********************** BBCHED0A **********************************00850826
C****                                                                   00860826
C**** WRITE REPORT TITLE                                                00870826
C****                                                                   00880826
      WRITE (I02, 90002)                                                00890826
      WRITE (I02, 90006)                                                00900826
      WRITE (I02, 90007)                                                00910826
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920826
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930826
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940826
CBE** ********************** BBCHED0A **********************************00950826
C*****                                                                  00960826
C*****    HEADER FOR SEGMENT 200                                        00970826
        WRITE(NUVI,20000)                                               00980826
20000   FORMAT(1H , / 36H  YDTANH - (200) INTRINSIC FUNCTIONS//         00990826
     1         46H  DTANH  (DOUBLE PRECISION HYPERBOLIC TANGENT)//      01000826
     2         17H  ANS REF. - 15.3)                                    01010826
CBB** ********************** BBCHED0B **********************************01020826
C**** WRITE DETAIL REPORT HEADERS                                       01030826
C****                                                                   01040826
      WRITE (I02,90004)                                                 01050826
      WRITE (I02,90004)                                                 01060826
      WRITE (I02,90013)                                                 01070826
      WRITE (I02,90014)                                                 01080826
      WRITE (I02,90015) IVTOTL                                          01090826
CBE** ********************** BBCHED0B **********************************01100826
C*****                                                                  01110826
CT001*  TEST 1                                       TEST AT ZERO (0.0) 01120826
           IVTNUM = 1                                                   01130826
        BVD = 0.0D0                                                     01140826
        AVD = DTANH(BVD)                                                01150826
           IF (AVD + 0.5000000000D-09) 20010, 10010, 40010              01160826
40010      IF (AVD - 0.5000000000D-09) 10010, 10010, 20010              01170826
10010      IVPASS = IVPASS + 1                                          01180826
           WRITE (NUVI, 80002) IVTNUM                                   01190826
           GO TO 0011                                                   01200826
20010      IVFAIL = IVFAIL + 1                                          01210826
           DVCORR = 0.00000000000000000000D+00                          01220826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01230826
 0011      CONTINUE                                                     01240826
CT002*  TEST 2                                      A NEGATIVE ARGUMENT 01250826
           IVTNUM = 2                                                   01260826
        AVD = DTANH(-2.5D0)                                             01270826
           IF (AVD + 0.9866142987D+00) 20020, 10020, 40020              01280826
40020      IF (AVD + 0.9866142976D+00) 10020, 10020, 20020              01290826
10020      IVPASS = IVPASS + 1                                          01300826
           WRITE (NUVI, 80002) IVTNUM                                   01310826
           GO TO 0021                                                   01320826
20020      IVFAIL = IVFAIL + 1                                          01330826
           DVCORR = -0.98661429815143028888D+00                         01340826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01350826
 0021      CONTINUE                                                     01360826
CT003*  TEST 3                       A VARIABLE SUPPLIED AS AN ARGUMENT 01370826
           IVTNUM = 3                                                   01380826
        BVD = 4.75D0                                                    01390826
        AVD = DTANH(BVD)                                                01400826
           IF (AVD - 0.9998503070D+00) 20030, 10030, 40030              01410826
40030      IF (AVD - 0.9998503081D+00) 10030, 10030, 20030              01420826
10030      IVPASS = IVPASS + 1                                          01430826
           WRITE (NUVI, 80002) IVTNUM                                   01440826
           GO TO 0031                                                   01450826
20030      IVFAIL = IVFAIL + 1                                          01460826
           DVCORR = 0.99985030754497877538D+00                          01470826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01480826
 0031      CONTINUE                                                     01490826
CT004*  TEST 4           A POSITIVE REAL NUMBER SUPPLIED AS AN ARGUMENT 01500826
           IVTNUM = 4                                                   01510826
        AVD = DTANH(15.125D0)                                           01520826
           IF (AVD - 0.9999999995D+00) 20040, 10040, 40040              01530826
40040      IF (AVD - 0.1000000001D+01) 10040, 10040, 20040              01540826
10040      IVPASS = IVPASS + 1                                          01550826
           WRITE (NUVI, 80002) IVTNUM                                   01560826
           GO TO 0041                                                   01570826
20040      IVFAIL = IVFAIL + 1                                          01580826
           DVCORR = 0.99999999999985424552D+00                          01590826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01600826
 0041      CONTINUE                                                     01610826
CT005*  TEST 5                                   TEST WITH LARGE VALUES 01620826
           IVTNUM = 5                                                   01630826
        BVD = 10.0D0 ** 2                                               01640826
        AVD = DTANH(BVD)                                                01650826
           IF (AVD - 0.9999999995D+00) 20050, 10050, 40050              01660826
40050      IF (AVD - 0.1000000001D+01) 10050, 10050, 20050              01670826
10050      IVPASS = IVPASS + 1                                          01680826
           WRITE (NUVI, 80002) IVTNUM                                   01690826
           GO TO 0051                                                   01700826
20050      IVFAIL = IVFAIL + 1                                          01710826
           DVCORR = 1.0000000000000000000D+00                           01720826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01730826
 0051      CONTINUE                                                     01740826
CT006*  TEST 6                                   TEST WITH LARGE VALUES 01750826
           IVTNUM = 6                                                   01760826
        BVD = -100.0D0 * 10.0D0                                         01770826
        AVD = DTANH(BVD)                                                01780826
           IF (AVD + 0.1000000001D+01) 20060, 10060, 40060              01790826
40060      IF (AVD + 0.9999999995D+00) 10060, 10060, 20060              01800826
10060      IVPASS = IVPASS + 1                                          01810826
           WRITE (NUVI, 80002) IVTNUM                                   01820826
           GO TO 0061                                                   01830826
20060      IVFAIL = IVFAIL + 1                                          01840826
           DVCORR = -1.0000000000000000000D+00                          01850826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01860826
 0061      CONTINUE                                                     01870826
CT007*  TEST 7                            AN ARGUMENT OF HIGH MAGNITUDE 01880826
           IVTNUM = 7                                                   01890826
        BVD = 3.0D+36                                                   01900826
        AVD = DTANH(BVD)                                                01910826
           IF (AVD - 0.9999999995D+00) 20070, 10070, 40070              01920826
40070      IF (AVD - 0.1000000001D+01) 10070, 10070, 20070              01930826
10070      IVPASS = IVPASS + 1                                          01940826
           WRITE (NUVI, 80002) IVTNUM                                   01950826
           GO TO 0071                                                   01960826
20070      IVFAIL = IVFAIL + 1                                          01970826
           DVCORR = 1.0000000000000000000D+00                           01980826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01990826
 0071      CONTINUE                                                     02000826
CT008*  TEST 8                             AN ARGUMENT OF LOW MAGNITUDE 02010826
           IVTNUM = 8                                                   02020826
        BVD = -1.0D-15                                                  02030826
        AVD = DTANH(BVD)                                                02040826
           IF (AVD + 0.1000000001D-14) 20080, 10080, 40080              02050826
40080      IF (AVD + 0.9999999995D-15) 10080, 10080, 20080              02060826
10080      IVPASS = IVPASS + 1                                          02070826
           WRITE (NUVI, 80002) IVTNUM                                   02080826
           GO TO 0081                                                   02090826
20080      IVFAIL = IVFAIL + 1                                          02100826
           DVCORR = -1.0000000000000000000D-15                          02110826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02120826
 0081      CONTINUE                                                     02130826
CT009*  TEST 9                               THE FUNCTION APPLIED TWICE 02140826
           IVTNUM = 9                                                   02150826
        AVD = DTANH(0.5D0) * DTANH(0.75D0)                              02160826
           IF (AVD - 0.2935132281D+00) 20090, 10090, 40090              02170826
40090      IF (AVD - 0.2935132285D+00) 10090, 10090, 20090              02180826
10090      IVPASS = IVPASS + 1                                          02190826
           WRITE (NUVI, 80002) IVTNUM                                   02200826
           GO TO 0091                                                   02210826
20090      IVFAIL = IVFAIL + 1                                          02220826
           DVCORR = 0.293513228313886504621D+00                         02230826
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02240826
 0091      CONTINUE                                                     02250826
C*****                                                                  02260826
CBB** ********************** BBCSUM0  **********************************02270826
C**** WRITE OUT TEST SUMMARY                                            02280826
C****                                                                   02290826
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02300826
      WRITE (I02, 90004)                                                02310826
      WRITE (I02, 90014)                                                02320826
      WRITE (I02, 90004)                                                02330826
      WRITE (I02, 90020) IVPASS                                         02340826
      WRITE (I02, 90022) IVFAIL                                         02350826
      WRITE (I02, 90024) IVDELE                                         02360826
      WRITE (I02, 90026) IVINSP                                         02370826
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02380826
CBE** ********************** BBCSUM0  **********************************02390826
CBB** ********************** BBCFOOT0 **********************************02400826
C**** WRITE OUT REPORT FOOTINGS                                         02410826
C****                                                                   02420826
      WRITE (I02,90016) ZPROG, ZPROG                                    02430826
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02440826
      WRITE (I02,90019)                                                 02450826
CBE** ********************** BBCFOOT0 **********************************02460826
CBB** ********************** BBCFMT0A **********************************02470826
C**** FORMATS FOR TEST DETAIL LINES                                     02480826
C****                                                                   02490826
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02500826
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02510826
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02520826
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02530826
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02540826
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02550826
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02560826
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02570826
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02580826
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02590826
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02600826
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02610826
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02620826
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02630826
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02640826
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02650826
80050 FORMAT (1H ,48X,A31)                                              02660826
CBE** ********************** BBCFMT0A **********************************02670826
CBB** ********************** BBCFMAT1 **********************************02680826
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02690826
C****                                                                   02700826
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02710826
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02720826
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02730826
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02740826
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02750826
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02760826
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02770826
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02780826
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02790826
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02800826
     21H(,F12.5,2H, ,F12.5,1H))                                         02810826
CBE** ********************** BBCFMAT1 **********************************02820826
CBB** ********************** BBCFMT0B **********************************02830826
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02840826
C****                                                                   02850826
90002 FORMAT (1H1)                                                      02860826
90004 FORMAT (1H )                                                      02870826
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02880826
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02890826
90008 FORMAT (1H ,21X,A13,A17)                                          02900826
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02910826
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02920826
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02930826
     1       7X,7HREMARKS,24X)                                          02940826
90014 FORMAT (1H ,46H----------------------------------------------,    02950826
     1        33H---------------------------------)                     02960826
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02970826
C****                                                                   02980826
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02990826
C****                                                                   03000826
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03010826
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03020826
     1        A13)                                                      03030826
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03040826
C****                                                                   03050826
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03060826
C****                                                                   03070826
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03080826
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03090826
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03100826
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03110826
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03120826
CBE** ********************** BBCFMT0B **********************************03130826
C*****                                                                  03140826
C*****    END OF TEST SEGMENT 200                                       03150826
      STOP                                                              03160826
      END                                                               03170826
                                                                        03180826
