C***********************************************************************00010378
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020378
C*****   FM378                                                          00030378
C*****                       XTANH - (199)                              00040378
C*****                                                                  00050378
C***********************************************************************00060378
C*****  GENERAL PURPOSE                                      SUBSET REF 00070378
C*****    TEST INTRINSIC FUNCTION TANH                         15.3     00080378
C*****                                                        TABLE 5   00090378
C*****                                                                  00100378
CBB** ********************** BBCCOMNT **********************************00110378
C****                                                                   00120378
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130378
C****                          VERSION 2.0                              00140378
C****                                                                   00150378
C****                                                                   00160378
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170378
C****                   GENERAL SERVICES ADMINISTRATION                 00180378
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190378
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200378
C****                      FALLS CHURCH, VA. 22041                      00210378
C****                                                                   00220378
C****                          (703) 756-6153                           00230378
C****                                                                   00240378
CBE** ********************** BBCCOMNT **********************************00250378
CBB** ********************** BBCINITA **********************************00260378
C**** SPECIFICATION STATEMENTS                                          00270378
C****                                                                   00280378
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290378
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300378
CBE** ********************** BBCINITA **********************************00310378
CBB** ********************** BBCINITB **********************************00320378
C**** INITIALIZE SECTION                                                00330378
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340378
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350378
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360378
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370378
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380378
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390378
      DATA   REMRKS /'                               '/                 00400378
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410378
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420378
C****                                                                   00430378
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440378
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450378
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460378
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530378
      IVPASS = 0                                                        00540378
      IVFAIL = 0                                                        00550378
      IVDELE = 0                                                        00560378
      IVINSP = 0                                                        00570378
      IVTOTL = 0                                                        00580378
      IVTOTN = 0                                                        00590378
      ICZERO = 0                                                        00600378
C                                                                       00610378
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620378
      I01 = 05                                                          00630378
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640378
      I02 = 06                                                          00650378
C                                                                       00660378
      I01 = 5                                                           00670378
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680378
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690378
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700378
C                                                                       00710378
      I02 = 6                                                           00720378
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730378
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740378
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750378
C                                                                       00760378
CBE** ********************** BBCINITB **********************************00770378
      NUVI = I02                                                        00780378
      IVTOTL = 9                                                        00790378
      ZPROG = 'FM378'                                                   00800378
CBB** ********************** BBCHED0A **********************************00810378
C****                                                                   00820378
C**** WRITE REPORT TITLE                                                00830378
C****                                                                   00840378
      WRITE (I02, 90002)                                                00850378
      WRITE (I02, 90006)                                                00860378
      WRITE (I02, 90007)                                                00870378
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880378
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890378
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900378
CBE** ********************** BBCHED0A **********************************00910378
C*****                                                                  00920378
C*****    HEADER FOR SEGMENT 199                                        00930378
        WRITE(NUVI,19900)                                               00940378
19900   FORMAT(1H , / 35H  XTANH - (199) INTRINSIC FUNCTIONS//          00950378
     1         28H  TANH  (HYPERBOLIC TANGENT)//                        00960378
     2         20H  SUBSET REF. - 15.3)                                 00970378
CBB** ********************** BBCHED0B **********************************00980378
C**** WRITE DETAIL REPORT HEADERS                                       00990378
C****                                                                   01000378
      WRITE (I02,90004)                                                 01010378
      WRITE (I02,90004)                                                 01020378
      WRITE (I02,90013)                                                 01030378
      WRITE (I02,90014)                                                 01040378
      WRITE (I02,90015) IVTOTL                                          01050378
CBE** ********************** BBCHED0B **********************************01060378
C*****                                                                  01070378
CT001*  TEST 1                                       TEST AT ZERO (0.0) 01080378
           IVTNUM = 1                                                   01090378
        BVS = 0.0                                                       01100378
        AVS = TANH(BVS)                                                 01110378
           IF (AVS + 0.50000E-04) 20010, 10010, 40010                   01120378
40010      IF (AVS - 0.50000E-04) 10010, 10010, 20010                   01130378
10010      IVPASS = IVPASS + 1                                          01140378
           WRITE (NUVI, 80002) IVTNUM                                   01150378
           GO TO 0011                                                   01160378
20010      IVFAIL = IVFAIL + 1                                          01170378
           RVCORR = 0.00000000000000                                    01180378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01190378
 0011      CONTINUE                                                     01200378
CT002*  TEST 2                                      A NEGATIVE ARGUMENT 01210378
           IVTNUM = 2                                                   01220378
        AVS = TANH(-2.5)                                                01230378
           IF (AVS + 0.98667E+00) 20020, 10020, 40020                   01240378
40020      IF (AVS + 0.98656E+00) 10020, 10020, 20020                   01250378
10020      IVPASS = IVPASS + 1                                          01260378
           WRITE (NUVI, 80002) IVTNUM                                   01270378
           GO TO 0021                                                   01280378
20020      IVFAIL = IVFAIL + 1                                          01290378
           RVCORR = -0.98661429815143                                   01300378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01310378
 0021      CONTINUE                                                     01320378
CT003*  TEST 3                       A VARIABLE SUPPLIED AS AN ARGUMENT 01330378
           IVTNUM = 3                                                   01340378
        BVS = 4.75                                                      01350378
        AVS = TANH(BVS)                                                 01360378
           IF (AVS - 0.99980E+00) 20030, 10030, 40030                   01370378
40030      IF (AVS - 0.99990E+00) 10030, 10030, 20030                   01380378
10030      IVPASS = IVPASS + 1                                          01390378
           WRITE (NUVI, 80002) IVTNUM                                   01400378
           GO TO 0031                                                   01410378
20030      IVFAIL = IVFAIL + 1                                          01420378
           RVCORR = 0.99985030754498                                    01430378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01440378
 0031      CONTINUE                                                     01450378
CT004*  TEST 4           A POSITIVE REAL NUMBER SUPPLIED AS AN ARGUMENT 01460378
           IVTNUM = 4                                                   01470378
        AVS = TANH(15.125)                                              01480378
           IF (AVS - 0.99995E+00) 20040, 10040, 40040                   01490378
40040      IF (AVS - 0.10001E+01) 10040, 10040, 20040                   01500378
10040      IVPASS = IVPASS + 1                                          01510378
           WRITE (NUVI, 80002) IVTNUM                                   01520378
           GO TO 0041                                                   01530378
20040      IVFAIL = IVFAIL + 1                                          01540378
           RVCORR = 0.99999999999985                                    01550378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01560378
 0041      CONTINUE                                                     01570378
CT005*  TEST 5                                   TEST WITH LARGE VALUES 01580378
           IVTNUM = 5                                                   01590378
        BVS = 10.0 ** 2                                                 01600378
        AVS = TANH(BVS)                                                 01610378
           IF (AVS - 0.99995E+00) 20050, 10050, 40050                   01620378
40050      IF (AVS - 0.10001E+01) 10050, 10050, 20050                   01630378
10050      IVPASS = IVPASS + 1                                          01640378
           WRITE (NUVI, 80002) IVTNUM                                   01650378
           GO TO 0051                                                   01660378
20050      IVFAIL = IVFAIL + 1                                          01670378
           RVCORR = 1.00000000000000                                    01680378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01690378
 0051      CONTINUE                                                     01700378
CT006*  TEST 6                                   TEST WITH LARGE VALUES 01710378
           IVTNUM = 6                                                   01720378
        BVS = -100.0 * 10.0                                             01730378
        AVS = TANH(BVS)                                                 01740378
           IF (AVS + 0.10001E+01) 20060, 10060, 40060                   01750378
40060      IF (AVS + 0.99995E+00) 10060, 10060, 20060                   01760378
10060      IVPASS = IVPASS + 1                                          01770378
           WRITE (NUVI, 80002) IVTNUM                                   01780378
           GO TO 0061                                                   01790378
20060      IVFAIL = IVFAIL + 1                                          01800378
           RVCORR = -1.00000000000000                                   01810378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01820378
 0061      CONTINUE                                                     01830378
CT007*  TEST 7                            AN ARGUMENT OF HIGH MAGNITUDE 01840378
           IVTNUM = 7                                                   01850378
        BVS = 3.0E+36                                                   01860378
        AVS = TANH(BVS)                                                 01870378
           IF (AVS - 0.99995E+00) 20070, 10070, 40070                   01880378
40070      IF (AVS - 0.10001E+01) 10070, 10070, 20070                   01890378
10070      IVPASS = IVPASS + 1                                          01900378
           WRITE (NUVI, 80002) IVTNUM                                   01910378
           GO TO 0071                                                   01920378
20070      IVFAIL = IVFAIL + 1                                          01930378
           RVCORR = 1.00000000000000                                    01940378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01950378
 0071      CONTINUE                                                     01960378
CT008*  TEST 8                             AN ARGUMENT OF LOW MAGNITUDE 01970378
           IVTNUM = 8                                                   01980378
        BVS = -1.0E-15                                                  01990378
        AVS = TANH(BVS)                                                 02000378
           IF (AVS + 0.10001E-14) 20080, 10080, 40080                   02010378
40080      IF (AVS + 0.99995E-15) 10080, 10080, 20080                   02020378
10080      IVPASS = IVPASS + 1                                          02030378
           WRITE (NUVI, 80002) IVTNUM                                   02040378
           GO TO 0081                                                   02050378
20080      IVFAIL = IVFAIL + 1                                          02060378
           RVCORR = -1.00000000000000E-15                               02070378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02080378
 0081      CONTINUE                                                     02090378
CT009*  TEST 9                               THE FUNCTION APPLIED TWICE 02100378
           IVTNUM = 9                                                   02110378
        AVS = TANH(0.5) * TANH(0.75)                                    02120378
           IF (AVS - 0.29349E+00) 20090, 10090, 40090                   02130378
40090      IF (AVS - 0.29353E+00) 10090, 10090, 20090                   02140378
10090      IVPASS = IVPASS + 1                                          02150378
           WRITE (NUVI, 80002) IVTNUM                                   02160378
           GO TO 0091                                                   02170378
20090      IVFAIL = IVFAIL + 1                                          02180378
           RVCORR = 0.293513228313886                                   02190378
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02200378
 0091      CONTINUE                                                     02210378
C*****                                                                  02220378
CBB** ********************** BBCSUM0  **********************************02230378
C**** WRITE OUT TEST SUMMARY                                            02240378
C****                                                                   02250378
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02260378
      WRITE (I02, 90004)                                                02270378
      WRITE (I02, 90014)                                                02280378
      WRITE (I02, 90004)                                                02290378
      WRITE (I02, 90020) IVPASS                                         02300378
      WRITE (I02, 90022) IVFAIL                                         02310378
      WRITE (I02, 90024) IVDELE                                         02320378
      WRITE (I02, 90026) IVINSP                                         02330378
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02340378
CBE** ********************** BBCSUM0  **********************************02350378
CBB** ********************** BBCFOOT0 **********************************02360378
C**** WRITE OUT REPORT FOOTINGS                                         02370378
C****                                                                   02380378
      WRITE (I02,90016) ZPROG, ZPROG                                    02390378
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02400378
      WRITE (I02,90019)                                                 02410378
CBE** ********************** BBCFOOT0 **********************************02420378
CBB** ********************** BBCFMT0A **********************************02430378
C**** FORMATS FOR TEST DETAIL LINES                                     02440378
C****                                                                   02450378
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02460378
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02470378
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02480378
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02490378
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02500378
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02510378
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02520378
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02530378
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02540378
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02550378
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02560378
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02570378
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02580378
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02590378
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02600378
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02610378
80050 FORMAT (1H ,48X,A31)                                              02620378
CBE** ********************** BBCFMT0A **********************************02630378
CBB** ********************** BBCFMT0B **********************************02640378
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02650378
C****                                                                   02660378
90002 FORMAT (1H1)                                                      02670378
90004 FORMAT (1H )                                                      02680378
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02690378
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02700378
90008 FORMAT (1H ,21X,A13,A17)                                          02710378
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02720378
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02730378
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02740378
     1       7X,7HREMARKS,24X)                                          02750378
90014 FORMAT (1H ,46H----------------------------------------------,    02760378
     1        33H---------------------------------)                     02770378
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02780378
C****                                                                   02790378
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02800378
C****                                                                   02810378
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02820378
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02830378
     1        A13)                                                      02840378
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02850378
C****                                                                   02860378
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02870378
C****                                                                   02880378
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02890378
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02900378
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02910378
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02920378
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02930378
CBE** ********************** BBCFMT0B **********************************02940378
C*****                                                                  02950378
C*****    END OF TEST SEGMENT 199                                       02960378
      STOP                                                              02970378
      END                                                               02980378
                                                                        02990378
