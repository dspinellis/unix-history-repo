C***********************************************************************00010802
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020802
C*****   FM802               YDABS - (157)                              00030802
C*****                                                                  00040802
C***********************************************************************00050802
C*****  GENERAL PURPOSE                                         ANS REF 00060802
C*****    TEST INTRINSIC FUNCTION DABS (ABSOLUTE VALUE OF        15.3   00070802
C*****    A DOUBLE PRECISION ARGUMENT)                         (TABLE 5)00080802
CBB** ********************** BBCCOMNT **********************************00090802
C****                                                                   00100802
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00110802
C****                          VERSION 2.0                              00120802
C****                                                                   00130802
C****                                                                   00140802
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00150802
C****                   GENERAL SERVICES ADMINISTRATION                 00160802
C****                   FEDERAL SOFTWARE TESTING CENTER                 00170802
C****                   5203 LEESBURG PIKE, SUITE 1100                  00180802
C****                      FALLS CHURCH, VA. 22041                      00190802
C****                                                                   00200802
C****                          (703) 756-6153                           00210802
C****                                                                   00220802
CBE** ********************** BBCCOMNT **********************************00230802
C*****                                                                  00240802
C*****    S P E C I F I C A T I O N S  SEGMENT 157                      00250802
        DOUBLE PRECISION DOAVD, DOBVD, DODVD, DOEVD, DVCORR             00260802
C*****                                                                  00270802
CBB** ********************** BBCINITA **********************************00280802
C**** SPECIFICATION STATEMENTS                                          00290802
C****                                                                   00300802
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00310802
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00320802
CBE** ********************** BBCINITA **********************************00330802
CBB** ********************** BBCINITB **********************************00340802
C**** INITIALIZE SECTION                                                00350802
      DATA  ZVERS,                  ZVERSD,             ZDATE           00360802
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00370802
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00380802
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00390802
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00400802
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00410802
      DATA   REMRKS /'                               '/                 00420802
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00430802
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00440802
C****                                                                   00450802
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00460802
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00470802
CZ03  ZPROG  = 'PROGRAM NAME'                                           00480802
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00550802
      IVPASS = 0                                                        00560802
      IVFAIL = 0                                                        00570802
      IVDELE = 0                                                        00580802
      IVINSP = 0                                                        00590802
      IVTOTL = 0                                                        00600802
      IVTOTN = 0                                                        00610802
      ICZERO = 0                                                        00620802
C                                                                       00630802
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640802
      I01 = 05                                                          00650802
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660802
      I02 = 06                                                          00670802
C                                                                       00680802
      I01 = 5                                                           00690802
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700802
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00710802
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00720802
C                                                                       00730802
      I02 = 6                                                           00740802
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00750802
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00760802
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00770802
C                                                                       00780802
CBE** ********************** BBCINITB **********************************00790802
      NUVI = I02                                                        00800802
      IVTOTL = 6                                                        00810802
      ZPROG = 'FM802'                                                   00820802
CBB** ********************** BBCHED0A **********************************00830802
C****                                                                   00840802
C**** WRITE REPORT TITLE                                                00850802
C****                                                                   00860802
      WRITE (I02, 90002)                                                00870802
      WRITE (I02, 90006)                                                00880802
      WRITE (I02, 90007)                                                00890802
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00900802
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00910802
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00920802
CBE** ********************** BBCHED0A **********************************00930802
C*****                                                                  00940802
C*****    HEADER FOR SEGMENT 157 WRITTEN                                00950802
        WRITE (NUVI,15701)                                              00960802
15701   FORMAT (1H //1X,34HYDABS - (157) INTRINSIC FUNCTION--//16X,     00970802
     1         23HDABS (ABSOLUTE VALUE ) //  2X,                        00980802
     2         15HANS REF. - 15.3)                                      00990802
CBB** ********************** BBCHED0B **********************************01000802
C**** WRITE DETAIL REPORT HEADERS                                       01010802
C****                                                                   01020802
      WRITE (I02,90004)                                                 01030802
      WRITE (I02,90004)                                                 01040802
      WRITE (I02,90013)                                                 01050802
      WRITE (I02,90014)                                                 01060802
      WRITE (I02,90015) IVTOTL                                          01070802
CBE** ********************** BBCHED0B **********************************01080802
C*****                                                                  01090802
CT001*  TEST 1                                         THE VALUE ZERO   01100802
           IVTNUM = 1                                                   01110802
        DOBVD = 0.0D0                                                   01120802
        DOAVD = DABS(DOBVD)                                             01130802
           IF (DOAVD + 5.0D-10) 20010, 10010, 40010                     01140802
40010      IF (DOAVD - 5.0D-10) 10010, 10010, 20010                     01150802
10010      IVPASS = IVPASS + 1                                          01160802
           WRITE (NUVI, 80002) IVTNUM                                   01170802
           GO TO 0011                                                   01180802
20010      IVFAIL = IVFAIL + 1                                          01190802
           DVCORR = 0.0D0                                               01200802
           WRITE(NUVI, 80031) IVTNUM, DOAVD, DVCORR                     01210802
 0011      CONTINUE                                                     01220802
CT002*  TEST 2                          ZERO PREFIXED WITH A MINUS SIGN 01230802
           IVTNUM = 2                                                   01240802
        DOBVD = 0.0D0                                                   01250802
        DOAVD = DABS(-DOBVD)                                            01260802
           IF (DOAVD + 5.0D-10) 20020, 10020, 40020                     01270802
40020      IF (DOAVD - 5.0D-10) 10020, 10020, 20020                     01280802
10020      IVPASS = IVPASS + 1                                          01290802
           WRITE (NUVI, 80002) IVTNUM                                   01300802
           GO TO 0021                                                   01310802
20020      IVFAIL = IVFAIL + 1                                          01320802
           DVCORR = 0.0D1                                               01330802
           WRITE(NUVI, 80031) IVTNUM, DOAVD, DVCORR                     01340802
 0021      CONTINUE                                                     01350802
CT003*  TEST 3                            A POSITIVE NON-INTEGRAL VALUE 01360802
           IVTNUM = 3                                                   01370802
        DOBVD = 0.35875D2                                               01380802
        DOAVD = DABS(DOBVD)                                             01390802
           IF (DOAVD - 0.3587499998D2) 20030, 10030, 40030              01400802
40030      IF (DOAVD - 0.3587500002D2) 10030, 10030, 20030              01410802
10030      IVPASS = IVPASS + 1                                          01420802
           WRITE (NUVI, 80002) IVTNUM                                   01430802
           GO TO 0031                                                   01440802
20030      IVFAIL = IVFAIL + 1                                          01450802
           DVCORR = 0.35875D2                                           01460802
           WRITE(NUVI, 80031) IVTNUM, DOAVD, DVCORR                     01470802
 0031      CONTINUE                                                     01480802
CT004*  TEST 4                            A NEGATIVE NON-INTEGRAL VALUE 01490802
           IVTNUM = 4                                                   01500802
        DOBVD = -0.35875D2                                              01510802
        DOAVD = DABS(DOBVD)                                             01520802
           IF (DOAVD - 0.3587499998D2) 20040, 10040, 40040              01530802
40040      IF (DOAVD - 0.3587500002D2) 10040, 10040, 20040              01540802
10040      IVPASS = IVPASS + 1                                          01550802
           WRITE (NUVI, 80002) IVTNUM                                   01560802
           GO TO 0041                                                   01570802
20040      IVFAIL = IVFAIL + 1                                          01580802
           DVCORR = 0.35875D2                                           01590802
           WRITE(NUVI, 80031) IVTNUM, DOAVD, DVCORR                     01600802
 0041      CONTINUE                                                     01610802
CT005*  TEST 5                                A POSITIVE INTEGRAL VALUE 01620802
           IVTNUM = 5                                                   01630802
        DOBVD = 7.0D1                                                   01640802
        DOAVD = DABS(DOBVD)                                             01650802
           IF (DOAVD - 6.999999996D1) 20050, 10050, 40050               01660802
40050      IF (DOAVD - 7.000000004D1) 10050, 10050, 20050               01670802
10050      IVPASS = IVPASS + 1                                          01680802
           WRITE (NUVI, 80002) IVTNUM                                   01690802
           GO TO 0051                                                   01700802
20050      IVFAIL = IVFAIL + 1                                          01710802
           DVCORR = 7.0D1                                               01720802
           WRITE(NUVI, 80031) IVTNUM, DOAVD, DVCORR                     01730802
 0051      CONTINUE                                                     01740802
CT006*  TEST 6              ARITHMETIC EXPRESSION PRESENTED TO FUNCTION 01750802
           IVTNUM = 6                                                   01760802
        DODVD = 2.625D0                                                 01770802
        DOEVD = 3.0D0                                                   01780802
        DOAVD = DABS((-DODVD) - DOEVD ** 3)                             01790802
           IF (DOAVD - 29.62499998D0) 20060, 10060, 40060               01800802
40060      IF (DOAVD - 29.62500002D0) 10060, 10060, 20060               01810802
10060      IVPASS = IVPASS + 1                                          01820802
           WRITE (NUVI, 80002) IVTNUM                                   01830802
           GO TO 0061                                                   01840802
20060      IVFAIL = IVFAIL + 1                                          01850802
           DVCORR = 29.625D0                                            01860802
           WRITE(NUVI, 80031) IVTNUM, DOAVD, DVCORR                     01870802
 0061      CONTINUE                                                     01880802
C*****                                                                  01890802
CBB** ********************** BBCSUM0  **********************************01900802
C**** WRITE OUT TEST SUMMARY                                            01910802
C****                                                                   01920802
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01930802
      WRITE (I02, 90004)                                                01940802
      WRITE (I02, 90014)                                                01950802
      WRITE (I02, 90004)                                                01960802
      WRITE (I02, 90020) IVPASS                                         01970802
      WRITE (I02, 90022) IVFAIL                                         01980802
      WRITE (I02, 90024) IVDELE                                         01990802
      WRITE (I02, 90026) IVINSP                                         02000802
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02010802
CBE** ********************** BBCSUM0  **********************************02020802
CBB** ********************** BBCFOOT0 **********************************02030802
C**** WRITE OUT REPORT FOOTINGS                                         02040802
C****                                                                   02050802
      WRITE (I02,90016) ZPROG, ZPROG                                    02060802
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02070802
      WRITE (I02,90019)                                                 02080802
CBE** ********************** BBCFOOT0 **********************************02090802
CBB** ********************** BBCFMT0A **********************************02100802
C**** FORMATS FOR TEST DETAIL LINES                                     02110802
C****                                                                   02120802
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02130802
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02140802
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02150802
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02160802
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02170802
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02180802
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02190802
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02200802
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02210802
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02220802
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02230802
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02240802
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02250802
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02260802
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02270802
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02280802
80050 FORMAT (1H ,48X,A31)                                              02290802
CBE** ********************** BBCFMT0A **********************************02300802
CBB** ********************** BBCFMAT1 **********************************02310802
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02320802
C****                                                                   02330802
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02340802
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02350802
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02360802
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02370802
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02380802
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02390802
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02400802
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02410802
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02420802
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02430802
     21H(,F12.5,2H, ,F12.5,1H))                                         02440802
CBE** ********************** BBCFMAT1 **********************************02450802
CBB** ********************** BBCFMT0B **********************************02460802
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02470802
C****                                                                   02480802
90002 FORMAT (1H1)                                                      02490802
90004 FORMAT (1H )                                                      02500802
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02510802
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02520802
90008 FORMAT (1H ,21X,A13,A17)                                          02530802
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02540802
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02550802
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02560802
     1       7X,7HREMARKS,24X)                                          02570802
90014 FORMAT (1H ,46H----------------------------------------------,    02580802
     1        33H---------------------------------)                     02590802
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02600802
C****                                                                   02610802
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02620802
C****                                                                   02630802
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02640802
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02650802
     1        A13)                                                      02660802
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02670802
C****                                                                   02680802
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02690802
C****                                                                   02700802
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02710802
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02720802
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02730802
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02740802
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02750802
CBE** ********************** BBCFMT0B **********************************02760802
C*****                                                                  02770802
C*****    END OF TEST SEGMENT 157                                       02780802
        STOP                                                            02790802
        END                                                             02800802
                                                                        02810802
