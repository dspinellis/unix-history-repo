C***********************************************************************00010834
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020834
C*****   FM834                                                          00030834
C*****                       YGEN7 - (212)                              00040834
C*****                                                                  00050834
C***********************************************************************00060834
C*****  GENERAL PURPOSE                                         ANS REF 00070834
C*****      TEST GENERIC FUNCTIONS                               15.3   00080834
C*****          USES GENERIC FUNCTIONS AS ARGUMENTS TO          TABLE 5 00090834
C*****               OTHER GENERIC FUNCTIONS                            00100834
C*****                                                                  00110834
CBB** ********************** BBCCOMNT **********************************00120834
C****                                                                   00130834
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140834
C****                          VERSION 2.0                              00150834
C****                                                                   00160834
C****                                                                   00170834
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180834
C****                   GENERAL SERVICES ADMINISTRATION                 00190834
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200834
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210834
C****                      FALLS CHURCH, VA. 22041                      00220834
C****                                                                   00230834
C****                          (703) 756-6153                           00240834
C****                                                                   00250834
CBE** ********************** BBCCOMNT **********************************00260834
C*****                                                                  00270834
C*****  S P E C I F I C A T I O N S  SEGMENT 212                        00280834
        DOUBLE PRECISION AVD, DVCORR                                    00290834
        COMPLEX AVC, ZVCORR                                             00300834
        REAL R2E(2)                                                     00310834
        EQUIVALENCE (AVC, R2E)                                          00320834
C*****                                                                  00330834
CBB** ********************** BBCINITA **********************************00340834
C**** SPECIFICATION STATEMENTS                                          00350834
C****                                                                   00360834
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00370834
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00380834
CBE** ********************** BBCINITA **********************************00390834
CBB** ********************** BBCINITB **********************************00400834
C**** INITIALIZE SECTION                                                00410834
      DATA  ZVERS,                  ZVERSD,             ZDATE           00420834
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00430834
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00440834
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00450834
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00460834
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00470834
      DATA   REMRKS /'                               '/                 00480834
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00490834
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00500834
C****                                                                   00510834
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00520834
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00530834
CZ03  ZPROG  = 'PROGRAM NAME'                                           00540834
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00610834
      IVPASS = 0                                                        00620834
      IVFAIL = 0                                                        00630834
      IVDELE = 0                                                        00640834
      IVINSP = 0                                                        00650834
      IVTOTL = 0                                                        00660834
      IVTOTN = 0                                                        00670834
      ICZERO = 0                                                        00680834
C                                                                       00690834
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00700834
      I01 = 05                                                          00710834
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00720834
      I02 = 06                                                          00730834
C                                                                       00740834
      I01 = 5                                                           00750834
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00760834
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00770834
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00780834
C                                                                       00790834
      I02 = 6                                                           00800834
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00810834
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00820834
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00830834
C                                                                       00840834
CBE** ********************** BBCINITB **********************************00850834
      NUVI = I02                                                        00860834
      IVTOTL = 7                                                        00870834
      ZPROG = 'FM834'                                                   00880834
CBB** ********************** BBCHED0A **********************************00890834
C****                                                                   00900834
C**** WRITE REPORT TITLE                                                00910834
C****                                                                   00920834
      WRITE (I02, 90002)                                                00930834
      WRITE (I02, 90006)                                                00940834
      WRITE (I02, 90007)                                                00950834
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00960834
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00970834
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00980834
CBE** ********************** BBCHED0A **********************************00990834
C*****                                                                  01000834
C*****    HEADER FOR SEGMENT 212                                        01010834
        WRITE(NUVI,21200)                                               01020834
21200   FORMAT( 1H , /  35H YGEN7 - (212) GENERIC FUNCTIONS --//        01030834
     1          33H  AS ARGUMENTS TO OTHER FUNCTIONS//                  01040834
     2          17H  ANS REF. - 15.3)                                   01050834
CBB** ********************** BBCHED0B **********************************01060834
C**** WRITE DETAIL REPORT HEADERS                                       01070834
C****                                                                   01080834
      WRITE (I02,90004)                                                 01090834
      WRITE (I02,90004)                                                 01100834
      WRITE (I02,90013)                                                 01110834
      WRITE (I02,90014)                                                 01120834
      WRITE (I02,90015) IVTOTL                                          01130834
CBE** ********************** BBCHED0B **********************************01140834
C*****                                                                  01150834
CT001*  TEST 1                        TEST OF ABS AND MIN WITH INTEGERS 01160834
C*****                                                                  01170834
           IVTNUM = 1                                                   01180834
        LVI = 2 - ABS( MIN( -3, -8))                                    01190834
           IF (LVI + 6) 20010, 10010, 20010                             01200834
10010      IVPASS = IVPASS + 1                                          01210834
           WRITE (NUVI, 80002) IVTNUM                                   01220834
           GO TO 0011                                                   01230834
20010      IVFAIL = IVFAIL + 1                                          01240834
           IVCORR = -6                                                  01250834
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01260834
 0011      CONTINUE                                                     01270834
CT002*  TEST 2                      TEST OF MOD AND SIGN WITH INTEGERS  01280834
           IVTNUM = 2                                                   01290834
        LVI = 25 * MOD( SIGN( 14, -2), 3)                               01300834
           IF (LVI + 50) 20020, 10020, 20020                            01310834
10020      IVPASS = IVPASS + 1                                          01320834
           WRITE (NUVI, 80002) IVTNUM                                   01330834
           GO TO 0021                                                   01340834
20020      IVFAIL = IVFAIL + 1                                          01350834
           IVCORR = -50                                                 01360834
           WRITE (NUVI, 80010) IVTNUM, LVI, IVCORR                      01370834
 0021      CONTINUE                                                     01380834
CT003*  TEST 3                          TEST OF COS AND SQRT WITH REALS 01390834
           IVTNUM = 3                                                   01400834
        AVS = 2.0 * COS( 1.25 + SQRT( 3.50))                            01410834
           IF (AVS + 0.19997E+01) 20030, 10030, 40030                   01420834
40030      IF (AVS + 0.19994E+01) 10030, 10030, 20030                   01430834
10030      IVPASS = IVPASS + 1                                          01440834
           WRITE (NUVI, 80002) IVTNUM                                   01450834
           GO TO 0031                                                   01460834
20030      IVFAIL = IVFAIL + 1                                          01470834
           RVCORR = -1.9995689                                          01480834
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01490834
 0031      CONTINUE                                                     01500834
CT004*  TEST 4                    TEST OF MAX, LOG AND LOG10 WITH REALS 01510834
           IVTNUM = 4                                                   01520834
        AVS = MAX( LOG( 274.125), 4.5 * LOG10( 121.75))                 01530834
           IF (AVS - 0.93841E+01) 20040, 10040, 40040                   01540834
40040      IF (AVS - 0.93851E+01) 10040, 10040, 20040                   01550834
10040      IVPASS = IVPASS + 1                                          01560834
           WRITE (NUVI, 80002) IVTNUM                                   01570834
           GO TO 0041                                                   01580834
20040      IVFAIL = IVFAIL + 1                                          01590834
           RVCORR = 9.3846103                                           01600834
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01610834
 0041      CONTINUE                                                     01620834
CT005*  TEST 5                     TEST OF EXP AND MOD WITH DOUBLE PREC 01630834
           IVTNUM = 5                                                   01640834
        AVD = 1.0D0 - EXP(5.25D0 + MOD(76.0D0, 2.5D0))                  01650834
           IF (AVD + 0.5170128250D+03) 20050, 10050, 40050              01660834
40050      IF (AVD + 0.5170128244D+03) 10050, 10050, 20050              01670834
10050      IVPASS = IVPASS + 1                                          01680834
           WRITE (NUVI, 80002) IVTNUM                                   01690834
           GO TO 0051                                                   01700834
20050      IVFAIL = IVFAIL + 1                                          01710834
           DVCORR = -517.01282466834D0                                  01720834
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01730834
 0051      CONTINUE                                                     01740834
CT006*  TEST 6                          TEST OF SINH, ABS, TAN AND ATAN 01750834
           IVTNUM = 6                                                   01760834
        AVD = SINH( ABS( TAN( 3.25D0) - ATAN( 1.1D-1)) - 0.01D0)        01770834
           IF (AVD + 0.9274631705D-02) 20060, 10060, 40060              01780834
40060      IF (AVD + 0.9274631695D-02) 10060, 10060, 20060              01790834
10060      IVPASS = IVPASS + 1                                          01800834
           WRITE (NUVI, 80002) IVTNUM                                   01810834
           GO TO 0061                                                   01820834
20060      IVFAIL = IVFAIL + 1                                          01830834
           DVCORR = -0.92746316996764D-2                                01840834
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01850834
 0061      CONTINUE                                                     01860834
CT007*  TEST 7               TEST OF EXP WITH COMPLEX AND COS WITH REAL 01870834
           IVTNUM = 7                                                   01880834
        AVC = EXP( CMPLX(3.5, COS(0.925))) * CMPLX(1.0, 1.50)           01890834
           IF (R2E(1) + 0.82578E+00) 20070, 40072, 40071                01900834
40071      IF (R2E(1) + 0.82569E+00) 40072, 40072, 20070                01910834
40072      IF (R2E(2) - 0.59691E+02) 20070, 10070, 40070                01920834
40070      IF (R2E(2) - 0.59697E+02) 10070, 10070, 20070                01930834
10070      IVPASS = IVPASS + 1                                          01940834
           WRITE (NUVI, 80002) IVTNUM                                   01950834
           GO TO 0071                                                   01960834
20070      IVFAIL = IVFAIL + 1                                          01970834
           ZVCORR = (-0.8257397, 59.6940191)                            01980834
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01990834
 0071      CONTINUE                                                     02000834
CBB** ********************** BBCSUM0  **********************************02010834
C**** WRITE OUT TEST SUMMARY                                            02020834
C****                                                                   02030834
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02040834
      WRITE (I02, 90004)                                                02050834
      WRITE (I02, 90014)                                                02060834
      WRITE (I02, 90004)                                                02070834
      WRITE (I02, 90020) IVPASS                                         02080834
      WRITE (I02, 90022) IVFAIL                                         02090834
      WRITE (I02, 90024) IVDELE                                         02100834
      WRITE (I02, 90026) IVINSP                                         02110834
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02120834
CBE** ********************** BBCSUM0  **********************************02130834
CBB** ********************** BBCFOOT0 **********************************02140834
C**** WRITE OUT REPORT FOOTINGS                                         02150834
C****                                                                   02160834
      WRITE (I02,90016) ZPROG, ZPROG                                    02170834
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02180834
      WRITE (I02,90019)                                                 02190834
CBE** ********************** BBCFOOT0 **********************************02200834
CBB** ********************** BBCFMT0A **********************************02210834
C**** FORMATS FOR TEST DETAIL LINES                                     02220834
C****                                                                   02230834
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02240834
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02250834
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02260834
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02270834
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02280834
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02290834
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02300834
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02310834
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02320834
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02330834
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02340834
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02350834
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02360834
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02370834
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02380834
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02390834
80050 FORMAT (1H ,48X,A31)                                              02400834
CBE** ********************** BBCFMT0A **********************************02410834
CBB** ********************** BBCFMAT1 **********************************02420834
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02430834
C****                                                                   02440834
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02450834
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02460834
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02470834
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02480834
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02490834
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02500834
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02510834
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02520834
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02530834
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02540834
     21H(,F12.5,2H, ,F12.5,1H))                                         02550834
CBE** ********************** BBCFMAT1 **********************************02560834
CBB** ********************** BBCFMT0B **********************************02570834
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02580834
C****                                                                   02590834
90002 FORMAT (1H1)                                                      02600834
90004 FORMAT (1H )                                                      02610834
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02620834
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02630834
90008 FORMAT (1H ,21X,A13,A17)                                          02640834
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02650834
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02660834
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02670834
     1       7X,7HREMARKS,24X)                                          02680834
90014 FORMAT (1H ,46H----------------------------------------------,    02690834
     1        33H---------------------------------)                     02700834
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02710834
C****                                                                   02720834
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02730834
C****                                                                   02740834
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02750834
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02760834
     1        A13)                                                      02770834
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02780834
C****                                                                   02790834
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02800834
C****                                                                   02810834
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02820834
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02830834
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02840834
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02850834
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02860834
CBE** ********************** BBCFMT0B **********************************02870834
C*****                                                                  02880834
C*****    END OF TEST SEGMENT 212                                       02890834
      STOP                                                              02900834
      END                                                               02910834
