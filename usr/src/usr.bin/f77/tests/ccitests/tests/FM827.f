C***********************************************************************00010827
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020827
C*****   FM827                                                          00030827
C*****                       YDFOR - (202)                              00040827
C*****                                                                  00050827
C***********************************************************************00060827
C*****  GENERAL PURPOSE                                         ANS REF 00070827
C*****    TEST DOUBLE PRECISION TRIGONOMETRIC FORMULA            15.3   00080827
C*****                                                          TABLE 5 00090827
CBB** ********************** BBCCOMNT **********************************00100827
C****                                                                   00110827
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120827
C****                          VERSION 2.0                              00130827
C****                                                                   00140827
C****                                                                   00150827
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160827
C****                   GENERAL SERVICES ADMINISTRATION                 00170827
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180827
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190827
C****                      FALLS CHURCH, VA. 22041                      00200827
C****                                                                   00210827
C****                          (703) 756-6153                           00220827
C****                                                                   00230827
CBE** ********************** BBCCOMNT **********************************00240827
C*****                                                                  00250827
C*****  S P E C I F I C A T I O N S  SEGMENT 202                        00260827
        DOUBLE PRECISION AVD, BVD, CVD, DVD, PIVD, DVCORR               00270827
C*****                                                                  00280827
CBB** ********************** BBCINITA **********************************00290827
C**** SPECIFICATION STATEMENTS                                          00300827
C****                                                                   00310827
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320827
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330827
CBE** ********************** BBCINITA **********************************00340827
CBB** ********************** BBCINITB **********************************00350827
C**** INITIALIZE SECTION                                                00360827
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370827
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380827
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390827
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400827
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410827
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420827
      DATA   REMRKS /'                               '/                 00430827
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440827
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450827
C****                                                                   00460827
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470827
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480827
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490827
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560827
      IVPASS = 0                                                        00570827
      IVFAIL = 0                                                        00580827
      IVDELE = 0                                                        00590827
      IVINSP = 0                                                        00600827
      IVTOTL = 0                                                        00610827
      IVTOTN = 0                                                        00620827
      ICZERO = 0                                                        00630827
C                                                                       00640827
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650827
      I01 = 05                                                          00660827
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670827
      I02 = 06                                                          00680827
C                                                                       00690827
      I01 = 5                                                           00700827
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710827
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720827
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730827
C                                                                       00740827
      I02 = 6                                                           00750827
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760827
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770827
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780827
C                                                                       00790827
CBE** ********************** BBCINITB **********************************00800827
      NUVI = I02                                                        00810827
      IVTOTL = 10                                                       00820827
      ZPROG = 'FM827'                                                   00830827
CBB** ********************** BBCHED0A **********************************00840827
C****                                                                   00850827
C**** WRITE REPORT TITLE                                                00860827
C****                                                                   00870827
      WRITE (I02, 90002)                                                00880827
      WRITE (I02, 90006)                                                00890827
      WRITE (I02, 90007)                                                00900827
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910827
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920827
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930827
CBE** ********************** BBCHED0A **********************************00940827
C*****                                                                  00950827
C*****    HEADER FOR SEGMENT 202                                        00960827
        WRITE(NUVI,20200)                                               00970827
20200   FORMAT(1H , / 35H  YDFOR - (202) INTRINSIC FUNCTIONS//          00980827
     1         41H  DOUBLE PRECISION TRIGONOMETRIC FORMULAE//           00990827
     2         17H  ANS REF. - 15.3)                                    01000827
CBB** ********************** BBCHED0B **********************************01010827
C**** WRITE DETAIL REPORT HEADERS                                       01020827
C****                                                                   01030827
      WRITE (I02,90004)                                                 01040827
      WRITE (I02,90004)                                                 01050827
      WRITE (I02,90013)                                                 01060827
      WRITE (I02,90014)                                                 01070827
      WRITE (I02,90015) IVTOTL                                          01080827
CBE** ********************** BBCHED0B **********************************01090827
C*****                                                                  01100827
        PIVD = 3.1415926535897932384626434D0                            01110827
C*****                                                                  01120827
CT001*  TEST 1                                           LN(EXP(X)) = X 01130827
           IVTNUM = 1                                                   01140827
        BVD = 17.5D0                                                    01150827
        AVD = DLOG(DEXP(1.75D0)) - BVD / 10.0D0                         01160827
           IF (AVD + 0.5000000000D-09) 20010, 10010, 40010              01170827
40010      IF (AVD - 0.5000000000D-09) 10010, 10010, 20010              01180827
10010      IVPASS = IVPASS + 1                                          01190827
           WRITE (NUVI, 80002) IVTNUM                                   01200827
           GO TO 0011                                                   01210827
20010      IVFAIL = IVFAIL + 1                                          01220827
           DVCORR = 0.0D+00                                             01230827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01240827
 0011      CONTINUE                                                     01250827
CT002*  TEST 2                                      SIN**2 + COS**2 = 1 01260827
           IVTNUM = 2                                                   01270827
        BVD = 10.0D0 / 4.0D0                                            01280827
        CVD = DSIN(BVD) ** 2                                            01290827
        DVD = DCOS(BVD) ** 2                                            01300827
        AVD = CVD + DVD - 1.0D0                                         01310827
           IF (AVD + 0.5000000000D-09) 20020, 10020, 40020              01320827
40020      IF (AVD - 0.5000000000D-09) 10020, 10020, 20020              01330827
10020      IVPASS = IVPASS + 1                                          01340827
           WRITE (NUVI, 80002) IVTNUM                                   01350827
           GO TO 0021                                                   01360827
20020      IVFAIL = IVFAIL + 1                                          01370827
           DVCORR = 0.0D+00                                             01380827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01390827
 0021      CONTINUE                                                     01400827
CT003*  TEST 3                                SIN(2X) = 2*SIN(X)*COS(X) 01410827
           IVTNUM = 3                                                   01420827
        BVD = 8.5D0                                                     01430827
        CVD = BVD * (-0.5D0)                                            01440827
        AVD = (DSIN(-4.25D0) * DCOS(CVD)) * 2.0D0 - DSIN(-8.5D0)        01450827
           IF (AVD + 0.5000000000D-09) 20030, 10030, 40030              01460827
40030      IF (AVD - 0.5000000000D-09) 10030, 10030, 20030              01470827
10030      IVPASS = IVPASS + 1                                          01480827
           WRITE (NUVI, 80002) IVTNUM                                   01490827
           GO TO 0031                                                   01500827
20030      IVFAIL = IVFAIL + 1                                          01510827
           DVCORR = 0.0D+00                                             01520827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01530827
 0031      CONTINUE                                                     01540827
CT004*  TEST 4                             ARCSIN(X) = ARCCOS(1 - X**2) 01550827
           IVTNUM = 4                                                   01560827
        AVD = DASIN(-0.875D0) + DACOS(DSQRT(1.0D0 - (0.875D0) ** 2))    01570827
           IF (AVD + 0.5000000000D-09) 20040, 10040, 40040              01580827
40040      IF (AVD - 0.5000000000D-09) 10040, 10040, 20040              01590827
10040      IVPASS = IVPASS + 1                                          01600827
           WRITE (NUVI, 80002) IVTNUM                                   01610827
           GO TO 0041                                                   01620827
20040      IVFAIL = IVFAIL + 1                                          01630827
           DVCORR = 0.0D+00                                             01640827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01650827
 0041      CONTINUE                                                     01660827
CT005*  TEST 5                       TAN(X)**2 - 1 = -COS(2X)/COS(X)**2 01670827
           IVTNUM = 5                                                   01680827
        BVD = 7.0D0                                                     01690827
        AVD = DCOS(1.75D0) / DCOS(BVD / 8.0D0) ** 2                     01700827
     1        + DTAN(0.875D0) ** 2 - 1.0D0                              01710827
           IF (AVD + 0.5000000000D-09) 20050, 10050, 40050              01720827
40050      IF (AVD - 0.5000000000D-09) 10050, 10050, 20050              01730827
10050      IVPASS = IVPASS + 1                                          01740827
           WRITE (NUVI, 80002) IVTNUM                                   01750827
           GO TO 0051                                                   01760827
20050      IVFAIL = IVFAIL + 1                                          01770827
           DVCORR = 0.0D+00                                             01780827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01790827
 0051      CONTINUE                                                     01800827
CT006*  TEST 6                             ATAN(X/Y) = ATAN2(X,Y),  Y>0 01810827
           IVTNUM = 6                                                   01820827
        BVD = 12.0D0                                                    01830827
        CVD = DATAN2(BVD / 4.0D0, BVD / 3.0D0)                          01840827
        AVD = CVD - DATAN(0.75D0)                                       01850827
           IF (AVD + 0.5000000000D-09) 20060, 10060, 40060              01860827
40060      IF (AVD - 0.5000000000D-09) 10060, 10060, 20060              01870827
10060      IVPASS = IVPASS + 1                                          01880827
           WRITE (NUVI, 80002) IVTNUM                                   01890827
           GO TO 0061                                                   01900827
20060      IVFAIL = IVFAIL + 1                                          01910827
           DVCORR = 0.0D+00                                             01920827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01930827
 0061      CONTINUE                                                     01940827
CT007*  TEST 7                                           SQRT(X)**2 = X 01950827
           IVTNUM = 7                                                   01960827
        AVD = DSQRT(9.125D0) ** 2 - 9.125D0                             01970827
           IF (AVD + 0.5000000000D-09) 20070, 10070, 40070              01980827
40070      IF (AVD - 0.5000000000D-09) 10070, 10070, 20070              01990827
10070      IVPASS = IVPASS + 1                                          02000827
           WRITE (NUVI, 80002) IVTNUM                                   02010827
           GO TO 0071                                                   02020827
20070      IVFAIL = IVFAIL + 1                                          02030827
           DVCORR = 0.0D+00                                             02040827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02050827
 0071      CONTINUE                                                     02060827
CT008*  TEST 8                                LN(X) = LN(10) * LOG10(X) 02070827
           IVTNUM = 8                                                   02080827
        BVD = 62.5D0 / 1000.0D0                                         02090827
        AVD = DLOG10(BVD) * DLOG(10.0D0) - DLOG(0.0625D0)               02100827
           IF (AVD + 0.5000000000D-09) 20080, 10080, 40080              02110827
40080      IF (AVD - 0.5000000000D-09) 10080, 10080, 20080              02120827
10080      IVPASS = IVPASS + 1                                          02130827
           WRITE (NUVI, 80002) IVTNUM                                   02140827
           GO TO 0081                                                   02150827
20080      IVFAIL = IVFAIL + 1                                          02160827
           DVCORR = 0.0D+00                                             02170827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02180827
 0081      CONTINUE                                                     02190827
CT009*  TEST 9                                    COSH**2 - SINH**2 = 1 02200827
           IVTNUM = 9                                                   02210827
        BVD = 0.125D0                                                   02220827
        CVD = DSINH(2.125D0)                                            02230827
        DVD = DCOSH(2.0D0 + BVD)                                        02240827
        AVD = DVD  ** 2 - CVD ** 2 - DCOSH(0.0D0)                       02250827
           IF (AVD + 0.5000000000D-09) 20090, 10090, 40090              02260827
40090      IF (AVD - 0.5000000000D-09) 10090, 10090, 20090              02270827
10090      IVPASS = IVPASS + 1                                          02280827
           WRITE (NUVI, 80002) IVTNUM                                   02290827
           GO TO 0091                                                   02300827
20090      IVFAIL = IVFAIL + 1                                          02310827
           DVCORR = 0.0D+00                                             02320827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02330827
 0091      CONTINUE                                                     02340827
CT010*  TEST 10                             TANH(X) = 1 - 2/(EXP(2X)+1) 02350827
           IVTNUM = 10                                                  02360827
        BVD = 5.0D0                                                     02370827
        CVD = 2.0D0                                                     02380827
        DVD = DLOG10(BVD * CVD) - DSQRT(4.0D0) /                        02390827
     1  (DEXP(2.0D0 * (BVD - CVD)) + DCOS(0.0D0))                       02400827
        AVD = DVD - DTANH(3.0D0)                                        02410827
           IF (AVD + 0.5000000000D-09) 20100, 10100, 40100              02420827
40100      IF (AVD - 0.5000000000D-09) 10100, 10100, 20100              02430827
10100      IVPASS = IVPASS + 1                                          02440827
           WRITE (NUVI, 80002) IVTNUM                                   02450827
           GO TO 0101                                                   02460827
20100      IVFAIL = IVFAIL + 1                                          02470827
           DVCORR = 0.0D+00                                             02480827
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02490827
 0101      CONTINUE                                                     02500827
C*****                                                                  02510827
CBB** ********************** BBCSUM0  **********************************02520827
C**** WRITE OUT TEST SUMMARY                                            02530827
C****                                                                   02540827
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02550827
      WRITE (I02, 90004)                                                02560827
      WRITE (I02, 90014)                                                02570827
      WRITE (I02, 90004)                                                02580827
      WRITE (I02, 90020) IVPASS                                         02590827
      WRITE (I02, 90022) IVFAIL                                         02600827
      WRITE (I02, 90024) IVDELE                                         02610827
      WRITE (I02, 90026) IVINSP                                         02620827
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02630827
CBE** ********************** BBCSUM0  **********************************02640827
CBB** ********************** BBCFOOT0 **********************************02650827
C**** WRITE OUT REPORT FOOTINGS                                         02660827
C****                                                                   02670827
      WRITE (I02,90016) ZPROG, ZPROG                                    02680827
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02690827
      WRITE (I02,90019)                                                 02700827
CBE** ********************** BBCFOOT0 **********************************02710827
CBB** ********************** BBCFMT0A **********************************02720827
C**** FORMATS FOR TEST DETAIL LINES                                     02730827
C****                                                                   02740827
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02750827
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02760827
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02770827
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02780827
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02790827
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02800827
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02810827
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02820827
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02830827
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02840827
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02850827
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02860827
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02870827
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02880827
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02890827
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02900827
80050 FORMAT (1H ,48X,A31)                                              02910827
CBE** ********************** BBCFMT0A **********************************02920827
CBB** ********************** BBCFMAT1 **********************************02930827
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02940827
C****                                                                   02950827
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02960827
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02970827
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02980827
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02990827
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03000827
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03010827
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03020827
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03030827
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03040827
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03050827
     21H(,F12.5,2H, ,F12.5,1H))                                         03060827
CBE** ********************** BBCFMAT1 **********************************03070827
CBB** ********************** BBCFMT0B **********************************03080827
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03090827
C****                                                                   03100827
90002 FORMAT (1H1)                                                      03110827
90004 FORMAT (1H )                                                      03120827
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03130827
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03140827
90008 FORMAT (1H ,21X,A13,A17)                                          03150827
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03160827
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03170827
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03180827
     1       7X,7HREMARKS,24X)                                          03190827
90014 FORMAT (1H ,46H----------------------------------------------,    03200827
     1        33H---------------------------------)                     03210827
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03220827
C****                                                                   03230827
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03240827
C****                                                                   03250827
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03260827
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03270827
     1        A13)                                                      03280827
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03290827
C****                                                                   03300827
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03310827
C****                                                                   03320827
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03330827
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03340827
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03350827
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03360827
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03370827
CBE** ********************** BBCFMT0B **********************************03380827
C*****                                                                  03390827
C*****    END OF TEST SEGMENT 202                                       03400827
      STOP                                                              03410827
      END                                                               03420827
                                                                        03430827
