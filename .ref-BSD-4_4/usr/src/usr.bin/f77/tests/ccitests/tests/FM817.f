C***********************************************************************00010817
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020817
C*****   FM817                                                          00030817
C*****                       YCLOG - (183)                              00040817
C*****                                                                  00050817
C***********************************************************************00060817
C*****  GENERAL PURPOSE                                         ANS REF 00070817
C*****    TEST INTRINSIC FUNCTION CLOG                           15.3   00080817
C*****    INTRINSIC FUNCTIONS AIMAG AND CMPLX ASSUMED WORKING   TABLE 5 00090817
C*****                                                                  00100817
CBB** ********************** BBCCOMNT **********************************00110817
C****                                                                   00120817
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130817
C****                          VERSION 2.0                              00140817
C****                                                                   00150817
C****                                                                   00160817
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170817
C****                   GENERAL SERVICES ADMINISTRATION                 00180817
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190817
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200817
C****                      FALLS CHURCH, VA. 22041                      00210817
C****                                                                   00220817
C****                          (703) 756-6153                           00230817
C****                                                                   00240817
CBE** ********************** BBCCOMNT **********************************00250817
C*****                                                                  00260817
C*****  S P E C I F I C A T I O N S  SEGMENT 183                        00270817
        COMPLEX AVC, BVC, CVC, ZVCORR                                   00280817
        REAL R2E(2)                                                     00290817
        EQUIVALENCE (AVC, R2E)                                          00300817
C*****                                                                  00310817
CBB** ********************** BBCINITA **********************************00320817
C**** SPECIFICATION STATEMENTS                                          00330817
C****                                                                   00340817
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350817
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360817
CBE** ********************** BBCINITA **********************************00370817
CBB** ********************** BBCINITB **********************************00380817
C**** INITIALIZE SECTION                                                00390817
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400817
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410817
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420817
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430817
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440817
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450817
      DATA   REMRKS /'                               '/                 00460817
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470817
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480817
C****                                                                   00490817
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500817
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510817
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520817
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590817
      IVPASS = 0                                                        00600817
      IVFAIL = 0                                                        00610817
      IVDELE = 0                                                        00620817
      IVINSP = 0                                                        00630817
      IVTOTL = 0                                                        00640817
      IVTOTN = 0                                                        00650817
      ICZERO = 0                                                        00660817
C                                                                       00670817
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680817
      I01 = 05                                                          00690817
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700817
      I02 = 06                                                          00710817
C                                                                       00720817
      I01 = 5                                                           00730817
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740817
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750817
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760817
C                                                                       00770817
      I02 = 6                                                           00780817
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790817
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800817
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810817
C                                                                       00820817
CBE** ********************** BBCINITB **********************************00830817
      NUVI = I02                                                        00840817
      IVTOTL = 11                                                       00850817
      ZPROG = 'FM817'                                                   00860817
CBB** ********************** BBCHED0A **********************************00870817
C****                                                                   00880817
C**** WRITE REPORT TITLE                                                00890817
C****                                                                   00900817
      WRITE (I02, 90002)                                                00910817
      WRITE (I02, 90006)                                                00920817
      WRITE (I02, 90007)                                                00930817
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940817
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950817
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960817
CBE** ********************** BBCHED0A **********************************00970817
C*****                                                                  00980817
C*****    HEADER FOR SEGMENT 183                                        00990817
        WRITE(NUVI,18300)                                               01000817
18300   FORMAT(1H , / 35H  YCLOG - (183) INTRINSIC FUNCTIONS//          01010817
     1         34H  CLOG (COMPLEX NATURAL LOGARITHM)//                  01020817
     2         17H  ANS REF. - 15.3)                                    01030817
CBB** ********************** BBCHED0B **********************************01040817
C**** WRITE DETAIL REPORT HEADERS                                       01050817
C****                                                                   01060817
      WRITE (I02,90004)                                                 01070817
      WRITE (I02,90004)                                                 01080817
      WRITE (I02,90013)                                                 01090817
      WRITE (I02,90014)                                                 01100817
      WRITE (I02,90015) IVTOTL                                          01110817
CBE** ********************** BBCHED0B **********************************01120817
C*****                                                                  01130817
        PIVS = 3.1415926535897932384626434                              01140817
C*****    TESTS 1 THRU 3 - POSITIVE REAL NUMBERS--CLOG, ALOG AGREE ON   01150817
C*****                     REAL LINE                                    01160817
CT001*  TEST 1                                                          01170817
           IVTNUM = 1                                                   01180817
        AVC = CLOG((1.0, 0.0))                                          01190817
           IF (R2E(1) + 0.50000E-04) 20010, 40012, 40011                01200817
40011      IF (R2E(1) - 0.50000E-04) 40012, 40012, 20010                01210817
40012      IF (R2E(2) + 0.50000E-04) 20010, 10010, 40010                01220817
40010      IF (R2E(2) - 0.50000E-04) 10010, 10010, 20010                01230817
10010      IVPASS = IVPASS + 1                                          01240817
           WRITE (NUVI, 80002) IVTNUM                                   01250817
           GO TO 0011                                                   01260817
20010      IVFAIL = IVFAIL + 1                                          01270817
           ZVCORR = (0.00000000000000, 0.00000000000000)                01280817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01290817
 0011      CONTINUE                                                     01300817
CT002*  TEST 2                                                          01310817
           IVTNUM = 2                                                   01320817
        AVC = CLOG((5.125, 0.0))                                        01330817
           IF (R2E(1) - 0.16340E+01) 20020, 40022, 40021                01340817
40021      IF (R2E(1) - 0.16343E+01) 40022, 40022, 20020                01350817
40022      IF (R2E(2) + 0.50000E-04) 20020, 10020, 40020                01360817
40020      IF (R2E(2) - 0.50000E-04) 10020, 10020, 20020                01370817
10020      IVPASS = IVPASS + 1                                          01380817
           WRITE (NUVI, 80002) IVTNUM                                   01390817
           GO TO 0021                                                   01400817
20020      IVFAIL = IVFAIL + 1                                          01410817
           ZVCORR = (1.6341305250245, 0.00000000000000)                 01420817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01430817
 0021      CONTINUE                                                     01440817
CT003*  TEST 3                                                          01450817
           IVTNUM = 3                                                   01460817
        AVC = CLOG((100.0, 0.0))                                        01470817
           IF (R2E(1) - 0.46049E+01) 20030, 40032, 40031                01480817
40031      IF (R2E(1) - 0.46054E+01) 40032, 40032, 20030                01490817
40032      IF (R2E(2) + 0.50000E-04) 20030, 10030, 40030                01500817
40030      IF (R2E(2) - 0.50000E-04) 10030, 10030, 20030                01510817
10030      IVPASS = IVPASS + 1                                          01520817
           WRITE (NUVI, 80002) IVTNUM                                   01530817
           GO TO 0031                                                   01540817
20030      IVFAIL = IVFAIL + 1                                          01550817
           ZVCORR = (4.6051701859881, 0.00000000000000)                 01560817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01570817
 0031      CONTINUE                                                     01580817
CT004*  TEST 4                          AN EXPRESSION PRESENTED TO CLOG 01590817
           IVTNUM = 4                                                   01600817
        AVC = CLOG((2.6875, 0.0) * (-1.0, 0.0))                         01610817
           IF (R2E(1) - 0.98856E+00) 20040, 40042, 40041                01620817
40041      IF (R2E(1) - 0.98866E+00) 40042, 40042, 20040                01630817
40042      IF (R2E(2) - 0.31414E+01) 20040, 10040, 40040                01640817
40040      IF (R2E(2) - 0.31418E+01) 10040, 10040, 20040                01650817
10040      IVPASS = IVPASS + 1                                          01660817
           WRITE (NUVI, 80002) IVTNUM                                   01670817
           GO TO 0041                                                   01680817
20040      IVFAIL = IVFAIL + 1                                          01690817
           ZVCORR = (0.98861139345378, 3.1415926535898)                 01700817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01710817
 0041      CONTINUE                                                     01720817
C*****    TESTS 5 AND 6 - NEGATIVE REAL NUMBERS--CHECK RIGHT BRANCH AT  01730817
C*****                    ENDPOINTS                                     01740817
CT005*  TEST 5                                                          01750817
           IVTNUM = 5                                                   01760817
        BVC = (-2.5, 0.0)                                               01770817
        AVC = CLOG(BVC + BVC)                                           01780817
           IF (R2E(1) - 0.16093E+01) 20050, 40052, 40051                01790817
40051      IF (R2E(1) - 0.16096E+01) 40052, 40052, 20050                01800817
40052      IF (R2E(2) - 0.31414E+01) 20050, 10050, 40050                01810817
40050      IF (R2E(2) - 0.31418E+01) 10050, 10050, 20050                01820817
10050      IVPASS = IVPASS + 1                                          01830817
           WRITE (NUVI, 80002) IVTNUM                                   01840817
           GO TO 0051                                                   01850817
20050      IVFAIL = IVFAIL + 1                                          01860817
           ZVCORR = (1.6094379124341, 3.1415926535898)                  01870817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01880817
 0051      CONTINUE                                                     01890817
CT006*  TEST 6                                                          01900817
           IVTNUM = 6                                                   01910817
        BVC = (-10.0, 0.0) + (-10.25, 0.0)                              01920817
        AVC = CLOG(BVC)                                                 01930817
           IF (R2E(1) - 0.30080E+01) 20060, 40062, 40061                01940817
40061      IF (R2E(1) - 0.30083E+01) 40062, 40062, 20060                01950817
40062      IF (R2E(2) - 0.31414E+01) 20060, 10060, 40060                01960817
40060      IF (R2E(2) - 0.31418E+01) 10060, 10060, 20060                01970817
10060      IVPASS = IVPASS + 1                                          01980817
           WRITE (NUVI, 80002) IVTNUM                                   01990817
           GO TO 0061                                                   02000817
20060      IVFAIL = IVFAIL + 1                                          02010817
           ZVCORR = (3.0081547935525, 3.1415926535898)                  02020817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02030817
 0061      CONTINUE                                                     02040817
CT007*  TEST 7              POSITIVE REAL, POSITIVE IMAGINARY ARGUMENTS 02050817
           IVTNUM = 7                                                   02060817
        BVC = (2.0, 1.5)                                                02070817
        AVC = CLOG(BVC)                                                 02080817
           IF (R2E(1) - 0.91624E+00) 20070, 40072, 40071                02090817
40071      IF (R2E(1) - 0.91634E+00) 40072, 40072, 20070                02100817
40072      IF (R2E(2) - 0.64346E+00) 20070, 10070, 40070                02110817
40070      IF (R2E(2) - 0.64354E+00) 10070, 10070, 20070                02120817
10070      IVPASS = IVPASS + 1                                          02130817
           WRITE (NUVI, 80002) IVTNUM                                   02140817
           GO TO 0071                                                   02150817
20070      IVFAIL = IVFAIL + 1                                          02160817
           ZVCORR = (0.91629073187416, 0.64350110879328)                02170817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02180817
 0071      CONTINUE                                                     02190817
CT008*  TEST 8              NEGATIVE REAL, POSITIVE IMAGINARY ARGUMENTS 02200817
           IVTNUM = 8                                                   02210817
        BVC = (-2.75, 1.375)                                            02220817
        AVC = CLOG(BVC)                                                 02230817
           IF (R2E(1) - 0.11231E+01) 20080, 40082, 40081                02240817
40081      IF (R2E(1) - 0.11233E+01) 40082, 40082, 20080                02250817
40082      IF (R2E(2) - 0.26778E+01) 20080, 10080, 40080                02260817
40080      IF (R2E(2) - 0.26781E+01) 10080, 10080, 20080                02270817
10080      IVPASS = IVPASS + 1                                          02280817
           WRITE (NUVI, 80002) IVTNUM                                   02290817
           GO TO 0081                                                   02300817
20080      IVFAIL = IVFAIL + 1                                          02310817
           ZVCORR = (1.1231726873356, 2.6779450445890)                  02320817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02330817
 0081      CONTINUE                                                     02340817
CT009*  TEST 9              NEGATIVE REAL, NEGATIVE IMAGINARY ARGUMENTS 02350817
           IVTNUM = 9                                                   02360817
        BVC = (-10.0, -10.0)                                            02370817
        AVC = CLOG(BVC)                                                 02380817
           IF (R2E(1) - 0.26490E+01) 20090, 40092, 40091                02390817
40091      IF (R2E(1) - 0.26493E+01) 40092, 40092, 20090                02400817
40092      IF (R2E(2) + 0.23564E+01) 20090, 10090, 40090                02410817
40090      IF (R2E(2) + 0.23560E+01) 10090, 10090, 20090                02420817
10090      IVPASS = IVPASS + 1                                          02430817
           WRITE (NUVI, 80002) IVTNUM                                   02440817
           GO TO 0091                                                   02450817
20090      IVFAIL = IVFAIL + 1                                          02460817
           ZVCORR = (2.6491586832740, -2.3561944901923)                 02470817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02480817
 0091      CONTINUE                                                     02490817
CT010*  TEST 10                           CLOG USED TOGETHER WITH AIMAG 02500817
           IVTNUM = 10                                                  02510817
        AVS = (AIMAG(CLOG((3.0, 1.75))) + AIMAG(CLOG((-3.0, 1.75))))    02520817
     1        - PIVS                                                    02530817
           IF (AVS + 0.50000E-04) 20100, 10100, 40100                   02540817
40100      IF (AVS - 0.50000E-04) 10100, 10100, 20100                   02550817
10100      IVPASS = IVPASS + 1                                          02560817
           WRITE (NUVI, 80002) IVTNUM                                   02570817
           GO TO 0101                                                   02580817
20100      IVFAIL = IVFAIL + 1                                          02590817
           RVCORR = 0.00000000000000                                    02600817
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02610817
 0101      CONTINUE                                                     02620817
CT011*  TEST 11                 CLOG USED TOGETHER WITH CMPLX AND AIMAG 02630817
           IVTNUM = 11                                                  02640817
        BVC = CLOG((4.5, -3.75))                                        02650817
        CVC = CLOG((-4.5, -3.75))                                       02660817
        AVC = (BVC - CMPLX(0.0, AIMAG(BVC))) -                          02670817
     1        (CVC - CMPLX(0.0, AIMAG(CVC)))                            02680817
           IF (R2E(1) + 0.50000E-04) 20110, 40112, 40111                02690817
40111      IF (R2E(1) - 0.50000E-04) 40112, 40112, 20110                02700817
40112      IF (R2E(2) + 0.50000E-04) 20110, 10110, 40110                02710817
40110      IF (R2E(2) - 0.50000E-04) 10110, 10110, 20110                02720817
10110      IVPASS = IVPASS + 1                                          02730817
           WRITE (NUVI, 80002) IVTNUM                                   02740817
           GO TO 0111                                                   02750817
20110      IVFAIL = IVFAIL + 1                                          02760817
           ZVCORR = (0.00000000000000, 0.00000000000000)                02770817
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02780817
 0111      CONTINUE                                                     02790817
C*****                                                                  02800817
CBB** ********************** BBCSUM0  **********************************02810817
C**** WRITE OUT TEST SUMMARY                                            02820817
C****                                                                   02830817
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02840817
      WRITE (I02, 90004)                                                02850817
      WRITE (I02, 90014)                                                02860817
      WRITE (I02, 90004)                                                02870817
      WRITE (I02, 90020) IVPASS                                         02880817
      WRITE (I02, 90022) IVFAIL                                         02890817
      WRITE (I02, 90024) IVDELE                                         02900817
      WRITE (I02, 90026) IVINSP                                         02910817
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02920817
CBE** ********************** BBCSUM0  **********************************02930817
CBB** ********************** BBCFOOT0 **********************************02940817
C**** WRITE OUT REPORT FOOTINGS                                         02950817
C****                                                                   02960817
      WRITE (I02,90016) ZPROG, ZPROG                                    02970817
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02980817
      WRITE (I02,90019)                                                 02990817
CBE** ********************** BBCFOOT0 **********************************03000817
CBB** ********************** BBCFMT0A **********************************03010817
C**** FORMATS FOR TEST DETAIL LINES                                     03020817
C****                                                                   03030817
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03040817
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03050817
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03060817
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03070817
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03080817
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03090817
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03100817
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03110817
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03120817
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03130817
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03140817
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03150817
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03160817
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03170817
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03180817
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03190817
80050 FORMAT (1H ,48X,A31)                                              03200817
CBE** ********************** BBCFMT0A **********************************03210817
CBB** ********************** BBCFMAT1 **********************************03220817
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03230817
C****                                                                   03240817
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03250817
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03260817
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03270817
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03280817
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03290817
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03300817
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03310817
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03320817
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03330817
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03340817
     21H(,F12.5,2H, ,F12.5,1H))                                         03350817
CBE** ********************** BBCFMAT1 **********************************03360817
CBB** ********************** BBCFMT0B **********************************03370817
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03380817
C****                                                                   03390817
90002 FORMAT (1H1)                                                      03400817
90004 FORMAT (1H )                                                      03410817
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03420817
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03430817
90008 FORMAT (1H ,21X,A13,A17)                                          03440817
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03450817
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03460817
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03470817
     1       7X,7HREMARKS,24X)                                          03480817
90014 FORMAT (1H ,46H----------------------------------------------,    03490817
     1        33H---------------------------------)                     03500817
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03510817
C****                                                                   03520817
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03530817
C****                                                                   03540817
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03550817
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03560817
     1        A13)                                                      03570817
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03580817
C****                                                                   03590817
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03600817
C****                                                                   03610817
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03620817
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03630817
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03640817
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03650817
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03660817
CBE** ********************** BBCFMT0B **********************************03670817
C*****                                                                  03680817
C*****    END OF TEST SEGMENT 183                                       03690817
      STOP                                                              03700817
      END                                                               03710817
                                                                        03720817
