C***********************************************************************00010811
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020811
C*****   FM811               YCMMX - (174)                              00030811
C*****                                                                  00040811
C***********************************************************************00050811
C*****  GENERAL PURPOSE                                         ANS REF 00060811
C*****    TESTS THE USE OF INTEGER, REAL, DOUBLE PRECISION,      15.10  00070811
C*****    AND COMPLEX EXPRESSIONS CONTAINING REFERENCE         (TABLE 5)00080811
C*****    TO THE INTRINSIC FUNCTIONS OF THE FULL LANGUAGE        6.1.4  00090811
C*****                                                                  00100811
C*****  GENERAL COMMENTS                                                00110811
C*****    SEGMENTS TESTING XINT, XREAL, XAINT, XABS, XAMOD,             00120811
C*****    XSIGN, XDIM, XMAX, XMIN, YIDINT, YSNGL                        00130811
C*****    YDINT, YDABS, YCABS, YDMOD, YDSIGN,                           00140811
C*****    YDMAX1, YDMIN1, YDBLE, YCONJG ASSUMED WORKING                 00150811
C*****                                                                  00160811
CBB** ********************** BBCCOMNT **********************************00170811
C****                                                                   00180811
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00190811
C****                          VERSION 2.0                              00200811
C****                                                                   00210811
C****                                                                   00220811
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00230811
C****                   GENERAL SERVICES ADMINISTRATION                 00240811
C****                   FEDERAL SOFTWARE TESTING CENTER                 00250811
C****                   5203 LEESBURG PIKE, SUITE 1100                  00260811
C****                      FALLS CHURCH, VA. 22041                      00270811
C****                                                                   00280811
C****                          (703) 756-6153                           00290811
C****                                                                   00300811
CBE** ********************** BBCCOMNT **********************************00310811
C*****                                                                  00320811
C*****    S P E C I F I C A T I O N S  SEGMENT 174                      00330811
        DOUBLE PRECISION DYAVD, DYBVD, DYDVD, DVCORR                    00340811
        COMPLEX CYAVC, CYDVC, ZVCORR                                    00350811
        REAL R2E(2)                                                     00360811
        EQUIVALENCE (CYAVC,R2E)                                         00370811
C*****                                                                  00380811
CBB** ********************** BBCINITA **********************************00390811
C**** SPECIFICATION STATEMENTS                                          00400811
C****                                                                   00410811
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00420811
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00430811
CBE** ********************** BBCINITA **********************************00440811
CBB** ********************** BBCINITB **********************************00450811
C**** INITIALIZE SECTION                                                00460811
      DATA  ZVERS,                  ZVERSD,             ZDATE           00470811
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00480811
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00490811
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00500811
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00510811
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00520811
      DATA   REMRKS /'                               '/                 00530811
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00540811
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00550811
C****                                                                   00560811
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00570811
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00580811
CZ03  ZPROG  = 'PROGRAM NAME'                                           00590811
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00660811
      IVPASS = 0                                                        00670811
      IVFAIL = 0                                                        00680811
      IVDELE = 0                                                        00690811
      IVINSP = 0                                                        00700811
      IVTOTL = 0                                                        00710811
      IVTOTN = 0                                                        00720811
      ICZERO = 0                                                        00730811
C                                                                       00740811
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00750811
      I01 = 05                                                          00760811
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00770811
      I02 = 06                                                          00780811
C                                                                       00790811
      I01 = 5                                                           00800811
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00810811
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00820811
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00830811
C                                                                       00840811
      I02 = 6                                                           00850811
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00860811
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00870811
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00880811
C                                                                       00890811
CBE** ********************** BBCINITB **********************************00900811
      NUVI = I02                                                        00910811
      IVTOTL = 10                                                       00920811
      ZPROG = 'FM811'                                                   00930811
CBB** ********************** BBCHED0A **********************************00940811
C****                                                                   00950811
C**** WRITE REPORT TITLE                                                00960811
C****                                                                   00970811
      WRITE (I02, 90002)                                                00980811
      WRITE (I02, 90006)                                                00990811
      WRITE (I02, 90007)                                                01000811
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01010811
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01020811
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01030811
CBE** ********************** BBCHED0A **********************************01040811
C*****                                                                  01050811
C*****    HEADER FOR SEGMENT 174 WRITTEN                                01060811
        WRITE (NUVI,17401)                                              01070811
17401   FORMAT(  1H , //1X, 35HYCMMX - (174) INTRINSIC FUNCTIONS--//    01080811
     1         16X, 19HINTEGER, REAL, D.P./                             01090811
     2         16X, 37HAND COMPLEX IN MIXED MODE EXPRESSIONS//          01100811
     3         2X, 16HANS REF. - 15.10)                                 01110811
CBB** ********************** BBCHED0B **********************************01120811
C**** WRITE DETAIL REPORT HEADERS                                       01130811
C****                                                                   01140811
      WRITE (I02,90004)                                                 01150811
      WRITE (I02,90004)                                                 01160811
      WRITE (I02,90013)                                                 01170811
      WRITE (I02,90014)                                                 01180811
      WRITE (I02,90015) IVTOTL                                          01190811
CBE** ********************** BBCHED0B **********************************01200811
C*****                                                                  01210811
CT001*  TEST 1                                                    IDINT 01220811
           IVTNUM = 1                                                   01230811
        DYBVD = 5.2D0                                                   01240811
        CYAVC = IDINT(DYBVD) + (1.0, 2.0)                               01250811
           IF (R2E(1) - 5.9997) 20010, 40012, 40011                     01260811
40011      IF (R2E(1) - 6.0003) 40012, 40012, 20010                     01270811
40012      IF (R2E(2) - 1.9999) 20010, 10010, 40010                     01280811
40010      IF (R2E(2) - 2.0001) 10010, 10010, 20010                     01290811
10010      IVPASS = IVPASS + 1                                          01300811
           WRITE (NUVI, 80002) IVTNUM                                   01310811
           GO TO 0011                                                   01320811
20010      IVFAIL = IVFAIL + 1                                          01330811
           ZVCORR = (6.0, 2.0)                                          01340811
           WRITE (NUVI, 80045) IVTNUM, CYAVC, ZVCORR                    01350811
 0011      CONTINUE                                                     01360811
CT002*  TEST 2                                                     SNGL 01370811
           IVTNUM = 2                                                   01380811
        DYAVD = 5.5D0                                                   01390811
        CYAVC = SNGL(DYAVD) - (3.0, 4.0)                                01400811
           IF (R2E(1) - 2.4998) 20020, 40022, 40021                     01410811
40021      IF (R2E(1) - 2.5002) 40022, 40022, 20020                     01420811
40022      IF (R2E(2) + 4.0002) 20020, 10020, 40020                     01430811
40020      IF (R2E(2) + 3.9998) 10020, 10020, 20020                     01440811
10020      IVPASS = IVPASS + 1                                          01450811
           WRITE (NUVI, 80002) IVTNUM                                   01460811
           GO TO 0021                                                   01470811
20020      IVFAIL = IVFAIL + 1                                          01480811
           ZVCORR = (2.5, -4.0)                                         01490811
           WRITE (NUVI, 80045) IVTNUM, CYAVC, ZVCORR                    01500811
 0021      CONTINUE                                                     01510811
CT003*  TEST 3                                  SNGL, DINT, DNINT, CABS 01520811
           IVTNUM = 3                                                   01530811
        DYBVD = 5.8D0                                                   01540811
        RYAVS = SNGL(DINT(DYBVD) + DNINT(DYBVD)) * CABS((3.0, 4.0))     01550811
           IF (RYAVS - 54.997) 20030, 10030, 40030                      01560811
40030      IF (RYAVS - 55.003) 10030, 10030, 20030                      01570811
10030      IVPASS = IVPASS + 1                                          01580811
           WRITE (NUVI, 80002) IVTNUM                                   01590811
           GO TO 0031                                                   01600811
20030      IVFAIL = IVFAIL + 1                                          01610811
           RVCORR = 55.0                                                01620811
           WRITE (NUVI, 80012) IVTNUM, RYAVS, RVCORR                    01630811
 0031      CONTINUE                                                     01640811
CT004*  TEST 4                                            IDNINT, AIMAG 01650811
           IVTNUM = 4                                                   01660811
        CYDVC = (3.0, 4.0)                                              01670811
        DYBVD = 5.8D0                                                   01680811
        CYAVC = ((IDNINT(DYBVD) - CYDVC)) * AIMAG((4.0, 3.0))           01690811
           IF (R2E(1) - 8.9995) 20040, 40042, 40041                     01700811
40041      IF (R2E(1) - 9.0005) 40042, 40042, 20040                     01710811
40042      IF (R2E(2) + 12.001) 20040, 10040, 40040                     01720811
40040      IF (R2E(2) + 11.999) 10040, 10040, 20040                     01730811
10040      IVPASS = IVPASS + 1                                          01740811
           WRITE (NUVI, 80002) IVTNUM                                   01750811
           GO TO 0041                                                   01760811
20040      IVFAIL = IVFAIL + 1                                          01770811
           ZVCORR = (9.0, -12.0)                                        01780811
           WRITE (NUVI, 80045) IVTNUM, CYAVC, ZVCORR                    01790811
 0041      CONTINUE                                                     01800811
CT005*  TEST 5                                              CABS, CMPLX 01810811
           IVTNUM = 5                                                   01820811
        IYAVI = 5                                                       01830811
        RYAVS = CABS(CMPLX(3.0, 4.0)) / IYAVI                           01840811
           IF (RYAVS - 0.99995) 20050, 10050, 40050                     01850811
40050      IF (RYAVS - 1.0001) 10050, 10050, 20050                      01860811
10050      IVPASS = IVPASS + 1                                          01870811
           WRITE (NUVI, 80002) IVTNUM                                   01880811
           GO TO 0051                                                   01890811
20050      IVFAIL = IVFAIL + 1                                          01900811
           RVCORR = 1.0                                                 01910811
           WRITE (NUVI, 80012) IVTNUM, RYAVS, RVCORR                    01920811
 0051      CONTINUE                                                     01930811
CT006*  TEST 6                                        CONJG, SNGL, DMOD 01940811
           IVTNUM = 6                                                   01950811
        DYBVD = 5.0D0                                                   01960811
        DYDVD = 3.0D0                                                   01970811
        CYAVC = CONJG((3.0, 4.0)) * SNGL(DMOD(DYBVD, DYDVD))            01980811
           IF (R2E(1) - 5.9997) 20060, 40062, 40061                     01990811
40061      IF (R2E(1) - 6.0003) 40062, 40062, 20060                     02000811
40062      IF (R2E(2) + 8.0004) 20060, 10060, 40060                     02010811
40060      IF (R2E(2) + 7.9996) 10060, 10060, 20060                     02020811
10060      IVPASS = IVPASS + 1                                          02030811
           WRITE (NUVI, 80002) IVTNUM                                   02040811
           GO TO 0061                                                   02050811
20060      IVFAIL = IVFAIL + 1                                          02060811
           ZVCORR = (6.0, -8.0)                                         02070811
           WRITE (NUVI, 80045) IVTNUM, CYAVC, ZVCORR                    02080811
 0061      CONTINUE                                                     02090811
CT007*  TEST 7                                      DSIGN, AIMAG, CONJG 02100811
           IVTNUM = 7                                                   02110811
        CYDVC = (-3.0, -4.0)                                            02120811
        DYBVD = 4.0D0                                                   02130811
        DYDVD = 1.0D0                                                   02140811
        DYAVD = DSIGN(DYBVD, DYDVD) / AIMAG(CONJG(CYDVC))               02150811
           IF (DYAVD - 0.9999999995D0) 20070, 10070, 40070              02160811
40070      IF (DYAVD - 1.000000001D0) 10070, 10070, 20070               02170811
10070      IVPASS = IVPASS + 1                                          02180811
           WRITE (NUVI, 80002) IVTNUM                                   02190811
           GO TO 0071                                                   02200811
20070      IVFAIL = IVFAIL + 1                                          02210811
           DVCORR = 1.0D0                                               02220811
           WRITE (NUVI, 80031) IVTNUM, DYAVD, DVCORR                    02230811
 0071      CONTINUE                                                     02240811
CT008*  TEST 8                           DPROD, CABS, AIMAG, SNGL, DDIM 02250811
           IVTNUM = 8                                                   02260811
        CYDVC = (3.0, 4.0)                                              02270811
        DYBVD = -7.0D0                                                  02280811
        DYDVD = 3.0D0                                                   02290811
        DYAVD = DPROD(CABS(CYDVC + (-3.0, 3.0)),                        02300811
     1               AIMAG(CYDVC) + (SNGL(DDIM(DYBVD, DYDVD))))         02310811
           IF (DYAVD - 27.99999998D0) 20080, 10080, 40080               02320811
40080      IF (DYAVD - 28.00000002D0) 10080, 10080, 20080               02330811
10080      IVPASS = IVPASS + 1                                          02340811
           WRITE (NUVI, 80002) IVTNUM                                   02350811
           GO TO 0081                                                   02360811
20080      IVFAIL = IVFAIL + 1                                          02370811
           DVCORR = 28.0D0                                              02380811
           WRITE (NUVI, 80031) IVTNUM, DYAVD, DVCORR                    02390811
 0081      CONTINUE                                                     02400811
CT009*  TEST 9                                       AMAX1, CABS, AIMAG 02410811
           IVTNUM = 9                                                   02420811
        CYDVC = (3.0, 4.0)                                              02430811
        DYAVD = AMAX1(CABS(CYDVC), AIMAG(CYDVC * CYDVC))                02440811
           IF (DYAVD - 23.99999998D0) 20090, 10090, 40090               02450811
40090      IF (DYAVD - 24.00000002D0) 10090, 10090, 20090               02460811
10090      IVPASS = IVPASS + 1                                          02470811
           WRITE (NUVI, 80002) IVTNUM                                   02480811
           GO TO 0091                                                   02490811
20090      IVFAIL = IVFAIL + 1                                          02500811
           DVCORR = 24.0D0                                              02510811
           WRITE (NUVI, 80031) IVTNUM, DYAVD, DVCORR                    02520811
 0091      CONTINUE                                                     02530811
CT010*  TEST 10                                       AIMAG, ABS, AMIN0 02540811
           IVTNUM = 10                                                  02550811
        CYDVC = (3.0, -3.)                                              02560811
        IYBVI = 4                                                       02570811
        IYDVI = -3                                                      02580811
        CYAVC = ((3.0, 4.0) + AIMAG((3.0, 4.0))) *                      02590811
     1         (ABS(AMIN0(IYBVI, IYDVI)) - CYDVC)                       02600811
           IF (R2E(1) + 12.001) 20100, 40102, 40101                     02610811
40101      IF (R2E(1) + 11.999) 40102, 40102, 20100                     02620811
40102      IF (R2E(2) - 20.999) 20100, 10100, 40100                     02630811
40100      IF (R2E(2) - 21.001) 10100, 10100, 20100                     02640811
10100      IVPASS = IVPASS + 1                                          02650811
           WRITE (NUVI, 80002) IVTNUM                                   02660811
           GO TO 0101                                                   02670811
20100      IVFAIL = IVFAIL + 1                                          02680811
           ZVCORR = (-12.0, 21.0)                                       02690811
           WRITE (NUVI, 80045) IVTNUM, CYAVC, ZVCORR                    02700811
 0101      CONTINUE                                                     02710811
C*****                                                                  02720811
CBB** ********************** BBCSUM0  **********************************02730811
C**** WRITE OUT TEST SUMMARY                                            02740811
C****                                                                   02750811
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02760811
      WRITE (I02, 90004)                                                02770811
      WRITE (I02, 90014)                                                02780811
      WRITE (I02, 90004)                                                02790811
      WRITE (I02, 90020) IVPASS                                         02800811
      WRITE (I02, 90022) IVFAIL                                         02810811
      WRITE (I02, 90024) IVDELE                                         02820811
      WRITE (I02, 90026) IVINSP                                         02830811
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02840811
CBE** ********************** BBCSUM0  **********************************02850811
CBB** ********************** BBCFOOT0 **********************************02860811
C**** WRITE OUT REPORT FOOTINGS                                         02870811
C****                                                                   02880811
      WRITE (I02,90016) ZPROG, ZPROG                                    02890811
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02900811
      WRITE (I02,90019)                                                 02910811
CBE** ********************** BBCFOOT0 **********************************02920811
CBB** ********************** BBCFMT0A **********************************02930811
C**** FORMATS FOR TEST DETAIL LINES                                     02940811
C****                                                                   02950811
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02960811
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02970811
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02980811
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02990811
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03000811
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03010811
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03020811
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03030811
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03040811
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03050811
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03060811
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03070811
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03080811
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03090811
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03100811
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03110811
80050 FORMAT (1H ,48X,A31)                                              03120811
CBE** ********************** BBCFMT0A **********************************03130811
CBB** ********************** BBCFMAT1 **********************************03140811
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03150811
C****                                                                   03160811
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03170811
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03180811
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03190811
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03200811
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03210811
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03220811
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03230811
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03240811
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03250811
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03260811
     21H(,F12.5,2H, ,F12.5,1H))                                         03270811
CBE** ********************** BBCFMAT1 **********************************03280811
CBB** ********************** BBCFMT0B **********************************03290811
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03300811
C****                                                                   03310811
90002 FORMAT (1H1)                                                      03320811
90004 FORMAT (1H )                                                      03330811
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03340811
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03350811
90008 FORMAT (1H ,21X,A13,A17)                                          03360811
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03370811
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03380811
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03390811
     1       7X,7HREMARKS,24X)                                          03400811
90014 FORMAT (1H ,46H----------------------------------------------,    03410811
     1        33H---------------------------------)                     03420811
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03430811
C****                                                                   03440811
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03450811
C****                                                                   03460811
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03470811
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03480811
     1        A13)                                                      03490811
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03500811
C****                                                                   03510811
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03520811
C****                                                                   03530811
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03540811
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03550811
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03560811
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03570811
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03580811
CBE** ********************** BBCFMT0B **********************************03590811
C*****                                                                  03600811
C*****    END OF TEST SEGMENT 174                                       03610811
        STOP                                                            03620811
        END                                                             03630811
                                                                        03640811
