C***********************************************************************00010810
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020810
C*****   FM810               YDMMX - (173)                              00030810
C*****                                                                  00040810
C***********************************************************************00050810
C*****  GENERAL PURPOSE                                         ANS REF 00060810
C*****    TESTS THE USE OF INTEGER, REAL, DOUBLE PRECISION,       15.3  00070810
C*****    AND MIXED MODE EXPRESSIONS CONTAINING REFERENCES TO     15.10 00080810
C*****    THE INTRINSIC FUNCTIONS OF THE FULL LANGUAGE            6.1.4 00090810
C*****                                                                  00100810
C*****  GENERAL COMMENTS                                                00110810
C*****    SEGMENTS TESTING XINT, XREAL, XAINT, XABS, XAMOD,             00120810
C*****    XSIGN, XDIM, XMAX, XMIN, YIDINT, YSNGL                        00130810
C*****    YDINT, YDABS, YCABS, YDMOD, YDSIGN,                           00140810
C*****    YDMAX1, YDMIN1, YDBLE, YCONJG ASSUMED WORKING                 00150810
CBB** ********************** BBCCOMNT **********************************00160810
C****                                                                   00170810
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00180810
C****                          VERSION 2.0                              00190810
C****                                                                   00200810
C****                                                                   00210810
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00220810
C****                   GENERAL SERVICES ADMINISTRATION                 00230810
C****                   FEDERAL SOFTWARE TESTING CENTER                 00240810
C****                   5203 LEESBURG PIKE, SUITE 1100                  00250810
C****                      FALLS CHURCH, VA. 22041                      00260810
C****                                                                   00270810
C****                          (703) 756-6153                           00280810
C****                                                                   00290810
CBE** ********************** BBCCOMNT **********************************00300810
C*****                                                                  00310810
C*****    S P E C I F I C A T I O N S  SEGMENT 173                      00320810
        DOUBLE PRECISION DXAVD,DXBVD,DXDVD,DXEVD,DXFVD,DXGVD,DVCORR     00330810
CBB** ********************** BBCINITA **********************************00340810
C**** SPECIFICATION STATEMENTS                                          00350810
C****                                                                   00360810
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00370810
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00380810
CBE** ********************** BBCINITA **********************************00390810
CBB** ********************** BBCINITB **********************************00400810
C**** INITIALIZE SECTION                                                00410810
      DATA  ZVERS,                  ZVERSD,             ZDATE           00420810
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00430810
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00440810
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00450810
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00460810
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00470810
      DATA   REMRKS /'                               '/                 00480810
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00490810
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00500810
C****                                                                   00510810
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00520810
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00530810
CZ03  ZPROG  = 'PROGRAM NAME'                                           00540810
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00610810
      IVPASS = 0                                                        00620810
      IVFAIL = 0                                                        00630810
      IVDELE = 0                                                        00640810
      IVINSP = 0                                                        00650810
      IVTOTL = 0                                                        00660810
      IVTOTN = 0                                                        00670810
      ICZERO = 0                                                        00680810
C                                                                       00690810
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00700810
      I01 = 05                                                          00710810
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00720810
      I02 = 06                                                          00730810
C                                                                       00740810
      I01 = 5                                                           00750810
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00760810
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00770810
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00780810
C                                                                       00790810
      I02 = 6                                                           00800810
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00810810
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00820810
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00830810
C                                                                       00840810
CBE** ********************** BBCINITB **********************************00850810
      NUVI = I02                                                        00860810
      IVTOTL = 10                                                       00870810
      ZPROG = 'FM810'                                                   00880810
CBB** ********************** BBCHED0A **********************************00890810
C****                                                                   00900810
C**** WRITE REPORT TITLE                                                00910810
C****                                                                   00920810
      WRITE (I02, 90002)                                                00930810
      WRITE (I02, 90006)                                                00940810
      WRITE (I02, 90007)                                                00950810
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00960810
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00970810
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00980810
CBE** ********************** BBCHED0A **********************************00990810
C*****                                                                  01000810
C*****                                                                  01010810
C*****    HEADER FOR SEGMENT 173 WRITTEN                                01020810
        WRITE (NUVI,17301)                                              01030810
17301   FORMAT(1H , //1X, 35HYDMMX - (173) INTRINSIC FUNCTIONS--//      01040810
     1        16X, 22HINTEGER, REAL AND D.P./,                          01050810
     2        16X, 26HAND MIXED MODE EXPRESSIONS//                      01060810
     3        2X,  29HANS REF. - 15.3, 15.10, 6.1.4)                    01070810
CBB** ********************** BBCHED0B **********************************01080810
C**** WRITE DETAIL REPORT HEADERS                                       01090810
C****                                                                   01100810
      WRITE (I02,90004)                                                 01110810
      WRITE (I02,90004)                                                 01120810
      WRITE (I02,90013)                                                 01130810
      WRITE (I02,90014)                                                 01140810
      WRITE (I02,90015) IVTOTL                                          01150810
CBE** ********************** BBCHED0B **********************************01160810
C*****                                                                  01170810
CT001*  TEST 1                                                          01180810
           IVTNUM = 1                                                   01190810
        DXBVD = 3.5D0                                                   01200810
        IXAVI = IDINT(DXBVD) + 2                                        01210810
           IF (IXAVI - 5) 20010, 10010, 20010                           01220810
10010      IVPASS = IVPASS + 1                                          01230810
           WRITE (NUVI, 80002) IVTNUM                                   01240810
           GO TO 0011                                                   01250810
20010      IVFAIL = IVFAIL + 1                                          01260810
           IVCORR = 5                                                   01270810
           WRITE (NUVI, 80010) IVTNUM, IXAVI, IVCORR                    01280810
 0011      CONTINUE                                                     01290810
CT002*  TEST 2                                                          01300810
           IVTNUM = 2                                                   01310810
        DXBVD = 5.25D0                                                  01320810
        RXAVS = SNGL(DXBVD) * 3.0                                       01330810
           IF (RXAVS - 15.749) 20020, 10020, 40020                      01340810
40020      IF (RXAVS - 15.751) 10020, 10020, 20020                      01350810
10020      IVPASS = IVPASS + 1                                          01360810
           WRITE (NUVI, 80002) IVTNUM                                   01370810
           GO TO 0021                                                   01380810
20020      IVFAIL = IVFAIL + 1                                          01390810
           RVCORR = 15.75                                               01400810
           WRITE (NUVI, 80012) IVTNUM, RXAVS, RVCORR                    01410810
 0021      CONTINUE                                                     01420810
CT003*  TEST 3                                                          01430810
           IVTNUM = 3                                                   01440810
        DXBVD = 3.2D0                                                   01450810
        DXAVD = DINT(DXBVD) ** 2.0                                      01460810
           IF (DXAVD - 8.999999995D0) 20030, 10030, 40030               01470810
40030      IF (DXAVD - 9.000000005D0) 10030, 10030, 20030               01480810
10030      IVPASS = IVPASS + 1                                          01490810
           WRITE (NUVI, 80002) IVTNUM                                   01500810
           GO TO 0031                                                   01510810
20030      IVFAIL = IVFAIL + 1                                          01520810
           DVCORR = 9.0D0                                               01530810
           WRITE (NUVI, 80031) IVTNUM, DXAVD, DVCORR                    01540810
 0031      CONTINUE                                                     01550810
CT004*  TEST 4                                                          01560810
           IVTNUM = 4                                                   01570810
        DXBVD = 3.2D0                                                   01580810
        DXAVD = DNINT(DXBVD) + 2.5                                      01590810
           IF (DXAVD - 5.499999997D0) 20040, 10040, 40040               01600810
40040      IF (DXAVD - 5.500000003D0) 10040, 10040, 20040               01610810
10040      IVPASS = IVPASS + 1                                          01620810
           WRITE (NUVI, 80002) IVTNUM                                   01630810
           GO TO 0041                                                   01640810
20040      IVFAIL = IVFAIL + 1                                          01650810
           DVCORR = 5.5D0                                               01660810
           WRITE (NUVI, 80031) IVTNUM, DXAVD, DVCORR                    01670810
 0041      CONTINUE                                                     01680810
CT005*  TEST 5                                                          01690810
           IVTNUM = 5                                                   01700810
        DXBVD = 3.5D0                                                   01710810
        RXAVS = IDINT(DXBVD) * 2.5                                      01720810
           IF (RXAVS - 7.4996) 20050, 10050, 40050                      01730810
40050      IF (RXAVS - 7.5004) 10050, 10050, 20050                      01740810
10050      IVPASS = IVPASS + 1                                          01750810
           WRITE (NUVI, 80002) IVTNUM                                   01760810
           GO TO 0051                                                   01770810
20050      IVFAIL = IVFAIL + 1                                          01780810
           RVCORR = 7.5                                                 01790810
           WRITE (NUVI, 80012) IVTNUM, RXAVS, RVCORR                    01800810
 0051      CONTINUE                                                     01810810
CT006*  TEST 6                                                          01820810
           IVTNUM = 6                                                   01830810
        DXBVD = -2.5D0                                                  01840810
        DXAVD = DABS(DXBVD) * 2                                         01850810
           IF (DXAVD - 4.999999997D0) 20060, 10060, 40060               01860810
40060      IF (DXAVD - 5.000000003D0) 10060, 10060, 20060               01870810
10060      IVPASS = IVPASS + 1                                          01880810
           WRITE (NUVI, 80002) IVTNUM                                   01890810
           GO TO 0061                                                   01900810
20060      IVFAIL = IVFAIL + 1                                          01910810
           DVCORR = 5.0D0                                               01920810
           WRITE (NUVI, 80031) IVTNUM, DXAVD, DVCORR                    01930810
 0061      CONTINUE                                                     01940810
CT007*  TEST 7                                                          01950810
           IVTNUM = 7                                                   01960810
        DXBVD = 5.0D0                                                   01970810
        DXDVD = 2.0D0                                                   01980810
        DXEVD = 3.0D0                                                   01990810
        DXFVD = -1.0D0                                                  02000810
        DXAVD = DMOD(DXBVD, DXDVD) * 3 + DSIGN(DXEVD, DXFVD)            02010810
           IF (DXAVD + 5.0D-10) 20070, 10070, 40070                     02020810
40070      IF (DXAVD - 5.0D-10) 10070, 10070, 20070                     02030810
10070      IVPASS = IVPASS + 1                                          02040810
           WRITE (NUVI, 80002) IVTNUM                                   02050810
           GO TO 0071                                                   02060810
20070      IVFAIL = IVFAIL + 1                                          02070810
           DVCORR = 0.0D0                                               02080810
           WRITE (NUVI, 80031) IVTNUM, DXAVD, DVCORR                    02090810
 0071      CONTINUE                                                     02100810
CT008*  TEST 8                                                          02110810
           IVTNUM = 8                                                   02120810
        DXBVD = 1.5D1                                                   02130810
        DXDVD = 0.5D1                                                   02140810
        RXBVS = 5.0                                                     02150810
        RXDVS = 2.0                                                     02160810
        DXAVD = DDIM(DXBVD, DXDVD) / DPROD(RXBVS, RXDVS)                02170810
           IF (DXAVD - 0.9999999995D0) 20080, 10080, 40080              02180810
40080      IF (DXAVD - 1.000000001D0) 10080, 10080, 20080               02190810
10080      IVPASS = IVPASS + 1                                          02200810
           WRITE (NUVI, 80002) IVTNUM                                   02210810
           GO TO 0081                                                   02220810
20080      IVFAIL = IVFAIL + 1                                          02230810
           DVCORR = 1.0D0                                               02240810
           WRITE (NUVI, 80031) IVTNUM, DXAVD, DVCORR                    02250810
 0081      CONTINUE                                                     02260810
CT009*  TEST 9                                                          02270810
           IVTNUM = 9                                                   02280810
        DXBVD = 5.5D0                                                   02290810
        DXDVD = 2.5D0                                                   02300810
        DXEVD = 1.0D0                                                   02310810
        RXBVS = 1.0                                                     02320810
        DXAVD = (10 - DMAX1(DXBVD, DXDVD)) * (DMIN1(DXEVD, DXDVD)       02330810
     1          + DBLE(RXBVS))                                          02340810
           IF (DXAVD - 8.999999995D0) 20090, 10090, 40090               02350810
40090      IF (DXAVD - 9.000000005D0) 10090, 10090, 20090               02360810
10090      IVPASS = IVPASS + 1                                          02370810
           WRITE (NUVI, 80002) IVTNUM                                   02380810
           GO TO 0091                                                   02390810
20090      IVFAIL = IVFAIL + 1                                          02400810
           DVCORR = 9.0D0                                               02410810
           WRITE (NUVI, 80031) IVTNUM, DXAVD, DVCORR                    02420810
 0091      CONTINUE                                                     02430810
CT010*  TEST 10                                                         02440810
           IVTNUM = 10                                                  02450810
        DXBVD = 0.635D2                                                 02460810
        RXBVS = 5.0                                                     02470810
        DXDVD = 5.7D0                                                   02480810
        DXEVD = -6.0D0                                                  02490810
        DXFVD = 1.0D0                                                   02500810
        DXGVD = 3.0D0                                                   02510810
        DXAVD = (IDINT(DXBVD) + 1.0) / (7 - DBLE(RXBVS)) -              02520810
     1          (DINT(DXDVD) + 5 + 5.5) * (DSIGN(DXEVD, DXFVD) /        02530810
     2              SNGL(DXGVD))                                        02540810
           IF (DXAVD - 0.9999999995D0) 20100, 10100, 40100              02550810
40100      IF (DXAVD - 1.000000001D0) 10100, 10100, 20100               02560810
10100      IVPASS = IVPASS + 1                                          02570810
           WRITE (NUVI, 80002) IVTNUM                                   02580810
           GO TO 0101                                                   02590810
20100      IVFAIL = IVFAIL + 1                                          02600810
           DVCORR = 1.0D0                                               02610810
           WRITE (NUVI, 80031) IVTNUM, DXAVD, DVCORR                    02620810
 0101      CONTINUE                                                     02630810
C*****                                                                  02640810
CBB** ********************** BBCSUM0  **********************************02650810
C**** WRITE OUT TEST SUMMARY                                            02660810
C****                                                                   02670810
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02680810
      WRITE (I02, 90004)                                                02690810
      WRITE (I02, 90014)                                                02700810
      WRITE (I02, 90004)                                                02710810
      WRITE (I02, 90020) IVPASS                                         02720810
      WRITE (I02, 90022) IVFAIL                                         02730810
      WRITE (I02, 90024) IVDELE                                         02740810
      WRITE (I02, 90026) IVINSP                                         02750810
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02760810
CBE** ********************** BBCSUM0  **********************************02770810
CBB** ********************** BBCFOOT0 **********************************02780810
C**** WRITE OUT REPORT FOOTINGS                                         02790810
C****                                                                   02800810
      WRITE (I02,90016) ZPROG, ZPROG                                    02810810
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02820810
      WRITE (I02,90019)                                                 02830810
CBE** ********************** BBCFOOT0 **********************************02840810
CBB** ********************** BBCFMT0A **********************************02850810
C**** FORMATS FOR TEST DETAIL LINES                                     02860810
C****                                                                   02870810
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02880810
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02890810
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02900810
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02910810
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02920810
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02930810
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02940810
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02950810
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02960810
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02970810
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02980810
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02990810
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03000810
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03010810
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03020810
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03030810
80050 FORMAT (1H ,48X,A31)                                              03040810
CBE** ********************** BBCFMT0A **********************************03050810
CBB** ********************** BBCFMAT1 **********************************03060810
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03070810
C****                                                                   03080810
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03090810
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03100810
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03110810
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03120810
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03130810
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03140810
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03150810
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03160810
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03170810
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03180810
     21H(,F12.5,2H, ,F12.5,1H))                                         03190810
CBE** ********************** BBCFMAT1 **********************************03200810
CBB** ********************** BBCFMT0B **********************************03210810
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03220810
C****                                                                   03230810
90002 FORMAT (1H1)                                                      03240810
90004 FORMAT (1H )                                                      03250810
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03260810
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03270810
90008 FORMAT (1H ,21X,A13,A17)                                          03280810
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03290810
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03300810
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03310810
     1       7X,7HREMARKS,24X)                                          03320810
90014 FORMAT (1H ,46H----------------------------------------------,    03330810
     1        33H---------------------------------)                     03340810
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03350810
C****                                                                   03360810
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03370810
C****                                                                   03380810
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03390810
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03400810
     1        A13)                                                      03410810
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03420810
C****                                                                   03430810
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03440810
C****                                                                   03450810
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03460810
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03470810
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03480810
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03490810
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03500810
CBE** ********************** BBCFMT0B **********************************03510810
C*****    END OF TEST SEGMENT 173                                       03520810
        STOP                                                            03530810
        END                                                             03540810
                                                                        03550810
