C***********************************************************************00010806
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020806
C*****   FM806               YDMAX1 - (166)                             00030806
C*****                                                                  00040806
C***********************************************************************00050806
C*****  GENERAL PURPOSE                                         ANS REF 00060806
C*****    TEST OF INTRINSIC FUNCTION --                          15.3   00070806
C*****    DMAX1 -- CHOOSING LARGEST VALUE                      (TABLE 5)00080806
C*****                                                                  00090806
CBB** ********************** BBCCOMNT **********************************00100806
C****                                                                   00110806
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120806
C****                          VERSION 2.0                              00130806
C****                                                                   00140806
C****                                                                   00150806
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160806
C****                   GENERAL SERVICES ADMINISTRATION                 00170806
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180806
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190806
C****                      FALLS CHURCH, VA. 22041                      00200806
C****                                                                   00210806
C****                          (703) 756-6153                           00220806
C****                                                                   00230806
CBE** ********************** BBCCOMNT **********************************00240806
C*****    S P E C I F I C A T I O N S  SEGMENT 166                      00250806
      DOUBLE PRECISION DTAVD, DTBVD, DTCVD, DTDVD, DTEVD, DVCORR        00260806
C*****                                                                  00270806
CBB** ********************** BBCINITA **********************************00280806
C**** SPECIFICATION STATEMENTS                                          00290806
C****                                                                   00300806
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00310806
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00320806
CBE** ********************** BBCINITA **********************************00330806
CBB** ********************** BBCINITB **********************************00340806
C**** INITIALIZE SECTION                                                00350806
      DATA  ZVERS,                  ZVERSD,             ZDATE           00360806
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00370806
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00380806
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00390806
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00400806
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00410806
      DATA   REMRKS /'                               '/                 00420806
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00430806
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00440806
C****                                                                   00450806
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00460806
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00470806
CZ03  ZPROG  = 'PROGRAM NAME'                                           00480806
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00550806
      IVPASS = 0                                                        00560806
      IVFAIL = 0                                                        00570806
      IVDELE = 0                                                        00580806
      IVINSP = 0                                                        00590806
      IVTOTL = 0                                                        00600806
      IVTOTN = 0                                                        00610806
      ICZERO = 0                                                        00620806
C                                                                       00630806
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640806
      I01 = 05                                                          00650806
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660806
      I02 = 06                                                          00670806
C                                                                       00680806
      I01 = 5                                                           00690806
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700806
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00710806
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00720806
C                                                                       00730806
      I02 = 6                                                           00740806
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00750806
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00760806
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00770806
C                                                                       00780806
CBE** ********************** BBCINITB **********************************00790806
      NUVI = I02                                                        00800806
      IVTOTL = 12                                                       00810806
      ZPROG = 'FM806'                                                   00820806
CBB** ********************** BBCHED0A **********************************00830806
C****                                                                   00840806
C**** WRITE REPORT TITLE                                                00850806
C****                                                                   00860806
      WRITE (I02, 90002)                                                00870806
      WRITE (I02, 90006)                                                00880806
      WRITE (I02, 90007)                                                00890806
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00900806
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00910806
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00920806
CBE** ********************** BBCHED0A **********************************00930806
C*****                                                                  00940806
        WRITE (NUVI,16601)                                              00950806
16601   FORMAT (1H , // 1X,36HYDMAX1 - (166) INTRINSIC FUNCTION-- //17X,00960806
     1          31HDMAX1  (CHOOSING LARGEST VALUE)//2X,                 00970806
     2          15HANS REF. - 15.3)                                     00980806
CBB** ********************** BBCHED0B **********************************00990806
C**** WRITE DETAIL REPORT HEADERS                                       01000806
C****                                                                   01010806
      WRITE (I02,90004)                                                 01020806
      WRITE (I02,90004)                                                 01030806
      WRITE (I02,90013)                                                 01040806
      WRITE (I02,90014)                                                 01050806
      WRITE (I02,90015) IVTOTL                                          01060806
CBE** ********************** BBCHED0B **********************************01070806
C*****                                                                  01080806
CT001*  TEST 1                                              BOTH ZEROES 01090806
           IVTNUM = 1                                                   01100806
        DTBVD = 0.0D0                                                   01110806
        DTDVD = 0.0D0                                                   01120806
        DTAVD = DMAX1(DTBVD, DTDVD)                                     01130806
           IF (DTAVD + 5.0D-10) 20010, 10010, 40010                     01140806
40010      IF (DTAVD - 5.0D-10) 10010, 10010, 20010                     01150806
10010      IVPASS = IVPASS + 1                                          01160806
           WRITE (NUVI, 80002) IVTNUM                                   01170806
           GO TO 0011                                                   01180806
20010      IVFAIL = IVFAIL + 1                                          01190806
           DVCORR = 0.0D0                                               01200806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    01210806
 0011      CONTINUE                                                     01220806
CT002*  TEST 2                                   ONE NON-ZERO, ONE ZERO 01230806
           IVTNUM = 2                                                   01240806
        DTBVD = 5.625D0                                                 01250806
        DTDVD = 0.0D0                                                   01260806
        DTAVD = DMAX1(DTBVD, DTDVD)                                     01270806
           IF (DTAVD - 5.624999997D0) 20020, 10020, 40020               01280806
40020      IF (DTAVD - 5.625000003D0) 10020, 10020, 20020               01290806
10020      IVPASS = IVPASS + 1                                          01300806
           WRITE (NUVI, 80002) IVTNUM                                   01310806
           GO TO 0021                                                   01320806
20020      IVFAIL = IVFAIL + 1                                          01330806
           DVCORR = 5.625D0                                             01340806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    01350806
 0021      CONTINUE                                                     01360806
CT003*  TEST 3                                        BOTH VALUES EQUAL 01370806
           IVTNUM = 3                                                   01380806
        DTBVD = 6.5D0                                                   01390806
        DTDVD = 6.5D0                                                   01400806
        DTAVD = DMAX1(DTBVD, DTDVD)                                     01410806
           IF (DTAVD - 6.499999996D0) 20030, 10030, 40030               01420806
40030      IF (DTAVD - 6.500000004D0) 10030, 10030, 20030               01430806
10030      IVPASS = IVPASS + 1                                          01440806
           WRITE (NUVI, 80002) IVTNUM                                   01450806
           GO TO 0031                                                   01460806
20030      IVFAIL = IVFAIL + 1                                          01470806
           DVCORR = 6.5D0                                               01480806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    01490806
 0031      CONTINUE                                                     01500806
CT004*  TEST 4                                         VALUES NOT EQUAL 01510806
           IVTNUM = 4                                                   01520806
        DTBVD = 7.125D0                                                 01530806
        DTDVD = 5.125D0                                                 01540806
        DTAVD = DMAX1(DTBVD, DTDVD)                                     01550806
           IF (DTAVD - 7.124999996D0) 20040, 10040, 40040               01560806
40040      IF (DTAVD - 7.125000004D0) 10040, 10040, 20040               01570806
10040      IVPASS = IVPASS + 1                                          01580806
           WRITE (NUVI, 80002) IVTNUM                                   01590806
           GO TO 0041                                                   01600806
20040      IVFAIL = IVFAIL + 1                                          01610806
           DVCORR = 7.125D0                                             01620806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    01630806
 0041      CONTINUE                                                     01640806
CT005*  TEST 5                             ONE VALUE ZERO, ONE NEGATIVE 01650806
           IVTNUM = 5                                                   01660806
        DTBVD = -5.625D0                                                01670806
        DTDVD = 0.0D0                                                   01680806
        DTAVD = DMAX1(DTBVD, DTDVD)                                     01690806
           IF (DTAVD + 5.0D-10) 20050, 10050, 40050                     01700806
40050      IF (DTAVD - 5.0D-10) 10050, 10050, 20050                     01710806
10050      IVPASS = IVPASS + 1                                          01720806
           WRITE (NUVI, 80002) IVTNUM                                   01730806
           GO TO 0051                                                   01740806
20050      IVFAIL = IVFAIL + 1                                          01750806
           DVCORR = 0.0D0                                               01760806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    01770806
 0051      CONTINUE                                                     01780806
CT006*  TEST 6                         BOTH VALUES EQUAL, BOTH NEGATIVE 01790806
           IVTNUM = 6                                                   01800806
        DTBVD = -6.5D0                                                  01810806
        DTDVD = -6.5D0                                                  01820806
        DTAVD = DMAX1(DTBVD, DTDVD)                                     01830806
           IF (DTAVD + 6.500000004D0) 20060, 10060, 40060               01840806
40060      IF (DTAVD + 6.499999996D0) 10060, 10060, 20060               01850806
10060      IVPASS = IVPASS + 1                                          01860806
           WRITE (NUVI, 80002) IVTNUM                                   01870806
           GO TO 0061                                                   01880806
20060      IVFAIL = IVFAIL + 1                                          01890806
           DVCORR = -6.5D0                                              01900806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    01910806
 0061      CONTINUE                                                     01920806
CT007*  TEST 7                          VALUES NOT EQUAL, BOTH NEGATIVE 01930806
           IVTNUM = 7                                                   01940806
        DTBVD = -7.125D0                                                01950806
        DTDVD = -5.125D0                                                01960806
        DTAVD = DMAX1(DTBVD, DTDVD)                                     01970806
           IF (DTAVD + 5.125000003D0) 20070, 10070, 40070               01980806
40070      IF (DTAVD + 5.124999997D0) 10070, 10070, 20070               01990806
10070      IVPASS = IVPASS + 1                                          02000806
           WRITE (NUVI, 80002) IVTNUM                                   02010806
           GO TO 0071                                                   02020806
20070      IVFAIL = IVFAIL + 1                                          02030806
           DVCORR = -5.125D0                                            02040806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    02050806
 0071      CONTINUE                                                     02060806
CT008*  TEST 8      1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN 02070806
           IVTNUM = 8                                                   02080806
        DTDVD = 5.625D0                                                 02090806
        DTEVD = 0.0D0                                                   02100806
        DTAVD = DMAX1(DTDVD, -DTEVD)                                    02110806
           IF (DTAVD - 5.624999997D0) 20080, 10080, 40080               02120806
40080      IF (DTAVD - 5.625000003D0) 10080, 10080, 20080               02130806
10080      IVPASS = IVPASS + 1                                          02140806
           WRITE (NUVI, 80002) IVTNUM                                   02150806
           GO TO 0081                                                   02160806
20080      IVFAIL = IVFAIL + 1                                          02170806
           DVCORR = 5.625D0                                             02180806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    02190806
 0081      CONTINUE                                                     02200806
CT009*  TEST 9             ARITHMETIC EXPRESSIONS PRESENTED TO FUNCTION 02210806
           IVTNUM = 9                                                   02220806
        DTDVD = 3.5D0                                                   02230806
        DTEVD = 4.0D0                                                   02240806
        DTAVD = DMAX1(DTDVD + DTEVD, -DTEVD - DTDVD)                    02250806
           IF (DTAVD - 7.499999996D0) 20090, 10090, 40090               02260806
40090      IF (DTAVD - 7.500000004D0) 10090, 10090, 20090               02270806
10090      IVPASS = IVPASS + 1                                          02280806
           WRITE (NUVI, 80002) IVTNUM                                   02290806
           GO TO 0091                                                   02300806
20090      IVFAIL = IVFAIL + 1                                          02310806
           DVCORR = 7.5D0                                               02320806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    02330806
 0091      CONTINUE                                                     02340806
CT010*  TEST 10                                             3 ARGUMENTS 02350806
           IVTNUM = 10                                                  02360806
        DTBVD = 0.0D0                                                   02370806
        DTCVD = -1.99D0                                                 02380806
        DTAVD = DMAX1(DTCVD, DTBVD, -DTCVD)                             02390806
           IF (DTAVD - 1.98999999D0) 20100, 10100, 40100                02400806
40100      IF (DTAVD - 1.99000001D0) 10100, 10100, 20100                02410806
10100      IVPASS = IVPASS + 1                                          02420806
           WRITE (NUVI, 80002) IVTNUM                                   02430806
           GO TO 0101                                                   02440806
20100      IVFAIL = IVFAIL + 1                                          02450806
           DVCORR = 1.99D0                                              02460806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    02470806
 0101      CONTINUE                                                     02480806
CT011*  TEST 11                                             4 ARGUMENTS 02490806
           IVTNUM = 11                                                  02500806
C*****                             ARGUMENTS OF HIGH AND LOW MAGNITUDES 02510806
        DTAVD = 1.0D-34                                                 02520806
        DTBVD = -1.0D-34                                                02530806
        DTCVD = 1.0D+34                                                 02540806
        DTAVD = DMAX1(DTAVD, DTBVD, DTCVD, -DTCVD)                      02550806
           IF (DTAVD - 0.9999999995D34) 20110, 10110, 40110             02560806
40110      IF (DTAVD - 1.000000001D34) 10110, 10110, 20110              02570806
10110      IVPASS = IVPASS + 1                                          02580806
           WRITE (NUVI, 80002) IVTNUM                                   02590806
           GO TO 0111                                                   02600806
20110      IVFAIL = IVFAIL + 1                                          02610806
           DVCORR = 1.0D+34                                             02620806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    02630806
 0111      CONTINUE                                                     02640806
CT012*  TEST 12                                             5 ARGUMENTS 02650806
           IVTNUM = 12                                                  02660806
        DTDVD = 3.5D0                                                   02670806
        DTEVD = 4.5D0                                                   02680806
        DTAVD = DMAX1(DTDVD, -DTDVD, -DTEVD, +DTDVD, DTEVD)             02690806
           IF (DTAVD - 4.499999997D0) 20120, 10120, 40120               02700806
40120      IF (DTAVD - 4.500000003D0) 10120, 10120, 20120               02710806
10120      IVPASS = IVPASS + 1                                          02720806
           WRITE (NUVI, 80002) IVTNUM                                   02730806
           GO TO 0121                                                   02740806
20120      IVFAIL = IVFAIL + 1                                          02750806
           DVCORR = 4.5D0                                               02760806
           WRITE (NUVI, 80031) IVTNUM, DTAVD, DVCORR                    02770806
 0121      CONTINUE                                                     02780806
C*****                                                                  02790806
CBB** ********************** BBCSUM0  **********************************02800806
C**** WRITE OUT TEST SUMMARY                                            02810806
C****                                                                   02820806
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02830806
      WRITE (I02, 90004)                                                02840806
      WRITE (I02, 90014)                                                02850806
      WRITE (I02, 90004)                                                02860806
      WRITE (I02, 90020) IVPASS                                         02870806
      WRITE (I02, 90022) IVFAIL                                         02880806
      WRITE (I02, 90024) IVDELE                                         02890806
      WRITE (I02, 90026) IVINSP                                         02900806
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02910806
CBE** ********************** BBCSUM0  **********************************02920806
CBB** ********************** BBCFOOT0 **********************************02930806
C**** WRITE OUT REPORT FOOTINGS                                         02940806
C****                                                                   02950806
      WRITE (I02,90016) ZPROG, ZPROG                                    02960806
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02970806
      WRITE (I02,90019)                                                 02980806
CBE** ********************** BBCFOOT0 **********************************02990806
CBB** ********************** BBCFMT0A **********************************03000806
C**** FORMATS FOR TEST DETAIL LINES                                     03010806
C****                                                                   03020806
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03030806
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03040806
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03050806
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03060806
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03070806
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03080806
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03090806
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03100806
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03110806
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03120806
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03130806
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03140806
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03150806
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03160806
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03170806
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03180806
80050 FORMAT (1H ,48X,A31)                                              03190806
CBE** ********************** BBCFMT0A **********************************03200806
CBB** ********************** BBCFMAT1 **********************************03210806
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03220806
C****                                                                   03230806
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03240806
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03250806
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03260806
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03270806
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03280806
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03290806
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03300806
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03310806
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03320806
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03330806
     21H(,F12.5,2H, ,F12.5,1H))                                         03340806
CBE** ********************** BBCFMAT1 **********************************03350806
CBB** ********************** BBCFMT0B **********************************03360806
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03370806
C****                                                                   03380806
90002 FORMAT (1H1)                                                      03390806
90004 FORMAT (1H )                                                      03400806
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03410806
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03420806
90008 FORMAT (1H ,21X,A13,A17)                                          03430806
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03440806
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03450806
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03460806
     1       7X,7HREMARKS,24X)                                          03470806
90014 FORMAT (1H ,46H----------------------------------------------,    03480806
     1        33H---------------------------------)                     03490806
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03500806
C****                                                                   03510806
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03520806
C****                                                                   03530806
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03540806
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03550806
     1        A13)                                                      03560806
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03570806
C****                                                                   03580806
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03590806
C****                                                                   03600806
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03610806
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03620806
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03630806
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03640806
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03650806
CBE** ********************** BBCFMT0B **********************************03660806
C*****    END OF TEST SEGMENT 166                                       03670806
        STOP                                                            03680806
        END                                                             03690806
                                                                        03700806
