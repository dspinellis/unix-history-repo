C***********************************************************************00010807
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020807
C*****   FM807               YDMIN1 - (168)                             00030807
C*****                                                                  00040807
C***********************************************************************00050807
C*****  GENERAL PURPOSE                                         ANS REF 00060807
C*****    TEST OF INTRINSIC FUNCTION --                          15.3   00070807
C*****    DMIN1 -- CHOOSING SMALLEST VALUE                     (TABLE 5)00080807
C*****                                                                  00090807
CBB** ********************** BBCCOMNT **********************************00100807
C****                                                                   00110807
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120807
C****                          VERSION 2.0                              00130807
C****                                                                   00140807
C****                                                                   00150807
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160807
C****                   GENERAL SERVICES ADMINISTRATION                 00170807
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180807
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190807
C****                      FALLS CHURCH, VA. 22041                      00200807
C****                                                                   00210807
C****                          (703) 756-6153                           00220807
C****                                                                   00230807
CBE** ********************** BBCCOMNT **********************************00240807
C*****    S P E C I F I C A T I O N S  SEGMENT 168                      00250807
      DOUBLE PRECISION DUAVD, DUBVD, DUCVD, DUDVD, DUEVD, DVCORR        00260807
CBB** ********************** BBCINITA **********************************00270807
C**** SPECIFICATION STATEMENTS                                          00280807
C****                                                                   00290807
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00300807
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00310807
CBE** ********************** BBCINITA **********************************00320807
CBB** ********************** BBCINITB **********************************00330807
C**** INITIALIZE SECTION                                                00340807
      DATA  ZVERS,                  ZVERSD,             ZDATE           00350807
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00360807
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00370807
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00380807
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00390807
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00400807
      DATA   REMRKS /'                               '/                 00410807
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00420807
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00430807
C****                                                                   00440807
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00450807
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00460807
CZ03  ZPROG  = 'PROGRAM NAME'                                           00470807
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00540807
      IVPASS = 0                                                        00550807
      IVFAIL = 0                                                        00560807
      IVDELE = 0                                                        00570807
      IVINSP = 0                                                        00580807
      IVTOTL = 0                                                        00590807
      IVTOTN = 0                                                        00600807
      ICZERO = 0                                                        00610807
C                                                                       00620807
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00630807
      I01 = 05                                                          00640807
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00650807
      I02 = 06                                                          00660807
C                                                                       00670807
      I01 = 5                                                           00680807
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690807
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00700807
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00710807
C                                                                       00720807
      I02 = 6                                                           00730807
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00740807
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00750807
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00760807
C                                                                       00770807
CBE** ********************** BBCINITB **********************************00780807
      NUVI = I02                                                        00790807
      IVTOTL = 12                                                       00800807
      ZPROG = 'FM807'                                                   00810807
CBB** ********************** BBCHED0A **********************************00820807
C****                                                                   00830807
C**** WRITE REPORT TITLE                                                00840807
C****                                                                   00850807
      WRITE (I02, 90002)                                                00860807
      WRITE (I02, 90006)                                                00870807
      WRITE (I02, 90007)                                                00880807
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00890807
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00900807
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00910807
CBE** ********************** BBCHED0A **********************************00920807
C*****                                                                  00930807
C*****                                                                  00940807
        WRITE (NUVI,16801)                                              00950807
16801   FORMAT (1H , // 1X,36HYDMIN1 - (168) INTRINSIC FUNCTION-- //17X,00960807
     1          33HDMIN1  (CHOOSING SMALLEST VALUE) //2X,               00970807
     2          15HANS REF. - 15.3)                                     00980807
CBB** ********************** BBCHED0B **********************************00990807
C**** WRITE DETAIL REPORT HEADERS                                       01000807
C****                                                                   01010807
      WRITE (I02,90004)                                                 01020807
      WRITE (I02,90004)                                                 01030807
      WRITE (I02,90013)                                                 01040807
      WRITE (I02,90014)                                                 01050807
      WRITE (I02,90015) IVTOTL                                          01060807
CBE** ********************** BBCHED0B **********************************01070807
C*****                                                                  01080807
CT001*  TEST 1                                        BOTH VALUES EQUAL 01090807
           IVTNUM = 1                                                   01100807
        DUBVD = 0.0D0                                                   01110807
        DUDVD = 0.0D0                                                   01120807
        DUAVD = DMIN1(DUBVD, DUDVD)                                     01130807
           IF (DUAVD + 5.0D-10) 20010, 10010, 40010                     01140807
40010      IF (DUAVD - 5.0D-10) 10010, 10010, 20010                     01150807
10010      IVPASS = IVPASS + 1                                          01160807
           WRITE (NUVI, 80002) IVTNUM                                   01170807
           GO TO 0011                                                   01180807
20010      IVFAIL = IVFAIL + 1                                          01190807
           DVCORR = 0.0D0                                               01200807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    01210807
 0011      CONTINUE                                                     01220807
CT002*  TEST 2                        FIRST VALUE NON-ZERO, SECOND ZERO 01230807
           IVTNUM = 2                                                   01240807
        DUBVD = 5.625D0                                                 01250807
        DUDVD = 0.0D0                                                   01260807
        DUAVD = DMIN1(DUBVD, DUDVD)                                     01270807
           IF (DUAVD + 5.0D-10) 20020, 10020, 40020                     01280807
40020      IF (DUAVD - 5.0D-10) 10020, 10020, 20020                     01290807
10020      IVPASS = IVPASS + 1                                          01300807
           WRITE (NUVI, 80002) IVTNUM                                   01310807
           GO TO 0021                                                   01320807
20020      IVFAIL = IVFAIL + 1                                          01330807
           DVCORR = 0.0D0                                               01340807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    01350807
 0021      CONTINUE                                                     01360807
CT003*  TEST 3                                        BOTH VALUES EQUAL 01370807
           IVTNUM = 3                                                   01380807
        DUBVD = 6.5D0                                                   01390807
        DUDVD = 6.5D0                                                   01400807
        DUAVD = DMIN1(DUBVD, DUDVD)                                     01410807
           IF (DUAVD - 6.499999996D0) 20030, 10030, 40030               01420807
40030      IF (DUAVD - 6.500000004D0) 10030, 10030, 20030               01430807
10030      IVPASS = IVPASS + 1                                          01440807
           WRITE (NUVI, 80002) IVTNUM                                   01450807
           GO TO 0031                                                   01460807
20030      IVFAIL = IVFAIL + 1                                          01470807
           DVCORR = 6.5D0                                               01480807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    01490807
 0031      CONTINUE                                                     01500807
CT004*  TEST 4                                         VALUES NOT EQUAL 01510807
           IVTNUM = 4                                                   01520807
        DUBVD = 7.125D0                                                 01530807
        DUDVD = 5.125D0                                                 01540807
        DUAVD = DMIN1(DUBVD, DUDVD)                                     01550807
           IF (DUAVD - 5.124999997D0) 20040, 10040, 40040               01560807
40040      IF (DUAVD - 5.125000003D0) 10040, 10040, 20040               01570807
10040      IVPASS = IVPASS + 1                                          01580807
           WRITE (NUVI, 80002) IVTNUM                                   01590807
           GO TO 0041                                                   01600807
20040      IVFAIL = IVFAIL + 1                                          01610807
           DVCORR = 5.125D0                                             01620807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    01630807
 0041      CONTINUE                                                     01640807
CT005*  TEST 5                        FIRST VALUE NEGATIVE, SECOND ZERO 01650807
           IVTNUM = 5                                                   01660807
        DUBVD = -5.625D0                                                01670807
        DUDVD = 0.0D0                                                   01680807
        DUAVD = DMIN1(DUBVD, DUDVD)                                     01690807
           IF (DUAVD + 5.625000003D0) 20050, 10050, 40050               01700807
40050      IF (DUAVD + 5.624999997D0) 10050, 10050, 20050               01710807
10050      IVPASS = IVPASS + 1                                          01720807
           WRITE (NUVI, 80002) IVTNUM                                   01730807
           GO TO 0051                                                   01740807
20050      IVFAIL = IVFAIL + 1                                          01750807
           DVCORR = -5.625D0                                            01760807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    01770807
 0051      CONTINUE                                                     01780807
CT006*  TEST 6                         BOTH VALUES EQUAL, BOTH NEGATIVE 01790807
           IVTNUM = 6                                                   01800807
        DUBVD = -6.5D0                                                  01810807
        DUDVD = -6.5D0                                                  01820807
        DUAVD = DMIN1(DUBVD, DUDVD)                                     01830807
           IF (DUAVD + 6.500000004D0) 20060, 10060, 40060               01840807
40060      IF (DUAVD + 6.499999996D0) 10060, 10060, 20060               01850807
10060      IVPASS = IVPASS + 1                                          01860807
           WRITE (NUVI, 80002) IVTNUM                                   01870807
           GO TO 0061                                                   01880807
20060      IVFAIL = IVFAIL + 1                                          01890807
           DVCORR = -6.5D0                                              01900807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    01910807
 0061      CONTINUE                                                     01920807
CT007*  TEST 7                          VALUES NOT EQUAL, BOTH NEGATIVE 01930807
           IVTNUM = 7                                                   01940807
        DUBVD = -7.125D0                                                01950807
        DUDVD = -5.125D0                                                01960807
        DUAVD = DMIN1(DUBVD, DUDVD)                                     01970807
           IF (DUAVD + 7.125000004D0) 20070, 10070, 40070               01980807
40070      IF (DUAVD + 7.124999996D0) 10070, 10070, 20070               01990807
10070      IVPASS = IVPASS + 1                                          02000807
           WRITE (NUVI, 80002) IVTNUM                                   02010807
           GO TO 0071                                                   02020807
20070      IVFAIL = IVFAIL + 1                                          02030807
           DVCORR = -7.125D0                                            02040807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    02050807
 0071      CONTINUE                                                     02060807
CT008*  TEST 8      1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN 02070807
           IVTNUM = 8                                                   02080807
        DUDVD = 5.625D0                                                 02090807
        DUEVD = 0.0D0                                                   02100807
        DUAVD = DMIN1(DUDVD, -DUEVD)                                    02110807
           IF (DUAVD + 5.0D-10) 20080, 10080, 40080                     02120807
40080      IF (DUAVD - 5.0D-10) 10080, 10080, 20080                     02130807
10080      IVPASS = IVPASS + 1                                          02140807
           WRITE (NUVI, 80002) IVTNUM                                   02150807
           GO TO 0081                                                   02160807
20080      IVFAIL = IVFAIL + 1                                          02170807
           DVCORR = 0.0D0                                               02180807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    02190807
 0081      CONTINUE                                                     02200807
CT009*  TEST 9             ARITHMETIC EXPRESSIONS PRESENTED TO FUNCTION 02210807
           IVTNUM = 9                                                   02220807
        DUDVD = 3.5D0                                                   02230807
        DUEVD = 4.0D0                                                   02240807
        DUAVD = DMIN1(DUDVD + DUEVD, -DUEVD - DUDVD)                    02250807
           IF (DUAVD + 7.500000004D0) 20090, 10090, 40090               02260807
40090      IF (DUAVD + 7.499999996D0) 10090, 10090, 20090               02270807
10090      IVPASS = IVPASS + 1                                          02280807
           WRITE (NUVI, 80002) IVTNUM                                   02290807
           GO TO 0091                                                   02300807
20090      IVFAIL = IVFAIL + 1                                          02310807
           DVCORR = -7.5D0                                              02320807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    02330807
 0091      CONTINUE                                                     02340807
CT010*  TEST 10                                             3 ARGUMENTS 02350807
           IVTNUM = 10                                                  02360807
        DUBVD = 0.0D0                                                   02370807
        DUCVD = 1.0D0                                                   02380807
        DUDVD = 2.0D0                                                   02390807
        DUAVD = DMIN1(DUBVD, DUCVD, DUDVD)                              02400807
           IF (DUAVD + 5.0D-10) 20100, 10100, 40100                     02410807
40100      IF (DUAVD - 5.0D-10) 10100, 10100, 20100                     02420807
10100      IVPASS = IVPASS + 1                                          02430807
           WRITE (NUVI, 80002) IVTNUM                                   02440807
           GO TO 0101                                                   02450807
20100      IVFAIL = IVFAIL + 1                                          02460807
           DVCORR = 0.0D0                                               02470807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    02480807
 0101      CONTINUE                                                     02490807
CT011*  TEST 11                                             4 ARGUMENTS 02500807
           IVTNUM = 11                                                  02510807
C*****                             ARGUMENTS OF HIGH AND LOW MAGNITUDES 02520807
        DUAVD = 1.0D+14                                                 02530807
        DUBVD = -1.0D+14                                                02540807
        DUCVD = 1.0D-14                                                 02550807
        DUAVD = DMIN1(DUAVD, DUBVD, DUCVD, -DUCVD)                      02560807
           IF (DUAVD + 1.000000001D14) 20110, 10110, 40110              02570807
40110      IF (DUAVD + 0.9999999995D14) 10110, 10110, 20110             02580807
10110      IVPASS = IVPASS + 1                                          02590807
           WRITE (NUVI, 80002) IVTNUM                                   02600807
           GO TO 0111                                                   02610807
20110      IVFAIL = IVFAIL + 1                                          02620807
           DVCORR = -1.0D14                                             02630807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    02640807
 0111      CONTINUE                                                     02650807
CT012*  TEST 12                                             5 ARGUMENTS 02660807
           IVTNUM = 12                                                  02670807
        DUDVD = 3.5D0                                                   02680807
        DUEVD = 4.5D0                                                   02690807
        DUAVD = DMIN1(DUDVD, -DUDVD, -DUEVD, +DUDVD, DUEVD)             02700807
           IF (DUAVD + 4.500000003D0) 20120, 10120, 40120               02710807
40120      IF (DUAVD + 4.499999997D0) 10120, 10120, 20120               02720807
10120      IVPASS = IVPASS + 1                                          02730807
           WRITE (NUVI, 80002) IVTNUM                                   02740807
           GO TO 0121                                                   02750807
20120      IVFAIL = IVFAIL + 1                                          02760807
           DVCORR = -4.5D0                                              02770807
           WRITE (NUVI, 80031) IVTNUM, DUAVD, DVCORR                    02780807
 0121      CONTINUE                                                     02790807
C*****                                                                  02800807
CBB** ********************** BBCSUM0  **********************************02810807
C**** WRITE OUT TEST SUMMARY                                            02820807
C****                                                                   02830807
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02840807
      WRITE (I02, 90004)                                                02850807
      WRITE (I02, 90014)                                                02860807
      WRITE (I02, 90004)                                                02870807
      WRITE (I02, 90020) IVPASS                                         02880807
      WRITE (I02, 90022) IVFAIL                                         02890807
      WRITE (I02, 90024) IVDELE                                         02900807
      WRITE (I02, 90026) IVINSP                                         02910807
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02920807
CBE** ********************** BBCSUM0  **********************************02930807
CBB** ********************** BBCFOOT0 **********************************02940807
C**** WRITE OUT REPORT FOOTINGS                                         02950807
C****                                                                   02960807
      WRITE (I02,90016) ZPROG, ZPROG                                    02970807
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02980807
      WRITE (I02,90019)                                                 02990807
CBE** ********************** BBCFOOT0 **********************************03000807
CBB** ********************** BBCFMT0A **********************************03010807
C**** FORMATS FOR TEST DETAIL LINES                                     03020807
C****                                                                   03030807
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03040807
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03050807
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03060807
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03070807
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03080807
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03090807
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03100807
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03110807
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03120807
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03130807
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03140807
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03150807
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03160807
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03170807
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03180807
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03190807
80050 FORMAT (1H ,48X,A31)                                              03200807
CBE** ********************** BBCFMT0A **********************************03210807
CBB** ********************** BBCFMAT1 **********************************03220807
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03230807
C****                                                                   03240807
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03250807
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03260807
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03270807
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03280807
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03290807
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03300807
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03310807
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03320807
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03330807
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03340807
     21H(,F12.5,2H, ,F12.5,1H))                                         03350807
CBE** ********************** BBCFMAT1 **********************************03360807
CBB** ********************** BBCFMT0B **********************************03370807
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03380807
C****                                                                   03390807
90002 FORMAT (1H1)                                                      03400807
90004 FORMAT (1H )                                                      03410807
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03420807
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03430807
90008 FORMAT (1H ,21X,A13,A17)                                          03440807
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03450807
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03460807
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03470807
     1       7X,7HREMARKS,24X)                                          03480807
90014 FORMAT (1H ,46H----------------------------------------------,    03490807
     1        33H---------------------------------)                     03500807
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03510807
C****                                                                   03520807
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03530807
C****                                                                   03540807
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03550807
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03560807
     1        A13)                                                      03570807
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03580807
C****                                                                   03590807
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03600807
C****                                                                   03610807
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03620807
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03630807
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03640807
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03650807
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03660807
CBE** ********************** BBCFMT0B **********************************03670807
C*****    END OF TEST SEGMENT 168                                       03680807
        STOP                                                            03690807
        END                                                             03700807
                                                                        03710807
