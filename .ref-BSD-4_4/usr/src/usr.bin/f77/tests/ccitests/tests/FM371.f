C***********************************************************************00010371
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020371
C*****   FM371                                                          00030371
C*****                       XALG10 - (184)                             00040371
C*****                                                                  00050371
C***********************************************************************00060371
C*****  GENERAL PURPOSE                                      SUBSET REF 00070371
C*****    TEST INTRINSIC FUNCTION ALOG10                        15.3    00080371
C*****                                                        TABLE 5   00090371
C*****                                                                  00100371
CBB** ********************** BBCCOMNT **********************************00110371
C****                                                                   00120371
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130371
C****                          VERSION 2.0                              00140371
C****                                                                   00150371
C****                                                                   00160371
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170371
C****                   GENERAL SERVICES ADMINISTRATION                 00180371
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190371
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200371
C****                      FALLS CHURCH, VA. 22041                      00210371
C****                                                                   00220371
C****                          (703) 756-6153                           00230371
C****                                                                   00240371
CBE** ********************** BBCCOMNT **********************************00250371
CBB** ********************** BBCINITA **********************************00260371
C**** SPECIFICATION STATEMENTS                                          00270371
C****                                                                   00280371
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290371
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300371
CBE** ********************** BBCINITA **********************************00310371
CBB** ********************** BBCINITB **********************************00320371
C**** INITIALIZE SECTION                                                00330371
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340371
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350371
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360371
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370371
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380371
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390371
      DATA   REMRKS /'                               '/                 00400371
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410371
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420371
C****                                                                   00430371
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440371
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450371
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460371
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530371
      IVPASS = 0                                                        00540371
      IVFAIL = 0                                                        00550371
      IVDELE = 0                                                        00560371
      IVINSP = 0                                                        00570371
      IVTOTL = 0                                                        00580371
      IVTOTN = 0                                                        00590371
      ICZERO = 0                                                        00600371
C                                                                       00610371
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620371
      I01 = 05                                                          00630371
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640371
      I02 = 06                                                          00650371
C                                                                       00660371
      I01 = 5                                                           00670371
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680371
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690371
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700371
C                                                                       00710371
      I02 = 6                                                           00720371
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730371
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740371
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750371
C                                                                       00760371
CBE** ********************** BBCINITB **********************************00770371
      NUVI = I02                                                        00780371
      IVTOTL = 16                                                       00790371
      ZPROG = 'FM371'                                                   00800371
CBB** ********************** BBCHED0A **********************************00810371
C****                                                                   00820371
C**** WRITE REPORT TITLE                                                00830371
C****                                                                   00840371
      WRITE (I02, 90002)                                                00850371
      WRITE (I02, 90006)                                                00860371
      WRITE (I02, 90007)                                                00870371
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880371
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890371
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900371
CBE** ********************** BBCHED0A **********************************00910371
C*****                                                                  00920371
C*****    HEADER FOR SEGMENT 184                                        00930371
        WRITE(NUVI,18400)                                               00940371
18400   FORMAT(1H , / 36H  XALG10 - (184) INTRINSIC FUNCTIONS//         00950371
     1         27H  ALOG10 (COMMON LOGARITHM)//                         00960371
     2         20H  SUBSET REF. - 15.3)                                 00970371
CBB** ********************** BBCHED0B **********************************00980371
C**** WRITE DETAIL REPORT HEADERS                                       00990371
C****                                                                   01000371
      WRITE (I02,90004)                                                 01010371
      WRITE (I02,90004)                                                 01020371
      WRITE (I02,90013)                                                 01030371
      WRITE (I02,90014)                                                 01040371
      WRITE (I02,90015) IVTOTL                                          01050371
CBE** ********************** BBCHED0B **********************************01060371
C*****                                                                  01070371
CT001*  TEST 1                                 ONE, SINCE LN(1.0) = 0.0 01080371
           IVTNUM = 1                                                   01090371
        BVS = 1.0                                                       01100371
        AVS = ALOG10(BVS)                                               01110371
           IF (AVS + 0.50000E-04) 20010, 10010, 40010                   01120371
40010      IF (AVS - 0.50000E-04) 10010, 10010, 20010                   01130371
10010      IVPASS = IVPASS + 1                                          01140371
           WRITE (NUVI, 80002) IVTNUM                                   01150371
           GO TO 0011                                                   01160371
20010      IVFAIL = IVFAIL + 1                                          01170371
           RVCORR = 0.00000000000000                                    01180371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01190371
 0011      CONTINUE                                                     01200371
CT002*  TEST 2                                     A VALUE CLOSE TO TEN 01210371
           IVTNUM = 2                                                   01220371
        AVS = ALOG10(9.875)                                             01230371
           IF (AVS - 0.99448E+00) 20020, 10020, 40020                   01240371
40020      IF (AVS - 0.99459E+00) 10020, 10020, 20020                   01250371
10020      IVPASS = IVPASS + 1                                          01260371
           WRITE (NUVI, 80002) IVTNUM                                   01270371
           GO TO 0021                                                   01280371
20020      IVFAIL = IVFAIL + 1                                          01290371
           RVCORR = 0.99453710429850                                    01300371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01310371
 0021      CONTINUE                                                     01320371
CT003*  TEST 3                                           THE VALUE 10.0 01330371
           IVTNUM = 3                                                   01340371
        AVS = ALOG10(10.0)                                              01350371
           IF (AVS - 0.99995E+00) 20030, 10030, 40030                   01360371
40030      IF (AVS - 0.10001E+01) 10030, 10030, 20030                   01370371
10030      IVPASS = IVPASS + 1                                          01380371
           WRITE (NUVI, 80002) IVTNUM                                   01390371
           GO TO 0031                                                   01400371
20030      IVFAIL = IVFAIL + 1                                          01410371
           RVCORR = 1.00000000000000                                    01420371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01430371
 0031      CONTINUE                                                     01440371
CT004*  TEST 4                                           THE VALUE 20.5 01450371
           IVTNUM = 4                                                   01460371
        AVS = ALOG10(20.5)                                              01470371
           IF (AVS - 0.13116E+01) 20040, 10040, 40040                   01480371
40040      IF (AVS - 0.13119E+01) 10040, 10040, 20040                   01490371
10040      IVPASS = IVPASS + 1                                          01500371
           WRITE (NUVI, 80002) IVTNUM                                   01510371
           GO TO 0041                                                   01520371
20040      IVFAIL = IVFAIL + 1                                          01530371
           RVCORR = 1.31175386105575                                    01540371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01550371
 0041      CONTINUE                                                     01560371
CT005*  TEST 5                                           THE VALUE 99.0 01570371
           IVTNUM = 5                                                   01580371
        AVS = ALOG10(99.0)                                              01590371
           IF (AVS - 0.19955E+01) 20050, 10050, 40050                   01600371
40050      IF (AVS - 0.19958E+01) 10050, 10050, 20050                   01610371
10050      IVPASS = IVPASS + 1                                          01620371
           WRITE (NUVI, 80002) IVTNUM                                   01630371
           GO TO 0051                                                   01640371
20050      IVFAIL = IVFAIL + 1                                          01650371
           RVCORR = 1.99563519459755                                    01660371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01670371
 0051      CONTINUE                                                     01680371
CT006*  TEST 6                           VARIABLES WITHIN AN EXPRESSION 01690371
           IVTNUM = 6                                                   01700371
        BVS = 1.0                                                       01710371
        CVS = 8.0                                                       01720371
        AVS = ALOG10(3.0 * BVS / CVS)                                   01730371
           IF (AVS + 0.42599E+00) 20060, 10060, 40060                   01740371
40060      IF (AVS + 0.42594E+00) 10060, 10060, 20060                   01750371
10060      IVPASS = IVPASS + 1                                          01760371
           WRITE (NUVI, 80002) IVTNUM                                   01770371
           GO TO 0061                                                   01780371
20060      IVFAIL = IVFAIL + 1                                          01790371
           RVCORR = -0.42596873227228                                   01800371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01810371
 0061      CONTINUE                                                     01820371
CT007*  TEST 7                           VARIABLES WITHIN AN EXPRESSION 01830371
           IVTNUM = 7                                                   01840371
        BVS = 1.0                                                       01850371
        CVS = 8.0                                                       01860371
        AVS = ALOG10(5.0 * BVS / CVS)                                   01870371
           IF (AVS + 0.20413E+00) 20070, 10070, 40070                   01880371
40070      IF (AVS + 0.20411E+00) 10070, 10070, 20070                   01890371
10070      IVPASS = IVPASS + 1                                          01900371
           WRITE (NUVI, 80002) IVTNUM                                   01910371
           GO TO 0071                                                   01920371
20070      IVFAIL = IVFAIL + 1                                          01930371
           RVCORR = -0.20411998265592                                   01940371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      01950371
 0071      CONTINUE                                                     01960371
CT008*  TEST 8                         AN EXPRESSION SUPPLIED TO ALOG10 01970371
           IVTNUM = 8                                                   01980371
        AVS = ALOG10(75.0 / 100.0)                                      01990371
           IF (AVS + 0.12495E+00) 20080, 10080, 40080                   02000371
40080      IF (AVS + 0.12493E+00) 10080, 10080, 20080                   02010371
10080      IVPASS = IVPASS + 1                                          02020371
           WRITE (NUVI, 80002) IVTNUM                                   02030371
           GO TO 0081                                                   02040371
20080      IVFAIL = IVFAIL + 1                                          02050371
           RVCORR = -0.12493873660830                                   02060371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02070371
 0081      CONTINUE                                                     02080371
CT009*  TEST 9                           VARIABLES WITHIN AN EXPRESSION 02090371
           IVTNUM = 9                                                   02100371
        BVS = 1.0                                                       02110371
        CVS = 8.0                                                       02120371
        AVS = ALOG10(7.0 * BVS / CVS)                                   02130371
           IF (AVS + 0.57995E-01) 20090, 10090, 40090                   02140371
40090      IF (AVS + 0.57989E-01) 10090, 10090, 20090                   02150371
10090      IVPASS = IVPASS + 1                                          02160371
           WRITE (NUVI, 80002) IVTNUM                                   02170371
           GO TO 0091                                                   02180371
20090      IVFAIL = IVFAIL + 1                                          02190371
           RVCORR = -0.05799194697769                                   02200371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02210371
 0091      CONTINUE                                                     02220371
CT010*  TEST 10                                    A VALUE CLOSE TO ONE 02230371
           IVTNUM = 10                                                  02240371
        AVS = ALOG10(0.9921875)                                         02250371
           IF (AVS + 0.34065E-02) 20100, 10100, 40100                   02260371
40100      IF (AVS + 0.34060E-02) 10100, 10100, 20100                   02270371
10100      IVPASS = IVPASS + 1                                          02280371
           WRITE (NUVI, 80002) IVTNUM                                   02290371
           GO TO 0101                                                   02300371
20100      IVFAIL = IVFAIL + 1                                          02310371
           RVCORR = -0.0034062486919115                                 02320371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02330371
 0101      CONTINUE                                                     02340371
CT011*  TEST 11                                    A VALUE CLOSE TO ONE 02350371
           IVTNUM = 11                                                  02360371
        BVS = 1.0009765625                                              02370371
        AVS = ALOG10(BVS)                                               02380371
           IF (AVS - 0.42388E-03) 20110, 10110, 40110                   02390371
40110      IF (AVS - 0.42393E-03) 10110, 10110, 20110                   02400371
10110      IVPASS = IVPASS + 1                                          02410371
           WRITE (NUVI, 80002) IVTNUM                                   02420371
           GO TO 0111                                                   02430371
20110      IVFAIL = IVFAIL + 1                                          02440371
           RVCORR = 0.00042390875196115                                 02450371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02460371
70111      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       02460*TI
           WRITE (NUVI,70111)                                           02460*TI
 0111      CONTINUE                                                     02470371
CT012*  TEST 12                                   A VALUE CLOSE TO ZERO 02480371
           IVTNUM = 12                                                  02490371
        BVS = 256.0                                                     02500371
        AVS = ALOG10(1.0 / BVS)                                         02510371
           IF (AVS + 0.24084E+01) 20120, 10120, 40120                   02520371
40120      IF (AVS + 0.24081E+01) 10120, 10120, 20120                   02530371
10120      IVPASS = IVPASS + 1                                          02540371
           WRITE (NUVI, 80002) IVTNUM                                   02550371
           GO TO 0121                                                   02560371
20120      IVFAIL = IVFAIL + 1                                          02570371
           RVCORR = -2.40823996531185                                   02580371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02590371
 0121      CONTINUE                                                     02600371
CT013*  TEST 13                                   A VALUE CLOSE TO ZERO 02610371
           IVTNUM = 13                                                  02620371
        BVS = 128.0                                                     02630371
        AVS = ALOG10(1.0 / (BVS * 8.0))                                 02640371
           IF (AVS + 0.30105E+01) 20130, 10130, 40130                   02650371
40130      IF (AVS + 0.30101E+01) 10130, 10130, 20130                   02660371
10130      IVPASS = IVPASS + 1                                          02670371
           WRITE (NUVI, 80002) IVTNUM                                   02680371
           GO TO 0131                                                   02690371
20130      IVFAIL = IVFAIL + 1                                          02700371
           RVCORR = -3.01029995663981                                   02710371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02720371
 0131      CONTINUE                                                     02730371
CT014*  TEST 14                           AN ARGUMENT OF HIGH MAGNITUDE 02740371
           IVTNUM = 14                                                  02750371
        BVS = 2.0E+35                                                   02760371
        AVS = ALOG10(BVS)                                               02770371
           IF (AVS - 0.35299E+02) 20140, 10140, 40140                   02780371
40140      IF (AVS - 0.35303E+02) 10140, 10140, 20140                   02790371
10140      IVPASS = IVPASS + 1                                          02800371
           WRITE (NUVI, 80002) IVTNUM                                   02810371
           GO TO 0141                                                   02820371
20140      IVFAIL = IVFAIL + 1                                          02830371
           RVCORR = 35.30102999566398                                   02840371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02850371
 0141      CONTINUE                                                     02860371
CT015*  TEST 15                            AN ARGUMENT OF LOW MAGNITUDE 02870371
           IVTNUM = 15                                                  02880371
        BVS = 2.0E-35                                                   02890371
        AVS = ALOG10(BVS)                                               02900371
           IF (AVS + 0.34701E+02) 20150, 10150, 40150                   02910371
40150      IF (AVS + 0.34697E+02) 10150, 10150, 20150                   02920371
10150      IVPASS = IVPASS + 1                                          02930371
           WRITE (NUVI, 80002) IVTNUM                                   02940371
           GO TO 0151                                                   02950371
20150      IVFAIL = IVFAIL + 1                                          02960371
           RVCORR = -34.69897000433602                                  02970371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      02980371
 0151      CONTINUE                                                     02990371
CT016*  TEST 16                              THE FUNCTION APPLIED TWICE 03000371
           IVTNUM = 16                                                  03010371
        AVS = ALOG10(20.0) - ALOG10(2.0)                                03020371
           IF (AVS - 0.99995E+00) 20160, 10160, 40160                   03030371
40160      IF (AVS - 0.10001E+01) 10160, 10160, 20160                   03040371
10160      IVPASS = IVPASS + 1                                          03050371
           WRITE (NUVI, 80002) IVTNUM                                   03060371
           GO TO 0161                                                   03070371
20160      IVFAIL = IVFAIL + 1                                          03080371
           RVCORR = 1.0000000                                           03090371
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03100371
 0161      CONTINUE                                                     03110371
C*****                                                                  03120371
CBB** ********************** BBCSUM0  **********************************03130371
C**** WRITE OUT TEST SUMMARY                                            03140371
C****                                                                   03150371
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03160371
      WRITE (I02, 90004)                                                03170371
      WRITE (I02, 90014)                                                03180371
      WRITE (I02, 90004)                                                03190371
      WRITE (I02, 90020) IVPASS                                         03200371
      WRITE (I02, 90022) IVFAIL                                         03210371
      WRITE (I02, 90024) IVDELE                                         03220371
      WRITE (I02, 90026) IVINSP                                         03230371
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03240371
CBE** ********************** BBCSUM0  **********************************03250371
CBB** ********************** BBCFOOT0 **********************************03260371
C**** WRITE OUT REPORT FOOTINGS                                         03270371
C****                                                                   03280371
      WRITE (I02,90016) ZPROG, ZPROG                                    03290371
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03300371
      WRITE (I02,90019)                                                 03310371
CBE** ********************** BBCFOOT0 **********************************03320371
CBB** ********************** BBCFMT0A **********************************03330371
C**** FORMATS FOR TEST DETAIL LINES                                     03340371
C****                                                                   03350371
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03360371
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03370371
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03380371
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03390371
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03400371
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03410371
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03420371
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03430371
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03440371
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03450371
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03460371
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03470371
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03480371
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03490371
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03500371
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03510371
80050 FORMAT (1H ,48X,A31)                                              03520371
CBE** ********************** BBCFMT0A **********************************03530371
CBB** ********************** BBCFMT0B **********************************03540371
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03550371
C****                                                                   03560371
90002 FORMAT (1H1)                                                      03570371
90004 FORMAT (1H )                                                      03580371
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03590371
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03600371
90008 FORMAT (1H ,21X,A13,A17)                                          03610371
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03620371
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03630371
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03640371
     1       7X,7HREMARKS,24X)                                          03650371
90014 FORMAT (1H ,46H----------------------------------------------,    03660371
     1        33H---------------------------------)                     03670371
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03680371
C****                                                                   03690371
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03700371
C****                                                                   03710371
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03720371
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03730371
     1        A13)                                                      03740371
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03750371
C****                                                                   03760371
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03770371
C****                                                                   03780371
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03790371
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03800371
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03810371
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03820371
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03830371
CBE** ********************** BBCFMT0B **********************************03840371
C*****                                                                  03850371
C*****    END OF TEST SEGMENT 184                                       03860371
      STOP                                                              03870371
      END                                                               03880371
                                                                        03890371
