C***********************************************************************00010813
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020813
C*****   FM813                                                          00030813
C*****                       YCSQRT - (177)                             00040813
C*****                                                                  00050813
C***********************************************************************00060813
C*****  GENERAL PURPOSE                                         ANS REF 00070813
C*****    TEST INTRINSIC FUNCTION CSQRT                          15.3   00080813
C*****                                                          TABLE 5 00090813
C*****                                                                  00100813
CBB** ********************** BBCCOMNT **********************************00110813
C****                                                                   00120813
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130813
C****                          VERSION 2.0                              00140813
C****                                                                   00150813
C****                                                                   00160813
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170813
C****                   GENERAL SERVICES ADMINISTRATION                 00180813
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190813
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200813
C****                      FALLS CHURCH, VA. 22041                      00210813
C****                                                                   00220813
C****                          (703) 756-6153                           00230813
C****                                                                   00240813
CBE** ********************** BBCCOMNT **********************************00250813
C*****                                                                  00260813
C*****  S P E C F I C A T I O N S  SEGMENT 177                          00270813
        COMPLEX AVC, BVC, CVC, ZVCORR                                   00280813
        REAL R2E(2)                                                     00290813
        EQUIVALENCE (AVC, R2E)                                          00300813
C*****                                                                  00310813
CBB** ********************** BBCINITA **********************************00320813
C**** SPECIFICATION STATEMENTS                                          00330813
C****                                                                   00340813
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350813
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360813
CBE** ********************** BBCINITA **********************************00370813
CBB** ********************** BBCINITB **********************************00380813
C**** INITIALIZE SECTION                                                00390813
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400813
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410813
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420813
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430813
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440813
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450813
      DATA   REMRKS /'                               '/                 00460813
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470813
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480813
C****                                                                   00490813
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500813
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510813
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520813
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590813
      IVPASS = 0                                                        00600813
      IVFAIL = 0                                                        00610813
      IVDELE = 0                                                        00620813
      IVINSP = 0                                                        00630813
      IVTOTL = 0                                                        00640813
      IVTOTN = 0                                                        00650813
      ICZERO = 0                                                        00660813
C                                                                       00670813
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680813
      I01 = 05                                                          00690813
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700813
      I02 = 06                                                          00710813
C                                                                       00720813
      I01 = 5                                                           00730813
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740813
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750813
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760813
C                                                                       00770813
      I02 = 6                                                           00780813
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790813
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800813
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810813
C                                                                       00820813
CBE** ********************** BBCINITB **********************************00830813
      NUVI = I02                                                        00840813
      IVTOTL = 13                                                       00850813
      ZPROG = 'FM813'                                                   00860813
CBB** ********************** BBCHED0A **********************************00870813
C****                                                                   00880813
C**** WRITE REPORT TITLE                                                00890813
C****                                                                   00900813
      WRITE (I02, 90002)                                                00910813
      WRITE (I02, 90006)                                                00920813
      WRITE (I02, 90007)                                                00930813
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940813
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950813
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960813
CBE** ********************** BBCHED0A **********************************00970813
C*****                                                                  00980813
C*****    HEADER FOR SEGMENT 177                                        00990813
        WRITE(NUVI,17700)                                               01000813
17700   FORMAT(1H , / 36H  YCSQRT - (177) INTRINSIC FUNCTIONS//         01010813
     1         29H  CSQRT (COMPLEX SQUARE ROOT)//                       01020813
     2         17H  ANS REF. - 15.3)                                    01030813
CBB** ********************** BBCHED0B **********************************01040813
C**** WRITE DETAIL REPORT HEADERS                                       01050813
C****                                                                   01060813
      WRITE (I02,90004)                                                 01070813
      WRITE (I02,90004)                                                 01080813
      WRITE (I02,90013)                                                 01090813
      WRITE (I02,90014)                                                 01100813
      WRITE (I02,90015) IVTOTL                                          01110813
CBE** ********************** BBCHED0B **********************************01120813
C*****                                                                  01130813
CT001*  TEST 1                                                   ZERO   01140813
           IVTNUM = 1                                                   01150813
        BVC = (0.0, 0.0)                                                01160813
        AVC = CSQRT(BVC)                                                01170813
           IF (R2E(1) + 0.50000E-04) 20010, 40012, 40011                01180813
40011      IF (R2E(1) - 0.50000E-04) 40012, 40012, 20010                01190813
40012      IF (R2E(2) + 0.50000E-04) 20010, 10010, 40010                01200813
40010      IF (R2E(2) - 0.50000E-04) 10010, 10010, 20010                01210813
10010      IVPASS = IVPASS + 1                                          01220813
           WRITE (NUVI, 80002) IVTNUM                                   01230813
           GO TO 0011                                                   01240813
20010      IVFAIL = IVFAIL + 1                                          01250813
           ZVCORR = (0.00000000000000, 0.00000000000000)                01260813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01270813
 0011      CONTINUE                                                     01280813
CT002*  TEST 2                                  POSITIVE REAL NUMBERS   01290813
           IVTNUM = 2                                                   01300813
        BVC = (4.0, 4.0)                                                01310813
        AVC = CSQRT(BVC - (0.0, 4.0))                                   01320813
           IF (R2E(1) - 0.19999E+01) 20020, 40022, 40021                01330813
40021      IF (R2E(1) - 0.20001E+01) 40022, 40022, 20020                01340813
40022      IF (R2E(2) + 0.50000E-04) 20020, 10020, 40020                01350813
40020      IF (R2E(2) - 0.50000E-04) 10020, 10020, 20020                01360813
10020      IVPASS = IVPASS + 1                                          01370813
           WRITE (NUVI, 80002) IVTNUM                                   01380813
           GO TO 0021                                                   01390813
20020      IVFAIL = IVFAIL + 1                                          01400813
           ZVCORR = (2.00000000000000, 0.00000000000000)                01410813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01420813
 0021      CONTINUE                                                     01430813
CT003*  TEST 3                                    POSITIVE REAL NUMBERS 01440813
           IVTNUM = 3                                                   01450813
        BVC = (4.0, 4.0)                                                01460813
        CVC = (4.0, -4.0)                                               01470813
        AVC = CSQRT(BVC + CVC)                                          01480813
           IF (R2E(1) - 0.28282E+01) 20030, 40032, 40031                01490813
40031      IF (R2E(1) - 0.28286E+01) 40032, 40032, 20030                01500813
40032      IF (R2E(2) + 0.50000E-04) 20030, 10030, 40030                01510813
40030      IF (R2E(2) - 0.50000E-04) 10030, 10030, 20030                01520813
10030      IVPASS = IVPASS + 1                                          01530813
           WRITE (NUVI, 80002) IVTNUM                                   01540813
           GO TO 0031                                                   01550813
20030      IVFAIL = IVFAIL + 1                                          01560813
           ZVCORR = (2.8284271247462, 0.00000000000000)                 01570813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01580813
 0031      CONTINUE                                                     01590813
CT004*  TEST 4                                    POSITIVE REAL NUMBERS 01600813
           IVTNUM = 4                                                   01610813
        BVC = (4.0, 0.0)                                                01620813
        CVC = BVC + (5.0, 0.0)                                          01630813
        AVC = CSQRT(CVC)                                                01640813
           IF (R2E(1) - 0.29998E+01) 20040, 40042, 40041                01650813
40041      IF (R2E(1) - 0.30002E+01) 40042, 40042, 20040                01660813
40042      IF (R2E(2) + 0.50000E-04) 20040, 10040, 40040                01670813
40040      IF (R2E(2) - 0.50000E-04) 10040, 10040, 20040                01680813
10040      IVPASS = IVPASS + 1                                          01690813
           WRITE (NUVI, 80002) IVTNUM                                   01700813
           GO TO 0041                                                   01710813
20040      IVFAIL = IVFAIL + 1                                          01720813
           ZVCORR = (3.00000000000000, 0.00000000000000)                01730813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01740813
 0041      CONTINUE                                                     01750813
CT005*  TEST  5                                 NEGATIVE REAL NUMBERS   01760813
           IVTNUM = 5                                                   01770813
        BVC = (-1.0, 0.0)                                               01780813
        AVC = CSQRT(BVC)                                                01790813
           IF (R2E(1) + 0.50000E-04) 20050, 40052, 40051                01800813
40051      IF (R2E(1) - 0.50000E-04) 40052, 40052, 20050                01810813
40052      IF (R2E(2) - 0.99995E+00) 20050, 10050, 40050                01820813
40050      IF (R2E(2) - 0.10001E+01) 10050, 10050, 20050                01830813
10050      IVPASS = IVPASS + 1                                          01840813
           WRITE (NUVI, 80002) IVTNUM                                   01850813
           GO TO 0051                                                   01860813
20050      IVFAIL = IVFAIL + 1                                          01870813
           ZVCORR = (0.00000000000000, 1.0000000000000)                 01880813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      01890813
 0051      CONTINUE                                                     01900813
CT006*  TEST 6                                    NEGATIVE REAL NUMBERS 01910813
           IVTNUM = 6                                                   01920813
        AVC = CSQRT((-5.0, 0.0))                                        01930813
           IF (R2E(1) + 0.50000E-04) 20060, 40062, 40061                01940813
40061      IF (R2E(1) - 0.50000E-04) 40062, 40062, 20060                01950813
40062      IF (R2E(2) - 0.22359E+01) 20060, 10060, 40060                01960813
40060      IF (R2E(2) - 0.22362E+01) 10060, 10060, 20060                01970813
10060      IVPASS = IVPASS + 1                                          01980813
           WRITE (NUVI, 80002) IVTNUM                                   01990813
           GO TO 0061                                                   02000813
20060      IVFAIL = IVFAIL + 1                                          02010813
           ZVCORR = (0.00000000000000, 2.2360679774998)                 02020813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02030813
 0061      CONTINUE                                                     02040813
CT007*  TEST 7                                    NEGATIVE REAL NUMBERS 02050813
           IVTNUM = 7                                                   02060813
        BVC = (-25.0, 0.0)                                              02070813
        AVC = CSQRT(BVC)                                                02080813
           IF (R2E(1) + 0.50000E-04) 20070, 40072, 40071                02090813
40071      IF (R2E(1) - 0.50000E-04) 40072, 40072, 20070                02100813
40072      IF (R2E(2) - 0.49997E+01) 20070, 10070, 40070                02110813
40070      IF (R2E(2) - 0.50003E+01) 10070, 10070, 20070                02120813
10070      IVPASS = IVPASS + 1                                          02130813
           WRITE (NUVI, 80002) IVTNUM                                   02140813
           GO TO 0071                                                   02150813
20070      IVFAIL = IVFAIL + 1                                          02160813
           ZVCORR = (0.00000000000000, 5.0000000000000)                 02170813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02180813
 0071      CONTINUE                                                     02190813
CT008*  TEST 8                  VARIABLES SUPPLIED WITHIN AN EXPRESSION 02200813
           IVTNUM = 8                                                   02210813
        BVC = (0.203125,0.0)                                            02220813
        CVC = (0.0, 1.3125)                                             02230813
        AVC = CSQRT(BVC + CVC)                                          02240813
           IF (R2E(1) - 0.87495E+00) 20080, 40082, 40081                02250813
40081      IF (R2E(1) - 0.87505E+00) 40082, 40082, 20080                02260813
40082      IF (R2E(2) - 0.74996E+00) 20080, 10080, 40080                02270813
40080      IF (R2E(2) - 0.75004E+00) 10080, 10080, 20080                02280813
10080      IVPASS = IVPASS + 1                                          02290813
           WRITE (NUVI, 80002) IVTNUM                                   02300813
           GO TO 0081                                                   02310813
20080      IVFAIL = IVFAIL + 1                                          02320813
           ZVCORR = (0.87500000000000, 0.75000000000000)                02330813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02340813
 0081      CONTINUE                                                     02350813
CT009*  TEST 9                  VARIABLES SUPPLIED WITHIN AN EXPRESSION 02360813
           IVTNUM = 9                                                   02370813
        BVC = (1.0,0.0)                                                 02380813
        AVC = CSQRT(BVC - (0.38671875, 0.515625))                       02390813
           IF (R2E(1) - 0.84094E+00) 20090, 40092, 40091                02400813
40091      IF (R2E(1) - 0.84103E+00) 40092, 40092, 20090                02410813
40092      IF (R2E(2) + 0.30658E+00) 20090, 10090, 40090                02420813
40090      IF (R2E(2) + 0.30654E+00) 10090, 10090, 20090                02430813
10090      IVPASS = IVPASS + 1                                          02440813
           WRITE (NUVI, 80002) IVTNUM                                   02450813
           GO TO 0091                                                   02460813
20090      IVFAIL = IVFAIL + 1                                          02470813
           ZVCORR = (0.84098742159541, -0.30655928183909)               02480813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02490813
 0091      CONTINUE                                                     02500813
CT010*  TEST 10                 VARIABLES SUPPLIED WITHIN AN EXPRESSION 02510813
           IVTNUM = 10                                                  02520813
        BVC = (-0.375, 0.5)                                             02530813
        AVC = CSQRT(BVC + BVC)                                          02540813
           IF (R2E(1) - 0.49997E+00) 20100, 40102, 40101                02550813
40101      IF (R2E(1) - 0.50003E+00) 40102, 40102, 20100                02560813
40102      IF (R2E(2) - 0.99995E+00) 20100, 10100, 40100                02570813
40100      IF (R2E(2) - 0.10001E+01) 10100, 10100, 20100                02580813
10100      IVPASS = IVPASS + 1                                          02590813
           WRITE (NUVI, 80002) IVTNUM                                   02600813
           GO TO 0101                                                   02610813
20100      IVFAIL = IVFAIL + 1                                          02620813
           ZVCORR = (0.50000000000000, 1.0000000000000)                 02630813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02640813
 0101      CONTINUE                                                     02650813
CT011*  TEST 11                              PURELY IMAGINARY NUMBERS   02660813
           IVTNUM = 11                                                  02670813
        AVC = CSQRT((0.0, 2.0))                                         02680813
           IF (R2E(1) - 0.99995E+00) 20110, 40112, 40111                02690813
40111      IF (R2E(1) - 0.10001E+01) 40112, 40112, 20110                02700813
40112      IF (R2E(2) - 0.99995E+00) 20110, 10110, 40110                02710813
40110      IF (R2E(2) - 0.10001E+01) 10110, 10110, 20110                02720813
10110      IVPASS = IVPASS + 1                                          02730813
           WRITE (NUVI, 80002) IVTNUM                                   02740813
           GO TO 0111                                                   02750813
20110      IVFAIL = IVFAIL + 1                                          02760813
           ZVCORR = (1.00000000000000, 1.0000000000000)                 02770813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02780813
 0111      CONTINUE                                                     02790813
CT012*  TEST 12                                PURELY IMAGINARY NUMBERS 02800813
           IVTNUM = 12                                                  02810813
        AVC = CSQRT((0.0, -8.0))                                        02820813
           IF (R2E(1) - 0.19999E+01) 20120, 40122, 40121                02830813
40121      IF (R2E(1) - 0.20001E+01) 40122, 40122, 20120                02840813
40122      IF (R2E(2) + 0.20001E+01) 20120, 10120, 40120                02850813
40120      IF (R2E(2) + 0.19999E+01) 10120, 10120, 20120                02860813
10120      IVPASS = IVPASS + 1                                          02870813
           WRITE (NUVI, 80002) IVTNUM                                   02880813
           GO TO 0121                                                   02890813
20120      IVFAIL = IVFAIL + 1                                          02900813
           ZVCORR = (2.00000000000000, -2.0000000000000)                02910813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      02920813
 0121      CONTINUE                                                     02930813
CT013*  TEST 13                                      (-0.5,SQRT(3)/2)   02940813
           IVTNUM = 13                                                  02950813
        BVC = (-0.5, -0.8660254038)                                     02960813
        CVC = CSQRT(CSQRT(BVC))                                         02970813
        AVC = CVC - BVC * (0.0, 1.0)                                    02980813
           IF (R2E(1) + 0.50000E-04) 20130, 40132, 40131                02990813
40131      IF (R2E(1) - 0.50000E-04) 40132, 40132, 20130                03000813
40132      IF (R2E(2) + 0.50000E-04) 20130, 10130, 40130                03010813
40130      IF (R2E(2) - 0.50000E-04) 10130, 10130, 20130                03020813
10130      IVPASS = IVPASS + 1                                          03030813
           WRITE (NUVI, 80002) IVTNUM                                   03040813
           GO TO 0131                                                   03050813
20130      IVFAIL = IVFAIL + 1                                          03060813
           ZVCORR = (0.00000000000000, 0.00000000000000)                03070813
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      03080813
 0131      CONTINUE                                                     03090813
C*****                                                                  03100813
CBB** ********************** BBCSUM0  **********************************03110813
C**** WRITE OUT TEST SUMMARY                                            03120813
C****                                                                   03130813
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03140813
      WRITE (I02, 90004)                                                03150813
      WRITE (I02, 90014)                                                03160813
      WRITE (I02, 90004)                                                03170813
      WRITE (I02, 90020) IVPASS                                         03180813
      WRITE (I02, 90022) IVFAIL                                         03190813
      WRITE (I02, 90024) IVDELE                                         03200813
      WRITE (I02, 90026) IVINSP                                         03210813
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03220813
CBE** ********************** BBCSUM0  **********************************03230813
CBB** ********************** BBCFOOT0 **********************************03240813
C**** WRITE OUT REPORT FOOTINGS                                         03250813
C****                                                                   03260813
      WRITE (I02,90016) ZPROG, ZPROG                                    03270813
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03280813
      WRITE (I02,90019)                                                 03290813
CBE** ********************** BBCFOOT0 **********************************03300813
CBB** ********************** BBCFMT0A **********************************03310813
C**** FORMATS FOR TEST DETAIL LINES                                     03320813
C****                                                                   03330813
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03340813
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03350813
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03360813
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03370813
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03380813
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03390813
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03400813
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03410813
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03420813
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03430813
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03440813
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03450813
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03460813
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03470813
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03480813
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03490813
80050 FORMAT (1H ,48X,A31)                                              03500813
CBE** ********************** BBCFMT0A **********************************03510813
CBB** ********************** BBCFMAT1 **********************************03520813
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03530813
C****                                                                   03540813
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03550813
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03560813
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03570813
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03580813
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03590813
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03600813
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03610813
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03620813
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03630813
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03640813
     21H(,F12.5,2H, ,F12.5,1H))                                         03650813
CBE** ********************** BBCFMAT1 **********************************03660813
CBB** ********************** BBCFMT0B **********************************03670813
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03680813
C****                                                                   03690813
90002 FORMAT (1H1)                                                      03700813
90004 FORMAT (1H )                                                      03710813
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03720813
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03730813
90008 FORMAT (1H ,21X,A13,A17)                                          03740813
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03750813
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03760813
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03770813
     1       7X,7HREMARKS,24X)                                          03780813
90014 FORMAT (1H ,46H----------------------------------------------,    03790813
     1        33H---------------------------------)                     03800813
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03810813
C****                                                                   03820813
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03830813
C****                                                                   03840813
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03850813
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03860813
     1        A13)                                                      03870813
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03880813
C****                                                                   03890813
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03900813
C****                                                                   03910813
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03920813
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03930813
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03940813
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03950813
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03960813
CBE** ********************** BBCFMT0B **********************************03970813
C*****                                                                  03980813
C*****    END OF TEST SEGMENT 177                                       03990813
      STOP                                                              04000813
      END                                                               04010813
                                                                        04020813
