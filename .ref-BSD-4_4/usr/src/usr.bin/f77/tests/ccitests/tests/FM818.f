C***********************************************************************00010818
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020818
C*****   FM818                                                          00030818
C*****                       YDLG10 - (185)                             00040818
C*****                                                                  00050818
C***********************************************************************00060818
C*****  GENERAL PURPOSE                                         ANS REF 00070818
C*****    TEST INTRINSIC FUNCTION DLOG10                         15.3   00080818
C*****                                                          TABLE 5 00090818
C*****                                                                  00100818
CBB** ********************** BBCCOMNT **********************************00110818
C****                                                                   00120818
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130818
C****                          VERSION 2.0                              00140818
C****                                                                   00150818
C****                                                                   00160818
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170818
C****                   GENERAL SERVICES ADMINISTRATION                 00180818
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190818
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200818
C****                      FALLS CHURCH, VA. 22041                      00210818
C****                                                                   00220818
C****                          (703) 756-6153                           00230818
C****                                                                   00240818
CBE** ********************** BBCCOMNT **********************************00250818
C*****  S P E C I F I C A T I O N S  SEGMENT 185                        00260818
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00270818
C*****                                                                  00280818
CBB** ********************** BBCINITA **********************************00290818
C**** SPECIFICATION STATEMENTS                                          00300818
C****                                                                   00310818
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320818
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330818
CBE** ********************** BBCINITA **********************************00340818
CBB** ********************** BBCINITB **********************************00350818
C**** INITIALIZE SECTION                                                00360818
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370818
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380818
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390818
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400818
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410818
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420818
      DATA   REMRKS /'                               '/                 00430818
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440818
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450818
C****                                                                   00460818
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470818
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480818
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490818
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560818
      IVPASS = 0                                                        00570818
      IVFAIL = 0                                                        00580818
      IVDELE = 0                                                        00590818
      IVINSP = 0                                                        00600818
      IVTOTL = 0                                                        00610818
      IVTOTN = 0                                                        00620818
      ICZERO = 0                                                        00630818
C                                                                       00640818
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650818
      I01 = 05                                                          00660818
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670818
      I02 = 06                                                          00680818
C                                                                       00690818
      I01 = 5                                                           00700818
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710818
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720818
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730818
C                                                                       00740818
      I02 = 6                                                           00750818
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760818
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770818
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780818
C                                                                       00790818
CBE** ********************** BBCINITB **********************************00800818
      NUVI = I02                                                        00810818
      IVTOTL = 16                                                       00820818
      ZPROG = 'FM818'                                                   00830818
CBB** ********************** BBCHED0A **********************************00840818
C****                                                                   00850818
C**** WRITE REPORT TITLE                                                00860818
C****                                                                   00870818
      WRITE (I02, 90002)                                                00880818
      WRITE (I02, 90006)                                                00890818
      WRITE (I02, 90007)                                                00900818
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910818
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920818
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930818
CBE** ********************** BBCHED0A **********************************00940818
C*****                                                                  00950818
C*****    HEADER FOR SEGMENT 185                                        00960818
        WRITE(NUVI,18500)                                               00970818
18500   FORMAT(1H , / 36H  YDLG10 - (185) INTRINSIC FUNCTIONS//         00980818
     1         44H  DLOG10 (DOUBLE PRECISION COMMON LOGARITHM)//        00990818
     2         17H  ANS REF. - 15.3)                                    01000818
CBB** ********************** BBCHED0B **********************************01010818
C**** WRITE DETAIL REPORT HEADERS                                       01020818
C****                                                                   01030818
      WRITE (I02,90004)                                                 01040818
      WRITE (I02,90004)                                                 01050818
      WRITE (I02,90013)                                                 01060818
      WRITE (I02,90014)                                                 01070818
      WRITE (I02,90015) IVTOTL                                          01080818
CBE** ********************** BBCHED0B **********************************01090818
C*****                                                                  01100818
CT001*  TEST 1                                 ONE, SINCE LN(1.0) = 0.0 01110818
           IVTNUM = 1                                                   01120818
        BVD = 1.0D0                                                     01130818
        AVD = DLOG10(BVD)                                               01140818
           IF (AVD + 0.5000000000D-09) 20010, 10010, 40010              01150818
40010      IF (AVD - 0.5000000000D-09) 10010, 10010, 20010              01160818
10010      IVPASS = IVPASS + 1                                          01170818
           WRITE (NUVI, 80002) IVTNUM                                   01180818
           GO TO 0011                                                   01190818
20010      IVFAIL = IVFAIL + 1                                          01200818
           DVCORR = 0.00000000000000000000D+00                          01210818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01220818
 0011      CONTINUE                                                     01230818
CT002*  TEST 2                                      A VALUE CLOSE TO 10 01240818
           IVTNUM = 2                                                   01250818
        AVD = DLOG10(9.875D0)                                           01260818
           IF (AVD - 0.9945371038D+00) 20020, 10020, 40020              01270818
40020      IF (AVD - 0.9945371048D+00) 10020, 10020, 20020              01280818
10020      IVPASS = IVPASS + 1                                          01290818
           WRITE (NUVI, 80002) IVTNUM                                   01300818
           GO TO 0021                                                   01310818
20020      IVFAIL = IVFAIL + 1                                          01320818
           DVCORR = 0.99453710429849784235D+00                          01330818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01340818
 0021      CONTINUE                                                     01350818
CT003*  TEST 3                                          THE VALUE 10.D0 01360818
           IVTNUM = 3                                                   01370818
        AVD = DLOG10(10.0D0)                                            01380818
           IF (AVD - 0.9999999995D+00) 20030, 10030, 40030              01390818
40030      IF (AVD - 0.1000000001D+01) 10030, 10030, 20030              01400818
10030      IVPASS = IVPASS + 1                                          01410818
           WRITE (NUVI, 80002) IVTNUM                                   01420818
           GO TO 0031                                                   01430818
20030      IVFAIL = IVFAIL + 1                                          01440818
           DVCORR = 1.0000000000000000000D+00                           01450818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01460818
 0031      CONTINUE                                                     01470818
CT004*  TEST 4                                         THE VALUE 20.5D0 01480818
           IVTNUM = 4                                                   01490818
        AVD = DLOG10(20.5D0)                                            01500818
           IF (AVD - 0.1311753860D+01) 20040, 10040, 40040              01510818
40040      IF (AVD - 0.1311753862D+01) 10040, 10040, 20040              01520818
10040      IVPASS = IVPASS + 1                                          01530818
           WRITE (NUVI, 80002) IVTNUM                                   01540818
           GO TO 0041                                                   01550818
20040      IVFAIL = IVFAIL + 1                                          01560818
           DVCORR = 1.3117538610557542993D+00                           01570818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01580818
 0041      CONTINUE                                                     01590818
CT005*  TEST 5                                         THE VALUE 99.0D0 01600818
           IVTNUM = 5                                                   01610818
        AVD = DLOG10(99.0D0)                                            01620818
           IF (AVD - 0.1995635193D+01) 20050, 10050, 40050              01630818
40050      IF (AVD - 0.1995635196D+01) 10050, 10050, 20050              01640818
10050      IVPASS = IVPASS + 1                                          01650818
           WRITE (NUVI, 80002) IVTNUM                                   01660818
           GO TO 0051                                                   01670818
20050      IVFAIL = IVFAIL + 1                                          01680818
           DVCORR = 1.9956351945975499153D+00                           01690818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01700818
 0051      CONTINUE                                                     01710818
CT006*  TEST 6                            VARIABLE WITHIN AN EXPRESSION 01720818
           IVTNUM = 6                                                   01730818
        BVD = 1.0D0                                                     01740818
        CVD = 8.0D0                                                     01750818
        AVD = DLOG10(3.0D0 * BVD / CVD)                                 01760818
           IF (AVD + 0.4259687325D+00) 20060, 10060, 40060              01770818
40060      IF (AVD + 0.4259687320D+00) 10060, 10060, 20060              01780818
10060      IVPASS = IVPASS + 1                                          01790818
           WRITE (NUVI, 80002) IVTNUM                                   01800818
           GO TO 0061                                                   01810818
20060      IVFAIL = IVFAIL + 1                                          01820818
           DVCORR = -0.42596873227228114835D+00                         01830818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01840818
 0061      CONTINUE                                                     01850818
CT007*  TEST 7                            VARIABLE WITHIN AN EXPRESSION 01860818
           IVTNUM = 7                                                   01870818
        BVD = 1.0D0                                                     01880818
        CVD = 8.0D0                                                     01890818
        AVD = DLOG10(5.0D0 * BVD / CVD)                                 01900818
           IF (AVD + 0.2041199828D+00) 20070, 10070, 40070              01910818
40070      IF (AVD + 0.2041199825D+00) 10070, 10070, 20070              01920818
10070      IVPASS = IVPASS + 1                                          01930818
           WRITE (NUVI, 80002) IVTNUM                                   01940818
           GO TO 0071                                                   01950818
20070      IVFAIL = IVFAIL + 1                                          01960818
           DVCORR = -0.20411998265592478085D+00                         01970818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01980818
 0071      CONTINUE                                                     01990818
CT008*  TEST 8                         AN EXPRESSION SUPPLIED TO DLOG10 02000818
           IVTNUM = 8                                                   02010818
        AVD = DLOG10(75.D0 / 100.0D0)                                   02020818
           IF (AVD + 0.1249387367D+00) 20080, 10080, 40080              02030818
40080      IF (AVD + 0.1249387365D+00) 10080, 10080, 20080              02040818
10080      IVPASS = IVPASS + 1                                          02050818
           WRITE (NUVI, 80002) IVTNUM                                   02060818
           GO TO 0081                                                   02070818
20080      IVFAIL = IVFAIL + 1                                          02080818
           DVCORR = -0.12493873660829995313D+00                         02090818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02100818
 0081      CONTINUE                                                     02110818
CT009*  TEST 9                            VARIABLE WITHIN AN EXPRESSION 02120818
           IVTNUM = 9                                                   02130818
        BVD = 1.0D0                                                     02140818
        CVD = 8.0D0                                                     02150818
        AVD = DLOG10(7.0D0 * BVD / CVD)                                 02160818
           IF (AVD + 0.5799194701D-01) 20090, 10090, 40090              02170818
40090      IF (AVD + 0.5799194694D-01) 10090, 10090, 20090              02180818
10090      IVPASS = IVPASS + 1                                          02190818
           WRITE (NUVI, 80002) IVTNUM                                   02200818
           GO TO 0091                                                   02210818
20090      IVFAIL = IVFAIL + 1                                          02220818
           DVCORR = -0.057991946977686754929D+00                        02230818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02240818
 0091      CONTINUE                                                     02250818
CT010*  TEST 10                                    A VALUE CLOSE TO ONE 02260818
           IVTNUM = 10                                                  02270818
        AVD = DLOG10(0.9921875D0)                                       02280818
           IF (AVD + 0.3406248694D-02) 20100, 10100, 40100              02290818
40100      IF (AVD + 0.3406248690D-02) 10100, 10100, 20100              02300818
10100      IVPASS = IVPASS + 1                                          02310818
           WRITE (NUVI, 80002) IVTNUM                                   02320818
           GO TO 0101                                                   02330818
20100      IVFAIL = IVFAIL + 1                                          02340818
           DVCORR = -0.0034062486919115022492D+00                       02350818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02360818
 0101      CONTINUE                                                     02370818
CT011*  TEST 11                                   A VALUE CLOSE TO ONE  02380818
           IVTNUM = 11                                                  02390818
        BVD = 1.0009765625                                              02400818
        AVD = DLOG10(BVD)                                               02410818
           IF (AVD - 0.4239087517D-03) 20110, 10110, 40110              02420818
40110      IF (AVD - 0.4239087522D-03) 10110, 10110, 20110              02430818
10110      IVPASS = IVPASS + 1                                          02440818
           WRITE (NUVI, 80002) IVTNUM                                   02450818
           GO TO 0111                                                   02460818
20110      IVFAIL = IVFAIL + 1                                          02470818
           DVCORR = 0.00042390875196115194455D+00                       02480818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02490818
70111      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       02490*TI
           WRITE (NUVI,70111)                                           02490*TI
 0111      CONTINUE                                                     02500818
CT012*  TEST 12                                   A VALUE CLOSE TO ZERO 02510818
           IVTNUM = 12                                                  02520818
        BVD = 256.0D0                                                   02530818
        AVD = DLOG10(1.0D0 / BVD)                                       02540818
           IF (AVD + 0.2408239967D+01) 20120, 10120, 40120              02550818
40120      IF (AVD + 0.2408239964D+01) 10120, 10120, 20120              02560818
10120      IVPASS = IVPASS + 1                                          02570818
           WRITE (NUVI, 80002) IVTNUM                                   02580818
           GO TO 0121                                                   02590818
20120      IVFAIL = IVFAIL + 1                                          02600818
           DVCORR = -2.4082399653118495617D+00                          02610818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02620818
 0121      CONTINUE                                                     02630818
CT013*  TEST 13                                   A VALUE CLOSE TO ZERO 02640818
           IVTNUM = 13                                                  02650818
        BVD = 128.0D0                                                   02660818
        AVD = DLOG10(1.0D0 / (BVD * 8D0))                               02670818
           IF (AVD + 0.3010299959D+01) 20130, 10130, 40130              02680818
40130      IF (AVD + 0.3010299955D+01) 10130, 10130, 20130              02690818
10130      IVPASS = IVPASS + 1                                          02700818
           WRITE (NUVI, 80002) IVTNUM                                   02710818
           GO TO 0131                                                   02720818
20130      IVFAIL = IVFAIL + 1                                          02730818
           DVCORR = -3.0102999566398119521D+00                          02740818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02750818
 0131      CONTINUE                                                     02760818
CT014*  TEST 14                           AN ARGUMENT OF HIGH MAGNITUDE 02770818
           IVTNUM = 14                                                  02780818
        BVD = 2.0D+35                                                   02790818
        AVD = DLOG10(BVD)                                               02800818
           IF (AVD - 0.3530102997D+01) 20140, 10140, 40140              02810818
40140      IF (AVD - 0.3530103002D+02) 10140, 10140, 20140              02820818
10140      IVPASS = IVPASS + 1                                          02830818
           WRITE (NUVI, 80002) IVTNUM                                   02840818
           GO TO 0141                                                   02850818
20140      IVFAIL = IVFAIL + 1                                          02860818
           DVCORR = 35.301029995663981195D+00                           02870818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02880818
 0141      CONTINUE                                                     02890818
CT015*  TEST 15                            AN ARGUMENT OF LOW MAGNITUDE 02900818
           IVTNUM = 15                                                  02910818
        BVD = 2.0D-35                                                   02920818
        AVD = DLOG10(BVD)                                               02930818
           IF (AVD + 0.3469897003D+02) 20150, 10150, 40150              02940818
40150      IF (AVD + 0.3469896998D+02) 10150, 10150, 20150              02950818
10150      IVPASS = IVPASS + 1                                          02960818
           WRITE (NUVI, 80002) IVTNUM                                   02970818
           GO TO 0151                                                   02980818
20150      IVFAIL = IVFAIL + 1                                          02990818
           DVCORR = -34.698970004336018805D+00                          03000818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03010818
 0151      CONTINUE                                                     03020818
CT016*  TEST 16                               THE FUNCTION APPIED TWICE 03030818
           IVTNUM = 16                                                  03040818
        AVD = DLOG10(20.0D0) - DLOG10(2.0D0)                            03050818
           IF (AVD - 0.9999999995D+00) 20160, 10160, 40160              03060818
40160      IF (AVD - 0.1000000001D+01) 10160, 10160, 20160              03070818
10160      IVPASS = IVPASS + 1                                          03080818
           WRITE (NUVI, 80002) IVTNUM                                   03090818
           GO TO 0161                                                   03100818
20160      IVFAIL = IVFAIL + 1                                          03110818
           DVCORR = 1.00000000000000D+00                                03120818
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03130818
 0161      CONTINUE                                                     03140818
C*****                                                                  03150818
CBB** ********************** BBCSUM0  **********************************03160818
C**** WRITE OUT TEST SUMMARY                                            03170818
C****                                                                   03180818
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03190818
      WRITE (I02, 90004)                                                03200818
      WRITE (I02, 90014)                                                03210818
      WRITE (I02, 90004)                                                03220818
      WRITE (I02, 90020) IVPASS                                         03230818
      WRITE (I02, 90022) IVFAIL                                         03240818
      WRITE (I02, 90024) IVDELE                                         03250818
      WRITE (I02, 90026) IVINSP                                         03260818
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03270818
CBE** ********************** BBCSUM0  **********************************03280818
CBB** ********************** BBCFOOT0 **********************************03290818
C**** WRITE OUT REPORT FOOTINGS                                         03300818
C****                                                                   03310818
      WRITE (I02,90016) ZPROG, ZPROG                                    03320818
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03330818
      WRITE (I02,90019)                                                 03340818
CBE** ********************** BBCFOOT0 **********************************03350818
CBB** ********************** BBCFMT0A **********************************03360818
C**** FORMATS FOR TEST DETAIL LINES                                     03370818
C****                                                                   03380818
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03390818
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03400818
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03410818
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03420818
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03430818
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03440818
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03450818
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03460818
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03470818
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03480818
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03490818
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03500818
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03510818
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03520818
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03530818
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03540818
80050 FORMAT (1H ,48X,A31)                                              03550818
CBE** ********************** BBCFMT0A **********************************03560818
CBB** ********************** BBCFMAT1 **********************************03570818
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03580818
C****                                                                   03590818
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03600818
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03610818
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03620818
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03630818
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03640818
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03650818
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03660818
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03670818
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03680818
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03690818
     21H(,F12.5,2H, ,F12.5,1H))                                         03700818
CBE** ********************** BBCFMAT1 **********************************03710818
CBB** ********************** BBCFMT0B **********************************03720818
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03730818
C****                                                                   03740818
90002 FORMAT (1H1)                                                      03750818
90004 FORMAT (1H )                                                      03760818
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03770818
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03780818
90008 FORMAT (1H ,21X,A13,A17)                                          03790818
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03800818
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03810818
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03820818
     1       7X,7HREMARKS,24X)                                          03830818
90014 FORMAT (1H ,46H----------------------------------------------,    03840818
     1        33H---------------------------------)                     03850818
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03860818
C****                                                                   03870818
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03880818
C****                                                                   03890818
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03900818
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03910818
     1        A13)                                                      03920818
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03930818
C****                                                                   03940818
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03950818
C****                                                                   03960818
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03970818
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03980818
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03990818
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04000818
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04010818
CBE** ********************** BBCFMT0B **********************************04020818
C*****    END OF TEST SEGMENT 185                                       04030818
      STOP                                                              04040818
      END                                                               04050818
                                                                        04060818
