C***********************************************************************00010825
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020825
C*****   FM825                                                          00030825
C*****                       YDSINH - (198)                             00040825
C*****                                                                  00050825
C***********************************************************************00060825
C*****  GENERAL PURPOSE                                         ANS REF 00070825
C*****    TEST INTRINSIC FUNCTION DSINH, DCOSH                   15.3   00080825
C*****                                                          TABLE 5 00090825
CBB** ********************** BBCCOMNT **********************************00100825
C****                                                                   00110825
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120825
C****                          VERSION 2.0                              00130825
C****                                                                   00140825
C****                                                                   00150825
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160825
C****                   GENERAL SERVICES ADMINISTRATION                 00170825
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180825
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190825
C****                      FALLS CHURCH, VA. 22041                      00200825
C****                                                                   00210825
C****                          (703) 756-6153                           00220825
C****                                                                   00230825
CBE** ********************** BBCCOMNT **********************************00240825
C*****                                                                  00250825
C*****    S P E C I F I C A T I O N S SEGMENT 198                       00260825
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00270825
C*****                                                                  00280825
CBB** ********************** BBCINITA **********************************00290825
C**** SPECIFICATION STATEMENTS                                          00300825
C****                                                                   00310825
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320825
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330825
CBE** ********************** BBCINITA **********************************00340825
CBB** ********************** BBCINITB **********************************00350825
C**** INITIALIZE SECTION                                                00360825
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370825
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380825
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390825
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400825
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410825
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420825
      DATA   REMRKS /'                               '/                 00430825
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440825
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450825
C****                                                                   00460825
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470825
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480825
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490825
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560825
      IVPASS = 0                                                        00570825
      IVFAIL = 0                                                        00580825
      IVDELE = 0                                                        00590825
      IVINSP = 0                                                        00600825
      IVTOTL = 0                                                        00610825
      IVTOTN = 0                                                        00620825
      ICZERO = 0                                                        00630825
C                                                                       00640825
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650825
      I01 = 05                                                          00660825
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670825
      I02 = 06                                                          00680825
C                                                                       00690825
      I01 = 5                                                           00700825
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710825
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720825
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730825
C                                                                       00740825
      I02 = 6                                                           00750825
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760825
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770825
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780825
C                                                                       00790825
CBE** ********************** BBCINITB **********************************00800825
      NUVI = I02                                                        00810825
      IVTOTL = 16                                                       00820825
      ZPROG = 'FM825'                                                   00830825
CBB** ********************** BBCHED0A **********************************00840825
C****                                                                   00850825
C**** WRITE REPORT TITLE                                                00860825
C****                                                                   00870825
      WRITE (I02, 90002)                                                00880825
      WRITE (I02, 90006)                                                00890825
      WRITE (I02, 90007)                                                00900825
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910825
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920825
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930825
CBE** ********************** BBCHED0A **********************************00940825
C*****                                                                  00950825
C*****    HEADER FOR SEGMENT 198                                        00960825
        WRITE(NUVI,19800)                                               00970825
19800   FORMAT(1H , / 36H  YDSINH - (198) INTRINSIC FUNCTIONS//         00980825
     1  57H  DSINH, DCOSH (DOUBLE PRECISION HYPERBOLIC SINE, COSINE)//  00990825
     2  17H  ANS REF. - 15.3)                                           01000825
CBB** ********************** BBCHED0B **********************************01010825
C**** WRITE DETAIL REPORT HEADERS                                       01020825
C****                                                                   01030825
      WRITE (I02,90004)                                                 01040825
      WRITE (I02,90004)                                                 01050825
      WRITE (I02,90013)                                                 01060825
      WRITE (I02,90014)                                                 01070825
      WRITE (I02,90015) IVTOTL                                          01080825
CBE** ********************** BBCHED0B **********************************01090825
C*****                                                                  01100825
        WRITE(NUVI,19801)                                               01110825
19801   FORMAT(/ 8X, 13HTEST OF DSINH)                                  01120825
C*****                                                                  01130825
CT001*  TEST 1                                     TEST AT ZERO (0.0D0) 01140825
           IVTNUM = 1                                                   01150825
        BVD = 0.0D0                                                     01160825
        AVD = DSINH(BVD)                                                01170825
           IF (AVD +  0.5000000000D-09) 20010, 10010, 40010             01180825
40010      IF (AVD -  0.5000000000D-09) 10010, 10010, 20010             01190825
10010      IVPASS = IVPASS + 1                                          01200825
           WRITE (NUVI, 80002) IVTNUM                                   01210825
           GO TO 0011                                                   01220825
20010      IVFAIL = IVFAIL + 1                                          01230825
           DVCORR =  0.00000000000000000000D+00                         01240825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01250825
 0011      CONTINUE                                                     01260825
CT002*  TEST 2                            TEST ARGUMENTS CLOSE TO 1.0D0 01270825
           IVTNUM = 2                                                   01280825
        AVD = DSINH(15.0D0 / 16.0D0)                                    01290825
           IF (AVD -  0.1080991915D+01) 20020, 10020, 40020             01300825
40020      IF (AVD -  0.1080991917D+01) 10020, 10020, 20020             01310825
10020      IVPASS = IVPASS + 1                                          01320825
           WRITE (NUVI, 80002) IVTNUM                                   01330825
           GO TO 0021                                                   01340825
20020      IVFAIL = IVFAIL + 1                                          01350825
           DVCORR =  1.0809919156930639401D+00                          01360825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01370825
 0021      CONTINUE                                                     01380825
CT003*  TEST 3                                            TEST AT 1.0D0 01390825
           IVTNUM = 3                                                   01400825
        BVD = 1.0D0                                                     01410825
        AVD = DSINH(BVD)                                                01420825
           IF (AVD -  0.1175201193D+01) 20030, 10030, 40030             01430825
40030      IF (AVD -  0.1175201195D+01) 10030, 10030, 20030             01440825
10030      IVPASS = IVPASS + 1                                          01450825
           WRITE (NUVI, 80002) IVTNUM                                   01460825
           GO TO 0031                                                   01470825
20030      IVFAIL = IVFAIL + 1                                          01480825
           DVCORR =  1.1752011936438014569D+00                          01490825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01500825
 0031      CONTINUE                                                     01510825
CT004*  TEST 4                            TEST ARGUMENTS CLOSE TO 1.0D0 01520825
           IVTNUM = 4                                                   01530825
        AVD = DSINH(33.0D0 / 32.0D0)                                    01540825
           IF (AVD -  0.1224004187D+01) 20040, 10040, 40040             01550825
40040      IF (AVD -  0.1224004189D+01) 10040, 10040, 20040             01560825
10040      IVPASS = IVPASS + 1                                          01570825
           WRITE (NUVI, 80002) IVTNUM                                   01580825
           GO TO 0041                                                   01590825
20040      IVFAIL = IVFAIL + 1                                          01600825
           DVCORR =  1.2240041877866398138D+00                          01610825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01620825
 0041      CONTINUE                                                     01630825
CT005*  TEST 5                                            TEST AT 2.0D0 01640825
           IVTNUM = 5                                                   01650825
        BVD = 2.0D0                                                     01660825
        AVD = DSINH(BVD)                                                01670825
           IF (AVD -  0.3626860406D+01) 20050, 10050, 40050             01680825
40050      IF (AVD -  0.3626860410D+01) 10050, 10050, 20050             01690825
10050      IVPASS = IVPASS + 1                                          01700825
           WRITE (NUVI, 80002) IVTNUM                                   01710825
           GO TO 0051                                                   01720825
20050      IVFAIL = IVFAIL + 1                                          01730825
           DVCORR =  3.6268604078470187677D+00                          01740825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01750825
 0051      CONTINUE                                                     01760825
CT006*  TEST 6                                      A NEGATIVE ARGUMENT 01770825
           IVTNUM = 6                                                   01780825
        AVD = DSINH(-2.0D0)                                             01790825
           IF (AVD +  0.3626860410D+01) 20060, 10060, 40060             01800825
40060      IF (AVD +  0.3626860406D+01) 10060, 10060, 20060             01810825
10060      IVPASS = IVPASS + 1                                          01820825
           WRITE (NUVI, 80002) IVTNUM                                   01830825
           GO TO 0061                                                   01840825
20060      IVFAIL = IVFAIL + 1                                          01850825
           DVCORR = -3.6268604078470187677D+00                          01860825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01870825
 0061      CONTINUE                                                     01880825
CT007*  TEST 7                             AN ARGUMENT OF LOW MAGNITUDE 01890825
           IVTNUM = 7                                                   01900825
        AVD = DSINH(1.0D-14)                                            01910825
           IF (AVD -  0.9999999995D-14) 20070, 10070, 40070             01920825
40070      IF (AVD -  0.1000000001D-13) 10070, 10070, 20070             01930825
10070      IVPASS = IVPASS + 1                                          01940825
           WRITE (NUVI, 80002) IVTNUM                                   01950825
           GO TO 0071                                                   01960825
20070      IVFAIL = IVFAIL + 1                                          01970825
           DVCORR =  1.0000000000000000000D-14                          01980825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01990825
 0071      CONTINUE                                                     02000825
C*****                                                                  02010825
        WRITE (NUVI, 90002)                                             02020825
        WRITE (NUVI, 90013)                                             02030825
        WRITE (NUVI, 90014)                                             02040825
C*****                                                                  02050825
        WRITE(NUVI,19809)                                               02060825
19809   FORMAT(/ 08X, 13HTEST OF DCOSH)                                 02070825
C*****                                                                  02080825
CT008*  TEST 8                                     TEST AT ZERO (0.0D0) 02090825
           IVTNUM = 8                                                   02100825
        BVD = 0.0D0                                                     02110825
        AVD = DCOSH(BVD)                                                02120825
           IF (AVD -  0.9999999995D+00) 20080, 10080, 40080             02130825
40080      IF (AVD -  0.1000000001D+01) 10080, 10080, 20080             02140825
10080      IVPASS = IVPASS + 1                                          02150825
           WRITE (NUVI, 80002) IVTNUM                                   02160825
           GO TO 0081                                                   02170825
20080      IVFAIL = IVFAIL + 1                                          02180825
           DVCORR =  1.0000000000000000000D+00                          02190825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02200825
 0081      CONTINUE                                                     02210825
CT009*  TEST 9                                    VALUES CLOSE TO 1.0D0 02220825
           IVTNUM = 9                                                   02230825
        AVD = DCOSH(15.0D0 / 16.0D0)                                    02240825
           IF (AVD -  0.1472597541D+01) 20090, 10090, 40090             02250825
40090      IF (AVD -  0.1472597543D+01) 10090, 10090, 20090             02260825
10090      IVPASS = IVPASS + 1                                          02270825
           WRITE (NUVI, 80002) IVTNUM                                   02280825
           GO TO 0091                                                   02290825
20090      IVFAIL = IVFAIL + 1                                          02300825
           DVCORR =  1.4725975423698629333D+00                          02310825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02320825
 0091      CONTINUE                                                     02330825
CT010*  TEST 10                           TEST ARGUMENTS CLOSE TO 1.0D0 02340825
           IVTNUM = 10                                                  02350825
        BVD = 1.0D0                                                     02360825
        AVD = DCOSH(BVD)                                                02370825
           IF (AVD -  0.1543080634D+01) 20100, 10100, 40100             02380825
40100      IF (AVD -  0.1543080636D+01) 10100, 10100, 20100             02390825
10100      IVPASS = IVPASS + 1                                          02400825
           WRITE (NUVI, 80002) IVTNUM                                   02410825
           GO TO 0101                                                   02420825
20100      IVFAIL = IVFAIL + 1                                          02430825
           DVCORR =  1.5430806348152437785D+00                          02440825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02450825
 0101      CONTINUE                                                     02460825
CT011*  TEST 11                           TEST ARGUMENTS CLOSE TO 1.0D0 02470825
           IVTNUM = 11                                                  02480825
        AVD = DCOSH(33.0D0 / 32.0D0)                                    02490825
           IF (AVD -  0.1580565167D+01) 20110, 10110, 40110             02500825
40110      IF (AVD -  0.1580565170D+01) 10110, 10110, 20110             02510825
10110      IVPASS = IVPASS + 1                                          02520825
           WRITE (NUVI, 80002) IVTNUM                                   02530825
           GO TO 0111                                                   02540825
20110      IVFAIL = IVFAIL + 1                                          02550825
           DVCORR =  1.5805651684505867982D+00                          02560825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02570825
 0111      CONTINUE                                                     02580825
CT012*  TEST 12                                           TEST AT 2.0D0 02590825
           IVTNUM = 12                                                  02600825
        BVD = 2.0D0                                                     02610825
        AVD = DCOSH(BVD)                                                02620825
           IF (AVD -  0.3762195689D+01) 20120, 10120, 40120             02630825
40120      IF (AVD -  0.3762195693D+01) 10120, 10120, 20120             02640825
10120      IVPASS = IVPASS + 1                                          02650825
           WRITE (NUVI, 80002) IVTNUM                                   02660825
           GO TO 0121                                                   02670825
20120      IVFAIL = IVFAIL + 1                                          02680825
           DVCORR =  3.7621956910836314596D+00                          02690825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02700825
 0121      CONTINUE                                                     02710825
CT013*  TEST 13                                     A NEGATIVE ARGUMENT 02720825
           IVTNUM = 13                                                  02730825
        AVD = DCOSH(-2.0D0)                                             02740825
           IF (AVD -  0.3762195689D+01) 20130, 10130, 40130             02750825
40130      IF (AVD -  0.3762195693D+01) 10130, 10130, 20130             02760825
10130      IVPASS = IVPASS + 1                                          02770825
           WRITE (NUVI, 80002) IVTNUM                                   02780825
           GO TO 0131                                                   02790825
20130      IVFAIL = IVFAIL + 1                                          02800825
           DVCORR =  3.7621956910836314596D+00                          02810825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02820825
 0131      CONTINUE                                                     02830825
CT014*  TEST 14                            AN ARGUMENT OF LOW MAGNITUDE 02840825
           IVTNUM = 14                                                  02850825
        AVD = DCOSH(-1.0D-14)                                           02860825
           IF (AVD -  0.9999999995D+00) 20140, 10140, 40140             02870825
40140      IF (AVD -  0.1000000001D+01) 10140, 10140, 20140             02880825
10140      IVPASS = IVPASS + 1                                          02890825
           WRITE (NUVI, 80002) IVTNUM                                   02900825
           GO TO 0141                                                   02910825
20140      IVFAIL = IVFAIL + 1                                          02920825
           DVCORR =  1.0000000000000000000D+00                          02930825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02940825
 0141      CONTINUE                                                     02950825
CT015*  TEST 15                   NEGATIVE VALUES SUPPLIED AS ARGUMENTS 02960825
C*****                               TO BOTH FUNCTIONS IN AN EXPRESSION 02970825
           IVTNUM = 15                                                  02980825
        BVD = DSINH(-3.145D0) ** 2                                      02990825
        CVD = DCOSH(-3.145D0) ** 2                                      03000825
        AVD = CVD - BVD                                                 03010825
           IF (AVD -  0.9999999990D+00) 20150, 10150, 40150             03020825
40150      IF (AVD -  0.1000000001D+01) 10150, 10150, 20150             03030825
10150      IVPASS = IVPASS + 1                                          03040825
           WRITE (NUVI, 80002) IVTNUM                                   03050825
           GO TO 0151                                                   03060825
20150      IVFAIL = IVFAIL + 1                                          03070825
           DVCORR =  1.0000000000000000000D+00                          03080825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03090825
 0151      CONTINUE                                                     03100825
CT016*  TEST 16                   POSITIVE VALUES SUPPLIED AS ARGUMENTS 03110825
C*****                               TO BOTH FUNCTIONS IN AN EXPRESSION 03120825
           IVTNUM = 16                                                  03130825
        AVD = DSINH(3.25D0) + DCOSH(3.25D0)                             03140825
           IF (AVD -  0.2579033990D+02) 20160, 10160, 40160             03150825
40160      IF (AVD -  0.2579033993D+02) 10160, 10160, 20160             03160825
10160      IVPASS = IVPASS + 1                                          03170825
           WRITE (NUVI, 80002) IVTNUM                                   03180825
           GO TO 0161                                                   03190825
20160      IVFAIL = IVFAIL + 1                                          03200825
           DVCORR = 25.790339917193062089D+00                           03210825
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03220825
 0161      CONTINUE                                                     03230825
C*****                                                                  03240825
CBB** ********************** BBCSUM0  **********************************03250825
C**** WRITE OUT TEST SUMMARY                                            03260825
C****                                                                   03270825
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03280825
      WRITE (I02, 90004)                                                03290825
      WRITE (I02, 90014)                                                03300825
      WRITE (I02, 90004)                                                03310825
      WRITE (I02, 90020) IVPASS                                         03320825
      WRITE (I02, 90022) IVFAIL                                         03330825
      WRITE (I02, 90024) IVDELE                                         03340825
      WRITE (I02, 90026) IVINSP                                         03350825
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03360825
CBE** ********************** BBCSUM0  **********************************03370825
CBB** ********************** BBCFOOT0 **********************************03380825
C**** WRITE OUT REPORT FOOTINGS                                         03390825
C****                                                                   03400825
      WRITE (I02,90016) ZPROG, ZPROG                                    03410825
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03420825
      WRITE (I02,90019)                                                 03430825
CBE** ********************** BBCFOOT0 **********************************03440825
CBB** ********************** BBCFMT0A **********************************03450825
C**** FORMATS FOR TEST DETAIL LINES                                     03460825
C****                                                                   03470825
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03480825
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03490825
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03500825
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03510825
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03520825
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03530825
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03540825
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03550825
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03560825
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03570825
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03580825
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03590825
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03600825
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03610825
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03620825
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03630825
80050 FORMAT (1H ,48X,A31)                                              03640825
CBE** ********************** BBCFMT0A **********************************03650825
CBB** ********************** BBCFMAT1 **********************************03660825
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03670825
C****                                                                   03680825
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03690825
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03700825
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03710825
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03720825
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03730825
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03740825
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03750825
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03760825
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03770825
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03780825
     21H(,F12.5,2H, ,F12.5,1H))                                         03790825
CBE** ********************** BBCFMAT1 **********************************03800825
CBB** ********************** BBCFMT0B **********************************03810825
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03820825
C****                                                                   03830825
90002 FORMAT (1H1)                                                      03840825
90004 FORMAT (1H )                                                      03850825
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03860825
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03870825
90008 FORMAT (1H ,21X,A13,A17)                                          03880825
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03890825
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03900825
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03910825
     1       7X,7HREMARKS,24X)                                          03920825
90014 FORMAT (1H ,46H----------------------------------------------,    03930825
     1        33H---------------------------------)                     03940825
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03950825
C****                                                                   03960825
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03970825
C****                                                                   03980825
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03990825
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04000825
     1        A13)                                                      04010825
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04020825
C****                                                                   04030825
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04040825
C****                                                                   04050825
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04060825
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04070825
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04080825
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04090825
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04100825
CBE** ********************** BBCFMT0B **********************************04110825
C*****                                                                  04120825
C*****    END OF TEST SEGMENT 198                                       04130825
      STOP                                                              04140825
      END                                                               04150825
