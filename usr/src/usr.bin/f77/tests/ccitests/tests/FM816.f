C***********************************************************************00010816
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020816
C*****   FM816                                                          00030816
C*****                       YDLOG - (182)                              00040816
C*****                                                                  00050816
C***********************************************************************00060816
C*****  GENERAL PURPOSE                                         ANS REF 00070816
C*****    TEST INTRINSIC FUNCTION DLOG                           15.3   00080816
C*****                                                          TABLE 5 00090816
CBB** ********************** BBCCOMNT **********************************00100816
C****                                                                   00110816
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120816
C****                          VERSION 2.0                              00130816
C****                                                                   00140816
C****                                                                   00150816
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160816
C****                   GENERAL SERVICES ADMINISTRATION                 00170816
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180816
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190816
C****                      FALLS CHURCH, VA. 22041                      00200816
C****                                                                   00210816
C****                          (703) 756-6153                           00220816
C****                                                                   00230816
CBE** ********************** BBCCOMNT **********************************00240816
C*****                                                                  00250816
C*****  S P E C I F I C A T I O N S  SEGMENT 182                        00260816
        DOUBLE PRECISION AVD, BVD, CVD, DVCORR                          00270816
C*****                                                                  00280816
CBB** ********************** BBCINITA **********************************00290816
C**** SPECIFICATION STATEMENTS                                          00300816
C****                                                                   00310816
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320816
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330816
CBE** ********************** BBCINITA **********************************00340816
CBB** ********************** BBCINITB **********************************00350816
C**** INITIALIZE SECTION                                                00360816
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370816
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380816
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390816
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400816
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410816
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420816
      DATA   REMRKS /'                               '/                 00430816
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440816
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450816
C****                                                                   00460816
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470816
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480816
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490816
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560816
      IVPASS = 0                                                        00570816
      IVFAIL = 0                                                        00580816
      IVDELE = 0                                                        00590816
      IVINSP = 0                                                        00600816
      IVTOTL = 0                                                        00610816
      IVTOTN = 0                                                        00620816
      ICZERO = 0                                                        00630816
C                                                                       00640816
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650816
      I01 = 05                                                          00660816
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670816
      I02 = 06                                                          00680816
C                                                                       00690816
      I01 = 5                                                           00700816
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710816
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720816
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730816
C                                                                       00740816
      I02 = 6                                                           00750816
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760816
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770816
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780816
C                                                                       00790816
CBE** ********************** BBCINITB **********************************00800816
      NUVI = I02                                                        00810816
      IVTOTL = 16                                                       00820816
      ZPROG = 'FM816'                                                   00830816
CBB** ********************** BBCHED0A **********************************00840816
C****                                                                   00850816
C**** WRITE REPORT TITLE                                                00860816
C****                                                                   00870816
      WRITE (I02, 90002)                                                00880816
      WRITE (I02, 90006)                                                00890816
      WRITE (I02, 90007)                                                00900816
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910816
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920816
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930816
CBE** ********************** BBCHED0A **********************************00940816
C*****                                                                  00950816
C*****    HEADER FOR SEGMENT 182                                        00960816
        WRITE(NUVI,18200)                                               00970816
18200   FORMAT(1H , / 35H  YDLOG - (182) INTRINSIC FUNCTIONS//          00980816
     1         43H  DLOG (DOUBLE PRECISION NATURAL LOGARITHM)//         00990816
     2         17H  ANS REF. - 15.3)                                    01000816
CBB** ********************** BBCHED0B **********************************01010816
C**** WRITE DETAIL REPORT HEADERS                                       01020816
C****                                                                   01030816
      WRITE (I02,90004)                                                 01040816
      WRITE (I02,90004)                                                 01050816
      WRITE (I02,90013)                                                 01060816
      WRITE (I02,90014)                                                 01070816
      WRITE (I02,90015) IVTOTL                                          01080816
CBE** ********************** BBCHED0B **********************************01090816
C*****                                                                  01100816
CT001*  TEST 1                               ONE, SINCE LN(1.0) = 0.0   01110816
           IVTNUM = 1                                                   01120816
        BVD = 1.0D0                                                     01130816
        AVD = DLOG(BVD)                                                 01140816
           IF (AVD + 0.5000000000D-09) 20010, 10010, 40010              01150816
40010      IF (AVD - 0.5000000000D-09) 10010, 10010, 20010              01160816
10010      IVPASS = IVPASS + 1                                          01170816
           WRITE (NUVI, 80002) IVTNUM                                   01180816
           GO TO 0011                                                   01190816
20010      IVFAIL = IVFAIL + 1                                          01200816
           DVCORR = 0.00000000000000000000D+00                          01210816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01220816
 0011      CONTINUE                                                     01230816
CT002*  TEST 2                                       VALUE CLOSE TO E   01240816
           IVTNUM = 2                                                   01250816
        AVD = DLOG(2.6875D0)                                            01260816
           IF (AVD - 0.9886113929D+00) 20020, 10020, 40020              01270816
40020      IF (AVD - 0.9886113940D+00) 10020, 10020, 20020              01280816
10020      IVPASS = IVPASS + 1                                          01290816
           WRITE (NUVI, 80002) IVTNUM                                   01300816
           GO TO 0021                                                   01310816
20020      IVFAIL = IVFAIL + 1                                          01320816
           DVCORR = 0.98861139345378118580D+00                          01330816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01340816
 0021      CONTINUE                                                     01350816
CT003*  TEST 3                                                          01360816
           IVTNUM = 3                                                   01370816
        AVD = DLOG(5.125D0)                                             01380816
           IF (AVD - 0.1634130524D+01) 20030, 10030, 40030              01390816
40030      IF (AVD - 0.1634130526D+01) 10030, 10030, 20030              01400816
10030      IVPASS = IVPASS + 1                                          01410816
           WRITE (NUVI, 80002) IVTNUM                                   01420816
           GO TO 0031                                                   01430816
20030      IVFAIL = IVFAIL + 1                                          01440816
           DVCORR = 1.6341305250244718756D+00                           01450816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01460816
 0031      CONTINUE                                                     01470816
CT004*  TEST 4                                                          01480816
           IVTNUM = 4                                                   01490816
        AVD = DLOG(10.0D0)                                              01500816
           IF (AVD - 0.2302585091D+01) 20040, 10040, 40040              01510816
40040      IF (AVD - 0.2302585095D+01) 10040, 10040, 20040              01520816
10040      IVPASS = IVPASS + 1                                          01530816
           WRITE (NUVI, 80002) IVTNUM                                   01540816
           GO TO 0041                                                   01550816
20040      IVFAIL = IVFAIL + 1                                          01560816
           DVCORR = 2.3025850929940456840D+00                           01570816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01580816
 0041      CONTINUE                                                     01590816
CT005*  TEST 5                                                          01600816
           IVTNUM = 5                                                   01610816
        AVD = DLOG(100.0D0)                                             01620816
           IF (AVD - 0.4605170183D+01) 20050, 10050, 40050              01630816
40050      IF (AVD - 0.4605170189D+01) 10050, 10050, 20050              01640816
10050      IVPASS = IVPASS + 1                                          01650816
           WRITE (NUVI, 80002) IVTNUM                                   01660816
           GO TO 0051                                                   01670816
20050      IVFAIL = IVFAIL + 1                                          01680816
           DVCORR = 4.6051701859880913680D+00                           01690816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01700816
 0051      CONTINUE                                                     01710816
CT006*  TEST 6                                                          01720816
           IVTNUM = 6                                                   01730816
        BVD = 1.0D0                                                     01740816
        AVD = DLOG(BVD / 4.D0)                                          01750816
           IF (AVD + 0.1386294362D+01) 20060, 10060, 40060              01760816
40060      IF (AVD + 0.1386294360D+01) 10060, 10060, 20060              01770816
10060      IVPASS = IVPASS + 1                                          01780816
           WRITE (NUVI, 80002) IVTNUM                                   01790816
           GO TO 0061                                                   01800816
20060      IVFAIL = IVFAIL + 1                                          01810816
           DVCORR = -1.3862943611198906188D+00                          01820816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01830816
 0061      CONTINUE                                                     01840816
CT007*  TEST 7                                                          01850816
           IVTNUM = 7                                                   01860816
        BVD = 1.0D0                                                     01870816
        CVD = 8.0D0                                                     01880816
        AVD = DLOG(3.0D0 * BVD / CVD)                                   01890816
           IF (AVD + 0.9808292535D+00) 20070, 10070, 40070              01900816
40070      IF (AVD + 0.9808292525D+00) 10070, 10070, 20070              01910816
10070      IVPASS = IVPASS + 1                                          01920816
           WRITE (NUVI, 80002) IVTNUM                                   01930816
           GO TO 0071                                                   01940816
20070      IVFAIL = IVFAIL + 1                                          01950816
           DVCORR = -0.98082925301172623686D+00                         01960816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01970816
 0071      CONTINUE                                                     01980816
CT008*  TEST 8                                                          01990816
           IVTNUM = 8                                                   02000816
        AVD = DLOG(50.0D0 / 100.0D0)                                    02010816
           IF (AVD + 0.6931471809D+00) 20080, 10080, 40080              02020816
40080      IF (AVD + 0.6931471802D+00) 10080, 10080, 20080              02030816
10080      IVPASS = IVPASS + 1                                          02040816
           WRITE (NUVI, 80002) IVTNUM                                   02050816
           GO TO 0081                                                   02060816
20080      IVFAIL = IVFAIL + 1                                          02070816
           DVCORR = -0.69314718055994530942D+00                         02080816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02090816
 0081      CONTINUE                                                     02100816
CT009*  TEST 9                                                          02110816
           IVTNUM = 9                                                   02120816
        BVD = 68.75D0                                                   02130816
        AVD = DLOG(BVD * 0.01D0)                                        02140816
           IF (AVD + 0.3746934497D+00) 20090, 10090, 40090              02150816
40090      IF (AVD + 0.3746934492D+00) 10090, 10090, 20090              02160816
10090      IVPASS = IVPASS + 1                                          02170816
           WRITE (NUVI, 80002) IVTNUM                                   02180816
           GO TO 0091                                                   02190816
20090      IVFAIL = IVFAIL + 1                                          02200816
           DVCORR = -0.37469344944141069361D+00                         02210816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02220816
 0091      CONTINUE                                                     02230816
CT010*  TEST 10                                   VALUES CLOSE TO ONE   02240816
           IVTNUM = 10                                                  02250816
        AVD = DLOG(0.96875D0)                                           02260816
           IF (AVD + 0.3174869833D-01) 20100, 10100, 40100              02270816
40100      IF (AVD + 0.3174869829D-01) 10100, 10100, 20100              02280816
10100      IVPASS = IVPASS + 1                                          02290816
           WRITE (NUVI, 80002) IVTNUM                                   02300816
           GO TO 0101                                                   02310816
20100      IVFAIL = IVFAIL + 1                                          02320816
           DVCORR = -0.031748698314580301157D+00                        02330816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02340816
 0101      CONTINUE                                                     02350816
CT011*  TEST 11                                                         02360816
           IVTNUM = 11                                                  02370816
        BVD = 1.015625D0                                                02380816
        AVD = DLOG(BVD)                                                 02390816
           IF (AVD - 0.1550418652D-01) 20110, 10110, 40110              02400816
40110      IF (AVD - 0.1550418655D-01) 10110, 10110, 20110              02410816
10110      IVPASS = IVPASS + 1                                          02420816
           WRITE (NUVI, 80002) IVTNUM                                   02430816
           GO TO 0111                                                   02440816
20110      IVFAIL = IVFAIL + 1                                          02450816
           DVCORR = 0.015504186535965254150D+00                         02460816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02470816
 0111      CONTINUE                                                     02480816
CT012*  TEST 12                                  VALUES CLOSE TO ZERO   02490816
           IVTNUM = 12                                                  02500816
        BVD = 128.0D0                                                   02510816
        AVD = DLOG(1.0D0 / BVD)                                         02520816
           IF (AVD + 0.4852030267D+01) 20120, 10120, 40120              02530816
40120      IF (AVD + 0.4852030261D+01) 10120, 10120, 20120              02540816
10120      IVPASS = IVPASS + 1                                          02550816
           WRITE (NUVI, 80002) IVTNUM                                   02560816
           GO TO 0121                                                   02570816
20120      IVFAIL = IVFAIL + 1                                          02580816
           DVCORR = -4.8520302639196171659D+00                          02590816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02600816
 0121      CONTINUE                                                     02610816
CT013*  TEST 13                                                         02620816
           IVTNUM = 13                                                  02630816
        BVD = 128.0D0                                                   02640816
        AVD = DLOG(1.0D0 / (BVD * 4.0D0))                               02650816
           IF (AVD + 0.6238324629D+01) 20130, 10130, 40130              02660816
40130      IF (AVD + 0.6238324622D+01) 10130, 10130, 20130              02670816
10130      IVPASS = IVPASS + 1                                          02680816
           WRITE (NUVI, 80002) IVTNUM                                   02690816
           GO TO 0131                                                   02700816
20130      IVFAIL = IVFAIL + 1                                          02710816
           DVCORR = -6.2383246250395077848D+00                          02720816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02730816
 0131      CONTINUE                                                     02740816
CT014*  TEST 14                         AN ARGUMENT OF HIGH MAGNITUDE   02750816
           IVTNUM = 14                                                  02760816
        BVD = 1.0D+37                                                   02770816
        AVD = DLOG(BVD)                                                 02780816
           IF (AVD - 0.8519564839D+02) 20140, 10140, 40140              02790816
40140      IF (AVD - 0.8519564849D+02) 10140, 10140, 20140              02800816
10140      IVPASS = IVPASS + 1                                          02810816
           WRITE (NUVI, 80002) IVTNUM                                   02820816
           GO TO 0141                                                   02830816
20140      IVFAIL = IVFAIL + 1                                          02840816
           DVCORR = 85.195648440779690309D+00                           02850816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02860816
 0141      CONTINUE                                                     02870816
CT015*  TEST 15                          AN ARGUMENT OF LOW MAGNITUDE   02880816
           IVTNUM = 15                                                  02890816
        BVD = 1.0D-37                                                   02900816
        AVD = DLOG(BVD)                                                 02910816
           IF (AVD + 0.8519564849D+02) 20150, 10150, 40150              02920816
40150      IF (AVD + 0.8519564840D+02) 10150, 10150, 20150              02930816
10150      IVPASS = IVPASS + 1                                          02940816
           WRITE (NUVI, 80002) IVTNUM                                   02950816
           GO TO 0151                                                   02960816
20150      IVFAIL = IVFAIL + 1                                          02970816
           DVCORR = -85.195648440779690309D+00                          02980816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02990816
 0151      CONTINUE                                                     03000816
CT016*  TEST 16                                                         03010816
           IVTNUM = 16                                                  03020816
        AVD = DLOG(8.0D0) + DLOG(0.125D0)                               03030816
           IF (AVD + 0.5000000000D-09) 20160, 10160, 40160              03040816
40160      IF (AVD - 0.5000000000D-09) 10160, 10160, 20160              03050816
10160      IVPASS = IVPASS + 1                                          03060816
           WRITE (NUVI, 80002) IVTNUM                                   03070816
           GO TO 0161                                                   03080816
20160      IVFAIL = IVFAIL + 1                                          03090816
           DVCORR = 0.00000000000000D+00                                03100816
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03110816
 0161      CONTINUE                                                     03120816
C*****                                                                  03130816
CBB** ********************** BBCSUM0  **********************************03140816
C**** WRITE OUT TEST SUMMARY                                            03150816
C****                                                                   03160816
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03170816
      WRITE (I02, 90004)                                                03180816
      WRITE (I02, 90014)                                                03190816
      WRITE (I02, 90004)                                                03200816
      WRITE (I02, 90020) IVPASS                                         03210816
      WRITE (I02, 90022) IVFAIL                                         03220816
      WRITE (I02, 90024) IVDELE                                         03230816
      WRITE (I02, 90026) IVINSP                                         03240816
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03250816
CBE** ********************** BBCSUM0  **********************************03260816
CBB** ********************** BBCFOOT0 **********************************03270816
C**** WRITE OUT REPORT FOOTINGS                                         03280816
C****                                                                   03290816
      WRITE (I02,90016) ZPROG, ZPROG                                    03300816
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03310816
      WRITE (I02,90019)                                                 03320816
CBE** ********************** BBCFOOT0 **********************************03330816
CBB** ********************** BBCFMT0A **********************************03340816
C**** FORMATS FOR TEST DETAIL LINES                                     03350816
C****                                                                   03360816
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03370816
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03380816
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03390816
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03400816
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03410816
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03420816
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03430816
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03440816
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03450816
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03460816
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03470816
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03480816
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03490816
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03500816
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03510816
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03520816
80050 FORMAT (1H ,48X,A31)                                              03530816
CBE** ********************** BBCFMT0A **********************************03540816
CBB** ********************** BBCFMAT1 **********************************03550816
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03560816
C****                                                                   03570816
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03580816
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03590816
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03600816
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03610816
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03620816
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03630816
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03640816
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03650816
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03660816
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03670816
     21H(,F12.5,2H, ,F12.5,1H))                                         03680816
CBE** ********************** BBCFMAT1 **********************************03690816
CBB** ********************** BBCFMT0B **********************************03700816
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03710816
C****                                                                   03720816
90002 FORMAT (1H1)                                                      03730816
90004 FORMAT (1H )                                                      03740816
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03750816
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03760816
90008 FORMAT (1H ,21X,A13,A17)                                          03770816
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03780816
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03790816
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03800816
     1       7X,7HREMARKS,24X)                                          03810816
90014 FORMAT (1H ,46H----------------------------------------------,    03820816
     1        33H---------------------------------)                     03830816
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03840816
C****                                                                   03850816
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03860816
C****                                                                   03870816
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03880816
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03890816
     1        A13)                                                      03900816
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03910816
C****                                                                   03920816
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03930816
C****                                                                   03940816
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03950816
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03960816
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03970816
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03980816
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03990816
CBE** ********************** BBCFMT0B **********************************04000816
C*****                                                                  04010816
C*****    END OF TEST SEGMENT 182                                       04020816
      STOP                                                              04030816
      END                                                               04040816
                                                                        04050816
