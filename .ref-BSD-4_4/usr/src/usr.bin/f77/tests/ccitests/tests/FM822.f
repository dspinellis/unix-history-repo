C***********************************************************************00010822
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020822
C*****   FM822                                                          00030822
C*****                       YDTAN - (192)                              00040822
C*****                                                                  00050822
C***********************************************************************00060822
C*****  GENERAL PURPOSE                                         ANS REF 00070822
C*****    TEST INTRINSIC FUNCTION DTAN                           15.3   00080822
C*****                                                          TABLE 5 00090822
C*****                                                                  00100822
CBB** ********************** BBCCOMNT **********************************00110822
C****                                                                   00120822
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130822
C****                          VERSION 2.0                              00140822
C****                                                                   00150822
C****                                                                   00160822
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170822
C****                   GENERAL SERVICES ADMINISTRATION                 00180822
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190822
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200822
C****                      FALLS CHURCH, VA. 22041                      00210822
C****                                                                   00220822
C****                          (703) 756-6153                           00230822
C****                                                                   00240822
CBE** ********************** BBCCOMNT **********************************00250822
C*****                                                                  00260822
C*****    S P E C I F I C A T I O N S SEGMENT 192                       00270822
        DOUBLE PRECISION AVD, BVD, PIVD, DVCORR                         00280822
C*****                                                                  00290822
CBB** ********************** BBCINITA **********************************00300822
C**** SPECIFICATION STATEMENTS                                          00310822
C****                                                                   00320822
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330822
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340822
CBE** ********************** BBCINITA **********************************00350822
CBB** ********************** BBCINITB **********************************00360822
C**** INITIALIZE SECTION                                                00370822
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380822
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390822
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400822
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410822
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420822
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430822
      DATA   REMRKS /'                               '/                 00440822
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450822
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460822
C****                                                                   00470822
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480822
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490822
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500822
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570822
      IVPASS = 0                                                        00580822
      IVFAIL = 0                                                        00590822
      IVDELE = 0                                                        00600822
      IVINSP = 0                                                        00610822
      IVTOTL = 0                                                        00620822
      IVTOTN = 0                                                        00630822
      ICZERO = 0                                                        00640822
C                                                                       00650822
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660822
      I01 = 05                                                          00670822
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680822
      I02 = 06                                                          00690822
C                                                                       00700822
      I01 = 5                                                           00710822
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720822
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730822
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740822
C                                                                       00750822
      I02 = 6                                                           00760822
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770822
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780822
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790822
C                                                                       00800822
CBE** ********************** BBCINITB **********************************00810822
      NUVI = I02                                                        00820822
      IVTOTL = 14                                                       00830822
      ZPROG = 'FM822'                                                   00840822
CBB** ********************** BBCHED0A **********************************00850822
C****                                                                   00860822
C**** WRITE REPORT TITLE                                                00870822
C****                                                                   00880822
      WRITE (I02, 90002)                                                00890822
      WRITE (I02, 90006)                                                00900822
      WRITE (I02, 90007)                                                00910822
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920822
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930822
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940822
CBE** ********************** BBCHED0A **********************************00950822
C*****                                                                  00960822
C*****    HEADER FOR SEGMENT 192                                        00970822
        WRITE(NUVI,19200)                                               00980822
19200   FORMAT(1H , / 35H  YDTAN - (192) INTRINSIC FUNCTIONS//          00990822
     1         34H  DTAN  (DOUBLE PRECISION TANGENT)//                  01000822
     2         17H  ANS REF. - 15.3)                                    01010822
CBB** ********************** BBCHED0B **********************************01020822
C**** WRITE DETAIL REPORT HEADERS                                       01030822
C****                                                                   01040822
      WRITE (I02,90004)                                                 01050822
      WRITE (I02,90004)                                                 01060822
      WRITE (I02,90013)                                                 01070822
      WRITE (I02,90014)                                                 01080822
      WRITE (I02,90015) IVTOTL                                          01090822
CBE** ********************** BBCHED0B **********************************01100822
C*****                                                                  01110822
        PIVD = 3.1415926535897932384626434D0                            01120822
C*****                                                                  01130822
CT001*  TEST 1                            ZERO (0.0), SINCE TAN(0) = 0. 01140822
           IVTNUM = 1                                                   01150822
        BVD = 0.0D0                                                     01160822
        AVD = DTAN(BVD)                                                 01170822
           IF (AVD +  0.5000000000D-09) 20010, 10010, 40010             01180822
40010      IF (AVD -  0.5000000000D-09) 10010, 10010, 20010             01190822
10010      IVPASS = IVPASS + 1                                          01200822
           WRITE (NUVI, 80002) IVTNUM                                   01210822
           GO TO 0011                                                   01220822
20010      IVFAIL = IVFAIL + 1                                          01230822
           DVCORR =    0.00000000000000000000D+00                       01240822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01250822
 0011      CONTINUE                                                     01260822
CT002*  TEST 2                                                     2*PI 01270822
           IVTNUM = 2                                                   01280822
        BVD = 6.28318530717958647692D0                                  01290822
        AVD = DTAN(BVD)                                                 01300822
           IF (AVD +  0.5000000000D-09) 20020, 10020, 40020             01310822
40020      IF (AVD -  0.5000000000D-09) 10020, 10020, 20020             01320822
10020      IVPASS = IVPASS + 1                                          01330822
           WRITE (NUVI, 80002) IVTNUM                                   01340822
           GO TO 0021                                                   01350822
20020      IVFAIL = IVFAIL + 1                                          01360822
           DVCORR =    0.00000000000000000000D+00                       01370822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01380822
 0021      CONTINUE                                                     01390822
CT003*  TEST 3                                                     3*PI 01400822
           IVTNUM = 3                                                   01410822
        BVD = 9.42477796076937971538D0                                  01420822
        AVD = DTAN(BVD)                                                 01430822
           IF (AVD +  0.5000000000D-09) 20030, 10030, 40030             01440822
40030      IF (AVD -  0.5000000000D-09) 10030, 10030, 20030             01450822
10030      IVPASS = IVPASS + 1                                          01460822
           WRITE (NUVI, 80002) IVTNUM                                   01470822
           GO TO 0031                                                   01480822
20030      IVFAIL = IVFAIL + 1                                          01490822
           DVCORR =    0.00000000000000000000D+00                       01500822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01510822
 0031      CONTINUE                                                     01520822
CT004*  TEST 4                                                     PI/4 01530822
           IVTNUM = 4                                                   01540822
        AVD = DTAN(PIVD / 4.0D0)                                        01550822
           IF (AVD -  0.9999999995D+00) 20040, 10040, 40040             01560822
40040      IF (AVD -  0.1000000001D+01) 10040, 10040, 20040             01570822
10040      IVPASS = IVPASS + 1                                          01580822
           WRITE (NUVI, 80002) IVTNUM                                   01590822
           GO TO 0041                                                   01600822
20040      IVFAIL = IVFAIL + 1                                          01610822
           DVCORR =    1.00000000000000000000D+00                       01620822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01630822
 0041      CONTINUE                                                     01640822
CT005*  TEST 5                                                   5*PI/4 01650822
           IVTNUM = 5                                                   01660822
        BVD = 5.0D0 * PIVD / 4.0D0                                      01670822
        AVD = DTAN(BVD)                                                 01680822
           IF (AVD -  0.9999999995D+00) 20050, 10050, 40050             01690822
40050      IF (AVD -  0.1000000001D+01) 10050, 10050, 20050             01700822
10050      IVPASS = IVPASS + 1                                          01710822
           WRITE (NUVI, 80002) IVTNUM                                   01720822
           GO TO 0051                                                   01730822
20050      IVFAIL = IVFAIL + 1                                          01740822
           DVCORR =    1.00000000000000000000D+00                       01750822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01760822
 0051      CONTINUE                                                     01770822
CT006*  TEST 6                                         A NEGATIVE VALUE 01780822
           IVTNUM = 6                                                   01790822
        BVD = -2.0D0 / 1.0D0                                            01800822
        AVD = DTAN(BVD)                                                 01810822
           IF (AVD -  0.2185039862D+01) 20060, 10060, 40060             01820822
40060      IF (AVD -  0.2185039865D+01) 10060, 10060, 20060             01830822
10060      IVPASS = IVPASS + 1                                          01840822
           WRITE (NUVI, 80002) IVTNUM                                   01850822
           GO TO 0061                                                   01860822
20060      IVFAIL = IVFAIL + 1                                          01870822
           DVCORR =    2.1850398632615189916D+00                        01880822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01890822
 0061      CONTINUE                                                     01900822
CT007*  TEST 7                                         A POSITIVE VALUE 01910822
           IVTNUM = 7                                                   01920822
        BVD = 350.0D0 / 100.0D0                                         01930822
        AVD = DTAN(BVD)                                                 01940822
           IF (AVD -  0.3745856399D+00) 20070, 10070, 40070             01950822
40070      IF (AVD -  0.3745856404D+00) 10070, 10070, 20070             01960822
10070      IVPASS = IVPASS + 1                                          01970822
           WRITE (NUVI, 80002) IVTNUM                                   01980822
           GO TO 0071                                                   01990822
20070      IVFAIL = IVFAIL + 1                                          02000822
           DVCORR =    0.37458564015859466633D+00                       02010822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02020822
 0071      CONTINUE                                                     02030822
CT008*  TEST 8                                           (PI / 2) - 1/8 02040822
           IVTNUM = 8                                                   02050822
        BVD = 1.44579632679489661923D0                                  02060822
        AVD = DTAN(BVD)                                                 02070822
           IF (AVD -  0.7958289861D+01) 20080, 10080, 40080             02080822
40080      IF (AVD -  0.7958289870D+01) 10080, 10080, 20080             02090822
10080      IVPASS = IVPASS + 1                                          02100822
           WRITE (NUVI, 80002) IVTNUM                                   02110822
           GO TO 0081                                                   02120822
20080      IVFAIL = IVFAIL + 1                                          02130822
           DVCORR =    7.9582898658670111779D+00                        02140822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02150822
 0081      CONTINUE                                                     02160822
CT009*  TEST 9                                        (PI / 2) + 1/256  02170822
           IVTNUM = 9                                                   02180822
        BVD = 1.57470257679489661923D0                                  02190822
        AVD = DTAN(BVD)                                                 02200822
           IF (AVD +  0.2559986981D+03) 20090, 10090, 40090             02210822
40090      IF (AVD +  0.2559986977D+03) 10090, 10090, 20090             02220822
10090      IVPASS = IVPASS + 1                                          02230822
           WRITE (NUVI, 80002) IVTNUM                                   02240822
           GO TO 0091                                                   02250822
20090      IVFAIL = IVFAIL + 1                                          02260822
           DVCORR = -255.99869791534211708D+00                          02270822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02280822
 0091      CONTINUE                                                     02290822
CT010*  TEST 10                                         3*PI/2 - 1/1024 02300822
           IVTNUM = 10                                                  02310822
        AVD = DTAN((3.0D0 * PIVD / 2.0D0) - 1.0D0 / 1024.0D0)           02320822
           IF (AVD -  0.1023999674D+04) 20100, 10100, 40100             02330822
40100      IF (AVD -  0.1023999675D+04) 10100, 10100, 20100             02340822
10100      IVPASS = IVPASS + 1                                          02350822
           WRITE (NUVI, 80002) IVTNUM                                   02360822
           GO TO 0101                                                   02370822
20100      IVFAIL = IVFAIL + 1                                          02380822
           DVCORR = 1023.9996744791459706D+00                           02390822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02400822
 0101      CONTINUE                                                     02410822
CT011*  TEST 11                                             3*PI + 1/64 02420822
           IVTNUM = 11                                                  02430822
        BVD = (3.0D0 * PIVD / 2.0D0) + 1.0D0 / 64.0D0                   02440822
        AVD = DTAN(BVD)                                                 02450822
           IF (AVD +  0.6399479162D+02) 20110, 10110, 40110             02460822
40110      IF (AVD +  0.6399479155D+02) 10110, 10110, 20110             02470822
10110      IVPASS = IVPASS + 1                                          02480822
           WRITE (NUVI, 80002) IVTNUM                                   02490822
           GO TO 0111                                                   02500822
20110      IVFAIL = IVFAIL + 1                                          02510822
           DVCORR =  -63.994791581893645218D+00                         02520822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02530822
 0111      CONTINUE                                                     02540822
CT012*  TEST 12                  LARGE VALUE TO TEST ARGUMENT REDUCTION 02550822
           IVTNUM = 12                                                  02560822
        AVD = DTAN(2000.0D0)                                            02570822
           IF (AVD +  0.2530998330D+01) 20120, 10120, 40120             02580822
40120      IF (AVD +  0.2530998326D+01) 10120, 10120, 20120             02590822
10120      IVPASS = IVPASS + 1                                          02600822
           WRITE (NUVI, 80002) IVTNUM                                   02610822
           GO TO 0121                                                   02620822
20120      IVFAIL = IVFAIL + 1                                          02630822
           DVCORR =   -2.5309983280933409104D+00                        02640822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02650822
 0121      CONTINUE                                                     02660822
CT013*  TEST 13                               ARGUMENT OF LOW MAGNITUDE 02670822
           IVTNUM = 13                                                  02680822
        BVD = PIVD * 1.0D-15                                            02690822
        AVD = DTAN(BVD)                                                 02700822
           IF (AVD -  0.3141592652D-14) 20130, 10130, 40130             02710822
40130      IF (AVD -  0.3141592655D-14) 10130, 10130, 20130             02720822
10130      IVPASS = IVPASS + 1                                          02730822
           WRITE (NUVI, 80002) IVTNUM                                   02740822
           GO TO 0131                                                   02750822
20130      IVFAIL = IVFAIL + 1                                          02760822
           DVCORR =    3.1415926535897932385D-15                        02770822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02780822
 0131      CONTINUE                                                     02790822
CT014*  TEST 14                              THE FUNCTION APPLIED TWICE 02800822
           IVTNUM = 14                                                  02810822
        AVD = DTAN(PIVD / 6.0D0) * DTAN(PIVD / 6.0D0)                   02820822
           IF (AVD -  0.3333333331D+00) 20140, 10140, 40140             02830822
40140      IF (AVD -  0.3333333335D+00) 10140, 10140, 20140             02840822
10140      IVPASS = IVPASS + 1                                          02850822
           WRITE (NUVI, 80002) IVTNUM                                   02860822
           GO TO 0141                                                   02870822
20140      IVFAIL = IVFAIL + 1                                          02880822
           DVCORR =    0.33333333333333333333D+00                       02890822
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02900822
 0141      CONTINUE                                                     02910822
C*****                                                                  02920822
CBB** ********************** BBCSUM0  **********************************02930822
C**** WRITE OUT TEST SUMMARY                                            02940822
C****                                                                   02950822
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02960822
      WRITE (I02, 90004)                                                02970822
      WRITE (I02, 90014)                                                02980822
      WRITE (I02, 90004)                                                02990822
      WRITE (I02, 90020) IVPASS                                         03000822
      WRITE (I02, 90022) IVFAIL                                         03010822
      WRITE (I02, 90024) IVDELE                                         03020822
      WRITE (I02, 90026) IVINSP                                         03030822
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03040822
CBE** ********************** BBCSUM0  **********************************03050822
CBB** ********************** BBCFOOT0 **********************************03060822
C**** WRITE OUT REPORT FOOTINGS                                         03070822
C****                                                                   03080822
      WRITE (I02,90016) ZPROG, ZPROG                                    03090822
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03100822
      WRITE (I02,90019)                                                 03110822
CBE** ********************** BBCFOOT0 **********************************03120822
CBB** ********************** BBCFMT0A **********************************03130822
C**** FORMATS FOR TEST DETAIL LINES                                     03140822
C****                                                                   03150822
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03160822
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03170822
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03180822
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03190822
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03200822
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03210822
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03220822
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03230822
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03240822
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03250822
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03260822
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03270822
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03280822
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03290822
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03300822
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03310822
80050 FORMAT (1H ,48X,A31)                                              03320822
CBE** ********************** BBCFMT0A **********************************03330822
CBB** ********************** BBCFMAT1 **********************************03340822
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03350822
C****                                                                   03360822
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03370822
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03380822
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03390822
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03400822
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03410822
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03420822
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03430822
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03440822
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03450822
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03460822
     21H(,F12.5,2H, ,F12.5,1H))                                         03470822
CBE** ********************** BBCFMAT1 **********************************03480822
CBB** ********************** BBCFMT0B **********************************03490822
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03500822
C****                                                                   03510822
90002 FORMAT (1H1)                                                      03520822
90004 FORMAT (1H )                                                      03530822
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03540822
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03550822
90008 FORMAT (1H ,21X,A13,A17)                                          03560822
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03570822
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03580822
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03590822
     1       7X,7HREMARKS,24X)                                          03600822
90014 FORMAT (1H ,46H----------------------------------------------,    03610822
     1        33H---------------------------------)                     03620822
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03630822
C****                                                                   03640822
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03650822
C****                                                                   03660822
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03670822
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03680822
     1        A13)                                                      03690822
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03700822
C****                                                                   03710822
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03720822
C****                                                                   03730822
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03740822
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03750822
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03760822
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03770822
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03780822
CBE** ********************** BBCFMT0B **********************************03790822
C*****                                                                  03800822
C*****    END OF TEST SEGMENT 192                                       03810822
      STOP                                                              03820822
      END                                                               03830822
