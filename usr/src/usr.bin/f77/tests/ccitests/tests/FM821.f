C***********************************************************************00010821
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020821
C*****   FM821                                                          00030821
C*****                       YDCOS - (190)                              00040821
C*****                                                                  00050821
C***********************************************************************00060821
C*****  GENERAL PURPOSE                                         ANS REF 00070821
C*****    TEST INTRINSIC FUNCTION DCOS                           15.3   00080821
C*****                                                          TABLE 5 00090821
C*****                                                                  00100821
CBB** ********************** BBCCOMNT **********************************00110821
C****                                                                   00120821
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130821
C****                          VERSION 2.0                              00140821
C****                                                                   00150821
C****                                                                   00160821
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170821
C****                   GENERAL SERVICES ADMINISTRATION                 00180821
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190821
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200821
C****                      FALLS CHURCH, VA. 22041                      00210821
C****                                                                   00220821
C****                          (703) 756-6153                           00230821
C****                                                                   00240821
CBE** ********************** BBCCOMNT **********************************00250821
C*****                                                                  00260821
C*****    S P E C I F I C A T I O N S SEGMENT 190                       00270821
        DOUBLE PRECISION AVD, BVD, PIVD, DVCORR                         00280821
C*****                                                                  00290821
CBB** ********************** BBCINITA **********************************00300821
C**** SPECIFICATION STATEMENTS                                          00310821
C****                                                                   00320821
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330821
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340821
CBE** ********************** BBCINITA **********************************00350821
CBB** ********************** BBCINITB **********************************00360821
C**** INITIALIZE SECTION                                                00370821
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380821
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390821
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400821
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410821
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420821
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430821
      DATA   REMRKS /'                               '/                 00440821
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450821
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460821
C****                                                                   00470821
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480821
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490821
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500821
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570821
      IVPASS = 0                                                        00580821
      IVFAIL = 0                                                        00590821
      IVDELE = 0                                                        00600821
      IVINSP = 0                                                        00610821
      IVTOTL = 0                                                        00620821
      IVTOTN = 0                                                        00630821
      ICZERO = 0                                                        00640821
C                                                                       00650821
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660821
      I01 = 05                                                          00670821
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680821
      I02 = 06                                                          00690821
C                                                                       00700821
      I01 = 5                                                           00710821
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720821
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730821
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740821
C                                                                       00750821
      I02 = 6                                                           00760821
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770821
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780821
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790821
C                                                                       00800821
CBE** ********************** BBCINITB **********************************00810821
      NUVI = I02                                                        00820821
      IVTOTL = 19                                                       00830821
      ZPROG = 'FM821'                                                   00840821
CBB** ********************** BBCHED0A **********************************00850821
C****                                                                   00860821
C**** WRITE REPORT TITLE                                                00870821
C****                                                                   00880821
      WRITE (I02, 90002)                                                00890821
      WRITE (I02, 90006)                                                00900821
      WRITE (I02, 90007)                                                00910821
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920821
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930821
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940821
CBE** ********************** BBCHED0A **********************************00950821
C*****                                                                  00960821
C*****    HEADER FOR SEGMENT 190                                        00970821
        WRITE(NUVI,19000)                                               00980821
19000   FORMAT(1H /34H YDCOS - (190) INTRINSIC FUNCTIONS//              00990821
     1         32H  DCOS (DOUBLE PRECISION COSINE)//                    01000821
     2         17H  ANS REF. - 15.3)                                    01010821
CBB** ********************** BBCHED0B **********************************01020821
C**** WRITE DETAIL REPORT HEADERS                                       01030821
C****                                                                   01040821
      WRITE (I02,90004)                                                 01050821
      WRITE (I02,90004)                                                 01060821
      WRITE (I02,90013)                                                 01070821
      WRITE (I02,90014)                                                 01080821
      WRITE (I02,90015) IVTOTL                                          01090821
CBE** ********************** BBCHED0B **********************************01100821
C*****                                                                  01110821
        PIVD = 3.1415926535897932384626434D0                            01120821
C*****                                                                  01130821
CT001*  TEST 1                               ZERO (0.0), SINCE COS(0)=1 01140821
           IVTNUM = 1                                                   01150821
        BVD = 0.0D0                                                     01160821
        AVD = DCOS(BVD)                                                 01170821
           IF (AVD - 0.9999999995D+00) 20010, 10010, 40010              01180821
40010      IF (AVD - 0.1000000001D+01) 10010, 10010, 20010              01190821
10010      IVPASS = IVPASS + 1                                          01200821
           WRITE (NUVI, 80002) IVTNUM                                   01210821
           GO TO 0011                                                   01220821
20010      IVFAIL = IVFAIL + 1                                          01230821
           DVCORR = 1.00000000000000000000D+00                          01240821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01250821
 0011      CONTINUE                                                     01260821
CT002*  TEST 2                                           VALUES NEAR PI 01270821
           IVTNUM = 2                                                   01280821
        AVD = DCOS(PIVD)                                                01290821
           IF (AVD + 0.1000000001D+01) 20020, 10020, 40020              01300821
40020      IF (AVD + 0.9999999995D+00) 10020, 10020, 20020              01310821
10020      IVPASS = IVPASS + 1                                          01320821
           WRITE (NUVI, 80002) IVTNUM                                   01330821
           GO TO 0021                                                   01340821
20020      IVFAIL = IVFAIL + 1                                          01350821
           DVCORR = -1.00000000000000000000D+00                         01360821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01370821
 0021      CONTINUE                                                     01380821
CT003*  TEST 3                                                PI - 1/16 01390821
           IVTNUM = 3                                                   01400821
        BVD = 3.07909265358979323846D0                                  01410821
        AVD = DCOS(BVD)                                                 01420821
           IF (AVD + 0.9980475112D+00) 20030, 10030, 40030              01430821
40030      IF (AVD + 0.9980475102D+00) 10030, 10030, 20030              01440821
10030      IVPASS = IVPASS + 1                                          01450821
           WRITE (NUVI, 80002) IVTNUM                                   01460821
           GO TO 0031                                                   01470821
20030      IVFAIL = IVFAIL + 1                                          01480821
           DVCORR = -0.99804751070009914963D+00                         01490821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01500821
 0031      CONTINUE                                                     01510821
CT004*  TEST 4                                                PI + 1/32 01520821
           IVTNUM = 4                                                   01530821
        AVD = DCOS(3.17284265358979323846D0)                            01540821
           IF (AVD + 0.9995117590D+00) 20040, 10040, 40040              01550821
40040      IF (AVD + 0.9995117580D+00) 10040, 10040, 20040              01560821
10040      IVPASS = IVPASS + 1                                          01570821
           WRITE (NUVI, 80002) IVTNUM                                   01580821
           GO TO 0041                                                   01590821
20040      IVFAIL = IVFAIL + 1                                          01600821
           DVCORR = -0.99951175848513636924D+00                         01610821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01620821
 0041      CONTINUE                                                     01630821
CT005*  TEST 5                                         VALUES NEAR 2*PI 01640821
           IVTNUM = 5                                                   01650821
        BVD = PIVD * 2.0D0                                              01660821
        AVD = DCOS(BVD)                                                 01670821
           IF (AVD - 0.9999999995D+00) 20050, 10050, 40050              01680821
40050      IF (AVD - 0.1000000001D+01) 10050, 10050, 20050              01690821
10050      IVPASS = IVPASS + 1                                          01700821
           WRITE (NUVI, 80002) IVTNUM                                   01710821
           GO TO 0051                                                   01720821
20050      IVFAIL = IVFAIL + 1                                          01730821
           DVCORR = 1.00000000000000000000D+00                          01740821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01750821
 0051      CONTINUE                                                     01760821
CT006*  TEST 6                                         VALUES NEAR 2*PI 01770821
           IVTNUM = 6                                                   01780821
        BVD = (2.0D0 * PIVD) - 1.0D0 / 64.0D0                           01790821
        AVD = DCOS(BVD)                                                 01800821
           IF (AVD - 0.9998779316D+00) 20060, 10060, 40060              01810821
40060      IF (AVD - 0.9998779327D+00) 10060, 10060, 20060              01820821
10060      IVPASS = IVPASS + 1                                          01830821
           WRITE (NUVI, 80002) IVTNUM                                   01840821
           GO TO 0061                                                   01850821
20060      IVFAIL = IVFAIL + 1                                          01860821
           DVCORR = 0.99987793217100665474D+00                          01870821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01880821
 0061      CONTINUE                                                     01890821
CT007*  TEST 7                                         VALUES NEAR 2*PI 01900821
           IVTNUM = 7                                                   01910821
        BVD = (2.0D0 * PIVD) + 1.0D0 / 128.0D0                          01920821
        AVD = DCOS(BVD)                                                 01930821
           IF (AVD - 0.9999694820D+00) 20070, 10070, 40070              01940821
40070      IF (AVD - 0.9999694831D+00) 10070, 10070, 20070              01950821
10070      IVPASS = IVPASS + 1                                          01960821
           WRITE (NUVI, 80002) IVTNUM                                   01970821
           GO TO 0071                                                   01980821
20070      IVFAIL = IVFAIL + 1                                          01990821
           DVCORR = 0.99996948257709511331D+00                          02000821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02010821
 0071      CONTINUE                                                     02020821
CT008*  TEST 8                          AN EXPRESSION PRESENTED TO DCOS 02030821
           IVTNUM = 8                                                   02040821
        BVD = 350.0D1                                                   02050821
        AVD = DCOS(BVD / 100.0D1)                                       02060821
           IF (AVD + 0.9364566878D+00) 20080, 10080, 40080              02070821
40080      IF (AVD + 0.9364566868D+00) 10080, 10080, 20080              02080821
10080      IVPASS = IVPASS + 1                                          02090821
           WRITE (NUVI, 80002) IVTNUM                                   02100821
           GO TO 0081                                                   02110821
20080      IVFAIL = IVFAIL + 1                                          02120821
           DVCORR = -0.93645668729079633770D+00                         02130821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02140821
 0081      CONTINUE                                                     02150821
CT009*  TEST 9                                      A NEGATIVE ARGUMENT 02160821
           IVTNUM = 9                                                   02170821
        BVD = -1.5D0                                                    02180821
        AVD = DCOS(BVD)                                                 02190821
           IF (AVD - 0.7073720163D-01) 20090, 10090, 40090              02200821
40090      IF (AVD - 0.7073720171D-01) 10090, 10090, 20090              02210821
10090      IVPASS = IVPASS + 1                                          02220821
           WRITE (NUVI, 80002) IVTNUM                                   02230821
           GO TO 0091                                                   02240821
20090      IVFAIL = IVFAIL + 1                                          02250821
           DVCORR = 0.070737201667702910088D+00                         02260821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02270821
 0091      CONTINUE                                                     02280821
CT010*  TEST 10                LARGE VALUES TO CHECK ARGUMENT REDUCTION 02290821
           IVTNUM = 10                                                  02300821
        AVD = DCOS(200.0D0)                                             02310821
           IF (AVD - 0.4871876747D+00) 20100, 10100, 40100              02320821
40100      IF (AVD - 0.4871876753D+00) 10100, 10100, 20100              02330821
10100      IVPASS = IVPASS + 1                                          02340821
           WRITE (NUVI, 80002) IVTNUM                                   02350821
           GO TO 0101                                                   02360821
20100      IVFAIL = IVFAIL + 1                                          02370821
           DVCORR = 0.48718767500700591035D+00                          02380821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02390821
 0101      CONTINUE                                                     02400821
CT011*  TEST 11                LARGE VALUES TO CHECK ARGUMENT REDUCTION 02410821
           IVTNUM = 11                                                  02420821
        AVD = DCOS(-31416.0D0)                                          02430821
           IF (AVD - 0.9973027257D+00) 20110, 10110, 40110              02440821
40110      IF (AVD - 0.9973027268D+00) 10110, 10110, 20110              02450821
10110      IVPASS = IVPASS + 1                                          02460821
           WRITE (NUVI, 80002) IVTNUM                                   02470821
           GO TO 0111                                                   02480821
20110      IVFAIL = IVFAIL + 1                                          02490821
           DVCORR = 0.99730272627420107808D+00                          02500821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02510821
 0111      CONTINUE                                                     02520821
CT012*  TEST 12                                        VALUES NEAR PI/2 02530821
           IVTNUM = 12                                                  02540821
        AVD = DCOS(1.57079632679489661923D0)                            02550821
           IF (AVD + 0.5000000000D-09) 20120, 10120, 40120              02560821
40120      IF (AVD - 0.5000000000D-09) 10120, 10120, 20120              02570821
10120      IVPASS = IVPASS + 1                                          02580821
           WRITE (NUVI, 80002) IVTNUM                                   02590821
           GO TO 0121                                                   02600821
20120      IVFAIL = IVFAIL + 1                                          02610821
           DVCORR = 0.00000000000000000000D+00                          02620821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02630821
 0121      CONTINUE                                                     02640821
CT013*  TEST 13                                         (PI / 2) - 1/32 02650821
           IVTNUM = 13                                                  02660821
        BVD = (1.53954632679489661923D0)                                02670821
        AVD = DCOS(BVD)                                                 02680821
           IF (AVD - 0.3124491397D-01) 20130, 10130, 40130              02690821
40130      IF (AVD - 0.3124491400D-01) 10130, 10130, 20130              02700821
10130      IVPASS = IVPASS + 1                                          02710821
           WRITE (NUVI, 80002) IVTNUM                                   02720821
           GO TO 0131                                                   02730821
20130      IVFAIL = IVFAIL + 1                                          02740821
           DVCORR = 0.031244913985326078739D+00                         02750821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02760821
 0131      CONTINUE                                                     02770821
CT014*  TEST 14                                         (PI / 2) + 1/16 02780821
           IVTNUM = 14                                                  02790821
        AVD = DCOS(1.63329632679489661923D0)                            02800821
           IF (AVD + 0.6245931788D-01) 20140, 10140, 40140              02810821
40140      IF (AVD + 0.6245931781D-01) 10140, 10140, 20140              02820821
10140      IVPASS = IVPASS + 1                                          02830821
           WRITE (NUVI, 80002) IVTNUM                                   02840821
           GO TO 0141                                                   02850821
20140      IVFAIL = IVFAIL + 1                                          02860821
           DVCORR = -0.062459317842380198585D+00                        02870821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02880821
 0141      CONTINUE                                                     02890821
CT015*  TEST 15                                      VALUES NEAR 3*PI/2 02900821
           IVTNUM = 15                                                  02910821
        BVD = 3.0D0 * PIVD / 2.0D0                                      02920821
        AVD = DCOS(BVD)                                                 02930821
           IF (AVD + 0.5000000000D-09) 20150, 10150, 40150              02940821
40150      IF (AVD - 0.5000000000D-09) 10150, 10150, 20150              02950821
10150      IVPASS = IVPASS + 1                                          02960821
           WRITE (NUVI, 80002) IVTNUM                                   02970821
           GO TO 0151                                                   02980821
20150      IVFAIL = IVFAIL + 1                                          02990821
           DVCORR = 0.00000000000000000000D+00                          03000821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03010821
 0151      CONTINUE                                                     03020821
CT016*  TEST 16                                      VALUES NEAR 3*PI/2 03030821
           IVTNUM = 16                                                  03040821
        BVD = (3.0D0 * PIVD / 2.0D0) + 1.0D0 / 16.0D0                   03050821
        AVD = DCOS(BVD)                                                 03060821
           IF (AVD - 0.6245931781D-01) 20160, 10160, 40160              03070821
40160      IF (AVD - 0.6245931788D-01) 10160, 10160, 20160              03080821
10160      IVPASS = IVPASS + 1                                          03090821
           WRITE (NUVI, 80002) IVTNUM                                   03100821
           GO TO 0161                                                   03110821
20160      IVFAIL = IVFAIL + 1                                          03120821
           DVCORR = 0.062459317842380198585D+00                         03130821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03140821
 0161      CONTINUE                                                     03150821
CT017*  TEST 17                                      VALUES NEAR 3*PI/2 03160821
           IVTNUM = 17                                                  03170821
        BVD = (3.0D0 * PIVD / 2.0D0) - 1.0D0 / 512.0D0                  03180821
        AVD = DCOS(BVD)                                                 03190821
           IF (AVD + 0.1953123760D-02) 20170, 10170, 40170              03200821
40170      IF (AVD + 0.1953123757D-02) 10170, 10170, 20170              03210821
10170      IVPASS = IVPASS + 1                                          03220821
           WRITE (NUVI, 80002) IVTNUM                                   03230821
           GO TO 0171                                                   03240821
20170      IVFAIL = IVFAIL + 1                                          03250821
           DVCORR = -0.0019531237582368040269D+00                       03260821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03270821
 0171      CONTINUE                                                     03280821
CT018*  TEST 18                               ARGUMENT OF LOW MAGNITUDE 03290821
           IVTNUM = 18                                                  03300821
        BVD = -3.1415926535898D-35                                      03310821
        AVD = DCOS(BVD)                                                 03320821
           IF (AVD - 0.9999999995D+00) 20180, 10180, 40180              03330821
40180      IF (AVD - 0.1000000001D+01) 10180, 10180, 20180              03340821
10180      IVPASS = IVPASS + 1                                          03350821
           WRITE (NUVI, 80002) IVTNUM                                   03360821
           GO TO 0181                                                   03370821
20180      IVFAIL = IVFAIL + 1                                          03380821
           DVCORR = 1.00000000000000000000D+00                          03390821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03400821
 0181      CONTINUE                                                     03410821
CT019*  TEST 19                              THE FUNCTION APPLIED TWICE 03420821
           IVTNUM = 19                                                  03430821
        AVD = DCOS(PIVD / 4.0D0) * DCOS(3.0D0 * PIVD / 4.0D0)           03440821
           IF (AVD + 0.5000000003D+00) 20190, 10190, 40190              03450821
40190      IF (AVD + 0.4999999997D+00) 10190, 10190, 20190              03460821
10190      IVPASS = IVPASS + 1                                          03470821
           WRITE (NUVI, 80002) IVTNUM                                   03480821
           GO TO 0191                                                   03490821
20190      IVFAIL = IVFAIL + 1                                          03500821
           DVCORR = -0.5000000000000000000000D+00                       03510821
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03520821
 0191      CONTINUE                                                     03530821
C*****                                                                  03540821
CBB** ********************** BBCSUM0  **********************************03550821
C**** WRITE OUT TEST SUMMARY                                            03560821
C****                                                                   03570821
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03580821
      WRITE (I02, 90004)                                                03590821
      WRITE (I02, 90014)                                                03600821
      WRITE (I02, 90004)                                                03610821
      WRITE (I02, 90020) IVPASS                                         03620821
      WRITE (I02, 90022) IVFAIL                                         03630821
      WRITE (I02, 90024) IVDELE                                         03640821
      WRITE (I02, 90026) IVINSP                                         03650821
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03660821
CBE** ********************** BBCSUM0  **********************************03670821
CBB** ********************** BBCFOOT0 **********************************03680821
C**** WRITE OUT REPORT FOOTINGS                                         03690821
C****                                                                   03700821
      WRITE (I02,90016) ZPROG, ZPROG                                    03710821
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03720821
      WRITE (I02,90019)                                                 03730821
CBE** ********************** BBCFOOT0 **********************************03740821
CBB** ********************** BBCFMT0A **********************************03750821
C**** FORMATS FOR TEST DETAIL LINES                                     03760821
C****                                                                   03770821
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03780821
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03790821
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03800821
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03810821
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03820821
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03830821
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03840821
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03850821
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03860821
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03870821
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03880821
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03890821
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03900821
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03910821
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03920821
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03930821
80050 FORMAT (1H ,48X,A31)                                              03940821
CBE** ********************** BBCFMT0A **********************************03950821
CBB** ********************** BBCFMAT1 **********************************03960821
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03970821
C****                                                                   03980821
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03990821
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04000821
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04010821
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04020821
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04030821
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04040821
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04050821
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04060821
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04070821
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04080821
     21H(,F12.5,2H, ,F12.5,1H))                                         04090821
CBE** ********************** BBCFMAT1 **********************************04100821
CBB** ********************** BBCFMT0B **********************************04110821
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04120821
C****                                                                   04130821
90002 FORMAT (1H1)                                                      04140821
90004 FORMAT (1H )                                                      04150821
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04160821
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04170821
90008 FORMAT (1H ,21X,A13,A17)                                          04180821
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04190821
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04200821
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04210821
     1       7X,7HREMARKS,24X)                                          04220821
90014 FORMAT (1H ,46H----------------------------------------------,    04230821
     1        33H---------------------------------)                     04240821
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04250821
C****                                                                   04260821
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04270821
C****                                                                   04280821
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04290821
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04300821
     1        A13)                                                      04310821
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04320821
C****                                                                   04330821
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04340821
C****                                                                   04350821
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04360821
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04370821
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04380821
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04390821
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04400821
CBE** ********************** BBCFMT0B **********************************04410821
C*****                                                                  04420821
C*****  END OF TEST SEGMENT 190                                         04430821
      STOP                                                              04440821
      END                                                               04450821
                                                                        04460821
