C***********************************************************************00010908
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020908
C*****   FM908                                                          00030908
C*****                       INTER3 - (392)                             00040908
C*****                                                                  00050908
C***********************************************************************00060908
C*****  TESTING OF INTERNAL FILES -                           ANS. REF  00070908
C*****          USING READ                                      12.2.5  00080908
C*****                                                                  00090908
CBB** ********************** BBCCOMNT **********************************00100908
C****                                                                   00110908
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120908
C****                          VERSION 2.0                              00130908
C****                                                                   00140908
C****                                                                   00150908
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160908
C****                   GENERAL SERVICES ADMINISTRATION                 00170908
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180908
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190908
C****                      FALLS CHURCH, VA. 22041                      00200908
C****                                                                   00210908
C****                          (703) 756-6153                           00220908
C****                                                                   00230908
CBE** ********************** BBCCOMNT **********************************00240908
C*****                                                                  00250908
C*****  S P E C I F I C A T I O N S  SEGMENT 392                        00260908
C*****                                                                  00270908
        DOUBLE PRECISION AVD, BVD, CVD, DVD, EVD, DVCORR                00280908
        LOGICAL AVB                                                     00290908
        CHARACTER*43 A43VK, D43VK, F43VK, G43VK, K43VK, N43VK           00300908
        CHARACTER A8VK*8, E51VK*51, L53VK*53, I82VK*82                  00310908
        CHARACTER J97VK*97, C431K(2)*43, CVCORR*30                      00320908
        CHARACTER*29 B291K(5), M291K(5), H131K(2)*13                    00330908
        COMPLEX AVC, BVC, CVC, DVC, ZVCORR                              00340908
        REAL R2E(8)                                                     00350908
        EQUIVALENCE (R2E(1),AVC),(R2E(3),BVC),(R2E(5),CVC),(R2E(7),DVC) 00360908
C*****                                                                  00370908
CBB** ********************** BBCINITA **********************************00380908
C**** SPECIFICATION STATEMENTS                                          00390908
C****                                                                   00400908
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00410908
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00420908
CBE** ********************** BBCINITA **********************************00430908
CBB** ********************** BBCINITB **********************************00440908
C**** INITIALIZE SECTION                                                00450908
      DATA  ZVERS,                  ZVERSD,             ZDATE           00460908
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00470908
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00480908
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00490908
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00500908
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00510908
      DATA   REMRKS /'                               '/                 00520908
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00530908
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00540908
C****                                                                   00550908
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00560908
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00570908
CZ03  ZPROG  = 'PROGRAM NAME'                                           00580908
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00650908
      IVPASS = 0                                                        00660908
      IVFAIL = 0                                                        00670908
      IVDELE = 0                                                        00680908
      IVINSP = 0                                                        00690908
      IVTOTL = 0                                                        00700908
      IVTOTN = 0                                                        00710908
      ICZERO = 0                                                        00720908
C                                                                       00730908
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00740908
      I01 = 05                                                          00750908
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00760908
      I02 = 06                                                          00770908
C                                                                       00780908
      I01 = 5                                                           00790908
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00800908
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00810908
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00820908
C                                                                       00830908
      I02 = 6                                                           00840908
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00850908
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00860908
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00870908
C                                                                       00880908
CBE** ********************** BBCINITB **********************************00890908
      NUVI = I02                                                        00900908
      IVTOTL = 54                                                       00910908
      ZPROG = 'FM908'                                                   00920908
CBB** ********************** BBCHED0A **********************************00930908
C****                                                                   00940908
C**** WRITE REPORT TITLE                                                00950908
C****                                                                   00960908
      WRITE (I02, 90002)                                                00970908
      WRITE (I02, 90006)                                                00980908
      WRITE (I02, 90007)                                                00990908
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01000908
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01010908
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01020908
CBE** ********************** BBCHED0A **********************************01030908
C*****                                                                  01040908
C*****    HEADER FOR SEGMENT 392                                        01050908
C*****                                                                  01060908
        WRITE(NUVI,39200)                                               01070908
39200   FORMAT(1H ,/ 44H INTER3 - (392) INTERNAL FILES -- USING READ    01080908
     1             //19H ANS. REF. - 12.2.5)                            01090908
CBB** ********************** BBCHED0B **********************************01100908
C**** WRITE DETAIL REPORT HEADERS                                       01110908
C****                                                                   01120908
      WRITE (I02,90004)                                                 01130908
      WRITE (I02,90004)                                                 01140908
      WRITE (I02,90013)                                                 01150908
      WRITE (I02,90014)                                                 01160908
      WRITE (I02,90015) IVTOTL                                          01170908
CBE** ********************** BBCHED0B **********************************01180908
      A43VK = ' 2.1000000D1 23.45600D3      23.450000000D2'             01190908
      D43VK = '34.58673D2 3458.67300 34.58673D2 3458.673  '             01200908
      F43VK = 'T   10.98THISISIT  3.4945D2  3             '             01210908
      G43VK = '   2.343   34.394                      '                 01220908
      K43VK = '  0.934, 34.567   34.65        0.63540D1   '             01230908
      N43VK = '34 34.98395.83000D2 F.FALSE.13.45300E+2    '             01240908
      E51VK = ' 348  3.4783E1384.3847D1    T      3.48570 KDFJ D/.'     01250908
      L53VK = '   0.345 ,3.4345E01,F, 34.85900D-1,  10.000012345678'    01260908
      I82VK = '  2.34 ,  2.456     2.34 ,  2.456     0.234E01,  2.456E0001270908
     1   0.234E+001, 2.456E-000'                                        01280908
      J97VK = '   5.67980,   0.9876       5.67980,    0.9876   05.6798E001290908
     10, 9.8760E-1  5.67980E0000,0.09876E+001'                          01300908
      B291K(1) =  '34.38457D1 34.38457D1       '                        01310908
      B291K(2) = '34.38457D1                   '                        01320908
      B291K(3) = '34.38457D1 34.38457D1        '                        01330908
      B291K(4) = '                             '                        01340908
      B291K(5) = '34.38457D1                   '                        01350908
      M291K(1) = '   98                        '                        01360908
      M291K(2) = '8.40485D02                   '                        01370908
      M291K(3) = '                             '                        01380908
      M291K(4) = ' .TRUE. 340.435E-1,  3.494E+1'                        01390908
      M291K(5) = '87654321                     '                        01400908
      C431K(1) = ' 2.1000000D1 23.45600D3      23.450000000D2'          01410908
      C431K(2) = '                                           '          01420908
      H131K(1) = '34.84'                                                01430908
      H131K(2) = '349.887'                                              01440908
CT001*  TEST 1                          DOUBLE PRECISION FROM VARIABLE  01450908
           IVTNUM = 1                                                   01460908
        READ(UNIT=A43VK,FMT=39201) AVD                                  01470908
39201   FORMAT(13X,D10.5)                                               01480908
           IF (AVD - 0.2345599998D+05) 20010, 10010, 40010              01490908
40010      IF (AVD - 0.2345600002D+05) 10010, 10010, 20010              01500908
10010      IVPASS = IVPASS + 1                                          01510908
           WRITE (NUVI, 80002) IVTNUM                                   01520908
           GO TO 0011                                                   01530908
20010      IVFAIL = IVFAIL + 1                                          01540908
           DVCORR = 23.456D3                                            01550908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01560908
 0011      CONTINUE                                                     01570908
CT002*  TEST 2                                          FROM ELEMENT    01580908
           IVTNUM = 2                                                   01590908
        READ(UNIT=C431K(1),FMT=39204) AVD                               01600908
39204   FORMAT(D12.7)                                                   01610908
           IF (AVD - 0.2099999999D+02) 20020, 10020, 40020              01620908
40020      IF (AVD - 0.2100000001D+02) 10020, 10020, 20020              01630908
10020      IVPASS = IVPASS + 1                                          01640908
           WRITE (NUVI, 80002) IVTNUM                                   01650908
           GO TO 0021                                                   01660908
20020      IVFAIL = IVFAIL + 1                                          01670908
           DVCORR = 2.1D1                                               01680908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01690908
 0021      CONTINUE                                                     01700908
CT003*  TEST 3                                          FROM SUBSTRING  01710908
           IVTNUM = 3                                                   01720908
        READ(UNIT=A43VK(19:),FMT=39206) AVD                             01730908
39206   FORMAT(11X,D14.9)                                               01740908
           IF (AVD - 0.2344999998D+04) 20030, 10030, 40030              01750908
40030      IF (AVD - 0.2345000002D+04) 10030, 10030, 20030              01760908
10030      IVPASS = IVPASS + 1                                          01770908
           WRITE (NUVI, 80002) IVTNUM                                   01780908
           GO TO 0031                                                   01790908
20030      IVFAIL = IVFAIL + 1                                          01800908
           DVCORR = 23.45D2                                             01810908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      01820908
 0031      CONTINUE                                                     01830908
CT004*  TEST 4                                          FROM ARRAY      01840908
           IVTNUM = 4                                                   01850908
        READ(UNIT=C431K,FMT=39208) CVD                                  01860908
39208   FORMAT(25X,D18.10)                                              01870908
           IF (CVD - 0.2344999998D+04) 20040, 10040, 40040              01880908
40040      IF (CVD - 0.2345000002D+04) 10040, 10040, 20040              01890908
10040      IVPASS = IVPASS + 1                                          01900908
           WRITE (NUVI, 80002) IVTNUM                                   01910908
           GO TO 0041                                                   01920908
20040      IVFAIL = IVFAIL + 1                                          01930908
           DVCORR = 23.45D2                                             01940908
           WRITE (NUVI, 80031) IVTNUM, CVD, DVCORR                      01950908
 0041      CONTINUE                                                     01960908
C*****                                                                  01970908
C*****  TESTS 5 THRU 9 - LIST FROM ARRAY                                01980908
C*****                                                                  01990908
CT005*  TEST 5                                                          02000908
           IVTNUM = 5                                                   02010908
        READ(UNIT=B291K,FMT=39210) AVD, BVD, CVD, DVD, EVD              02020908
39210   FORMAT(D10.5,1X,D10.5,/,D10.5,/,D10.5,//,D10.5)                 02030908
           IF (AVD - 0.3438456998D+03) 20050, 10050, 40050              02040908
40050      IF (AVD - 0.3438457002D+03) 10050, 10050, 20050              02050908
10050      IVPASS = IVPASS + 1                                          02060908
           WRITE (NUVI, 80002) IVTNUM                                   02070908
           GO TO 0051                                                   02080908
20050      IVFAIL = IVFAIL + 1                                          02090908
           DVCORR = 34.38457D1                                          02100908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02110908
 0051      CONTINUE                                                     02120908
CT006*  TEST 6                                                          02130908
           IVTNUM = 6                                                   02140908
           IF (BVD - 0.3438456998D+03) 20060, 10060, 40060              02150908
40060      IF (BVD - 0.3438457002D+03) 10060, 10060, 20060              02160908
10060      IVPASS = IVPASS + 1                                          02170908
           WRITE (NUVI, 80002) IVTNUM                                   02180908
           GO TO 0061                                                   02190908
20060      IVFAIL = IVFAIL + 1                                          02200908
           DVCORR = 34.38457D1                                          02210908
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      02220908
 0061      CONTINUE                                                     02230908
CT007*  TEST 7                                                          02240908
           IVTNUM = 7                                                   02250908
           IF (CVD - 0.3438456998D+03) 20070, 10070, 40070              02260908
40070      IF (CVD - 0.3438457002D+03) 10070, 10070, 20070              02270908
10070      IVPASS = IVPASS + 1                                          02280908
           WRITE (NUVI, 80002) IVTNUM                                   02290908
           GO TO 0071                                                   02300908
20070      IVFAIL = IVFAIL + 1                                          02310908
           DVCORR = 34.38457D1                                          02320908
           WRITE (NUVI, 80031) IVTNUM, CVD, DVCORR                      02330908
 0071      CONTINUE                                                     02340908
CT008*  TEST 8                                                          02350908
           IVTNUM = 8                                                   02360908
           IF (DVD - 0.3438456998D+03) 20080, 10080, 40080              02370908
40080      IF (DVD - 0.3438457002D+03) 10080, 10080, 20080              02380908
10080      IVPASS = IVPASS + 1                                          02390908
           WRITE (NUVI, 80002) IVTNUM                                   02400908
           GO TO 0081                                                   02410908
20080      IVFAIL = IVFAIL + 1                                          02420908
           DVCORR = 34.38457D1                                          02430908
           WRITE (NUVI, 80031) IVTNUM, DVD, DVCORR                      02440908
 0081      CONTINUE                                                     02450908
CT009*  TEST 9                                                          02460908
           IVTNUM = 9                                                   02470908
           IF (EVD - 0.3438456998D+03) 20090, 10090, 40090              02480908
40090      IF (EVD - 0.3438457002D+03) 10090, 10090, 20090              02490908
10090      IVPASS = IVPASS + 1                                          02500908
           WRITE (NUVI, 80002) IVTNUM                                   02510908
           GO TO 0091                                                   02520908
20090      IVFAIL = IVFAIL + 1                                          02530908
           DVCORR = 34.38457D1                                          02540908
           WRITE (NUVI, 80031) IVTNUM, EVD, DVCORR                      02550908
 0091      CONTINUE                                                     02560908
C*****                                                                  02570908
C*****  TESTS 10 THRU 13 - LIST FROM VARIABLE WITH DIFFERENT FORMATS    02580908
C*****                                                                  02590908
CT010*  TEST 10                                                         02600908
           IVTNUM = 10                                                  02610908
        READ(UNIT=D43VK,FMT=39212) AVD, BVD, CVD, DVD                   02620908
39212   FORMAT(D10.5,1X,F10.5,D11.5,G11.5)                              02630908
           IF (AVD - 0.3458672998D+04) 20100, 10100, 40100              02640908
40100      IF (AVD - 0.3458673002D+04) 10100, 10100, 20100              02650908
10100      IVPASS = IVPASS + 1                                          02660908
           WRITE (NUVI, 80002) IVTNUM                                   02670908
           GO TO 0101                                                   02680908
20100      IVFAIL = IVFAIL + 1                                          02690908
           DVCORR = 34.58673D2                                          02700908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      02710908
 0101      CONTINUE                                                     02720908
CT011*  TEST 11                                                         02730908
           IVTNUM = 11                                                  02740908
           IF (BVD - 0.3458672998D+04) 20110, 10110, 40110              02750908
40110      IF (BVD - 0.3458673002D+04) 10110, 10110, 20110              02760908
10110      IVPASS = IVPASS + 1                                          02770908
           WRITE (NUVI, 80002) IVTNUM                                   02780908
           GO TO 0111                                                   02790908
20110      IVFAIL = IVFAIL + 1                                          02800908
           DVCORR = 34.58673D2                                          02810908
           WRITE (NUVI, 80031) IVTNUM, BVD, DVCORR                      02820908
 0111      CONTINUE                                                     02830908
CT012*  TEST 12                                                         02840908
           IVTNUM = 12                                                  02850908
           IF (CVD - 0.3458672998D+04) 20120, 10120, 40120              02860908
40120      IF (CVD - 0.3458673002D+04) 10120, 10120, 20120              02870908
10120      IVPASS = IVPASS + 1                                          02880908
           WRITE (NUVI, 80002) IVTNUM                                   02890908
           GO TO 0121                                                   02900908
20120      IVFAIL = IVFAIL + 1                                          02910908
           DVCORR = 34.58673D2                                          02920908
           WRITE (NUVI, 80031) IVTNUM, CVD, DVCORR                      02930908
 0121      CONTINUE                                                     02940908
CT013*  TEST 13                                                         02950908
           IVTNUM = 13                                                  02960908
           IF (DVD - 0.3458672998D+04) 20130, 10130, 40130              02970908
40130      IF (DVD - 0.3458673002D+04) 10130, 10130, 20130              02980908
10130      IVPASS = IVPASS + 1                                          02990908
           WRITE (NUVI, 80002) IVTNUM                                   03000908
           GO TO 0131                                                   03010908
20130      IVFAIL = IVFAIL + 1                                          03020908
           DVCORR = 34.58673D2                                          03030908
           WRITE (NUVI, 80031) IVTNUM, DVD, DVCORR                      03040908
 0131      CONTINUE                                                     03050908
C*****                                                                  03060908
        WRITE (NUVI, 90002)                                             03070908
        WRITE (NUVI, 90013)                                             03080908
        WRITE (NUVI, 90014)                                             03090908
C*****                                                                  03100908
C*****  TESTS 14 THRU 19 - MIXED TYPES                                  03110908
C*****                                                                  03120908
CT014*  TEST 14                                                         03130908
           IVTNUM = 14                                                  03140908
        READ(UNIT=E51VK,FMT=39214) KVI, AVS, AVD, AVB, BVS, A8VK        03150908
39214   FORMAT(I4,1X,E9.4,D10.4,1X,L4,1X,F12.5,1X,A8)                   03160908
           IF (KVI - 348) 20140, 10140, 20140                           03170908
10140      IVPASS = IVPASS + 1                                          03180908
           WRITE (NUVI, 80002) IVTNUM                                   03190908
           GO TO 0141                                                   03200908
20140      IVFAIL = IVFAIL + 1                                          03210908
           IVCORR = 348                                                 03220908
           WRITE (NUVI, 80010) IVTNUM, KVI, IVCORR                      03230908
 0141      CONTINUE                                                     03240908
CT015*  TEST 15                                                         03250908
           IVTNUM = 15                                                  03260908
           IF (AVS - 0.34781E+02) 20150, 10150, 40150                   03270908
40150      IF (AVS - 0.34785E+02) 10150, 10150, 20150                   03280908
10150      IVPASS = IVPASS + 1                                          03290908
           WRITE (NUVI, 80002) IVTNUM                                   03300908
           GO TO 0151                                                   03310908
20150      IVFAIL = IVFAIL + 1                                          03320908
           RVCORR = 34.783                                              03330908
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      03340908
 0151      CONTINUE                                                     03350908
CT016*  TEST 16                                                         03360908
           IVTNUM = 16                                                  03370908
           IF (AVD - 0.3843846998D+04) 20160, 10160, 40160              03380908
40160      IF (AVD - 0.3843847002D+04) 10160, 10160, 20160              03390908
10160      IVPASS = IVPASS + 1                                          03400908
           WRITE (NUVI, 80002) IVTNUM                                   03410908
           GO TO 0161                                                   03420908
20160      IVFAIL = IVFAIL + 1                                          03430908
           DVCORR = 384.3847D1                                          03440908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      03450908
 0161      CONTINUE                                                     03460908
CT017*  TEST 17                                                         03470908
           IVTNUM = 17                                                  03480908
           IVCOMP = 0                                                   03490908
           IF (AVB) IVCOMP = 1                                          03500908
           IF (IVCOMP - 1) 20170, 10170, 20170                          03510908
10170      IVPASS = IVPASS + 1                                          03520908
           WRITE (NUVI, 80002) IVTNUM                                   03530908
           GO TO 0171                                                   03540908
20170      IVFAIL = IVFAIL + 1                                          03550908
           LVCORR = 1                                                   03560908
           REMRKS = '1 = TRUE ;  0 = FALSE'                             03570908
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           03580908
           WRITE (NUVI, 80024) IVCOMP                                   03590908
           WRITE (NUVI, 80026) LVCORR                                   03600908
 0171      CONTINUE                                                     03610908
CT018*  TEST 18                                                         03620908
           IVTNUM = 18                                                  03630908
           IF (BVS - 0.34855E+01) 20180, 10180, 40180                   03640908
40180      IF (BVS - 0.34859E+01) 10180, 10180, 20180                   03650908
10180      IVPASS = IVPASS + 1                                          03660908
           WRITE (NUVI, 80002) IVTNUM                                   03670908
           GO TO 0181                                                   03680908
20180      IVFAIL = IVFAIL + 1                                          03690908
           RVCORR = 3.4857                                              03700908
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      03710908
 0181      CONTINUE                                                     03720908
CT019*  TEST 19                                                         03730908
           IVTNUM = 19                                                  03740908
           IVCOMP = 0                                                   03750908
           IF (A8VK.EQ.'KDFJ D/.') IVCOMP = 1                           03760908
           IF (IVCOMP - 1) 20190, 10190, 20190                          03770908
10190      IVPASS = IVPASS + 1                                          03780908
           WRITE (NUVI, 80002) IVTNUM                                   03790908
           GO TO 0191                                                   03800908
20190      IVFAIL = IVFAIL + 1                                          03810908
           CVCORR = 'KDFJ D/.'                                          03820908
           WRITE (NUVI, 80018) IVTNUM, A8VK, CVCORR                     03830908
 0191      CONTINUE                                                     03840908
C*****                                                                  03850908
C*****  TESTS 20 THRU 25 - MIXED TYPES WITH TC, TLC, TRC, AND NX        03860908
C*****                                                                  03870908
CT020*  TEST 20                                                         03880908
           IVTNUM = 20                                                  03890908
        READ(UNIT=F43VK,FMT=39216) AVB, AVS, A8VK, AVD, BVS, KVI        03900908
39216   FORMAT(L1,T5,F5.2,A8,TR2,D8.4,TL8,F6.4,4X,I1)                   03910908
           IVCOMP = 0                                                   03920908
           IF (AVB) IVCOMP = 1                                          03930908
           IF (IVCOMP - 1) 20200, 10200, 20200                          03940908
10200      IVPASS = IVPASS + 1                                          03950908
           WRITE (NUVI, 80002) IVTNUM                                   03960908
           GO TO 0201                                                   03970908
20200      IVFAIL = IVFAIL + 1                                          03980908
           LVCORR = 1                                                   03990908
           REMRKS = '1 = TRUE ;  0 = FALSE'                             04000908
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           04010908
           WRITE (NUVI, 80024) IVCOMP                                   04020908
           WRITE (NUVI, 80026) LVCORR                                   04030908
 0201      CONTINUE                                                     04040908
CT021*  TEST 21                                                         04050908
           IVTNUM = 21                                                  04060908
           IF (AVS - 0.10979E+02) 20210, 10210, 40210                   04070908
40210      IF (AVS - 0.10981E+02) 10210, 10210, 20210                   04080908
10210      IVPASS = IVPASS + 1                                          04090908
           WRITE (NUVI, 80002) IVTNUM                                   04100908
           GO TO 0211                                                   04110908
20210      IVFAIL = IVFAIL + 1                                          04120908
           RVCORR = 10.98                                               04130908
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      04140908
 0211      CONTINUE                                                     04150908
CT022*  TEST 22                                                         04160908
           IVTNUM = 22                                                  04170908
           IVCOMP = 0                                                   04180908
           IF (A8VK.EQ.'THISISIT') IVCOMP = 1                           04190908
           IF (IVCOMP - 1) 20220, 10220, 20220                          04200908
10220      IVPASS = IVPASS + 1                                          04210908
           WRITE (NUVI, 80002) IVTNUM                                   04220908
           GO TO 0221                                                   04230908
20220      IVFAIL = IVFAIL + 1                                          04240908
           CVCORR = 'THISISIT'                                          04250908
           WRITE (NUVI, 80018) IVTNUM, A8VK, CVCORR                     04260908
 0221      CONTINUE                                                     04270908
CT023*  TEST 23                                                         04280908
           IVTNUM = 23                                                  04290908
           IF (AVD - 0.3494499998D+03) 20230, 10230, 40230              04300908
40230      IF (AVD - 0.3494500002D+03) 10230, 10230, 20230              04310908
10230      IVPASS = IVPASS + 1                                          04320908
           WRITE (NUVI, 80002) IVTNUM                                   04330908
           GO TO 0231                                                   04340908
20230      IVFAIL = IVFAIL + 1                                          04350908
           DVCORR = 3.4945D2                                            04360908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      04370908
 0231      CONTINUE                                                     04380908
CT024*  TEST 24                                                         04390908
           IVTNUM = 24                                                  04400908
           IF (BVS - 0.34943E+01) 20240, 10240, 40240                   04410908
40240      IF (BVS - 0.34947E+01) 10240, 10240, 20240                   04420908
10240      IVPASS = IVPASS + 1                                          04430908
           WRITE (NUVI, 80002) IVTNUM                                   04440908
           GO TO 0241                                                   04450908
20240      IVFAIL = IVFAIL + 1                                          04460908
           RVCORR = 3.4945                                              04470908
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      04480908
 0241      CONTINUE                                                     04490908
CT025*  TEST 25                                                         04500908
           IVTNUM = 25                                                  04510908
           IF (KVI - 3) 20250, 10250, 20250                             04520908
10250      IVPASS = IVPASS + 1                                          04530908
           WRITE (NUVI, 80002) IVTNUM                                   04540908
           GO TO 0251                                                   04550908
20250      IVFAIL = IVFAIL + 1                                          04560908
           IVCORR = 3                                                   04570908
           WRITE (NUVI, 80010) IVTNUM, KVI, IVCORR                      04580908
 0251      CONTINUE                                                     04590908
CT026*  TEST 26                                 COMPLEX FROM VARIABLE   04600908
           IVTNUM = 26                                                  04610908
        READ(UNIT=G43VK,FMT=39218) AVC                                  04620908
39218   FORMAT(F10.5,1X,F10.5)                                          04630908
           IF (R2E(1) - 0.23428E+01) 20260, 40262, 40261                04640908
40261      IF (R2E(1) - 0.23432E+01) 40262, 40262, 20260                04650908
40262      IF (R2E(2) - 0.34392E+02) 20260, 10260, 40260                04660908
40260      IF (R2E(2) - 0.34396E+02) 10260, 10260, 20260                04670908
10260      IVPASS = IVPASS + 1                                          04680908
           WRITE (NUVI, 80002) IVTNUM                                   04690908
           GO TO 0261                                                   04700908
20260      IVFAIL = IVFAIL + 1                                          04710908
           ZVCORR = (2.343, 34.394)                                     04720908
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      04730908
 0261      CONTINUE                                                     04740908
CT027*  TEST 27                                 COMPLEX FROM ARRAY      04750908
           IVTNUM = 27                                                  04760908
        READ(UNIT=H131K,FMT=39220) AVC                                  04770908
39220   FORMAT(E12.5,/,E12.5)                                           04780908
           IF (R2E(1) - 0.34838E+02) 20270, 40272, 40271                04790908
40271      IF (R2E(1) - 0.34842E+02) 40272, 40272, 20270                04800908
40272      IF (R2E(2) - 0.34987E+03) 20270, 10270, 40270                04810908
40270      IF (R2E(2) - 0.34991E+03) 10270, 10270, 20270                04820908
10270      IVPASS = IVPASS + 1                                          04830908
           WRITE (NUVI, 80002) IVTNUM                                   04840908
           GO TO 0271                                                   04850908
20270      IVFAIL = IVFAIL + 1                                          04860908
           ZVCORR = (34.84, 349.887)                                    04870908
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      04880908
 0271      CONTINUE                                                     04890908
C*****                                                                  04900908
        WRITE (NUVI, 90002)                                             04910908
        WRITE (NUVI, 90013)                                             04920908
        WRITE (NUVI, 90014)                                             04930908
C*****                                                                  04940908
C*****  TESTS 28 THRU 31 - COMPLEX LIST FROM VARIABLE POSITION 1X BEYOND04950908
C*****                     VARIABLE LENGTH                              04960908
CT028*  TEST 28                                                         04970908
           IVTNUM = 28                                                  04980908
        READ(UNIT=I82VK,FMT=39222) AVC, BVC, CVC, DVC                   04990908
39222   FORMAT(2(2(G7.5,1X),2X),2(G10.4E2,1X),1X,2(G11.7E4,1X))         05000908
           IF (R2E(1) - 0.23398E+01) 20280, 40282, 40281                05010908
40281      IF (R2E(1) - 0.23402E+01) 40282, 40282, 20280                05020908
40282      IF (R2E(2) - 0.24558E+01) 20280, 10280, 40280                05030908
40280      IF (R2E(2) - 0.24562E+01) 10280, 10280, 20280                05040908
10280      IVPASS = IVPASS + 1                                          05050908
           WRITE (NUVI, 80002) IVTNUM                                   05060908
           GO TO 0281                                                   05070908
20280      IVFAIL = IVFAIL + 1                                          05080908
           ZVCORR = (2.34, 2.456)                                       05090908
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      05100908
 0281      CONTINUE                                                     05110908
CT029*  TEST 29                                                         05120908
           IVTNUM = 29                                                  05130908
           IF (R2E(3) - 0.23398E+01) 20290, 40292, 40291                05140908
40291      IF (R2E(3) - 0.23402E+01) 40292, 40292, 20290                05150908
40292      IF (R2E(4) - 0.24558E+01) 20290, 10290, 40290                05160908
40290      IF (R2E(4) - 0.24562E+01) 10290, 10290, 20290                05170908
10290      IVPASS = IVPASS + 1                                          05180908
           WRITE (NUVI, 80002) IVTNUM                                   05190908
           GO TO 0291                                                   05200908
20290      IVFAIL = IVFAIL + 1                                          05210908
           ZVCORR = (2.34, 2.456)                                       05220908
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      05230908
 0291      CONTINUE                                                     05240908
CT030*  TEST 30                                                         05250908
           IVTNUM = 30                                                  05260908
           IF (R2E(5) - 0.23398E+01) 20300, 40302, 40301                05270908
40301      IF (R2E(5) - 0.23402E+01) 40302, 40302, 20300                05280908
40302      IF (R2E(6) - 0.24558E+01) 20300, 10300, 40300                05290908
40300      IF (R2E(6) - 0.24562E+01) 10300, 10300, 20300                05300908
10300      IVPASS = IVPASS + 1                                          05310908
           WRITE (NUVI, 80002) IVTNUM                                   05320908
           GO TO 0301                                                   05330908
20300      IVFAIL = IVFAIL + 1                                          05340908
           ZVCORR = (2.34, 2.456)                                       05350908
           WRITE (NUVI, 80045) IVTNUM, CVC, ZVCORR                      05360908
 0301      CONTINUE                                                     05370908
CT031*  TEST 31                                                         05380908
           IVTNUM = 31                                                  05390908
           IF (R2E(7) - 0.23398E+01) 20310, 40312, 40311                05400908
40311      IF (R2E(7) - 0.23402E+01) 40312, 40312, 20310                05410908
40312      IF (R2E(8) - 0.24558E+01) 20310, 10310, 40310                05420908
40310      IF (R2E(8) - 0.24562E+01) 10310, 10310, 20310                05430908
10310      IVPASS = IVPASS + 1                                          05440908
           WRITE (NUVI, 80002) IVTNUM                                   05450908
           GO TO 0311                                                   05460908
20310      IVFAIL = IVFAIL + 1                                          05470908
           ZVCORR = (2.34, 2.456)                                       05480908
           WRITE (NUVI, 80045) IVTNUM, DVC, ZVCORR                      05490908
 0311      CONTINUE                                                     05500908
C*****                                                                  05510908
C*****  TESTS 32 THRU 35 - COMPLEX LIST USING EW.D AND EW.DEN           05520908
C*****                                                                  05530908
CT032*  TEST 32                                                         05540908
           IVTNUM = 32                                                  05550908
        READ(UNIT=J97VK(1:),FMT=39224) AVC, BVC, CVC, DVC               05560908
39224   FORMAT(2(2(E10.5,1X),2X),2(E10.4E2,1X),1X,2(E12.5E4,1X))        05570908
           IF (R2E(1) - 0.56795E+01) 20320, 40322, 40321                05580908
40321      IF (R2E(1) - 0.56801E+01) 40322, 40322, 20320                05590908
40322      IF (R2E(2) - 0.98755E+00) 20320, 10320, 40320                05600908
40320      IF (R2E(2) - 0.98765E+00) 10320, 10320, 20320                05610908
10320      IVPASS = IVPASS + 1                                          05620908
           WRITE (NUVI, 80002) IVTNUM                                   05630908
           GO TO 0321                                                   05640908
20320      IVFAIL = IVFAIL + 1                                          05650908
           ZVCORR = (5.6798, 0.9876)                                    05660908
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      05670908
 0321      CONTINUE                                                     05680908
CT033*  TEST 33                                                         05690908
           IVTNUM = 33                                                  05700908
           IF (R2E(3) - 0.56795E+01) 20330, 40332, 40331                05710908
40331      IF (R2E(3) - 0.56801E+01) 40332, 40332, 20330                05720908
40332      IF (R2E(4) - 0.98755E+00) 20330, 10330, 40330                05730908
40330      IF (R2E(4) - 0.98765E+00) 10330, 10330, 20330                05740908
10330      IVPASS = IVPASS + 1                                          05750908
           WRITE (NUVI, 80002) IVTNUM                                   05760908
           GO TO 0331                                                   05770908
20330      IVFAIL = IVFAIL + 1                                          05780908
           ZVCORR = (5.6798, 0.9876)                                    05790908
           WRITE (NUVI, 80045) IVTNUM, BVC, ZVCORR                      05800908
 0331      CONTINUE                                                     05810908
CT034*  TEST 34                                                         05820908
           IVTNUM = 34                                                  05830908
           IF (R2E(5) - 0.56795E+01) 20340, 40342, 40341                05840908
40341      IF (R2E(5) - 0.56801E+01) 40342, 40342, 20340                05850908
40342      IF (R2E(6) - 0.98755E+00) 20340, 10340, 40340                05860908
40340      IF (R2E(6) - 0.98765E+00) 10340, 10340, 20340                05870908
10340      IVPASS = IVPASS + 1                                          05880908
           WRITE (NUVI, 80002) IVTNUM                                   05890908
           GO TO 0341                                                   05900908
20340      IVFAIL = IVFAIL + 1                                          05910908
           ZVCORR = (5.6798, 0.9876)                                    05920908
           WRITE (NUVI, 80045) IVTNUM, CVC, ZVCORR                      05930908
 0341      CONTINUE                                                     05940908
CT035*  TEST 35                                                         05950908
           IVTNUM = 35                                                  05960908
           IF (R2E(7) - 0.56795E+01) 20350, 40352, 40351                05970908
40351      IF (R2E(7) - 0.56801E+01) 40352, 40352, 20350                05980908
40352      IF (R2E(8) - 0.98755E+00) 20350, 10350, 40350                05990908
40350      IF (R2E(8) - 0.98765E+00) 10350, 10350, 20350                06000908
10350      IVPASS = IVPASS + 1                                          06010908
           WRITE (NUVI, 80002) IVTNUM                                   06020908
           GO TO 0351                                                   06030908
20350      IVFAIL = IVFAIL + 1                                          06040908
           ZVCORR = (5.6798, 0.9876)                                    06050908
           WRITE (NUVI, 80045) IVTNUM, DVC, ZVCORR                      06060908
 0351      CONTINUE                                                     06070908
C*****                                                                  06080908
C*****  TESTS 36 THRU 38 - MIXED TYPES FROM VARIABLE                    06090908
C*****                                                                  06100908
CT036*  TEST 36                                                         06110908
           IVTNUM = 36                                                  06120908
        READ(UNIT=K43VK,FMT=39226) AVC, AVS, AVD                        06130908
39226   FORMAT(F7.3,1X,F7.3,1X,F10.5,1X,D13.5)                          06140908
           IF (R2E(1) - 0.93395E+00) 20360, 40362, 40361                06150908
40361      IF (R2E(1) - 0.93405E+00) 40362, 40362, 20360                06160908
40362      IF (R2E(2) - 0.34565E+02) 20360, 10360, 40360                06170908
40360      IF (R2E(2) - 0.34569E+02) 10360, 10360, 20360                06180908
10360      IVPASS = IVPASS + 1                                          06190908
           WRITE (NUVI, 80002) IVTNUM                                   06200908
           GO TO 0361                                                   06210908
20360      IVFAIL = IVFAIL + 1                                          06220908
           ZVCORR = (0.934, 34.567)                                     06230908
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      06240908
 0361      CONTINUE                                                     06250908
CT037*  TEST 37                                                         06260908
           IVTNUM = 37                                                  06270908
           IF (AVS - 0.34648E+02) 20370, 10370, 40370                   06280908
40370      IF (AVS - 0.34652E+02) 10370, 10370, 20370                   06290908
10370      IVPASS = IVPASS + 1                                          06300908
           WRITE (NUVI, 80002) IVTNUM                                   06310908
           GO TO 0371                                                   06320908
20370      IVFAIL = IVFAIL + 1                                          06330908
           RVCORR = 34.65                                               06340908
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      06350908
 0371      CONTINUE                                                     06360908
CT038*  TEST 38                                                         06370908
           IVTNUM = 38                                                  06380908
           IF (AVD - 0.6353999996D+01) 20380, 10380, 40380              06390908
40380      IF (AVD - 0.6354000004D+01) 10380, 10380, 20380              06400908
10380      IVPASS = IVPASS + 1                                          06410908
           WRITE (NUVI, 80002) IVTNUM                                   06420908
           GO TO 0381                                                   06430908
20380      IVFAIL = IVFAIL + 1                                          06440908
           DVCORR = 0.6354D1                                            06450908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      06460908
 0381      CONTINUE                                                     06470908
C*****                                                                  06480908
C*****  TESTS 39 THRU 43 - MIXED TYPES FROM ARRAY                       06490908
C*****                                                                  06500908
CT039*  TEST 39                                                         06510908
           IVTNUM = 39                                                  06520908
        READ(UNIT=L53VK,FMT=39228) AVC, AVB, AVD, AVS, A8VK             06530908
39228   FORMAT(F9.4,1X,E9.4,1X,L1,1X,D12.5,1X,G9.4,A8)                  06540908
           IF (R2E(1) - 0.34498E+00) 20390, 40392, 40391                06550908
40391      IF (R2E(1) - 0.34502E+00) 40392, 40392, 20390                06560908
40392      IF (R2E(2) - 0.34343E+02) 20390, 10390, 40390                06570908
40390      IF (R2E(2) - 0.34347E+02) 10390, 10390, 20390                06580908
10390      IVPASS = IVPASS + 1                                          06590908
           WRITE (NUVI, 80002) IVTNUM                                   06600908
           GO TO 0391                                                   06610908
20390      IVFAIL = IVFAIL + 1                                          06620908
           ZVCORR = (0.345, 34.345)                                     06630908
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      06640908
 0391      CONTINUE                                                     06650908
CT040*  TEST 40                                                         06660908
           IVTNUM = 40                                                  06670908
           IVCOMP = 0                                                   06680908
           IF (AVB) IVCOMP = 1                                          06690908
           IF (IVCOMP - 0) 20400, 10400, 20400                          06700908
10400      IVPASS = IVPASS + 1                                          06710908
           WRITE (NUVI, 80002) IVTNUM                                   06720908
           GO TO 0401                                                   06730908
20400      IVFAIL = IVFAIL + 1                                          06740908
           LVCORR = 0                                                   06750908
           REMRKS = '1 = TRUE ;  0 = FALSE'                             06760908
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           06770908
           WRITE (NUVI, 80024) IVCOMP                                   06780908
           WRITE (NUVI, 80026) LVCORR                                   06790908
 0401      CONTINUE                                                     06800908
CT041*  TEST 41                                                         06810908
           IVTNUM = 41                                                  06820908
           IF (AVD - 0.3485899998D+01) 20410, 10410, 40410              06830908
40410      IF (AVD - 0.3485900002D+01) 10410, 10410, 20410              06840908
10410      IVPASS = IVPASS + 1                                          06850908
           WRITE (NUVI, 80002) IVTNUM                                   06860908
           GO TO 0411                                                   06870908
20410      IVFAIL = IVFAIL + 1                                          06880908
           DVCORR = 34.859D-1                                           06890908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      06900908
 0411      CONTINUE                                                     06910908
CT042*  TEST 42                                                         06920908
           IVTNUM = 42                                                  06930908
           IF (AVS - 0.99995E+01) 20420, 10420, 40420                   06940908
40420      IF (AVS - 0.10001E+02) 10420, 10420, 20420                   06950908
10420      IVPASS = IVPASS + 1                                          06960908
           WRITE (NUVI, 80002) IVTNUM                                   06970908
           GO TO 0421                                                   06980908
20420      IVFAIL = IVFAIL + 1                                          06990908
           RVCORR = 10.0                                                07000908
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      07010908
 0421      CONTINUE                                                     07020908
CT043*  TEST 43                                                         07030908
           IVTNUM = 43                                                  07040908
           IVCOMP = 0                                                   07050908
           IF (A8VK.EQ.'12345678') IVCOMP = 1                           07060908
           IF (IVCOMP - 1) 20430, 10430, 20430                          07070908
10430      IVPASS = IVPASS + 1                                          07080908
           WRITE (NUVI, 80002) IVTNUM                                   07090908
           GO TO 0431                                                   07100908
20430      IVFAIL = IVFAIL + 1                                          07110908
           CVCORR = '12345678'                                          07120908
           WRITE (NUVI, 80018) IVTNUM, A8VK, CVCORR                     07130908
 0431      CONTINUE                                                     07140908
C*****                                                                  07150908
        WRITE (NUVI, 90002)                                             07160908
        WRITE (NUVI, 90013)                                             07170908
        WRITE (NUVI, 90014)                                             07180908
C*****                                                                  07190908
C*****  TESTS 44 THRU 48 - READ 5 RECORD FROM ARRAY POSITION 1X BEYOND  07200908
C*****                     ARRAY ELEMENT                                07210908
C*****                                                                  07220908
CT044*  TEST 44                                                         07230908
           IVTNUM = 44                                                  07240908
        READ(UNIT=M291K,FMT=39230) KVI, AVD, AVB, AVC, A8VK             07250908
39230   FORMAT(I5,/,D10.5,//,1X,L6,1X,2(E10.3,1X),/,A8)                 07260908
           IF (KVI - 98) 20440, 10440, 20440                            07270908
10440      IVPASS = IVPASS + 1                                          07280908
           WRITE (NUVI, 80002) IVTNUM                                   07290908
           GO TO 0441                                                   07300908
20440      IVFAIL = IVFAIL + 1                                          07310908
           IVCORR = 98                                                  07320908
           WRITE (NUVI, 80010) IVTNUM, KVI, IVCORR                      07330908
 0441      CONTINUE                                                     07340908
CT045*  TEST 45                                                         07350908
           IVTNUM = 45                                                  07360908
           IF (AVD - 0.8404849995D+03) 20450, 10450, 40450              07370908
40450      IF (AVD - 0.8404850004D+03) 10450, 10450, 20450              07380908
10450      IVPASS = IVPASS + 1                                          07390908
           WRITE (NUVI, 80002) IVTNUM                                   07400908
           GO TO 0451                                                   07410908
20450      IVFAIL = IVFAIL + 1                                          07420908
           DVCORR = 84.0485D1                                           07430908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      07440908
 0451      CONTINUE                                                     07450908
CT046*  TEST 46                                                         07460908
           IVTNUM = 46                                                  07470908
           IVCOMP = 0                                                   07480908
           IF (AVB) IVCOMP = 1                                          07490908
           IF (IVCOMP - 1) 20460, 10460, 20460                          07500908
10460      IVPASS = IVPASS + 1                                          07510908
           WRITE (NUVI, 80002) IVTNUM                                   07520908
           GO TO 0461                                                   07530908
20460      IVFAIL = IVFAIL + 1                                          07540908
           LVCORR = 1                                                   07550908
           REMRKS = '1 = TRUE ;  0 = FALSE'                             07560908
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           07570908
           WRITE (NUVI, 80024) IVCOMP                                   07580908
           WRITE (NUVI, 80026) LVCORR                                   07590908
 0461      CONTINUE                                                     07600908
CT047*  TEST 47                                                         07610908
           IVTNUM = 47                                                  07620908
           IF (R2E(1) - 0.34041E+02) 20470, 40472, 40471                07630908
40471      IF (R2E(1) - 0.34046E+02) 40472, 40472, 20470                07640908
40472      IF (R2E(2) - 0.34938E+02) 20470, 10470, 40470                07650908
40470      IF (R2E(2) - 0.34942E+02) 10470, 10470, 20470                07660908
10470      IVPASS = IVPASS + 1                                          07670908
           WRITE (NUVI, 80002) IVTNUM                                   07680908
           GO TO 0471                                                   07690908
20470      IVFAIL = IVFAIL + 1                                          07700908
           ZVCORR = (34.0435, 34.94)                                    07710908
           WRITE (NUVI, 80045) IVTNUM, AVC, ZVCORR                      07720908
 0471      CONTINUE                                                     07730908
CT048*  TEST 48                                                         07740908
           IVTNUM = 48                                                  07750908
           IVCOMP = 0                                                   07760908
           IF (A8VK.EQ.'87654321') IVCOMP = 1                           07770908
           IF (IVCOMP - 1) 20480, 10480, 20480                          07780908
10480      IVPASS = IVPASS + 1                                          07790908
           WRITE (NUVI, 80002) IVTNUM                                   07800908
           GO TO 0481                                                   07810908
20480      IVFAIL = IVFAIL + 1                                          07820908
           CVCORR = '87654321'                                          07830908
           WRITE (NUVI, 80018) IVTNUM, A8VK, CVCORR                     07840908
 0481      CONTINUE                                                     07850908
C*****                                                                  07860908
C***** TESTS 49 THRU 54 - MIXED TYPES, NX, AND :                        07870908
C*****                                                                  07880908
CT049*  TEST 49                                                         07890908
           IVTNUM = 49                                                  07900908
        READ(UNIT=N43VK,FMT=39232)JVI,AVS,AVD,AVB,A8VK,BVS              07910908
39232   FORMAT(I2,1X,F6.3,D10.5,L2,A8,E10.5,:,I5,2X,F10.4)              07920908
           IF (JVI - 34) 20490, 10490, 20490                            07930908
10490      IVPASS = IVPASS + 1                                          07940908
           WRITE (NUVI, 80002) IVTNUM                                   07950908
           GO TO 0491                                                   07960908
20490      IVFAIL = IVFAIL + 1                                          07970908
           IVCORR = 34                                                  07980908
           WRITE (NUVI, 80010) IVTNUM, JVI, IVCORR                      07990908
 0491      CONTINUE                                                     08000908
CT050*  TEST 50                                                         08010908
           IVTNUM = 50                                                  08020908
           IF (AVS - 0.34981E+02) 20500, 10500, 40500                   08030908
40500      IF (AVS - 0.34985E+02) 10500, 10500, 20500                   08040908
10500      IVPASS = IVPASS + 1                                          08050908
           WRITE (NUVI, 80002) IVTNUM                                   08060908
           GO TO 0501                                                   08070908
20500      IVFAIL = IVFAIL + 1                                          08080908
           RVCORR = 34.983                                              08090908
           WRITE (NUVI, 80012) IVTNUM, AVS, RVCORR                      08100908
 0501      CONTINUE                                                     08110908
CT051*  TEST 51                                                         08120908
           IVTNUM = 51                                                  08130908
           IF (AVD - 0.9582999995D+04) 20510, 10510, 40510              08140908
40510      IF (AVD - 0.9583000005D+04) 10510, 10510, 20510              08150908
10510      IVPASS = IVPASS + 1                                          08160908
           WRITE (NUVI, 80002) IVTNUM                                   08170908
           GO TO 0511                                                   08180908
20510      IVFAIL = IVFAIL + 1                                          08190908
           DVCORR = 95.83D2                                             08200908
           WRITE (NUVI, 80031) IVTNUM, AVD, DVCORR                      08210908
 0511      CONTINUE                                                     08220908
CT052*  TEST 52                                                         08230908
           IVTNUM = 52                                                  08240908
           IVCOMP = 0                                                   08250908
           IF (AVB) IVCOMP = 1                                          08260908
           IF (IVCOMP - 0) 20520, 10520, 20520                          08270908
10520      IVPASS = IVPASS + 1                                          08280908
           WRITE (NUVI, 80002) IVTNUM                                   08290908
           GO TO 0521                                                   08300908
20520      IVFAIL = IVFAIL + 1                                          08310908
           LVCORR = 0                                                   08320908
           REMRKS = '1 = TRUE ;  0 = FALSE'                             08330908
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           08340908
           WRITE (NUVI, 80024) IVCOMP                                   08350908
           WRITE (NUVI, 80026) LVCORR                                   08360908
 0521      CONTINUE                                                     08370908
CT053*  TEST 53                                                         08380908
           IVTNUM = 53                                                  08390908
           IVCOMP = 0                                                   08400908
           IF (A8VK.EQ.'.FALSE.1') IVCOMP = 1                           08410908
           IF (IVCOMP - 1) 20530, 10530, 20530                          08420908
10530      IVPASS = IVPASS + 1                                          08430908
           WRITE (NUVI, 80002) IVTNUM                                   08440908
           GO TO 0531                                                   08450908
20530      IVFAIL = IVFAIL + 1                                          08460908
           CVCORR = '.FALSE.1'                                          08470908
           WRITE (NUVI, 80018) IVTNUM, A8VK, CVCORR                     08480908
 0531      CONTINUE                                                     08490908
CT054*  TEST 54                                                         08500908
           IVTNUM = 54                                                  08510908
           IF (BVS - 0.34528E+03) 20540, 10540, 40540                   08520908
40540      IF (BVS - 0.34532E+03) 10540, 10540, 20540                   08530908
10540      IVPASS = IVPASS + 1                                          08540908
           WRITE (NUVI, 80002) IVTNUM                                   08550908
           GO TO 0541                                                   08560908
20540      IVFAIL = IVFAIL + 1                                          08570908
           RVCORR = 345.3                                               08580908
           WRITE (NUVI, 80012) IVTNUM, BVS, RVCORR                      08590908
 0541      CONTINUE                                                     08600908
C*****                                                                  08610908
CBB** ********************** BBCSUM0  **********************************08620908
C**** WRITE OUT TEST SUMMARY                                            08630908
C****                                                                   08640908
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        08650908
      WRITE (I02, 90004)                                                08660908
      WRITE (I02, 90014)                                                08670908
      WRITE (I02, 90004)                                                08680908
      WRITE (I02, 90020) IVPASS                                         08690908
      WRITE (I02, 90022) IVFAIL                                         08700908
      WRITE (I02, 90024) IVDELE                                         08710908
      WRITE (I02, 90026) IVINSP                                         08720908
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 08730908
CBE** ********************** BBCSUM0  **********************************08740908
CBB** ********************** BBCFOOT0 **********************************08750908
C**** WRITE OUT REPORT FOOTINGS                                         08760908
C****                                                                   08770908
      WRITE (I02,90016) ZPROG, ZPROG                                    08780908
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     08790908
      WRITE (I02,90019)                                                 08800908
CBE** ********************** BBCFOOT0 **********************************08810908
CBB** ********************** BBCFMT0A **********************************08820908
C**** FORMATS FOR TEST DETAIL LINES                                     08830908
C****                                                                   08840908
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           08850908
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           08860908
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           08870908
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           08880908
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           08890908
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    08900908
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08910908
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              08920908
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08930908
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  08940908
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         08950908
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         08960908
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         08970908
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         08980908
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      08990908
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      09000908
80050 FORMAT (1H ,48X,A31)                                              09010908
CBE** ********************** BBCFMT0A **********************************09020908
CBB** ********************** BBCFMAT1 **********************************09030908
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     09040908
C****                                                                   09050908
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           09060908
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            09070908
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     09080908
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     09090908
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    09100908
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    09110908
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    09120908
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    09130908
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           09140908
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  09150908
     21H(,F12.5,2H, ,F12.5,1H))                                         09160908
CBE** ********************** BBCFMAT1 **********************************09170908
CBB** ********************** BBCFMT0B **********************************09180908
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                09190908
C****                                                                   09200908
90002 FORMAT (1H1)                                                      09210908
90004 FORMAT (1H )                                                      09220908
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               09230908
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09240908
90008 FORMAT (1H ,21X,A13,A17)                                          09250908
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       09260908
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    09270908
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     09280908
     1       7X,7HREMARKS,24X)                                          09290908
90014 FORMAT (1H ,46H----------------------------------------------,    09300908
     1        33H---------------------------------)                     09310908
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               09320908
C****                                                                   09330908
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             09340908
C****                                                                   09350908
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          09360908
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        09370908
     1        A13)                                                      09380908
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 09390908
C****                                                                   09400908
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 09410908
C****                                                                   09420908
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              09430908
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              09440908
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             09450908
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  09460908
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  09470908
CBE** ********************** BBCFMT0B **********************************09480908
C*****                                                                  09490908
C*****    END OF TEST SEGMENT 392                                       09500908
      STOP                                                              09510908
      END                                                               09520908
