      PROGRAM FM520                                                     00010520
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020520
C     TESTING PARAMETER STATEMENT                                       00030520
C                                                                       00040520
C                                                                       00050520
CBB** ********************** BBCCOMNT **********************************00060520
C****                                                                   00070520
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00080520
C****                          VERSION 2.0                              00090520
C****                                                                   00100520
C****                                                                   00110520
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00120520
C****                   GENERAL SERVICES ADMINISTRATION                 00130520
C****                   FEDERAL SOFTWARE TESTING CENTER                 00140520
C****                   5203 LEESBURG PIKE, SUITE 1100                  00150520
C****                      FALLS CHURCH, VA. 22041                      00160520
C****                                                                   00170520
C****                          (703) 756-6153                           00180520
C****                                                                   00190520
CBE** ********************** BBCCOMNT **********************************00200520
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00210520
           IMPLICIT CHARACTER*27 (C)                                    00220520
CBB** ********************** BBCINITA **********************************00230520
C**** SPECIFICATION STATEMENTS                                          00240520
C****                                                                   00250520
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00260520
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00270520
CBE** ********************** BBCINITA **********************************00280520
C                                                                       00290520
      PARAMETER(IPN001=5+5,IPN002=8-3,IPN003=1*5)                       00300520
      PARAMETER(RPN001=5.1+4.9,RPN002=8.7-3.7,RPN003=2.0*2.5)           00310520
C                                                                       00320520
C     TEST 1 - 7 TEST INTEGER ARITHMETIC EXPRESSION USING               00330520
C                ONLY SYMBOLIC NAMES OF ARITHMETIC CONSTANTS            00340520
C                S06AF-2P 4.A                                           00350520
C                                                                       00360520
C                                                                       00370520
C                                                                       00380520
CBB** ********************** BBCINITB **********************************00390520
C**** INITIALIZE SECTION                                                00400520
      DATA  ZVERS,                  ZVERSD,             ZDATE           00410520
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00420520
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00430520
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00440520
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00450520
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00460520
      DATA   REMRKS /'                               '/                 00470520
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00480520
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00490520
C****                                                                   00500520
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00510520
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00520520
CZ03  ZPROG  = 'PROGRAM NAME'                                           00530520
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00600520
      IVPASS = 0                                                        00610520
      IVFAIL = 0                                                        00620520
      IVDELE = 0                                                        00630520
      IVINSP = 0                                                        00640520
      IVTOTL = 0                                                        00650520
      IVTOTN = 0                                                        00660520
      ICZERO = 0                                                        00670520
C                                                                       00680520
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00690520
      I01 = 05                                                          00700520
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00710520
      I02 = 06                                                          00720520
C                                                                       00730520
      I01 = 5                                                           00740520
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750520
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00760520
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00770520
C                                                                       00780520
      I02 = 6                                                           00790520
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00800520
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00810520
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00820520
C                                                                       00830520
CBE** ********************** BBCINITB **********************************00840520
           ZPROG='FM520'                                                00850520
           IVTOTL =  30                                                 00860520
CBB** ********************** BBCHED0A **********************************00870520
C****                                                                   00880520
C**** WRITE REPORT TITLE                                                00890520
C****                                                                   00900520
      WRITE (I02, 90002)                                                00910520
      WRITE (I02, 90006)                                                00920520
      WRITE (I02, 90007)                                                00930520
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940520
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950520
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960520
CBE** ********************** BBCHED0A **********************************00970520
CBB** ********************** BBCHED0B **********************************00980520
C**** WRITE DETAIL REPORT HEADERS                                       00990520
C****                                                                   01000520
      WRITE (I02,90004)                                                 01010520
      WRITE (I02,90004)                                                 01020520
      WRITE (I02,90013)                                                 01030520
      WRITE (I02,90014)                                                 01040520
      WRITE (I02,90015) IVTOTL                                          01050520
CBE** ********************** BBCHED0B **********************************01060520
C                                                                       01070520
CT001*  TEST 001   ****  FCVS PROGRAM 520  ****                         01080520
C                                                                       01090520
C                                                                       01100520
           IVTNUM =   1                                                 01110520
        IVCOMP=+IPN001                                                  01120520
           IVCORR=+10                                                   01130520
40010      IF (IVCOMP - 10) 20010, 10010, 20010                         01140520
10010      IVPASS = IVPASS + 1                                          01150520
           WRITE (I02,80002) IVTNUM                                     01160520
           GO TO 0021                                                   01170520
20010      IVFAIL = IVFAIL + 1                                          01180520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01190520
 0021      CONTINUE                                                     01200520
C                                                                       01210520
CT002*  TEST 002   ****  FCVS PROGRAM 520  ****                         01220520
C                                                                       01230520
C                                                                       01240520
           IVTNUM =   2                                                 01250520
        IVCOMP=-IPN001                                                  01260520
           IVCORR=-10                                                   01270520
40020      IF (IVCOMP + 10) 20020, 10020, 20020                         01280520
10020      IVPASS = IVPASS + 1                                          01290520
           WRITE (I02,80002) IVTNUM                                     01300520
           GO TO 0031                                                   01310520
20020      IVFAIL = IVFAIL + 1                                          01320520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01330520
 0031      CONTINUE                                                     01340520
C                                                                       01350520
CT003*  TEST 003   ****  FCVS PROGRAM 520  ****                         01360520
C                                                                       01370520
C                                                                       01380520
           IVTNUM =   3                                                 01390520
        IVCOMP=IPN001+IPN002                                            01400520
           IVCORR=15                                                    01410520
40030      IF (IVCOMP - 15) 20030, 10030, 20030                         01420520
10030      IVPASS = IVPASS + 1                                          01430520
           WRITE (I02,80002) IVTNUM                                     01440520
           GO TO 0041                                                   01450520
20030      IVFAIL = IVFAIL + 1                                          01460520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01470520
 0041      CONTINUE                                                     01480520
C                                                                       01490520
CT004*  TEST 004   ****  FCVS PROGRAM 520  ****                         01500520
C                                                                       01510520
C                                                                       01520520
           IVTNUM =   4                                                 01530520
        IVCOMP=IPN001-IPN002                                            01540520
           IVCORR=5                                                     01550520
40040      IF (IVCOMP - 5) 20040, 10040, 20040                          01560520
10040      IVPASS = IVPASS + 1                                          01570520
           WRITE (I02,80002) IVTNUM                                     01580520
           GO TO 0051                                                   01590520
20040      IVFAIL = IVFAIL + 1                                          01600520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01610520
 0051      CONTINUE                                                     01620520
C                                                                       01630520
CT005*  TEST 005   ****  FCVS PROGRAM 520  ****                         01640520
C                                                                       01650520
C                                                                       01660520
           IVTNUM =   5                                                 01670520
        IVCOMP=IPN001*IPN002                                            01680520
           IVCORR=50                                                    01690520
40050      IF (IVCOMP - 50) 20050, 10050, 20050                         01700520
10050      IVPASS = IVPASS + 1                                          01710520
           WRITE (I02,80002) IVTNUM                                     01720520
           GO TO 0061                                                   01730520
20050      IVFAIL = IVFAIL + 1                                          01740520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01750520
 0061      CONTINUE                                                     01760520
C                                                                       01770520
CT006*  TEST 006   ****  FCVS PROGRAM 520  ****                         01780520
C                                                                       01790520
C                                                                       01800520
           IVTNUM =   6                                                 01810520
        IVCOMP=IPN001/IPN002                                            01820520
           IVCORR=2                                                     01830520
40060      IF (IVCOMP - 2) 20060, 10060, 20060                          01840520
10060      IVPASS = IVPASS + 1                                          01850520
           WRITE (I02,80002) IVTNUM                                     01860520
           GO TO 0071                                                   01870520
20060      IVFAIL = IVFAIL + 1                                          01880520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01890520
 0071      CONTINUE                                                     01900520
C                                                                       01910520
CT007*  TEST 007   ****  FCVS PROGRAM 520  ****                         01920520
C                                                                       01930520
C                                                                       01940520
           IVTNUM =   7                                                 01950520
        IVCOMP=IPN001**IPN002                                           01960520
           IVCORR=100000                                                01970520
40070      IF (IVCOMP - 100000) 20070, 10070, 20070                     01980520
10070      IVPASS = IVPASS + 1                                          01990520
           WRITE (I02,80002) IVTNUM                                     02000520
           GO TO 0081                                                   02010520
20070      IVFAIL = IVFAIL + 1                                          02020520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02030520
 0081      CONTINUE                                                     02040520
C                                                                       02050520
C                                                                       02060520
C     TEST 8 - 14 TEST REAL ARITHMETIC EXPRESSION USING                 02070520
C                 ONLY SYMBOLIC NAMES OF ARITHMETIC CONSTANTS           02080520
C                 S06AF-2P 4.A                                          02090520
C                                                                       02100520
CT008*  TEST 008   ****  FCVS PROGRAM 520  ****                         02110520
C                                                                       02120520
C                                                                       02130520
           IVTNUM =   8                                                 02140520
        RVCOMP=+RPN001                                                  02150520
           RVCORR=+10.0                                                 02160520
           IF (RVCOMP - 0.99995E+01) 20080, 10080, 40080                02170520
40080      IF (RVCOMP - 0.10001E+02) 10080, 10080, 20080                02180520
10080      IVPASS = IVPASS + 1                                          02190520
           WRITE (I02,80002) IVTNUM                                     02200520
           GO TO 0091                                                   02210520
20080      IVFAIL = IVFAIL + 1                                          02220520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02230520
 0091      CONTINUE                                                     02240520
C                                                                       02250520
CT009*  TEST 009   ****  FCVS PROGRAM 520  ****                         02260520
C                                                                       02270520
C                                                                       02280520
           IVTNUM =   9                                                 02290520
        RVCOMP=-RPN001                                                  02300520
           RVCORR=-10.0                                                 02310520
           IF (RVCOMP + 0.10001E+02) 20090, 10090, 40090                02320520
40090      IF (RVCOMP + 0.99995E+01) 10090, 10090, 20090                02330520
10090      IVPASS = IVPASS + 1                                          02340520
           WRITE (I02,80002) IVTNUM                                     02350520
           GO TO 0101                                                   02360520
20090      IVFAIL = IVFAIL + 1                                          02370520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02380520
 0101      CONTINUE                                                     02390520
C                                                                       02400520
CT010*  TEST 010   ****  FCVS PROGRAM 520  ****                         02410520
C                                                                       02420520
           IVTNUM =  10                                                 02430520
        RVCOMP=RPN001+RPN002                                            02440520
           RVCORR=15.0                                                  02450520
           IF (RVCOMP - 0.14999E+02) 20100, 10100, 40100                02460520
40100      IF (RVCOMP - 0.15001E+02) 10100, 10100, 20100                02470520
10100      IVPASS = IVPASS + 1                                          02480520
           WRITE (I02,80002) IVTNUM                                     02490520
           GO TO 0111                                                   02500520
20100      IVFAIL = IVFAIL + 1                                          02510520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02520520
 0111      CONTINUE                                                     02530520
C                                                                       02540520
CT011*  TEST 011   ****  FCVS PROGRAM 520  ****                         02550520
C                                                                       02560520
C                                                                       02570520
           IVTNUM =  11                                                 02580520
        RVCOMP=RPN001-RPN002                                            02590520
           RVCORR=5.0                                                   02600520
           IF (RVCOMP - 0.49997E+01) 20110, 10110, 40110                02610520
40110      IF (RVCOMP - 0.50003E+01) 10110, 10110, 20110                02620520
10110      IVPASS = IVPASS + 1                                          02630520
           WRITE (I02,80002) IVTNUM                                     02640520
           GO TO 0121                                                   02650520
20110      IVFAIL = IVFAIL + 1                                          02660520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02670520
 0121      CONTINUE                                                     02680520
C                                                                       02690520
CT012*  TEST 012   ****  FCVS PROGRAM 520  ****                         02700520
C                                                                       02710520
C                                                                       02720520
           IVTNUM =  12                                                 02730520
        RVCOMP=RPN001*RPN002                                            02740520
           RVCORR=50.0                                                  02750520
           IF (RVCOMP - 0.49997E+02) 20120, 10120, 40120                02760520
40120      IF (RVCOMP - 0.50003E+02) 10120, 10120, 20120                02770520
10120      IVPASS = IVPASS + 1                                          02780520
           WRITE (I02,80002) IVTNUM                                     02790520
           GO TO 0131                                                   02800520
20120      IVFAIL = IVFAIL + 1                                          02810520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02820520
 0131      CONTINUE                                                     02830520
C                                                                       02840520
CT013*  TEST 013   ****  FCVS PROGRAM 520  ****                         02850520
C                                                                       02860520
           IVTNUM =  13                                                 02870520
        RVCOMP=RPN001/RPN002                                            02880520
           RVCORR=2.0                                                   02890520
           IF (RVCOMP - 0.19999E+01) 20130, 10130, 40130                02900520
40130      IF (RVCOMP - 0.20001E+01) 10130, 10130, 20130                02910520
10130      IVPASS = IVPASS + 1                                          02920520
           WRITE (I02,80002) IVTNUM                                     02930520
           GO TO 0141                                                   02940520
20130      IVFAIL = IVFAIL + 1                                          02950520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02960520
 0141      CONTINUE                                                     02970520
C                                                                       02980520
CT014*  TEST 014   ****  FCVS PROGRAM 520  ****                         02990520
C                                                                       03000520
           IVTNUM =  14                                                 03010520
         RVCOMP=RPN001**RPN002                                          03020520
           RVCORR=100000.0                                              03030520
           IF (RVCOMP - 0.99995E+05) 20140, 10140, 40140                03040520
40140      IF (RVCOMP - 0.10001E+06) 10140, 10140, 20140                03050520
10140      IVPASS = IVPASS + 1                                          03060520
           WRITE (I02,80002) IVTNUM                                     03070520
           GO TO 0151                                                   03080520
20140      IVFAIL = IVFAIL + 1                                          03090520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     03100520
 0151      CONTINUE                                                     03110520
C                                                                       03120520
C                                                                       03130520
C     TEST 15 - 18 REPEATS TEST 1 - 7 USING MORE THAN ONE OPERATOR      03140520
C                  S06AF-2P 4.C                                         03150520
C                                                                       03160520
CT015*  TEST 015   ****  FCVS PROGRAM 520  ****                         03170520
C                                                                       03180520
C                                                                       03190520
           IVTNUM =  15                                                 03200520
        IVCOMP=IPN001+IPN001-IPN002                                     03210520
           IVCORR=15                                                    03220520
40150      IF (IVCOMP - 15) 20150, 10150, 20150                         03230520
10150      IVPASS = IVPASS + 1                                          03240520
           WRITE (I02,80002) IVTNUM                                     03250520
           GO TO 0161                                                   03260520
20150      IVFAIL = IVFAIL + 1                                          03270520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03280520
 0161      CONTINUE                                                     03290520
C                                                                       03300520
CT016*  TEST 016   ****  FCVS PROGRAM 520  ****                         03310520
C                                                                       03320520
           IVTNUM =  16                                                 03330520
        IVCOMP=IPN001+IPN001-IPN002*IPN002                              03340520
           IVCORR=-5                                                    03350520
40160      IF (IVCOMP + 5) 20160, 10160, 20160                          03360520
10160      IVPASS = IVPASS + 1                                          03370520
           WRITE (I02,80002) IVTNUM                                     03380520
           GO TO 0171                                                   03390520
20160      IVFAIL = IVFAIL + 1                                          03400520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03410520
 0171      CONTINUE                                                     03420520
C                                                                       03430520
CT017*  TEST 017   ****  FCVS PROGRAM 520  ****                         03440520
C                                                                       03450520
C                                                                       03460520
           IVTNUM =  17                                                 03470520
        IVCOMP=IPN001+IPN001-IPN002*IPN002/IPN003                       03480520
           IVCORR=15                                                    03490520
40170      IF (IVCOMP - 15) 20170, 10170, 20170                         03500520
10170      IVPASS = IVPASS + 1                                          03510520
           WRITE (I02,80002) IVTNUM                                     03520520
           GO TO 0181                                                   03530520
20170      IVFAIL = IVFAIL + 1                                          03540520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03550520
 0181      CONTINUE                                                     03560520
C                                                                       03570520
CT018*  TEST 018   ****  FCVS PROGRAM 520  ****                         03580520
C                                                                       03590520
           IVTNUM =  18                                                 03600520
        IVCOMP=IPN001+IPN001**IPN002-IPN002*IPN002/IPN003               03610520
           IVCORR=100005                                                03620520
40180      IF (IVCOMP - 100005) 20180, 10180, 20180                     03630520
10180      IVPASS = IVPASS + 1                                          03640520
           WRITE (I02,80002) IVTNUM                                     03650520
           GO TO 0191                                                   03660520
20180      IVFAIL = IVFAIL + 1                                          03670520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03680520
 0191      CONTINUE                                                     03690520
C                                                                       03700520
C                                                                       03710520
C     TEST 19 - 22 REPEATS TEST 8 - 14 USING MORE THAN ONE OPERATOR     03720520
C                  S06AF-2P 4.C                                         03730520
C                                                                       03740520
CT019*  TEST 019   ****  FCVS PROGRAM 520  ****                         03750520
C                                                                       03760520
           IVTNUM =  19                                                 03770520
        RVCOMP=RPN001+RPN001-RPN002                                     03780520
           RVCORR=15.0                                                  03790520
           IF (RVCOMP - 0.14999E+02) 20190, 10190, 40190                03800520
40190      IF (RVCOMP - 0.15001E+02) 10190, 10190, 20190                03810520
10190      IVPASS = IVPASS + 1                                          03820520
           WRITE (I02,80002) IVTNUM                                     03830520
           GO TO 0201                                                   03840520
20190      IVFAIL = IVFAIL + 1                                          03850520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     03860520
 0201      CONTINUE                                                     03870520
C                                                                       03880520
CT020*  TEST 020   ****  FCVS PROGRAM 520  ****                         03890520
C                                                                       03900520
           IVTNUM =  20                                                 03910520
        RVCOMP=RPN001+RPN001-RPN002*RPN002                              03920520
           RVCORR=-5.0                                                  03930520
           IF (RVCOMP + 0.50003E+01) 20200, 10200, 40200                03940520
40200      IF (RVCOMP + 0.49997E+01) 10200, 10200, 20200                03950520
10200      IVPASS = IVPASS + 1                                          03960520
           WRITE (I02,80002) IVTNUM                                     03970520
           GO TO 0211                                                   03980520
20200      IVFAIL = IVFAIL + 1                                          03990520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     04000520
 0211      CONTINUE                                                     04010520
C                                                                       04020520
CT021*  TEST 021   ****  FCVS PROGRAM 520  ****                         04030520
C                                                                       04040520
           IVTNUM =  21                                                 04050520
        RVCOMP=RPN001+RPN001-RPN002*RPN002/RPN003                       04060520
           RVCORR=15.0                                                  04070520
           IF (RVCOMP - 0.14999E+02) 20210, 10210, 40210                04080520
40210      IF (RVCOMP - 0.15001E+02) 10210, 10210, 20210                04090520
10210      IVPASS = IVPASS + 1                                          04100520
           WRITE (I02,80002) IVTNUM                                     04110520
           GO TO 0221                                                   04120520
20210      IVFAIL = IVFAIL + 1                                          04130520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     04140520
 0221      CONTINUE                                                     04150520
C                                                                       04160520
CT022*  TEST 022   ****  FCVS PROGRAM 520  ****                         04170520
C                                                                       04180520
           IVTNUM =  22                                                 04190520
        RVCOMP=RPN001+RPN001**RPN002-RPN002*RPN002/RPN003               04200520
           RVCORR=100005.0                                              04210520
           IF (RVCOMP - 0.10000E+06) 20220, 10220, 40220                04220520
40220      IF (RVCOMP - 0.10001E+06) 10220, 10220, 20220                04230520
10220      IVPASS = IVPASS + 1                                          04240520
           WRITE (I02,80002) IVTNUM                                     04250520
           GO TO 0231                                                   04260520
20220      IVFAIL = IVFAIL + 1                                          04270520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     04280520
 0231      CONTINUE                                                     04290520
C                                                                       04300520
C                                                                       04310520
C     TEST 23 - 26 REPEATS TEST 15 - 18 USING PARENTHESES               04320520
C                  S06AF-2P 4.D                                         04330520
C                                                                       04340520
C                                                                       04350520
C                                                                       04360520
CT023*  TEST 023   ****  FCVS PROGRAM 520  ****                         04370520
C                                                                       04380520
           IVTNUM =  23                                                 04390520
        IVCOMP=IPN001+(IPN001-IPN002)                                   04400520
           IVCORR=15                                                    04410520
40230      IF (IVCOMP - 15) 20230, 10230, 20230                         04420520
10230      IVPASS = IVPASS + 1                                          04430520
           WRITE (I02,80002) IVTNUM                                     04440520
           GO TO 0241                                                   04450520
20230      IVFAIL = IVFAIL + 1                                          04460520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04470520
 0241      CONTINUE                                                     04480520
C                                                                       04490520
CT024*  TEST 024   ****  FCVS PROGRAM 520  ****                         04500520
C                                                                       04510520
           IVTNUM =  24                                                 04520520
        IVCOMP=((IPN001+IPN001)-IPN002)*IPN002                          04530520
           IVCORR=75                                                    04540520
40240      IF (IVCOMP - 75) 20240, 10240, 20240                         04550520
10240      IVPASS = IVPASS + 1                                          04560520
           WRITE (I02,80002) IVTNUM                                     04570520
           GO TO 0251                                                   04580520
20240      IVFAIL = IVFAIL + 1                                          04590520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04600520
 0251      CONTINUE                                                     04610520
C                                                                       04620520
CT025*  TEST 025   ****  FCVS PROGRAM 520  ****                         04630520
C                                                                       04640520
           IVTNUM =  25                                                 04650520
        IVCOMP=(IPN001+IPN001)-IPN002*(IPN002/IPN003)                   04660520
           IVCORR=15                                                    04670520
40250      IF (IVCOMP - 15) 20250, 10250, 20250                         04680520
10250      IVPASS = IVPASS + 1                                          04690520
           WRITE (I02,80002) IVTNUM                                     04700520
           GO TO 0261                                                   04710520
20250      IVFAIL = IVFAIL + 1                                          04720520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04730520
 0261      CONTINUE                                                     04740520
C                                                                       04750520
CT026*  TEST 026   ****  FCVS PROGRAM 520  ****                         04760520
C                                                                       04770520
           IVTNUM =  26                                                 04780520
        IVCOMP=(IPN001+IPN001)**2-((IPN002*IPN002)/IPN003)              04790520
           IVCORR=395                                                   04800520
40260      IF (IVCOMP - 395) 20260, 10260, 20260                        04810520
10260      IVPASS = IVPASS + 1                                          04820520
           WRITE (I02,80002) IVTNUM                                     04830520
           GO TO 0271                                                   04840520
20260      IVFAIL = IVFAIL + 1                                          04850520
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04860520
 0271      CONTINUE                                                     04870520
C                                                                       04880520
C     TEST 27 - 30 REPEATS TEST 19 - 22 USING PARENTHESES               04890520
C                  S06AF-2P 4.D                                         04900520
C                                                                       04910520
CT027*  TEST 027   ****  FCVS PROGRAM 520  ****                         04920520
C                                                                       04930520
           IVTNUM =  27                                                 04940520
        RVCOMP=RPN001+(RPN001-RPN002)                                   04950520
           RVCORR=15.0                                                  04960520
           IF (RVCOMP - 0.14999E+02) 20270, 10270, 40270                04970520
40270      IF (RVCOMP - 0.15001E+02) 10270, 10270, 20270                04980520
10270      IVPASS = IVPASS + 1                                          04990520
           WRITE (I02,80002) IVTNUM                                     05000520
           GO TO 0281                                                   05010520
20270      IVFAIL = IVFAIL + 1                                          05020520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     05030520
 0281      CONTINUE                                                     05040520
C                                                                       05050520
CT028*  TEST 028   ****  FCVS PROGRAM 520  ****                         05060520
C                                                                       05070520
           IVTNUM =  28                                                 05080520
        RVCOMP=((RPN001+RPN001)-RPN002)*RPN002                          05090520
           RVCORR=75.0                                                  05100520
           IF (RVCOMP - 0.74996E+02) 20280, 10280, 40280                05110520
40280      IF (RVCOMP - 0.75004E+02) 10280, 10280, 20280                05120520
10280      IVPASS = IVPASS + 1                                          05130520
           WRITE (I02,80002) IVTNUM                                     05140520
           GO TO 0291                                                   05150520
20280      IVFAIL = IVFAIL + 1                                          05160520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     05170520
 0291      CONTINUE                                                     05180520
C                                                                       05190520
CT029*  TEST 029   ****  FCVS PROGRAM 520  ****                         05200520
C                                                                       05210520
           IVTNUM =  29                                                 05220520
        RVCOMP=(RPN001+RPN001)-RPN002*(RPN002/RPN003)                   05230520
           RVCORR=15.0                                                  05240520
           IF (RVCOMP - 0.14999E+02) 20290, 10290, 40290                05250520
40290      IF (RVCOMP - 0.15001E+02) 10290, 10290, 20290                05260520
10290      IVPASS = IVPASS + 1                                          05270520
           WRITE (I02,80002) IVTNUM                                     05280520
           GO TO 0301                                                   05290520
20290      IVFAIL = IVFAIL + 1                                          05300520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     05310520
 0301      CONTINUE                                                     05320520
C                                                                       05330520
CT030*  TEST 030   ****  FCVS PROGRAM 520  ****                         05340520
C                                                                       05350520
C                                                                       05360520
           IVTNUM =  30                                                 05370520
        RVCOMP=(RPN001+RPN001)**3.0-((RPN002*RPN002)/RPN003)            05380520
           RVCORR=7995.0                                                05390520
           IF (RVCOMP - 0.79946E+04) 20300, 10300, 40300                05400520
40300      IF (RVCOMP - 0.79954E+04) 10300, 10300, 20300                05410520
10300      IVPASS = IVPASS + 1                                          05420520
           WRITE (I02,80002) IVTNUM                                     05430520
           GO TO 0311                                                   05440520
20300      IVFAIL = IVFAIL + 1                                          05450520
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     05460520
 0311      CONTINUE                                                     05470520
C                                                                       05480520
CBB** ********************** BBCSUM0  **********************************05490520
C**** WRITE OUT TEST SUMMARY                                            05500520
C****                                                                   05510520
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        05520520
      WRITE (I02, 90004)                                                05530520
      WRITE (I02, 90014)                                                05540520
      WRITE (I02, 90004)                                                05550520
      WRITE (I02, 90020) IVPASS                                         05560520
      WRITE (I02, 90022) IVFAIL                                         05570520
      WRITE (I02, 90024) IVDELE                                         05580520
      WRITE (I02, 90026) IVINSP                                         05590520
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 05600520
CBE** ********************** BBCSUM0  **********************************05610520
CBB** ********************** BBCFOOT0 **********************************05620520
C**** WRITE OUT REPORT FOOTINGS                                         05630520
C****                                                                   05640520
      WRITE (I02,90016) ZPROG, ZPROG                                    05650520
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     05660520
      WRITE (I02,90019)                                                 05670520
CBE** ********************** BBCFOOT0 **********************************05680520
90001 FORMAT (1H ,56X,5HFM520)                                          05690520
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM520)                          05700520
CBB** ********************** BBCFMT0A **********************************05710520
C**** FORMATS FOR TEST DETAIL LINES                                     05720520
C****                                                                   05730520
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           05740520
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           05750520
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           05760520
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           05770520
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           05780520
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    05790520
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05800520
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              05810520
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05820520
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  05830520
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         05840520
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         05850520
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         05860520
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         05870520
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      05880520
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      05890520
80050 FORMAT (1H ,48X,A31)                                              05900520
CBE** ********************** BBCFMT0A **********************************05910520
CBB** ********************** BBCFMAT1 **********************************05920520
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     05930520
C****                                                                   05940520
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05950520
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            05960520
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     05970520
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     05980520
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05990520
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06000520
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06010520
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06020520
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06030520
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  06040520
     21H(,F12.5,2H, ,F12.5,1H))                                         06050520
CBE** ********************** BBCFMAT1 **********************************06060520
CBB** ********************** BBCFMT0B **********************************06070520
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                06080520
C****                                                                   06090520
90002 FORMAT (1H1)                                                      06100520
90004 FORMAT (1H )                                                      06110520
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               06120520
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06130520
90008 FORMAT (1H ,21X,A13,A17)                                          06140520
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       06150520
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    06160520
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     06170520
     1       7X,7HREMARKS,24X)                                          06180520
90014 FORMAT (1H ,46H----------------------------------------------,    06190520
     1        33H---------------------------------)                     06200520
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               06210520
C****                                                                   06220520
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             06230520
C****                                                                   06240520
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          06250520
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        06260520
     1        A13)                                                      06270520
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 06280520
C****                                                                   06290520
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 06300520
C****                                                                   06310520
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              06320520
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              06330520
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             06340520
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  06350520
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  06360520
CBE** ********************** BBCFMT0B **********************************06370520
           END                                                          06380520
