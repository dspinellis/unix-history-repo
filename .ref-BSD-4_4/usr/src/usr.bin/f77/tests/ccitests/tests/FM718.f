      PROGRAM FM718                                                     00010718
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020718
C     THIS ROUTINE TESTS LOGICAL EXPRESSIONS AND            ANS REF.    00030718
C     USE OF THE LOGICAL OPERATORS .NOT., .AND., .OR.,      6.4, 6.4.2, 00040718
C     .EQV., AND .NEQV.                                     6.4.3, 6.4.400050718
C                                                                       00060718
CBB** ********************** BBCCOMNT **********************************00070718
C****                                                                   00080718
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00090718
C****                          VERSION 2.0                              00100718
C****                                                                   00110718
C****                                                                   00120718
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00130718
C****                   GENERAL SERVICES ADMINISTRATION                 00140718
C****                   FEDERAL SOFTWARE TESTING CENTER                 00150718
C****                   5203 LEESBURG PIKE, SUITE 1100                  00160718
C****                      FALLS CHURCH, VA. 22041                      00170718
C****                                                                   00180718
C****                          (703) 756-6153                           00190718
C****                                                                   00200718
CBE** ********************** BBCCOMNT **********************************00210718
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00220718
           IMPLICIT CHARACTER*27 (C)                                    00230718
CBB** ********************** BBCINITA **********************************00240718
C**** SPECIFICATION STATEMENTS                                          00250718
C****                                                                   00260718
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00270718
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00280718
CBE** ********************** BBCINITA **********************************00290718
C                                                                       00300718
      LOGICAL LPN001, LPN002, LPN003, LPN004                            00310718
      LOGICAL LVCOMP, LVCORR, LVN001                                    00320718
      PARAMETER (LPN001 = .TRUE., LPN002 = .FALSE.,                     00330718
     1           LPN003 = .TRUE., LPN004 = .FALSE.)                     00340718
C                                                                       00350718
C                                                                       00360718
CBB** ********************** BBCINITB **********************************00370718
C**** INITIALIZE SECTION                                                00380718
      DATA  ZVERS,                  ZVERSD,             ZDATE           00390718
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00400718
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00410718
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00420718
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00430718
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00440718
      DATA   REMRKS /'                               '/                 00450718
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00460718
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00470718
C****                                                                   00480718
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00490718
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00500718
CZ03  ZPROG  = 'PROGRAM NAME'                                           00510718
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00580718
      IVPASS = 0                                                        00590718
      IVFAIL = 0                                                        00600718
      IVDELE = 0                                                        00610718
      IVINSP = 0                                                        00620718
      IVTOTL = 0                                                        00630718
      IVTOTN = 0                                                        00640718
      ICZERO = 0                                                        00650718
C                                                                       00660718
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00670718
      I01 = 05                                                          00680718
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00690718
      I02 = 06                                                          00700718
C                                                                       00710718
      I01 = 5                                                           00720718
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00730718
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00740718
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00750718
C                                                                       00760718
      I02 = 6                                                           00770718
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00780718
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00790718
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00800718
C                                                                       00810718
CBE** ********************** BBCINITB **********************************00820718
           ZPROG='FM718'                                                00830718
           IVTOTL =  29                                                 00840718
CBB** ********************** BBCHED0A **********************************00850718
C****                                                                   00860718
C**** WRITE REPORT TITLE                                                00870718
C****                                                                   00880718
      WRITE (I02, 90002)                                                00890718
      WRITE (I02, 90006)                                                00900718
      WRITE (I02, 90007)                                                00910718
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920718
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930718
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940718
CBE** ********************** BBCHED0A **********************************00950718
CBB** ********************** BBCHED0B **********************************00960718
C**** WRITE DETAIL REPORT HEADERS                                       00970718
C****                                                                   00980718
      WRITE (I02,90004)                                                 00990718
      WRITE (I02,90004)                                                 01000718
      WRITE (I02,90013)                                                 01010718
      WRITE (I02,90014)                                                 01020718
      WRITE (I02,90015) IVTOTL                                          01030718
CBE** ********************** BBCHED0B **********************************01040718
C                                                                       01050718
CT001*  TEST 001   ****  FCVS PROGRAM 718  ****                         01060718
C                                                                       01070718
C     LOGICAL EXPRESSION CONTAINING SYMBOLIC NAME OF A LOGICAL CONSTANT 01080718
C                                                                       01090718
           IVTNUM =   1                                                 01100718
           LVCORR = .TRUE.                                              01110718
      LVCOMP = LPN001                                                   01120718
           IVCOMP = 0                                                   01130718
           IF (LVCOMP) IVCOMP = 1                                       01140718
           IF (IVCOMP - 1) 20010, 10010, 20010                          01150718
10010      IVPASS = IVPASS + 1                                          01160718
           WRITE (I02,80002) IVTNUM                                     01170718
           GO TO 0011                                                   01180718
20010      IVFAIL = IVFAIL + 1                                          01190718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01200718
 0011      CONTINUE                                                     01210718
C                                                                       01220718
C     TESTS 2-3 - TEST LOGICAL EXPRESSIONS INVOLVING .NOT.              01230718
C                                                                       01240718
C                                                                       01250718
CT002*  TEST 002   ****  FCVS PROGRAM 718  ****                         01260718
C                                                                       01270718
C                                                                       01280718
           IVTNUM =   2                                                 01290718
           LVCORR = .TRUE.                                              01300718
      LVCOMP = .NOT..FALSE.                                             01310718
           IVCOMP = 0                                                   01320718
           IF (LVCOMP) IVCOMP = 1                                       01330718
           IF (IVCOMP - 1) 20020, 10020, 20020                          01340718
10020      IVPASS = IVPASS + 1                                          01350718
           WRITE (I02,80002) IVTNUM                                     01360718
           GO TO 0021                                                   01370718
20020      IVFAIL = IVFAIL + 1                                          01380718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01390718
 0021      CONTINUE                                                     01400718
C                                                                       01410718
CT003*  TEST 003   ****  FCVS PROGRAM 718  ****                         01420718
C                                                                       01430718
C                                                                       01440718
           IVTNUM =   3                                                 01450718
           IVCORR = 1                                                   01460718
      IVCOMP = 0                                                        01470718
      IF (.NOT. LPN002) IVCOMP = 1                                      01480718
40030      IF (IVCOMP - 1) 20030, 10030, 20030                          01490718
10030      IVPASS = IVPASS + 1                                          01500718
           WRITE (I02,80002) IVTNUM                                     01510718
           GO TO 0031                                                   01520718
20030      IVFAIL = IVFAIL + 1                                          01530718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01540718
 0031      CONTINUE                                                     01550718
C                                                                       01560718
C     TESTS 4-5 - TEST LOGICAL EXPRESSIONS INVOLVING .AND.              01570718
C                                                                       01580718
C                                                                       01590718
CT004*  TEST 004   ****  FCVS PROGRAM 718  ****                         01600718
C                                                                       01610718
C                                                                       01620718
           IVTNUM =   4                                                 01630718
           LVCORR = .TRUE.                                              01640718
      LVCOMP = .TRUE..AND.LPN003                                        01650718
           IVCOMP = 0                                                   01660718
           IF (LVCOMP) IVCOMP = 1                                       01670718
           IF (IVCOMP - 1) 20040, 10040, 20040                          01680718
10040      IVPASS = IVPASS + 1                                          01690718
           WRITE (I02,80002) IVTNUM                                     01700718
           GO TO 0041                                                   01710718
20040      IVFAIL = IVFAIL + 1                                          01720718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01730718
 0041      CONTINUE                                                     01740718
C                                                                       01750718
CT005*  TEST 005   ****  FCVS PROGRAM 718  ****                         01760718
C                                                                       01770718
C                                                                       01780718
           IVTNUM =   5                                                 01790718
           IVCORR = 1                                                   01800718
      IVCOMP = 0                                                        01810718
      IF (LPN003.AND..TRUE.) IVCOMP = 1                                 01820718
40050      IF (IVCOMP - 1) 20050, 10050, 20050                          01830718
10050      IVPASS = IVPASS + 1                                          01840718
           WRITE (I02,80002) IVTNUM                                     01850718
           GO TO 0051                                                   01860718
20050      IVFAIL = IVFAIL + 1                                          01870718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01880718
 0051      CONTINUE                                                     01890718
C                                                                       01900718
C     TESTS 6-7 - TEST LOGICAL EXPRESSIONS INVOLVING .OR.               01910718
C                                                                       01920718
C                                                                       01930718
CT006*  TEST 006   ****  FCVS PROGRAM 718  ****                         01940718
C                                                                       01950718
C                                                                       01960718
           IVTNUM =   6                                                 01970718
           LVCORR = .TRUE.                                              01980718
      LVCOMP = .TRUE..OR.LPN004                                         01990718
           IVCOMP = 0                                                   02000718
           IF (LVCOMP) IVCOMP = 1                                       02010718
           IF (IVCOMP - 1) 20060, 10060, 20060                          02020718
10060      IVPASS = IVPASS + 1                                          02030718
           WRITE (I02,80002) IVTNUM                                     02040718
           GO TO 0061                                                   02050718
20060      IVFAIL = IVFAIL + 1                                          02060718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02070718
 0061      CONTINUE                                                     02080718
C                                                                       02090718
CT007*  TEST 007   ****  FCVS PROGRAM 718  ****                         02100718
C                                                                       02110718
C                                                                       02120718
           IVTNUM =   7                                                 02130718
           IVCORR = 1                                                   02140718
      IVCOMP = 0                                                        02150718
      IF (LPN001.OR..FALSE.) IVCOMP = 1                                 02160718
40070      IF (IVCOMP - 1) 20070, 10070, 20070                          02170718
10070      IVPASS = IVPASS + 1                                          02180718
           WRITE (I02,80002) IVTNUM                                     02190718
           GO TO 0071                                                   02200718
20070      IVFAIL = IVFAIL + 1                                          02210718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02220718
 0071      CONTINUE                                                     02230718
C                                                                       02240718
C     TESTS 8-9 - TEST LOGICAL EXPRESSIONS INVOLVING .EQV.              02250718
C                                                                       02260718
C                                                                       02270718
CT008*  TEST 008   ****  FCVS PROGRAM 718  ****                         02280718
C                                                                       02290718
C                                                                       02300718
           IVTNUM =   8                                                 02310718
           LVCORR = .TRUE.                                              02320718
      LVCOMP = .FALSE..EQV.LPN002                                       02330718
           IVCOMP = 0                                                   02340718
           IF (LVCOMP) IVCOMP = 1                                       02350718
           IF (IVCOMP - 1) 20080, 10080, 20080                          02360718
10080      IVPASS = IVPASS + 1                                          02370718
           WRITE (I02,80002) IVTNUM                                     02380718
           GO TO 0081                                                   02390718
20080      IVFAIL = IVFAIL + 1                                          02400718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02410718
 0081      CONTINUE                                                     02420718
C                                                                       02430718
CT009*  TEST 009   ****  FCVS PROGRAM 718  ****                         02440718
C                                                                       02450718
C                                                                       02460718
           IVTNUM =   9                                                 02470718
           IVCORR = 1                                                   02480718
      IVCOMP = 0                                                        02490718
      IF (LPN003.EQV..TRUE.) IVCOMP = 1                                 02500718
40090      IF (IVCOMP - 1) 20090, 10090, 20090                          02510718
10090      IVPASS = IVPASS + 1                                          02520718
           WRITE (I02,80002) IVTNUM                                     02530718
           GO TO 0091                                                   02540718
20090      IVFAIL = IVFAIL + 1                                          02550718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02560718
 0091      CONTINUE                                                     02570718
C                                                                       02580718
C     TESTS 10-11 - TEST LOGICAL EXPRESSIONS INVOLVING .NEQV.           02590718
C                                                                       02600718
C                                                                       02610718
CT010*  TEST 010   ****  FCVS PROGRAM 718  ****                         02620718
C                                                                       02630718
C                                                                       02640718
           IVTNUM =  10                                                 02650718
           LVCORR = .TRUE.                                              02660718
      LVCOMP = .FALSE..NEQV.LPN001                                      02670718
           IVCOMP = 0                                                   02680718
           IF (LVCOMP) IVCOMP = 1                                       02690718
           IF (IVCOMP - 1) 20100, 10100, 20100                          02700718
10100      IVPASS = IVPASS + 1                                          02710718
           WRITE (I02,80002) IVTNUM                                     02720718
           GO TO 0101                                                   02730718
20100      IVFAIL = IVFAIL + 1                                          02740718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02750718
 0101      CONTINUE                                                     02760718
C                                                                       02770718
CT011*  TEST 011   ****  FCVS PROGRAM 718  ****                         02780718
C                                                                       02790718
C                                                                       02800718
           IVTNUM =  11                                                 02810718
           IVCORR = 1                                                   02820718
      IVCOMP = 0                                                        02830718
      IF (LPN003.NEQV..FALSE.) IVCOMP = 1                               02840718
40110      IF (IVCOMP - 1) 20110, 10110, 20110                          02850718
10110      IVPASS = IVPASS + 1                                          02860718
           WRITE (I02,80002) IVTNUM                                     02870718
           GO TO 0111                                                   02880718
20110      IVFAIL = IVFAIL + 1                                          02890718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02900718
 0111      CONTINUE                                                     02910718
C                                                                       02920718
C     TESTS 12-17 - TEST LOGICAL EXPRESSIONS INVOLVING VARIOUS COMBINA- 02930718
C     TIONS OF LOGICAL OPERATORS AND ALSO TEST PRECEDENCE AMONG THE     02940718
C     LOGICAL OPERATORS WITH OR WITHOUT PARENTHESES                     02950718
C                                                                       02960718
C                                                                       02970718
CT012*  TEST 012   ****  FCVS PROGRAM 718  ****                         02980718
C                                                                       02990718
C                                                                       03000718
           IVTNUM =  12                                                 03010718
           LVCORR = .TRUE.                                              03020718
      LVN001 = .TRUE.                                                   03030718
      LVCOMP = LVN001.EQV.LPN002.AND..TRUE..NEQV.LPN003                 03040718
           IVCOMP = 0                                                   03050718
           IF (LVCOMP) IVCOMP = 1                                       03060718
           IF (IVCOMP - 1) 20120, 10120, 20120                          03070718
10120      IVPASS = IVPASS + 1                                          03080718
           WRITE (I02,80002) IVTNUM                                     03090718
           GO TO 0121                                                   03100718
20120      IVFAIL = IVFAIL + 1                                          03110718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03120718
 0121      CONTINUE                                                     03130718
C                                                                       03140718
CT013*  TEST 013   ****  FCVS PROGRAM 718  ****                         03150718
C                                                                       03160718
C                                                                       03170718
           IVTNUM =  13                                                 03180718
           LVCORR = .FALSE.                                             03190718
      LVCOMP = (.TRUE..EQV..FALSE.).AND.(LVN001.NEQV.LPN003)            03200718
           IVCOMP = 0                                                   03210718
           IF (LVCOMP) IVCOMP = 1                                       03220718
           IF (IVCOMP - 0) 20130, 10130, 20130                          03230718
10130      IVPASS = IVPASS + 1                                          03240718
           WRITE (I02,80002) IVTNUM                                     03250718
           GO TO 0131                                                   03260718
20130      IVFAIL = IVFAIL + 1                                          03270718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03280718
 0131      CONTINUE                                                     03290718
C                                                                       03300718
CT014*  TEST 014   ****  FCVS PROGRAM 718  ****                         03310718
C                                                                       03320718
C                                                                       03330718
           IVTNUM =  14                                                 03340718
           LVCORR = .TRUE.                                              03350718
      LVN001 = .FALSE.                                                  03360718
      LVCOMP = LVN001.EQV.LPN002.AND..NOT.LPN001.OR..FALSE.             03370718
           IVCOMP = 0                                                   03380718
           IF (LVCOMP) IVCOMP = 1                                       03390718
           IF (IVCOMP - 1) 20140, 10140, 20140                          03400718
10140      IVPASS = IVPASS + 1                                          03410718
           WRITE (I02,80002) IVTNUM                                     03420718
           GO TO 0141                                                   03430718
20140      IVFAIL = IVFAIL + 1                                          03440718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03450718
 0141      CONTINUE                                                     03460718
C                                                                       03470718
CT015*  TEST 015   ****  FCVS PROGRAM 718  ****                         03480718
C                                                                       03490718
C                                                                       03500718
           IVTNUM =  15                                                 03510718
           LVCORR = .FALSE.                                             03520718
      LVCOMP = (LVN001.EQV.LPN002).AND.(.NOT.LPN001.OR..FALSE.)         03530718
           IVCOMP = 0                                                   03540718
           IF (LVCOMP) IVCOMP = 1                                       03550718
           IF (IVCOMP - 0) 20150, 10150, 20150                          03560718
10150      IVPASS = IVPASS + 1                                          03570718
           WRITE (I02,80002) IVTNUM                                     03580718
           GO TO 0151                                                   03590718
20150      IVFAIL = IVFAIL + 1                                          03600718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03610718
 0151      CONTINUE                                                     03620718
C                                                                       03630718
CT016*  TEST 016   ****  FCVS PROGRAM 718  ****                         03640718
C                                                                       03650718
C                                                                       03660718
           IVTNUM =  16                                                 03670718
           LVCORR = .TRUE.                                              03680718
      LVCOMP = LPN001.EQV.LVN001.OR..NOT.LPN003.NEQV..TRUE.             03690718
           IVCOMP = 0                                                   03700718
           IF (LVCOMP) IVCOMP = 1                                       03710718
           IF (IVCOMP - 1) 20160, 10160, 20160                          03720718
10160      IVPASS = IVPASS + 1                                          03730718
           WRITE (I02,80002) IVTNUM                                     03740718
           GO TO 0161                                                   03750718
20160      IVFAIL = IVFAIL + 1                                          03760718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03770718
 0161      CONTINUE                                                     03780718
C                                                                       03790718
CT017*  TEST 017   ****  FCVS PROGRAM 718  ****                         03800718
C                                                                       03810718
C                                                                       03820718
           IVTNUM =  17                                                 03830718
           LVCORR = .TRUE.                                              03840718
      LVCOMP = LPN001.AND.(LVN001.OR..NOT.(LPN002.EQV.(LPN003.NEQV.     03850718
     1         LPN004)))                                                03860718
           IVCOMP = 0                                                   03870718
           IF (LVCOMP) IVCOMP = 1                                       03880718
           IF (IVCOMP - 1) 20170, 10170, 20170                          03890718
10170      IVPASS = IVPASS + 1                                          03900718
           WRITE (I02,80002) IVTNUM                                     03910718
           GO TO 0171                                                   03920718
20170      IVFAIL = IVFAIL + 1                                          03930718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03940718
 0171      CONTINUE                                                     03950718
C                                                                       03960718
C    TESTS 18-21 - TEST LOGICAL EXPRESSIONS INVOLOVING .EQV.            03970718
C                                                                       03980718
C                                                                       03990718
CT018*  TEST 018   ****  FCVS PROGRAM 718  ****                         04000718
C                                                                       04010718
C                                                                       04020718
           IVTNUM =  18                                                 04030718
           LVCORR = .TRUE.                                              04040718
      LVCOMP = LPN001.EQV.LPN003                                        04050718
           IVCOMP = 0                                                   04060718
           IF (LVCOMP) IVCOMP = 1                                       04070718
           IF (IVCOMP - 1) 20180, 10180, 20180                          04080718
10180      IVPASS = IVPASS + 1                                          04090718
           WRITE (I02,80002) IVTNUM                                     04100718
           GO TO 0181                                                   04110718
20180      IVFAIL = IVFAIL + 1                                          04120718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04130718
 0181      CONTINUE                                                     04140718
C                                                                       04150718
CT019*  TEST 019   ****  FCVS PROGRAM 718  ****                         04160718
C                                                                       04170718
C                                                                       04180718
           IVTNUM =  19                                                 04190718
           LVCORR = .FALSE.                                             04200718
      LVCOMP = LPN001.EQV.LPN002                                        04210718
           IVCOMP = 0                                                   04220718
           IF (LVCOMP) IVCOMP = 1                                       04230718
           IF (IVCOMP - 0) 20190, 10190, 20190                          04240718
10190      IVPASS = IVPASS + 1                                          04250718
           WRITE (I02,80002) IVTNUM                                     04260718
           GO TO 0191                                                   04270718
20190      IVFAIL = IVFAIL + 1                                          04280718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04290718
 0191      CONTINUE                                                     04300718
C                                                                       04310718
CT020*  TEST 020   ****  FCVS PROGRAM 718  ****                         04320718
C                                                                       04330718
C                                                                       04340718
           IVTNUM =  20                                                 04350718
           LVCORR = .FALSE.                                             04360718
      LVCOMP = LPN002.EQV.LPN003                                        04370718
           IVCOMP = 0                                                   04380718
           IF (LVCOMP) IVCOMP = 1                                       04390718
           IF (IVCOMP - 0) 20200, 10200, 20200                          04400718
10200      IVPASS = IVPASS + 1                                          04410718
           WRITE (I02,80002) IVTNUM                                     04420718
           GO TO 0201                                                   04430718
20200      IVFAIL = IVFAIL + 1                                          04440718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04450718
 0201      CONTINUE                                                     04460718
C                                                                       04470718
CT021*  TEST 021   ****  FCVS PROGRAM 718  ****                         04480718
C                                                                       04490718
C                                                                       04500718
           IVTNUM =  21                                                 04510718
           LVCORR = .TRUE.                                              04520718
      LVCOMP = LPN002.EQV.LPN004                                        04530718
           IVCOMP = 0                                                   04540718
           IF (LVCOMP) IVCOMP = 1                                       04550718
           IF (IVCOMP - 1) 20210, 10210, 20210                          04560718
10210      IVPASS = IVPASS + 1                                          04570718
           WRITE (I02,80002) IVTNUM                                     04580718
           GO TO 0211                                                   04590718
20210      IVFAIL = IVFAIL + 1                                          04600718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04610718
 0211      CONTINUE                                                     04620718
C                                                                       04630718
C    TESTS 22-25 - TEST LOGICAL EXPRESSIONS INVOLVING .NEQV.            04640718
C                                                                       04650718
C                                                                       04660718
CT022*  TEST 022   ****  FCVS PROGRAM 718  ****                         04670718
C                                                                       04680718
C                                                                       04690718
           IVTNUM =  22                                                 04700718
           LVCORR = .FALSE.                                             04710718
      LVCOMP = LPN001.NEQV.LPN003                                       04720718
           IVCOMP = 0                                                   04730718
           IF (LVCOMP) IVCOMP = 1                                       04740718
           IF (IVCOMP - 0) 20220, 10220, 20220                          04750718
10220      IVPASS = IVPASS + 1                                          04760718
           WRITE (I02,80002) IVTNUM                                     04770718
           GO TO 0221                                                   04780718
20220      IVFAIL = IVFAIL + 1                                          04790718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04800718
 0221      CONTINUE                                                     04810718
C                                                                       04820718
CT023*  TEST 023   ****  FCVS PROGRAM 718  ****                         04830718
C                                                                       04840718
C                                                                       04850718
           IVTNUM =  23                                                 04860718
           LVCORR = .TRUE.                                              04870718
      LVCOMP = LPN001.NEQV.LPN002                                       04880718
           IVCOMP = 0                                                   04890718
           IF (LVCOMP) IVCOMP = 1                                       04900718
           IF (IVCOMP - 1) 20230, 10230, 20230                          04910718
10230      IVPASS = IVPASS + 1                                          04920718
           WRITE (I02,80002) IVTNUM                                     04930718
           GO TO 0231                                                   04940718
20230      IVFAIL = IVFAIL + 1                                          04950718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04960718
 0231      CONTINUE                                                     04970718
C                                                                       04980718
CT024*  TEST 024   ****  FCVS PROGRAM 718  ****                         04990718
C                                                                       05000718
C                                                                       05010718
           IVTNUM =  24                                                 05020718
           LVCORR = .TRUE.                                              05030718
      LVCOMP = LPN002.NEQV.LPN003                                       05040718
           IVCOMP = 0                                                   05050718
           IF (LVCOMP) IVCOMP = 1                                       05060718
           IF (IVCOMP - 1) 20240, 10240, 20240                          05070718
10240      IVPASS = IVPASS + 1                                          05080718
           WRITE (I02,80002) IVTNUM                                     05090718
           GO TO 0241                                                   05100718
20240      IVFAIL = IVFAIL + 1                                          05110718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05120718
 0241      CONTINUE                                                     05130718
C                                                                       05140718
CT025*  TEST 025   ****  FCVS PROGRAM 718  ****                         05150718
C                                                                       05160718
C                                                                       05170718
           IVTNUM =  25                                                 05180718
           LVCORR = .FALSE.                                             05190718
      LVCOMP = LPN002.NEQV.LPN004                                       05200718
           IVCOMP = 0                                                   05210718
           IF (LVCOMP) IVCOMP = 1                                       05220718
           IF (IVCOMP - 0) 20250, 10250, 20250                          05230718
10250      IVPASS = IVPASS + 1                                          05240718
           WRITE (I02,80002) IVTNUM                                     05250718
           GO TO 0251                                                   05260718
20250      IVFAIL = IVFAIL + 1                                          05270718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05280718
 0251      CONTINUE                                                     05290718
C                                                                       05300718
C    TESTS 26-29 TEST LOGICAL CONSTANT EXPRESSIONS USING SYMBOLIC NAMES 05310718
C    OF LOGICAL CONSTANTS                                               05320718
C                                                                       05330718
C                                                                       05340718
CT026*  TEST 026   ****  FCVS PROGRAM 718  ****                         05350718
C                                                                       05360718
C                                                                       05370718
           IVTNUM =  26                                                 05380718
           LVCORR = .FALSE.                                             05390718
      LVCOMP = LPN001.EQV.LPN002.NEQV.LPN004                            05400718
           IVCOMP = 0                                                   05410718
           IF (LVCOMP) IVCOMP = 1                                       05420718
           IF (IVCOMP - 0) 20260, 10260, 20260                          05430718
10260      IVPASS = IVPASS + 1                                          05440718
           WRITE (I02,80002) IVTNUM                                     05450718
           GO TO 0261                                                   05460718
20260      IVFAIL = IVFAIL + 1                                          05470718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05480718
 0261      CONTINUE                                                     05490718
C                                                                       05500718
CT027*  TEST 027   ****  FCVS PROGRAM 718  ****                         05510718
C                                                                       05520718
C                                                                       05530718
           IVTNUM =  27                                                 05540718
           LVCORR = .TRUE.                                              05550718
      LVCOMP = LPN003.NEQV.LPN001.AND.LPN002                            05560718
           IVCOMP = 0                                                   05570718
           IF (LVCOMP) IVCOMP = 1                                       05580718
           IF (IVCOMP - 1) 20270, 10270, 20270                          05590718
10270      IVPASS = IVPASS + 1                                          05600718
           WRITE (I02,80002) IVTNUM                                     05610718
           GO TO 0271                                                   05620718
20270      IVFAIL = IVFAIL + 1                                          05630718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05640718
 0271      CONTINUE                                                     05650718
C                                                                       05660718
CT028*  TEST 028   ****  FCVS PROGRAM 718  ****                         05670718
C                                                                       05680718
C                                                                       05690718
           IVTNUM =  28                                                 05700718
           LVCORR = .FALSE.                                             05710718
      LVCOMP = (LPN003.NEQV.LPN001).AND.LPN002                          05720718
           IVCOMP = 0                                                   05730718
           IF (LVCOMP) IVCOMP = 1                                       05740718
           IF (IVCOMP - 0) 20280, 10280, 20280                          05750718
10280      IVPASS = IVPASS + 1                                          05760718
           WRITE (I02,80002) IVTNUM                                     05770718
           GO TO 0281                                                   05780718
20280      IVFAIL = IVFAIL + 1                                          05790718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05800718
 0281      CONTINUE                                                     05810718
C                                                                       05820718
CT029*  TEST 029   ****  FCVS PROGRAM 718  ****                         05830718
C                                                                       05840718
C                                                                       05850718
           IVTNUM =  29                                                 05860718
           LVCORR = .TRUE.                                              05870718
      LVCOMP = .NOT.(LPN002.EQV.LPN004.AND.LPN001.OR.LPN003)            05880718
           IVCOMP = 0                                                   05890718
           IF (LVCOMP) IVCOMP = 1                                       05900718
           IF (IVCOMP - 1) 20290, 10290, 20290                          05910718
10290      IVPASS = IVPASS + 1                                          05920718
           WRITE (I02,80002) IVTNUM                                     05930718
           GO TO 0291                                                   05940718
20290      IVFAIL = IVFAIL + 1                                          05950718
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05960718
 0291      CONTINUE                                                     05970718
C                                                                       05980718
CBB** ********************** BBCSUM0  **********************************05990718
C**** WRITE OUT TEST SUMMARY                                            06000718
C****                                                                   06010718
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        06020718
      WRITE (I02, 90004)                                                06030718
      WRITE (I02, 90014)                                                06040718
      WRITE (I02, 90004)                                                06050718
      WRITE (I02, 90020) IVPASS                                         06060718
      WRITE (I02, 90022) IVFAIL                                         06070718
      WRITE (I02, 90024) IVDELE                                         06080718
      WRITE (I02, 90026) IVINSP                                         06090718
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 06100718
CBE** ********************** BBCSUM0  **********************************06110718
CBB** ********************** BBCFOOT0 **********************************06120718
C**** WRITE OUT REPORT FOOTINGS                                         06130718
C****                                                                   06140718
      WRITE (I02,90016) ZPROG, ZPROG                                    06150718
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     06160718
      WRITE (I02,90019)                                                 06170718
CBE** ********************** BBCFOOT0 **********************************06180718
90001 FORMAT (1H ,56X,5HFM718)                                          06190718
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM718)                          06200718
CBB** ********************** BBCFMT0A **********************************06210718
C**** FORMATS FOR TEST DETAIL LINES                                     06220718
C****                                                                   06230718
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           06240718
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           06250718
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           06260718
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           06270718
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           06280718
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    06290718
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06300718
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              06310718
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06320718
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  06330718
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         06340718
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         06350718
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         06360718
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         06370718
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      06380718
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      06390718
80050 FORMAT (1H ,48X,A31)                                              06400718
CBE** ********************** BBCFMT0A **********************************06410718
CBB** ********************** BBCFMAT1 **********************************06420718
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     06430718
C****                                                                   06440718
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06450718
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            06460718
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     06470718
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     06480718
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06490718
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06500718
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06510718
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06520718
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06530718
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  06540718
     21H(,F12.5,2H, ,F12.5,1H))                                         06550718
CBE** ********************** BBCFMAT1 **********************************06560718
CBB** ********************** BBCFMT0B **********************************06570718
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                06580718
C****                                                                   06590718
90002 FORMAT (1H1)                                                      06600718
90004 FORMAT (1H )                                                      06610718
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               06620718
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06630718
90008 FORMAT (1H ,21X,A13,A17)                                          06640718
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       06650718
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    06660718
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     06670718
     1       7X,7HREMARKS,24X)                                          06680718
90014 FORMAT (1H ,46H----------------------------------------------,    06690718
     1        33H---------------------------------)                     06700718
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               06710718
C****                                                                   06720718
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             06730718
C****                                                                   06740718
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          06750718
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        06760718
     1        A13)                                                      06770718
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 06780718
C****                                                                   06790718
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 06800718
C****                                                                   06810718
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              06820718
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              06830718
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             06840718
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  06850718
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  06860718
CBE** ********************** BBCFMT0B **********************************06870718
           END                                                          06880718
