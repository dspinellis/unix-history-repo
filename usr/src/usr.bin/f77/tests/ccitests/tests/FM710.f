      PROGRAM FM710                                                     00010710
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020710
C     THIS ROUTINE TESTS SUBSCRIPT EXPRESSIONS AND          ANS REF.    00030710
C     CHARACTER SUBSTRINGS.                                 5.4.2, 5.4.300040710
C                                                           5.7.1, 5.7.200050710
C                                                                       00060710
C     THIS ROUTINE ASSUMES THE INTRINSIC FUNCTIONS                      00070710
C                              INT AND IABS ARE WORKING.                00080710
C                                                                       00090710
CBB** ********************** BBCCOMNT **********************************00100710
C****                                                                   00110710
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120710
C****                          VERSION 2.0                              00130710
C****                                                                   00140710
C****                                                                   00150710
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160710
C****                   GENERAL SERVICES ADMINISTRATION                 00170710
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180710
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190710
C****                      FALLS CHURCH, VA. 22041                      00200710
C****                                                                   00210710
C****                          (703) 756-6153                           00220710
C****                                                                   00230710
CBE** ********************** BBCCOMNT **********************************00240710
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00250710
           IMPLICIT CHARACTER*27 (C)                                    00260710
CBB** ********************** BBCINITA **********************************00270710
C**** SPECIFICATION STATEMENTS                                          00280710
C****                                                                   00290710
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00300710
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00310710
CBE** ********************** BBCINITA **********************************00320710
C                                                                       00330710
      DIMENSION I2N001(2,3), I2N002(3,5), I1N003(-1:8), I2N004(10,4)    00340710
      CHARACTER*10 CVCOMP, CVCORR, CVN001, C2N001(2,4)                  00350710
      DATA I2N001 /1,2,3,4,5,6/                                         00360710
      DATA I2N002 /11,21,31,12,22,32,13,23,33,14,24,34,15,25,35/        00370710
      DATA I1N003 /1,2,3,4,5,6,7,8,9,10/                                00380710
      DATA I2N004 / 10, 9, 8, 7, 6, 5, 4, 3, 2, 1,                      00390710
     1               4,-2, 6,-3, 8,-4,10,-5, 2,-1,                      00400710
     2               1, 3, 5, 7, 9, 2, 4, 6, 8, 10,                     00410710
     3             -10,-9,-8,-7,-6,-5,-4,-3,-2,-1 /                     00420710
      DATA C2N001 /'11FIRSTELE','21SECONDXX','12THIRDXYZ','22FOURTHWW', 00430710
     1             '13FIFTHABC','23SIXTHIJK','14SEVENTHH','24EIGHTHUV'/ 00440710
C                                                                       00450710
C                                                                       00460710
CBB** ********************** BBCINITB **********************************00470710
C**** INITIALIZE SECTION                                                00480710
      DATA  ZVERS,                  ZVERSD,             ZDATE           00490710
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00500710
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00510710
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00520710
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00530710
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00540710
      DATA   REMRKS /'                               '/                 00550710
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00560710
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00570710
C****                                                                   00580710
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00590710
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00600710
CZ03  ZPROG  = 'PROGRAM NAME'                                           00610710
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00680710
      IVPASS = 0                                                        00690710
      IVFAIL = 0                                                        00700710
      IVDELE = 0                                                        00710710
      IVINSP = 0                                                        00720710
      IVTOTL = 0                                                        00730710
      IVTOTN = 0                                                        00740710
      ICZERO = 0                                                        00750710
C                                                                       00760710
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00770710
      I01 = 05                                                          00780710
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00790710
      I02 = 06                                                          00800710
C                                                                       00810710
      I01 = 5                                                           00820710
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00830710
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00840710
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00850710
C                                                                       00860710
      I02 = 6                                                           00870710
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00880710
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00890710
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00900710
C                                                                       00910710
CBE** ********************** BBCINITB **********************************00920710
           ZPROG='FM710'                                                00930710
           IVTOTL =  19                                                 00940710
CBB** ********************** BBCHED0A **********************************00950710
C****                                                                   00960710
C**** WRITE REPORT TITLE                                                00970710
C****                                                                   00980710
      WRITE (I02, 90002)                                                00990710
      WRITE (I02, 90006)                                                01000710
      WRITE (I02, 90007)                                                01010710
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01020710
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01030710
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01040710
CBE** ********************** BBCHED0A **********************************01050710
CBB** ********************** BBCHED0B **********************************01060710
C**** WRITE DETAIL REPORT HEADERS                                       01070710
C****                                                                   01080710
      WRITE (I02,90004)                                                 01090710
      WRITE (I02,90004)                                                 01100710
      WRITE (I02,90013)                                                 01110710
      WRITE (I02,90014)                                                 01120710
      WRITE (I02,90015) IVTOTL                                          01130710
CBE** ********************** BBCHED0B **********************************01140710
C                                                                       01150710
C     TESTS 1-2 - SUBSCRIPT EXPRESSION TO IDENTIFY VARIOUS              01160710
C                 ARRAY ELEMENTS                                        01170710
C                                                                       01180710
C                                                                       01190710
CT001*  TEST 001   ****  FCVS PROGRAM 710  ****                         01200710
C                                                                       01210710
C     TEST 001 ARRAY ELEMENT REFERENCE                                  01220710
C                                                                       01230710
           IVTNUM =   1                                                 01240710
           IVCOMP = 0                                                   01250710
           IVCORR = 34                                                  01260710
      IVCOMP = I2N002(I2N001(1,2),I2N001(2,3)/2 + 1)                    01270710
40010      IF (IVCOMP - 34) 20010, 10010, 20010                         01280710
10010      IVPASS = IVPASS + 1                                          01290710
           WRITE (I02,80002) IVTNUM                                     01300710
           GO TO 0011                                                   01310710
20010      IVFAIL = IVFAIL + 1                                          01320710
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01330710
 0011      CONTINUE                                                     01340710
C                                                                       01350710
CT002*  TEST 002   ****  FCVS PROGRAM 710  ****                         01360710
C                                                                       01370710
C     TEST 002 FUNCTION REFERENCE                                       01380710
C                                                                       01390710
           IVTNUM =   2                                                 01400710
           RVD001 = 2.64                                                01410710
           IVCOMP = 0                                                   01420710
           IVCORR = 25                                                  01430710
      IVCOMP = I2N002(INT(RVD001), 19 - IABS(-7)*2)                     01440710
40020      IF (IVCOMP - 25) 20020, 10020, 20020                         01450710
10020      IVPASS = IVPASS + 1                                          01460710
           WRITE (I02,80002) IVTNUM                                     01470710
           GO TO 0021                                                   01480710
20020      IVFAIL = IVFAIL + 1                                          01490710
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01500710
 0021      CONTINUE                                                     01510710
C                                                                       01520710
C     TESTS 3-7 - TEST SUBSCRIPT VALUE IN IDENTIFYING                   01530710
C                 ARRAY ELEMENTS                                        01540710
C                                                                       01550710
CT003*  TEST 003   ****  FCVS PROGRAM 710  ****                         01560710
C                                                                       01570710
C     TEST 003 RANGE                                                    01580710
C                                                                       01590710
           IVTNUM = 3                                                   01600710
           WRITE (I02, 80004) IVTNUM                                    01610710
           WRITE (I02, 80020)                                           01620710
      WRITE (I02, 70030) (I1N003(IVN001), IVN001=5,8)                   01630710
70030 FORMAT (1H ,26X,4I4)                                              01640710
           IVINSP = IVINSP + 1                                          01650710
           WRITE (I02, 80022)                                           01660710
           WRITE (I02, 70031)                                           01670710
70031      FORMAT (1H ,26X,16H   7   8   9  10)                         01680710
C                                                                       01690710
CT004*  TEST 004   ****  FCVS PROGRAM 710  ****                         01700710
C                                                                       01710710
C     TEST 004 SINGLE ELEMENT                                           01720710
C                                                                       01730710
           IVTNUM =   4                                                 01740710
           IVCOMP = 0                                                   01750710
           IVCORR = 4                                                   01760710
      IVCOMP = I1N003(2)                                                01770710
40040      IF (IVCOMP - 4) 20040, 10040, 20040                          01780710
10040      IVPASS = IVPASS + 1                                          01790710
           WRITE (I02,80002) IVTNUM                                     01800710
           GO TO 0041                                                   01810710
20040      IVFAIL = IVFAIL + 1                                          01820710
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01830710
 0041      CONTINUE                                                     01840710
C                                                                       01850710
CT005*  TEST 005   ****  FCVS PROGRAM 710  ****                         01860710
C                                                                       01870710
C     TEST 005 EXPRESSION                                               01880710
C                                                                       01890710
           IVTNUM =   5                                                 01900710
           IVN001 = -3                                                  01910710
           IVCOMP = 0                                                   01920710
           IVCORR = 1                                                   01930710
      IVCOMP = I1N003((IVN001+5)*3 - 7)                                 01940710
40050      IF (IVCOMP - 1) 20050, 10050, 20050                          01950710
10050      IVPASS = IVPASS + 1                                          01960710
           WRITE (I02,80002) IVTNUM                                     01970710
           GO TO 0051                                                   01980710
20050      IVFAIL = IVFAIL + 1                                          01990710
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02000710
 0051      CONTINUE                                                     02010710
C                                                                       02020710
CT006*  TEST 006   ****  FCVS PROGRAM 710  ****                         02030710
C                                                                       02040710
C     TEST 006 31ST ELEMENT IN 2 DIMENSIONAL, 40 ELEMENT ARRAY          02050710
C                                                                       02060710
           IVTNUM =   6                                                 02070710
           IVCOMP = 0                                                   02080710
           IVCORR = -10                                                 02090710
      IVCOMP = I2N004(1,4)                                              02100710
40060      IF (IVCOMP + 10) 20060, 10060, 20060                         02110710
10060      IVPASS = IVPASS + 1                                          02120710
           WRITE (I02,80002) IVTNUM                                     02130710
           GO TO 0061                                                   02140710
20060      IVFAIL = IVFAIL + 1                                          02150710
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02160710
 0061      CONTINUE                                                     02170710
C                                                                       02180710
CT007*  TEST 007   ****  FCVS PROGRAM 710  ****                         02190710
C                                                                       02200710
C     TEST 007 4TH ELEMENT OF FIRST ARRAY EQUAL TO                      02210710
C              11TH ELEMENT OF SECOND ARRAY                             02220710
C                                                                       02230710
           IVTNUM =   7                                                 02240710
           IVCOMP = 0                                                   02250710
           IVCORR = 1                                                   02260710
           IF (I1N003(2).EQ.I2N004(1,2)) IVCOMP = 1                     02270710
40070      IF (IVCOMP - 1) 20070, 10070, 20070                          02280710
10070      IVPASS = IVPASS + 1                                          02290710
           WRITE (I02,80002) IVTNUM                                     02300710
           GO TO 0071                                                   02310710
20070      IVFAIL = IVFAIL + 1                                          02320710
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02330710
 0071      CONTINUE                                                     02340710
C                                                                       02350710
C     TESTS 8-15 - CHARACTER SUBSTRING NAME                             02360710
C                                                                       02370710
C                                                                       02380710
CT008*  TEST 008   ****  FCVS PROGRAM 710  ****                         02390710
C                                                                       02400710
C     TEST 008 USING LEFT AND RIGHT POSITION OF SUBSTRING               02410710
C                                                                       02420710
           IVTNUM =   8                                                 02430710
           CVCOMP = ' '                                                 02440710
           IVCOMP = 0                                                   02450710
           CVN001 = 'THIS IS IT'                                        02460710
           CVCORR = 'HIS       '                                        02470710
      CVCOMP = CVN001(2:4)                                              02480710
           IF (CVCOMP .EQ. 'HIS       ') IVCOMP = 1                     02490710
           IF (IVCOMP - 1) 20080, 10080, 20080                          02500710
10080      IVPASS = IVPASS + 1                                          02510710
           WRITE (I02,80002) IVTNUM                                     02520710
           GO TO 0081                                                   02530710
20080      IVFAIL = IVFAIL + 1                                          02540710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     02550710
 0081      CONTINUE                                                     02560710
C                                                                       02570710
CT009*  TEST 009   ****  FCVS PROGRAM 710  ****                         02580710
C                                                                       02590710
C     TEST 009 LEFT POSITION OMITTED, VALUE OF 1 ASSUMED                02600710
C                                                                       02610710
           IVTNUM =   9                                                 02620710
           CVCOMP = ' '                                                 02630710
           IVCOMP = 0                                                   02640710
           CVCORR = 'THIS      '                                        02650710
      CVCOMP = CVN001(:4)                                               02660710
           IF (CVCOMP .EQ. 'THIS      ') IVCOMP = 1                     02670710
           IF (IVCOMP - 1) 20090, 10090, 20090                          02680710
10090      IVPASS = IVPASS + 1                                          02690710
           WRITE (I02,80002) IVTNUM                                     02700710
           GO TO 0091                                                   02710710
20090      IVFAIL = IVFAIL + 1                                          02720710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     02730710
 0091      CONTINUE                                                     02740710
C                                                                       02750710
CT010*  TEST 010   ****  FCVS PROGRAM 710  ****                         02760710
C                                                                       02770710
C     TEST 010 RIGHT POSITION OMITTED, RIGHT-HAND END OF STRING ASSUMED 02780710
C                                                                       02790710
           IVTNUM =  10                                                 02800710
           CVCOMP = ' '                                                 02810710
           IVCOMP = 0                                                   02820710
           CVCORR = 'S IS IT   '                                        02830710
      CVCOMP = CVN001(4:)                                               02840710
           IF (CVCOMP .EQ. 'S IS IT   ') IVCOMP = 1                     02850710
           IF (IVCOMP - 1) 20100, 10100, 20100                          02860710
10100      IVPASS = IVPASS + 1                                          02870710
           WRITE (I02,80002) IVTNUM                                     02880710
           GO TO 0101                                                   02890710
20100      IVFAIL = IVFAIL + 1                                          02900710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     02910710
 0101      CONTINUE                                                     02920710
C                                                                       02930710
CT011*  TEST 011   ****  FCVS PROGRAM 710  ****                         02940710
C                                                                       02950710
C     TEST 011 EXTRACT SUBSTRING FROM ARRAY                             02960710
C                                                                       02970710
           IVTNUM =  11                                                 02980710
           CVCOMP = ' '                                                 02990710
           IVCOMP = 0                                                   03000710
           CVCORR = '12THIR    '                                        03010710
      CVCOMP = C2N001(1,2)(1:6)                                         03020710
           IF (CVCOMP .EQ. '12THIR    ') IVCOMP = 1                     03030710
           IF (IVCOMP - 1) 20110, 10110, 20110                          03040710
10110      IVPASS = IVPASS + 1                                          03050710
           WRITE (I02,80002) IVTNUM                                     03060710
           GO TO 0111                                                   03070710
20110      IVFAIL = IVFAIL + 1                                          03080710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03090710
 0111      CONTINUE                                                     03100710
C                                                                       03110710
CT012*  TEST 012   ****  FCVS PROGRAM 710  ****                         03120710
C                                                                       03130710
C     TEST 012 ENTIRE SUBSTRING                                         03140710
C                                                                       03150710
           IVTNUM =  12                                                 03160710
           CVCOMP = ' '                                                 03170710
           IVCOMP = 0                                                   03180710
           CVCORR = 'THIS IS IT'                                        03190710
      CVCOMP = CVN001(:)                                                03200710
           IF (CVCOMP .EQ. 'THIS IS IT') IVCOMP = 1                     03210710
           IF (IVCOMP - 1) 20120, 10120, 20120                          03220710
10120      IVPASS = IVPASS + 1                                          03230710
           WRITE (I02,80002) IVTNUM                                     03240710
           GO TO 0121                                                   03250710
20120      IVFAIL = IVFAIL + 1                                          03260710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03270710
 0121      CONTINUE                                                     03280710
C                                                                       03290710
CT013*  TEST 013   ****  FCVS PROGRAM 710  ****                         03300710
C                                                                       03310710
C     TEST 013 ENTIRE SUBSTRING FROM ARRAY                              03320710
C                                                                       03330710
           IVTNUM =  13                                                 03340710
           CVCOMP = ' '                                                 03350710
           IVCOMP = 0                                                   03360710
           CVCORR = '23SIXTHIJK'                                        03370710
      CVCOMP = C2N001(2,3)(:)                                           03380710
           IF (CVCOMP .EQ. '23SIXTHIJK') IVCOMP = 1                     03390710
           IF (IVCOMP - 1) 20130, 10130, 20130                          03400710
10130      IVPASS = IVPASS + 1                                          03410710
           WRITE (I02,80002) IVTNUM                                     03420710
           GO TO 0131                                                   03430710
20130      IVFAIL = IVFAIL + 1                                          03440710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03450710
 0131      CONTINUE                                                     03460710
C                                                                       03470710
CT014*  TEST 014   ****  FCVS PROGRAM 710  ****                         03480710
C                                                                       03490710
C     RIGHT POSITION OMITTED USING ARRAY                                03500710
C                                                                       03510710
           IVTNUM =  14                                                 03520710
           CVCOMP = ' '                                                 03530710
           IVCOMP = 0                                                   03540710
           CVCORR = 'EVENTHH   '                                        03550710
      CVCOMP = C2N001(1,4)(4:)                                          03560710
           IF (CVCOMP .EQ. 'EVENTHH   ') IVCOMP = 1                     03570710
           IF (IVCOMP - 1) 20140, 10140, 20140                          03580710
10140      IVPASS = IVPASS + 1                                          03590710
           WRITE (I02,80002) IVTNUM                                     03600710
           GO TO 0141                                                   03610710
20140      IVFAIL = IVFAIL + 1                                          03620710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03630710
 0141      CONTINUE                                                     03640710
C                                                                       03650710
CT015*  TEST 015   ****  FCVS PROGRAM 710  ****                         03660710
C                                                                       03670710
C     LEFT POSITION OMITTED                                             03680710
C                                                                       03690710
           IVTNUM =  15                                                 03700710
           CVCOMP = ' '                                                 03710710
           IVCOMP = 0                                                   03720710
           CVCORR = '24EI      '                                        03730710
      CVCOMP = C2N001(2,4)(:4)                                          03740710
           IF (CVCOMP .EQ. '24EI      ') IVCOMP = 1                     03750710
           IF (IVCOMP - 1) 20150, 10150, 20150                          03760710
10150      IVPASS = IVPASS + 1                                          03770710
           WRITE (I02,80002) IVTNUM                                     03780710
           GO TO 0151                                                   03790710
20150      IVFAIL = IVFAIL + 1                                          03800710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03810710
 0151      CONTINUE                                                     03820710
C                                                                       03830710
C     TESTS 16-19 - SUBSTRING EXPRESSION                                03840710
C                                                                       03850710
C                                                                       03860710
CT016*  TEST 016   ****  FCVS PROGRAM 710  ****                         03870710
C                                                                       03880710
C     TEST 016 ARITHMETIC EXPRESSION                                    03890710
C                                                                       03900710
           IVTNUM =  16                                                 03910710
           CVCOMP = ' '                                                 03920710
           IVCOMP = 0                                                   03930710
           CVCORR = 'HIS IS IT '                                        03940710
      CVCOMP = CVN001(2:5*2)                                            03950710
           IF (CVCOMP .EQ. 'HIS IS IT ') IVCOMP = 1                     03960710
           IF (IVCOMP - 1) 20160, 10160, 20160                          03970710
10160      IVPASS = IVPASS + 1                                          03980710
           WRITE (I02,80002) IVTNUM                                     03990710
           GO TO 0161                                                   04000710
20160      IVFAIL = IVFAIL + 1                                          04010710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04020710
 0161      CONTINUE                                                     04030710
C                                                                       04040710
CT017*  TEST 017   ****  FCVS PROGRAM 710  ****                         04050710
C                                                                       04060710
C     TEST 017 SUBSTRING EXPRESSION IN AN ASSIGNMENT STATEMENT          04070710
C                                                                       04080710
           IVTNUM =  17                                                 04090710
           IVN001 = 5                                                   04100710
           IVN002 = 8                                                   04110710
           CVCOMP = ' '                                                 04120710
           IVCOMP = 0                                                   04130710
           CVCORR = 'THISLIKEIT'                                        04140710
      CVN001(IVN001:IVN002) = 'LIKE'                                    04150710
           CVCOMP = CVN001                                              04160710
           IF (CVCOMP .EQ. 'THISLIKEIT') IVCOMP = 1                     04170710
           IF (IVCOMP - 1) 20170, 10170, 20170                          04180710
10170      IVPASS = IVPASS + 1                                          04190710
           WRITE (I02,80002) IVTNUM                                     04200710
           GO TO 0171                                                   04210710
20170      IVFAIL = IVFAIL + 1                                          04220710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04230710
 0171      CONTINUE                                                     04240710
C                                                                       04250710
CT018*  TEST 018   ****  FCVS PROGRAM 710  ****                         04260710
C                                                                       04270710
C     TEST 018 SUBSTRING EXPRESSION CONTAINING ARRAY ELEMENT REFERENCE  04280710
C                                                                       04290710
           IVTNUM =  18                                                 04300710
           CVCOMP = ' '                                                 04310710
           IVCOMP = 0                                                   04320710
           CVCORR = 'HISLIKE   '                                        04330710
      CVCOMP = CVN001(I2N001(2,1):I2N002(3,5)-27)                       04340710
           IF (CVCOMP .EQ. 'HISLIKE   ') IVCOMP = 1                     04350710
           IF (IVCOMP - 1) 20180, 10180, 20180                          04360710
10180      IVPASS = IVPASS + 1                                          04370710
           WRITE (I02,80002) IVTNUM                                     04380710
           GO TO 0181                                                   04390710
20180      IVFAIL = IVFAIL + 1                                          04400710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04410710
 0181      CONTINUE                                                     04420710
C                                                                       04430710
CT019*  TEST 019   ****  FCVS PROGRAM 710  ****                         04440710
C                                                                       04450710
C     TEST 019 SUBSTRING EXPRESSION CONTAINING FUNCTION REFERENCES      04460710
C                                                                       04470710
           IVTNUM =  19                                                 04480710
           RVD001 = 1.475                                               04490710
           IVN001 = 1                                                   04500710
           CVCOMP = ' '                                                 04510710
           IVCOMP = 0                                                   04520710
           CVCORR = 'IFTHABC   '                                        04530710
      CVCOMP = C2N001(1,3)(INT(RVD001)+3 : (IVN001*5 + 7)/IABS(-6) + 8) 04540710
           IF (CVCOMP .EQ. 'IFTHABC   ') IVCOMP = 1                     04550710
           IF (IVCOMP - 1) 20190, 10190, 20190                          04560710
10190      IVPASS = IVPASS + 1                                          04570710
           WRITE (I02,80002) IVTNUM                                     04580710
           GO TO 0191                                                   04590710
20190      IVFAIL = IVFAIL + 1                                          04600710
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04610710
 0191      CONTINUE                                                     04620710
C                                                                       04630710
CBB** ********************** BBCSUM0  **********************************04640710
C**** WRITE OUT TEST SUMMARY                                            04650710
C****                                                                   04660710
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04670710
      WRITE (I02, 90004)                                                04680710
      WRITE (I02, 90014)                                                04690710
      WRITE (I02, 90004)                                                04700710
      WRITE (I02, 90020) IVPASS                                         04710710
      WRITE (I02, 90022) IVFAIL                                         04720710
      WRITE (I02, 90024) IVDELE                                         04730710
      WRITE (I02, 90026) IVINSP                                         04740710
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04750710
CBE** ********************** BBCSUM0  **********************************04760710
CBB** ********************** BBCFOOT0 **********************************04770710
C**** WRITE OUT REPORT FOOTINGS                                         04780710
C****                                                                   04790710
      WRITE (I02,90016) ZPROG, ZPROG                                    04800710
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04810710
      WRITE (I02,90019)                                                 04820710
CBE** ********************** BBCFOOT0 **********************************04830710
90001 FORMAT (1H ,56X,5HFM710)                                          04840710
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM710)                          04850710
CBB** ********************** BBCFMT0A **********************************04860710
C**** FORMATS FOR TEST DETAIL LINES                                     04870710
C****                                                                   04880710
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04890710
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04900710
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04910710
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04920710
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04930710
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04940710
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04950710
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04960710
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04970710
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04980710
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04990710
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         05000710
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         05010710
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         05020710
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      05030710
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      05040710
80050 FORMAT (1H ,48X,A31)                                              05050710
CBE** ********************** BBCFMT0A **********************************05060710
CBB** ********************** BBCFMAT1 **********************************05070710
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     05080710
C****                                                                   05090710
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05100710
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            05110710
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     05120710
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     05130710
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05140710
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05150710
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05160710
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05170710
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05180710
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  05190710
     21H(,F12.5,2H, ,F12.5,1H))                                         05200710
CBE** ********************** BBCFMAT1 **********************************05210710
CBB** ********************** BBCFMT0B **********************************05220710
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                05230710
C****                                                                   05240710
90002 FORMAT (1H1)                                                      05250710
90004 FORMAT (1H )                                                      05260710
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               05270710
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05280710
90008 FORMAT (1H ,21X,A13,A17)                                          05290710
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       05300710
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    05310710
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     05320710
     1       7X,7HREMARKS,24X)                                          05330710
90014 FORMAT (1H ,46H----------------------------------------------,    05340710
     1        33H---------------------------------)                     05350710
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               05360710
C****                                                                   05370710
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             05380710
C****                                                                   05390710
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          05400710
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        05410710
     1        A13)                                                      05420710
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 05430710
C****                                                                   05440710
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 05450710
C****                                                                   05460710
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              05470710
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              05480710
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             05490710
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  05500710
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  05510710
CBE** ********************** BBCFMT0B **********************************05520710
           END                                                          05530710
