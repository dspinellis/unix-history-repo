      PROGRAM FM352                                                     00010352
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020352
C                                                                       00030352
C          THIS PROGRAM CHECKS BASIC RELATIONAL EXPRESSIONS INVOLVING   00040352
C     OPERANDS OF REAL DATA TYPE.  IN EACH TEST, NOT ONLY THE RELATIONAL00050352
C     EXPRESSION IS TESTED, BUT THE TRICHOTOMY LAW OF MATHEMATICAL      00060352
C     RELATIONSHIPS IS ALSO TESTED (E.G., IF A .LT. B, THEN A CAN NOT   00070352
C     BE .GT. THAN B, AND A CAN NOT BE .EQ. B).  A TEST VARIABLE        00080352
C     (IVCOMP) IS USED TO REPORT THE RESULT OF THE TEST AS FOLLOWS,     00090352
C          IVCOMP = 0  IF BOTH THE TESTED RELATIONAL OPERATOR AND THE   00100352
C                      TRICHOTOMY TEST PASS.                            00110352
C          IVCOMP = 1  IF THE RELATIONAL TEST FAILS AND THE TRICHOTOMY  00120352
C                      TEST PASSES (WHICH WOULD INDICATE THAT A TESTED  00130352
C                      NOT .LT., .GT., OR .EQ. B).                      00140352
C          IVCOMP = 2  IF THE RELATIONAL TEST PASSES AND THE TRICHOTOMY 00150352
C                      TEST FAILS (WHICH WOULD INDICATE THAT A TESTED   00160352
C                      .LT., .GT., AND .EQ. B).                         00170352
C          IVCOMP = 3  IF BOTH THE RELATIONAL TEST AND THE TRICHOTOMY   00180352
C                      TEST FAIL (WHICH WOULD INDICATE THE RELATIONAL   00190352
C                      EXPRESSION TESTED OPPOSITE TO THAT EXPECTED      00200352
C                      (E.G., WHERE A WAS SUPPOSED TO BE .LT. B, IN     00210352
C                      FACT A .LT. B WAS FOUND TO BE FALSE AND A .GE. B 00220352
C                      WAS FOUND TO BE TRUE).                           00230352
C                                                                       00240352
C                                                                       00250352
C     REFERENCES -                                                      00260352
C                                                                       00270352
C     AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN, X3.9-197700280352
C          SECTION 4.4,  REAL TYPE                                      00290352
C          SECTION 6.3,  RELATIONAL EXPRESSIONS                         00300352
C          SECTION 6.5,  PRECEDENCE OF OPERATORS                        00310352
C          SECTION 6.6,  EVALUATION OF EXPRESSIONS                      00320352
C                                                                       00330352
C                                                                       00340352
C     ******************************************************************00350352
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00360352
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00370352
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00380352
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00390352
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00400352
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00410352
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00420352
C     THE RESULT OF EXECUTING THESE TESTS.                              00430352
C                                                                       00440352
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00450352
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00460352
C                                                                       00470352
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00480352
C                    DEPARTMENT OF THE NAVY                             00490352
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00500352
C                    WASHINGTON, D.C.   20376                           00510352
C                                                                       00520352
C     ******************************************************************00530352
C                                                                       00540352
C                                                                       00550352
      IMPLICIT LOGICAL (L)                                              00560352
      IMPLICIT CHARACTER*14 (C)                                         00570352
C                                                                       00580352
      DIMENSION RADN11(2)                                               00590352
      RFOS01(RDON01,RDON02) = RDON01 + RDON02                           00600352
C                                                                       00610352
C                                                                       00620352
C                                                                       00630352
C     INITIALIZATION SECTION.                                           00640352
C                                                                       00650352
C     INITIALIZE CONSTANTS                                              00660352
C     ********************                                              00670352
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00680352
      I01 = 5                                                           00690352
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00700352
      I02 = 6                                                           00710352
C     SYSTEM ENVIRONMENT SECTION                                        00720352
C                                                                       00730352
      I01 = 5                                                           00740352
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750352
C     (UNIT NUMBER FOR CARD READER).                                    00760352
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00770352
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00780352
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00790352
C                                                                       00800352
      I02 = 6                                                           00810352
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00820352
C     (UNIT NUMBER FOR PRINTER).                                        00830352
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00840352
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00850352
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00860352
C                                                                       00870352
      IVPASS = 0                                                        00880352
      IVFAIL = 0                                                        00890352
      IVDELE = 0                                                        00900352
      ICZERO = 0                                                        00910352
C                                                                       00920352
C     WRITE OUT PAGE HEADERS                                            00930352
C                                                                       00940352
      WRITE (I02,90002)                                                 00950352
      WRITE (I02,90006)                                                 00960352
      WRITE (I02,90008)                                                 00970352
      WRITE (I02,90004)                                                 00980352
      WRITE (I02,90010)                                                 00990352
      WRITE (I02,90004)                                                 01000352
      WRITE (I02,90016)                                                 01010352
      WRITE (I02,90001)                                                 01020352
      WRITE (I02,90004)                                                 01030352
      WRITE (I02,90012)                                                 01040352
      WRITE (I02,90014)                                                 01050352
      WRITE (I02,90004)                                                 01060352
C                                                                       01070352
C                                                                       01080352
C          TESTS 1 THROUGH 13 CHECK BASIC RELATIONAL EXPRESSIONS USING  01090352
C     ONLY REAL VARIABLE OPERANDS.  ALL THE VARIABLES ARE ASSIGNED REAL 01100352
C     CONSTANTS WITH EXPONENTIAL FORMAT.                                01110352
C                                                                       01120352
C                                                                       01130352
C     ****  FCVS PROGRAM 352  -  TEST 001  ****                         01140352
C                                                                       01150352
C          TEST 1 CHECKS THE .LT. OPERATOR USING TWO REAL OPERANDS      01160352
C     WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    01170352
C                                                                       01180352
      IVTNUM =   1                                                      01190352
      IF (ICZERO) 30010, 0010, 30010                                    01200352
 0010 CONTINUE                                                          01210352
      RVON01 = 1.0001 E17                                               01220352
      RVON02 = 1.0001 E18                                               01230352
      IVCOMP = 0                                                        01240352
      IVCORR = 0                                                        01250352
40010 IF(RVON01 .LT. RVON02)  GO TO 40011                               01260352
      IVCOMP = 1                                                        01270352
40011 IF (RVON01 .GE. RVON02)  IVCOMP = IVCOMP + 2                      01280352
      IF (IVCOMP) 20010, 10010, 20010                                   01290352
30010 IVDELE = IVDELE + 1                                               01300352
      WRITE (I02,80000) IVTNUM                                          01310352
      IF (ICZERO) 10010, 0021, 20010                                    01320352
10010 IVPASS = IVPASS + 1                                               01330352
      WRITE (I02,80002) IVTNUM                                          01340352
      GO TO 0021                                                        01350352
20010 IVFAIL = IVFAIL + 1                                               01360352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01370352
 0021 CONTINUE                                                          01380352
C                                                                       01390352
C     ****  FCVS PROGRAM 352  -  TEST 002  ****                         01400352
C                                                                       01410352
C          TEST 2 CHECKS THE .LT. OPERATOR USING TWO REAL OPERANDS      01420352
C     WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    01430352
C                                                                       01440352
      IVTNUM =   2                                                      01450352
      IF (ICZERO) 30020, 0020, 30020                                    01460352
 0020 CONTINUE                                                          01470352
      RVON01 = 1.0001 E17                                               01480352
      RVON02 = 1.9999 E17                                               01490352
      IVCOMP = 0                                                        01500352
      IVCORR = 0                                                        01510352
40020 IF (RVON01 .LT. RVON02)  GO TO 40021                              01520352
      IVCOMP = 1                                                        01530352
40021 IF (RVON01 .GE. RVON02)  IVCOMP = IVCOMP + 2                      01540352
      IF (IVCOMP)  20020, 10020, 20020                                  01550352
30020 IVDELE = IVDELE + 1                                               01560352
      WRITE (I02,80000) IVTNUM                                          01570352
      IF (ICZERO) 10020, 0031, 20020                                    01580352
10020 IVPASS = IVPASS + 1                                               01590352
      WRITE (I02,80002) IVTNUM                                          01600352
      GO TO 0031                                                        01610352
20020 IVFAIL = IVFAIL + 1                                               01620352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01630352
 0031 CONTINUE                                                          01640352
C                                                                       01650352
C     ****  FCVS PROGRAM 352  -  TEST 003  ****                         01660352
C                                                                       01670352
C          TEST 3 CHECKS THE .LE. OPERATOR USING TWO REAL OPERANDS      01680352
C     WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    01690352
C                                                                       01700352
      IVTNUM =   3                                                      01710352
      IF (ICZERO) 30030, 0030, 30030                                    01720352
 0030 CONTINUE                                                          01730352
      RVON01 = 1.0001 E17                                               01740352
      RVON02 = 1.0001 E18                                               01750352
      IVCOMP = 0                                                        01760352
      IVCORR = 0                                                        01770352
40030 IF (RVON01 .LE. RVON02)  GO TO 40031                              01780352
      IVCOMP = 1                                                        01790352
40031 IF (RVON01 .GT. RVON02)  IVCOMP = IVCOMP + 2                      01800352
      IF (IVCOMP)  20030, 10030, 20030                                  01810352
30030 IVDELE = IVDELE + 1                                               01820352
      WRITE (I02,80000) IVTNUM                                          01830352
      IF (ICZERO) 10030, 0041, 20030                                    01840352
10030 IVPASS = IVPASS + 1                                               01850352
      WRITE (I02,80002) IVTNUM                                          01860352
      GO TO 0041                                                        01870352
20030 IVFAIL = IVFAIL + 1                                               01880352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01890352
 0041 CONTINUE                                                          01900352
C                                                                       01910352
C     ****  FCVS PROGRAM 352  -  TEST 004  ****                         01920352
C                                                                       01930352
C          TEST 4 CHECKS THE .LE. OPERATOR USING TWO REAL OPERANDS      01940352
C     WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    01950352
C                                                                       01960352
      IVTNUM =   4                                                      01970352
      IF (ICZERO) 30040, 0040, 30040                                    01980352
 0040 CONTINUE                                                          01990352
      RVON01 = 1.0001 E17                                               02000352
      RVON02 = 1.9999 E17                                               02010352
      IVCOMP = 0                                                        02020352
      IVCORR = 0                                                        02030352
40040 IF (RVON01 .LE. RVON02)  GO TO 40041                              02040352
      IVCOMP = 1                                                        02050352
40041 IF (RVON01 .GT. RVON02)  IVCOMP = IVCOMP + 2                      02060352
      IF (IVCOMP)  20040, 10040, 20040                                  02070352
30040 IVDELE = IVDELE + 1                                               02080352
      WRITE (I02,80000) IVTNUM                                          02090352
      IF (ICZERO) 10040, 0051, 20040                                    02100352
10040 IVPASS = IVPASS + 1                                               02110352
      WRITE (I02,80002) IVTNUM                                          02120352
      GO TO 0051                                                        02130352
20040 IVFAIL = IVFAIL + 1                                               02140352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02150352
 0051 CONTINUE                                                          02160352
C                                                                       02170352
C     ****  FCVS PROGRAM 352  -  TEST 005  ****                         02180352
C                                                                       02190352
C          TEST 5 CHECKS THE .LE. OPERATOR USING TWO REAL OPERANDS      02200352
C     WHICH HAVE BEEN ASSIGNED THE SAME REAL CONSTANT.                  02210352
C                                                                       02220352
      IVTNUM =   5                                                      02230352
      IF (ICZERO) 30050, 0050, 30050                                    02240352
 0050 CONTINUE                                                          02250352
      RVON01 = 1.0001 E17                                               02260352
      RVON02 = 1.0001 E17                                               02270352
      IVCOMP = 0                                                        02280352
      IVCORR = 0                                                        02290352
40050 IF (RVON01 .LE. RVON02)  GO TO 40051                              02300352
      IVCOMP = 1                                                        02310352
40051 IF (RVON01 .GT. RVON02) IVCOMP = IVCOMP + 2                       02320352
      IF (IVCOMP)  20050, 10050, 20050                                  02330352
30050 IVDELE = IVDELE + 1                                               02340352
      WRITE (I02,80000) IVTNUM                                          02350352
      IF (ICZERO) 10050, 0061, 20050                                    02360352
10050 IVPASS = IVPASS + 1                                               02370352
      WRITE (I02,80002) IVTNUM                                          02380352
      GO TO 0061                                                        02390352
20050 IVFAIL = IVFAIL + 1                                               02400352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02410352
 0061 CONTINUE                                                          02420352
C                                                                       02430352
C     ****  FCVS PROGRAM 352  -  TEST 006  ****                         02440352
C                                                                       02450352
C          TEST 6 CHECKS THE .NE. OPERATOR USING TWO REAL OPERANDS      02460352
C     WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    02470352
C                                                                       02480352
      IVTNUM =   6                                                      02490352
      IF (ICZERO) 30060, 0060, 30060                                    02500352
 0060 CONTINUE                                                          02510352
      RVON01 = 1.0001 E17                                               02520352
      RVON02 = 1.0001 E18                                               02530352
      IVCOMP = 0                                                        02540352
      IVCORR = 0                                                        02550352
40060 IF (RVON01 .NE. RVON02)  GO TO 40061                              02560352
      IVCOMP = 1                                                        02570352
40061 IF (RVON01 .EQ. RVON02)  IVCOMP = IVCOMP + 2                      02580352
      IF (IVCOMP)  20060, 10060, 20060                                  02590352
30060 IVDELE = IVDELE + 1                                               02600352
      WRITE (I02,80000) IVTNUM                                          02610352
      IF (ICZERO) 10060, 0071, 20060                                    02620352
10060 IVPASS = IVPASS + 1                                               02630352
      WRITE (I02,80002) IVTNUM                                          02640352
      GO TO 0071                                                        02650352
20060 IVFAIL = IVFAIL + 1                                               02660352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02670352
 0071 CONTINUE                                                          02680352
C                                                                       02690352
C     ****  FCVS PROGRAM 352  -  TEST 007  ****                         02700352
C                                                                       02710352
C          TEST 7 CHECKS THE .NE. OPERATOR USING TWO REAL OPERANDS      02720352
C     WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    02730352
C                                                                       02740352
      IVTNUM =   7                                                      02750352
      IF (ICZERO) 30070, 0070, 30070                                    02760352
 0070 CONTINUE                                                          02770352
      RVON01 = 1.0001 E17                                               02780352
      RVON02 = 1.9999 E17                                               02790352
      IVCOMP = 0                                                        02800352
      IVCORR = 0                                                        02810352
40070 IF (RVON01 .NE. RVON02)  GO TO 40071                              02820352
      IVCOMP = 1                                                        02830352
40071 IF (RVON01 .EQ. RVON02)  IVCOMP = IVCOMP + 2                      02840352
      IF (IVCOMP)  20070, 10070, 20070                                  02850352
30070 IVDELE = IVDELE + 1                                               02860352
      WRITE (I02,80000) IVTNUM                                          02870352
      IF (ICZERO) 10070, 0081, 20070                                    02880352
10070 IVPASS = IVPASS + 1                                               02890352
      WRITE (I02,80002) IVTNUM                                          02900352
      GO TO 0081                                                        02910352
20070 IVFAIL = IVFAIL + 1                                               02920352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02930352
 0081 CONTINUE                                                          02940352
C                                                                       02950352
C     ****  FCVS PROGRAM 352  -  TEST 008  ****                         02960352
C                                                                       02970352
C          TEST 8 CHECKS THE .EQ. OPERATOR USING TWO REAL OPERANDS      02980352
C     WHICH HAVE BEEN ASSIGNED THE SAME REAL CONSTANT.                  02990352
C                                                                       03000352
      IVTNUM =   8                                                      03010352
      IF (ICZERO) 30080, 0080, 30080                                    03020352
 0080 CONTINUE                                                          03030352
      RVON01 = 1.0001 E17                                               03040352
      RVON02 = 1.0001 E17                                               03050352
      IVCOMP = 0                                                        03060352
      IVCORR = 0                                                        03070352
40080 IF (RVON01 .EQ. RVON02)  GO TO 40081                              03080352
      IVCOMP = 1                                                        03090352
40081 IF (RVON01 .NE. RVON02)  IVCOMP = IVCOMP + 2                      03100352
      IF (IVCOMP)  20080, 10080, 20080                                  03110352
30080 IVDELE = IVDELE + 1                                               03120352
      WRITE (I02,80000) IVTNUM                                          03130352
      IF (ICZERO) 10080, 0091, 20080                                    03140352
10080 IVPASS = IVPASS + 1                                               03150352
      WRITE (I02,80002) IVTNUM                                          03160352
      GO TO 0091                                                        03170352
20080 IVFAIL = IVFAIL + 1                                               03180352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03190352
 0091 CONTINUE                                                          03200352
C                                                                       03210352
C     ****  FCVS PROGRAM 352  -  TEST 009  ****                         03220352
C                                                                       03230352
C          TEST 9 CHECKS THE .GT. OPERATOR USING TWO REAL OPERANDS      03240352
C     WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    03250352
C                                                                       03260352
      IVTNUM =   9                                                      03270352
      IF (ICZERO) 30090, 0090, 30090                                    03280352
 0090 CONTINUE                                                          03290352
      RVON01 = 1.0001 E18                                               03300352
      RVON02 = 1.0001 E17                                               03310352
      IVCOMP = 0                                                        03320352
      IVCORR = 0                                                        03330352
40090 IF(RVON01 .GT. RVON02)  GO TO 40091                               03340352
      IVCOMP = 1                                                        03350352
40091 IF (RVON01 .LE. RVON02)  IVCOMP = IVCOMP + 2                      03360352
      IF (IVCOMP)  20090, 10090, 20090                                  03370352
30090 IVDELE = IVDELE + 1                                               03380352
      WRITE (I02,80000) IVTNUM                                          03390352
      IF (ICZERO) 10090, 0101, 20090                                    03400352
10090 IVPASS = IVPASS + 1                                               03410352
      WRITE (I02,80002) IVTNUM                                          03420352
      GO TO 0101                                                        03430352
20090 IVFAIL = IVFAIL + 1                                               03440352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03450352
 0101 CONTINUE                                                          03460352
C                                                                       03470352
C     ****  FCVS PROGRAM 352  -  TEST 010  ****                         03480352
C                                                                       03490352
C          TEST 10 CHECKS THE .GT. OPERATOR USING TWO REAL OPERANDS     03500352
C     WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    03510352
C                                                                       03520352
      IVTNUM =  10                                                      03530352
      IF (ICZERO) 30100, 0100, 30100                                    03540352
 0100 CONTINUE                                                          03550352
      RVON01 = 1.9999 E17                                               03560352
      RVON02 = 1.0001 E17                                               03570352
      IVCOMP = 0                                                        03580352
      IVCORR = 0                                                        03590352
40100 IF (RVON01 .GT. RVON02)  GO TO 40101                              03600352
      IVCOMP = 1                                                        03610352
40101 IF (RVON01 .LE. RVON02)  IVCOMP = IVCOMP + 2                      03620352
      IF (IVCOMP)  20100, 10100, 20100                                  03630352
30100 IVDELE = IVDELE + 1                                               03640352
      WRITE (I02,80000) IVTNUM                                          03650352
      IF (ICZERO) 10100, 0111, 20100                                    03660352
10100 IVPASS = IVPASS + 1                                               03670352
      WRITE (I02,80002) IVTNUM                                          03680352
      GO TO 0111                                                        03690352
20100 IVFAIL = IVFAIL + 1                                               03700352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03710352
 0111 CONTINUE                                                          03720352
C                                                                       03730352
C     ****  FCVS PROGRAM 352  -  TEST 011  ****                         03740352
C                                                                       03750352
C          TEST 11 CHECKS THE .GE. OPERATOR USING TWO REAL OPERANDS     03760352
C     WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    03770352
C                                                                       03780352
      IVTNUM =  11                                                      03790352
      IF (ICZERO) 30110, 0110, 30110                                    03800352
 0110 CONTINUE                                                          03810352
      RVON01 = 1.0001 E18                                               03820352
      RVON02 = 1.0001 E17                                               03830352
      IVCOMP = 0                                                        03840352
      IVCORR = 0                                                        03850352
40110 IF (RVON01 .GE. RVON02)  GO TO 40111                              03860352
      IVCOMP = 1                                                        03870352
40111 IF (RVON01 .LT. RVON02)  IVCOMP = IVCOMP + 2                      03880352
      IF (IVCOMP)  20110, 10110, 20110                                  03890352
30110 IVDELE = IVDELE + 1                                               03900352
      WRITE (I02,80000) IVTNUM                                          03910352
      IF (ICZERO) 10110, 0121, 20110                                    03920352
10110 IVPASS = IVPASS + 1                                               03930352
      WRITE (I02,80002) IVTNUM                                          03940352
      GO TO 0121                                                        03950352
20110 IVFAIL = IVFAIL + 1                                               03960352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03970352
 0121 CONTINUE                                                          03980352
C                                                                       03990352
C     ****  FCVS PROGRAM 352  -  TEST 012  ****                         04000352
C                                                                       04010352
C          TEST 12 CHECKS THE .GE. OPERATOR USING TWO REAL OPERANDS     04020352
C     WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    04030352
C                                                                       04040352
      IVTNUM =  12                                                      04050352
      IF (ICZERO) 30120, 0120, 30120                                    04060352
 0120 CONTINUE                                                          04070352
      RVON01 = 1.9999 E17                                               04080352
      RVON02 = 1.0001 E17                                               04090352
      IVCOMP = 0                                                        04100352
      IVCORR = 0                                                        04110352
40120 IF (RVON01 .GE. RVON02)  GO TO 40121                              04120352
      IVCOMP = 1                                                        04130352
40121 IF (RVON01 .LT. RVON02)  IVCOMP = IVCOMP + 2                      04140352
      IF (IVCOMP)  20120, 10120, 20120                                  04150352
30120 IVDELE = IVDELE + 1                                               04160352
      WRITE (I02,80000) IVTNUM                                          04170352
      IF (ICZERO) 10120, 0131, 20120                                    04180352
10120 IVPASS = IVPASS + 1                                               04190352
      WRITE (I02,80002) IVTNUM                                          04200352
      GO TO 0131                                                        04210352
20120 IVFAIL = IVFAIL + 1                                               04220352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04230352
 0131 CONTINUE                                                          04240352
C                                                                       04250352
C     ****  FCVS PROGRAM 352  -  TEST 013  ****                         04260352
C                                                                       04270352
C          TEST 13 CHECKS THE .GE. OPERATOR USING TWO REAL OPERANDS     04280352
C     WHERE EACH HAS BEEN ASSIGNED THE SAME REAL CONSTANT.              04290352
C                                                                       04300352
      IVTNUM =  13                                                      04310352
      IF (ICZERO) 30130, 0130, 30130                                    04320352
 0130 CONTINUE                                                          04330352
      RVON01 = 1.0001 E17                                               04340352
      RVON02 = 1.0001 E17                                               04350352
      IVCOMP = 0                                                        04360352
      IVCORR = 0                                                        04370352
40130 IF (RVON01 .GE. RVON02)  GO TO 40131                              04380352
      IVCOMP = 1                                                        04390352
40131 IF (RVON01 .LT. RVON02)  IVCOMP = IVCOMP + 2                      04400352
      IF (IVCOMP)  20130, 10130, 20130                                  04410352
30130 IVDELE = IVDELE + 1                                               04420352
      WRITE (I02,80000) IVTNUM                                          04430352
      IF (ICZERO) 10130, 0141, 20130                                    04440352
10130 IVPASS = IVPASS + 1                                               04450352
      WRITE (I02,80002) IVTNUM                                          04460352
      GO TO 0141                                                        04470352
20130 IVFAIL = IVFAIL + 1                                               04480352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04490352
 0141 CONTINUE                                                          04500352
C                                                                       04510352
C          TESTS 14 THROUGH 28 REPETITIVELY CHECK THE .LT. RELATIONSHIP 04520352
C     USING ALL TYPES AND ORDERINGS OF TWO REAL OPERANDS.               04530352
C                                                                       04540352
C                                                                       04550352
C          TESTS 14 THROUGH 16 CHECK REAL-VARIABLE .LT OTHER-REAL-TYPES.04560352
C                                                                       04570352
C                                                                       04580352
C     ****  FCVS PROGRAM 352  -  TEST 014  ****                         04590352
C                                                                       04600352
C          TEST 14 CHECKS REAL-VARIABLE .LT. REAL-CONSTANT              04610352
C                                                                       04620352
      IVTNUM =  14                                                      04630352
      IF (ICZERO) 30140, 0140, 30140                                    04640352
 0140 CONTINUE                                                          04650352
      RVON01 = 1.0001 E17                                               04660352
      IVCOMP = 0                                                        04670352
      IVCORR = 0                                                        04680352
40140 IF (RVON01 .LT. 1.9999 E17) GO TO 40141                           04690352
      IVCOMP = 1                                                        04700352
40141 IF (RVON01 .GE. 1.9999 E17)  IVCOMP = IVCOMP + 2                  04710352
      IF (IVCOMP)  20140, 10140, 20140                                  04720352
30140 IVDELE = IVDELE + 1                                               04730352
      WRITE (I02,80000) IVTNUM                                          04740352
      IF (ICZERO) 10140, 0151, 20140                                    04750352
10140 IVPASS = IVPASS + 1                                               04760352
      WRITE (I02,80002) IVTNUM                                          04770352
      GO TO 0151                                                        04780352
20140 IVFAIL = IVFAIL + 1                                               04790352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04800352
 0151 CONTINUE                                                          04810352
C                                                                       04820352
C     ****  FCVS PROGRAM 352  -  TEST 015  ****                         04830352
C                                                                       04840352
C          TEST 15 CHECKS REAL-VARIABLE .LT. ARRAY-ELEMENT              04850352
C                                                                       04860352
      IVTNUM =  15                                                      04870352
      IF (ICZERO) 30150, 0150, 30150                                    04880352
 0150 CONTINUE                                                          04890352
      RADN11(1) = 1.9999 E17                                            04900352
      RVON01 = 1.0001 E17                                               04910352
      IVCOMP = 0                                                        04920352
      IVCORR = 0                                                        04930352
40150 IF (RVON01 .LT. RADN11(1)) GO TO 40151                            04940352
      IVCOMP = 1                                                        04950352
40151 IF (RVON01 .GE. RADN11(1)) IVCOMP = IVCOMP + 2                    04960352
      IF (IVCOMP)  20150, 10150, 20150                                  04970352
30150 IVDELE = IVDELE + 1                                               04980352
      WRITE (I02,80000) IVTNUM                                          04990352
      IF (ICZERO) 10150, 0161, 20150                                    05000352
10150 IVPASS = IVPASS + 1                                               05010352
      WRITE (I02,80002) IVTNUM                                          05020352
      GO TO 0161                                                        05030352
20150 IVFAIL = IVFAIL + 1                                               05040352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05050352
 0161 CONTINUE                                                          05060352
C                                                                       05070352
C     ****  FCVS PROGRAM 352  -  TEST 016  ****                         05080352
C                                                                       05090352
C          TEST 16 CHECKS REAL-VARIABLE .LT. FUNCTION-REFERENCE         05100352
C                                                                       05110352
      IVTNUM =  16                                                      05120352
      IF (ICZERO) 30160, 0160, 30160                                    05130352
 0160 CONTINUE                                                          05140352
      RVON01 = 1.0001 E17                                               05150352
      RVON02 = 1 E17                                                    05160352
      RVON03 = 0.9999 E17                                               05170352
      IVCOMP = 0                                                        05180352
      IVCORR = 0                                                        05190352
40160 IF (RVON01 .LT. RFOS01(RVON02,RVON03)) GO TO 40161                05200352
      IVCOMP = 1                                                        05210352
40161 IF (RVON01 .GE. RFOS01(RVON02,RVON03)) IVCOMP = IVCOMP + 2        05220352
      IF (IVCOMP)  20160, 10160, 20160                                  05230352
30160 IVDELE = IVDELE + 1                                               05240352
      WRITE (I02,80000) IVTNUM                                          05250352
      IF (ICZERO) 10160, 0171, 20160                                    05260352
10160 IVPASS = IVPASS + 1                                               05270352
      WRITE (I02,80002) IVTNUM                                          05280352
      GO TO 0171                                                        05290352
20160 IVFAIL = IVFAIL + 1                                               05300352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05310352
 0171 CONTINUE                                                          05320352
C                                                                       05330352
C          TESTS 17 THROUGH 20 CHECK REAL-CONSTANT .LT. OTHER-REAL-TYPES05340352
C                                                                       05350352
C                                                                       05360352
C     ****  FCVS PROGRAM 352  -  TEST 017  ****                         05370352
C                                                                       05380352
C          TEST 17 CHECKS REAL-CONSTANT .LT. REAL-CONSTANT              05390352
C                                                                       05400352
      IVTNUM =  17                                                      05410352
      IF (ICZERO) 30170, 0170, 30170                                    05420352
 0170 CONTINUE                                                          05430352
      IVCOMP = 0                                                        05440352
      IVCORR = 0                                                        05450352
40170 IF (1.0001 E17 .LT. 1.9999 E17)  GO TO 40171                      05460352
      IVCOMP = 1                                                        05470352
40171 IF (1.0001 E17 .GE. 1.9999 E17)  IVCOMP = IVCOMP + 2              05480352
      IF (IVCOMP)  20170, 10170, 20170                                  05490352
30170 IVDELE = IVDELE + 1                                               05500352
      WRITE (I02,80000) IVTNUM                                          05510352
      IF (ICZERO) 10170, 0181, 20170                                    05520352
10170 IVPASS = IVPASS + 1                                               05530352
      WRITE (I02,80002) IVTNUM                                          05540352
      GO TO 0181                                                        05550352
20170 IVFAIL = IVFAIL + 1                                               05560352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05570352
 0181 CONTINUE                                                          05580352
C                                                                       05590352
C     ****  FCVS PROGRAM 352  -  TEST 018  ****                         05600352
C                                                                       05610352
C          TEST 18 CHECKS REAL-CONSTANT .LT. REAL-ARRAY-ELEMENT         05620352
C                                                                       05630352
      IVTNUM =  18                                                      05640352
      IF (ICZERO) 30180, 0180, 30180                                    05650352
 0180 CONTINUE                                                          05660352
      RADN11(1) = 1.9999 E17                                            05670352
      IVCOMP = 0                                                        05680352
      IVCORR = 0                                                        05690352
40180 IF (1.0001 E17 .LT. RADN11(1))  GO TO 40181                       05700352
      IVCOMP = 1                                                        05710352
40181 IF (1.0001 E17 .GE. RADN11(1))  IVCOMP = IVCOMP + 2               05720352
      IF (IVCOMP)  20180, 10180, 20180                                  05730352
30180 IVDELE = IVDELE + 1                                               05740352
      WRITE (I02,80000) IVTNUM                                          05750352
      IF (ICZERO) 10180, 0191, 20180                                    05760352
10180 IVPASS = IVPASS + 1                                               05770352
      WRITE (I02,80002) IVTNUM                                          05780352
      GO TO 0191                                                        05790352
20180 IVFAIL = IVFAIL + 1                                               05800352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05810352
 0191 CONTINUE                                                          05820352
C                                                                       05830352
C     ****  FCVS PROGRAM 352  -  TEST 019  ****                         05840352
C                                                                       05850352
C          TEST 19 CHECKS REAL-CONSTANT .LT. REAL-VARIABLE              05860352
C                                                                       05870352
      IVTNUM =  19                                                      05880352
      IF (ICZERO) 30190, 0190, 30190                                    05890352
 0190 CONTINUE                                                          05900352
      RVON01 = 1.9999 E17                                               05910352
      IVCOMP = 0                                                        05920352
      IVCORR = 0                                                        05930352
40190 IF (1.0001 E17 .LT. RVON01)  GO TO 40191                          05940352
      IVCOMP = 1                                                        05950352
40191 IF (1.0001 E17 .GE. RVON01)  IVCOMP = IVCOMP + 2                  05960352
      IF (IVCOMP) 20190, 10190, 20190                                   05970352
30190 IVDELE = IVDELE + 1                                               05980352
      WRITE (I02,80000) IVTNUM                                          05990352
      IF (ICZERO) 10190, 0201, 20190                                    06000352
10190 IVPASS = IVPASS + 1                                               06010352
      WRITE (I02,80002) IVTNUM                                          06020352
      GO TO 0201                                                        06030352
20190 IVFAIL = IVFAIL + 1                                               06040352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06050352
 0201 CONTINUE                                                          06060352
C                                                                       06070352
C     ****  FCVS PROGRAM 352  -  TEST 020  ****                         06080352
C                                                                       06090352
C          TEST 20 CHECKS REAL-CONSTANT .LT. REAL-FUNCTION-REFERENCE    06100352
C                                                                       06110352
      IVTNUM =  20                                                      06120352
      IF (ICZERO) 30200, 0200, 30200                                    06130352
 0200 CONTINUE                                                          06140352
      RVON01 = 1 E17                                                    06150352
      RVON02 = 0.9999 E17                                               06160352
      IVCOMP = 0                                                        06170352
      IVCORR = 0                                                        06180352
40200 IF (1.0001 E17 .LT. RFOS01(RVON01,RVON02))  GO TO 40201           06190352
      IVCOMP = 1                                                        06200352
40201 IF (1.0001 E17 .GE. RFOS01(RVON01,RVON02))  IVCOMP = IVCOMP + 2   06210352
      IF (IVCOMP)  20200, 10200, 20200                                  06220352
30200 IVDELE = IVDELE + 1                                               06230352
      WRITE (I02,80000) IVTNUM                                          06240352
      IF (ICZERO) 10200, 0211, 20200                                    06250352
10200 IVPASS = IVPASS + 1                                               06260352
      WRITE (I02,80002) IVTNUM                                          06270352
      GO TO 0211                                                        06280352
20200 IVFAIL = IVFAIL + 1                                               06290352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06300352
 0211 CONTINUE                                                          06310352
C                                                                       06320352
C          TESTS 21 THROUGH 24 CHECK REAL-ARRAY-ELEMENT .LT. OTHER-REALS06330352
C                                                                       06340352
C                                                                       06350352
C     ****  FCVS PROGRAM 352  -  TEST 021  ****                         06360352
C                                                                       06370352
C          TEST 21 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-CONSTANT         06380352
C                                                                       06390352
      IVTNUM =  21                                                      06400352
      IF (ICZERO) 30210, 0210, 30210                                    06410352
 0210 CONTINUE                                                          06420352
      RADN11(1) = 1.0001 E17                                            06430352
      IVCOMP = 0                                                        06440352
      IVCORR = 0                                                        06450352
40210 IF (RADN11(1) .LT. 1.9999 E17)  GO TO 40211                       06460352
      IVCOMP = 1                                                        06470352
40211 IF (RADN11(1) .GE. 1.9999 E17)  IVCOMP = IVCOMP + 2               06480352
      IF (IVCOMP)  20210, 10210, 20210                                  06490352
30210 IVDELE = IVDELE + 1                                               06500352
      WRITE (I02,80000) IVTNUM                                          06510352
      IF (ICZERO) 10210, 0221, 20210                                    06520352
10210 IVPASS = IVPASS + 1                                               06530352
      WRITE (I02,80002) IVTNUM                                          06540352
      GO TO 0221                                                        06550352
20210 IVFAIL = IVFAIL + 1                                               06560352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06570352
 0221 CONTINUE                                                          06580352
C                                                                       06590352
C     ****  FCVS PROGRAM 352  -  TEST 022  ****                         06600352
C                                                                       06610352
C          TEST 22 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-ARRAY-ELEMENT    06620352
C                                                                       06630352
      IVTNUM =  22                                                      06640352
      IF (ICZERO) 30220, 0220, 30220                                    06650352
 0220 CONTINUE                                                          06660352
      RADN11(1) = 1.0001 E17                                            06670352
      RADN11(2) = 1.9999 E17                                            06680352
      IVCOMP = 0                                                        06690352
      IVCORR = 0                                                        06700352
40220 IF (RADN11(1) .LT. RADN11(2))  GO TO 40221                        06710352
      IVCOMP = 1                                                        06720352
40221 IF (RADN11(1) .GE. RADN11(2))  IVCOMP = IVCOMP + 2                06730352
      IF (IVCOMP)  20220, 10220, 20220                                  06740352
30220 IVDELE = IVDELE + 1                                               06750352
      WRITE (I02,80000) IVTNUM                                          06760352
      IF (ICZERO) 10220, 0231, 20220                                    06770352
10220 IVPASS = IVPASS + 1                                               06780352
      WRITE (I02,80002) IVTNUM                                          06790352
      GO TO 0231                                                        06800352
20220 IVFAIL = IVFAIL + 1                                               06810352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06820352
 0231 CONTINUE                                                          06830352
C                                                                       06840352
C     ****  FCVS PROGRAM 352  -  TEST 023  ****                         06850352
C                                                                       06860352
C          TEST 23 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-VARIABLE         06870352
C                                                                       06880352
      IVTNUM =  23                                                      06890352
      IF (ICZERO) 30230, 0230, 30230                                    06900352
 0230 CONTINUE                                                          06910352
      RVON01 = 1.9999 E17                                               06920352
      RADN11(1) = 1.0001 E17                                            06930352
      IVCORR = 0                                                        06940352
      IVCOMP = 0                                                        06950352
40230 IF (RADN11(1) .LT. RVON01)  GO TO 40231                           06960352
      IVCOMP = 1                                                        06970352
40231 IF (RADN11(1) .GE. RVON01)  IVCOMP = IVCOMP + 2                   06980352
      IF (IVCOMP)  20230, 10230, 20230                                  06990352
30230 IVDELE = IVDELE + 1                                               07000352
      WRITE (I02,80000) IVTNUM                                          07010352
      IF (ICZERO) 10230, 0241, 20230                                    07020352
10230 IVPASS = IVPASS + 1                                               07030352
      WRITE (I02,80002) IVTNUM                                          07040352
      GO TO 0241                                                        07050352
20230 IVFAIL = IVFAIL + 1                                               07060352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07070352
 0241 CONTINUE                                                          07080352
C                                                                       07090352
C     ****  FCVS PROGRAM 352  -  TEST 024  ****                         07100352
C                                                                       07110352
C          TEST 24 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-FUNCTION-REF.    07120352
C                                                                       07130352
      IVTNUM =  24                                                      07140352
      IF (ICZERO) 30240, 0240, 30240                                    07150352
 0240 CONTINUE                                                          07160352
      RVON01 = 1.0000 E17                                               07170352
      RVON02 = 0.9999 E17                                               07180352
      RADN11(1) = 1.0001 E17                                            07190352
      IVCORR = 0                                                        07200352
      IVCOMP = 0                                                        07210352
40240 IF (RADN11(1) .LT. RFOS01(RVON01,RVON02))  GO TO 40241            07220352
      IVCOMP = 1                                                        07230352
40241 IF (RADN11(1) .GE. RFOS01(RVON01,RVON02))  IVCOMP = IVCOMP + 2    07240352
      IF (IVCOMP)  20240, 10240, 20240                                  07250352
30240 IVDELE = IVDELE + 1                                               07260352
      WRITE (I02,80000) IVTNUM                                          07270352
      IF (ICZERO) 10240, 0251, 20240                                    07280352
10240 IVPASS = IVPASS + 1                                               07290352
      WRITE (I02,80002) IVTNUM                                          07300352
      GO TO 0251                                                        07310352
20240 IVFAIL = IVFAIL + 1                                               07320352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07330352
 0251 CONTINUE                                                          07340352
C                                                                       07350352
C          TESTS 25 THROUGH 28 CHECK REAL-FUNCTION-REFERENCE .LT.       07360352
C                                    OTHER-REAL-TYPES                   07370352
C                                                                       07380352
C                                                                       07390352
C     ****  FCVS PROGRAM 352  -  TEST 025  ****                         07400352
C                                                                       07410352
C          TEST 25 CHECKS REAL-FUNCTION-REFERENCE .LT. REAL-CONSTANT    07420352
C                                                                       07430352
      IVTNUM =  25                                                      07440352
      IF (ICZERO) 30250, 0250, 30250                                    07450352
 0250 CONTINUE                                                          07460352
      RVON01 = 1.0000 E17                                               07470352
      RVON02 = 0.0001 E17                                               07480352
      IVCOMP = 0                                                        07490352
      IVCORR = 0                                                        07500352
40250 IF (RFOS01(RVON01,RVON02) .LT. 1.9999 E17)  GO TO 40251           07510352
      IVCOMP = 1                                                        07520352
40251 IF (RFOS01(RVON01,RVON02) .GE. 1.9999 E17)  IVCOMP = IVCOMP + 2   07530352
      IF (IVCOMP)  20250, 10250, 20250                                  07540352
30250 IVDELE = IVDELE + 1                                               07550352
      WRITE (I02,80000) IVTNUM                                          07560352
      IF (ICZERO) 10250, 0261, 20250                                    07570352
10250 IVPASS = IVPASS + 1                                               07580352
      WRITE (I02,80002) IVTNUM                                          07590352
      GO TO 0261                                                        07600352
20250 IVFAIL = IVFAIL + 1                                               07610352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07620352
 0261 CONTINUE                                                          07630352
C                                                                       07640352
C     ****  FCVS PROGRAM 352  -  TEST 026  ****                         07650352
C                                                                       07660352
C          TEST 26 CHECKS REAL-FUNCTION-REFERENCE .LT. REAL-ARRAY-ELEMNT07670352
C                                                                       07680352
      IVTNUM =  26                                                      07690352
      IF (ICZERO) 30260, 0260, 30260                                    07700352
 0260 CONTINUE                                                          07710352
      RVON01 = 1 E17                                                    07720352
      RVON02 = 0.0001 E17                                               07730352
      RADN11(1) = 1.9999 E17                                            07740352
      IVCOMP = 0                                                        07750352
      IVCORR = 0                                                        07760352
40260 IF (RFOS01(RVON01,RVON02) .LT. RADN11(1))  GO TO 40261            07770352
      IVCOMP = 1                                                        07780352
40261 IF (RFOS01(RVON01,RVON02) .GE. RADN11(1))  IVCOMP = IVCOMP + 2    07790352
      IF (IVCOMP)  20260, 10260, 20260                                  07800352
30260 IVDELE = IVDELE + 1                                               07810352
      WRITE (I02,80000) IVTNUM                                          07820352
      IF (ICZERO) 10260, 0271, 20260                                    07830352
10260 IVPASS = IVPASS + 1                                               07840352
      WRITE (I02,80002) IVTNUM                                          07850352
      GO TO 0271                                                        07860352
20260 IVFAIL = IVFAIL + 1                                               07870352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07880352
 0271 CONTINUE                                                          07890352
C                                                                       07900352
C     ****  FCVS PROGRAM 352  -  TEST 027  ****                         07910352
C                                                                       07920352
C          TEST 27 CHECKS REAL-FUNCTION-REFERENCE .LT. REAL-VARIABLE    07930352
C                                                                       07940352
      IVTNUM =  27                                                      07950352
      IF (ICZERO) 30270, 0270, 30270                                    07960352
 0270 CONTINUE                                                          07970352
      RVON01 = 1 E17                                                    07980352
      RVON02 = 0.0001 E17                                               07990352
      RVON03 = 1.9999 E17                                               08000352
      IVCOMP = 0                                                        08010352
      IVCORR = 0                                                        08020352
40270 IF (RFOS01(RVON01,RVON02) .LT. RVON03)  GO TO 40271               08030352
      IVCOMP = 1                                                        08040352
40271 IF (RFOS01(RVON01,RVON02) .GE. RVON03)  IVCOMP = IVCOMP + 2       08050352
      IF (IVCOMP)  20270, 10270, 20270                                  08060352
30270 IVDELE = IVDELE + 1                                               08070352
      WRITE (I02,80000) IVTNUM                                          08080352
      IF (ICZERO) 10270, 0281, 20270                                    08090352
10270 IVPASS = IVPASS + 1                                               08100352
      WRITE (I02,80002) IVTNUM                                          08110352
      GO TO 0281                                                        08120352
20270 IVFAIL = IVFAIL + 1                                               08130352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08140352
 0281 CONTINUE                                                          08150352
C                                                                       08160352
C     ****  FCVS PROGRAM 352  -  TEST 028  ****                         08170352
C                                                                       08180352
C          TEST 28 CHECKS REAL-FUNCTION-REFERENCE .LT REAL-FUNCTION-REF.08190352
C                                                                       08200352
      IVTNUM =  28                                                      08210352
      IF (ICZERO) 30280, 0280, 30280                                    08220352
 0280 CONTINUE                                                          08230352
      RVON01 = 1 E17                                                    08240352
      RVON02 = 0.0001 E17                                               08250352
      RVON03 = 0.9999 E17                                               08260352
      IVCOMP = 0                                                        08270352
      IVCORR = 0                                                        08280352
40280 IF (RFOS01(RVON01,RVON02) .LT. RFOS01(RVON01,RVON03)) GO TO 40281 08290352
      IVCOMP = 1                                                        08300352
40281 IF (RFOS01(RVON01,RVON02) .GE. RFOS01(RVON01,RVON03))             08310352
     1         IVCOMP = IVCOMP + 2                                      08320352
      IF (IVCOMP)  20280, 10280, 20280                                  08330352
30280 IVDELE = IVDELE + 1                                               08340352
      WRITE (I02,80000) IVTNUM                                          08350352
      IF (ICZERO) 10280, 0291, 20280                                    08360352
10280 IVPASS = IVPASS + 1                                               08370352
      WRITE (I02,80002) IVTNUM                                          08380352
      GO TO 0291                                                        08390352
20280 IVFAIL = IVFAIL + 1                                               08400352
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08410352
 0291 CONTINUE                                                          08420352
C                                                                       08430352
C                                                                       08440352
C     WRITE OUT TEST SUMMARY                                            08450352
C                                                                       08460352
      WRITE (I02,90004)                                                 08470352
      WRITE (I02,90014)                                                 08480352
      WRITE (I02,90004)                                                 08490352
      WRITE (I02,90000)                                                 08500352
      WRITE (I02,90004)                                                 08510352
      WRITE (I02,90020) IVFAIL                                          08520352
      WRITE (I02,90022) IVPASS                                          08530352
      WRITE (I02,90024) IVDELE                                          08540352
      STOP                                                              08550352
90001 FORMAT (1H ,24X,5HFM352)                                          08560352
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM352)                          08570352
C                                                                       08580352
C     FORMATS FOR TEST DETAIL LINES                                     08590352
C                                                                       08600352
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   08610352
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08620352
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08630352
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08640352
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        08650352
C                                                                       08660352
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08670352
C                                                                       08680352
90002 FORMAT (1H1)                                                      08690352
90004 FORMAT (1H )                                                      08700352
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08710352
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   08720352
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         08730352
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  08740352
90014 FORMAT (1H ,5X,46H----------------------------------------------) 08750352
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08760352
C                                                                       08770352
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 08780352
C                                                                       08790352
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              08800352
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              08810352
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             08820352
      END                                                               08830352
