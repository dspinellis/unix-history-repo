      PROGRAM FM306                                                     00010306
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020306
C                                                                       00030306
C          THIS ROUTINE TESTS THE USE OF THE SUBSET LEVEL FEATURES OF   00040306
C     THE IMPLICIT SPECIFICATION STATEMENT.  THE DEFAULT IMPLIED INTEGER00050306
C     AND REAL TYPING IS EITHER CONFIRMED OR OVERRIDDEN TO SPECIFY      00060306
C     INTEGER, REAL AND LOGICAL TYPING.  ALL 26 ALPHABETIC LETTERS ARE  00070306
C     USED TO INDICATE THE IMPLICIT TYPING.  VARIABLE AND ARRAY         00080306
C     ENTITIES ARE USED TO TEST THE ACTUAL TYPING.  THE SUBSET LEVEL    00090306
C     FEATURES OF THE IMPLICIT STATEMENT ARE ALSO TESTED IN ROUTINES    00100306
C     FM201 AND FM251.                                                  00110306
C                                                                       00120306
C     REFERENCES.                                                       00130306
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140306
C           X3.9-1978                                                   00150306
C                                                                       00160306
C        SECTION 4.1.2, TYPE RULES FOR DATA AND PROCEDURE IDENTIFIERS.  00170306
C        SECTION 8.5,   IMPLICIT STATEMENT                              00180306
C                                                                       00190306
C                                                                       00200306
C     ******************************************************************00210306
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00220306
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00230306
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00240306
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00250306
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00260306
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00270306
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00280306
C     THE RESULT OF EXECUTING THESE TESTS.                              00290306
C                                                                       00300306
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00310306
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00320306
C                                                                       00330306
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00340306
C                    DEPARTMENT OF THE NAVY                             00350306
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00360306
C                    WASHINGTON, D.C.   20376                           00370306
C                                                                       00380306
C     ******************************************************************00390306
C                                                                       00400306
C                                                                       00410306
      IMPLICIT LOGICAL (L)                                              00420306
      IMPLICIT CHARACTER*14 (C)                                         00430306
C                                                                       00440306
      IMPLICIT INTEGER (A)                                              00450306
      IMPLICIT LOGICAL (B)                                              00460306
      IMPLICIT INTEGER (D,E,F)                                          00470306
      IMPLICIT REAL (G-H)                                               00480306
      IMPLICIT INTEGER (I)                                              00490306
      IMPLICIT REAL (J)                                                 00500306
      IMPLICIT INTEGER (K,O-Q)                                          00510306
      IMPLICIT REAL (M), REAL (N)                                       00520306
      IMPLICIT REAL (R)                                                 00530306
      IMPLICIT REAL (S), INTEGER (T-V)                                  00540306
      IMPLICIT INTEGER (W), REAL (X), LOGICAL (Y), INTEGER (Z)          00550306
      DIMENSION AAIN11(5)                                               00560306
      DIMENSION HAIN11(5)                                               00570306
C                                                                       00580306
C                                                                       00590306
C                                                                       00600306
C     INITIALIZATION SECTION.                                           00610306
C                                                                       00620306
C     INITIALIZE CONSTANTS                                              00630306
C     ********************                                              00640306
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00650306
      I01 = 5                                                           00660306
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00670306
      I02 = 6                                                           00680306
C     SYSTEM ENVIRONMENT SECTION                                        00690306
C                                                                       00700306
      I01 = 5                                                           00710306
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720306
C     (UNIT NUMBER FOR CARD READER).                                    00730306
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00740306
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00750306
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00760306
C                                                                       00770306
      I02 = 6                                                           00780306
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00790306
C     (UNIT NUMBER FOR PRINTER).                                        00800306
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00810306
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00820306
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00830306
C                                                                       00840306
      IVPASS = 0                                                        00850306
      IVFAIL = 0                                                        00860306
      IVDELE = 0                                                        00870306
      ICZERO = 0                                                        00880306
C                                                                       00890306
C     WRITE OUT PAGE HEADERS                                            00900306
C                                                                       00910306
      WRITE (I02,90002)                                                 00920306
      WRITE (I02,90006)                                                 00930306
      WRITE (I02,90008)                                                 00940306
      WRITE (I02,90004)                                                 00950306
      WRITE (I02,90010)                                                 00960306
      WRITE (I02,90004)                                                 00970306
      WRITE (I02,90016)                                                 00980306
      WRITE (I02,90001)                                                 00990306
      WRITE (I02,90004)                                                 01000306
      WRITE (I02,90012)                                                 01010306
      WRITE (I02,90014)                                                 01020306
      WRITE (I02,90004)                                                 01030306
C                                                                       01040306
C                                                                       01050306
C     ****  FCVS PROGRAM 306  -  TEST 001  ****                         01060306
C                                                                       01070306
C     TEST 001 IS DESIGNED TO CONFIRM IMPLICIT INTEGER TYPING.          01080306
C                                                                       01090306
      IVTNUM =   1                                                      01100306
      IF (ICZERO) 30010, 0010, 30010                                    01110306
 0010 CONTINUE                                                          01120306
      RVCOMP = 10.0                                                     01130306
      IVIN01 = 4                                                        01140306
      RVCOMP = IVIN01 / 5                                               01150306
      RVCORR = 0.0                                                      01160306
40010 IF (RVCOMP) 20010, 10010, 20010                                   01170306
30010 IVDELE = IVDELE + 1                                               01180306
      WRITE (I02,80000) IVTNUM                                          01190306
      IF (ICZERO) 10010, 0021, 20010                                    01200306
10010 IVPASS = IVPASS + 1                                               01210306
      WRITE (I02,80002) IVTNUM                                          01220306
      GO TO 0021                                                        01230306
20010 IVFAIL = IVFAIL + 1                                               01240306
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01250306
 0021 CONTINUE                                                          01260306
C                                                                       01270306
C     ****  FCVS PROGRAM 306  -  TEST 002  ****                         01280306
C                                                                       01290306
C     TEST 002 IS DESIGNED TO CONFIRM IMPLICIT REAL TYPING.             01300306
C                                                                       01310306
      IVTNUM =   2                                                      01320306
      IF (ICZERO) 30020, 0020, 30020                                    01330306
 0020 CONTINUE                                                          01340306
      RVCOMP = 10.0                                                     01350306
      RVIN01 = 4                                                        01360306
      RVCOMP = RVIN01/5                                                 01370306
      RVCORR = .8                                                       01380306
40020 IF (RVCOMP - .79995) 20020, 10020, 40021                          01390306
40021 IF (RVCOMP - .80005) 10020, 10020, 20020                          01400306
30020 IVDELE = IVDELE + 1                                               01410306
      WRITE (I02,80000) IVTNUM                                          01420306
      IF (ICZERO) 10020, 0031, 20020                                    01430306
10020 IVPASS = IVPASS + 1                                               01440306
      WRITE (I02,80002) IVTNUM                                          01450306
      GO TO 0031                                                        01460306
20020 IVFAIL = IVFAIL + 1                                               01470306
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01480306
 0031 CONTINUE                                                          01490306
C                                                                       01500306
C     ****  FCVS PROGRAM 306  -  TEST 003  ****                         01510306
C                                                                       01520306
C     TEST 003 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF       01530306
C     INTEGER WITH IMPLICIT TYPING OF REAL.                             01540306
C                                                                       01550306
      IVTNUM =   3                                                      01560306
      IF (ICZERO) 30030, 0030, 30030                                    01570306
 0030 CONTINUE                                                          01580306
      RVCOMP = 10.0                                                     01590306
      JVIN01 = 4                                                        01600306
      RVCOMP = JVIN01/5                                                 01610306
      RVCORR = .8                                                       01620306
40030 IF (RVCOMP - .79995) 20030, 10030, 40031                          01630306
40031 IF (RVCOMP - .80005) 10030, 10030, 20030                          01640306
30030 IVDELE = IVDELE + 1                                               01650306
      WRITE (I02,80000) IVTNUM                                          01660306
      IF (ICZERO) 10030, 0041, 20030                                    01670306
10030 IVPASS = IVPASS + 1                                               01680306
      WRITE (I02,80002) IVTNUM                                          01690306
      GO TO 0041                                                        01700306
20030 IVFAIL = IVFAIL + 1                                               01710306
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01720306
 0041 CONTINUE                                                          01730306
C                                                                       01740306
C     ****  FCVS PROGRAM 306  -  TEST 004  ****                         01750306
C                                                                       01760306
C     TEST 004 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF       01770306
C     INTEGER WITH IMPLICIT TYPING OF LOGICAL.                          01780306
C                                                                       01790306
      IVTNUM =   4                                                      01800306
      IF (ICZERO) 30040, 0040, 30040                                    01810306
 0040 CONTINUE                                                          01820306
      LVIN01 = .TRUE.                                                   01830306
      IVCORR = 1                                                        01840306
      IVCOMP = 0                                                        01850306
      IF (LVIN01) IVCOMP = 1                                            01860306
40040 IF (IVCOMP - 1) 20040, 10040, 20040                               01870306
30040 IVDELE = IVDELE + 1                                               01880306
      WRITE (I02,80000) IVTNUM                                          01890306
      IF (ICZERO) 10040, 0051, 20040                                    01900306
10040 IVPASS = IVPASS + 1                                               01910306
      WRITE (I02,80002) IVTNUM                                          01920306
      GO TO 0051                                                        01930306
20040 IVFAIL = IVFAIL + 1                                               01940306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01950306
 0051 CONTINUE                                                          01960306
C                                                                       01970306
C     ****  FCVS PROGRAM 306  -  TEST 005  ****                         01980306
C                                                                       01990306
C     TEST 005 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF       02000306
C     REAL WITH IMPLICIT TYPING OF INTEGER.                             02010306
C                                                                       02020306
      IVTNUM =   5                                                      02030306
      IF (ICZERO) 30050, 0050, 30050                                    02040306
 0050 CONTINUE                                                          02050306
      RVCOMP = 10.0                                                     02060306
      AAIN11(2) = 4                                                     02070306
      RVCOMP = AAIN11(2)/5                                              02080306
      RVCORR = 0.0                                                      02090306
40050 IF (RVCOMP) 20050, 10050, 20050                                   02100306
30050 IVDELE = IVDELE + 1                                               02110306
      WRITE (I02,80000) IVTNUM                                          02120306
      IF (ICZERO) 10050, 0061, 20050                                    02130306
10050 IVPASS = IVPASS + 1                                               02140306
      WRITE (I02,80002) IVTNUM                                          02150306
      GO TO 0061                                                        02160306
20050 IVFAIL = IVFAIL + 1                                               02170306
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02180306
 0061 CONTINUE                                                          02190306
C                                                                       02200306
C     ****  FCVS PROGRAM 306  -  TEST 006  ****                         02210306
C                                                                       02220306
C     TEST 006 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF REAL  02230306
C     WITH IMPLICIT TYPING OF LOGICAL.                                  02240306
C                                                                       02250306
      IVTNUM =   6                                                      02260306
      IF (ICZERO) 30060, 0060, 30060                                    02270306
 0060 CONTINUE                                                          02280306
      BVIN01 = .TRUE.                                                   02290306
      IVCORR = 1                                                        02300306
      IVCOMP = 0                                                        02310306
      IF (BVIN01) IVCOMP = 1                                            02320306
40060 IF (IVCOMP - 1) 20060, 10060, 20060                               02330306
30060 IVDELE = IVDELE + 1                                               02340306
      WRITE (I02,80000) IVTNUM                                          02350306
      IF (ICZERO) 10060, 0071, 20060                                    02360306
10060 IVPASS = IVPASS + 1                                               02370306
      WRITE (I02,80002) IVTNUM                                          02380306
      GO TO 0071                                                        02390306
20060 IVFAIL = IVFAIL + 1                                               02400306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02410306
 0071 CONTINUE                                                          02420306
C                                                                       02430306
C     TESTS 007 THROUGH 012 ARE DESIGNED TO TEST VARIOUS SYNTACTICAL    02440306
C     CONSTRUCTS OF THE IMPLICIT STATEMENT.                             02450306
C                                                                       02460306
C                                                                       02470306
C     ****  FCVS PROGRAM 306  -  TEST 007  ****                         02480306
C                                                                       02490306
C     TEST 007 TESTS THE SPECIFYING OF MORE THAN ONE ALPHABETIC         02500306
C     CHARACTER IN AN IMPLICIT STATEMENT.                               02510306
C                                                                       02520306
      IVTNUM =   7                                                      02530306
      IF (ICZERO) 30070, 0070, 30070                                    02540306
 0070 CONTINUE                                                          02550306
      DVIN01 = 4                                                        02560306
      EVIN01 = 4                                                        02570306
      FVIN01 = 4                                                        02580306
      RVCMP1 = DVIN01/5                                                 02590306
      RVCMP2 = EVIN01/5                                                 02600306
      RVCMP3 = FVIN01/5                                                 02610306
      IVCOMP = 1                                                        02620306
      IF (RVCMP1 .EQ. 0.0) IVCOMP = IVCOMP * 2                          02630306
      IF (RVCMP2 .EQ. 0.0) IVCOMP = IVCOMP * 3                          02640306
      IF (RVCMP3 .EQ. 0.0) IVCOMP = IVCOMP * 5                          02650306
      IVCORR = 30                                                       02660306
C     30 = 2 * 3 * 5                                                    02670306
40070 IF (IVCOMP -    30) 20070, 10070, 20070                           02680306
30070 IVDELE = IVDELE + 1                                               02690306
      WRITE (I02,80000) IVTNUM                                          02700306
      IF (ICZERO) 10070, 0081, 20070                                    02710306
10070 IVPASS = IVPASS + 1                                               02720306
      WRITE (I02,80002) IVTNUM                                          02730306
      GO TO 0081                                                        02740306
20070 IVFAIL = IVFAIL + 1                                               02750306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02760306
 0081 CONTINUE                                                          02770306
C                                                                       02780306
C     ****  FCVS PROGRAM 306  -  TEST 008  ****                         02790306
C                                                                       02800306
C     TEST 008 TESTS THE SPECIFYING A RANGE OF SINGLE LETTERS IN        02810306
C     ALPHABETIC ORDER IN AN IMPLICIT STATEMENT.                        02820306
C                                                                       02830306
      IVTNUM =   8                                                      02840306
      IF (ICZERO) 30080, 0080, 30080                                    02850306
 0080 CONTINUE                                                          02860306
      GVIN01 = 4                                                        02870306
      HAIN11(4) = 4                                                     02880306
      RVCMP1 = GVIN01/5                                                 02890306
      RVCMP2 = HAIN11(4)/5                                              02900306
      IVCOMP = 1                                                        02910306
      IF (RVCMP1 .GE. .79995 .AND. RVCMP1 .LE. .80005) IVCOMP=IVCOMP*2  02920306
      IF (RVCMP2 .GE. .79995 .AND. RVCMP2 .LE. .80005) IVCOMP=IVCOMP*3  02930306
      IVCORR = 6                                                        02940306
C     6 = 2 * 3                                                         02950306
40080 IF (IVCOMP - 6) 20080, 10080, 20080                               02960306
30080 IVDELE = IVDELE + 1                                               02970306
      WRITE (I02,80000) IVTNUM                                          02980306
      IF (ICZERO) 10080, 0091, 20080                                    02990306
10080 IVPASS = IVPASS + 1                                               03000306
      WRITE (I02,80002) IVTNUM                                          03010306
      GO TO 0091                                                        03020306
20080 IVFAIL = IVFAIL + 1                                               03030306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03040306
 0091 CONTINUE                                                          03050306
C                                                                       03060306
C     ****  FCVS PROGRAM 306  -  TEST 009  ****                         03070306
C                                                                       03080306
C     TEST 009 TESTS THE SPECIFYING A SINGLE LETTER AND A RANGE OF      03090306
C     SINGLE LETTERS IN ALPHABETIC ORDER IN AN IMPLICIT STATEMENT.      03100306
C                                                                       03110306
      IVTNUM =   9                                                      03120306
      IF (ICZERO) 30090, 0090, 30090                                    03130306
 0090 CONTINUE                                                          03140306
      KVIN01 = 4                                                        03150306
      OVIN01 = 4                                                        03160306
      PVIN01 = 4                                                        03170306
      QVIN01 = 4                                                        03180306
      RVCMP1 = KVIN01/5                                                 03190306
      RVCMP2 = OVIN01/5                                                 03200306
      RVCMP3 = PVIN01/5                                                 03210306
      RVCMP4 = QVIN01/5                                                 03220306
      IVCOMP = 1                                                        03230306
      IF (RVCMP1 .EQ. 0.0) IVCOMP = IVCOMP * 2                          03240306
      IF (RVCMP2 .EQ. 0.0) IVCOMP = IVCOMP * 3                          03250306
      IF (RVCMP3 .EQ. 0.0) IVCOMP = IVCOMP * 5                          03260306
      IF (RVCMP4 .EQ. 0.0) IVCOMP = IVCOMP * 7                          03270306
      IVCORR = 210                                                      03280306
C     210 = 2 * 3 * 5 * 7                                               03290306
40090 IF (IVCOMP - 210) 20090, 10090, 20090                             03300306
30090 IVDELE = IVDELE + 1                                               03310306
      WRITE (I02,80000) IVTNUM                                          03320306
      IF (ICZERO) 10090, 0101, 20090                                    03330306
10090 IVPASS = IVPASS + 1                                               03340306
      WRITE (I02,80002) IVTNUM                                          03350306
      GO TO 0101                                                        03360306
20090 IVFAIL = IVFAIL + 1                                               03370306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03380306
 0101 CONTINUE                                                          03390306
C                                                                       03400306
C     ****  FCVS PROGRAM 306  -  TEST 010  ****                         03410306
C                                                                       03420306
C     TEST 010 TESTS THE SPECIFYING OF MORE THAN ONE TYPING IN ONE      03430306
C     IMPLICIT STATEMENT.                                               03440306
C                                                                       03450306
      IVTNUM =  10                                                      03460306
      IF (ICZERO) 30100, 0100, 30100                                    03470306
 0100 CONTINUE                                                          03480306
      SVIN01 = 4                                                        03490306
      TVIN01 = 4                                                        03500306
      UVIN01 = 4                                                        03510306
      VVIN01 = 4                                                        03520306
      RVCMP1 = SVIN01/5                                                 03530306
      RVCMP2 = TVIN01/5                                                 03540306
      RVCMP3 = UVIN01/5                                                 03550306
      RVCMP4 = VVIN01/5                                                 03560306
      IVCOMP = 1                                                        03570306
      IF (RVCMP1 .GE. .79995 .AND. RVCMP1 .LE. .80005) IVCOMP=IVCOMP*2  03580306
      IF (RVCMP2 .EQ. 0.0) IVCOMP = IVCOMP * 3                          03590306
      IF (RVCMP3 .EQ. 0.0) IVCOMP = IVCOMP * 5                          03600306
      IF (RVCMP4 .EQ. 0.0) IVCOMP = IVCOMP * 7                          03610306
      IVCORR = 210                                                      03620306
C     210 = 2 * 3 * 5 * 7                                               03630306
      IF (IVCOMP - 210) 20100, 10100, 20100                             03640306
30100 IVDELE = IVDELE + 1                                               03650306
      WRITE (I02,80000) IVTNUM                                          03660306
      IF (ICZERO) 10100, 0111, 20100                                    03670306
10100 IVPASS = IVPASS + 1                                               03680306
      WRITE (I02,80002) IVTNUM                                          03690306
      GO TO 0111                                                        03700306
20100 IVFAIL = IVFAIL + 1                                               03710306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03720306
 0111 CONTINUE                                                          03730306
C                                                                       03740306
C     ****  FCVS PROGRAM 306  -  TEST 011  ****                         03750306
C                                                                       03760306
C     TEST 011 TESTS THE SPECIFYING OF INTEGER, REAL, AND LOGICAL       03770306
C     TYPING IN ONE IMPLICIT STATEMENT.  IN THIS TEST INTEGER TYPING    03780306
C     IS REPEATED A SECOND TIME.                                        03790306
C                                                                       03800306
      IVTNUM =  11                                                      03810306
      IF (ICZERO) 30110, 0110, 30110                                    03820306
 0110 CONTINUE                                                          03830306
      WVIN01 = 4                                                        03840306
      XVIN01 = 4                                                        03850306
      YVIN01 = .TRUE.                                                   03860306
      ZVIN01 = 4                                                        03870306
      RVCMP1 = WVIN01/5                                                 03880306
      RVCMP2 = XVIN01/5                                                 03890306
      LVCOMP = YVIN01                                                   03900306
      RVCMP3 = ZVIN01/5                                                 03910306
      IVCOMP = 1                                                        03920306
      IF (RVCMP1 .EQ. 0.0) IVCOMP = IVCOMP * 2                          03930306
      IF (RVCMP2 .GE. .79995 .AND. RVCMP2 .LE. .80005) IVCOMP=IVCOMP*3  03940306
      IF (LVCOMP) IVCOMP = IVCOMP * 5                                   03950306
      IF (RVCMP3 .EQ. 0.0) IVCOMP = IVCOMP * 7                          03960306
      IVCORR = 210                                                      03970306
C     210 = 2 * 3 * 5 * 7                                               03980306
40110 IF (IVCOMP - 210) 20110, 10110, 20110                             03990306
30110 IVDELE = IVDELE + 1                                               04000306
      WRITE (I02,80000) IVTNUM                                          04010306
      IF (ICZERO) 10110, 0121, 20110                                    04020306
10110 IVPASS = IVPASS + 1                                               04030306
      WRITE (I02,80002) IVTNUM                                          04040306
      GO TO 0121                                                        04050306
20110 IVFAIL = IVFAIL + 1                                               04060306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04070306
 0121 CONTINUE                                                          04080306
C                                                                       04090306
C     ****  FCVS PROGRAM 306  -  TEST 012  ****                         04100306
C                                                                       04110306
C     TEST 012 TESTS THE SPECIFYING OF REAL TYPING TWICE IN ONE         04120306
C     IMPLICIT STATEMENT.                                               04130306
C                                                                       04140306
      IVTNUM =  12                                                      04150306
      IF (ICZERO) 30120, 0120, 30120                                    04160306
 0120 CONTINUE                                                          04170306
      MVIN01 = 4                                                        04180306
      NVIN01 = 4                                                        04190306
      RVCMP1 = MVIN01/5                                                 04200306
      RVCMP2 = NVIN01/5                                                 04210306
      IVCOMP = 1                                                        04220306
      IF (RVCMP1 .GE. .79995 .AND. RVCMP1 .LE. .80005) IVCOMP=IVCOMP*2  04230306
      IF (RVCMP2 .GE. .79995 .AND. RVCMP2 .LE. .80005) IVCOMP=IVCOMP*3  04240306
      IVCORR = 6                                                        04250306
C     6 = 2 * 3                                                         04260306
      IF (IVCOMP - 6) 20120, 10120, 20120                               04270306
30120 IVDELE = IVDELE + 1                                               04280306
      WRITE (I02,80000) IVTNUM                                          04290306
      IF (ICZERO) 10120, 0131, 20120                                    04300306
10120 IVPASS = IVPASS + 1                                               04310306
      WRITE (I02,80002) IVTNUM                                          04320306
      GO TO 0131                                                        04330306
20120 IVFAIL = IVFAIL + 1                                               04340306
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04350306
 0131 CONTINUE                                                          04360306
C                                                                       04370306
C                                                                       04380306
C     WRITE OUT TEST SUMMARY                                            04390306
C                                                                       04400306
      WRITE (I02,90004)                                                 04410306
      WRITE (I02,90014)                                                 04420306
      WRITE (I02,90004)                                                 04430306
      WRITE (I02,90000)                                                 04440306
      WRITE (I02,90004)                                                 04450306
      WRITE (I02,90020) IVFAIL                                          04460306
      WRITE (I02,90022) IVPASS                                          04470306
      WRITE (I02,90024) IVDELE                                          04480306
      STOP                                                              04490306
90001 FORMAT (1H ,24X,5HFM306)                                          04500306
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM306)                          04510306
C                                                                       04520306
C     FORMATS FOR TEST DETAIL LINES                                     04530306
C                                                                       04540306
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   04550306
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04560306
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04570306
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    04580306
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        04590306
C                                                                       04600306
C     FORMAT STATEMENTS FOR PAGE HEADERS                                04610306
C                                                                       04620306
90002 FORMAT (1H1)                                                      04630306
90004 FORMAT (1H )                                                      04640306
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04650306
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   04660306
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         04670306
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  04680306
90014 FORMAT (1H ,5X,46H----------------------------------------------) 04690306
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             04700306
C                                                                       04710306
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 04720306
C                                                                       04730306
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              04740306
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              04750306
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             04760306
      END                                                               04770306
