      PROGRAM FM317                                                     00010317
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020317
C                                                                       00030317
C          THIS ROUTINE TESTS SUBSET LEVEL FEATURES OF EXTERNAL         00040317
C     FUNCTION SUBPROGRAMS.  TESTS ARE DESIGNED TO CHECK THE            00050317
C     ASSOCIATION OF ALL PERMISSIBLE FORMS OF ACTUAL ARGUMENTS WITH     00060317
C     VARIABLE, ARRAY AND PROCEDURE NAME DUMMY ARGUMENTS.  THESE        00070317
C     INCLUDE,                                                          00080317
C                                                                       00090317
C          1) ACTUAL ARGUMENTS ASSOCIATED TO VARIABLE NAME DUMMY        00100317
C             ARGUMENT INCLUDE,                                         00110317
C                                                                       00120317
C             A) CONSTANT                                               00130317
C             B) VARIABLE NAME                                          00140317
C             C) ARRAY ELEMENT NAME                                     00150317
C             D) EXPRESSION INVOLVING OPERATORS                         00160317
C             E) EXPRESSION ENCLOSED IN PARENTHESES                     00170317
C             F) INTRINSIC FUNCTION REFERENCE                           00180317
C             G) EXTERNAL FUNCTION REFERENCE                            00190317
C             H) STATEMENT FUNCTION REFERENCE                           00200317
C             I) ACTUAL ARGUMENT NAME SAME AS DUMMY ARGUMENT NAME       00210317
C                                                                       00220317
C          2) ACTUAL ARGUMENTS ASSOCIATED TO ARRAY NAME DUMMY           00230317
C             ARGUMENT INCLUDE,                                         00240317
C                                                                       00250317
C             A) ARRAY NAME                                             00260317
C             B) ARRAY ELEMENT NAME                                     00270317
C                                                                       00280317
C          3) ACTUAL ARGUMENTS ASSOCIATED TO PROCEDURE NAME DUMMY       00290317
C             ARGUMENT INCLUDE,                                         00300317
C                                                                       00310317
C             A) EXTERNAL FUNCTION NAME                                 00320317
C             B) INTRINSIC FUNCTION NAME                                00330317
C             C) SUBROUTINE NAME                                        00340317
C                                                                       00350317
C     SUBSET LEVEL ROUTINES FM028,FM050 AND FM080 ALSO TEST THE USE OF  00360317
C     EXTERNAL FUNCTIONS.                                               00370317
C                                                                       00380317
C     REFERENCES.                                                       00390317
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00400317
C           X3.9-1978                                                   00410317
C                                                                       00420317
C        SECTION 2.8,     DUMMY ARGUMENTS                               00430317
C        SECTION 5.1.2.2, DUMMY ARRAY DECLARATOR                        00440317
C        SECTION 5.5,     DUMMY AND ACTUAL ARRAYS                       00450317
C        SECTION 8.1,     DIMENSION STATEMENT                           00460317
C        SECTION 8.3,     COMMON STATEMENT                              00470317
C        SECTION 8.4,     TYPE-STATEMENT                                00480317
C        SECTION 8.7,     EXTERNAL STATEMENT                            00490317
C        SECTION 8.8,     INTRINSIC STATEMENT                           00500317
C        SECTION 15.2,    REFERENCING A FUNCTION                        00510317
C        SECTION 15.3,    INTRINSIC FUNCTIONS                           00520317
C        SECTION 15.5,    EXTERNAL FUNCTIONS                            00530317
C        SECTION 15.6,    SUBROUTINES                                   00540317
C        SECTION 15.9,    ARGUMENTS AND COMMON BLOCKS                   00550317
C                                                                       00560317
C                                                                       00570317
C     ******************************************************************00580317
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00590317
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00600317
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00610317
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00620317
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00630317
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00640317
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00650317
C     THE RESULT OF EXECUTING THESE TESTS.                              00660317
C                                                                       00670317
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00680317
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00690317
C                                                                       00700317
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00710317
C                    DEPARTMENT OF THE NAVY                             00720317
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00730317
C                    WASHINGTON, D.C.   20376                           00740317
C                                                                       00750317
C     ******************************************************************00760317
C                                                                       00770317
C                                                                       00780317
      IMPLICIT LOGICAL (L)                                              00790317
      IMPLICIT CHARACTER*14 (C)                                         00800317
C                                                                       00810317
      INTEGER FF318, FF321, FF322, FF324, FF325                         00820317
      LOGICAL FF320                                                     00830317
      INTRINSIC  ABS, IABS, NINT                                        00840317
      EXTERNAL FF318, FF321, FF325, FS327                               00850317
      DIMENSION IADN11(4), IADN12(4)                                    00860317
      DIMENSION RADN11(4), RADN12(4)                                    00870317
      DIMENSION LADN11(4)                                               00880317
      COMMON IACN11(6), RACN11(10)                                      00890317
      INTEGER IATN11(2,3)                                               00900317
      REAL RATN11(3,4)                                                  00910317
      IFOS01(IDON04) = IDON04 + 1                                       00920317
C                                                                       00930317
C                                                                       00940317
C                                                                       00950317
C     INITIALIZATION SECTION.                                           00960317
C                                                                       00970317
C     INITIALIZE CONSTANTS                                              00980317
C     ********************                                              00990317
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01000317
      I01 = 5                                                           01010317
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01020317
      I02 = 6                                                           01030317
C     SYSTEM ENVIRONMENT SECTION                                        01040317
C                                                                       01050317
      I01 = 5                                                           01060317
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01070317
C     (UNIT NUMBER FOR CARD READER).                                    01080317
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01090317
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01100317
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01110317
C                                                                       01120317
      I02 = 6                                                           01130317
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01140317
C     (UNIT NUMBER FOR PRINTER).                                        01150317
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01160317
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01170317
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01180317
C                                                                       01190317
      IVPASS = 0                                                        01200317
      IVFAIL = 0                                                        01210317
      IVDELE = 0                                                        01220317
      ICZERO = 0                                                        01230317
C                                                                       01240317
C     WRITE OUT PAGE HEADERS                                            01250317
C                                                                       01260317
      WRITE (I02,90002)                                                 01270317
      WRITE (I02,90006)                                                 01280317
      WRITE (I02,90008)                                                 01290317
      WRITE (I02,90004)                                                 01300317
      WRITE (I02,90010)                                                 01310317
      WRITE (I02,90004)                                                 01320317
      WRITE (I02,90016)                                                 01330317
      WRITE (I02,90001)                                                 01340317
      WRITE (I02,90004)                                                 01350317
      WRITE (I02,90012)                                                 01360317
      WRITE (I02,90014)                                                 01370317
      WRITE (I02,90004)                                                 01380317
C                                                                       01390317
C                                                                       01400317
C     TEST 001 THROUGH TEST 022 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 01410317
C     OF ACTUAL ARGUMENTS TO VARIABLE NAMES USED AS EXTERNAL FUNCTION   01420317
C     DUMMY ARGUMENTS.  INTEGER, REAL AND LOGICAL DUMMY ARGUMENTS ARE   01430317
C     TESTED.                                                           01440317
C                                                                       01450317
C                                                                       01460317
C     ****  FCVS PROGRAM 317  -  TEST 001  ****                         01470317
C                                                                       01480317
C     INTEGER CONSTANT AS ACTUAL ARGUMENT                               01490317
C                                                                       01500317
      IVTNUM =   1                                                      01510317
      IF (ICZERO) 30010, 0010, 30010                                    01520317
 0010 CONTINUE                                                          01530317
      IVCOMP = 0                                                        01540317
      IVCOMP = FF318(3)                                                 01550317
      IVCORR = 4                                                        01560317
40010 IF (IVCOMP - 4) 20010, 10010, 20010                               01570317
30010 IVDELE = IVDELE + 1                                               01580317
      WRITE (I02,80000) IVTNUM                                          01590317
      IF (ICZERO) 10010, 0021, 20010                                    01600317
10010 IVPASS = IVPASS + 1                                               01610317
      WRITE (I02,80002) IVTNUM                                          01620317
      GO TO 0021                                                        01630317
20010 IVFAIL = IVFAIL + 1                                               01640317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01650317
 0021 CONTINUE                                                          01660317
C                                                                       01670317
C     ****  FCVS PROGRAM 317  -  TEST 002  ****                         01680317
C                                                                       01690317
C     REAL CONSTANT AS ACTUAL ARGUMENT                                  01700317
C                                                                       01710317
      IVTNUM =   2                                                      01720317
      IF (ICZERO) 30020, 0020, 30020                                    01730317
 0020 CONTINUE                                                          01740317
      RVCOMP = 0.0                                                      01750317
      RVCOMP = FF319(3.0)                                               01760317
      RVCORR = 4.0                                                      01770317
40020 IF (RVCOMP - 3.9995) 20020, 10020, 40021                          01780317
40021 IF (RVCOMP - 4.0005) 10020, 10020, 20020                          01790317
30020 IVDELE = IVDELE + 1                                               01800317
      WRITE (I02,80000) IVTNUM                                          01810317
      IF (ICZERO) 10020, 0031, 20020                                    01820317
10020 IVPASS = IVPASS + 1                                               01830317
      WRITE (I02,80002) IVTNUM                                          01840317
      GO TO 0031                                                        01850317
20020 IVFAIL = IVFAIL + 1                                               01860317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01870317
 0031 CONTINUE                                                          01880317
C                                                                       01890317
C     ****  FCVS PROGRAM 317  -  TEST 003  ****                         01900317
C                                                                       01910317
C     LOGICAL CONSTANT AS ACTUAL ARGUMENT                               01920317
C                                                                       01930317
      IVTNUM =   3                                                      01940317
      IF (ICZERO) 30030, 0030, 30030                                    01950317
 0030 CONTINUE                                                          01960317
      IVCOMP = 0                                                        01970317
      IF (FF320(.FALSE.)) IVCOMP = 1                                    01980317
      IVCORR = 1                                                        01990317
40030 IF (IVCOMP - 1) 20030, 10030, 20030                               02000317
30030 IVDELE = IVDELE + 1                                               02010317
      WRITE (I02,80000) IVTNUM                                          02020317
      IF (ICZERO) 10030, 0041, 20030                                    02030317
10030 IVPASS = IVPASS + 1                                               02040317
      WRITE (I02,80002) IVTNUM                                          02050317
      GO TO 0041                                                        02060317
20030 IVFAIL = IVFAIL + 1                                               02070317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02080317
 0041 CONTINUE                                                          02090317
C                                                                       02100317
C     ****  FCVS PROGRAM 317  -  TEST 004  ****                         02110317
C                                                                       02120317
C     INTEGER VARIABLE AS ACTUAL ARGUMENT                               02130317
C                                                                       02140317
      IVTNUM =   4                                                      02150317
      IF (ICZERO) 30040, 0040, 30040                                    02160317
 0040 CONTINUE                                                          02170317
      IVCOMP = 0                                                        02180317
      IVON01 = 7                                                        02190317
      IVCOMP = FF318(IVON01)                                            02200317
      IVCORR = 8                                                        02210317
40040 IF (IVCOMP - 8) 20040, 10040, 20040                               02220317
30040 IVDELE = IVDELE + 1                                               02230317
      WRITE (I02,80000) IVTNUM                                          02240317
      IF (ICZERO) 10040, 0051, 20040                                    02250317
10040 IVPASS = IVPASS + 1                                               02260317
      WRITE (I02,80002) IVTNUM                                          02270317
      GO TO 0051                                                        02280317
20040 IVFAIL = IVFAIL + 1                                               02290317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02300317
 0051 CONTINUE                                                          02310317
C                                                                       02320317
C     ****  FCVS PROGRAM 317  -  TEST 005  ****                         02330317
C                                                                       02340317
C     REAL VARIABLE AS ACTUAL ARGUMENT                                  02350317
C                                                                       02360317
      IVTNUM =   5                                                      02370317
      IF (ICZERO) 30050, 0050, 30050                                    02380317
 0050 CONTINUE                                                          02390317
      RVCOMP = 0.0                                                      02400317
      RVON01 = 7.0                                                      02410317
      RVCOMP = FF319(RVON01)                                            02420317
      RVCORR = 8.0                                                      02430317
40050 IF (RVCOMP - 7.9995) 20050, 10050, 40051                          02440317
40051 IF (RVCOMP - 8.0005) 10050, 10050, 20050                          02450317
30050 IVDELE = IVDELE + 1                                               02460317
      WRITE (I02,80000) IVTNUM                                          02470317
      IF (ICZERO) 10050, 0061, 20050                                    02480317
10050 IVPASS = IVPASS + 1                                               02490317
      WRITE (I02,80002) IVTNUM                                          02500317
      GO TO 0061                                                        02510317
20050 IVFAIL = IVFAIL + 1                                               02520317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02530317
 0061 CONTINUE                                                          02540317
C                                                                       02550317
C     ****  FCVS PROGRAM 317  -  TEST 006  ****                         02560317
C                                                                       02570317
C     LOGICAL VARIABLE AS ACTUAL ARGUMENT                               02580317
C                                                                       02590317
      IVTNUM =   6                                                      02600317
      IF (ICZERO) 30060, 0060, 30060                                    02610317
 0060 CONTINUE                                                          02620317
      LVON01 = .TRUE.                                                   02630317
      IVCOMP = 0                                                        02640317
      IF (.NOT. FF320(LVON01)) IVCOMP = 1                               02650317
      IVCORR = 1                                                        02660317
40060 IF (IVCOMP - 1) 20060, 10060, 20060                               02670317
30060 IVDELE = IVDELE + 1                                               02680317
      WRITE (I02,80000) IVTNUM                                          02690317
      IF (ICZERO) 10060, 0071, 20060                                    02700317
10060 IVPASS = IVPASS + 1                                               02710317
      WRITE (I02,80002) IVTNUM                                          02720317
      GO TO 0071                                                        02730317
20060 IVFAIL = IVFAIL + 1                                               02740317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02750317
 0071 CONTINUE                                                          02760317
C                                                                       02770317
C     ****  FCVS PROGRAM 317  -  TEST 007  ****                         02780317
C                                                                       02790317
C     INTEGER ARRAY ELEMENT NAME AS ACTUAL ARGUMENT                     02800317
C                                                                       02810317
      IVTNUM =   7                                                      02820317
      IF (ICZERO) 30070, 0070, 30070                                    02830317
 0070 CONTINUE                                                          02840317
      IVCOMP = 0                                                        02850317
      IADN11(2) = 2                                                     02860317
      IVCOMP = FF318(IADN11(2))                                         02870317
      IVCORR = 3                                                        02880317
40070 IF (IVCOMP - 3) 20070, 10070, 20070                               02890317
30070 IVDELE = IVDELE + 1                                               02900317
      WRITE (I02,80000) IVTNUM                                          02910317
      IF (ICZERO) 10070, 0081, 20070                                    02920317
10070 IVPASS = IVPASS + 1                                               02930317
      WRITE (I02,80002) IVTNUM                                          02940317
      GO TO 0081                                                        02950317
20070 IVFAIL = IVFAIL + 1                                               02960317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02970317
 0081 CONTINUE                                                          02980317
C                                                                       02990317
C     ****  FCVS PROGRAM 317  -  TEST 008  ****                         03000317
C                                                                       03010317
C     REAL ARRAY ELEMENT NAME AS ACTUAL ARGUMENT                        03020317
C                                                                       03030317
      IVTNUM =   8                                                      03040317
      IF (ICZERO) 30080, 0080, 30080                                    03050317
 0080 CONTINUE                                                          03060317
      RVCOMP = 0.0                                                      03070317
      RADN11(4) = 4.0                                                   03080317
      RVCOMP = FF319(RADN11(4))                                         03090317
      RVCORR = 5.0                                                      03100317
40080 IF (RVCOMP - 4.9995) 20080, 10080, 40081                          03110317
40081 IF (RVCOMP - 5.0005) 10080, 10080, 20080                          03120317
30080 IVDELE = IVDELE + 1                                               03130317
      WRITE (I02,80000) IVTNUM                                          03140317
      IF (ICZERO) 10080, 0091, 20080                                    03150317
10080 IVPASS = IVPASS + 1                                               03160317
      WRITE (I02,80002) IVTNUM                                          03170317
      GO TO 0091                                                        03180317
20080 IVFAIL = IVFAIL + 1                                               03190317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03200317
 0091 CONTINUE                                                          03210317
C                                                                       03220317
C     ****  FCVS PROGRAM 317  -  TEST 009  ****                         03230317
C                                                                       03240317
C     LOGICAL ARRAY ELEMENT NAME AS ACTUAL ARGUMENT                     03250317
C                                                                       03260317
      IVTNUM =   9                                                      03270317
      IF (ICZERO) 30090, 0090, 30090                                    03280317
 0090 CONTINUE                                                          03290317
      LADN11(1) = .FALSE.                                               03300317
      IVCOMP = 0                                                        03310317
      IF (FF320(LADN11(1))) IVCOMP = 1                                  03320317
      IVCORR = 1                                                        03330317
40090 IF (IVCOMP - 1) 20090, 10090, 20090                               03340317
30090 IVDELE = IVDELE + 1                                               03350317
      WRITE (I02,80000) IVTNUM                                          03360317
      IF (ICZERO) 10090, 0101, 20090                                    03370317
10090 IVPASS = IVPASS + 1                                               03380317
      WRITE (I02,80002) IVTNUM                                          03390317
      GO TO 0101                                                        03400317
20090 IVFAIL = IVFAIL + 1                                               03410317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03420317
 0101 CONTINUE                                                          03430317
C                                                                       03440317
C     ****  FCVS PROGRAM 317  -  TEST 010  ****                         03450317
C                                                                       03460317
C     INTEGER EXPRESSION INVOLVING OPERATORS AS ACTUAL ARGUMENT         03470317
C                                                                       03480317
      IVTNUM =  10                                                      03490317
      IF (ICZERO) 30100, 0100, 30100                                    03500317
 0100 CONTINUE                                                          03510317
      IVCOMP = 0                                                        03520317
      IVON02 = 2                                                        03530317
      IVON03 = 3                                                        03540317
      IVCOMP = FF318(IVON02 + 3 * IVON03 - 7)                           03550317
      IVCORR = 5                                                        03560317
40100 IF (IVCOMP - 5) 20100, 10100, 20100                               03570317
30100 IVDELE = IVDELE + 1                                               03580317
      WRITE (I02,80000) IVTNUM                                          03590317
      IF (ICZERO) 10100, 0111, 20100                                    03600317
10100 IVPASS = IVPASS + 1                                               03610317
      WRITE (I02,80002) IVTNUM                                          03620317
      GO TO 0111                                                        03630317
20100 IVFAIL = IVFAIL + 1                                               03640317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03650317
 0111 CONTINUE                                                          03660317
C                                                                       03670317
C     ****  FCVS PROGRAM 317  -  TEST 011  ****                         03680317
C                                                                       03690317
C     REAL EXPRESSION INVOLVING OPERATORS AS ACTUAL ARGUMENT            03700317
C                                                                       03710317
      IVTNUM =  11                                                      03720317
      IF (ICZERO) 30110, 0110, 30110                                    03730317
 0110 CONTINUE                                                          03740317
      RVCOMP = 0.0                                                      03750317
      RVON02 = 2.                                                       03760317
      RVON03 = 1.2                                                      03770317
      RVCOMP = FF319(RVON02 * RVON03 /.6)                               03780317
      RVCORR = 5.0                                                      03790317
40110 IF (RVCOMP - 4.9995) 20110, 10110, 40111                          03800317
40111 IF (RVCOMP - 5.0005) 10110, 10110, 20110                          03810317
30110 IVDELE = IVDELE + 1                                               03820317
      WRITE (I02,80000) IVTNUM                                          03830317
      IF (ICZERO) 10110, 0121, 20110                                    03840317
10110 IVPASS = IVPASS + 1                                               03850317
      WRITE (I02,80002) IVTNUM                                          03860317
      GO TO 0121                                                        03870317
20110 IVFAIL = IVFAIL + 1                                               03880317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03890317
 0121 CONTINUE                                                          03900317
C                                                                       03910317
C     ****  FCVS PROGRAM 317  -  TEST 012  ****                         03920317
C                                                                       03930317
C     REAL EXPRESSION INVOLVING INTEGER AND REAL PRIMARIES AND OPERATORS03940317
C     AS ACTUAL ARGUMENT.                                               03950317
C                                                                       03960317
      IVTNUM =  12                                                      03970317
      IF (ICZERO) 30120, 0120, 30120                                    03980317
 0120 CONTINUE                                                          03990317
      RVCOMP = 0.0                                                      04000317
      IVON01 = 2                                                        04010317
      RADN11(2) = 2.5                                                   04020317
      RVCOMP = FF319(IVON01**3 * (RADN11(2) - 1) + 2.0)                 04030317
      RVCORR = 15.0                                                     04040317
40120 IF (RVCOMP - 14.995) 20120, 10120, 40121                          04050317
40121 IF (RVCOMP - 15.005) 10120, 10120, 20120                          04060317
30120 IVDELE = IVDELE + 1                                               04070317
      WRITE (I02,80000) IVTNUM                                          04080317
      IF (ICZERO) 10120, 0131, 20120                                    04090317
10120 IVPASS = IVPASS + 1                                               04100317
      WRITE (I02,80002) IVTNUM                                          04110317
      GO TO 0131                                                        04120317
20120 IVFAIL = IVFAIL + 1                                               04130317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04140317
 0131 CONTINUE                                                          04150317
C                                                                       04160317
C     ****  FCVS PROGRAM 317  -  TEST 013  ****                         04170317
C                                                                       04180317
C     LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.NOT.) AS ACTUAL   04190317
C     ARGUMENT.                                                         04200317
C                                                                       04210317
      IVTNUM =  13                                                      04220317
      IF (ICZERO) 30130, 0130, 30130                                    04230317
 0130 CONTINUE                                                          04240317
      LVON01 = .TRUE.                                                   04250317
      IVCOMP = 0                                                        04260317
      IF (FF320(.NOT. LVON01)) IVCOMP = 1                               04270317
      IVCORR = 1                                                        04280317
40130 IF (IVCOMP - 1) 20130, 10130, 20130                               04290317
30130 IVDELE = IVDELE + 1                                               04300317
      WRITE (I02,80000) IVTNUM                                          04310317
      IF (ICZERO) 10130, 0141, 20130                                    04320317
10130 IVPASS = IVPASS + 1                                               04330317
      WRITE (I02,80002) IVTNUM                                          04340317
      GO TO 0141                                                        04350317
20130 IVFAIL = IVFAIL + 1                                               04360317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04370317
 0141 CONTINUE                                                          04380317
C                                                                       04390317
C     ****  FCVS PROGRAM 317  -  TEST 014  ****                         04400317
C                                                                       04410317
C     LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.OR.) AS ACTIVE    04420317
C     ARGUMENT.                                                         04430317
C                                                                       04440317
      IVTNUM =  14                                                      04450317
      IF (ICZERO) 30140, 0140, 30140                                    04460317
 0140 CONTINUE                                                          04470317
      LVON01 = .TRUE.                                                   04480317
      LVON02 = .FALSE.                                                  04490317
      IVCOMP = 0                                                        04500317
      IF (.NOT. FF320(LVON01 .OR. LVON02)) IVCOMP = 1                   04510317
      IVCORR = 1                                                        04520317
40140 IF (IVCOMP - 1) 20140, 10140, 20140                               04530317
30140 IVDELE = IVDELE + 1                                               04540317
      WRITE (I02,80000) IVTNUM                                          04550317
      IF (ICZERO) 10140, 0151, 20140                                    04560317
10140 IVPASS = IVPASS + 1                                               04570317
      WRITE (I02,80002) IVTNUM                                          04580317
      GO TO 0151                                                        04590317
20140 IVFAIL = IVFAIL + 1                                               04600317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04610317
 0151 CONTINUE                                                          04620317
C                                                                       04630317
C     ****  FCVS PROGRAM 317  -  TEST 015  ****                         04640317
C                                                                       04650317
C     LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.AND.) AS ACTUAL   04660317
C     ARGUMENT.                                                         04670317
C                                                                       04680317
      IVTNUM =  15                                                      04690317
      IF (ICZERO) 30150, 0150, 30150                                    04700317
 0150 CONTINUE                                                          04710317
      LVON01 = .FALSE.                                                  04720317
      LVON02 = .TRUE.                                                   04730317
      IVCOMP = 0                                                        04740317
      IF (FF320(LVON01 .AND. LVON02)) IVCOMP = 1                        04750317
      IVCORR = 1                                                        04760317
40150 IF (IVCOMP - 1) 20150, 10150, 20150                               04770317
30150 IVDELE = IVDELE + 1                                               04780317
      WRITE (I02,80000) IVTNUM                                          04790317
      IF (ICZERO) 10150, 0161, 20150                                    04800317
10150 IVPASS = IVPASS + 1                                               04810317
      WRITE (I02,80002) IVTNUM                                          04820317
      GO TO 0161                                                        04830317
20150 IVFAIL = IVFAIL + 1                                               04840317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04850317
 0161 CONTINUE                                                          04860317
C                                                                       04870317
C     ****  FCVS PROGRAM 317  -  TEST 016  ****                         04880317
C                                                                       04890317
C     EXPRESSION ENCLOSED IN PARENTHESES AS ACTUAL ARGUMENT             04900317
C                                                                       04910317
      IVTNUM =  16                                                      04920317
      IF (ICZERO) 30160, 0160, 30160                                    04930317
 0160 CONTINUE                                                          04940317
      IVCOMP = 0                                                        04950317
      IVON01 = 6                                                        04960317
      IVCOMP = FF318((IVON01 + 3))                                      04970317
      IVCORR = 10                                                       04980317
40160 IF (IVCOMP - 10) 20160, 10160, 20160                              04990317
30160 IVDELE = IVDELE + 1                                               05000317
      WRITE (I02,80000) IVTNUM                                          05010317
      IF (ICZERO) 10160, 0171, 20160                                    05020317
10160 IVPASS = IVPASS + 1                                               05030317
      WRITE (I02,80002) IVTNUM                                          05040317
      GO TO 0171                                                        05050317
20160 IVFAIL = IVFAIL + 1                                               05060317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05070317
 0171 CONTINUE                                                          05080317
C                                                                       05090317
C     ****  FCVS PROGRAM 317  -  TEST 017  ****                         05100317
C                                                                       05110317
C     REAL INTRINSIC FUNCTION REFERENCE AS ACTUAL ARGUMENT.             05120317
C                                                                       05130317
      IVTNUM =  17                                                      05140317
      IF (ICZERO) 30170, 0170, 30170                                    05150317
 0170 CONTINUE                                                          05160317
      RVCOMP = 0.0                                                      05170317
      RVON01 = -5.2                                                     05180317
      RVCOMP = FF319(ABS(RVON01))                                       05190317
      RVCORR = 6.2                                                      05200317
40170 IF (RVCOMP - 6.1995) 20170, 10170, 40171                          05210317
40171 IF (RVCOMP - 6.2005) 10170, 10170, 20170                          05220317
30170 IVDELE = IVDELE + 1                                               05230317
      WRITE (I02,80000) IVTNUM                                          05240317
      IF (ICZERO) 10170, 0181, 20170                                    05250317
10170 IVPASS = IVPASS + 1                                               05260317
      WRITE (I02,80002) IVTNUM                                          05270317
      GO TO 0181                                                        05280317
20170 IVFAIL = IVFAIL + 1                                               05290317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05300317
 0181 CONTINUE                                                          05310317
C                                                                       05320317
C     ****  FCVS PROGRAM 317  -  TEST 018  ****                         05330317
C                                                                       05340317
C     INTEGER INTRINSIC FUNCTION REFERENCE AS ACTUAL ARGUMENT.          05350317
C                                                                       05360317
      IVTNUM =  18                                                      05370317
      IF (ICZERO) 30180, 0180, 30180                                    05380317
 0180 CONTINUE                                                          05390317
      IVCOMP = 0                                                        05400317
      RVON01 = 4.7                                                      05410317
      IVCOMP = FF318(NINT(RVON01))                                      05420317
      IVCORR =  6                                                       05430317
40180 IF (IVCOMP - 6) 20180, 10180, 20180                               05440317
30180 IVDELE = IVDELE + 1                                               05450317
      WRITE (I02,80000) IVTNUM                                          05460317
      IF (ICZERO) 10180, 0191, 20180                                    05470317
10180 IVPASS = IVPASS + 1                                               05480317
      WRITE (I02,80002) IVTNUM                                          05490317
      GO TO 0191                                                        05500317
20180 IVFAIL = IVFAIL + 1                                               05510317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05520317
 0191 CONTINUE                                                          05530317
C                                                                       05540317
C     ****  FCVS PROGRAM 317  -  TEST 019  ****                         05550317
C                                                                       05560317
C     EXTERNAL FUNCTION REFERENCE AS ACTUAL ARGUMENT.                   05570317
C                                                                       05580317
      IVTNUM =  19                                                      05590317
      IF (ICZERO) 30190, 0190, 30190                                    05600317
 0190 CONTINUE                                                          05610317
      IVCOMP = 0                                                        05620317
      IVON01 = 4                                                        05630317
      IVCOMP = FF318(FF321(IVON01))                                     05640317
      IVCORR = 6                                                        05650317
40190 IF (IVCOMP - 6) 20190, 10190, 20190                               05660317
30190 IVDELE = IVDELE + 1                                               05670317
      WRITE (I02,80000) IVTNUM                                          05680317
      IF (ICZERO) 10190, 0201, 20190                                    05690317
10190 IVPASS = IVPASS + 1                                               05700317
      WRITE (I02,80002) IVTNUM                                          05710317
      GO TO 0201                                                        05720317
20190 IVFAIL = IVFAIL + 1                                               05730317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05740317
 0201 CONTINUE                                                          05750317
C                                                                       05760317
C     ****  FCVS PROGRAM 317  -  TEST 020  ****                         05770317
C                                                                       05780317
C     EXTERNAL FUNCTION REFERENCE WHICH USES A REFERENCE TO ITSELF      05790317
C     AS AN ACTUAL ARGUMENT.                                            05800317
C                                                                       05810317
      IVTNUM =  20                                                      05820317
      IF (ICZERO) 30200, 0200, 30200                                    05830317
 0200 CONTINUE                                                          05840317
      IVCOMP = 0                                                        05850317
      IVCOMP = FF318(FF318(4))                                          05860317
      IVCORR = 6                                                        05870317
40200 IF (IVCOMP - 6) 20200, 10200, 20200                               05880317
30200 IVDELE = IVDELE + 1                                               05890317
      WRITE (I02,80000) IVTNUM                                          05900317
      IF (ICZERO) 10200, 0211, 20200                                    05910317
10200 IVPASS = IVPASS + 1                                               05920317
      WRITE (I02,80002) IVTNUM                                          05930317
      GO TO 0211                                                        05940317
20200 IVFAIL = IVFAIL + 1                                               05950317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05960317
 0211 CONTINUE                                                          05970317
C                                                                       05980317
C     ****  FCVS PROGRAM 317  -  TEST 021  ****                         05990317
C                                                                       06000317
C     USE AN ACTUAL ARGUMENT NAME WHICH IS IDENTICAL TO THE DUMMY       06010317
C     ARGUMENT NAME.                                                    06020317
C                                                                       06030317
      IVTNUM =  21                                                      06040317
      IF (ICZERO) 30210, 0210, 30210                                    06050317
 0210 CONTINUE                                                          06060317
      IVCOMP = 0                                                        06070317
      IDON01 = 10                                                       06080317
      IVCOMP = FF318(IDON01)                                            06090317
      IVCORR = 11                                                       06100317
40210 IF (IVCOMP - 11) 20210, 10210, 20210                              06110317
30210 IVDELE = IVDELE + 1                                               06120317
      WRITE (I02,80000) IVTNUM                                          06130317
      IF (ICZERO) 10210, 0221, 20210                                    06140317
10210 IVPASS = IVPASS + 1                                               06150317
      WRITE (I02,80002) IVTNUM                                          06160317
      GO TO 0221                                                        06170317
20210 IVFAIL = IVFAIL + 1                                               06180317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06190317
 0221 CONTINUE                                                          06200317
C                                                                       06210317
C     ****  FCVS PROGRAM 317  -  TEST 022  ****                         06220317
C                                                                       06230317
C     USE STATEMENT FUNCTION REFERENCE AS ACTUAL ARGUMENT.              06240317
C                                                                       06250317
      IVTNUM =  22                                                      06260317
      IF (ICZERO) 30220, 0220, 30220                                    06270317
 0220 CONTINUE                                                          06280317
      IVCOMP = 0                                                        06290317
      IVCOMP = FF318(IFOS01(4))                                         06300317
      IVCORR = 6                                                        06310317
40220 IF (IVCOMP - 6) 20220, 10220, 20220                               06320317
30220 IVDELE = IVDELE + 1                                               06330317
      WRITE (I02,80000) IVTNUM                                          06340317
      IF (ICZERO) 10220, 0231, 20220                                    06350317
10220 IVPASS = IVPASS + 1                                               06360317
      WRITE (I02,80002) IVTNUM                                          06370317
      GO TO 0231                                                        06380317
20220 IVFAIL = IVFAIL + 1                                               06390317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06400317
 0231 CONTINUE                                                          06410317
C                                                                       06420317
C     TEST 023 THROUGH TEST 028 ARE DESIGNED TO ASSOCIATE VARIOUS       06430317
C     FORMS OF ACTUAL ARGUMENTS TO ARRAY NAMES USED AS EXTERNAL         06440317
C     FUNCTION DUMMY ARGUMENTS.                                         06450317
C                                                                       06460317
C                                                                       06470317
C     ****  FCVS PROGRAM 317  -  TEST 023  ****                         06480317
C                                                                       06490317
C     USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       06500317
C     ARGUMENT ARRAY DECLARATOR IS IDENTICAL TO THE ASSOCIATED DUMMY    06510317
C     ARGUMENT ARRAY DECLARATOR.                                        06520317
C                                                                       06530317
      IVTNUM =  23                                                      06540317
      IF (ICZERO) 30230, 0230, 30230                                    06550317
 0230 CONTINUE                                                          06560317
      IVCOMP = 0                                                        06570317
      IADN12(1) = 1                                                     06580317
      IADN12(2) = 10                                                    06590317
      IADN12(3) = 100                                                   06600317
      IADN12(4) = 1000                                                  06610317
      IVCOMP = FF322(IADN12)                                            06620317
      IVCORR = 1111                                                     06630317
40230 IF (IVCOMP - 1111) 20230, 10230, 20230                            06640317
30230 IVDELE = IVDELE + 1                                               06650317
      WRITE (I02,80000) IVTNUM                                          06660317
      IF (ICZERO) 10230, 0241, 20230                                    06670317
10230 IVPASS = IVPASS + 1                                               06680317
      WRITE (I02,80002) IVTNUM                                          06690317
      GO TO 0241                                                        06700317
20230 IVFAIL = IVFAIL + 1                                               06710317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06720317
 0241 CONTINUE                                                          06730317
C                                                                       06740317
C     ****  FCVS PROGRAM 317  -  TEST 024  ****                         06750317
C                                                                       06760317
C     USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE OF THE  06770317
C     ACTUAL ARGUMENT ARRAY IS LARGER THAN THE SIZE OF THE ASSOCIATED   06780317
C     DUMMY ARGUMENT ARRAY.                                             06790317
C                                                                       06800317
      IVTNUM =  24                                                      06810317
      IF (ICZERO) 30240, 0240, 30240                                    06820317
 0240 CONTINUE                                                          06830317
      IVCOMP = 0                                                        06840317
      IACN11(1) = 1                                                     06850317
      IACN11(2) = 10                                                    06860317
      IACN11(3) = 100                                                   06870317
      IACN11(4) = 1000                                                  06880317
      IACN11(5) = 10000                                                 06890317
      IVCOMP = FF322(IACN11)                                            06900317
      IVCORR = 1111                                                     06910317
40240 IF (IVCOMP - 1111) 20240, 10240, 20240                            06920317
30240 IVDELE = IVDELE + 1                                               06930317
      WRITE (I02,80000) IVTNUM                                          06940317
      IF (ICZERO) 10240, 0251, 20240                                    06950317
10240 IVPASS = IVPASS + 1                                               06960317
      WRITE (I02,80002) IVTNUM                                          06970317
      GO TO 0251                                                        06980317
20240 IVFAIL = IVFAIL + 1                                               06990317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07000317
 0251 CONTINUE                                                          07010317
C                                                                       07020317
C     ****  FCVS PROGRAM 317  -  TEST 025  ****                         07030317
C                                                                       07040317
C     USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       07050317
C     ARGUMENT ARRAY DECLARATOR IS LARGER AND HAS MORE SUBSCRIPT        07060317
C     EXPRESSIONS THAN THE ASSOCIATED DUMMY ARGUMENT ARRAY DECLARATOR.  07070317
C     THE ASSOCIATED DUMMY ARGUMENT ARRAY DECLARATOR.                   07080317
C                                                                       07090317
      IVTNUM =  25                                                      07100317
      IF (ICZERO) 30250, 0250, 30250                                    07110317
 0250 CONTINUE                                                          07120317
      IVCOMP = 0                                                        07130317
      IATN11(1,1) = 1                                                   07140317
      IATN11(2,1) = 10                                                  07150317
      IATN11(1,2) = 100                                                 07160317
      IATN11(2,2) = 1000                                                07170317
      IATN11(1,3) = 10000                                               07180317
      IVCOMP = FF322(IATN11)                                            07190317
      IVCORR = 1111                                                     07200317
40250 IF (IVCOMP - 1111) 20250, 10250, 20250                            07210317
30250 IVDELE = IVDELE + 1                                               07220317
      WRITE (I02,80000) IVTNUM                                          07230317
      IF (ICZERO) 10250, 0261, 20250                                    07240317
10250 IVPASS = IVPASS + 1                                               07250317
      WRITE (I02,80002) IVTNUM                                          07260317
      GO TO 0261                                                        07270317
20250 IVFAIL = IVFAIL + 1                                               07280317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07290317
 0261 CONTINUE                                                          07300317
C                                                                       07310317
C     ****  FCVS PROGRAM 317  -  TEST 026  ****                         07320317
C                                                                       07330317
C     USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE      07340317
C     ASSOCIATED ACTUAL AND DUMMY ARRAY DECLARATORS ARE IDENTICAL.  ALL 07350317
C     ARRAY ELEMENTS OF THE ACTUAL ARRAY SHOULD BE PASSED TO THE        07360317
C     DUMMY ARRAY OF THE EXTERNAL FUNCTION.                             07370317
C                                                                       07380317
      IVTNUM =  26                                                      07390317
      IF (ICZERO) 30260, 0260, 30260                                    07400317
 0260 CONTINUE                                                          07410317
      RVCOMP = 0.0                                                      07420317
      RADN12(1) = 1.                                                    07430317
      RADN12(2) = 10.                                                   07440317
      RADN12(3) = 100.                                                  07450317
      RADN12(4) = 1000.                                                 07460317
      RVCOMP = FF323(RADN12(1))                                         07470317
      RVCORR = 1111.                                                    07480317
40260 IF (RVCOMP - 1110.5) 20260, 10260, 40261                          07490317
40261 IF (RVCOMP - 1111.5) 10260, 10260, 20260                          07500317
30260 IVDELE = IVDELE + 1                                               07510317
      WRITE (I02,80000) IVTNUM                                          07520317
      IF (ICZERO) 10260, 0271, 20260                                    07530317
10260 IVPASS = IVPASS + 1                                               07540317
      WRITE (I02,80002) IVTNUM                                          07550317
      GO TO 0271                                                        07560317
20260 IVFAIL = IVFAIL + 1                                               07570317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07580317
 0271 CONTINUE                                                          07590317
C                                                                       07600317
C     ****  FCVS PROGRAM 317  -  TEST 027  ****                         07610317
C                                                                       07620317
C     USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 07630317
C     OF THE ACTUAL ARGUMENT ARRAY IS LARGER AND HAS FEWER SUBSCRIPT    07640317
C     EXPRESSIONS THAN THE ASSOCIATED DUMMY ARGUMENT ARRAY.  ONLY ACTUAL07650317
C     ARRAY ELEMENTS WITH SUBSCRIPT VALUES OF 5, 6, 7 AND 8 (OUT OF A   07660317
C     POSSIBLE 10 ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF THE  07670317
C     EXTERNAL FUNCTION.                                                07680317
C                                                                       07690317
      IVTNUM =  27                                                      07700317
      IF (ICZERO) 30270, 0270, 30270                                    07710317
 0270 CONTINUE                                                          07720317
      RVCOMP = 0.0                                                      07730317
      RACN11(4) = 1.                                                    07740317
      RACN11(5) = 10.                                                   07750317
      RACN11(6) = 100.                                                  07760317
      RACN11(7) = 1000.                                                 07770317
      RACN11(8) = 10000.                                                07780317
      RACN11(9) = 100000.                                               07790317
      RVCORR =  11110.                                                  07800317
      RVCOMP = FF323(RACN11(5))                                         07810317
40270 IF (RVCOMP - 11105.) 20270, 10270, 40271                          07820317
40271 IF (RVCOMP - 11115.) 10270, 10270, 20270                          07830317
30270 IVDELE = IVDELE + 1                                               07840317
      WRITE (I02,80000) IVTNUM                                          07850317
      IF (ICZERO) 10270, 0281, 20270                                    07860317
10270 IVPASS = IVPASS + 1                                               07870317
      WRITE (I02,80002) IVTNUM                                          07880317
      GO TO 0281                                                        07890317
20270 IVFAIL = IVFAIL + 1                                               07900317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07910317
 0281 CONTINUE                                                          07920317
C                                                                       07930317
C     ****  FCVS PROGRAM 317  -  TEST 028  ****                         07940317
C                                                                       07950317
C     USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 07960317
C     OF THE ACTUAL ARGUMENT ARRAY IS LARGE THAN THE SIZE OF THE        07970317
C     ASSOCIATED DUMMY ARGUMENT ARRAY.  ONLY ACTUAL ARRAY ELEMENTS WITH 07980317
C     SUBSCRIPT VALUES OF 9, 10, 11 AND 12 (OUT OF A POSSIBLE 12        07990317
C     ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF THE EXTERNAL     08000317
C     FUNCTION.                                                         08010317
C                                                                       08020317
      IVTNUM =  28                                                      08030317
      IF (ICZERO) 30280, 0280, 30280                                    08040317
 0280 CONTINUE                                                          08050317
      RVCOMP = 0.0                                                      08060317
      RATN11(2,3) = 1.                                                  08070317
      RATN11(3,3) = 10.                                                 08080317
      RATN11(1,4) = 100.                                                08090317
      RATN11(2,4) = 1000.                                               08100317
      RATN11(3,4) = 10000.                                              08110317
      RVCOMP = FF323(RATN11(3,3))                                       08120317
      RVCORR = 11110.                                                   08130317
40280 IF (RVCOMP - 11105.) 20280, 10280, 40281                          08140317
40281 IF (RVCOMP - 11115.) 10280, 10280, 20280                          08150317
30280 IVDELE = IVDELE + 1                                               08160317
      WRITE (I02,80000) IVTNUM                                          08170317
      IF (ICZERO) 10280, 0291, 20280                                    08180317
10280 IVPASS = IVPASS + 1                                               08190317
      WRITE (I02,80002) IVTNUM                                          08200317
      GO TO 0291                                                        08210317
20280 IVFAIL = IVFAIL + 1                                               08220317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08230317
 0291 CONTINUE                                                          08240317
C                                                                       08250317
C     TEST 029 THROUGH TEST 032 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 08260317
C     OF ACTUAL ARGUMENTS TO PROCEDURES USED AS DUMMY ARGUMENTS.        08270317
C     ACTUAL ARGUMENTS TESTED INCLUDE THE NAMES OF AN EXTERNAL FUNCTION,08280317
C     AN INTRINSIC FUNCTION, AND A SUBROUTINE.                          08290317
C                                                                       08300317
C                                                                       08310317
C     ****  FCVS PROGRAM 317  -  TEST 029  ****                         08320317
C                                                                       08330317
C     USE AN EXTERNAL FUNCTION NAME AS AN ACTUAL ARGUMENT.              08340317
C                                                                       08350317
      IVTNUM =  29                                                      08360317
      IF (ICZERO) 30290, 0290, 30290                                    08370317
 0290 CONTINUE                                                          08380317
      IVCOMP = 0                                                        08390317
      IVCOMP = FF324(FF325,5)                                           08400317
      IVCORR = 7                                                        08410317
40290 IF (IVCOMP - 7) 20290, 10290, 20290                               08420317
30290 IVDELE = IVDELE + 1                                               08430317
      WRITE (I02,80000) IVTNUM                                          08440317
      IF (ICZERO) 10290, 0301, 20290                                    08450317
10290 IVPASS = IVPASS + 1                                               08460317
      WRITE (I02,80002) IVTNUM                                          08470317
      GO TO 0301                                                        08480317
20290 IVFAIL = IVFAIL + 1                                               08490317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08500317
 0301 CONTINUE                                                          08510317
C                                                                       08520317
C     ****  FCVS PROGRAM 317  -  TEST 030  ****                         08530317
C                                                                       08540317
C     USE AN INTRINSIC FUNCTION NAME AS AN ACTUAL ARGUMENT.             08550317
C                                                                       08560317
      IVTNUM =  30                                                      08570317
      IF (ICZERO) 30300, 0300, 30300                                    08580317
 0300 CONTINUE                                                          08590317
      IVCOMP = 0                                                        08600317
      IVCOMP = FF324(IABS,-7)                                           08610317
      IVCORR = 8                                                        08620317
40300 IF (IVCOMP - 8) 20300, 10300, 20300                               08630317
30300 IVDELE = IVDELE + 1                                               08640317
      WRITE (I02,80000) IVTNUM                                          08650317
      IF (ICZERO) 10300, 0311, 20300                                    08660317
10300 IVPASS = IVPASS + 1                                               08670317
      WRITE (I02,80002) IVTNUM                                          08680317
      GO TO 0311                                                        08690317
20300 IVFAIL = IVFAIL + 1                                               08700317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08710317
 0311 CONTINUE                                                          08720317
C                                                                       08730317
C     ****  FCVS PROGRAM 317  -  TEST 031  ****                         08740317
C                                                                       08750317
C     USE AN EXTERNAL FUNCTION NAME AS AN ACTUAL ARGUMENT.  THE         08760317
C     INTRINSIC FUNCTION NAME (NINT) IS USED AS THE DUMMY PROCEDURE     08770317
C     NAME IN THE EXTERNAL FUNCTION AND THEREFORE CAN NOT BE USED AS    08780317
C     AN INTRINSIC FUNCTION WITHIN THAT PROGRAM UNIT.  HOWEVER IT CAN   08790317
C     BE REFERENCED IN THE MAIN PROGRAM FM317 AND IN THE SUBPROGRAM     08800317
C     FF325.                                                            08810317
C                                                                       08820317
      IVTNUM =  31                                                      08830317
      IF (ICZERO) 30310, 0310, 30310                                    08840317
 0310 CONTINUE                                                          08850317
      IVCOMP = 0                                                        08860317
      IVCOMP = NINT(3.7) + FF324(FF325,2)                               08870317
      IVCORR = 8                                                        08880317
40310 IF (IVCOMP - 8) 20310, 10310, 20310                               08890317
30310 IVDELE = IVDELE + 1                                               08900317
      WRITE (I02,80000) IVTNUM                                          08910317
      IF (ICZERO) 10310, 0321, 20310                                    08920317
10310 IVPASS = IVPASS + 1                                               08930317
      WRITE (I02,80002) IVTNUM                                          08940317
      GO TO 0321                                                        08950317
20310 IVFAIL = IVFAIL + 1                                               08960317
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08970317
 0321 CONTINUE                                                          08980317
C                                                                       08990317
C     ****  FCVS PROGRAM 317  -  TEST 032  ****                         09000317
C                                                                       09010317
C     USE A SUBROUTINE NAME AS AN ACTUAL ARGUMENT.                      09020317
C                                                                       09030317
      IVTNUM =  32                                                      09040317
      IF (ICZERO) 30320, 0320, 30320                                    09050317
 0320 CONTINUE                                                          09060317
      RVCOMP = 0.0                                                      09070317
      RVON01 = 3.5                                                      09080317
      RVCOMP = FF326(FS327,RVON01)                                      09090317
      RVCORR = 5.5                                                      09100317
40320 IF (RVCOMP - 5.4995) 20320, 10320, 40321                          09110317
40321 IF (RVCOMP - 5.5005) 10320, 10320, 20320                          09120317
30320 IVDELE = IVDELE + 1                                               09130317
      WRITE (I02,80000) IVTNUM                                          09140317
      IF (ICZERO) 10320, 0331, 20320                                    09150317
10320 IVPASS = IVPASS + 1                                               09160317
      WRITE (I02,80002) IVTNUM                                          09170317
      GO TO 0331                                                        09180317
20320 IVFAIL = IVFAIL + 1                                               09190317
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          09200317
 0331 CONTINUE                                                          09210317
C                                                                       09220317
C                                                                       09230317
C     WRITE OUT TEST SUMMARY                                            09240317
C                                                                       09250317
      WRITE (I02,90004)                                                 09260317
      WRITE (I02,90014)                                                 09270317
      WRITE (I02,90004)                                                 09280317
      WRITE (I02,90000)                                                 09290317
      WRITE (I02,90004)                                                 09300317
      WRITE (I02,90020) IVFAIL                                          09310317
      WRITE (I02,90022) IVPASS                                          09320317
      WRITE (I02,90024) IVDELE                                          09330317
      STOP                                                              09340317
90001 FORMAT (1H ,24X,5HFM317)                                          09350317
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM317)                          09360317
C                                                                       09370317
C     FORMATS FOR TEST DETAIL LINES                                     09380317
C                                                                       09390317
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   09400317
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      09410317
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         09420317
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    09430317
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        09440317
C                                                                       09450317
C     FORMAT STATEMENTS FOR PAGE HEADERS                                09460317
C                                                                       09470317
90002 FORMAT (1H1)                                                      09480317
90004 FORMAT (1H )                                                      09490317
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09500317
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   09510317
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         09520317
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  09530317
90014 FORMAT (1H ,5X,46H----------------------------------------------) 09540317
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             09550317
C                                                                       09560317
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 09570317
C                                                                       09580317
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              09590317
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              09600317
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             09610317
      END                                                               09620317
      INTEGER FUNCTION FF318(IDON01)                                    00010318
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 00020318
C     TO TEST THE ASSOCIATION OF VARIOUS FORMS OF INTEGER ACTUAL        00030318
C     ARGUMENTS TO AN INTEGER VARIABLE NAME USED AS AN EXTERNAL         00040318
C     FUNCTION DUMMY ARGUMENT.  THIS ROUTINE INCREMENTS THE ARGUMENT    00050318
C     VALUE BY ONE AND RETURNS THE RESULT AS THE FUNCTION VALUE.        00060318
      FF318 = IDON01 + 1                                                00070318
      RETURN                                                            00080318
      END                                                               00090318
      REAL FUNCTION FF319(RDON01)                                       00010319
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 00020319
C     TO TEST THE ASSOCIATION OF VARIOUS FORMS OF REAL ACTUAL           00030319
C     ARGUMENTS TO A REAL VARIABLE NAME USED AS AN EXTERNAL FUNCTION    00040319
C     DUMMY ARGUMENT.  THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY    00050319
C     ONE AND RETURNS THE RESULT AS THE FUNCTION VALUE.                 00060319
      FF319 = RDON01 + 1.0                                              00070319
      RETURN                                                            00080319
      END                                                               00090319
      LOGICAL FUNCTION FF320(LDON01)                                    00010320
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 00020320
C     TO TEST THE ASSOCIATION OF VARIOUS FORMS OF LOGICAL ACTUAL        00030320
C     ARGUMENTS TO A LOGICAL VARIABLE NAME USED AS AN EXTERNAL          00040320
C     FUNCTION DUMMY ARGUMENT.  THIS ROUTINE NEGATES THE ARGUMENT       00050320
C     VALUE AND RETURNS THE RESULT AS THE FUNCTION VALUE.               00060320
      LOGICAL LDON01                                                    00070320
      FF320 = .NOT. LDON01                                              00080320
      RETURN                                                            00090320
      END                                                               00100320
      INTEGER FUNCTION FF321(IDON02)                                    00010321
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED IN TEST 019 OF MAIN PROGRAM FM317 AS   00020321
C     THE TEST OF THE USE OF AN EXTERNAL FUNCTION REFERENCE AS AN       00030321
C     ACTUAL ARGUMENT TO A VARIABLE NAME USED AS AN EXTERNAL FUNCTION   00040321
C     DUMMY ARGUMENT.  THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY    00050321
C     ONE AND RETURNS THE RESULT AS THE FUNCTION VALUE.                 00060321
      FF321 = IDON02 + 1                                                00070321
      RETURN                                                            00080321
      END                                                               00090321
      INTEGER FUNCTION FF322(IDDN11)                                    00010322
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 00020322
C     TO TEST THE ASSOCIATION OF VARIOUS FORMS OF ARRAY NAMES USED AS   00030322
C     ACTUAL ARGUMENTS TO AN ARRAY NAME USED AS AN EXTERNAL FUNCTION    00040322
C     DUMMY ARGUMENT.  THIS ROUTINE ADDS TOGETHER THE FOUR ELEMENTS IN  00050322
C     THE DUMMY ARRAY AND RETURNS THE SUM AS THE FUNCTION VALUE.        00060322
      DIMENSION IDDN11(4)                                               00070322
      FF322 = IDDN11(1) + IDDN11(2) + IDDN11(3) + IDDN11(4)             00080322
      RETURN                                                            00090322
      END                                                               00100322
      REAL FUNCTION FF323(RDTN21)                                       00010323
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 00020323
C     TO TEST THE ASSOCIATION OF VARIOUS FORMS OF ARRAY ELEMENT NAMES   00030323
C     USED AS ACTUAL ARGUMENTS TO AN ARRAY NAME USED AS AN EXTERNAL     00040323
C     FUNCTION DUMMY ARGUMENT.  THIS ROUTINE ADDS TOGETHER THE FOUR     00050323
C     ELEMENTS IN THE DUMMY ARRAY AND RETURNS THE SUM AS THE FUNCTION   00060323
C     VALUE.                                                            00070323
      REAL RDTN21(2,2)                                                  00080323
      FF323 = RDTN21(1,1) + RDTN21(2,1) + RDTN21(1,2) + RDTN21(2,2)     00090323
      RETURN                                                            00100323
      END                                                               00110323
      INTEGER FUNCTION FF324(NINT, IDON03)                              00010324
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY TESTS 029, 030 AND 031 OF MAIN      00020324
C     PROGRAM FM317 TO TEST THE ASSOCIATION OF EXTERNAL FUNCTION AND    00030324
C     INTRINSIC FUNCTION NAMES USED AS ACTUAL ARGUMENTS TO A PROCEDURE  00040324
C     NAME USED AS A DUMMY ARGUMENT.  THIS FUNCTION REFERENCES THE      00050324
C     EXTERNAL FUNCTION OR INTRINSIC FUNCTION PASSED AS A PROCEDURE     00060324
C     NAME ARGUMENT, INCREMENTING THE RESULT BY ONE BEFORE RETURNING    00070324
C     THE RESULT AS THE FUNCTION VALUE.                                 00080324
      FF324 = NINT(IDON03) + 1                                          00090324
C          **** THE NAME NINT IS A DUMMY ARGUMENT                       00100324
C                   AND NOT AN INTRINSIC FUNCTION REFERENCE *****       00110324
      RETURN                                                            00120324
      END                                                               00130324
      INTEGER FUNCTION FF325(IDON05)                                    00010325
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY TESTS 029 AND 031 OF MAIN PROGRAM   00020325
C     FM317 TO TEST THE ASSOCIATION OF AN EXTERNAL FUNCTION NAME USED AS00030325
C     AN ACTUAL ARGUMENT TO A PROCEDURE NAME USED AS A DUMMY ARGUMENT.  00040325
C     FF325 IS REFERENCED FROM EXTERNAL FUNCTION FF324 VIA A DUMMY      00050325
C     PROCEDURE NAME REFERENCE.  THIS ROUTINE ADDS THE RESULT OF AN     00060325
C     INTRINSIC FUNCTION REFERENCE (NINT) TO THE ARGUMENT VALUE AND     00070325
C     RETURNS THE SUM AS THE FUNCTION VALUE.                            00080325
      FF325 = IDON05 + NINT(1.2)                                        00090325
      RETURN                                                            00100325
      END                                                               00110325
      REAL FUNCTION FF326(RDON02,RDON03)                                00010326
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY TEST 032 OF MAIN PROGRAM FM317 TO   00020326
C     TEST THE ASSOCIATION OF A SUBROUTINE NAME USED AS AN ACTUAL       00030326
C     ARGUMENT TO A PROCEDURE NAME USED AS A DUMMY ARGUMENT.  THIS      00040326
C     FUNCTION CALLS THE SUBROUTINE (FS327) PASSED AS A PROCEDURE NAME  00050326
C     ARGUMENT.  THE VALUE OF THE ARGUMENT RETURNED FROM THIS           00060326
C     REFERENCE IS THEN INCREMENTED BY ONE BEFORE RETURNING THE SUM AS  00070326
C     THE FUNCTION VALUE.                                               00080326
      CALL RDON02(RDON03)                                               00090326
      FF326 = RDON03 + 1.0                                              00100326
      RETURN                                                            00110326
      END                                                               00120326
      SUBROUTINE FS327(RDON04)                                          00010327
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS SUBROUTINE IS USED BY TEST 032 OF MAIN PROGRAM FM317 TO 00020327
C     TEST THE ASSOCIATION OF A SUBROUTINE NAME USED AS AN ACTUAL       00030327
C     ARGUMENT TO A PROCEDURE NAME USED AS A DUMMY ARGUMENT.  FS327 IS  00040327
C     CALLED FROM EXTERNAL PROGRAM FF326 VIA A DUMMY PROCEDURE NAME     00050327
C     REFERENCE.  THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY ONE.    00060327
      RDON04 = RDON04 + 1.0                                             00070327
      RETURN                                                            00080327
      END                                                               00090327
