      PROGRAM FM328                                                     00010328
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020328
C                                                                       00030328
C          THIS ROUTINE TEST SUBSET LEVEL FEATURES OF                   00040328
C     SUBROUTINE SUBPROGRAMS.  TESTS ARE DESIGNED TO CHECK THE          00050328
C     ASSOCIATION OF ALL PERMISSIBLE FORMS OF ACTUAL ARGUMENTS WITH     00060328
C     VARIABLE, ARRAY AND PROCEDURE NAME DUMMY ARGUMENTS.  THESE        00070328
C     INCLUDE,                                                          00080328
C                                                                       00090328
C          1) ACTUAL ARGUMENTS ASSOCIATED TO VARIABLE NAME DUMMY        00100328
C             ARGUMENT INCLUDE,                                         00110328
C                                                                       00120328
C             A) CONSTANT                                               00130328
C             B) VARIABLE NAME                                          00140328
C             C) ARRAY ELEMENT NAME                                     00150328
C             D) EXPRESSION INVOLVING OPERATORS                         00160328
C             E) EXPRESSION ENCLOSED IN PARENTHESES                     00170328
C             F) INTRINSIC FUNCTION REFERENCE                           00180328
C             G) EXTERNAL FUNCTION REFERENCE                            00190328
C             H) STATEMENT FUNCTION REFERENCE                           00200328
C             I) ACTUAL ARGUMENT NAME SAME AS DUMMY ARGUMENT NAME       00210328
C                                                                       00220328
C          2) ACTUAL ARGUMENTS ASSOCIATED TO ARRAY NAME DUMMY           00230328
C             ARGUMENT INCLUDE,                                         00240328
C                                                                       00250328
C             A) ARRAY NAME                                             00260328
C             B) ARRAY ELEMENT NAME                                     00270328
C                                                                       00280328
C          3) ACTUAL ARGUMENTS ASSOCIATED TO PROCEDURE NAME DUMMY       00290328
C             ARGUMENT INCLUDE,                                         00300328
C                                                                       00310328
C             A) EXTERNAL FUNCTION NAME                                 00320328
C             B) INTRINSIC FUNCTION NAME                                00330328
C             C) SUBROUTINE NAME                                        00340328
C                                                                       00350328
C     ALL DATA PASSED TO THE REFERENCED SUBPROGRAMS ARE PASSED VIA      00360328
C     ARGUMENT VALUES, WHILE ALL RESULTS RETURNED TO FM328 ARE          00370328
C     RETURNED VIA VARIABLES IN NAMED COMMON.   SUBSET LEVEL ROUTINES   00380328
C     FM026, FM050 AND FM056 ALSO TEST THE USE OF SUBROUTINES.          00390328
C                                                                       00400328
C     REFERENCES.                                                       00410328
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00420328
C           X3.9-1978                                                   00430328
C                                                                       00440328
C        SECTION 2.8,     DUMMY ARGUMENTS                               00450328
C        SECTION 5.1.2.2, DUMMY ARRAY DECLARATOR                        00460328
C        SECTION 5.5,     DUMMY AND ACTUAL ARRAYS                       00470328
C        SECTION 8.1,     DIMENSION STATEMENT                           00480328
C        SECTION 8.3,     COMMON STATEMENT                              00490328
C        SECTION 8.4,     TYPE-STATEMENT                                00500328
C        SECTION 8.7,     EXTERNAL STATEMENT                            00510328
C        SECTION 8.8,     INTRINSIC STATEMENT                           00520328
C        SECTION 15.2,    REFERENCING A FUNCTION                        00530328
C        SECTION 15.3,    INTRINSIC FUNCTIONS                           00540328
C        SECTION 15.5,    EXTERNAL FUNCTIONS                            00550328
C        SECTION 15.6,    SUBROUTINES                                   00560328
C        SECTION 15.9,    ARGUMENTS AND COMMON BLOCKS                   00570328
C                                                                       00580328
C                                                                       00590328
C     ******************************************************************00600328
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00610328
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00620328
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00630328
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00640328
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00650328
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00660328
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00670328
C     THE RESULT OF EXECUTING THESE TESTS.                              00680328
C                                                                       00690328
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00700328
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00710328
C                                                                       00720328
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00730328
C                    DEPARTMENT OF THE NAVY                             00740328
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00750328
C                    WASHINGTON, D.C.   20376                           00760328
C                                                                       00770328
C     ******************************************************************00780328
C                                                                       00790328
C                                                                       00800328
      IMPLICIT LOGICAL (L)                                              00810328
      IMPLICIT CHARACTER*14 (C)                                         00820328
C                                                                       00830328
      INTEGER IATN11(2,3)                                               00840328
      REAL RATN11(3,4)                                                  00850328
      INTEGER FF330                                                     00860328
      DIMENSION IADN11(4), IADN12(4)                                    00870328
      DIMENSION RADN11(4), RADN12(4)                                    00880328
      DIMENSION LADN11(4)                                               00890328
      COMMON /BLK1/IVCN01, RVCN01, LVCN01                               00900328
      COMMON IACN11(6), RACN11(10)                                      00910328
      EXTERNAL FF330, FS335                                             00920328
      INTRINSIC  ABS, IABS, NINT                                        00930328
      IFOS01(IDON04) = IDON04 + 1                                       00940328
      RFOS01(RDON04) = RDON04 + 1.0                                     00950328
      LFOS01(LDON04) = .NOT. LDON04                                     00960328
C                                                                       00970328
C                                                                       00980328
C                                                                       00990328
C     INITIALIZATION SECTION.                                           01000328
C                                                                       01010328
C     INITIALIZE CONSTANTS                                              01020328
C     ********************                                              01030328
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01040328
      I01 = 5                                                           01050328
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01060328
      I02 = 6                                                           01070328
C     SYSTEM ENVIRONMENT SECTION                                        01080328
C                                                                       01090328
      I01 = 5                                                           01100328
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01110328
C     (UNIT NUMBER FOR CARD READER).                                    01120328
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01130328
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01140328
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01150328
C                                                                       01160328
      I02 = 6                                                           01170328
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01180328
C     (UNIT NUMBER FOR PRINTER).                                        01190328
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01200328
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01210328
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01220328
C                                                                       01230328
      IVPASS = 0                                                        01240328
      IVFAIL = 0                                                        01250328
      IVDELE = 0                                                        01260328
      ICZERO = 0                                                        01270328
C                                                                       01280328
C     WRITE OUT PAGE HEADERS                                            01290328
C                                                                       01300328
      WRITE (I02,90002)                                                 01310328
      WRITE (I02,90006)                                                 01320328
      WRITE (I02,90008)                                                 01330328
      WRITE (I02,90004)                                                 01340328
      WRITE (I02,90010)                                                 01350328
      WRITE (I02,90004)                                                 01360328
      WRITE (I02,90016)                                                 01370328
      WRITE (I02,90001)                                                 01380328
      WRITE (I02,90004)                                                 01390328
      WRITE (I02,90012)                                                 01400328
      WRITE (I02,90014)                                                 01410328
      WRITE (I02,90004)                                                 01420328
C                                                                       01430328
C                                                                       01440328
C     TEST 001 THROUGH TEST 013 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 01450328
C     OF ACTUAL ARGUMENTS TO VARIABLE NAMES USED AS SUBROUTINE          01460328
C     DUMMY ARGUMENTS.  INTEGER, REAL AND LOGICAL DUMMY ARGUMENTS ARE   01470328
C     TESTED.                                                           01480328
C                                                                       01490328
C                                                                       01500328
C     ****  FCVS PROGRAM 328  -  TEST 001  ****                         01510328
C                                                                       01520328
C     USE INTEGER, REAL AND LOGICAL CONSTANTS AS ACTUAL ARGUMENTS.      01530328
C                                                                       01540328
      IVTNUM =   1                                                      01550328
      IF (ICZERO) 30010, 0010, 30010                                    01560328
 0010 CONTINUE                                                          01570328
      CALL FS329(3, 3.0, .FALSE.)                                       01580328
      IVCOMP = 1                                                        01590328
      IF (IVCN01 .EQ. 4) IVCOMP = IVCOMP * 2                            01600328
      IF (RVCN01 .GE. 3.9995 .AND. RVCN01 .LE. 4.0005) IVCOMP = IVCOMP*301610328
      IF (LVCN01) IVCOMP = IVCOMP * 5                                   01620328
      IVCORR = 30                                                       01630328
40010 IF (IVCOMP - 30) 20010, 10010, 20010                              01640328
30010 IVDELE = IVDELE + 1                                               01650328
      WRITE (I02,80000) IVTNUM                                          01660328
      IF (ICZERO) 10010, 0021, 20010                                    01670328
10010 IVPASS = IVPASS + 1                                               01680328
      WRITE (I02,80002) IVTNUM                                          01690328
      GO TO 0021                                                        01700328
20010 IVFAIL = IVFAIL + 1                                               01710328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01720328
 0021 CONTINUE                                                          01730328
C                                                                       01740328
C     ****  FCVS PROGRAM 328  -  TEST 002  ****                         01750328
C                                                                       01760328
C     USE INTEGER, REAL AND LOGICAL VARIABLES AS ACTUAL ARGUMENTS.      01770328
C                                                                       01780328
      IVTNUM =   2                                                      01790328
      IF (ICZERO) 30020, 0020, 30020                                    01800328
 0020 CONTINUE                                                          01810328
      IVON01 = 7                                                        01820328
      RVON01 = 7.0                                                      01830328
      LVON01 = .TRUE.                                                   01840328
      CALL FS329(IVON01, RVON01, LVON01)                                01850328
      IVCOMP = 1                                                        01860328
      IF (IVCN01 .EQ. 8) IVCOMP =IVCOMP * 2                             01870328
      IF (RVCN01 .GE. 7.9995 .AND. RVCN01 .LE. 8.0005) IVCOMP = IVCOMP*301880328
      IF (.NOT. LVCN01) IVCOMP = IVCOMP * 5                             01890328
      IVCORR = 30                                                       01900328
40020 IF (IVCOMP - 30) 20020, 10020, 20020                              01910328
30020 IVDELE = IVDELE + 1                                               01920328
      WRITE (I02,80000) IVTNUM                                          01930328
      IF (ICZERO) 10020, 0031, 20020                                    01940328
10020 IVPASS = IVPASS + 1                                               01950328
      WRITE (I02,80002) IVTNUM                                          01960328
      GO TO 0031                                                        01970328
20020 IVFAIL = IVFAIL + 1                                               01980328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01990328
 0031 CONTINUE                                                          02000328
C                                                                       02010328
C     ****  FCVS PROGRAM 328  -  TEST 003  ****                         02020328
C                                                                       02030328
C     USE INTEGER, REAL AND LOGICAL ARRAY ELEMENT NAMES AS ACTUAL       02040328
C     ARGUMENTS.                                                        02050328
C                                                                       02060328
      IVTNUM =   3                                                      02070328
      IF (ICZERO) 30030, 0030, 30030                                    02080328
 0030 CONTINUE                                                          02090328
      IADN11(2) = 2                                                     02100328
      RADN11(4) = 4.0                                                   02110328
      LADN11(1) = .FALSE.                                               02120328
      CALL FS329(IADN11(2), RADN11(4), LADN11(1))                       02130328
      IVCOMP = 1                                                        02140328
      IF (IVCN01 .EQ. 3) IVCOMP = IVCOMP * 2                            02150328
      IF (RVCN01 .GE. 4.9995 .AND. RVCN01 .LE. 5.0005) IVCOMP = IVCOMP*302160328
      IF (LVCN01) IVCOMP = IVCOMP * 5                                   02170328
      IVCORR = 30                                                       02180328
40030 IF (IVCOMP - 30) 20030, 10030, 20030                              02190328
30030 IVDELE = IVDELE + 1                                               02200328
      WRITE (I02,80000) IVTNUM                                          02210328
      IF (ICZERO) 10030, 0041, 20030                                    02220328
10030 IVPASS = IVPASS + 1                                               02230328
      WRITE (I02,80002) IVTNUM                                          02240328
      GO TO 0041                                                        02250328
20030 IVFAIL = IVFAIL + 1                                               02260328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02270328
 0041 CONTINUE                                                          02280328
C                                                                       02290328
C     ****  FCVS PROGRAM 328  -  TEST 004  ****                         02300328
C                                                                       02310328
C     INTEGER AND REAL EXPRESSIONS INVOLVING OPERATORS AS ACTUAL        02320328
C     ARGUMENTS.                                                        02330328
C                                                                       02340328
      IVTNUM =   4                                                      02350328
      IF (ICZERO) 30040, 0040, 30040                                    02360328
 0040 CONTINUE                                                          02370328
      IVON02 = 2                                                        02380328
      IVON03 = 3                                                        02390328
      RVON02 = 2.                                                       02400328
      RVON03 = 1.2                                                      02410328
      CALL FS329(IVON02 + 3 * IVON03 - 7, RVON02 *RVON03 / .6, .TRUE.)  02420328
      IVCOMP = 1                                                        02430328
      IF (IVCN01 .EQ. 5) IVCOMP = IVCOMP * 2                            02440328
      IF (RVCN01 .GE. 4.9995 .AND. RVCN01 .LE. 5.0005) IVCOMP = IVCOMP*302450328
      IVCORR = 6                                                        02460328
40040 IF (IVCOMP -  6) 20040, 10040, 20040                              02470328
30040 IVDELE = IVDELE + 1                                               02480328
      WRITE (I02,80000) IVTNUM                                          02490328
      IF (ICZERO) 10040, 0051, 20040                                    02500328
10040 IVPASS = IVPASS + 1                                               02510328
      WRITE (I02,80002) IVTNUM                                          02520328
      GO TO 0051                                                        02530328
20040 IVFAIL = IVFAIL + 1                                               02540328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02550328
 0051 CONTINUE                                                          02560328
C                                                                       02570328
C     ****  FCVS PROGRAM 328  -  TEST 005  ****                         02580328
C                                                                       02590328
C     REAL EXPRESSION INVOLVING INTEGER AND REAL PRIMARIES AND OPERATORS02600328
C     AS ACTUAL ARGUMENT.                                               02610328
C                                                                       02620328
      IVTNUM =   5                                                      02630328
      IF (ICZERO) 30050, 0050, 30050                                    02640328
 0050 CONTINUE                                                          02650328
      RVCOMP = 0.0                                                      02660328
      IVON01 = 2                                                        02670328
      RADN11(2) = 2.5                                                   02680328
      CALL FS329(1, IVON01**3 * (RADN11(2) - 1) + 2.0, .TRUE.)          02690328
      RVCOMP = RVCN01                                                   02700328
      RVCORR = 15.0                                                     02710328
40050 IF (RVCOMP - 14.995) 20050, 10050, 40051                          02720328
40051 IF (RVCOMP - 15.005) 10050, 10050, 20050                          02730328
30050 IVDELE = IVDELE + 1                                               02740328
      WRITE (I02,80000) IVTNUM                                          02750328
      IF (ICZERO) 10050, 0061, 20050                                    02760328
10050 IVPASS = IVPASS + 1                                               02770328
      WRITE (I02,80002) IVTNUM                                          02780328
      GO TO 0061                                                        02790328
20050 IVFAIL = IVFAIL + 1                                               02800328
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02810328
 0061 CONTINUE                                                          02820328
C                                                                       02830328
C     ****  FCVS PROGRAM 328  -  TEST 006  ****                         02840328
C                                                                       02850328
C     LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.NOT.) AS ACTUAL   02860328
C     ARGUMENT.                                                         02870328
C                                                                       02880328
      IVTNUM =   6                                                      02890328
      IF (ICZERO) 30060, 0060, 30060                                    02900328
 0060 CONTINUE                                                          02910328
      LVON01 = .TRUE.                                                   02920328
      CALL FS329(1, 1.0, .NOT. LVON01)                                  02930328
      IVCOMP = 0                                                        02940328
      IF (LVCN01) IVCOMP = 1                                            02950328
      IVCORR = 1                                                        02960328
40060 IF (IVCOMP - 1) 20060, 10060, 20060                               02970328
30060 IVDELE = IVDELE + 1                                               02980328
      WRITE (I02,80000) IVTNUM                                          02990328
      IF (ICZERO) 10060, 0071, 20060                                    03000328
10060 IVPASS = IVPASS + 1                                               03010328
      WRITE (I02,80002) IVTNUM                                          03020328
      GO TO 0071                                                        03030328
20060 IVFAIL = IVFAIL + 1                                               03040328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03050328
 0071 CONTINUE                                                          03060328
C                                                                       03070328
C     ****  FCVS PROGRAM 328  -  TEST 007  ****                         03080328
C                                                                       03090328
C     LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.OR.) AS ACTIVE    03100328
C     ARGUMENT.                                                         03110328
C                                                                       03120328
      IVTNUM =   7                                                      03130328
      IF (ICZERO) 30070, 0070, 30070                                    03140328
 0070 CONTINUE                                                          03150328
      LVON01 = .TRUE.                                                   03160328
      LVON02 = .FALSE.                                                  03170328
      CALL FS329(1, 1.0, LVON01 .OR. LVON02)                            03180328
      IVCOMP = 0                                                        03190328
      IF (.NOT. LVCN01) IVCOMP = 1                                      03200328
      IVCORR = 1                                                        03210328
40070 IF (IVCOMP - 1) 20070, 10070, 20070                               03220328
30070 IVDELE = IVDELE + 1                                               03230328
      WRITE (I02,80000) IVTNUM                                          03240328
      IF (ICZERO) 10070, 0081, 20070                                    03250328
10070 IVPASS = IVPASS + 1                                               03260328
      WRITE (I02,80002) IVTNUM                                          03270328
      GO TO 0081                                                        03280328
20070 IVFAIL = IVFAIL + 1                                               03290328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03300328
 0081 CONTINUE                                                          03310328
C                                                                       03320328
C     ****  FCVS PROGRAM 328  -  TEST 008  ****                         03330328
C                                                                       03340328
C     LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.AND.) AS ACTUAL   03350328
C     ARGUMENT.                                                         03360328
C                                                                       03370328
      IVTNUM =   8                                                      03380328
      IF (ICZERO) 30080, 0080, 30080                                    03390328
 0080 CONTINUE                                                          03400328
      LVON01 = .FALSE.                                                  03410328
      LVON02 = .TRUE.                                                   03420328
      CALL FS329(1, 1.0, LVON01 .AND. LVON02)                           03430328
      IVCOMP = 0                                                        03440328
      IF (LVCN01) IVCOMP = 1                                            03450328
      IVCORR = 1                                                        03460328
40080 IF (IVCOMP - 1) 20080, 10080, 20080                               03470328
30080 IVDELE = IVDELE + 1                                               03480328
      WRITE (I02,80000) IVTNUM                                          03490328
      IF (ICZERO) 10080, 0091, 20080                                    03500328
10080 IVPASS = IVPASS + 1                                               03510328
      WRITE (I02,80002) IVTNUM                                          03520328
      GO TO 0091                                                        03530328
20080 IVFAIL = IVFAIL + 1                                               03540328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03550328
 0091 CONTINUE                                                          03560328
C                                                                       03570328
C     ****  FCVS PROGRAM 328  -  TEST 009  ****                         03580328
C                                                                       03590328
C     EXPRESSION ENCLOSED IN PARENTHESES AS ACTUAL ARGUMENT.            03600328
C                                                                       03610328
      IVTNUM =   9                                                      03620328
      IF (ICZERO) 30090, 0090, 30090                                    03630328
 0090 CONTINUE                                                          03640328
      IVCOMP = 0                                                        03650328
      IVON01 = 6                                                        03660328
      CALL FS329((IVON01 + 3), 1.0, .TRUE.)                             03670328
      IVCOMP = IVCN01                                                   03680328
      IVCORR = 10                                                       03690328
40090 IF (IVCOMP - 10) 20090, 10090, 20090                              03700328
30090 IVDELE = IVDELE + 1                                               03710328
      WRITE (I02,80000) IVTNUM                                          03720328
      IF (ICZERO) 10090, 0101, 20090                                    03730328
10090 IVPASS = IVPASS + 1                                               03740328
      WRITE (I02,80002) IVTNUM                                          03750328
      GO TO 0101                                                        03760328
20090 IVFAIL = IVFAIL + 1                                               03770328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03780328
 0101 CONTINUE                                                          03790328
C                                                                       03800328
C     ****  FCVS PROGRAM 328  -  TEST 010  ****                         03810328
C                                                                       03820328
C     INTEGER AND REAL INTRINSIC FUNCTION REFERENCES AS ACTUAL ARGUMENTS03830328
C                                                                       03840328
      IVTNUM =  10                                                      03850328
      IF (ICZERO) 30100, 0100, 30100                                    03860328
 0100 CONTINUE                                                          03870328
      RVON01 = 4.7                                                      03880328
      RVON02 = -5.2                                                     03890328
      CALL FS329(NINT(RVON01), ABS(RVON02), .TRUE.)                     03900328
      IVCOMP = 1                                                        03910328
      IF (IVCN01 .EQ. 6) IVCOMP = IVCOMP * 2                            03920328
      IF (RVCN01 .GE. 6.1995 .AND. RVCN01 .LE. 6.2005) IVCOMP = IVCOMP*303930328
      IVCORR = 6                                                        03940328
40100 IF (IVCOMP -  6) 20100, 10100, 20100                              03950328
30100 IVDELE = IVDELE + 1                                               03960328
      WRITE (I02,80000) IVTNUM                                          03970328
      IF (ICZERO) 10100, 0111, 20100                                    03980328
10100 IVPASS = IVPASS + 1                                               03990328
      WRITE (I02,80002) IVTNUM                                          04000328
      GO TO 0111                                                        04010328
20100 IVFAIL = IVFAIL + 1                                               04020328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04030328
 0111 CONTINUE                                                          04040328
C                                                                       04050328
C     ****  FCVS PROGRAM 328  -  TEST 011  ****                         04060328
C                                                                       04070328
C     EXTERNAL FUNCTION REFERENCE AS ACTUAL ARGUMENT.                   04080328
C                                                                       04090328
      IVTNUM =  11                                                      04100328
      IF (ICZERO) 30110, 0110, 30110                                    04110328
 0110 CONTINUE                                                          04120328
      IVCOMP = 0                                                        04130328
      IVON01 = 4                                                        04140328
      CALL FS329(FF330(IVON01), 1.0, .TRUE.)                            04150328
      IVCOMP = IVCN01                                                   04160328
      IVCORR = 6                                                        04170328
40110 IF (IVCOMP - 6) 20110, 10110, 20110                               04180328
30110 IVDELE = IVDELE + 1                                               04190328
      WRITE (I02,80000) IVTNUM                                          04200328
      IF (ICZERO) 10110, 0121, 20110                                    04210328
10110 IVPASS = IVPASS + 1                                               04220328
      WRITE (I02,80002) IVTNUM                                          04230328
      GO TO 0121                                                        04240328
20110 IVFAIL = IVFAIL + 1                                               04250328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04260328
 0121 CONTINUE                                                          04270328
C                                                                       04280328
C     ****  FCVS PROGRAM 328  -  TEST 012  ****                         04290328
C                                                                       04300328
C     USE ACTUAL ARGUMENT NAMES WHICH ARE IDENTICAL TO THE DUMMY        04310328
C     ARGUMENT NAMES.                                                   04320328
C                                                                       04330328
      IVTNUM =  12                                                      04340328
      IF (ICZERO) 30120, 0120, 30120                                    04350328
 0120 CONTINUE                                                          04360328
      IDON01 = 10                                                       04370328
      RDON01 = 10.0                                                     04380328
      LDON01 = .FALSE.                                                  04390328
      CALL FS329(IDON01, RDON01, LDON01)                                04400328
      IVCOMP = 1                                                        04410328
      IF (IVCN01 .EQ. 11) IVCOMP = IVCOMP * 2                           04420328
      IF (RVCN01 .GE. 10.995 .AND. RVCN01 .LE. 11.005) IVCOMP = IVCOMP*304430328
      IF (LVCN01) IVCOMP = IVCOMP * 5                                   04440328
      IVCORR = 30                                                       04450328
40120 IF (IVCOMP - 30) 20120, 10120, 20120                              04460328
30120 IVDELE = IVDELE + 1                                               04470328
      WRITE (I02,80000) IVTNUM                                          04480328
      IF (ICZERO) 10120, 0131, 20120                                    04490328
10120 IVPASS = IVPASS + 1                                               04500328
      WRITE (I02,80002) IVTNUM                                          04510328
      GO TO 0131                                                        04520328
20120 IVFAIL = IVFAIL + 1                                               04530328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04540328
 0131 CONTINUE                                                          04550328
C                                                                       04560328
C     ****  FCVS PROGRAM 328  -  TEST 013  ****                         04570328
C                                                                       04580328
C     USE INTEGER, REAL AND LOGICAL STATEMENT FUNCTION REFERENCES AS    04590328
C     ARGUMENT NAMES.                                                   04600328
C                                                                       04610328
      IVTNUM =  13                                                      04620328
      IF (ICZERO) 30130, 0130, 30130                                    04630328
 0130 CONTINUE                                                          04640328
      RVON01 = 5.0                                                      04650328
      CALL FS329(IFOS01(4), RFOS01(RVON01), LFOS01(.TRUE.))             04660328
      IVCOMP = 1                                                        04670328
      IF (IVCN01 .EQ. 6) IVCOMP = IVCOMP * 2                            04680328
      IF (RVCN01 .GE. 6.9995 .AND. RVCN01 .LE. 7.0005) IVCOMP = IVCOMP*304690328
      IF (LVCN01) IVCOMP = IVCOMP * 5                                   04700328
      IVCORR = 30                                                       04710328
40130 IF (IVCOMP - 30) 20130, 10130, 20130                              04720328
30130 IVDELE = IVDELE + 1                                               04730328
      WRITE (I02,80000) IVTNUM                                          04740328
      IF (ICZERO) 10130, 0141, 20130                                    04750328
10130 IVPASS = IVPASS + 1                                               04760328
      WRITE (I02,80002) IVTNUM                                          04770328
      GO TO 0141                                                        04780328
20130 IVFAIL = IVFAIL + 1                                               04790328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04800328
 0141 CONTINUE                                                          04810328
C                                                                       04820328
C     TEST 014 THROUGH TEST 019 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 04830328
C     OF ACTUAL ARGUMENTS TO ARRAY NAMES USED AS SUBROUTINE DUMMY       04840328
C     ARGUMENTS.                                                        04850328
C                                                                       04860328
C                                                                       04870328
C     ****  FCVS PROGRAM 328  -  TEST 014  ****                         04880328
C                                                                       04890328
C     USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       04900328
C     ARGUMENT ARRAY DECLARATOR IS IDENTICAL TO THE ASSOCIATED DUMMY    04910328
C     ARGUMENT ARRAY DECLARATOR.                                        04920328
C                                                                       04930328
      IVTNUM =  14                                                      04940328
      IF (ICZERO) 30140, 0140, 30140                                    04950328
 0140 CONTINUE                                                          04960328
      IVCOMP = 0                                                        04970328
      IADN12(1) = 1                                                     04980328
      IADN12(2) = 10                                                    04990328
      IADN12(3) = 100                                                   05000328
      IADN12(4) = 1000                                                  05010328
      CALL FS331(IADN12)                                                05020328
      IVCOMP = IVCN01                                                   05030328
      IVCORR = 1111                                                     05040328
40140 IF (IVCOMP - 1111) 20140, 10140, 20140                            05050328
30140 IVDELE = IVDELE + 1                                               05060328
      WRITE (I02,80000) IVTNUM                                          05070328
      IF (ICZERO) 10140, 0151, 20140                                    05080328
10140 IVPASS = IVPASS + 1                                               05090328
      WRITE (I02,80002) IVTNUM                                          05100328
      GO TO 0151                                                        05110328
20140 IVFAIL = IVFAIL + 1                                               05120328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05130328
 0151 CONTINUE                                                          05140328
C                                                                       05150328
C     ****  FCVS PROGRAM 328  -  TEST 015  ****                         05160328
C                                                                       05170328
C     USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE OF THE  05180328
C     ACTUAL ARGUMENT ARRAY IS LARGER THAN THE SIZE OF THE ASSOCIATED   05190328
C     DUMMY ARGUMENT ARRAY.                                             05200328
C                                                                       05210328
      IVTNUM =  15                                                      05220328
      IF (ICZERO) 30150, 0150, 30150                                    05230328
 0150 CONTINUE                                                          05240328
      IVCOMP = 0                                                        05250328
      IACN11(1) = 1                                                     05260328
      IACN11(2) = 10                                                    05270328
      IACN11(3) = 100                                                   05280328
      IACN11(4) = 1000                                                  05290328
      IACN11(5) = 10000                                                 05300328
      CALL FS331(IACN11)                                                05310328
      IVCOMP = IVCN01                                                   05320328
      IVCORR = 1111                                                     05330328
40150 IF (IVCOMP - 1111) 20150, 10150, 20150                            05340328
30150 IVDELE = IVDELE + 1                                               05350328
      WRITE (I02,80000) IVTNUM                                          05360328
      IF (ICZERO) 10150, 0161, 20150                                    05370328
10150 IVPASS = IVPASS + 1                                               05380328
      WRITE (I02,80002) IVTNUM                                          05390328
      GO TO 0161                                                        05400328
20150 IVFAIL = IVFAIL + 1                                               05410328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05420328
 0161 CONTINUE                                                          05430328
C                                                                       05440328
C     ****  FCVS PROGRAM 328  -  TEST 016  ****                         05450328
C                                                                       05460328
C     USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       05470328
C     ARGUMENT ARRAY DECLARATOR IS LARGER AND HAS MORE SUBSCRIPT        05480328
C     EXPRESSIONS THAN THE ASSOCIATED DUMMY ARGUMENT ARRAY DECLARATOR.  05490328
C                                                                       05500328
      IVTNUM =  16                                                      05510328
      IF (ICZERO) 30160, 0160, 30160                                    05520328
 0160 CONTINUE                                                          05530328
      IVCOMP = 0                                                        05540328
      IATN11(1,1) = 1                                                   05550328
      IATN11(2,1) = 10                                                  05560328
      IATN11(1,2) = 100                                                 05570328
      IATN11(2,2) = 1000                                                05580328
      IATN11(1,3) = 10000                                               05590328
      CALL FS331(IATN11)                                                05600328
      IVCOMP = IVCN01                                                   05610328
      IVCORR = 1111                                                     05620328
40160 IF (IVCOMP - 1111) 20160, 10160, 20160                            05630328
30160 IVDELE = IVDELE + 1                                               05640328
      WRITE (I02,80000) IVTNUM                                          05650328
      IF (ICZERO) 10160, 0171, 20160                                    05660328
10160 IVPASS = IVPASS + 1                                               05670328
      WRITE (I02,80002) IVTNUM                                          05680328
      GO TO 0171                                                        05690328
20160 IVFAIL = IVFAIL + 1                                               05700328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05710328
 0171 CONTINUE                                                          05720328
C                                                                       05730328
C     ****  FCVS PROGRAM 328  -  TEST 017  ****                         05740328
C                                                                       05750328
C     USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE      05760328
C     ASSOCIATED ACTUAL AND DUMMY ARRAY DECLARATORS ARE IDENTICAL.  ALL 05770328
C     ARRAY ELEMENTS OF THE ACTUAL ARRAY SHOULD BE PASSED TO THE        05780328
C     DUMMY ARRAY OF THE SUBROUTINE.                                    05790328
C                                                                       05800328
      IVTNUM =  17                                                      05810328
      IF (ICZERO) 30170, 0170, 30170                                    05820328
 0170 CONTINUE                                                          05830328
      RVCOMP = 0.0                                                      05840328
      RADN12(1) = 1.                                                    05850328
      RADN12(2) = 10.                                                   05860328
      RADN12(3) = 100.                                                  05870328
      RADN12(4) = 1000.                                                 05880328
      CALL FS332(RADN12(1))                                             05890328
      RVCOMP = RVCN01                                                   05900328
      RVCORR = 1111.                                                    05910328
40170 IF (RVCOMP - 1110.5) 20170, 10170, 40171                          05920328
40171 IF (RVCOMP - 1111.5) 10170, 10170, 20170                          05930328
30170 IVDELE = IVDELE + 1                                               05940328
      WRITE (I02,80000) IVTNUM                                          05950328
      IF (ICZERO) 10170, 0181, 20170                                    05960328
10170 IVPASS = IVPASS + 1                                               05970328
      WRITE (I02,80002) IVTNUM                                          05980328
      GO TO 0181                                                        05990328
20170 IVFAIL = IVFAIL + 1                                               06000328
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06010328
 0181 CONTINUE                                                          06020328
C                                                                       06030328
C     ****  FCVS PROGRAM 328  -  TEST 018  ****                         06040328
C                                                                       06050328
C     USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 06060328
C     OF THE ACTUAL ARGUMENT ARRAY IS LARGER AND HAS FEWER SUBSCRIPT    06070328
C     EXPRESSIONS THAN THE ASSOCIATED DUMMY ARRAY.  ONLY ACTUAL ARRAY   06080328
C     ELEMENTS WITH SUBSCRIPT VALUES OF 5, 6, 7 AND 8 ( OUT OF A        06090328
C     POSSIBLE 10 ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF      06100328
C     THE SUBROUTINE.                                                   06110328
C                                                                       06120328
      IVTNUM =  18                                                      06130328
      IF (ICZERO) 30180, 0180, 30180                                    06140328
 0180 CONTINUE                                                          06150328
      RVCOMP = 0.0                                                      06160328
      RACN11(4) = 1.                                                    06170328
      RACN11(5) = 10.                                                   06180328
      RACN11(6) = 100.                                                  06190328
      RACN11(7) = 1000.                                                 06200328
      RACN11(8) = 10000.                                                06210328
      RACN11(9) = 100000.                                               06220328
      CALL FS332(RACN11(5))                                             06230328
      RVCOMP = RVCN01                                                   06240328
      RVCORR =  11110.                                                  06250328
40180 IF (RVCOMP - 11105.) 20180, 10180, 40181                          06260328
40181 IF (RVCOMP - 11115.) 10180, 10180, 20180                          06270328
30180 IVDELE = IVDELE + 1                                               06280328
      WRITE (I02,80000) IVTNUM                                          06290328
      IF (ICZERO) 10180, 0191, 20180                                    06300328
10180 IVPASS = IVPASS + 1                                               06310328
      WRITE (I02,80002) IVTNUM                                          06320328
      GO TO 0191                                                        06330328
20180 IVFAIL = IVFAIL + 1                                               06340328
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06350328
 0191 CONTINUE                                                          06360328
C                                                                       06370328
C     ****  FCVS PROGRAM 328  -  TEST 019  ****                         06380328
C                                                                       06390328
C     USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 06400328
C     OF THE ACTUAL ARGUMENT ARRAY IS LARGE THAN THE SIZE OF THE        06410328
C     ASSOCIATED DUMMY ARGUMENT ARRAY.  ONLY ACTUAL ARRAY ELEMENTS WITH 06420328
C     SUBSCRIPT VALUES OF 9, 10, 11 AND 12 (OUT OF A POSSIBLE 12        06430328
C     ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF THE SUBROUTINE.  06440328
C                                                                       06450328
      IVTNUM =  19                                                      06460328
      IF (ICZERO) 30190, 0190, 30190                                    06470328
 0190 CONTINUE                                                          06480328
      RVCOMP = 0.0                                                      06490328
      RATN11(2,3) = 1.                                                  06500328
      RATN11(3,3) = 10.                                                 06510328
      RATN11(1,4) = 100.                                                06520328
      RATN11(2,4) = 1000.                                               06530328
      RATN11(3,4) = 10000.                                              06540328
      CALL FS332(RATN11(3,3))                                           06550328
      RVCOMP = RVCN01                                                   06560328
      RVCORR = 11110.                                                   06570328
40190 IF (RVCOMP - 11105.) 20190, 10190, 40191                          06580328
40191 IF (RVCOMP - 11115.) 10190, 10190, 20190                          06590328
30190 IVDELE = IVDELE + 1                                               06600328
      WRITE (I02,80000) IVTNUM                                          06610328
      IF (ICZERO) 10190, 0201, 20190                                    06620328
10190 IVPASS = IVPASS + 1                                               06630328
      WRITE (I02,80002) IVTNUM                                          06640328
      GO TO 0201                                                        06650328
20190 IVFAIL = IVFAIL + 1                                               06660328
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06670328
 0201 CONTINUE                                                          06680328
C                                                                       06690328
C     TEST 020 THROUGH TEST 022 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 06700328
C     OF ACTUAL ARGUMENTS TO PROCEDURES USED AS SUBROUTINE DUMMY        06710328
C     ARGUMENTS.  ACTUAL ARGUMENTS TESTED INCLUDE THE NAMES OF AN       06720328
C     EXTERNAL FUNCTION, AN INTRINSIC FUNCTION AND A SUBROUTINE.        06730328
C                                                                       06740328
C                                                                       06750328
C     ****  FCVS PROGRAM 328  -  TEST 020  ****                         06760328
C                                                                       06770328
C     USE AN EXTERNAL FUNCTION NAME AS AN ACTUAL ARGUMENT.              06780328
C                                                                       06790328
      IVTNUM =  20                                                      06800328
      IF (ICZERO) 30200, 0200, 30200                                    06810328
 0200 CONTINUE                                                          06820328
      IVCOMP = 0                                                        06830328
      CALL FS333(FF330, 5)                                              06840328
      IVCOMP = IVCN01                                                   06850328
      IVCORR = 7                                                        06860328
40200 IF (IVCOMP - 7) 20200, 10200, 20200                               06870328
30200 IVDELE = IVDELE + 1                                               06880328
      WRITE (I02,80000) IVTNUM                                          06890328
      IF (ICZERO) 10200, 0211, 20200                                    06900328
10200 IVPASS = IVPASS + 1                                               06910328
      WRITE (I02,80002) IVTNUM                                          06920328
      GO TO 0211                                                        06930328
20200 IVFAIL = IVFAIL + 1                                               06940328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06950328
 0211 CONTINUE                                                          06960328
C                                                                       06970328
C     ****  FCVS PROGRAM 328  -  TEST 021  ****                         06980328
C                                                                       06990328
C     USE AN INTRINSIC FUNCTION NAME AS AN ACTUAL ARGUMENT.             07000328
C                                                                       07010328
      IVTNUM =  21                                                      07020328
      IF (ICZERO) 30210, 0210, 30210                                    07030328
 0210 CONTINUE                                                          07040328
      IVCOMP = 0                                                        07050328
      CALL FS333(IABS, -7)                                              07060328
      IVCOMP = IVCN01                                                   07070328
      IVCORR = 8                                                        07080328
40210 IF (IVCOMP - 8) 20210, 10210, 20210                               07090328
30210 IVDELE = IVDELE + 1                                               07100328
      WRITE (I02,80000) IVTNUM                                          07110328
      IF (ICZERO) 10210, 0221, 20210                                    07120328
10210 IVPASS = IVPASS + 1                                               07130328
      WRITE (I02,80002) IVTNUM                                          07140328
      GO TO 0221                                                        07150328
20210 IVFAIL = IVFAIL + 1                                               07160328
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07170328
 0221 CONTINUE                                                          07180328
C                                                                       07190328
C     ****  FCVS PROGRAM 328  -  TEST 022  ****                         07200328
C                                                                       07210328
C     USE A SUBROUTINE NAME AS AN ACTUAL ARGUMENT.                      07220328
C                                                                       07230328
      IVTNUM =  22                                                      07240328
      IF (ICZERO) 30220, 0220, 30220                                    07250328
 0220 CONTINUE                                                          07260328
      RVCOMP = 0.0                                                      07270328
      RVON01 = 3.5                                                      07280328
      CALL FS334(FS335, RVON01)                                         07290328
      RVCOMP = RVCN01                                                   07300328
      RVCORR = 5.5                                                      07310328
40220 IF (RVCOMP - 5.4995) 20220, 10220, 40221                          07320328
40221 IF (RVCOMP - 5.5005) 10220, 10220, 20220                          07330328
30220 IVDELE = IVDELE + 1                                               07340328
      WRITE (I02,80000) IVTNUM                                          07350328
      IF (ICZERO) 10220, 0231, 20220                                    07360328
10220 IVPASS = IVPASS + 1                                               07370328
      WRITE (I02,80002) IVTNUM                                          07380328
      GO TO 0231                                                        07390328
20220 IVFAIL = IVFAIL + 1                                               07400328
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07410328
 0231 CONTINUE                                                          07420328
C                                                                       07430328
C                                                                       07440328
C     WRITE OUT TEST SUMMARY                                            07450328
C                                                                       07460328
      WRITE (I02,90004)                                                 07470328
      WRITE (I02,90014)                                                 07480328
      WRITE (I02,90004)                                                 07490328
      WRITE (I02,90000)                                                 07500328
      WRITE (I02,90004)                                                 07510328
      WRITE (I02,90020) IVFAIL                                          07520328
      WRITE (I02,90022) IVPASS                                          07530328
      WRITE (I02,90024) IVDELE                                          07540328
      STOP                                                              07550328
90001 FORMAT (1H ,24X,5HFM328)                                          07560328
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM328)                          07570328
C                                                                       07580328
C     FORMATS FOR TEST DETAIL LINES                                     07590328
C                                                                       07600328
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   07610328
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07620328
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07630328
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07640328
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        07650328
C                                                                       07660328
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07670328
C                                                                       07680328
90002 FORMAT (1H1)                                                      07690328
90004 FORMAT (1H )                                                      07700328
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07710328
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   07720328
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         07730328
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  07740328
90014 FORMAT (1H ,5X,46H----------------------------------------------) 07750328
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07760328
C                                                                       07770328
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 07780328
C                                                                       07790328
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              07800328
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              07810328
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             07820328
      END                                                               07830328
      SUBROUTINE FS329(IDON01, RDON01, LDON01)                          00010329
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS SUBROUTINE IS USED BY VARIOUS TESTS IN THE MAIN PROGRAM 00020329
C     FM328 TO TEST THE DIFFERENT FORMS OF INTEGER, REAL AND LOGICAL    00030329
C     ACTUAL ARGUMENTS THAT CAN BE ASSOCIATED WITH INTEGER, REAL AND    00040329
C     LOGICAL DUMMY ARGUMENTS.  THIS ROUTINE INCREMENTS THE INTEGER     00050329
C     AND REAL ARGUMENTS BY ONE AND NEGATES THE LOGICAL ARGUMENT.  ALL  00060329
C     RESULTS ARE THEN RETURNED TO FM328 VIA VARIABLES IN NAMED COMMON. 00070329
      IMPLICIT LOGICAL (L)                                              00080329
      COMMON /BLK1/ IVCN01, RVCN01, LVCN01                              00090329
      IVCN01 = IDON01 + 1                                               00100329
      RVCN01 = RDON01 + 1.0                                             00110329
      LVCN01 = .NOT. LDON01                                             00120329
      RETURN                                                            00130329
      END                                                               00140329
      INTEGER FUNCTION FF330(IDON02)                                    00010330
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C         THIS FUNCTION IS USED BY TEST 011 OF THE MAIN PROGRAM FM328 TO00020330
C     TEST THE USE OF AN EXTERNAL FUNCTION REFERENCE AS AN ACTUAL       00030330
C     ARGUMENT WHEN THE ASSOCIATED DUMMY ARGUMENT IS A VARIABLE NAME.   00040330
C     THIS FUNCTION IS ALSO REFERENCED FROM SUBROUTINE FS333 VIA A      00050330
C     DUMMY PROCEDURE NAME REFERENCE.  THIS FUNCTION INCREMENTS THE     00060330
C     ARGUMENT VALUE BY ONE AND RETURNS THE RESULT AS THE FUNCTION      00070330
C     VALUE.                                                            00080330
      FF330 = IDON02 + 1                                                00090330
      RETURN                                                            00100330
      END                                                               00110330
      SUBROUTINE FS331(IDDN11)                                          00010331
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS SUBROUTINE IS USED BY VARIOUS TESTS IN THE MAIN PROGRAM 00020331
C     FM328 TO TEST THE USE OF AN ARRAY NAME AS AN ACTUAL ARGUMENT WHEN 00030331
C     THE ASSOCIATED DUMMY ARGUMENT IS AN ARRAY NAME.  THIS ROUTINE     00040331
C     ADDS TOGETHER THE FOUR ELEMENTS IN THE DUMMY ARGUMENT ARRAY AND   00050331
C     RETURNS THE RESULTS VIA A VARIABLE IN NAMED COMMON.               00060331
      LOGICAL LVCN01                                                    00070331
      DIMENSION IDDN11(4)                                               00080331
      COMMON /BLK1/IVCN01, RVCN01, LVCN01                               00090331
      IVCN01 = IDDN11(1) + IDDN11(2) + IDDN11(3) + IDDN11(4)            00100331
      RETURN                                                            00110331
      END                                                               00120331
      SUBROUTINE FS332(RDTN21)                                          00010332
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS SUBROUTINE IS USED BY VARIOUS TESTS IN THE MAIN PROGRAM 00020332
C     FM328 TO TEST THE USE OF AN ARRAY ELEMENT NAME AS AN ACTUAL       00030332
C     ARGUMENT WHEN THE ASSOCIATED DUMMY ARGUMENT IS AN ARRAY NAME.     00040332
C     THIS ROUTINE ADDS TOGETHER THE FOUR ELEMENTS IN THE DUMMY         00050332
C     ARGUMENT ARRAY AND RETURNS THE RESULT VIA A VARIABLE IN NAMED     00060332
C     COMMON.                                                           00070332
      IMPLICIT LOGICAL (L)                                              00080332
      REAL RDTN21(2,2)                                                  00090332
      COMMON /BLK1/IVCN01, RVCN01, LVCN01                               00100332
      RVCN01 = RDTN21(1,1) + RDTN21(2,1) + RDTN21(1,2) + RDTN21(2,2)    00110332
      RETURN                                                            00120332
      END                                                               00130332
      SUBROUTINE FS333(NINT, IDON03)                                    00010333
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS SUBROUTINE IS USED BY TESTS 020 AND 021 OF THE MAIN     00020333
C     PROGRAM FM328 TO TEST THE USE OF EXTERNAL AND INTRINSIC FUNCTION  00030333
C     NAMES AS ACTUAL ARGUMENTS WHEN THE ASSOCIATED DUMMY ARGUMENT IS A 00040333
C     PROCEDURE NAME.  THIS SUBROUTINE REFERENCES THE EXTERNAL FUNCTION 00050333
C     FF330 OR THE INTRINSIC FUNCTION IABS DEPENDING ON THE ACTUAL      00060333
C     ARGUMENT PASSED TO IT.  THE RESULT OF THIS FUNCTION REFERENCE IS  00070333
C     THEN INCREMENTED BY ONE AND THE RESULT IS RETURNED TO FS328 VIA   00080333
C     A VARIABLE IN NAMED COMMON.                                       00090333
      IMPLICIT LOGICAL (L)                                              00100333
      COMMON /BLK1/IVCN01, RVCN01, LVCN01                               00110333
      IVCN01 = NINT(IDON03) + 1                                         00120333
C              **** THE NAME NINT IS A DUMMY ARGUMENT NAME              00130333
C                     AND NOT AN INTRINSIC FUNCTION REFERENCE ****      00140333
      RETURN                                                            00150333
      END                                                               00160333
      SUBROUTINE FS334(IDON06, RDON03)                                  00010334
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS SUBROUTINE IS USED BY TEST 022 OF THE MAIN PROGRAM      00020334
C     FM328 TO TEST THE USE OF A SUBROUTINE NAME AS AN ACTUAL ARGUMENT  00030334
C     WHEN THE ASSOCIATED DUMMY ARGUMENT IS A PROCEDURE NAME.  THIS     00040334
C     SUBROUTINE CALLS THE SUBROUTINE FS335 VIA A DUMMY PROCEDURE NAME  00050334
C     REFERENCE.  THE ARGUMENT VALUE WHICH IS RETURNED FROM THE FS335   00060334
C     REFERENCE IS THEN INCREMENTED BY ONE AND RETURNED TO FM328 VIA    00070334
C     A VARIABLE IN NAMED COMMON.                                       00080334
      IMPLICIT LOGICAL (L)                                              00090334
      COMMON /BLK1/IVCN01, RVCN01, LVCN01                               00100334
      CALL IDON06(RDON03)                                               00110334
      RVCN01 = RDON03 + 1.0                                             00120334
      RETURN                                                            00130334
      END                                                               00140334
      SUBROUTINE FS335(RDON04)                                          00010335
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS SUBROUITNE IS USED BY TEST 022 OF THE MAIN PROGRAM FM32800020335
C     TO TEST THE USE OF A SUBROUTINE NAME AS AN ACTUAL ARGUMENT WHEN   00030335
C     THE ASSOCIATED DUMMY ARGUMENT IS A PROCEDURE NAME.  FS335 IS      00040335
C     CALLED FROM SUBROUTINE FS334 VIA A DUMMY PROCEDURE NAME REFERENCE.00050335
C     THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY ONE.                00060335
      RDON04 = RDON04 + 1.0                                             00070335
      RETURN                                                            00080335
      END                                                               00090335
