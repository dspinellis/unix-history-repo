      PROGRAM FM311                                                     00010311
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020311
C                                                                       00030311
C        THIS ROUTINE TESTS THE USE OF THE FORTRAN IN-LINE STATEMENT    00040311
C     FUNCTION OF TYPES INTEGER, REAL AND LOGICAL.  SPECIFIC FEATURES   00050311
C     TESTED INCLUDE,                                                   00060311
C                                                                       00070311
C        A) REAL STATEMENT FUNCTIONS USING REAL CONSTANTS AND VARIABLES 00080311
C           IN THE EXPRESSION AND AS ACTUAL ARGUMENTS.                  00090311
C                                                                       00100311
C        B) STATEMENT FUNCTIONS WHICH REQUIRE CONVERSION OF THE         00110311
C           EXPRESSION TO REAL AND INTEGER TYPING.                      00120311
C                                                                       00130311
C        C) THE USE OF VARIABLES, ARRAY ELEMENTS, EXTERNAL REFERENCES,  00140311
C           AND INITIALLY DEFINED ENITIIES IN THE EXPRESSION.           00150311
C                                                                       00160311
C        D) VARIOUS DEFINITIONS AND USES OF DUMMY ARGUMENTS.            00170311
C                                                                       00180311
C        E) ACTUAL ARGUMENTS CONSISTING OF EXPRESSIONS, INTRINSIC       00190311
C           FUNCTION REFERENCES, AND EXTERNAL FUNCTION REFERENCES.      00200311
C                                                                       00210311
C        F) CONFIRMING AND OVERRIDING THE TYPING OF STATEMENT FUNCTIONS 00220311
C           AND DUMMY ARGUMENTS.                                        00230311
C                                                                       00240311
C        G) USE OF STATEMENT FUNCTIONS AND DUMMY ARGUMENTS IN THE MAIN  00250311
C           PROGRAM AND IN EXTERNAL FUNCTION AND SUBROUTINE SUBPROGRAMS.00260311
C                                                                       00270311
C     THE SUBSET LEVEL FEATURES OF STATEMENT FUNCTIONS ARE ALSO TESTED  00280311
C     IN ROUTINE FM020.                                                 00290311
C                                                                       00300311
C     REFERENCES.                                                       00310311
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00320311
C           X3.9-1978                                                   00330311
C                                                                       00340311
C        SECTION 8.3,    COMMON STATEMENT                               00350311
C        SECTION 8.4,    TYPE-STATEMENT                                 00360311
C        SECTION 8.5,    IMPLICIT STATEMENT                             00370311
C        SECTION 8.7,    EXTERNAL STATEMENT                             00380311
C        SECTION 8.8,    INTRINSIC STATEMENT                            00390311
C        SECTION 9,      DATA STATEMENT                                 00400311
C        SECTION 15.3,   INTRINSIC FUNCTIONS                            00410311
C        SECTION 15.4,   STATEMENT FUNCTION                             00420311
C        SECTION 15.5,   EXTERNAL FUNCTIONS                             00430311
C        SECTION 15.6,   SUBROUTINES                                    00440311
C        SECTION 15.9.1, DUMMY ARGUMENTS                                00450311
C        SECTION 15.9.2, ACTUAL ARGUMENTS                               00460311
C        SECTION 15.9.3, ASSOCIATION OF DUMMY AND ACTUAL ARGUMENTS      00470311
C                                                                       00480311
C                                                                       00490311
C     ******************************************************************00500311
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00510311
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00520311
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00530311
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00540311
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00550311
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00560311
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00570311
C     THE RESULT OF EXECUTING THESE TESTS.                              00580311
C                                                                       00590311
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00600311
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00610311
C                                                                       00620311
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00630311
C                    DEPARTMENT OF THE NAVY                             00640311
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00650311
C                    WASHINGTON, D.C.   20376                           00660311
C                                                                       00670311
C     ******************************************************************00680311
C                                                                       00690311
C                                                                       00700311
      IMPLICIT LOGICAL (L)                                              00710311
      IMPLICIT CHARACTER*14 (C)                                         00720311
C                                                                       00730311
      IMPLICIT INTEGER (A)                                              00740311
      IMPLICIT INTEGER (B)                                              00750311
      IMPLICIT REAL (K)                                                 00760311
      IMPLICIT REAL (M)                                                 00770311
      REAL NDON01                                                       00780311
      INTEGER EDON01                                                    00790311
      INTEGER FF312, FF314                                              00800311
      EXTERNAL FF312                                                    00810311
      INTRINSIC NINT                                                    00820311
      DIMENSION RADN11(4), RADN12(4), RADN13(4)                         00830311
      DIMENSION IADN11(4), IADN12(4)                                    00840311
      DIMENSION LADN11(4)                                               00850311
      COMMON /IFOS19/IVCN01                                             00860311
      DATA IVOND1/6/                                                    00870311
C     TEST 001                                                          00880311
               RFOS01(RDON01) = 3.5                                     00890311
C     TEST 002                                                          00900311
               RFOS02(RDON02) = RDON02                                  00910311
C     TEST 003                                                          00920311
               RFOS03(RDON03) = RDON03 + 1.0                            00930311
C     TEST 004                                                          00940311
               IFOS01(RDON04) = RDON04 + 1.0                            00950311
C     TEST 005                                                          00960311
               RFOS04(IDON01) = IDON01 + 1                              00970311
C     TEST 006                                                          00980311
               IFOS02(IDON02) = IDON02 + 1.95                           00990311
C     TEST 007                                                          01000311
               IFOS03(IDON03) = IDON03 + IVON01                         01010311
C     TEST 008                                                          01020311
               RFOS05(RDON05) = RDON05 + RVON02                         01030311
C     TEST 009                                                          01040311
               LFOS01(LDON01) = LDON01 .OR. LVON01                      01050311
C     TEST 010                                                          01060311
               IFOS04(IDON04) = IDON04 + IADN11(1)                      01070311
C     TEST 011                                                          01080311
               RFOS06(RDON06) = RDON06 + RADN12(3)                      01090311
C     TEST 012                                                          01100311
               LFOS02(LDON02) = .NOT. LDON02 .AND. LADN11(2)            01110311
C     TEST 013                                                          01120311
               RFOS07(IDON05) = RADN13(IDON05)                          01130311
C     TEST 014                                                          01140311
               IFOS05(IDON06) = IDON06 + FF312(4)                       01150311
C     TEST 015                                                          01160311
               IFOS06(IDON07) = (IDON07 + 1)                            01170311
C     TEST 016                                                          01180311
               IFOS07(IDON08) = IDON08 + IVOND1                         01190311
C     TEST 017                                                          01200311
               IFOS08(IDON09) = IDON09 + 1                              01210311
               IFOS09(IDON10) = IFOS08(IDON10) + 1                      01220311
C     TEST 018                                                          01230311
               IFOS10() = IVON02                                        01240311
C     TEST 019                                                          01250311
               IFOS11(IDON11,IDON12,IDON13) = IDON11 + IDON12 + IDON13  01260311
C     TEST 020                                                          01270311
               IFOS12(IDON14) = IDON14 + 1                              01280311
               IFOS13(IDON14) = IDON14 + 2                              01290311
C     TEST 021,022,023                                                  01300311
               IFOS14(IDON15) = IDON15 + 1                              01310311
C     TEST 024                                                          01320311
               KFOS01(IDON16) = IDON16 + 1.0                            01330311
C     TEST 025                                                          01340311
               AFOS01(RDON07) = RDON07 + 1.0                            01350311
C     TEST 026                                                          01360311
               RFOS08(MDON01) = MDON01 / 5                              01370311
C     TEST 027                                                          01380311
               RFOS09(BDON01) = BDON01 / 5                              01390311
C     TEST 028                                                          01400311
               RFOS10(NDON01) = NDON01 / 5                              01410311
C     TEST 029                                                          01420311
               RFOS11(EDON01) = EDON01 / 5                              01430311
C     TEST 030                                                          01440311
               IFOS15(IVON04) = IVON04 + 1                              01450311
C     TEST 031                                                          01460311
               IFOS16(IDON17) = IDON17 + 1                              01470311
C     TEST 032                                                          01480311
               IFOS17(IDON18) = IDON18 + 1                              01490311
C     TEST 037                                                          01500311
               IFOS19(IDON21) = IDON21 + 1                              01510311
C                                                                       01520311
C                                                                       01530311
C                                                                       01540311
C     INITIALIZATION SECTION.                                           01550311
C                                                                       01560311
C     INITIALIZE CONSTANTS                                              01570311
C     ********************                                              01580311
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01590311
      I01 = 5                                                           01600311
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01610311
      I02 = 6                                                           01620311
C     SYSTEM ENVIRONMENT SECTION                                        01630311
C                                                                       01640311
      I01 = 5                                                           01650311
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01660311
C     (UNIT NUMBER FOR CARD READER).                                    01670311
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01680311
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01690311
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01700311
C                                                                       01710311
      I02 = 6                                                           01720311
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01730311
C     (UNIT NUMBER FOR PRINTER).                                        01740311
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01750311
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01760311
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01770311
C                                                                       01780311
      IVPASS = 0                                                        01790311
      IVFAIL = 0                                                        01800311
      IVDELE = 0                                                        01810311
      ICZERO = 0                                                        01820311
C                                                                       01830311
C     WRITE OUT PAGE HEADERS                                            01840311
C                                                                       01850311
      WRITE (I02,90002)                                                 01860311
      WRITE (I02,90006)                                                 01870311
      WRITE (I02,90008)                                                 01880311
      WRITE (I02,90004)                                                 01890311
      WRITE (I02,90010)                                                 01900311
      WRITE (I02,90004)                                                 01910311
      WRITE (I02,90016)                                                 01920311
      WRITE (I02,90001)                                                 01930311
      WRITE (I02,90004)                                                 01940311
      WRITE (I02,90012)                                                 01950311
      WRITE (I02,90014)                                                 01960311
      WRITE (I02,90004)                                                 01970311
C                                                                       01980311
C                                                                       01990311
C     TEST 001 THROUGH TEST 003 TEST REAL STATEMENT FUNCTIONS WHERE THE 02000311
C     EXPRESSION CONSISTS OF REAL CONSTANTS AND VARIABLES AND THE ACTUAL02010311
C     ARGUMENTS ARE EITHER REAL CONSTANTS OR VARIABLES.                 02020311
C                                                                       02030311
C                                                                       02040311
C     ****  FCVS PROGRAM 311  -  TEST 001  ****                         02050311
C                                                                       02060311
C     EXPRESSION CONSISTS OF REAL CONSTANT (NO DUMMY ARGUMENT).         02070311
C                                                                       02080311
      IVTNUM =   1                                                      02090311
      IF (ICZERO) 30010, 0010, 30010                                    02100311
 0010 CONTINUE                                                          02110311
      RVCOMP = 0.0                                                      02120311
      RVCOMP = RFOS01(1.0)                                              02130311
      RVCORR = 3.5                                                      02140311
40010 IF (RVCOMP - 3.4995) 20010, 10010, 40011                          02150311
40011 IF (RVCOMP - 3.5005) 10010, 10010, 20010                          02160311
30010 IVDELE = IVDELE + 1                                               02170311
      WRITE (I02,80000) IVTNUM                                          02180311
      IF (ICZERO) 10010, 0021, 20010                                    02190311
10010 IVPASS = IVPASS + 1                                               02200311
      WRITE (I02,80002) IVTNUM                                          02210311
      GO TO 0021                                                        02220311
20010 IVFAIL = IVFAIL + 1                                               02230311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02240311
 0021 CONTINUE                                                          02250311
C                                                                       02260311
C     ****  FCVS PROGRAM 311  -  TEST 002  ****                         02270311
C                                                                       02280311
C     DUMMY ARGUMENT USED IN EXPRESSION AND ACTUAL ARGUMENT IS REAL     02290311
C     CONSTANT.                                                         02300311
C                                                                       02310311
      IVTNUM =   2                                                      02320311
      IF (ICZERO) 30020, 0020, 30020                                    02330311
 0020 CONTINUE                                                          02340311
      RVCOMP = 0.0                                                      02350311
      RVCOMP = RFOS02(1.3333)                                           02360311
      RVCORR = 1.3333                                                   02370311
40020 IF (RVCOMP - 1.3328) 20020, 10020, 40021                          02380311
40021 IF (RVCOMP - 1.3338) 10020, 10020, 20020                          02390311
30020 IVDELE = IVDELE + 1                                               02400311
      WRITE (I02,80000) IVTNUM                                          02410311
      IF (ICZERO) 10020, 0031, 20020                                    02420311
10020 IVPASS = IVPASS + 1                                               02430311
      WRITE (I02,80002) IVTNUM                                          02440311
      GO TO 0031                                                        02450311
20020 IVFAIL = IVFAIL + 1                                               02460311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02470311
 0031 CONTINUE                                                          02480311
C                                                                       02490311
C     ****  FCVS PROGRAM 311  -  TEST 003  ****                         02500311
C                                                                       02510311
C     DUMMY ARGUMENT USED IN EXPRESSION AND ACTUAL ARGUMENT IS REAL     02520311
C     VARIABLE.                                                         02530311
C                                                                       02540311
      IVTNUM =   3                                                      02550311
      IF (ICZERO) 30030, 0030, 30030                                    02560311
 0030 CONTINUE                                                          02570311
      RVCOMP = 0.0                                                      02580311
      RVON01 = 4.5                                                      02590311
      RVCOMP = RFOS03(RVON01)                                           02600311
      RVCORR = 5.5                                                      02610311
40030 IF (RVCOMP - 5.4995) 20030, 10030, 40031                          02620311
40031 IF (RVCOMP - 5.5005) 10030, 10030, 20030                          02630311
30030 IVDELE = IVDELE + 1                                               02640311
      WRITE (I02,80000) IVTNUM                                          02650311
      IF (ICZERO) 10030, 0041, 20030                                    02660311
10030 IVPASS = IVPASS + 1                                               02670311
      WRITE (I02,80002) IVTNUM                                          02680311
      GO TO 0041                                                        02690311
20030 IVFAIL = IVFAIL + 1                                               02700311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02710311
 0041 CONTINUE                                                          02720311
C                                                                       02730311
C     TEST 004 THROUGH TEST 006 TEST STATEMENT FUNCTIONS WHICH REQUIRE  02740311
C     TYPE CONVERSION OF THE EXPRESSION.                                02750311
C                                                                       02760311
C                                                                       02770311
C     ****  FCVS PROGRAM 311  -  TEST 004  ****                         02780311
C                                                                       02790311
C     INTEGER STATEMENT FUNCTION WITH REAL EXPRESSION.                  02800311
C                                                                       02810311
      IVTNUM =   4                                                      02820311
      IF (ICZERO) 30040, 0040, 30040                                    02830311
 0040 CONTINUE                                                          02840311
      IVCOMP = 0                                                        02850311
      IVCOMP = IFOS01(2.3)                                              02860311
      IVCORR = 3                                                        02870311
40040 IF (IVCOMP - 3) 20040, 10040, 20040                               02880311
30040 IVDELE = IVDELE + 1                                               02890311
      WRITE (I02,80000) IVTNUM                                          02900311
      IF (ICZERO) 10040, 0051, 20040                                    02910311
10040 IVPASS = IVPASS + 1                                               02920311
      WRITE (I02,80002) IVTNUM                                          02930311
      GO TO 0051                                                        02940311
20040 IVFAIL = IVFAIL + 1                                               02950311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02960311
 0051 CONTINUE                                                          02970311
C                                                                       02980311
C     ****  FCVS PROGRAM 311  -  TEST 005  ****                         02990311
C                                                                       03000311
C     REAL STATEMENT FUNCTION WITH INTEGER EXPRESSION                   03010311
C                                                                       03020311
      IVTNUM =   5                                                      03030311
      IF (ICZERO) 30050, 0050, 30050                                    03040311
 0050 CONTINUE                                                          03050311
      RVCOMP = 0.0                                                      03060311
      RVCOMP = RFOS04(3)                                                03070311
      RVCORR = 4.0                                                      03080311
40050 IF (RVCOMP - 3.9995) 20050, 10050, 40051                          03090311
40051 IF (RVCOMP - 4.0005) 10050, 10050, 20050                          03100311
30050 IVDELE = IVDELE + 1                                               03110311
      WRITE (I02,80000) IVTNUM                                          03120311
      IF (ICZERO) 10050, 0061, 20050                                    03130311
10050 IVPASS = IVPASS + 1                                               03140311
      WRITE (I02,80002) IVTNUM                                          03150311
      GO TO 0061                                                        03160311
20050 IVFAIL = IVFAIL + 1                                               03170311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03180311
 0061 CONTINUE                                                          03190311
C                                                                       03200311
C     ****  FCVS PROGRAM 311  -  TEST 006  ****                         03210311
C                                                                       03220311
C     INTEGER STATEMENT FUNCTION WITH EXPRESSION CONSISTING OF INTEGER  03230311
C     AND REAL PRIMARIES.                                               03240311
C                                                                       03250311
      IVTNUM =   6                                                      03260311
      IF (ICZERO) 30060, 0060, 30060                                    03270311
 0060 CONTINUE                                                          03280311
      IVCOMP = 0                                                        03290311
      IVCOMP = IFOS02(2)                                                03300311
      IVCORR = 3                                                        03310311
40060 IF (IVCOMP - 3) 20060, 10060, 20060                               03320311
30060 IVDELE = IVDELE + 1                                               03330311
      WRITE (I02,80000) IVTNUM                                          03340311
      IF (ICZERO) 10060, 0071, 20060                                    03350311
10060 IVPASS = IVPASS + 1                                               03360311
      WRITE (I02,80002) IVTNUM                                          03370311
      GO TO 0071                                                        03380311
20060 IVFAIL = IVFAIL + 1                                               03390311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03400311
 0071 CONTINUE                                                          03410311
C                                                                       03420311
C     TEST 007 THROUGH TEST 017 TEST THE USAGE OF VARIOUS PRIMARIES     03430311
C     IN THE EXPRESSION OF A STATEMENT FUNCTION.                        03440311
C                                                                       03450311
C                                                                       03460311
C     ****  FCVS PROGRAM 311  -  TEST 007  ****                         03470311
C                                                                       03480311
C     USE INTEGER VARIABLE AS PRIMARY                                   03490311
C                                                                       03500311
      IVTNUM =   7                                                      03510311
      IF (ICZERO) 30070, 0070, 30070                                    03520311
 0070 CONTINUE                                                          03530311
      IVCOMP = 0                                                        03540311
      IVON01 = 3                                                        03550311
      IVCOMP = IFOS03(4)                                                03560311
      IVCORR = 7                                                        03570311
40070 IF (IVCOMP - 7) 20070, 10070, 20070                               03580311
30070 IVDELE = IVDELE + 1                                               03590311
      WRITE (I02,80000) IVTNUM                                          03600311
      IF (ICZERO) 10070, 0081, 20070                                    03610311
10070 IVPASS = IVPASS + 1                                               03620311
      WRITE (I02,80002) IVTNUM                                          03630311
      GO TO 0081                                                        03640311
20070 IVFAIL = IVFAIL + 1                                               03650311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03660311
 0081 CONTINUE                                                          03670311
C                                                                       03680311
C     ****  FCVS PROGRAM 311  -  TEST 008  ****                         03690311
C                                                                       03700311
C     USE REAL VARIABLE AS PRIMARY.                                     03710311
C                                                                       03720311
      IVTNUM =   8                                                      03730311
      IF (ICZERO) 30080, 0080, 30080                                    03740311
 0080 CONTINUE                                                          03750311
      RVCOMP = 0.0                                                      03760311
      RVON02 = 1.5                                                      03770311
      RADN11(2) = 1.3                                                   03780311
      RVCOMP = RFOS05(RADN11(2))                                        03790311
      RVCORR = 2.8                                                      03800311
40080 IF (RVCOMP - 2.7995) 20080, 10080, 40081                          03810311
40081 IF (RVCOMP - 2.8005) 10080, 10080, 20080                          03820311
30080 IVDELE = IVDELE + 1                                               03830311
      WRITE (I02,80000) IVTNUM                                          03840311
      IF (ICZERO) 10080, 0091, 20080                                    03850311
10080 IVPASS = IVPASS + 1                                               03860311
      WRITE (I02,80002) IVTNUM                                          03870311
      GO TO 0091                                                        03880311
20080 IVFAIL = IVFAIL + 1                                               03890311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03900311
 0091 CONTINUE                                                          03910311
C                                                                       03920311
C     ****  FCVS PROGRAM 311  -  TEST 009  ****                         03930311
C                                                                       03940311
C     USE LOGICAL VARIABLE AS PRIMARY.                                  03950311
C                                                                       03960311
      IVTNUM =   9                                                      03970311
      IF (ICZERO) 30090, 0090, 30090                                    03980311
 0090 CONTINUE                                                          03990311
      LVON01 = .TRUE.                                                   04000311
      IVCOMP = 0                                                        04010311
      IF (LFOS01(.FALSE.)) IVCOMP = 1                                   04020311
      IVCORR = 1                                                        04030311
40090 IF (IVCOMP - 1) 20090, 10090, 20090                               04040311
30090 IVDELE = IVDELE + 1                                               04050311
      WRITE (I02,80000) IVTNUM                                          04060311
      IF (ICZERO) 10090, 0101, 20090                                    04070311
10090 IVPASS = IVPASS + 1                                               04080311
      WRITE (I02,80002) IVTNUM                                          04090311
      GO TO 0101                                                        04100311
20090 IVFAIL = IVFAIL + 1                                               04110311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04120311
 0101 CONTINUE                                                          04130311
C                                                                       04140311
C     ****  FCVS PROGRAM 311  -  TEST 010  ****                         04150311
C                                                                       04160311
C     USE INTEGER ARRAY ELEMENT NAME AS PRIMARY.                        04170311
C                                                                       04180311
      IVTNUM =  10                                                      04190311
      IF (ICZERO) 30100, 0100, 30100                                    04200311
 0100 CONTINUE                                                          04210311
      IVCOMP = 0                                                        04220311
      IADN11(1) = 7                                                     04230311
      IVCOMP = IFOS04(-4)                                               04240311
      IVCORR = 3                                                        04250311
40100 IF (IVCOMP - 3) 20100, 10100, 20100                               04260311
30100 IVDELE = IVDELE + 1                                               04270311
      WRITE (I02,80000) IVTNUM                                          04280311
      IF (ICZERO) 10100, 0111, 20100                                    04290311
10100 IVPASS = IVPASS + 1                                               04300311
      WRITE (I02,80002) IVTNUM                                          04310311
      GO TO 0111                                                        04320311
20100 IVFAIL = IVFAIL + 1                                               04330311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04340311
 0111 CONTINUE                                                          04350311
C                                                                       04360311
C     ****  FCVS PROGRAM 311  -  TEST 011  ****                         04370311
C                                                                       04380311
C     USE REAL ARRAY ELEMENT NAME AS PRIMARY.                           04390311
C                                                                       04400311
      IVTNUM =  11                                                      04410311
      IF (ICZERO) 30110, 0110, 30110                                    04420311
 0110 CONTINUE                                                          04430311
      RVCOMP = 0.0                                                      04440311
      RADN12(3) = 1.23                                                  04450311
      RVCOMP = RFOS06(3.0)                                              04460311
      RVCORR = 4.23                                                     04470311
40110 IF (RVCOMP - 4.2295) 20110, 10110, 40111                          04480311
40111 IF (RVCOMP - 4.2305) 10110, 10110, 20110                          04490311
30110 IVDELE = IVDELE + 1                                               04500311
      WRITE (I02,80000) IVTNUM                                          04510311
      IF (ICZERO) 10110, 0121, 20110                                    04520311
10110 IVPASS = IVPASS + 1                                               04530311
      WRITE (I02,80002) IVTNUM                                          04540311
      GO TO 0121                                                        04550311
20110 IVFAIL = IVFAIL + 1                                               04560311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04570311
 0121 CONTINUE                                                          04580311
C                                                                       04590311
C     ****  FCVS PROGRAM 311  -  TEST 012  ****                         04600311
C                                                                       04610311
C     USE LOGICAL ARRAY ELEMENT NAME AS PRIMARY.                        04620311
C                                                                       04630311
      IVTNUM =  12                                                      04640311
      IF (ICZERO) 30120, 0120, 30120                                    04650311
 0120 CONTINUE                                                          04660311
      LADN11(2) = .TRUE.                                                04670311
      IVCOMP = 0                                                        04680311
      IF (LFOS02(.FALSE.)) IVCOMP = 1                                   04690311
      IVCORR = 1                                                        04700311
40120 IF (IVCOMP - 1) 20120, 10120, 20120                               04710311
30120 IVDELE = IVDELE + 1                                               04720311
      WRITE (I02,80000) IVTNUM                                          04730311
      IF (ICZERO) 10120, 0131, 20120                                    04740311
10120 IVPASS = IVPASS + 1                                               04750311
      WRITE (I02,80002) IVTNUM                                          04760311
      GO TO 0131                                                        04770311
20120 IVFAIL = IVFAIL + 1                                               04780311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04790311
 0131 CONTINUE                                                          04800311
C                                                                       04810311
C     ****  FCVS PROGRAM 311  -  TEST 013  ****                         04820311
C                                                                       04830311
C     USE A REAL ARRAY ELEMENT NAME AS PRIMARY WHERE THE SUBSCRIPT      04840311
C     VALUE IS THE DUMMY ARGUMENT NAME.                                 04850311
C                                                                       04860311
      IVTNUM =  13                                                      04870311
      IF (ICZERO) 30130, 0130, 30130                                    04880311
 0130 CONTINUE                                                          04890311
      RVCOMP = 0.0                                                      04900311
      RADN13(4) = 13.4                                                  04910311
      RVCOMP = RFOS07(4)                                                04920311
      RVCORR = 13.4                                                     04930311
40130 IF (RVCOMP - 13.395) 20130, 10130, 40131                          04940311
40131 IF (RVCOMP - 13.405) 10130, 10130, 20130                          04950311
30130 IVDELE = IVDELE + 1                                               04960311
      WRITE (I02,80000) IVTNUM                                          04970311
      IF (ICZERO) 10130, 0141, 20130                                    04980311
10130 IVPASS = IVPASS + 1                                               04990311
      WRITE (I02,80002) IVTNUM                                          05000311
      GO TO 0141                                                        05010311
20130 IVFAIL = IVFAIL + 1                                               05020311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05030311
 0141 CONTINUE                                                          05040311
C                                                                       05050311
C     ****  FCVS PROGRAM 311  -  TEST 014  ****                         05060311
C                                                                       05070311
C     USE EXTERNAL FUNCTION REFERENCE AS PRIMARY.                       05080311
C                                                                       05090311
      IVTNUM =  14                                                      05100311
      IF (ICZERO) 30140, 0140, 30140                                    05110311
 0140 CONTINUE                                                          05120311
      IVCOMP = 0                                                        05130311
      IVCOMP = IFOS05(6)                                                05140311
      IVCORR = 11                                                       05150311
40140 IF (IVCOMP - 11) 20140, 10140, 20140                              05160311
30140 IVDELE = IVDELE + 1                                               05170311
      WRITE (I02,80000) IVTNUM                                          05180311
      IF (ICZERO) 10140, 0151, 20140                                    05190311
10140 IVPASS = IVPASS + 1                                               05200311
      WRITE (I02,80002) IVTNUM                                          05210311
      GO TO 0151                                                        05220311
20140 IVFAIL = IVFAIL + 1                                               05230311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05240311
 0151 CONTINUE                                                          05250311
C                                                                       05260311
C     ****  FCVS PROGRAM 311  -  TEST 015  ****                         05270311
C                                                                       05280311
C     USE EXPRESSION ENCLOSED IN PARENTHESES.                           05290311
C                                                                       05300311
      IVTNUM =  15                                                      05310311
      IF (ICZERO) 30150, 0150, 30150                                    05320311
 0150 CONTINUE                                                          05330311
      IVCOMP = 0                                                        05340311
      IVCOMP = IFOS06(4)                                                05350311
      IVCORR = 5                                                        05360311
40150 IF (IVCOMP - 5) 20150, 10150, 20150                               05370311
30150 IVDELE = IVDELE + 1                                               05380311
      WRITE (I02,80000) IVTNUM                                          05390311
      IF (ICZERO) 10150, 0161, 20150                                    05400311
10150 IVPASS = IVPASS + 1                                               05410311
      WRITE (I02,80002) IVTNUM                                          05420311
      GO TO 0161                                                        05430311
20150 IVFAIL = IVFAIL + 1                                               05440311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05450311
 0161 CONTINUE                                                          05460311
C                                                                       05470311
C     ****  FCVS PROGRAM 311  -  TEST 016  ****                         05480311
C                                                                       05490311
C     USE VARIABLE INITIALLY DEFINED IN DATA STATEMENT AS PRIMARY.      05500311
C                                                                       05510311
      IVTNUM =  16                                                      05520311
      IF (ICZERO) 30160, 0160, 30160                                    05530311
 0160 CONTINUE                                                          05540311
      IVCOMP = 0                                                        05550311
      IVCOMP = IFOS07(3)                                                05560311
      IVCORR = 9                                                        05570311
40160 IF (IVCOMP - 9) 20160, 10160, 20160                               05580311
30160 IVDELE = IVDELE + 1                                               05590311
      WRITE (I02,80000) IVTNUM                                          05600311
      IF (ICZERO) 10160, 0171, 20160                                    05610311
10160 IVPASS = IVPASS + 1                                               05620311
      WRITE (I02,80002) IVTNUM                                          05630311
      GO TO 0171                                                        05640311
20160 IVFAIL = IVFAIL + 1                                               05650311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05660311
 0171 CONTINUE                                                          05670311
C                                                                       05680311
C     ****  FCVS PROGRAM 311  -  TEST 017  ****                         05690311
C                                                                       05700311
C     USE PREVIOUSLY DEFINED STATEMENT FUNCTION REFERENCE AS PRIMARY.   05710311
C                                                                       05720311
      IVTNUM =  17                                                      05730311
      IF (ICZERO) 30170, 0170, 30170                                    05740311
 0170 CONTINUE                                                          05750311
      IVCOMP = 0                                                        05760311
      IVCOMP = IFOS09(3)                                                05770311
      IVCORR = 5                                                        05780311
40170 IF (IVCOMP - 5) 20170, 10170, 20170                               05790311
30170 IVDELE = IVDELE + 1                                               05800311
      WRITE (I02,80000) IVTNUM                                          05810311
      IF (ICZERO) 10170, 0181, 20170                                    05820311
10170 IVPASS = IVPASS + 1                                               05830311
      WRITE (I02,80002) IVTNUM                                          05840311
      GO TO 0181                                                        05850311
20170 IVFAIL = IVFAIL + 1                                               05860311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05870311
 0181 CONTINUE                                                          05880311
C                                                                       05890311
C     TEST 018 THROUGH TEST 020 APPLY TO THE DEFINITION OF THE          05900311
C     STATEMENT FUNCTION DUMMY ARGUMENTS.                               05910311
C                                                                       05920311
C                                                                       05930311
C     ****  FCVS PROGRAM 311  -  TEST 018  ****                         05940311
C                                                                       05950311
C     DEFINE STATEMENT FUNCTION WITH NO DUMMY ARGUMENTS.                05960311
C                                                                       05970311
      IVTNUM =  18                                                      05980311
      IF (ICZERO) 30180, 0180, 30180                                    05990311
 0180 CONTINUE                                                          06000311
      IVCOMP = 0                                                        06010311
      IVON02 = 4                                                        06020311
      IVCOMP = IFOS10()                                                 06030311
      IVCORR = 4                                                        06040311
40180 IF (IVCOMP - 4) 20180, 10180, 20180                               06050311
30180 IVDELE = IVDELE + 1                                               06060311
      WRITE (I02,80000) IVTNUM                                          06070311
      IF (ICZERO) 10180, 0191, 20180                                    06080311
10180 IVPASS = IVPASS + 1                                               06090311
      WRITE (I02,80002) IVTNUM                                          06100311
      GO TO 0191                                                        06110311
20180 IVFAIL = IVFAIL + 1                                               06120311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06130311
 0191 CONTINUE                                                          06140311
C                                                                       06150311
C     ****  FCVS PROGRAM 311  -  TEST 019  ****                         06160311
C                                                                       06170311
C     DEFINE STATEMENT FUNCTION WITH THREE DUMMY ARGUMENTS.             06180311
C                                                                       06190311
      IVTNUM =  19                                                      06200311
      IF (ICZERO) 30190, 0190, 30190                                    06210311
 0190 CONTINUE                                                          06220311
      IVCOMP = 0                                                        06230311
      IVCOMP = IFOS11(1,2,3)                                            06240311
      IVCORR = 6                                                        06250311
40190 IF (IVCOMP - 6) 20190, 10190, 20190                               06260311
30190 IVDELE = IVDELE + 1                                               06270311
      WRITE (I02,80000) IVTNUM                                          06280311
      IF (ICZERO) 10190, 0201, 20190                                    06290311
10190 IVPASS = IVPASS + 1                                               06300311
      WRITE (I02,80002) IVTNUM                                          06310311
      GO TO 0201                                                        06320311
20190 IVFAIL = IVFAIL + 1                                               06330311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06340311
 0201 CONTINUE                                                          06350311
C                                                                       06360311
C     ****  FCVS PROGRAM 311  -  TEST 020  ****                         06370311
C                                                                       06380311
C     USE THE SAME DUMMY ARGUMENT NAME IN TWO DIFFERENT                 06390311
C     STATEMENT FUNCTIONS.                                              06400311
C                                                                       06410311
      IVTNUM =  20                                                      06420311
      IF (ICZERO) 30200, 0200, 30200                                    06430311
 0200 CONTINUE                                                          06440311
      IVCOMP = 1                                                        06450311
      IF (IFOS12(3) .EQ. 4) IVCOMP = IVCOMP * 2                         06460311
      IF (IFOS13(4) .EQ. 6) IVCOMP = IVCOMP * 3                         06470311
      IVCORR = 6                                                        06480311
C     6 = 2 * 3                                                         06490311
40200 IF (IVCOMP - 6) 20200, 10200, 20200                               06500311
30200 IVDELE = IVDELE + 1                                               06510311
      WRITE (I02,80000) IVTNUM                                          06520311
      IF (ICZERO) 10200, 0211, 20200                                    06530311
10200 IVPASS = IVPASS + 1                                               06540311
      WRITE (I02,80002) IVTNUM                                          06550311
      GO TO 0211                                                        06560311
20200 IVFAIL = IVFAIL + 1                                               06570311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06580311
 0211 CONTINUE                                                          06590311
C                                                                       06600311
C     TEST 021 THROUGH TEST 022 TEST THE USAGE OF DIFFERENT TYPES OF    06610311
C     ACTUAL ARGUMENTS IN A STATEMENT FUNCTION REFERENCE.               06620311
C                                                                       06630311
C                                                                       06640311
C     ****  FCVS PROGRAM 311  -  TEST 021  ****                         06650311
C                                                                       06660311
C     USE AN EXPRESSION WITH OPERATORS AS AN ACTUAL ARGUMENT.           06670311
C                                                                       06680311
      IVTNUM =  21                                                      06690311
      IF (ICZERO) 30210, 0210, 30210                                    06700311
 0210 CONTINUE                                                          06710311
      IVCOMP = 0                                                        06720311
      IVON03 = 4                                                        06730311
      IVCOMP = IFOS14(IVON03 * 4 + 1)                                   06740311
      IVCORR = 18                                                       06750311
40210 IF (IVCOMP - 18) 20210, 10210, 20210                              06760311
30210 IVDELE = IVDELE + 1                                               06770311
      WRITE (I02,80000) IVTNUM                                          06780311
      IF (ICZERO) 10210, 0221, 20210                                    06790311
10210 IVPASS = IVPASS + 1                                               06800311
      WRITE (I02,80002) IVTNUM                                          06810311
      GO TO 0221                                                        06820311
20210 IVFAIL = IVFAIL + 1                                               06830311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06840311
 0221 CONTINUE                                                          06850311
C                                                                       06860311
C     ****  FCVS PROGRAM 311  -  TEST 022  ****                         06870311
C                                                                       06880311
C     USE AN INTRINSIC FUNCTION REFERENCE AS AN ACTUAL ARGUMENT.        06890311
C                                                                       06900311
      IVTNUM =  22                                                      06910311
      IF (ICZERO) 30220, 0220, 30220                                    06920311
 0220 CONTINUE                                                          06930311
      IVCOMP = 0                                                        06940311
      RVON01 = 1.75                                                     06950311
      IVCOMP = IFOS14(NINT(RVON01))                                     06960311
      IVCORR = 3                                                        06970311
40220 IF (IVCOMP - 3) 20220, 10220, 20220                               06980311
30220 IVDELE = IVDELE + 1                                               06990311
      WRITE (I02,80000) IVTNUM                                          07000311
      IF (ICZERO) 10220, 0231, 20220                                    07010311
10220 IVPASS = IVPASS + 1                                               07020311
      WRITE (I02,80002) IVTNUM                                          07030311
      GO TO 0231                                                        07040311
20220 IVFAIL = IVFAIL + 1                                               07050311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07060311
 0231 CONTINUE                                                          07070311
C                                                                       07080311
C     ****  FCVS PROGRAM 311  -  TEST 023  ****                         07090311
C                                                                       07100311
C     USE AN EXTERNAL FUNCTION REFERENCE AS AN ACTUAL ARGUMENT.         07110311
C                                                                       07120311
      IVTNUM =  23                                                      07130311
      IF (ICZERO) 30230, 0230, 30230                                    07140311
 0230 CONTINUE                                                          07150311
      IVCOMP = 0                                                        07160311
      IVCOMP = IFOS14(FF312(5))                                         07170311
      IVCORR = 7                                                        07180311
40230 IF (IVCOMP - 7) 20230, 10230, 20230                               07190311
30230 IVDELE = IVDELE + 1                                               07200311
      WRITE (I02,80000) IVTNUM                                          07210311
      IF (ICZERO) 10230, 0241, 20230                                    07220311
10230 IVPASS = IVPASS + 1                                               07230311
      WRITE (I02,80002) IVTNUM                                          07240311
      GO TO 0241                                                        07250311
20230 IVFAIL = IVFAIL + 1                                               07260311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07270311
 0241 CONTINUE                                                          07280311
C                                                                       07290311
C     TEST 024 THROUGH TEST 029 APPLY TO THE TYPING OF STATEMENT        07300311
C     FUNCTIONS AND THE ASSOCIATED DUMMY ARGUMENT NAMES.                07310311
C                                                                       07320311
C                                                                       07330311
C     ****  FCVS PROGRAM 311  -  TEST 024  ****                         07340311
C                                                                       07350311
C     OVERRIDE THE INTEGER DEFAULT TYPING OF A STATEMENT FUNCTION WITH  07360311
C     THE IMPLICIT STATEMENT TYPING OF REAL.                            07370311
C                                                                       07380311
      IVTNUM =  24                                                      07390311
      IF (ICZERO) 30240, 0240, 30240                                    07400311
 0240 CONTINUE                                                          07410311
      RVCOMP = 10.0                                                     07420311
      RVCOMP = KFOS01(3) / 5                                            07430311
      RVCORR = 0.8                                                      07440311
40240 IF (RVCOMP - .79995) 20240, 10240, 40241                          07450311
40241 IF (RVCOMP - .80005) 10240, 10240, 20240                          07460311
30240 IVDELE = IVDELE + 1                                               07470311
      WRITE (I02,80000) IVTNUM                                          07480311
      IF (ICZERO) 10240, 0251, 20240                                    07490311
10240 IVPASS = IVPASS + 1                                               07500311
      WRITE (I02,80002) IVTNUM                                          07510311
      GO TO 0251                                                        07520311
20240 IVFAIL = IVFAIL + 1                                               07530311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07540311
 0251 CONTINUE                                                          07550311
C                                                                       07560311
C     ****  FCVS PROGRAM 311  -  TEST 025  ****                         07570311
C                                                                       07580311
C     OVERRIDE THE REAL DEFAULT TYPING OF A STATEMENT FUNCTION WITH     07590311
C     THE IMPLICIT STATEMENT TYPING OF INTEGER.                         07600311
C                                                                       07610311
      IVTNUM =  25                                                      07620311
      IF (ICZERO) 30250, 0250, 30250                                    07630311
 0250 CONTINUE                                                          07640311
      RVCOMP = 10.0                                                     07650311
      RVCOMP = AFOS01(3.0) / 5                                          07660311
      RVCORR = 0.0                                                      07670311
40250 IF (RVCOMP + .00005) 20250, 10250, 40251                          07680311
40251 IF (RVCOMP - .00005) 10250, 10250, 20250                          07690311
30250 IVDELE = IVDELE + 1                                               07700311
      WRITE (I02,80000) IVTNUM                                          07710311
      IF (ICZERO) 10250, 0261, 20250                                    07720311
10250 IVPASS = IVPASS + 1                                               07730311
      WRITE (I02,80002) IVTNUM                                          07740311
      GO TO 0261                                                        07750311
20250 IVFAIL = IVFAIL + 1                                               07760311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07770311
 0261 CONTINUE                                                          07780311
C                                                                       07790311
C     ****  FCVS PROGRAM 311  -  TEST 026  ****                         07800311
C                                                                       07810311
C     OVERRIDE THE INTEGER DEFAULT TYPING OF A STATEMENT FUNCTION       07820311
C     DUMMY ARGUMENT WITH THE IMPLICIT STATEMENT TYPING OF REAL.        07830311
C                                                                       07840311
      IVTNUM =  26                                                      07850311
      IF (ICZERO) 30260, 0260, 30260                                    07860311
 0260 CONTINUE                                                          07870311
      RVCOMP = 10.0                                                     07880311
      RVCOMP = RFOS08(4.0)                                              07890311
      RVCORR = 0.8                                                      07900311
40260 IF (RVCOMP - .79995) 20260, 10260, 40261                          07910311
40261 IF (RVCOMP - .80005) 10260, 10260, 20260                          07920311
30260 IVDELE = IVDELE + 1                                               07930311
      WRITE (I02,80000) IVTNUM                                          07940311
      IF (ICZERO) 10260, 0271, 20260                                    07950311
10260 IVPASS = IVPASS + 1                                               07960311
      WRITE (I02,80002) IVTNUM                                          07970311
      GO TO 0271                                                        07980311
20260 IVFAIL = IVFAIL + 1                                               07990311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08000311
 0271 CONTINUE                                                          08010311
C                                                                       08020311
C     ****  FCVS PROGRAM 311  -  TEST 027  ****                         08030311
C                                                                       08040311
C     OVERRIDE THE REAL DEFAULT TYPING OF A STATEMENT FUNCTION DUMMY    08050311
C     ARGUMENT WITH THE IMPLICIT STATEMENT TYPING OF INTEGER.           08060311
C                                                                       08070311
      IVTNUM =  27                                                      08080311
      IF (ICZERO) 30270, 0270, 30270                                    08090311
 0270 CONTINUE                                                          08100311
      RVCOMP = 10.0                                                     08110311
      RVCOMP = RFOS09(4)                                                08120311
      RVCORR = 0.0                                                      08130311
40270 IF (RVCOMP + .00005) 20270, 10270, 40271                          08140311
40271 IF (RVCOMP - .00005) 10270, 10270, 20270                          08150311
30270 IVDELE = IVDELE + 1                                               08160311
      WRITE (I02,80000) IVTNUM                                          08170311
      IF (ICZERO) 10270, 0281, 20270                                    08180311
10270 IVPASS = IVPASS + 1                                               08190311
      WRITE (I02,80002) IVTNUM                                          08200311
      GO TO 0281                                                        08210311
20270 IVFAIL = IVFAIL + 1                                               08220311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08230311
 0281 CONTINUE                                                          08240311
C                                                                       08250311
C     ****  FCVS PROGRAM 311  -  TEST 028  ****                         08260311
C                                                                       08270311
C     OVERRIDE INTEGER DEFAULT TYPING OF A STATEMENT FUNCTION DUMMY     08280311
C     ARGUMENT WITH TYPE-STATEMENT TYPING OF REAL.                      08290311
C                                                                       08300311
      IVTNUM =  28                                                      08310311
      IF (ICZERO) 30280, 0280, 30280                                    08320311
 0280 CONTINUE                                                          08330311
      RVCOMP = 10.0                                                     08340311
      RVCOMP = RFOS10(4.0)                                              08350311
      RVCORR = 0.8                                                      08360311
40280 IF (RVCOMP - .79995) 20280, 10280, 40281                          08370311
40281 IF (RVCOMP - .80005) 10280, 10280, 20280                          08380311
30280 IVDELE = IVDELE + 1                                               08390311
      WRITE (I02,80000) IVTNUM                                          08400311
      IF (ICZERO) 10280, 0291, 20280                                    08410311
10280 IVPASS = IVPASS + 1                                               08420311
      WRITE (I02,80002) IVTNUM                                          08430311
      GO TO 0291                                                        08440311
20280 IVFAIL = IVFAIL + 1                                               08450311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08460311
 0291 CONTINUE                                                          08470311
C                                                                       08480311
C     ****  FCVS PROGRAM 311  -  TEST 029  ****                         08490311
C                                                                       08500311
C     OVERRIDE THE REAL DEFAULT TYPING OF A STATEMENT FUNCTION DUMMY    08510311
C     ARGUMENT WITH TYPE-STATEMENT TYPING OF INTEGER.                   08520311
C                                                                       08530311
      IVTNUM =  29                                                      08540311
      IF (ICZERO) 30290, 0290, 30290                                    08550311
 0290 CONTINUE                                                          08560311
      RVCOMP = 10.0                                                     08570311
      RVCOMP = RFOS11(4)                                                08580311
      RVCORR = 0.0                                                      08590311
40290 IF (RVCOMP + .00005) 20290, 10290, 40291                          08600311
40291 IF (RVCOMP - .00005) 10290, 10290, 20290                          08610311
30290 IVDELE = IVDELE + 1                                               08620311
      WRITE (I02,80000) IVTNUM                                          08630311
      IF (ICZERO) 10290, 0301, 20290                                    08640311
10290 IVPASS = IVPASS + 1                                               08650311
      WRITE (I02,80002) IVTNUM                                          08660311
      GO TO 0301                                                        08670311
20290 IVFAIL = IVFAIL + 1                                               08680311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08690311
 0301 CONTINUE                                                          08700311
C                                                                       08710311
C     ****  FCVS PROGRAM 311  -  TEST 030  ****                         08720311
C                                                                       08730311
C     TEST 030 TESTS A STATEMENT FUNCTION WHERE THE DUMMY ARGUMENT      08740311
C     NAME IS IDENTICAL TO A VARIABLE NAME WITHIN THE PROGRAM.          08750311
C                                                                       08760311
      IVTNUM =  30                                                      08770311
      IF (ICZERO) 30300, 0300, 30300                                    08780311
 0300 CONTINUE                                                          08790311
      IVON04 = 10                                                       08800311
      IVCOMP = 1                                                        08810311
      IF (IFOS15(3) .EQ. 4) IVCOMP = IVCOMP * 2                         08820311
      IF (IVON04 .EQ. 10) IVCOMP = IVCOMP * 3                           08830311
      IVCORR = 6                                                        08840311
C     6 = 2 * 3                                                         08850311
40300 IF (IVCOMP - 6) 20300, 10300, 20300                               08860311
30300 IVDELE = IVDELE + 1                                               08870311
      WRITE (I02,80000) IVTNUM                                          08880311
      IF (ICZERO) 10300, 0311, 20300                                    08890311
10300 IVPASS = IVPASS + 1                                               08900311
      WRITE (I02,80002) IVTNUM                                          08910311
      GO TO 0311                                                        08920311
20300 IVFAIL = IVFAIL + 1                                               08930311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08940311
 0311 CONTINUE                                                          08950311
C                                                                       08960311
C     ****  FCVS PROGRAM 311  -  TEST 031  ****                         08970311
C                                                                       08980311
C     TEST 031 TESTS THE ASSIGNMENT OF A STATEMENT FUNCTION TO AN       08990311
C     ARRAY ELEMENT.                                                    09000311
C                                                                       09010311
      IVTNUM =  31                                                      09020311
      IF (ICZERO) 30310, 0310, 30310                                    09030311
 0310 CONTINUE                                                          09040311
      IVCOMP = 0                                                        09050311
      IADN12(3) = IFOS16(4)                                             09060311
      IVCOMP = IADN12(3)                                                09070311
      IVCORR = 5                                                        09080311
40310 IF (IVCOMP - 5) 20310, 10310, 20310                               09090311
30310 IVDELE = IVDELE + 1                                               09100311
      WRITE (I02,80000) IVTNUM                                          09110311
      IF (ICZERO) 10310, 0321, 20310                                    09120311
10310 IVPASS = IVPASS + 1                                               09130311
      WRITE (I02,80002) IVTNUM                                          09140311
      GO TO 0321                                                        09150311
20310 IVFAIL = IVFAIL + 1                                               09160311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09170311
 0321 CONTINUE                                                          09180311
C                                                                       09190311
C     ****  FCVS PROGRAM 311  -  TEST 032  ****                         09200311
C                                                                       09210311
C     TEST 032 TESTS THE USE OF A STATEMENT FUNCTION REFERENCE          09220311
C     IN AN ARITHMETIC EXPRESSION.                                      09230311
C                                                                       09240311
      IVTNUM =  32                                                      09250311
      IF (ICZERO) 30320, 0320, 30320                                    09260311
 0320 CONTINUE                                                          09270311
      IVCOMP = 0                                                        09280311
      IVON05 = 12                                                       09290311
      IVCOMP = IVON05 + IFOS17(4) * 2 - 3                               09300311
      IVCORR = 19                                                       09310311
40320 IF (IVCOMP - 19) 20320, 10320, 20320                              09320311
30320 IVDELE = IVDELE + 1                                               09330311
      WRITE (I02,80000) IVTNUM                                          09340311
      IF (ICZERO) 10320, 0331, 20320                                    09350311
10320 IVPASS = IVPASS + 1                                               09360311
      WRITE (I02,80002) IVTNUM                                          09370311
      GO TO 0331                                                        09380311
20320 IVFAIL = IVFAIL + 1                                               09390311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09400311
 0331 CONTINUE                                                          09410311
C                                                                       09420311
C     ****  FCVS PROGRAM 311  -  TEST 033  ****                         09430311
C                                                                       09440311
C     TEST 033 TESTS THE USE OF A STATEMENT FUNCTION DEFINITION AND     09450311
C     REFERENCE WITHIN AN EXTERNAL FUNCTION.                            09460311
C                                                                       09470311
      IVTNUM =  33                                                      09480311
      IF (ICZERO) 30330, 0330, 30330                                    09490311
 0330 CONTINUE                                                          09500311
      RVCOMP = 0.0                                                      09510311
      RVCOMP = FF313(1.3)                                               09520311
      RVCORR = 5.8                                                      09530311
40330 IF (RVCOMP - 5.7995) 20330, 10330, 40331                          09540311
40331 IF (RVCOMP - 5.8005) 10330, 10330, 20330                          09550311
30330 IVDELE = IVDELE + 1                                               09560311
      WRITE (I02,80000) IVTNUM                                          09570311
      IF (ICZERO) 10330, 0341, 20330                                    09580311
10330 IVPASS = IVPASS + 1                                               09590311
      WRITE (I02,80002) IVTNUM                                          09600311
      GO TO 0341                                                        09610311
20330 IVFAIL = IVFAIL + 1                                               09620311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          09630311
 0341 CONTINUE                                                          09640311
C                                                                       09650311
C     ****  FCVS PROGRAM 311  -  TEST 034  ****                         09660311
C                                                                       09670311
C     TEST 034 TESTS THE USE OF A STATEMENT FUNCTION DEFINITION AND     09680311
C     REFERENCE WITHIN A SUBROUTINE.                                    09690311
C                                                                       09700311
      IVTNUM =  34                                                      09710311
      IF (ICZERO) 30340, 0340, 30340                                    09720311
 0340 CONTINUE                                                          09730311
      RVCOMP = 0.0                                                      09740311
      RVON05 = 10.0                                                     09750311
      CALL FS316(RVON05)                                                09760311
      RVCOMP = RVON05                                                   09770311
      RVCORR = 5.5                                                      09780311
40340 IF (RVCOMP - 5.4995) 20340, 10340, 40341                          09790311
40341 IF (RVCOMP - 5.5005) 10340, 10340, 20340                          09800311
30340 IVDELE = IVDELE + 1                                               09810311
      WRITE (I02,80000) IVTNUM                                          09820311
      IF (ICZERO) 10340, 0351, 20340                                    09830311
10340 IVPASS = IVPASS + 1                                               09840311
      WRITE (I02,80002) IVTNUM                                          09850311
      GO TO 0351                                                        09860311
20340 IVFAIL = IVFAIL + 1                                               09870311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          09880311
 0351 CONTINUE                                                          09890311
C                                                                       09900311
C     ****  FCVS PROGRAM 311  -  TEST 035  ****                         09910311
C                                                                       09920311
C     TEST 035 REFERENCES THE DUMMY ARGUMENT NAME OF AN EXTERNAL        09930311
C     FUNCTION WITHIN THE EXPRESSION OF A STATEMENT FUNCTION DEFINED    09940311
C     IN THAT EXTERNAL FUNCTION.                                        09950311
C                                                                       09960311
      IVTNUM =  35                                                      09970311
      IF (ICZERO) 30350, 0350, 30350                                    09980311
 0350 CONTINUE                                                          09990311
      IVCOMP = 0                                                        10000311
      IVCOMP = FF314(4)                                                 10010311
      IVCORR = 7                                                        10020311
40350 IF (IVCOMP - 7) 20350, 10350, 20350                               10030311
30350 IVDELE = IVDELE + 1                                               10040311
      WRITE (I02,80000) IVTNUM                                          10050311
      IF (ICZERO) 10350, 0361, 20350                                    10060311
10350 IVPASS = IVPASS + 1                                               10070311
      WRITE (I02,80002) IVTNUM                                          10080311
      GO TO 0361                                                        10090311
20350 IVFAIL = IVFAIL + 1                                               10100311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10110311
 0361 CONTINUE                                                          10120311
C                                                                       10130311
C     ****  FCVS PROGRAM 311  -  TEST 036  ****                         10140311
C                                                                       10150311
C     TEST 036 TESTS A STATEMENT FUNCTION DEFINED WITHIN AN EXTERNAL    10160311
C     FUNCTION IN WHICH THE STATEMENT FUNCTION DUMMY ARGUMENT NAME IS   10170311
C     IDENTICAL TO THE EXTERNAL FUNCTION DUMMY ARGUMENT NAME.           10180311
C                                                                       10190311
      IVTNUM =  36                                                      10200311
      IF (ICZERO) 30360, 0360, 30360                                    10210311
 0360 CONTINUE                                                          10220311
      RVCOMP = 0.0                                                      10230311
      RVCOMP = FF315(5.5)                                               10240311
      RVCORR = 16.7                                                     10250311
40360 IF (RVCOMP - 16.695) 20360, 10360, 40361                          10260311
40361 IF (RVCOMP - 16.705) 10360, 10360, 20360                          10270311
30360 IVDELE = IVDELE + 1                                               10280311
      WRITE (I02,80000) IVTNUM                                          10290311
      IF (ICZERO) 10360, 0371, 20360                                    10300311
10360 IVPASS = IVPASS + 1                                               10310311
      WRITE (I02,80002) IVTNUM                                          10320311
      GO TO 0371                                                        10330311
20360 IVFAIL = IVFAIL + 1                                               10340311
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          10350311
 0371 CONTINUE                                                          10360311
C                                                                       10370311
C     ****  FCVS PROGRAM 311  -  TEST 037  ****                         10380311
C                                                                       10390311
C     TEST 037 TESTS THE USAGE OF THE NAME OF A COMMON BLOCK AS THE     10400311
C     SYMBOLIC NAME OF A STATEMENT FUNCTION.                            10410311
C                                                                       10420311
      IVTNUM =  37                                                      10430311
      IF (ICZERO) 30370, 0370, 30370                                    10440311
 0370 CONTINUE                                                          10450311
      IVCOMP = 0                                                        10460311
      IVCOMP = IFOS19(4)                                                10470311
      IVCORR = 5                                                        10480311
40370 IF (IVCOMP - 5) 20370, 10370, 20370                               10490311
30370 IVDELE = IVDELE + 1                                               10500311
      WRITE (I02,80000) IVTNUM                                          10510311
      IF (ICZERO) 10370, 0381, 20370                                    10520311
10370 IVPASS = IVPASS + 1                                               10530311
      WRITE (I02,80002) IVTNUM                                          10540311
      GO TO 0381                                                        10550311
20370 IVFAIL = IVFAIL + 1                                               10560311
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10570311
 0381 CONTINUE                                                          10580311
C                                                                       10590311
C                                                                       10600311
C     WRITE OUT TEST SUMMARY                                            10610311
C                                                                       10620311
      WRITE (I02,90004)                                                 10630311
      WRITE (I02,90014)                                                 10640311
      WRITE (I02,90004)                                                 10650311
      WRITE (I02,90000)                                                 10660311
      WRITE (I02,90004)                                                 10670311
      WRITE (I02,90020) IVFAIL                                          10680311
      WRITE (I02,90022) IVPASS                                          10690311
      WRITE (I02,90024) IVDELE                                          10700311
      STOP                                                              10710311
90001 FORMAT (1H ,24X,5HFM311)                                          10720311
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM311)                          10730311
C                                                                       10740311
C     FORMATS FOR TEST DETAIL LINES                                     10750311
C                                                                       10760311
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   10770311
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      10780311
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         10790311
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    10800311
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        10810311
C                                                                       10820311
C     FORMAT STATEMENTS FOR PAGE HEADERS                                10830311
C                                                                       10840311
90002 FORMAT (1H1)                                                      10850311
90004 FORMAT (1H )                                                      10860311
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            10870311
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   10880311
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         10890311
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  10900311
90014 FORMAT (1H ,5X,46H----------------------------------------------) 10910311
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             10920311
C                                                                       10930311
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 10940311
C                                                                       10950311
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              10960311
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              10970311
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             10980311
      END                                                               10990311
      INTEGER FUNCTION FF312(IDONX1)                                    00010312
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS SUBPROGRAM IS USED BY TESTS 014 AND 023 OF THE MAIN PROGRAM  00020312
C     FM311 TO TEST STATEMENT FUNCTION.  IN TEST 014 REFERENCE TO FF312 00030312
C     IS USED IN THE EXPRESSION OF A STATEMENT FUNCTION.  IN TEST 023   00040312
C     REFERENCE TO FF312 IS USED AS AN ACTUAL ARGUMENT IN A STATEMENT   00050312
C     FUNCTION REFERENCE.  THIS ROUTINE MERELY INCREMENTS THE VALUE OF  00060312
C     ACTUAL/DUMMY ARGUMENT BY ONE AND RETURN THE RESULT AS THE         00070312
C     FUNCTION VALUE.                                                   00080312
      IDONX2 = IDONX1 + 1                                               00090312
      FF312 = IDONX2                                                    00100312
      RETURN                                                            00110312
      END                                                               00120312
      REAL FUNCTION FF313(RDON08)                                       00010313
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS SUBPROGRAM IS USED BY TEST 033 OF THE MAIN PROGRAM FM311 TO  00020313
C     TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  00030313
C     AN EXTERNAL FUNCTION.                                             00040313
      RFOS12(RDON09) = RDON09 + 1.0                                     00050313
      RVON04 = RFOS12(3.5)                                              00060313
      FF313 = RDON08 + RVON04                                           00070313
      RETURN                                                            00080313
      END                                                               00090313
      INTEGER FUNCTION FF314(IDON19)                                    00010314
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS SUBPROGRAM IS USED BY TEST 035 OF THE MAIN PROGRAM FM311 TO  00020314
C     TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  00030314
C     AN EXTERNAL FUNCTION.  IN THIS TEST THE EXTERNAL FUNCTION DUMMY   00040314
C     ARGUMENT IS REFERENCED WITHIN THE EXPRESSION OF THE STATEMENT     00050314
C     FUNCTION.                                                         00060314
      IFOS18(IDON20) = IDON19 + IDON20                                  00070314
      FF314 = IFOS18(3)                                                 00080314
      RETURN                                                            00090314
      END                                                               00100314
      REAL FUNCTION FF315(RDON12)                                       00010315
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS SUBPROGRAM IS USED BY TEST 036 OF THE MAIN PROGRAM FM311 TO  00020315
C     TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  00030315
C     AN EXTERNAL FUNCTION.  IN THIS TEST THE EXTERNAL FUNCTION AND     00040315
C     STATEMENT FUNCTION DUMMY ARGUMENTS NAMES ARE IDENTICAL.           00050315
      RFOS14(RDON12) = RDON12 + 1.0                                     00060315
      RVON06 = 10.2                                                     00070315
      RVON07 = RFOS14(RVON06)                                           00080315
      FF315 = RDON12 + RVON07                                           00090315
      RETURN                                                            00100315
      END                                                               00110315
      SUBROUTINE FS316(RDON10)                                          00010316
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS SUBPROGRAM IS USED BY TEST 034 OF THE MAIN PROGRAM FM311 TO  00020316
C     TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  00030316
C     A SUBROUTINE.                                                     00040316
      RFOS13(RDON11) = RDON11 + 1.0                                     00050316
      RDON10 = RFOS13(3.5) + 1.0                                        00060316
      RETURN                                                            00070316
      END                                                               00080316
