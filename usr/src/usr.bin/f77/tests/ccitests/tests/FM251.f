      PROGRAM FM251                                                     00010251
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020251
C                                                                       00030251
C                                                                       00040251
C        THIS ROUTINE TESTS THE IMPLICIT STATEMENT FOR DECLARING        00050251
C     VARIABLES AS TYPE LOGICAL.  THE TYPE OF A VARIABLE ( LOGICAL,     00060251
C     INTEGER, OR REAL ) IS SET BY BOTH IMPLICIT STATEMENTS AND ALSO    00070251
C     BY EXPLICIT TYPE STATEMENTS.  TESTS ARE MADE TO CHECK THAT        00080251
C     EXPLICIT TYPE STATEMENTS OVERIDE THE TYPE SET BY AN IMPLICIT      00090251
C     STATEMENT FOR THE VARIABLES LISTED.                               00100251
C                                                                       00110251
C     REFERENCES                                                        00120251
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00130251
C             X3.9-1977                                                 00140251
C        SECTION 4.7,    LOGICAL TYPE                                   00150251
C        SECTION 8.4.1,  LOGICAL TYPE STAEMENT                          00160251
C        SECTION 8.5,    IMPLICIT STATEMENT                             00170251
C        SECTION 11.5,   LOGICAL IF STATEMENT                           00180251
C                                                                       00190251
C                                                                       00200251
C        FM016 - TESTS LOGICAL TYPE STATEMENTS WITH VARIOUS FORMS OF    00210251
C                LOGICAL CONSTANTS AND VARIABLES.                       00220251
C                                                                       00230251
C                                                                       00240251
C                                                                       00250251
C                                                                       00260251
C     ******************************************************************00270251
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00280251
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00290251
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00300251
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00310251
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00320251
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00330251
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00340251
C     THE RESULT OF EXECUTING THESE TESTS.                              00350251
C                                                                       00360251
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00370251
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00380251
C                                                                       00390251
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00400251
C                    DEPARTMENT OF THE NAVY                             00410251
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00420251
C                    WASHINGTON, D.C.   20376                           00430251
C                                                                       00440251
C     ******************************************************************00450251
C                                                                       00460251
C                                                                       00470251
      IMPLICIT LOGICAL (L)                                              00480251
      IMPLICIT CHARACTER*14 (C)                                         00490251
C                                                                       00500251
      IMPLICIT LOGICAL (M,N)                                            00510251
      IMPLICIT LOGICAL ( E-H, O, P-Q, S-T, X-Y ), INTEGER ( U-W )       00520251
      IMPLICIT INTEGER (A, B), REAL (I, J)                              00530251
      INTEGER IVCOMP, IVPASS, IVCORR, IVTNUM, IVDELE, IVFAIL, I01, I02  00540251
      INTEGER ICZERO                                                    00550251
      INTEGER MVTN01                                                    00560251
      REAL NVTN01                                                       00570251
      LOGICAL MVTN02, NVTN02, MATN21(3,3)                               00580251
      LOGICAL AVTN01                                                    00590251
      LOGICAL IVTN01                                                    00600251
C                                                                       00610251
C                                                                       00620251
C                                                                       00630251
C     INITIALIZATION SECTION.                                           00640251
C                                                                       00650251
C     INITIALIZE CONSTANTS                                              00660251
C     ********************                                              00670251
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00680251
      I01 = 5                                                           00690251
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00700251
      I02 = 6                                                           00710251
C     SYSTEM ENVIRONMENT SECTION                                        00720251
C                                                                       00730251
      I01 = 5                                                           00740251
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750251
C     (UNIT NUMBER FOR CARD READER).                                    00760251
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00770251
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00780251
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00790251
C                                                                       00800251
      I02 = 6                                                           00810251
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00820251
C     (UNIT NUMBER FOR PRINTER).                                        00830251
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00840251
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00850251
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00860251
C                                                                       00870251
      IVPASS = 0                                                        00880251
      IVFAIL = 0                                                        00890251
      IVDELE = 0                                                        00900251
      ICZERO = 0                                                        00910251
C                                                                       00920251
C     WRITE OUT PAGE HEADERS                                            00930251
C                                                                       00940251
      WRITE (I02,90002)                                                 00950251
      WRITE (I02,90006)                                                 00960251
      WRITE (I02,90008)                                                 00970251
      WRITE (I02,90004)                                                 00980251
      WRITE (I02,90010)                                                 00990251
      WRITE (I02,90004)                                                 01000251
      WRITE (I02,90016)                                                 01010251
      WRITE (I02,90001)                                                 01020251
      WRITE (I02,90004)                                                 01030251
      WRITE (I02,90012)                                                 01040251
      WRITE (I02,90014)                                                 01050251
      WRITE (I02,90004)                                                 01060251
C                                                                       01070251
C                                                                       01080251
C     ****  FCVS PROGRAM 251  -  TEST 001  ****                         01090251
C                                                                       01100251
C        TEST 001 ASSIGNS A LOGICAL VALUE OF .TRUE. TO MVIN01 WHICH WAS 01110251
C     SPECIFIED AS TYPE LOGICAL IN AN IMPLICIT STATEMENT.               01120251
C                  IMPLICIT LOGICAL (M,N)                               01130251
C                                                                       01140251
      IVTNUM =   1                                                      01150251
      IF (ICZERO) 30010, 0010, 30010                                    01160251
 0010 CONTINUE                                                          01170251
      IVCOMP = 0                                                        01180251
      MVIN01 = .TRUE.                                                   01190251
      IF ( MVIN01 )  IVCOMP = 1                                         01200251
      IVCORR = 1                                                        01210251
40010 IF ( IVCOMP - 1 )  20010, 10010, 20010                            01220251
30010 IVDELE = IVDELE + 1                                               01230251
      WRITE (I02,80000) IVTNUM                                          01240251
      IF (ICZERO) 10010, 0021, 20010                                    01250251
10010 IVPASS = IVPASS + 1                                               01260251
      WRITE (I02,80002) IVTNUM                                          01270251
      GO TO 0021                                                        01280251
20010 IVFAIL = IVFAIL + 1                                               01290251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01300251
 0021 CONTINUE                                                          01310251
C                                                                       01320251
C     ****  FCVS PROGRAM 251  -  TEST 002  ****                         01330251
C                                                                       01340251
C        TEST 002 ASSIGNS A LOGICAL VALUE OF .FALSE. TO NVIN01 WHICH    01350251
C     WAS SPECIFIED AS TYPE LOGICAL IN AN IMPLICIT STATEMENT.           01360251
C                  IMPLICIT LOGICAL (M,N)                               01370251
C                                                                       01380251
      IVTNUM =   2                                                      01390251
      IF (ICZERO) 30020, 0020, 30020                                    01400251
 0020 CONTINUE                                                          01410251
      IVCOMP = 1                                                        01420251
      LCON01 = .FALSE.                                                  01430251
      NVIN01 = LCON01                                                   01440251
      IF ( NVIN01 )  IVCOMP = 0                                         01450251
      IVCORR = 1                                                        01460251
40020 IF ( IVCOMP - 1 )  20020, 10020, 20020                            01470251
30020 IVDELE = IVDELE + 1                                               01480251
      WRITE (I02,80000) IVTNUM                                          01490251
      IF (ICZERO) 10020, 0031, 20020                                    01500251
10020 IVPASS = IVPASS + 1                                               01510251
      WRITE (I02,80002) IVTNUM                                          01520251
      GO TO 0031                                                        01530251
20020 IVFAIL = IVFAIL + 1                                               01540251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01550251
 0031 CONTINUE                                                          01560251
C                                                                       01570251
C     ****  FCVS PROGRAM 251  -  TEST 003  ****                         01580251
C                                                                       01590251
C        TEST 003 ASSIGNS AN INTEGER VALUE OF 4 TO MVTN01 WHICH         01600251
C     WAS SPECIFIED AS TYPE INTEGER EXPLICITLY IN A TYPE STATEMENT.     01610251
C                  INTEGER MVTN01                                       01620251
C     THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT INTEGER TYPE        01630251
C     STATEMENT CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD         01640251
C     SET THE TYPE AS LOGICAL.                                          01650251
C                  IMPLICIT LOGICAL (M,N)                               01660251
C                                                                       01670251
      IVTNUM =   3                                                      01680251
      IF (ICZERO) 30030, 0030, 30030                                    01690251
 0030 CONTINUE                                                          01700251
      RVCOMP = 10.0                                                     01710251
      MVTN01 = 4                                                        01720251
      RVCOMP = MVTN01/5                                                 01730251
      RVCORR = 0.0                                                      01740251
40030 IF ( RVCOMP )  20030, 10030, 20030                                01750251
30030 IVDELE = IVDELE + 1                                               01760251
      WRITE (I02,80000) IVTNUM                                          01770251
      IF (ICZERO) 10030, 0041, 20030                                    01780251
10030 IVPASS = IVPASS + 1                                               01790251
      WRITE (I02,80002) IVTNUM                                          01800251
      GO TO 0041                                                        01810251
20030 IVFAIL = IVFAIL + 1                                               01820251
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01830251
 0041 CONTINUE                                                          01840251
C                                                                       01850251
C     ****  FCVS PROGRAM 251  -  TEST 004  ****                         01860251
C                                                                       01870251
C        TEST 004 ASSIGNS A REAL VALUE OF 4.0 TO NVTN01 WHICH           01880251
C     WAS SPECIFIED AS TYPE REAL EXPLICITLY IN A TYPE STATEMENT.        01890251
C                  REAL NVTN01                                          01900251
C     THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT REAL TYPE           01910251
C     STATEMENT CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD         01920251
C     SET THE TYPE AS LOGICAL.                                          01930251
C                  IMPLICIT LOGICAL (M,N)                               01940251
C                                                                       01950251
      IVTNUM =   4                                                      01960251
      IF (ICZERO) 30040, 0040, 30040                                    01970251
 0040 CONTINUE                                                          01980251
      RVCOMP = 10.0                                                     01990251
      NVTN01 = 4.0                                                      02000251
      RVCOMP = NVTN01/5                                                 02010251
      RVCORR = 0.8                                                      02020251
40040 IF ( RVCOMP - 0.79995 )  20040, 10040, 40041                      02030251
40041 IF ( RVCOMP - 0.80005 )  10040, 10040, 20040                      02040251
30040 IVDELE = IVDELE + 1                                               02050251
      WRITE (I02,80000) IVTNUM                                          02060251
      IF (ICZERO) 10040, 0051, 20040                                    02070251
10040 IVPASS = IVPASS + 1                                               02080251
      WRITE (I02,80002) IVTNUM                                          02090251
      GO TO 0051                                                        02100251
20040 IVFAIL = IVFAIL + 1                                               02110251
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02120251
 0051 CONTINUE                                                          02130251
C                                                                       02140251
C     ****  FCVS PROGRAM 251  -  TEST 005  ****                         02150251
C                                                                       02160251
C        TEST 005 ASSIGNS A LOGICAL VALUE OF .TRUE. TO MVTN02 WHICH WAS 02170251
C     SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT TYPE STATEMENT AFTER ALSO02180251
C     HAVING ITS FIRST LETTER M SPECIFIED AS TYPE LOGICAL IN AN         02190251
C     IMPLICIT STATEMENT.                                               02200251
C                  IMPLICIT LOGICAL (M,N)                               02210251
C                  LOGICAL MVTN02                                       02220251
C                                                                       02230251
      IVTNUM =   5                                                      02240251
      IF (ICZERO) 30050, 0050, 30050                                    02250251
 0050 CONTINUE                                                          02260251
      IVCOMP = 0                                                        02270251
      LCON02 = .TRUE.                                                   02280251
      MVTN02 = LCON02                                                   02290251
      IF ( MVTN02 )  IVCOMP = 1                                         02300251
      IVCORR = 1                                                        02310251
40050 IF ( IVCOMP - 1 )  20050, 10050, 20050                            02320251
30050 IVDELE = IVDELE + 1                                               02330251
      WRITE (I02,80000) IVTNUM                                          02340251
      IF (ICZERO) 10050, 0061, 20050                                    02350251
10050 IVPASS = IVPASS + 1                                               02360251
      WRITE (I02,80002) IVTNUM                                          02370251
      GO TO 0061                                                        02380251
20050 IVFAIL = IVFAIL + 1                                               02390251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02400251
 0061 CONTINUE                                                          02410251
C                                                                       02420251
C     ****  FCVS PROGRAM 251  -  TEST 006  ****                         02430251
C                                                                       02440251
C        TEST 006 ASSIGNS A LOGICAL VALUE OF .FALSE. TO NVTN02 WHICH WAS02450251
C     SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT TYPE STATEMENT AFTER ALSO02460251
C     HAVING ITS FIRST LETTER N SPECIFIED AS TYPE LOGICAL IN AN         02470251
C     IMPLICIT STATEMENT.                                               02480251
C                  IMPLICIT LOGICAL (M,N)                               02490251
C                  LOGICAL NVTN02                                       02500251
C                                                                       02510251
      IVTNUM =   6                                                      02520251
      IF (ICZERO) 30060, 0060, 30060                                    02530251
 0060 CONTINUE                                                          02540251
      IVCOMP = 1                                                        02550251
      NVTN02 = .FALSE.                                                  02560251
      IF ( NVTN02 )  IVCOMP = 0                                         02570251
      IVCORR = 1                                                        02580251
40060 IF ( IVCOMP - 1 )  20060, 10060, 20060                            02590251
30060 IVDELE = IVDELE + 1                                               02600251
      WRITE (I02,80000) IVTNUM                                          02610251
      IF (ICZERO) 10060, 0071, 20060                                    02620251
10060 IVPASS = IVPASS + 1                                               02630251
      WRITE (I02,80002) IVTNUM                                          02640251
      GO TO 0071                                                        02650251
20060 IVFAIL = IVFAIL + 1                                               02660251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02670251
 0071 CONTINUE                                                          02680251
C                                                                       02690251
C     ****  FCVS PROGRAM 251  -  TEST 007  ****                         02700251
C                                                                       02710251
C        TEST 007 ASSIGNS A LOGICAL VALUE OF .TRUE. TO THE ARRAY ELEMENT02720251
C     MATN21(1,1) WHICH WAS SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT    02730251
C     TYPE STATEMENT AFTER ALSO HAVING ITS FIRST LETTER M SPECIFIED AS  02740251
C     TYPE LOGICAL IN AN IMPLICIT STATEMENT.                            02750251
C                  IMPLICIT LOGICAL (M,N)                               02760251
C                  LOGICAL MATN21(3,3)                                  02770251
C                                                                       02780251
      IVTNUM =   7                                                      02790251
      IF (ICZERO) 30070, 0070, 30070                                    02800251
 0070 CONTINUE                                                          02810251
      IVCOMP = 0                                                        02820251
      MATN21(1,1) = .TRUE.                                              02830251
      IF ( MATN21(1,1) )  IVCOMP = 1                                    02840251
      IVCORR = 1                                                        02850251
40070 IF ( IVCOMP - 1 )  20070, 10070, 20070                            02860251
30070 IVDELE = IVDELE + 1                                               02870251
      WRITE (I02,80000) IVTNUM                                          02880251
      IF (ICZERO) 10070, 0081, 20070                                    02890251
10070 IVPASS = IVPASS + 1                                               02900251
      WRITE (I02,80002) IVTNUM                                          02910251
      GO TO 0081                                                        02920251
20070 IVFAIL = IVFAIL + 1                                               02930251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02940251
 0081 CONTINUE                                                          02950251
C                                                                       02960251
C     ****  FCVS PROGRAM 251  -  TEST 008  ****                         02970251
C                                                                       02980251
C        TEST 008 ASSIGNS AN INTEGER VALUE OF 4 TO AVIN01 WHICH WAS     02990251
C     SPECIFIED AS TYPE INTEGER IN AN IMPLICIT STATEMENT.               03000251
C                  IMPLICIT INTEGER (A,B)                               03010251
C                                                                       03020251
      IVTNUM =   8                                                      03030251
      IF (ICZERO) 30080, 0080, 30080                                    03040251
 0080 CONTINUE                                                          03050251
      RVCOMP = 10.0                                                     03060251
      AVIN01 = 4                                                        03070251
      RVCOMP = AVIN01/5                                                 03080251
      RVCORR = 0.0                                                      03090251
40080 IF ( RVCOMP )  20080, 10080, 20080                                03100251
30080 IVDELE = IVDELE + 1                                               03110251
      WRITE (I02,80000) IVTNUM                                          03120251
      IF (ICZERO) 10080, 0091, 20080                                    03130251
10080 IVPASS = IVPASS + 1                                               03140251
      WRITE (I02,80002) IVTNUM                                          03150251
      GO TO 0091                                                        03160251
20080 IVFAIL = IVFAIL + 1                                               03170251
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03180251
 0091 CONTINUE                                                          03190251
C                                                                       03200251
C     ****  FCVS PROGRAM 251  -  TEST 009  ****                         03210251
C                                                                       03220251
C        TEST 009 ASSIGNS A LOGICAL VALUE OF .TRUE. TO AVTN01 WHICH WAS 03230251
C     SPECIFIED AS TYPE LOGICAL EXPLICITLY IN A TYPE STATEMENT.         03240251
C                  LOGICAL AVTN01                                       03250251
C     THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT LOGICAL TYPE        03260251
C     STATEMENT CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD         03270251
C     SET THE TYPE AS INTEGER.                                          03280251
C                  IMPLICIT INTEGER (A,B)                               03290251
C                                                                       03300251
      IVTNUM =   9                                                      03310251
      IF (ICZERO) 30090, 0090, 30090                                    03320251
 0090 CONTINUE                                                          03330251
      IVCOMP = 0                                                        03340251
      AVTN01 = .TRUE.                                                   03350251
      IF ( AVTN01 )  IVCOMP = 1                                         03360251
      IVCORR = 1                                                        03370251
40090 IF ( IVCOMP - 1 )  20090, 10090, 20090                            03380251
30090 IVDELE = IVDELE + 1                                               03390251
      WRITE (I02,80000) IVTNUM                                          03400251
      IF (ICZERO) 10090, 0101, 20090                                    03410251
10090 IVPASS = IVPASS + 1                                               03420251
      WRITE (I02,80002) IVTNUM                                          03430251
      GO TO 0101                                                        03440251
20090 IVFAIL = IVFAIL + 1                                               03450251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03460251
 0101 CONTINUE                                                          03470251
C                                                                       03480251
C     ****  FCVS PROGRAM 251  -  TEST 010  ****                         03490251
C                                                                       03500251
C        TEST 010 ASSIGNS A REAL VALUE OF 4.0 TO IVIN01 WHICH WAS       03510251
C     SPECIFIED AS REAL IMPLICITLY IN AN IMPLICIT STATEMENT.            03520251
C                  IMPLICIT REAL (I,J)                                  03530251
C                                                                       03540251
      IVTNUM =  10                                                      03550251
      IF (ICZERO) 30100, 0100, 30100                                    03560251
 0100 CONTINUE                                                          03570251
      RVCOMP = 10.0                                                     03580251
      IVIN01 = 4.0                                                      03590251
      RVCOMP = IVIN01/5                                                 03600251
      RVCORR = 0.8                                                      03610251
40100 IF ( RVCOMP - 0.79995 ) 20100, 10100, 40101                       03620251
40101 IF ( RVCOMP - 0.80005 ) 10100, 10100, 20100                       03630251
30100 IVDELE = IVDELE + 1                                               03640251
      WRITE (I02,80000) IVTNUM                                          03650251
      IF (ICZERO) 10100, 0111, 20100                                    03660251
10100 IVPASS = IVPASS + 1                                               03670251
      WRITE (I02,80002) IVTNUM                                          03680251
      GO TO 0111                                                        03690251
20100 IVFAIL = IVFAIL + 1                                               03700251
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03710251
 0111 CONTINUE                                                          03720251
C                                                                       03730251
C     ****  FCVS PROGRAM 251  -  TEST 011  ****                         03740251
C                                                                       03750251
C        TEST 011 ASSIGNS A LOGICAL VALUE OF .FALSE. TO IVTN01 WHICH WAS03760251
C     SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT TYPE STATEMENT.          03770251
C                  LOGICAL IVTN01                                       03780251
C     THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT TYPE STATEMENT      03790251
C     CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD SET THE TYPE      03800251
C     AS REAL.                                                          03810251
C                  IMPLICIT REAL (I,J)                                  03820251
C                                                                       03830251
      IVTNUM =  11                                                      03840251
      IF (ICZERO) 30110, 0110, 30110                                    03850251
 0110 CONTINUE                                                          03860251
      IVCOMP = 1                                                        03870251
      IVTN01 = .FALSE.                                                  03880251
      IF ( IVTN01 )  IVCOMP = 0                                         03890251
      IVCORR = 1                                                        03900251
40110 IF ( IVCOMP - 1 )  20110, 10110, 20110                            03910251
30110 IVDELE = IVDELE + 1                                               03920251
      WRITE (I02,80000) IVTNUM                                          03930251
      IF (ICZERO) 10110, 0121, 20110                                    03940251
10110 IVPASS = IVPASS + 1                                               03950251
      WRITE (I02,80002) IVTNUM                                          03960251
      GO TO 0121                                                        03970251
20110 IVFAIL = IVFAIL + 1                                               03980251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03990251
 0121 CONTINUE                                                          04000251
C                                                                       04010251
C                                                                       04020251
C        THE NEXT TWO TESTS CHECK THE RANGE OF LETTERS THAT             04030251
C     ARE SET BY THE IMPLICIT STATEMENT AS FOLLOWS -                    04040251
C     IMPLICIT LOGICAL ( E-H, O, P-Q,  S-T, X-Y ), INTEGER ( U-W )      04050251
C                                                                       04060251
C                                                                       04070251
C                                                                       04080251
C     ****  FCVS PROGRAM 251  -  TEST 012  ****                         04090251
C                                                                       04100251
C        TEST 012 ASSIGNS A LOGICAL VALUE OF .TRUE. TO A SERIES OF      04110251
C     VARIABLES THAT BEGIN WITH THE FOLLOWING LETTERS -                 04120251
C                                                                       04130251
C        E  F  G  H  O  P  Q  S  T  X  Y                                04140251
C                                                                       04150251
C     VARIABLES THAT BEGIN WITH THESE LETTERS SHOULD BE IMPLICITLY TYPED04160251
C     LOGICAL BECAUSE OF THE IMPLICIT STATEMENT USING BOTH THE RANGE AND04170251
C     SINGLE LETTER SPECIFICATION FOR TYPE LOGICAL.  THE VARIABLE XVIN0104180251
C     IS FIRST USED IN A LOGICAL IF STATEMENT.  THE TRUE BRANCH SHOULD  04190251
C     BE TAKEN TO SET IVCOMP = 1.  THEN EACH OF THE VARIABLES SET TO    04200251
C     .TRUE. ARE USED IN A SECOND LOGICAL IF STATEMENT WHICH IS ONE     04210251
C     LARGE LOGICAL CONJUNCTION ( VARIABLE .AND. VARIABLE .AND. ... ).  04220251
C     THE TRUE BRANCH SHOULD BE TAKEN TO INCREMENT THE VALUE OF IVCOMP  04230251
C     TO A FINAL VALUE OF THREE (3).                                    04240251
C                                                                       04250251
C                                                                       04260251
      IVTNUM =  12                                                      04270251
      IF (ICZERO) 30120, 0120, 30120                                    04280251
 0120 CONTINUE                                                          04290251
      IVCOMP = 0                                                        04300251
      IVCORR = 3                                                        04310251
      EVIN01 = .TRUE.                                                   04320251
      FVIN01 = .TRUE.                                                   04330251
      GVIN01 = .TRUE.                                                   04340251
      HVIN01 = .TRUE.                                                   04350251
      OVIN01 = .TRUE.                                                   04360251
      PVIN01 = .TRUE.                                                   04370251
      QVIN01 = .TRUE.                                                   04380251
      SVIN01 = .TRUE.                                                   04390251
      TVIN01 = .TRUE.                                                   04400251
      XVIN01 = .TRUE.                                                   04410251
      YVIN01 = .TRUE.                                                   04420251
      IF ( XVIN01 )  IVCOMP = 1                                         04430251
      IF ( EVIN01 .AND. FVIN01 .AND. GVIN01 .AND. HVIN01 .AND. OVIN01   04440251
     1.AND. PVIN01 .AND. QVIN01 .AND. SVIN01 .AND. TVIN01 .AND. XVIN01  04450251
     2.AND. YVIN01 )  IVCOMP = IVCOMP + 2                               04460251
40120 IF ( IVCOMP - 3 ) 20120, 10120, 20120                             04470251
30120 IVDELE = IVDELE + 1                                               04480251
      WRITE (I02,80000) IVTNUM                                          04490251
      IF (ICZERO) 10120, 0131, 20120                                    04500251
10120 IVPASS = IVPASS + 1                                               04510251
      WRITE (I02,80002) IVTNUM                                          04520251
      GO TO 0131                                                        04530251
20120 IVFAIL = IVFAIL + 1                                               04540251
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04550251
 0131 CONTINUE                                                          04560251
C                                                                       04570251
C     ****  FCVS PROGRAM 251  -  TEST 013  ****                         04580251
C                                                                       04590251
C        TEST 013 ASSIGNS AN INTEGER VALUE OF 4 TO VVIN01 WHICH         04600251
C     WAS SPECIFIED AS TYPE INTEGER IMPLICITLY USING THE RANGE OF       04610251
C     LETTERS  U-W  IN THE IMPLICIT INTEGER SPECIFICATION STATEMENT.    04620251
C     DIVISION IS USED TO DETERMINE WHETHER VVIN01 IS TYPE INTEGER.     04630251
C                                                                       04640251
C                                                                       04650251
      IVTNUM =  13                                                      04660251
      IF (ICZERO) 30130, 0130, 30130                                    04670251
 0130 CONTINUE                                                          04680251
      RVCOMP = 10.0                                                     04690251
      VVIN01 = 4                                                        04700251
      RVCOMP = VVIN01/5                                                 04710251
      RVCORR = 0.0                                                      04720251
40130 IF ( RVCOMP )  20130, 10130, 20130                                04730251
30130 IVDELE = IVDELE + 1                                               04740251
      WRITE (I02,80000) IVTNUM                                          04750251
      IF (ICZERO) 10130, 0141, 20130                                    04760251
10130 IVPASS = IVPASS + 1                                               04770251
      WRITE (I02,80002) IVTNUM                                          04780251
      GO TO 0141                                                        04790251
20130 IVFAIL = IVFAIL + 1                                               04800251
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04810251
 0141 CONTINUE                                                          04820251
C                                                                       04830251
C                                                                       04840251
C     WRITE OUT TEST SUMMARY                                            04850251
C                                                                       04860251
      WRITE (I02,90004)                                                 04870251
      WRITE (I02,90014)                                                 04880251
      WRITE (I02,90004)                                                 04890251
      WRITE (I02,90000)                                                 04900251
      WRITE (I02,90004)                                                 04910251
      WRITE (I02,90020) IVFAIL                                          04920251
      WRITE (I02,90022) IVPASS                                          04930251
      WRITE (I02,90024) IVDELE                                          04940251
      STOP                                                              04950251
90001 FORMAT (1H ,24X,5HFM251)                                          04960251
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM251)                          04970251
C                                                                       04980251
C     FORMATS FOR TEST DETAIL LINES                                     04990251
C                                                                       05000251
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   05010251
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      05020251
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         05030251
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    05040251
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        05050251
C                                                                       05060251
C     FORMAT STATEMENTS FOR PAGE HEADERS                                05070251
C                                                                       05080251
90002 FORMAT (1H1)                                                      05090251
90004 FORMAT (1H )                                                      05100251
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05110251
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   05120251
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         05130251
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  05140251
90014 FORMAT (1H ,5X,46H----------------------------------------------) 05150251
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             05160251
C                                                                       05170251
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 05180251
C                                                                       05190251
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              05200251
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              05210251
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             05220251
      END                                                               05230251
