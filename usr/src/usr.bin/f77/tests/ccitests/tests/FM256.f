      PROGRAM FM256                                                     00010256
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020256
C                                                                       00030256
C                                                                       00040256
C        THIS ROUTINE IS A TEST OF THE DO STATEMENT.  THE DO IS TESTED  00050256
C     BOTH OUTSIDE AND INSIDE THE BLOCK-IF STRUCTURE.  TESTS ARE MADE OF00060256
C     THE DO-VARIABLE WHEN THE DO BECOMES INACTIVE.  OTHER TESTS CHECK  00070256
C     LOOP AND INCREMENTATION PROCESSING.  THE DO-LOOP EXECUTION        00080256
C     IS TESTED FOR THOSE CONDITIONS WHICH MAKE THE DO-LOOP INACTIVE.   00090256
C                                                                       00100256
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110256
C             X3.9-1978                                                 00120256
C        SECTION 11.10,      DO STATEMENT                               00130256
C        SECTION 11.10.1,    RANGE OF A DO-LOOP                         00140256
C        SECTION 11.10.2,    ACTIVE AND INACTIVE DO-LOOPS               00150256
C        SECTION 11.10.3,    EXECUTING A DO STATEMENT                   00160256
C        SECTION 11.10.4,    LOOP CONTROL PROCESSING                    00170256
C        SECTION 11.10.5,    EXECUTION OF THE RANGE                     00180256
C        SECTION 11.10.6,    TERMINAL STATEMENT EXECUTION               00190256
C        SECTION 11.10.7,    INCREMENTATION PROCESSING                  00200256
C                                                                       00210256
C        FM012 - TESTS THE DO STATEMENT WITH THE FORTRAN 66 CONCEPTS OF 00220256
C                EXTENDED RANGE OF A DO STATEMENT.                      00230256
C                                                                       00240256
C                                                                       00250256
C                                                                       00260256
C     ******************************************************************00270256
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00280256
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00290256
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00300256
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00310256
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00320256
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00330256
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00340256
C     THE RESULT OF EXECUTING THESE TESTS.                              00350256
C                                                                       00360256
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00370256
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00380256
C                                                                       00390256
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00400256
C                    DEPARTMENT OF THE NAVY                             00410256
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00420256
C                    WASHINGTON, D.C.   20376                           00430256
C                                                                       00440256
C     ******************************************************************00450256
C                                                                       00460256
C                                                                       00470256
      IMPLICIT LOGICAL (L)                                              00480256
      IMPLICIT CHARACTER*14 (C)                                         00490256
C                                                                       00500256
C                                                                       00510256
C                                                                       00520256
C     INITIALIZATION SECTION.                                           00530256
C                                                                       00540256
C     INITIALIZE CONSTANTS                                              00550256
C     ********************                                              00560256
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00570256
      I01 = 5                                                           00580256
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00590256
      I02 = 6                                                           00600256
C     SYSTEM ENVIRONMENT SECTION                                        00610256
C                                                                       00620256
      I01 = 5                                                           00630256
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00640256
C     (UNIT NUMBER FOR CARD READER).                                    00650256
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00660256
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00670256
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00680256
C                                                                       00690256
      I02 = 6                                                           00700256
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00710256
C     (UNIT NUMBER FOR PRINTER).                                        00720256
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00730256
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00740256
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00750256
C                                                                       00760256
      IVPASS = 0                                                        00770256
      IVFAIL = 0                                                        00780256
      IVDELE = 0                                                        00790256
      ICZERO = 0                                                        00800256
C                                                                       00810256
C     WRITE OUT PAGE HEADERS                                            00820256
C                                                                       00830256
      WRITE (I02,90002)                                                 00840256
      WRITE (I02,90006)                                                 00850256
      WRITE (I02,90008)                                                 00860256
      WRITE (I02,90004)                                                 00870256
      WRITE (I02,90010)                                                 00880256
      WRITE (I02,90004)                                                 00890256
      WRITE (I02,90016)                                                 00900256
      WRITE (I02,90001)                                                 00910256
      WRITE (I02,90004)                                                 00920256
      WRITE (I02,90012)                                                 00930256
      WRITE (I02,90014)                                                 00940256
      WRITE (I02,90004)                                                 00950256
C                                                                       00960256
C                                                                       00970256
C     ****  FCVS PROGRAM 256  -  TEST 001  ****                         00980256
C                                                                       00990256
C        TEST 001 CHECKS THE SIMPLE DO STATEMENT WITH THE OPTIONAL      01000256
C     COMMAS AND ALL DO PARAMETERS SPECIFIED.  THE LOOP IS ACTIVE FOR   01010256
C     TEN COUNTS.  THE FINAL VALUE OF THE INTEGER COUNTER SHOULD BE     01020256
C     EQUAL TO TEN (10).  THE FORM OF THE DO STATEMENT USED IN THIS TEST01030256
C     IS SHOWN BELOW -                                                  01040256
C                                                                       01050256
C        DO S, I = E1, E2, E3                                           01060256
C                                                                       01070256
C                                                                       01080256
      IVTNUM =   1                                                      01090256
      IF (ICZERO) 30010, 0010, 30010                                    01100256
 0010 CONTINUE                                                          01110256
      IVCOMP = 0                                                        01120256
      DO 0012, IVON01 = 1, 10, 1                                        01130256
      IVCOMP = IVCOMP + 1                                               01140256
 0012 CONTINUE                                                          01150256
      IVCORR = 10                                                       01160256
40010 IF ( IVCOMP - 10 )  20010, 10010, 20010                           01170256
30010 IVDELE = IVDELE + 1                                               01180256
      WRITE (I02,80000) IVTNUM                                          01190256
      IF (ICZERO) 10010, 0021, 20010                                    01200256
10010 IVPASS = IVPASS + 1                                               01210256
      WRITE (I02,80002) IVTNUM                                          01220256
      GO TO 0021                                                        01230256
20010 IVFAIL = IVFAIL + 1                                               01240256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01250256
 0021 CONTINUE                                                          01260256
C                                                                       01270256
C     ****  FCVS PROGRAM 256  -  TEST 002  ****                         01280256
C                                                                       01290256
C        TEST 002 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT THE COMMAS01300256
C     THAT ARE OPTIONAL HAVE BEEN DELETED AS A SYNTAX CHECK.            01310256
C                                                                       01320256
C        THE INCREMENTATION PARAMETER IS OPTIONAL AND NOT PRESENT IN    01330256
C     THIS TEST.    ACCORDING TO SECTION 11.10.3,  IF E3 DOES NOT APPEAR01340256
C     THEN M3 HAS A VALUE OF ONE.  THE DO STATEMENT FOR THIS TEST IS OF 01350256
C     THE FORM SHOWN BELOW -                                            01360256
C                                                                       01370256
C        DO S I = E1, E2                                                01380256
C                                                                       01390256
C                                                                       01400256
      IVTNUM =   2                                                      01410256
      IF (ICZERO) 30020, 0020, 30020                                    01420256
 0020 CONTINUE                                                          01430256
      IVCOMP = 0                                                        01440256
      DO 0022 IVON01 = 1, 10                                            01450256
      IVCOMP = IVCOMP + 1                                               01460256
 0022 CONTINUE                                                          01470256
      IVCORR = 10                                                       01480256
40020 IF ( IVCOMP - 10 )  20020, 10020, 20020                           01490256
30020 IVDELE = IVDELE + 1                                               01500256
      WRITE (I02,80000) IVTNUM                                          01510256
      IF (ICZERO) 10020, 0031, 20020                                    01520256
10020 IVPASS = IVPASS + 1                                               01530256
      WRITE (I02,80002) IVTNUM                                          01540256
      GO TO 0031                                                        01550256
20020 IVFAIL = IVFAIL + 1                                               01560256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01570256
 0031 CONTINUE                                                          01580256
C                                                                       01590256
C     ****  FCVS PROGRAM 256  -  TEST 003  ****                         01600256
C                                                                       01610256
C        TEST 003 HAS A DO STATEMENT INSIDE A BLOCKED IF STRUCTURE.     01620256
C     THE LOGICAL EXPRESSION IS TRUE SO THE DO-LOOP SHOULD BE EXECUTED  01630256
C     A TOTAL OF TEN TIMES.                                             01640256
C                                                                       01650256
C                                                                       01660256
      IVTNUM =   3                                                      01670256
      IF (ICZERO) 30030, 0030, 30030                                    01680256
 0030 CONTINUE                                                          01690256
      IVCOMP = 0                                                        01700256
      LVON01 = .TRUE.                                                   01710256
      IF ( LVON01 )  THEN                                               01720256
           DO 0032, IVON01 = 1, 10, 1                                   01730256
           IVCOMP = IVCOMP + 1                                          01740256
 0032      CONTINUE                                                     01750256
      END IF                                                            01760256
      IVCORR = 10                                                       01770256
40030 IF ( IVCOMP - 10 )  20030, 10030, 20030                           01780256
30030 IVDELE = IVDELE + 1                                               01790256
      WRITE (I02,80000) IVTNUM                                          01800256
      IF (ICZERO) 10030, 0041, 20030                                    01810256
10030 IVPASS = IVPASS + 1                                               01820256
      WRITE (I02,80002) IVTNUM                                          01830256
      GO TO 0041                                                        01840256
20030 IVFAIL = IVFAIL + 1                                               01850256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01860256
 0041 CONTINUE                                                          01870256
C                                                                       01880256
C     ****  FCVS PROGRAM 256  -  TEST 004  ****                         01890256
C                                                                       01900256
C        TEST 004 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT THE DO    01910256
C     STATEMENT IS LOCATED IN AN ELSE IF-BLOCK.  THE DO-LOOP SHOULD BE  01920256
C     EXECUTED FIVE (5) TIMES.                                          01930256
C                                                                       01940256
C                                                                       01950256
      IVTNUM =   4                                                      01960256
      IF (ICZERO) 30040, 0040, 30040                                    01970256
 0040 CONTINUE                                                          01980256
      IVCOMP = 0                                                        01990256
      LVON01 = .FALSE.                                                  02000256
      LVON02 = .TRUE.                                                   02010256
      IF ( LVON01 )  THEN                                               02020256
           IVCOMP = 32000                                               02030256
      ELSE IF ( LVON02 )  THEN                                          02040256
           DO 0042 IVON01 = 1, 5                                        02050256
           IVCOMP = IVCOMP + 1                                          02060256
 0042      CONTINUE                                                     02070256
      END IF                                                            02080256
      IVCORR = 5                                                        02090256
40040 IF ( IVCOMP - 5 )  20040, 10040, 20040                            02100256
30040 IVDELE = IVDELE + 1                                               02110256
      WRITE (I02,80000) IVTNUM                                          02120256
      IF (ICZERO) 10040, 0051, 20040                                    02130256
10040 IVPASS = IVPASS + 1                                               02140256
      WRITE (I02,80002) IVTNUM                                          02150256
      GO TO 0051                                                        02160256
20040 IVFAIL = IVFAIL + 1                                               02170256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02180256
 0051 CONTINUE                                                          02190256
C                                                                       02200256
C     ****  FCVS PROGRAM 256  -  TEST 005  ****                         02210256
C                                                                       02220256
C        TEST 005 IS SIMILAR TO THE PREVIOUS TWO TESTS EXCEPT THAT THE  02230256
C     DO STATEMENT IS CONTAINED IN AN ELSE-BLOCK.  THE DO-LOOP SHOULD BE02240256
C     EXECUTED A TOTAL OF 3 TIMES.                                      02250256
C                                                                       02260256
C                                                                       02270256
      IVTNUM =   5                                                      02280256
      IF (ICZERO) 30050, 0050, 30050                                    02290256
 0050 CONTINUE                                                          02300256
      IVCOMP = 0                                                        02310256
      LVON01 = .FALSE.                                                  02320256
      LVON02 = .FALSE.                                                  02330256
      IF ( LVON01 )  THEN                                               02340256
           IVCOMP = 100                                                 02350256
      ELSE IF ( LVON02 )  THEN                                          02360256
           IVCOMP = 1000                                                02370256
      ELSE                                                              02380256
           DO 0052, IVON01 = 1, 3                                       02390256
           IVCOMP = IVCOMP + 1                                          02400256
 0052      CONTINUE                                                     02410256
      END IF                                                            02420256
      IVCORR = 3                                                        02430256
40050 IF ( IVCOMP - 3 )  20050, 10050, 20050                            02440256
30050 IVDELE = IVDELE + 1                                               02450256
      WRITE (I02,80000) IVTNUM                                          02460256
      IF (ICZERO) 10050, 0061, 20050                                    02470256
10050 IVPASS = IVPASS + 1                                               02480256
      WRITE (I02,80002) IVTNUM                                          02490256
      GO TO 0061                                                        02500256
20050 IVFAIL = IVFAIL + 1                                               02510256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02520256
 0061 CONTINUE                                                          02530256
C                                                                       02540256
C     ****  FCVS PROGRAM 256  -  TEST 006  ****                         02550256
C                                                                       02560256
C        TEST 006 HAS A BLOCKED IF STRUCTURE INSIDE A DO-LOOP.          02570256
C     THE LOOP IS EXECUTED THREE (3) TIMES.  ALL THREE PARTS OF THE     02580256
C     BLOCK-IF STRUCTURE SHOULD BE EXECUTED.                            02590256
C                                                                       02600256
C                                                                       02610256
      IVTNUM =   6                                                      02620256
      IF (ICZERO) 30060, 0060, 30060                                    02630256
 0060 CONTINUE                                                          02640256
      IVCOMP = 1                                                        02650256
      DO 0062, IVON01 = 3, 5, 1                                         02660256
      IF ( IVON01 .LE. 3 )  THEN                                        02670256
           IVCOMP = IVCOMP * 2                                          02680256
      ELSE IF ( IVON01 .GT. 3 .AND. IVON01 .LT. 5 )  THEN               02690256
           IVCOMP = IVCOMP * 3                                          02700256
      ELSE                                                              02710256
           IVCOMP = IVCOMP * 5                                          02720256
      END IF                                                            02730256
 0062 CONTINUE                                                          02740256
C                                                                       02750256
C        **** IVCOMP IS DETERMINED BY IVCOMP = 30 = 1 * 2 * 3 * 5   ****02760256
C                                                                       02770256
      IVCORR = 30                                                       02780256
40060 IF ( IVCOMP - 30 )  20060, 10060, 20060                           02790256
30060 IVDELE = IVDELE + 1                                               02800256
      WRITE (I02,80000) IVTNUM                                          02810256
      IF (ICZERO) 10060, 0071, 20060                                    02820256
10060 IVPASS = IVPASS + 1                                               02830256
      WRITE (I02,80002) IVTNUM                                          02840256
      GO TO 0071                                                        02850256
20060 IVFAIL = IVFAIL + 1                                               02860256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02870256
 0071 CONTINUE                                                          02880256
C                                                                       02890256
C                                                                       02900256
C        THE FOLLOWING SERIES OF TESTS CHECK THE DO-VARIABLE WHEN THE   02910256
C     DO-LOOP BECOMES INACTIVE.  ACCORDING TO SECTION 11.10.2,  WHEN A  02920256
C     DO-LOOP BECOMES INACTIVE, THE DO-VARIABLE OF THE DO-LOOP RETAINS  02930256
C     ITS LAST DEFINED VALUE.                                           02940256
C                                                                       02950256
C                                                                       02960256
C                                                                       02970256
C     ****  FCVS PROGRAM 256  -  TEST 007  ****                         02980256
C                                                                       02990256
C        TEST 007 CHECKS THAT THE DO-VARIABLE CONTAINS ITS LAST DEFINED 03000256
C     VALUE WHEN THE ITERATION COUNT IS ZERO.                           03010256
C                                                                       03020256
C                                                                       03030256
      IVTNUM =   7                                                      03040256
      IF (ICZERO) 30070, 0070, 30070                                    03050256
 0070 CONTINUE                                                          03060256
      IVCOMP = 0                                                        03070256
      IVON02 = 0                                                        03080256
      DO 0072 IVON01 = 100, 105, 2                                      03090256
      IVON02 = IVON02 + 1                                               03100256
 0072 CONTINUE                                                          03110256
      IVCOMP = IVON01                                                   03120256
      IVCORR = 106                                                      03130256
40070 IF ( IVCOMP - 106 )  20070, 10070, 20070                          03140256
30070 IVDELE = IVDELE + 1                                               03150256
      WRITE (I02,80000) IVTNUM                                          03160256
      IF (ICZERO) 10070, 0081, 20070                                    03170256
10070 IVPASS = IVPASS + 1                                               03180256
      WRITE (I02,80002) IVTNUM                                          03190256
      GO TO 0081                                                        03200256
20070 IVFAIL = IVFAIL + 1                                               03210256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03220256
 0081 CONTINUE                                                          03230256
C                                                                       03240256
C     ****  FCVS PROGRAM 256  -  TEST 008  ****                         03250256
C                                                                       03260256
C        TEST 008 CHECKS THAT THE LOOP COUNTER IN THE PREVIOUS TEST HAD 03270256
C     A VALUE OF THREE TO SHOW THAT THE DO-LOOP WAS EXECUTED THREE TIMES03280256
C     BEFORE TERMINATING ( BECOMMING INACTIVE ).                        03290256
C                                                                       03300256
C                                                                       03310256
      IVTNUM =   8                                                      03320256
      IF (ICZERO) 30080, 0080, 30080                                    03330256
 0080 CONTINUE                                                          03340256
      IVCOMP = 0                                                        03350256
      IVCOMP = IVON02                                                   03360256
      IVCORR = 3                                                        03370256
40080 IF ( IVCOMP - 3 )  20080, 10080, 20080                            03380256
30080 IVDELE = IVDELE + 1                                               03390256
      WRITE (I02,80000) IVTNUM                                          03400256
      IF (ICZERO) 10080, 0091, 20080                                    03410256
10080 IVPASS = IVPASS + 1                                               03420256
      WRITE (I02,80002) IVTNUM                                          03430256
      GO TO 0091                                                        03440256
20080 IVFAIL = IVFAIL + 1                                               03450256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03460256
 0091 CONTINUE                                                          03470256
C                                                                       03480256
C     ****  FCVS PROGRAM 256  -  TEST 009  ****                         03490256
C                                                                       03500256
C        TEST 009 CHECKS THAT A DO-LOOP BECOMES INACTIVE IF THERE IS A  03510256
C     TRANSFER OF CONTROL OUTSIDE THE RANGE OF THE DO-LOOP.  THE TRANS- 03520256
C     FER MUST BE INSIDE OF THE SAME PROGRAM UNIT - NOT A CALL OR       03530256
C     FUNCTION REFERENCE TO A SUBPROGRAM.                               03540256
C                                                                       03550256
C        THIS IS A SIGNIFICANT DIFFERENCE BETWEEN FORTRAN 66 AND FORTRAN03560256
C     77.  FORTRAN 66 HAD AN EXTENDED RANGE OF THE DO FEATURE WHICH     03570256
C     ALLOWED FOR A TRANSFER OUTSIDE THE RANGE OF A DO-LOOP WITHOUT     03580256
C     MAKING THE DO-LOOP INACTIVE.                                      03590256
C                                                                       03600256
C                                                                       03610256
      IVTNUM =   9                                                      03620256
      IF (ICZERO) 30090, 0090, 30090                                    03630256
 0090 CONTINUE                                                          03640256
      IVCOMP = 0                                                        03650256
      DO 0092 IVON01 = 1, 7                                             03660256
      IF ( IVON01 .GE. 3 )  GO TO 0093                                  03670256
 0092 CONTINUE                                                          03680256
 0093 IVCOMP = IVON01                                                   03690256
      IVCORR = 3                                                        03700256
40090 IF ( IVCOMP - 3 )  20090, 10090, 20090                            03710256
30090 IVDELE = IVDELE + 1                                               03720256
      WRITE (I02,80000) IVTNUM                                          03730256
      IF (ICZERO) 10090, 0101, 20090                                    03740256
10090 IVPASS = IVPASS + 1                                               03750256
      WRITE (I02,80002) IVTNUM                                          03760256
      GO TO 0101                                                        03770256
20090 IVFAIL = IVFAIL + 1                                               03780256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03790256
 0101 CONTINUE                                                          03800256
C                                                                       03810256
C     ****  FCVS PROGRAM 256  -  TEST 010  ****                         03820256
C                                                                       03830256
C        TEST 010 CHECKS FOR AN INITIAL COUNT EQUAL TO ZERO BECAUSE     03840256
C     M1 IS GREATER THAN M2 AND M3 IS GREATER THAN ZERO - SEE SECTION   03850256
C     11.10.3 FOR CONDITIONS WHICH MAKE THE ITERATION COUNT ZERO.       03860256
C     THE LOOP SHOULD NOT BE EXECUTED AT ALL.                           03870256
C                                                                       03880256
C                                                                       03890256
      IVTNUM =  10                                                      03900256
      IF (ICZERO) 30100, 0100, 30100                                    03910256
 0100 CONTINUE                                                          03920256
      IVCOMP = 0                                                        03930256
      DO 0102, IVON01 = 100, 10, 3                                      03940256
      IVCOMP = IVCOMP + 1                                               03950256
 0102 CONTINUE                                                          03960256
      IVCORR = 0                                                        03970256
40100 IF ( IVCOMP )  20100, 10100, 20100                                03980256
30100 IVDELE = IVDELE + 1                                               03990256
      WRITE (I02,80000) IVTNUM                                          04000256
      IF (ICZERO) 10100, 0111, 20100                                    04010256
10100 IVPASS = IVPASS + 1                                               04020256
      WRITE (I02,80002) IVTNUM                                          04030256
      GO TO 0111                                                        04040256
20100 IVFAIL = IVFAIL + 1                                               04050256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04060256
 0111 CONTINUE                                                          04070256
C                                                                       04080256
C     ****  FCVS PROGRAM 256  -  TEST 011  ****                         04090256
C                                                                       04100256
C        TEST 011 CHECKS FOR THE PROPER EXECUTION OF THE STEPS AS SHOWN 04110256
C     IN SECTION 11.10.3 - EXECUTING A DO STATEMENT.  THE VARIABLE IVON004120256
C     SHOULD HAVE BEEN SET TO 100.  THE ITERATION COUNT IS ZERO BY THE  04130256
C     FORMULA IN 11.10.3(3). AS DESCRIBED IN SECTION 11.10.4 - THE      04140256
C     ITERATION COUNT IS TESTED.  IF IT IS NOT ZERO, EXECUTION OF THE   04150256
C     FIRST STATEMENT IN THE RANGE OF THE DO-LOOP BEGINS.  IF THE       04160256
C     ITERATION COUNT IS ZERO, THE DO-LOOP BECOMES INACTIVE.            04170256
C                                                                       04180256
C                                                                       04190256
      IVTNUM =  11                                                      04200256
      IF (ICZERO) 30110, 0110, 30110                                    04210256
 0110 CONTINUE                                                          04220256
      IVCOMP = 0                                                        04230256
      IVCOMP = IVON01                                                   04240256
      IVCORR = 100                                                      04250256
40110 IF ( IVCOMP - 100 )  20110, 10110, 20110                          04260256
30110 IVDELE = IVDELE + 1                                               04270256
      WRITE (I02,80000) IVTNUM                                          04280256
      IF (ICZERO) 10110, 0121, 20110                                    04290256
10110 IVPASS = IVPASS + 1                                               04300256
      WRITE (I02,80002) IVTNUM                                          04310256
      GO TO 0121                                                        04320256
20110 IVFAIL = IVFAIL + 1                                               04330256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04340256
 0121 CONTINUE                                                          04350256
C                                                                       04360256
C                                                                       04370256
C        THE FOLLOWING TWO TESTS ARE SIMILAR TO THE PREVIOUS TWO TESTS  04380256
C     IN THAT THE PARAMETERS OF THE DO STATEMENT MAKE THE ITERATION     04390256
C     COUNT ZERO WHEN THE DO STATEMENT IS EXECUTED.                     04400256
C                                                                       04410256
C                                                                       04420256
C                                                                       04430256
C     ****  FCVS PROGRAM 256  -  TEST 012  ****                         04440256
C                                                                       04450256
C        TEST 012 HAS M1 LESS THAN M2, BUT M3 IS NEGATIVE.  THE LOOP    04460256
C     SHOULD NOT BE EXECUTED AT ALL.                                    04470256
C                                                                       04480256
C                                                                       04490256
      IVTNUM =  12                                                      04500256
      IF (ICZERO) 30120, 0120, 30120                                    04510256
 0120 CONTINUE                                                          04520256
      IVCOMP = 0                                                        04530256
      DO 0122 IVON01 = 10, 100, -3                                      04540256
      IVCOMP = IVCOMP + 1                                               04550256
 0122 CONTINUE                                                          04560256
      IVCORR = 0                                                        04570256
40120 IF ( IVCOMP )  20120, 10120, 20120                                04580256
30120 IVDELE = IVDELE + 1                                               04590256
      WRITE (I02,80000) IVTNUM                                          04600256
      IF (ICZERO) 10120, 0131, 20120                                    04610256
10120 IVPASS = IVPASS + 1                                               04620256
      WRITE (I02,80002) IVTNUM                                          04630256
      GO TO 0131                                                        04640256
20120 IVFAIL = IVFAIL + 1                                               04650256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04660256
 0131 CONTINUE                                                          04670256
C                                                                       04680256
C     ****  FCVS PROGRAM 256  -  TEST 013  ****                         04690256
C                                                                       04700256
C        TEST 013 CHECKS THAT THE VALUE RETAINED FOR THE DO-VARIABLE    04710256
C     IN THE PREVIOUS TEST IS EQUAL TO THE INITIAL PARAMETER VALUE - M3.04720256
C                                                                       04730256
C                                                                       04740256
      IVTNUM =  13                                                      04750256
      IF (ICZERO) 30130, 0130, 30130                                    04760256
 0130 CONTINUE                                                          04770256
      IVCOMP = 0                                                        04780256
      IVCOMP = IVON01                                                   04790256
      IVCORR = 10                                                       04800256
40130 IF ( IVCOMP - 10 )  20130, 10130, 20130                           04810256
30130 IVDELE = IVDELE + 1                                               04820256
      WRITE (I02,80000) IVTNUM                                          04830256
      IF (ICZERO) 10130, 0141, 20130                                    04840256
10130 IVPASS = IVPASS + 1                                               04850256
      WRITE (I02,80002) IVTNUM                                          04860256
      GO TO 0141                                                        04870256
20130 IVFAIL = IVFAIL + 1                                               04880256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04890256
 0141 CONTINUE                                                          04900256
C                                                                       04910256
C     ****  FCVS PROGRAM 256  -  TEST 014  ****                         04920256
C                                                                       04930256
C        TEST 014 CHECKS FOR ONE EXECUTION OF THE RANGE OF A DO-LOOP    04940256
C     ACCORDING TO THE FORMULA SHOWN IN 11.10.3(3) WITH M1 = M2.        04950256
C                                                                       04960256
C        THE DO-LOOPS IN THIS TEST ARE A NEST OF THREE EACH WITH ITS    04970256
C     OWN TERMINAL STATEMENT.                                           04980256
C                                                                       04990256
C                                                                       05000256
      IVTNUM =  14                                                      05010256
      IF (ICZERO) 30140, 0140, 30140                                    05020256
 0140 CONTINUE                                                          05030256
      IVCOMP = 1                                                        05040256
      DO 0144 IVON01 = 1, 1, 1                                          05050256
           IVCOMP = IVCOMP * 2                                          05060256
           DO 0143 IVON02 = 10,10,10                                    05070256
                IVCOMP = IVCOMP * 3                                     05080256
                DO 0142, IVON03 = 100, 100, -2                          05090256
                     IVCOMP = IVCOMP * 5                                05100256
 0142           CONTINUE                                                05110256
 0143      CONTINUE                                                     05120256
 0144 CONTINUE                                                          05130256
C                                                                       05140256
C        **** IVCOMP IS DETERMINED BY IVCOMP = 30 = 1 * 2 * 3 * 5    ***05150256
C                                                                       05160256
      IVCORR = 30                                                       05170256
40140 IF ( IVCOMP - 30 )  20140, 10140, 20140                           05180256
30140 IVDELE = IVDELE + 1                                               05190256
      WRITE (I02,80000) IVTNUM                                          05200256
      IF (ICZERO) 10140, 0151, 20140                                    05210256
10140 IVPASS = IVPASS + 1                                               05220256
      WRITE (I02,80002) IVTNUM                                          05230256
      GO TO 0151                                                        05240256
20140 IVFAIL = IVFAIL + 1                                               05250256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05260256
 0151 CONTINUE                                                          05270256
C                                                                       05280256
C     ****  FCVS PROGRAM 256  -  TEST 015  ****                         05290256
C                                                                       05300256
C        TEST 015 IS A CHECK ON THE FIRST EXAMPLE SHOWN IN SECTION      05310256
C     11.10.7.  THIS IS A TEST OF INCREMENTATION PROCESSING OF TWO NEST 05320256
C     DO-LOOPS HAVING THE SAME TERMINAL STATEMENT.                      05330256
C                                                                       05340256
C        THIS IS A TEST OF A DO-LOOP THAT BECOMES ACTIVE INSIDE AN      05350256
C     ALREADY ACTIVE DO-LOOP.                                           05360256
C                                                                       05370256
C                                                                       05380256
      IVTNUM =  15                                                      05390256
      IF (ICZERO) 30150, 0150, 30150                                    05400256
 0150 CONTINUE                                                          05410256
      IVCOMP = 0                                                        05420256
      IVON01 = 0                                                        05430256
      DO 0152 IVON02 = 1, 10                                            05440256
      IVON03 = IVON02                                                   05450256
      DO 0152 IVON04 = 1, 5                                             05460256
      IVON05 = IVON04                                                   05470256
 0152 IVON01 = IVON01 + 1                                               05480256
 0153 CONTINUE                                                          05490256
      IVCOMP = IVON02                                                   05500256
C     THIS IS THE VALUE FOR I IN THE EXAMPLE.                           05510256
      IVCORR = 11                                                       05520256
40150 IF ( IVCOMP - 11 )  20150, 10150, 20150                           05530256
30150 IVDELE = IVDELE + 1                                               05540256
      WRITE (I02,80000) IVTNUM                                          05550256
      IF (ICZERO) 10150, 0161, 20150                                    05560256
10150 IVPASS = IVPASS + 1                                               05570256
      WRITE (I02,80002) IVTNUM                                          05580256
      GO TO 0161                                                        05590256
20150 IVFAIL = IVFAIL + 1                                               05600256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05610256
 0161 CONTINUE                                                          05620256
C                                                                       05630256
C     ****  FCVS PROGRAM 256  -  TEST 016  ****                         05640256
C                                                                       05650256
C        TEST 016 CHECKS THE VALUE OF J (IVON03) IN THE FIRST EXAMPLE.  05660256
C                                                                       05670256
C                                                                       05680256
      IVTNUM =  16                                                      05690256
      IF (ICZERO) 30160, 0160, 30160                                    05700256
 0160 CONTINUE                                                          05710256
      IVCOMP = 0                                                        05720256
      IVCOMP = IVON03                                                   05730256
      IVCORR = 10                                                       05740256
40160 IF ( IVCOMP - 10 )  20160, 10160, 20160                           05750256
30160 IVDELE = IVDELE + 1                                               05760256
      WRITE (I02,80000) IVTNUM                                          05770256
      IF (ICZERO) 10160, 0171, 20160                                    05780256
10160 IVPASS = IVPASS + 1                                               05790256
      WRITE (I02,80002) IVTNUM                                          05800256
      GO TO 0171                                                        05810256
20160 IVFAIL = IVFAIL + 1                                               05820256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05830256
 0171 CONTINUE                                                          05840256
C                                                                       05850256
C     ****  FCVS PROGRAM 256  -  TEST 017  ****                         05860256
C                                                                       05870256
C        TEST 017 CHECKS THE VALUE OF K (IVON04) IN THE FIRST EXAMPLE.  05880256
C                                                                       05890256
C                                                                       05900256
      IVTNUM =  17                                                      05910256
      IF (ICZERO) 30170, 0170, 30170                                    05920256
 0170 CONTINUE                                                          05930256
      IVCOMP = 0                                                        05940256
      IVCOMP = IVON04                                                   05950256
      IVCORR = 6                                                        05960256
40170 IF ( IVCOMP - 6  )  20170, 10170, 20170                           05970256
30170 IVDELE = IVDELE + 1                                               05980256
      WRITE (I02,80000) IVTNUM                                          05990256
      IF (ICZERO) 10170, 0181, 20170                                    06000256
10170 IVPASS = IVPASS + 1                                               06010256
      WRITE (I02,80002) IVTNUM                                          06020256
      GO TO 0181                                                        06030256
20170 IVFAIL = IVFAIL + 1                                               06040256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06050256
 0181 CONTINUE                                                          06060256
C                                                                       06070256
C     ****  FCVS PROGRAM 256  -  TEST 018  ****                         06080256
C                                                                       06090256
C        TEST 018 CHECKS THE VALUE OF L (IVON05) IN THE FIRST EXAMPLE.  06100256
C                                                                       06110256
C                                                                       06120256
      IVTNUM =  18                                                      06130256
      IF (ICZERO) 30180, 0180, 30180                                    06140256
 0180 CONTINUE                                                          06150256
      IVCOMP = 0                                                        06160256
      IVCOMP = IVON05                                                   06170256
      IVCORR = 5                                                        06180256
40180 IF ( IVCOMP - 5  )  20180, 10180, 20180                           06190256
30180 IVDELE = IVDELE + 1                                               06200256
      WRITE (I02,80000) IVTNUM                                          06210256
      IF (ICZERO) 10180, 0191, 20180                                    06220256
10180 IVPASS = IVPASS + 1                                               06230256
      WRITE (I02,80002) IVTNUM                                          06240256
      GO TO 0191                                                        06250256
20180 IVFAIL = IVFAIL + 1                                               06260256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06270256
 0191 CONTINUE                                                          06280256
C                                                                       06290256
C     ****  FCVS PROGRAM 256  -  TEST 019  ****                         06300256
C                                                                       06310256
C        TEST 019 CHECKS THE VALUE OF N (IVON01) IN THE FIRST EXAMPLE.  06320256
C                                                                       06330256
C                                                                       06340256
      IVTNUM =  19                                                      06350256
      IF (ICZERO) 30190, 0190, 30190                                    06360256
 0190 CONTINUE                                                          06370256
      IVCOMP = 0                                                        06380256
      IVCOMP = IVON01                                                   06390256
      IVCORR = 50                                                       06400256
40190 IF ( IVCOMP - 50 )  20190, 10190, 20190                           06410256
30190 IVDELE = IVDELE + 1                                               06420256
      WRITE (I02,80000) IVTNUM                                          06430256
      IF (ICZERO) 10190, 0201, 20190                                    06440256
10190 IVPASS = IVPASS + 1                                               06450256
      WRITE (I02,80002) IVTNUM                                          06460256
      GO TO 0201                                                        06470256
20190 IVFAIL = IVFAIL + 1                                               06480256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06490256
 0201 CONTINUE                                                          06500256
C                                                                       06510256
C     ****  FCVS PROGRAM 256  -  TEST 020  ****                         06520256
C                                                                       06530256
C        TEST 020 IS A CHECK ON THE SECOND EXAMPLE IN SECTION 11.10.7.  06540256
C     IN THIS EXAMPLE, THE INNER DO-LOOP BECOMES ACTIVE AND THEN        06550256
C     IMMEDIATELY INACTIVE INSIDE AN ALREADY ACTIVE OUTER DO-LOOP.      06560256
C                                                                       06570256
C        ALTHOUGH IN SOME WAYS SIMILAR TO THE FIRST EXAMPLE, THE SECOND 06580256
C     EXAMPLE SHOULD HAVE DIFFERENT FINAL VALUES ON THE INTEGER COUNTERS06590256
C     AND THE VALUE OF L (IVON10) WILL NOT BE TESTED BECAUSE IT IS NOT  06600256
C     DEFINED DURING THE RANGE OF THE DO-LOOP INVOLVED.                 06610256
C                                                                       06620256
C                                                                       06630256
      IVTNUM =  20                                                      06640256
      IF (ICZERO) 30200, 0200, 30200                                    06650256
 0200 CONTINUE                                                          06660256
      IVCOMP = 0                                                        06670256
      IVON06 = 0                                                        06680256
      DO 0202 IVON07 = 1, 10                                            06690256
      IVON08 = IVON07                                                   06700256
      DO 0202 IVON09 = 5, 1                                             06710256
      IVON10 = IVON09                                                   06720256
 0202 IVON06 = IVON06 + 1                                               06730256
 0203 CONTINUE                                                          06740256
      IVCOMP = IVON07                                                   06750256
C     THIS IS THE VALUE FOR I IN THE SECOND EXAMPLE.                    06760256
      IVCORR = 11                                                       06770256
40200 IF ( IVCOMP - 11 )  20200, 10200, 20200                           06780256
30200 IVDELE = IVDELE + 1                                               06790256
      WRITE (I02,80000) IVTNUM                                          06800256
      IF (ICZERO) 10200, 0211, 20200                                    06810256
10200 IVPASS = IVPASS + 1                                               06820256
      WRITE (I02,80002) IVTNUM                                          06830256
      GO TO 0211                                                        06840256
20200 IVFAIL = IVFAIL + 1                                               06850256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06860256
 0211 CONTINUE                                                          06870256
C                                                                       06880256
C     ****  FCVS PROGRAM 256  -  TEST 021  ****                         06890256
C                                                                       06900256
C        TEST 021 CHECKS THE VALUE OF J (IVON08) IN THE SECOND EXAMPLE. 06910256
C                                                                       06920256
C                                                                       06930256
      IVTNUM =  21                                                      06940256
      IF (ICZERO) 30210, 0210, 30210                                    06950256
 0210 CONTINUE                                                          06960256
      IVCOMP = 0                                                        06970256
      IVCOMP = IVON08                                                   06980256
      IVCORR = 10                                                       06990256
40210 IF ( IVCOMP - 10 )  20210, 10210, 20210                           07000256
30210 IVDELE = IVDELE + 1                                               07010256
      WRITE (I02,80000) IVTNUM                                          07020256
      IF (ICZERO) 10210, 0221, 20210                                    07030256
10210 IVPASS = IVPASS + 1                                               07040256
      WRITE (I02,80002) IVTNUM                                          07050256
      GO TO 0221                                                        07060256
20210 IVFAIL = IVFAIL + 1                                               07070256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07080256
 0221 CONTINUE                                                          07090256
C                                                                       07100256
C     ****  FCVS PROGRAM 256  -  TEST 022  ****                         07110256
C                                                                       07120256
C        TEST 022 CHECKS THE VALUE OF K (IVON09) IN THE SECOND EXAMPLE. 07130256
C                                                                       07140256
C                                                                       07150256
      IVTNUM =  22                                                      07160256
      IF (ICZERO) 30220, 0220, 30220                                    07170256
 0220 CONTINUE                                                          07180256
      IVCOMP = 0                                                        07190256
      IVCOMP = IVON09                                                   07200256
      IVCORR = 5                                                        07210256
40220 IF ( IVCOMP - 5  )  20220, 10220, 20220                           07220256
30220 IVDELE = IVDELE + 1                                               07230256
      WRITE (I02,80000) IVTNUM                                          07240256
      IF (ICZERO) 10220, 0231, 20220                                    07250256
10220 IVPASS = IVPASS + 1                                               07260256
      WRITE (I02,80002) IVTNUM                                          07270256
      GO TO 0231                                                        07280256
20220 IVFAIL = IVFAIL + 1                                               07290256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07300256
 0231 CONTINUE                                                          07310256
C                                                                       07320256
C     ****  FCVS PROGRAM 256  -  TEST 023  ****                         07330256
C                                                                       07340256
C        TEST 023 CHECKS THE VALUE OF N (IVON06) IN THE SECOND EXAMPLE. 07350256
C                                                                       07360256
C                                                                       07370256
      IVTNUM =  23                                                      07380256
      IF (ICZERO) 30230, 0230, 30230                                    07390256
 0230 CONTINUE                                                          07400256
      IVCOMP = 0                                                        07410256
      IVCOMP = IVON06                                                   07420256
      IVCORR = 0                                                        07430256
40230 IF ( IVCOMP - 0  )  20230, 10230, 20230                           07440256
30230 IVDELE = IVDELE + 1                                               07450256
      WRITE (I02,80000) IVTNUM                                          07460256
      IF (ICZERO) 10230, 0241, 20230                                    07470256
10230 IVPASS = IVPASS + 1                                               07480256
      WRITE (I02,80002) IVTNUM                                          07490256
      GO TO 0241                                                        07500256
20230 IVFAIL = IVFAIL + 1                                               07510256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07520256
 0241 CONTINUE                                                          07530256
C                                                                       07540256
C     ****  FCVS PROGRAM 256  -  TEST 024  ****                         07550256
C                                                                       07560256
C        TEST 024 IS A CHECK ON USING A LOGICAL IF STATEMENT AS THE     07570256
C     TERMINAL STATEMENT IN THE RANGE OF A DO-LOOP.  THE LOGICAL IF     07580256
C     STATEMENT HAS AN UNCONDITIONAL GO TO STATEMENT AS ITS EXECUTABLE  07590256
C     STATEMENT  AS ALLOWED IN SECTION 11.10.                           07600256
C                                                                       07610256
C                                                                       07620256
      IVTNUM =  24                                                      07630256
      IF (ICZERO) 30240, 0240, 30240                                    07640256
 0240 CONTINUE                                                          07650256
      IVCOMP = 0                                                        07660256
      DO 0242 IVON01 = 1, 10                                            07670256
      IVCOMP = IVCOMP + 1                                               07680256
 0242 IF ( IVON01 .GE. 5 )  GO TO 0243                                  07690256
C                                                                       07700256
C                                                                       07710256
C     IF THE LOGIC DOES NOT BRANCH OUT OF THE RANGE OF THE DO-LOOP WHEN 07720256
C     THE DO-VARIABLE (IVON01) IS EQUAL TO FIVE (5), THEN IVCOMP WILL BE07730256
C     SET BACK TO THE VALUE OF ZERO.                                    07740256
C                                                                       07750256
      IVCOMP = 0                                                        07760256
C                                                                       07770256
C                                                                       07780256
 0243 IVCORR = 5                                                        07790256
40240 IF ( IVCOMP - 5 )  20240, 10240, 20240                            07800256
30240 IVDELE = IVDELE + 1                                               07810256
      WRITE (I02,80000) IVTNUM                                          07820256
      IF (ICZERO) 10240, 0251, 20240                                    07830256
10240 IVPASS = IVPASS + 1                                               07840256
      WRITE (I02,80002) IVTNUM                                          07850256
      GO TO 0251                                                        07860256
20240 IVFAIL = IVFAIL + 1                                               07870256
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07880256
 0251 CONTINUE                                                          07890256
C                                                                       07900256
C                                                                       07910256
C     WRITE OUT TEST SUMMARY                                            07920256
C                                                                       07930256
      WRITE (I02,90004)                                                 07940256
      WRITE (I02,90014)                                                 07950256
      WRITE (I02,90004)                                                 07960256
      WRITE (I02,90000)                                                 07970256
      WRITE (I02,90004)                                                 07980256
      WRITE (I02,90020) IVFAIL                                          07990256
      WRITE (I02,90022) IVPASS                                          08000256
      WRITE (I02,90024) IVDELE                                          08010256
      STOP                                                              08020256
90001 FORMAT (1H ,24X,5HFM256)                                          08030256
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM256)                          08040256
C                                                                       08050256
C     FORMATS FOR TEST DETAIL LINES                                     08060256
C                                                                       08070256
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   08080256
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08090256
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08100256
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08110256
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        08120256
C                                                                       08130256
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08140256
C                                                                       08150256
90002 FORMAT (1H1)                                                      08160256
90004 FORMAT (1H )                                                      08170256
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08180256
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   08190256
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         08200256
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  08210256
90014 FORMAT (1H ,5X,46H----------------------------------------------) 08220256
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08230256
C                                                                       08240256
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 08250256
C                                                                       08260256
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              08270256
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              08280256
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             08290256
      END                                                               08300256
