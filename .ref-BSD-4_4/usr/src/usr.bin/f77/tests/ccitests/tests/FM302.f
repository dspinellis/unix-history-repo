      PROGRAM FM302                                                     00010302
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020302
C                                                                       00030302
C        THIS ROUTINE TESTS THE SUBSET LEVEL FEATURES OF THE COMMON     00040302
C     SPECIFICATION STATEMENT.  INTEGER, REAL AND LOGICAL VARIABLES AND 00050302
C     ARRAYS ARE PASSED BACK-AND-FORTH BETWEEN THE MAIN PROGRAM,EXTERNAL00060302
C     FUNCTIONS AND SUBROUTINES.  BOTH NAMED AND UNNAMED (BLANK) COMMON 00070302
C     ARE TESTED.  SPECIFIC TESTS ARE INCLUDED FOR RENAMING ENTITIES IN 00080302
C     COMMON BETWEEN PROGRAM UNITS, THE PASSING OF DATA THROUGH COMMON  00090302
C     BY EQUIVALENCE ASSOCIATION, AND THE SPECIFYING OF BLANK COMMON OF 00100302
C     DIFFERENT LENGTHS IN DIFFERENT PROGRAM UNITS.  THE SUBSET LEVEL   00110302
C     FEATURES OF THE COMMON STATEMENT ARE ALSO TESTED IN FM022 THROUGH 00120302
C     FM025, FM050 AND FM056.                                           00130302
C                                                                       00140302
C     REFERENCES.                                                       00150302
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00160302
C           X3.9-1978                                                   00170302
C                                                                       00180302
C        SECTION 8.2,    EQUIVALENCE STATEMENT                          00190302
C        SECTION 8.3,    COMMON STATEMENT                               00200302
C        SECTION 15.5,   EXTERNAL FUNCTIONS                             00210302
C        SECTION 15.6,   SUBROUTINES                                    00220302
C        SECTION 15.9.4, COMMON BLOCKS                                  00230302
C                                                                       00240302
C                                                                       00250302
C     ******************************************************************00260302
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00270302
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00280302
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00290302
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00300302
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00310302
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00320302
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00330302
C     THE RESULT OF EXECUTING THESE TESTS.                              00340302
C                                                                       00350302
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00360302
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00370302
C                                                                       00380302
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00390302
C                    DEPARTMENT OF THE NAVY                             00400302
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00410302
C                    WASHINGTON, D.C.   20376                           00420302
C                                                                       00430302
C     ******************************************************************00440302
C                                                                       00450302
C                                                                       00460302
      IMPLICIT LOGICAL (L)                                              00470302
      IMPLICIT CHARACTER*14 (C)                                         00480302
C                                                                       00490302
                                                                        00500302
C                                                                       00510302
C          *** SPECIFICATION STATEMENT FOR TEST 001 ***                 00520302
C                                                                       00530302
      COMMON IVCN01                                                     00540302
C                                                                       00550302
C          *** SPECIFICATION STATEMENT FOR TEST 002 ***                 00560302
C                                                                       00570302
      COMMON //IVCN02,LVCN01                                            00580302
C                                                                       00590302
C          *** SPECIFICATION STATEMENT FOR TEST 003 ***                 00600302
C                                                                       00610302
      COMMON RVCN01//IVCN03                                             00620302
C                                                                       00630302
C          *** SPECIFICATION STATEMENT FOR TEST 004 ***                 00640302
C                                                                       00650302
      COMMON IVCN04, IVCN05,  // IACN11(4)                              00660302
C                                                                       00670302
C          *** SPECIFICATION STATEMENT FOR TEST 005 ***                 00680302
C                                                                       00690302
      COMMON /BLK1/ IVCNA1                                              00700302
C                                                                       00710302
C          *** SPECIFICATION STATEMENT FOR TEST 006 ***                 00720302
C                                                                       00730302
      COMMON /BLK2/IVCNB1,RVCNB1, /BLK2/IVCNB2                          00740302
C                                                                       00750302
C          *** SPECIFICATION STATEMENT FOR TEST 007 ***                 00760302
C                                                                       00770302
      DIMENSION RACN11(10)                                              00780302
      COMMON /BLK3/LVCNC1,IVCNC1/BLK4/RACN11,IACN21(2,3)                00790302
C                                                                       00800302
C          *** SPECIFICATION STATEMENT FOR TEST 008 ***                 00810302
C                                                                       00820302
      COMMON /BLK5/IVCND1, IVCND2                                       00830302
C                                                                       00840302
C          *** SPECIFICATION STATEMENT FOR TEST 009 ***                 00850302
C                                                                       00860302
      COMMON IVCN06/BLK5/RVCND1,LVCND1//IVCN07,IVCN08/BLK6/RVCNE1       00870302
C                                                                       00880302
C          *** SPECIFICATION STATEMENT FOR TEST 010 ***                 00890302
C                                                                       00900302
      DIMENSION IACN1F(3)                                               00910302
      COMMON /BLK7/IVCNF1,IVCNF2,IVCNF3,IACN1F                          00920302
C                                                                       00930302
C          *** SPECIFICATION STATEMENT FOR TEST 011 ***                 00940302
C                                                                       00950302
      EQUIVALENCE (IVCEH1,IVCEH2)                                       00960302
      COMMON /BLK8/IVCEH1                                               00970302
C                                                                       00980302
C          *** SPECIFICATION STATEMENT FOR TEST 012                     00990302
      EQUIVALENCE (IVCE09,IVCE10)                                       01000302
      COMMON IVCE09                                                     01010302
C                                                                       01020302
C          *** SPECIFICATION STATEMENT FOR TEST 013                     01030302
C                                                                       01040302
      EQUIVALENCE (IVCEI1,IACE1I)                                       01050302
      DIMENSION IACE1I(3)                                               01060302
      COMMON /BLK9/IVCEI1                                               01070302
C                                                                       01080302
C          *** SPECIFICATION STATEMENT FOR TEST 014 ***                 01090302
C                                                                       01100302
      COMMON IVCN12                                                     01110302
C                                                                       01120302
C          *** SPECIFICATION STATEMENT FOR TEST 015 ***                 01130302
C                                                                       01140302
      COMMON /BLK10/IVCNJ1                                              01150302
C                                                                       01160302
C          *** SPECIFICATION STATEMENT FOR TEST 016 ***                 01170302
C                                                                       01180302
      COMMON /BLKCHR/CVTN01,CVTN02,CATN11                               01190302
      CHARACTER CVTN01*2, CVTN02*3, CATN11(3)*5                         01200302
      INTEGER FF304                                                     01210302
C                                                                       01220302
C                                                                       01230302
C                                                                       01240302
C     INITIALIZATION SECTION.                                           01250302
C                                                                       01260302
C     INITIALIZE CONSTANTS                                              01270302
C     ********************                                              01280302
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01290302
      I01 = 5                                                           01300302
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01310302
      I02 = 6                                                           01320302
C     SYSTEM ENVIRONMENT SECTION                                        01330302
C                                                                       01340302
      I01 = 5                                                           01350302
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01360302
C     (UNIT NUMBER FOR CARD READER).                                    01370302
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01380302
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01390302
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01400302
C                                                                       01410302
      I02 = 6                                                           01420302
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01430302
C     (UNIT NUMBER FOR PRINTER).                                        01440302
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01450302
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01460302
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01470302
C                                                                       01480302
      IVPASS = 0                                                        01490302
      IVFAIL = 0                                                        01500302
      IVDELE = 0                                                        01510302
      ICZERO = 0                                                        01520302
C                                                                       01530302
C     WRITE OUT PAGE HEADERS                                            01540302
C                                                                       01550302
      WRITE (I02,90002)                                                 01560302
      WRITE (I02,90006)                                                 01570302
      WRITE (I02,90008)                                                 01580302
      WRITE (I02,90004)                                                 01590302
      WRITE (I02,90010)                                                 01600302
      WRITE (I02,90004)                                                 01610302
      WRITE (I02,90016)                                                 01620302
      WRITE (I02,90001)                                                 01630302
      WRITE (I02,90004)                                                 01640302
      WRITE (I02,90012)                                                 01650302
      WRITE (I02,90014)                                                 01660302
      WRITE (I02,90004)                                                 01670302
C                                                                       01680302
C                                                                       01690302
C          THE FOLLOWING ASSIGNMENT STATEMENTS INITIALIZE THE DATA      01700302
C     ENTITIES BEING PASSED THROUGH COMMON TO SUBROUTINE FS303.  ONLY   01710302
C     ONE REFERENCE TO THIS SUBPROGRAM IS MADE FROM THIS PROGRAM.  THE  01720302
C     CONTENTS OF THE DATA ENTITIES BEING RETURNED THROUGH COMMON ARE   01730302
C     THEN CHECKED IN THIS PROGRAM.                                     01740302
C                                                                       01750302
C                                                                       01760302
      IVCN01 = 3                                                        01770302
      IVCN02 = 2                                                        01780302
      LVCN01 = .FALSE.                                                  01790302
      IVCNA1 = 25                                                       01800302
      IVCNB1 = 3                                                        01810302
      RVCNB1 = 4.0                                                      01820302
      IVCNB2 = 5                                                        01830302
      LVCNC1 = .TRUE.                                                   01840302
      IVCNC1 = 13                                                       01850302
      RACN11(1) = 1.                                                    01860302
      RACN11(10) = 10.0                                                 01870302
      IACN21(1,1) = 11                                                  01880302
      IACN21(2,3) = 23                                                  01890302
      IVCNF1 = 41                                                       01900302
      IVCNF3 = 43                                                       01910302
      IACN1F(1) = 141                                                   01920302
      IACN1F(2) = 142                                                   01930302
      IVCEH1 = 1                                                        01940302
      IVCEH2 = 5                                                        01950302
      CVTN01 = 'AB'                                                     01960302
      CVTN02 = 'CDE'                                                    01970302
      CATN11(1) = 'FGHIJ'                                               01980302
      CATN11(2) = 'KLMNO'                                               01990302
      CATN11(3) = 'PQRST'                                               02000302
      CALL FS303                                                        02010302
C                                                                       02020302
C          THE FOLLOWING ASSIGNMENT STATEMENTS INITIALIZE THE DATA      02030302
C     ENTITIES BEING PASSED THROUGH COMMON TO EXTERNAL FUNCTION FF304.  02040302
C     ONLY ONE REFERENCE TO THIS SUBPROGRAM IS MADE FROM THIS PROGRAM.  02050302
C     THE CONTENTS OF THE DATA ENTITIES BEING RETURNED THROUGH COMMON   02060302
C     ARE THEN CHECKED IN THIS PROGRAM.                                 02070302
C                                                                       02080302
      RVCN01 = 6.4                                                      02090302
      IVCN03 = 11                                                       02100302
      IVCN03 = IVCN03*2                                                 02110302
      IVCN04 = 16                                                       02120302
      IVCN05 = 16                                                       02130302
      IACN11(1) = 1                                                     02140302
      IACN11(2) = 2                                                     02150302
      IACN11(3) = 3                                                     02160302
      IACN11(4) = 4                                                     02170302
      IVCND1 = +33                                                      02180302
      IVCND2 = 10                                                       02190302
      IVCN06 = 6                                                        02200302
      IVCN07 = 7                                                        02210302
      IVCN08 = 8                                                        02220302
      RVCND1 = 1.3                                                      02230302
      LVCND1 = .FALSE.                                                  02240302
      RVCNE1 = +3.5                                                     02250302
      IVCE09 = 9                                                        02260302
      IVCE10 = 10                                                       02270302
      IVCEI1 = 5                                                        02280302
      IACE1I(1) = 10                                                    02290302
      IACE1I(2) = 15                                                    02300302
      IACE1I(3) = 20                                                    02310302
      IVCNJ1 = 1                                                        02320302
      IVON99 = FF304 ( )                                                02330302
C                                                                       02340302
C          TESTS 001 THROUGH 009 ARE DESIGNED TO TEST VARIOUS           02350302
C     SYNTACTICAL CONSTRUCTS OF THE COMMON STATEMENT USING NAMED AND    02360302
C     UNNAMED (BLANK) COMMON IN THE MAIN PROGRAM, A SUBROUTINE AND AN   02370302
C     EXTERNAL FUNCTION.  DATA ENTITIES CONSIST OF INTEGER, REAL AND    02380302
C     LOGICAL VARIABLES AND INTEGER AND REAL ARRAYS.                    02390302
C                                                                       02400302
C     ****  FCVS PROGRAM 302  -  TEST 001  ****                         02410302
C                                                                       02420302
C          TESTS 001 AND 002 TEST THE USE OF UNNAMED COMMON IN A MAIN   02430302
C     PROGRAM AND A SUBROUTINE.                                         02440302
C                                                                       02450302
      IVTNUM =   1                                                      02460302
      IF (ICZERO) 30010, 0010, 30010                                    02470302
 0010 CONTINUE                                                          02480302
      IVCOMP = 0                                                        02490302
      IVCOMP = IVCN01                                                   02500302
      IVCORR = 4                                                        02510302
40010 IF (IVCOMP - 4) 20010, 10010, 20010                               02520302
30010 IVDELE = IVDELE + 1                                               02530302
      WRITE (I02,80000) IVTNUM                                          02540302
      IF (ICZERO) 10010, 0021, 20010                                    02550302
10010 IVPASS = IVPASS + 1                                               02560302
      WRITE (I02,80002) IVTNUM                                          02570302
      GO TO 0021                                                        02580302
20010 IVFAIL = IVFAIL + 1                                               02590302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02600302
 0021 CONTINUE                                                          02610302
C                                                                       02620302
C     ****  FCVS PROGRAM 302  -  TEST 002  ****                         02630302
C                                                                       02640302
C                                                                       02650302
      IVTNUM =   2                                                      02660302
      IF (ICZERO) 30020, 0020, 30020                                    02670302
 0020 CONTINUE                                                          02680302
      IVCOMP = 1                                                        02690302
      IF (IVCN02 .EQ. 7) IVCOMP = IVCOMP * 2                            02700302
      IF (LVCN01) IVCOMP = IVCOMP * 3                                   02710302
      IVCORR = 6                                                        02720302
C          6 = 2 * 3                                                    02730302
40020 IF (IVCOMP - 6) 20020, 10020, 20020                               02740302
30020 IVDELE = IVDELE + 1                                               02750302
      WRITE (I02,80000) IVTNUM                                          02760302
      IF (ICZERO) 10020, 0031, 20020                                    02770302
10020 IVPASS = IVPASS + 1                                               02780302
      WRITE (I02,80002) IVTNUM                                          02790302
      GO TO 0031                                                        02800302
20020 IVFAIL = IVFAIL + 1                                               02810302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02820302
 0031 CONTINUE                                                          02830302
C                                                                       02840302
C     ****  FCVS PROGRAM 302  -  TEST 003  ****                         02850302
C                                                                       02860302
C          TESTS 003 AND 004 TEST THE USE OF UNNAMED COMMON IN A MAIN   02870302
C     PROGRAM AND AN EXTERNAL FUNCTION.                                 02880302
C                                                                       02890302
      IVTNUM =   3                                                      02900302
      IF (ICZERO) 30030, 0030, 30030                                    02910302
 0030 CONTINUE                                                          02920302
      IVCOMP = 1                                                        02930302
      IF (RVCN01 .GE. 4.1995 .AND. RVCN01 .LE. 4.2005) IVCOMP=IVCOMP*2  02940302
      IF (IVCN03 .EQ.  23) IVCOMP = IVCOMP * 3                          02950302
      IVCORR = 6                                                        02960302
C          6 = 2 * 3                                                    02970302
40030 IF (IVCOMP - 6) 20030, 10030, 20030                               02980302
30030 IVDELE = IVDELE + 1                                               02990302
      WRITE (I02,80000) IVTNUM                                          03000302
      IF (ICZERO) 10030, 0041, 20030                                    03010302
10030 IVPASS = IVPASS + 1                                               03020302
      WRITE (I02,80002) IVTNUM                                          03030302
      GO TO 0041                                                        03040302
20030 IVFAIL = IVFAIL + 1                                               03050302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03060302
 0041 CONTINUE                                                          03070302
C                                                                       03080302
C     ****  FCVS PROGRAM 302  -  TEST 004  ****                         03090302
C                                                                       03100302
C                                                                       03110302
      IVTNUM =   4                                                      03120302
      IF (ICZERO) 30040, 0040, 30040                                    03130302
 0040 CONTINUE                                                          03140302
      IVCOMP = 1                                                        03150302
      IF (IVCN04 .EQ. 8) IVCOMP = IVCOMP * 2                            03160302
      IF (IVCN05 .EQ. 16) IVCOMP = IVCOMP * 3                           03170302
      IF (IACN11(1) .EQ. 5) IVCOMP = IVCOMP * 5                         03180302
      IF (IACN11(2) .EQ. 5) IVCOMP = IVCOMP * 7                         03190302
      IF (IACN11(3) .EQ. 5) IVCOMP = IVCOMP * 11                        03200302
      IF (IACN11(4) .EQ. 5) IVCOMP = IVCOMP * 13                        03210302
      IVCORR = 30030                                                    03220302
C     30030  = 2 * 3 * 5 * 7 * 11 * 13                                  03230302
40040 IF (IVCOMP - 30030) 20040, 10040, 20040                           03240302
30040 IVDELE = IVDELE + 1                                               03250302
      WRITE (I02,80000) IVTNUM                                          03260302
      IF (ICZERO) 10040, 0051, 20040                                    03270302
10040 IVPASS = IVPASS + 1                                               03280302
      WRITE (I02,80002) IVTNUM                                          03290302
      GO TO 0051                                                        03300302
20040 IVFAIL = IVFAIL + 1                                               03310302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03320302
 0051 CONTINUE                                                          03330302
C                                                                       03340302
C     ****  FCVS PROGRAM 302  -  TEST 005  ****                         03350302
C                                                                       03360302
C          TESTS 005 THROUGH 007 TEST THE USE OF NAMED COMMON BLOCKS    03370302
C     IN A MAIN PROGRAM AND A SUBROUTINE.                               03380302
C                                                                       03390302
      IVTNUM =   5                                                      03400302
      IF (ICZERO) 30050, 0050, 30050                                    03410302
 0050 CONTINUE                                                          03420302
      IVCOMP = 0                                                        03430302
      IVCOMP = IVCNA1                                                   03440302
      IVCORR = 5                                                        03450302
40050 IF (IVCOMP - 5) 20050, 10050, 20050                               03460302
30050 IVDELE = IVDELE + 1                                               03470302
      WRITE (I02,80000) IVTNUM                                          03480302
      IF (ICZERO) 10050, 0061, 20050                                    03490302
10050 IVPASS = IVPASS + 1                                               03500302
      WRITE (I02,80002) IVTNUM                                          03510302
      GO TO 0061                                                        03520302
20050 IVFAIL = IVFAIL + 1                                               03530302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03540302
 0061 CONTINUE                                                          03550302
C                                                                       03560302
C     ****  FCVS PROGRAM 302  -  TEST 006  ****                         03570302
C                                                                       03580302
C                                                                       03590302
      IVTNUM =   6                                                      03600302
      IF (ICZERO) 30060, 0060, 30060                                    03610302
 0060 CONTINUE                                                          03620302
      IVCOMP = 1                                                        03630302
      IF (IVCNB1 .EQ. 8) IVCOMP = IVCOMP * 2                            03640302
      IF (RVCNB1 .GE. 3.4995 .AND. RVCNB1 .LE. 3.5005) IVCOMP=IVCOMP*3  03650302
      IF (IVCNB2 .EQ. 5) IVCOMP = IVCOMP * 5                            03660302
      IVCORR = 30                                                       03670302
C         30 = 2 * 3 * 5                                                03680302
40060 IF (IVCOMP - 30) 20060, 10060, 20060                              03690302
30060 IVDELE = IVDELE + 1                                               03700302
      WRITE (I02,80000) IVTNUM                                          03710302
      IF (ICZERO) 10060, 0071, 20060                                    03720302
10060 IVPASS = IVPASS + 1                                               03730302
      WRITE (I02,80002) IVTNUM                                          03740302
      GO TO 0071                                                        03750302
20060 IVFAIL = IVFAIL + 1                                               03760302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03770302
 0071 CONTINUE                                                          03780302
C                                                                       03790302
C     ****  FCVS PROGRAM 302  -  TEST 007  ****                         03800302
C                                                                       03810302
C                                                                       03820302
      IVTNUM =   7                                                      03830302
      IF (ICZERO) 30070, 0070, 30070                                    03840302
 0070 CONTINUE                                                          03850302
      IVCOMP = 1                                                        03860302
      IF (.NOT. LVCNC1) IVCOMP = IVCOMP * 2                             03870302
      IF (IVCNC1 .EQ. 12) IVCOMP = IVCOMP * 3                           03880302
      IF (RACN11(1).GE.110.95 .AND. RACN11(1).LE.111.05) IVCOMP=IVCOMP*503890302
      IF (RACN11(10).GE.109.95.AND.RACN11(10).LE.110.05)IVCOMP=IVCOMP*7 03900302
      IF (IACN21(1,1) .EQ. 12) IVCOMP = IVCOMP * 11                     03910302
      IF (IACN21 (2,3) .EQ. 24) IVCOMP = IVCOMP * 13                    03920302
      IVCORR = 30030                                                    03930302
C     30030  = 2* 3 * 5 * 7 * 11 * 13                                   03940302
40070 IF (IVCOMP - 30030) 20070, 10070, 20070                           03950302
30070 IVDELE = IVDELE + 1                                               03960302
      WRITE (I02,80000) IVTNUM                                          03970302
      IF (ICZERO) 10070, 0081, 20070                                    03980302
10070 IVPASS = IVPASS + 1                                               03990302
      WRITE (I02,80002) IVTNUM                                          04000302
      GO TO 0081                                                        04010302
20070 IVFAIL = IVFAIL + 1                                               04020302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04030302
 0081 CONTINUE                                                          04040302
C                                                                       04050302
C     ****  FCVS PROGRAM 302  -  TEST 008  ****                         04060302
C                                                                       04070302
C          TESTS 008 AND 009 TEST THE USE OF NAMED COMMON BLOCKS IN A   04080302
C     MAIN PROGRAM AND AN EXTERNAL FUNCTION.                            04090302
C                                                                       04100302
      IVTNUM =   8                                                      04110302
      IF (ICZERO) 30080, 0080, 30080                                    04120302
 0080 CONTINUE                                                          04130302
      IVCOMP = 1                                                        04140302
      IF (IVCND1 .EQ. 34) IVCOMP = IVCOMP * 2                           04150302
      IF (IVCND2 .EQ. 11) IVCOMP = IVCOMP * 3                           04160302
      IVCORR = 6                                                        04170302
C          6 = 2 * 3                                                    04180302
40080 IF (IVCOMP - 6) 20080, 10080, 20080                               04190302
30080 IVDELE = IVDELE + 1                                               04200302
      WRITE (I02,80000) IVTNUM                                          04210302
      IF (ICZERO) 10080, 0091, 20080                                    04220302
10080 IVPASS = IVPASS + 1                                               04230302
      WRITE (I02,80002) IVTNUM                                          04240302
      GO TO 0091                                                        04250302
20080 IVFAIL = IVFAIL + 1                                               04260302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04270302
 0091 CONTINUE                                                          04280302
C                                                                       04290302
C     ****  FCVS PROGRAM 302  -  TEST 009  ****                         04300302
C                                                                       04310302
C                                                                       04320302
      IVTNUM =   9                                                      04330302
      IF (ICZERO) 30090, 0090, 30090                                    04340302
 0090 CONTINUE                                                          04350302
      IVCOMP = 1                                                        04360302
      IF (IVCN06 .EQ. 7) IVCOMP = IVCOMP * 2                            04370302
      IF (RVCND1 .GE. 4.4995 .AND. RVCND1 .LE. 4.5005) IVCOMP = IVCOMP*304380302
      IF (LVCND1) IVCOMP = IVCOMP * 5                                   04390302
      IF (IVCN07 .EQ. -7) IVCOMP = IVCOMP * 7                           04400302
      IF (IVCN08 .EQ. -3) IVCOMP = IVCOMP * 11                          04410302
      IF (RVCNE1.GE.-6.7005.AND.RVCNE1.LE.-6.6995) IVCOMP=IVCOMP*13     04420302
      IVCORR = 30030                                                    04430302
C     30030  = 2 * 3 * 5 * 7 * 11 * 13                                  04440302
40090 IF (IVCOMP - 30030) 20090, 10090, 20090                           04450302
30090 IVDELE = IVDELE + 1                                               04460302
      WRITE (I02,80000) IVTNUM                                          04470302
      IF (ICZERO) 10090, 0101, 20090                                    04480302
10090 IVPASS = IVPASS + 1                                               04490302
      WRITE (I02,80002) IVTNUM                                          04500302
      GO TO 0101                                                        04510302
20090 IVFAIL = IVFAIL + 1                                               04520302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04530302
 0101 CONTINUE                                                          04540302
C                                                                       04550302
C     ****  FCVS PROGRAM 302  -  TEST 010  ****                         04560302
C                                                                       04570302
C          TEST 010 IS DESIGNED TO TEST THE ABILITY TO RENAME ENTITIES  04580302
C     IN NAMED COMMON BETWEEN A MAIN PROGRAM AND A SUBROUTINE.          04590302
C                                                                       04600302
      IVTNUM =  10                                                      04610302
      IF (ICZERO) 30100, 0100, 30100                                    04620302
 0100 CONTINUE                                                          04630302
      IVCOMP = 1                                                        04640302
      IF (IVCNF1 .EQ. 42) IVCOMP = IVCOMP * 2                           04650302
      IF (IVCNF2 .EQ. 43) IVCOMP = IVCOMP * 3                           04660302
      IF (IVCNF3 .EQ. 44) IVCOMP = IVCOMP * 5                           04670302
      IF (IACN1F(1) .EQ. 142) IVCOMP = IVCOMP * 7                       04680302
      IF (IACN1F(2) .EQ. 143) IVCOMP = IVCOMP * 11                      04690302
      IF (IACN1F(3) .EQ. 144) IVCOMP = IVCOMP * 13                      04700302
      IVCORR = 30030                                                    04710302
C     30030 = 2 * 3 * 5 * 7 * 11 * 13                                   04720302
40100 IF (IVCOMP - 30030) 20100, 10100, 20100                           04730302
30100 IVDELE = IVDELE + 1                                               04740302
      WRITE (I02,80000) IVTNUM                                          04750302
      IF (ICZERO) 10100, 0111, 20100                                    04760302
10100 IVPASS = IVPASS + 1                                               04770302
      WRITE (I02,80002) IVTNUM                                          04780302
      GO TO 0111                                                        04790302
20100 IVFAIL = IVFAIL + 1                                               04800302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04810302
 0111 CONTINUE                                                          04820302
C                                                                       04830302
C     ****  FCVS PROGRAM 302  -  TEST 011  ****                         04840302
C                                                                       04850302
C          TEST 011 IS DESIGNED TO TEST THE STORAGE OF A VARIABLE  IN   04860302
C     NAMED COMMON BY EQUIVALENCE ASSOCIATION.                          04870302
C                                                                       04880302
      IVTNUM =  11                                                      04890302
      IF (ICZERO) 30110, 0110, 30110                                    04900302
 0110 CONTINUE                                                          04910302
      IVCOMP = 0                                                        04920302
      IVCOMP = IVCEH2                                                   04930302
      IVCORR = 6                                                        04940302
40110 IF (IVCOMP - 6) 20110, 10110, 20110                               04950302
30110 IVDELE = IVDELE + 1                                               04960302
      WRITE (I02,80000) IVTNUM                                          04970302
      IF (ICZERO) 10110, 0121, 20110                                    04980302
10110 IVPASS = IVPASS + 1                                               04990302
      WRITE (I02,80002) IVTNUM                                          05000302
      GO TO 0121                                                        05010302
20110 IVFAIL = IVFAIL + 1                                               05020302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05030302
 0121 CONTINUE                                                          05040302
C                                                                       05050302
C     ****  FCVS PROGRAM 302  -  TEST 012  ****                         05060302
C                                                                       05070302
C          TEST 012 IS DESIGNED TO TEST THE STORAGE OF A VARIABLE IN    05080302
C     UNNAMED COMMON BY EQUIVALENCE ASSOCIATION.                        05090302
C                                                                       05100302
      IVTNUM =  12                                                      05110302
      IF (ICZERO) 30120, 0120, 30120                                    05120302
 0120 CONTINUE                                                          05130302
      IVCOMP = 1                                                        05140302
      IF (IVCE09 .EQ. 100) IVCOMP = IVCOMP * 2                          05150302
      IF (IVCE10 .EQ. 100) IVCOMP = IVCOMP * 3                          05160302
      IVCORR = 6                                                        05170302
C     6 = 2 * 3                                                         05180302
40120 IF (IVCOMP - 6) 20120, 10120, 20120                               05190302
30120 IVDELE = IVDELE + 1                                               05200302
      WRITE (I02,80000) IVTNUM                                          05210302
      IF (ICZERO) 10120, 0131, 20120                                    05220302
10120 IVPASS = IVPASS + 1                                               05230302
      WRITE (I02,80002) IVTNUM                                          05240302
      GO TO 0131                                                        05250302
20120 IVFAIL = IVFAIL + 1                                               05260302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05270302
 0131 CONTINUE                                                          05280302
C                                                                       05290302
C     ****  FCVS PROGRAM 302  -  TEST 013  ****                         05300302
C                                                                       05310302
C          TEST 013 IS DESIGNED TO TEST THE EXTENSION OF NAMED COMMON   05320302
C     BLOCK STORAGE BY EQUIVALENCE ASSOCIATION OF A VARIABLE AND AN     05330302
C     ARRAY.                                                            05340302
C                                                                       05350302
      IVTNUM =  13                                                      05360302
      IF (ICZERO) 30130, 0130, 30130                                    05370302
 0130 CONTINUE                                                          05380302
      IVCOMP = 1                                                        05390302
      IF (IVCEI1 .EQ. 11) IVCOMP = IVCOMP * 2                           05400302
      IF (IACE1I(1) .EQ. 11) IVCOMP = IVCOMP * 3                        05410302
      IF (IACE1I(2) .EQ. 16) IVCOMP = IVCOMP * 5                        05420302
      IF (IACE1I(3) .EQ. 21) IVCOMP = IVCOMP * 7                        05430302
      IVCORR = 210                                                      05440302
C     210 = 2 * 3 * 5 * 7                                               05450302
40130 IF (IVCOMP - 210) 20130, 10130, 20130                             05460302
30130 IVDELE = IVDELE + 1                                               05470302
      WRITE (I02,80000) IVTNUM                                          05480302
      IF (ICZERO) 10130, 0141, 20130                                    05490302
10130 IVPASS = IVPASS + 1                                               05500302
      WRITE (I02,80002) IVTNUM                                          05510302
      GO TO 0141                                                        05520302
20130 IVFAIL = IVFAIL + 1                                               05530302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05540302
 0141 CONTINUE                                                          05550302
C                                                                       05560302
C     ****  FCVS PROGRAM 302  -  TEST 014  ****                         05570302
C                                                                       05580302
C          TEST 014 IS DESIGNED TO TEST THE ABILITY OF PASSING DATA     05590302
C     THROUGH UNNAMED COMMON FROM EXTERNAL FUNCTIONS WHICH HAVE MORE    05600302
C     ENTITIES IN UNNAMED COMMON THAN THE MAIN PROGRAM.                 05610302
C                                                                       05620302
      IVTNUM =  14                                                      05630302
      IF (ICZERO) 30140, 0140, 30140                                    05640302
 0140 CONTINUE                                                          05650302
      IVCOMP = 0                                                        05660302
      IVCOMP = IVCN12                                                   05670302
      IVCORR = 11                                                       05680302
40140 IF (IVCOMP - 11) 20140, 10140, 20140                              05690302
30140 IVDELE = IVDELE + 1                                               05700302
      WRITE (I02,80000) IVTNUM                                          05710302
      IF (ICZERO) 10140, 0151, 20140                                    05720302
10140 IVPASS = IVPASS + 1                                               05730302
      WRITE (I02,80002) IVTNUM                                          05740302
      GO TO 0151                                                        05750302
20140 IVFAIL = IVFAIL + 1                                               05760302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05770302
 0151 CONTINUE                                                          05780302
C                                                                       05790302
C     ****  FCVS PROGRAM 302  -  TEST 015  ****                         05800302
C                                                                       05810302
C          TEST 015 IS DESIGNED TO TEST THE ABILITY OF PASSING DATA     05820302
C     THROUGH NAMED COMMON BETWEEN EXTERNAL FUNCTIONS WHERE THE NAMED   05830302
C     COMMON BLOCK IS NOT SPECIFIED IN THE MAIN PROGRAM.                05840302
C                                                                       05850302
      IVTNUM =  15                                                      05860302
      IF (ICZERO) 30150, 0150, 30150                                    05870302
 0150 CONTINUE                                                          05880302
      IVCOMP = 0                                                        05890302
      IVCOMP = IVCNJ1                                                   05900302
      IVCORR = 5                                                        05910302
40150 IF (IVCOMP - 5) 20150, 10150, 20150                               05920302
30150 IVDELE = IVDELE + 1                                               05930302
      WRITE (I02,80000) IVTNUM                                          05940302
      IF (ICZERO) 10150, 0161, 20150                                    05950302
10150 IVPASS = IVPASS + 1                                               05960302
      WRITE (I02,80002) IVTNUM                                          05970302
      GO TO 0161                                                        05980302
20150 IVFAIL = IVFAIL + 1                                               05990302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06000302
 0161 CONTINUE                                                          06010302
C                                                                       06020302
C     ****  FCVS PROGRAM 302  -  TEST 016  ****                         06030302
C                                                                       06040302
C          TEST 016 IS DESIGNED TO TEST THE PASSING OF CHARACTER DATA   06050302
C     IN NAMED COMMON BETWEEN THE MAIN PROGRAM AND A SUBROUTINE.        06060302
C                                                                       06070302
      IVTNUM =  16                                                      06080302
      IF (ICZERO) 30160, 0160, 30160                                    06090302
 0160 CONTINUE                                                          06100302
      IVCOMP = 1                                                        06110302
      IF (CVTN01 .EQ. 'YZ') IVCOMP = IVCOMP * 2                         06120302
      IF (CVTN02 .EQ. 'UVW') IVCOMP = IVCOMP * 3                        06130302
      IF (CATN11(1) .EQ. 'VWXYZ') IVCOMP = IVCOMP * 5                   06140302
      IF (CATN11(2) .EQ. 'KLMNO') IVCOMP = IVCOMP * 7                   06150302
      IF (CATN11(3) .EQ. 'ABCDE') IVCOMP = IVCOMP * 11                  06160302
      IVCORR = 2310                                                     06170302
C     2310 = 2 * 3 * 5 * 7 * 11                                         06180302
40160 IF (IVCOMP - 2310) 20160, 10160, 20160                            06190302
30160 IVDELE = IVDELE + 1                                               06200302
      WRITE (I02,80000) IVTNUM                                          06210302
      IF (ICZERO) 10160, 0171, 20160                                    06220302
10160 IVPASS = IVPASS + 1                                               06230302
      WRITE (I02,80002) IVTNUM                                          06240302
      GO TO 0171                                                        06250302
20160 IVFAIL = IVFAIL + 1                                               06260302
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06270302
 0171 CONTINUE                                                          06280302
C                                                                       06290302
C                                                                       06300302
C     WRITE OUT TEST SUMMARY                                            06310302
C                                                                       06320302
      WRITE (I02,90004)                                                 06330302
      WRITE (I02,90014)                                                 06340302
      WRITE (I02,90004)                                                 06350302
      WRITE (I02,90000)                                                 06360302
      WRITE (I02,90004)                                                 06370302
      WRITE (I02,90020) IVFAIL                                          06380302
      WRITE (I02,90022) IVPASS                                          06390302
      WRITE (I02,90024) IVDELE                                          06400302
      STOP                                                              06410302
90001 FORMAT (1H ,24X,5HFM302)                                          06420302
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM302)                          06430302
C                                                                       06440302
C     FORMATS FOR TEST DETAIL LINES                                     06450302
C                                                                       06460302
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   06470302
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      06480302
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         06490302
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    06500302
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        06510302
C                                                                       06520302
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06530302
C                                                                       06540302
90002 FORMAT (1H1)                                                      06550302
90004 FORMAT (1H )                                                      06560302
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06570302
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   06580302
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         06590302
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  06600302
90014 FORMAT (1H ,5X,46H----------------------------------------------) 06610302
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             06620302
C                                                                       06630302
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 06640302
C                                                                       06650302
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              06660302
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              06670302
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             06680302
      END                                                               06690302
      SUBROUTINE FS303                                                  00010303
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020303
C        FS303 IS A SUBROUTINE WHICH IS CALLED ONCE FROM PROGRAM FM302. 00030303
C     IT IS USED TO MODIFY VARIABLES AND ARRAY PASSED THROUGH NAMED AND 00040303
C     UNNAMED COMMON FROM FM302.  AFTER THE DATA ENTITIES ARE MODIFIED  00050303
C     CONTROL IS RETURNED TO FM302 WHERE EACH ENTITY IS TESTED.         00060303
C                                                                       00070303
      IMPLICIT LOGICAL (L)                                              00080303
      DIMENSION RACN11(10)                                              00090303
      COMMON IVCN01                                                     00100303
      COMMON //IVCN02, LVCN01                                           00110303
      COMMON RVCN01//IVCN03                                             00120303
      COMMON IVCN04,IVCN05, //IACN11(4)                                 00130303
      COMMON /BLK1/IVCNA1                                               00140303
      COMMON /BLK2/IVCNB1,RVCNB1,/BLK2/IVCNB2                           00150303
      COMMON /BLK3/LVCNC1,IVCNC1/BLK4/RACN11,IACN21(2,3)                00160303
      COMMON /BLK7/IACN1G(5),IVCNG1                                     00170303
      COMMON /BLK8/IVCNH1                                               00180303
      COMMON /BLKCHR/CVTN01,CVTN02,CATN11                               00190303
      CHARACTER CVTN01*2, CVTN02*3, CATN11(3)*5                         00200303
C     TEST 001                                                          00210303
           IVCN01 = IVCN01 + 1                                          00220303
C     TEST 002                                                          00230303
           IVCN02 = IVCN02 + 5                                          00240303
           LVCN01 = .NOT. LVCN01                                        00250303
C     TEST 005                                                          00260303
           IVCNA1 = IVCNA1 / 5                                          00270303
C     TEST 006                                                          00280303
           IVCNB1 = IVCNB1 + IVCNB2                                     00290303
           RVCNB1 = 3.5                                                 00300303
C     TEST 007                                                          00310303
           LVCNC1 = .FALSE.                                             00320303
           IVCNC1 = IVCNC1 - 1                                          00330303
           RACN11(1) = 111.                                             00340303
           RACN11(10) = 110.                                            00350303
           IACN21(1,1) = IACN21(1,1) + 1                                00360303
           IACN21(2,3) = IACN21(2,3) + 1                                00370303
C     TEST 010                                                          00380303
           IACN1G(1) = IACN1G(1) + 1                                    00390303
           IACN1G(2) = 43                                               00400303
           IACN1G(3) = IACN1G(3) + 1                                    00410303
           IACN1G(4) = IACN1G(4) + 1                                    00420303
           IACN1G(5) = IACN1G(5) + 1                                    00430303
           IVCNG1 = 144                                                 00440303
C     TEST 011                                                          00450303
           IVCNH1 = IVCNH1 + 1                                          00460303
C     TEST 017                                                          00470303
           CVTN01 = 'YZ'                                                00480303
           CVTN02 = 'UVW'                                               00490303
           CATN11(1) = 'VWXYZ'                                          00500303
           CATN11(3) = 'ABCDE'                                          00510303
      RETURN                                                            00520303
      END                                                               00530303
      INTEGER FUNCTION FF304 ()                                         00010304
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020304
C          FF304 IS AN EXTERNAL FUNCTION WHICH IS REFERENCED ONCE FROM  00030304
C     PROGRAM FM302.  IT IS USED TO MODIFY VARIABLES AND ARRAYS PASSED  00040304
C     THROUGH NAMED AND UNNAMED COMMON FROM FM302.  AFTER THE DATA      00050304
C     ENTITIES ARE MODIFIED CONTROL IS RETURNED TO FM302 WHERE EACH     00060304
C     ENTITY IS TESTED.  A FUNCTION VALUE OF 999 IS RETURNED BUT IT IS  00070304
C     NOT SIGNIFICANT NOR IS IT TESTED BY FM302.                        00080304
C                                                                       00090304
      IMPLICIT LOGICAL (L)                                              00100304
      DIMENSION IACN11(4)                                               00110304
      COMMON IVCN01                                                     00120304
      COMMON IVCN02, LVCN01                                             00130304
      COMMON RVCN01, IVCN03                                             00140304
      COMMON IVCN04,IVCN05,IACN11                                       00150304
      COMMON /BLK5/IVCND1,IVCND2                                        00160304
      COMMON IVCN06                                                     00170304
      COMMON /BLK5/RVCND1,LVCND1                                        00180304
      COMMON IVCN07, IVCN08                                             00190304
      COMMON /BLK6/RVCNE1                                               00200304
      COMMON IVCN10                                                     00210304
      COMMON /BLK9/IVCNI1, IVCNI2, IVCNI3                               00220304
      COMMON IVCN12, IVCN13                                             00230304
      COMMON /BLK10/IVCNJ1                                              00240304
      COMMON /BLK11/IVCNK1                                              00250304
      INTEGER FF305                                                     00260304
C     TEST 003                                                          00270304
           RVCN01 = 4.2                                                 00280304
           IVCN03 = IVCN03 + 1                                          00290304
C     TEST 004                                                          00300304
           IVCN04 = 32                                                  00310304
           IVCN04 = IVCN04 / 4                                          00320304
           IVCN05 = IVCN05                                              00330304
           IACN11(1) = IACN11(1) + 4                                    00340304
           IACN11(2) = IACN11(2) + 3                                    00350304
           IACN11(3) = IACN11(3) + 2                                    00360304
           IACN11(4) = IACN11(4) + 1                                    00370304
C     TEST 008                                                          00380304
           IVCND1 = IVCND1 + 1                                          00390304
           IVCND2 = IVCND2 + 1                                          00400304
C     TEST 009                                                          00410304
           IVCN06 = IVCN06 + 1                                          00420304
           RVCND1 = 4.5                                                 00430304
           LVCND1 = .TRUE.                                              00440304
           IVCN07 = -IVCN07                                             00450304
           IVCN08 = -3                                                  00460304
           RVCNE1 = -6.7                                                00470304
C     TEST 012                                                          00480304
           IVCN10 = IVCN10 * IVCN10                                     00490304
C     TEST 013                                                          00500304
           IVCNI1 = IVCNI1 + 1                                          00510304
           IVCNI2 = IVCNI2 + 1                                          00520304
           IVCNI3 = IVCNI3 + 1                                          00530304
C     TEST 014                                                          00540304
           IVCN13 = 5                                                   00550304
C     TEST 015                                                          00560304
           IVCNK1 = 3                                                   00570304
C                                                                       00580304
C     FOR TESTS 014 AND 015 EXTERNAL FUNCTION FF305 IS REFERENCED       00590304
C                                                                       00600304
           IVON99 = FF305 ()                                            00610304
C     TEST 014                                                          00620304
           IVCN12 = IVCN13                                              00630304
C     TEST 015                                                          00640304
           IVCNJ1 = IVCNK1                                              00650304
      FF304 = 999                                                       00660304
      RETURN                                                            00670304
      END                                                               00680304
      INTEGER FUNCTION FF305 ()                                         00010305
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020305
C          FF305 IS AN EXTERNAL FUNCTION WHICH IS USED IN TEST 014 AND  00030305
C     015 OF PROGRAM FM302. THIS SUBPROGRAM IS REFERENCED FROM EXTERNAL 00040305
C     FUNCTION FF304.                                                   00050305
C                                                                       00060305
      COMMON IACN11(15)                                                 00070305
      COMMON IVCN12, IVCN13, IVCN14                                     00080305
      COMMON /BLK10/IVCNJ1, /BLK11/IVCNK1                               00090305
C     TEST 014                                                          00100305
           IVCN14 = 11                                                  00110305
           IVCN13 = IVCN14                                              00120305
C     TEST 015                                                          00130305
           IVCNK1 = 5                                                   00140305
      FF305 = 999                                                       00150305
      RETURN                                                            00160305
      END                                                               00170305
