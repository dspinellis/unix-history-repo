      PROGRAM FM201                                                     00010201
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020201
C                                                                       00030201
C        THIS ROUTINE VERIFIES THAT                                     00040201
C                                                                       00050201
C        (1)  THE VALUE OF A SIGNED ZERO IS THE SAME AS THE VALUE OF    00060201
C             AN UNSIGNED ZERO FOR INTEGER AND REAL VARIABLES.          00070201
C                                                                       00080201
C        (2)  A BASIC REAL CONSTANT MAY BE WRITTEN WITH MORE DIGITS     00090201
C             THAN A PROCESSOR WILL USE TO APPROXIMATE THE VALUE OF     00100201
C             THE CONSTANT.                                             00110201
C                                                                       00120201
C        (3)  AN IMPLICIT STATEMENT CAN BE USED TO CHANGE THE DEFAULT   00130201
C             IMPLICIT INTEGER AND REAL TYPING.                         00140201
C                                                                       00150201
C        (4)  THE IMPLICIT INTEGER AND REAL TYPING OF AN IMPLICIT       00160201
C             STATEMENT MAY BE OVERRIDDEN BY THE APPEARANCE OF A        00170201
C             VARIABLE NAME IN A TYPE-STATEMENT.                        00180201
C                                                                       00190201
C     REFERENCES                                                        00200201
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00210201
C              X3.9-1978                                                00220201
C                                                                       00230201
C        SECTION 4.1.3, DATA TYPE PROPERTIES                            00240201
C        SECTION 4.4.1, BASIC REAL CONSTANT                             00250201
C        SECTION 6.1.5, INTEGER DIVISION                                00260201
C        SECTION 8.4,   TYPE-STATEMENTS                                 00270201
C        SECTION 8.5,   IMPLICIT STATEMENT                              00280201
C                                                                       00290201
C                                                                       00300201
C     ******************************************************************00310201
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00320201
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00330201
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00340201
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00350201
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00360201
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00370201
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00380201
C     THE RESULT OF EXECUTING THESE TESTS.                              00390201
C                                                                       00400201
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00410201
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00420201
C                                                                       00430201
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00440201
C                    DEPARTMENT OF THE NAVY                             00450201
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00460201
C                    WASHINGTON, D.C.   20376                           00470201
C                                                                       00480201
C     ******************************************************************00490201
C                                                                       00500201
C                                                                       00510201
      IMPLICIT LOGICAL (L)                                              00520201
      IMPLICIT CHARACTER*14 (C)                                         00530201
C                                                                       00540201
      IMPLICIT INTEGER (Y, V-X), REAL (M)                               00550201
      REAL RVTN01, RVTN02, RVTN03, YVTN02                               00560201
      INTEGER IVTN01, IVTN02, MVTN02                                    00570201
C        THE ABOVE THREE STATEMENTS ARE REFERENCED IN TESTS 29 THRU 35. 00580201
C                                                                       00590201
C                                                                       00600201
C                                                                       00610201
C     INITIALIZATION SECTION.                                           00620201
C                                                                       00630201
C     INITIALIZE CONSTANTS                                              00640201
C     ********************                                              00650201
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00660201
      I01 = 5                                                           00670201
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00680201
      I02 = 6                                                           00690201
C     SYSTEM ENVIRONMENT SECTION                                        00700201
C                                                                       00710201
      I01 = 5                                                           00720201
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00730201
C     (UNIT NUMBER FOR CARD READER).                                    00740201
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00750201
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760201
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00770201
C                                                                       00780201
      I02 = 6                                                           00790201
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00800201
C     (UNIT NUMBER FOR PRINTER).                                        00810201
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00820201
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00830201
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00840201
C                                                                       00850201
      IVPASS = 0                                                        00860201
      IVFAIL = 0                                                        00870201
      IVDELE = 0                                                        00880201
      ICZERO = 0                                                        00890201
C                                                                       00900201
C     WRITE OUT PAGE HEADERS                                            00910201
C                                                                       00920201
      WRITE (I02,90002)                                                 00930201
      WRITE (I02,90006)                                                 00940201
      WRITE (I02,90008)                                                 00950201
      WRITE (I02,90004)                                                 00960201
      WRITE (I02,90010)                                                 00970201
      WRITE (I02,90004)                                                 00980201
      WRITE (I02,90016)                                                 00990201
      WRITE (I02,90001)                                                 01000201
      WRITE (I02,90004)                                                 01010201
      WRITE (I02,90012)                                                 01020201
      WRITE (I02,90014)                                                 01030201
      WRITE (I02,90004)                                                 01040201
C                                                                       01050201
C                                                                       01060201
C        TEST 14 THROUGH TEST 17 COMPARE INTEGER VARIABLES WHICH ARE    01070201
C     SET TO SIGNED ZERO AND UNSIGNED ZERO VALUES BY THE FOLLOWING      01080201
C     STATEMENTS                                                        01090201
C                                                                       01100201
         IVON01 = 0                                                     01110201
         IVON02 = -0                                                    01120201
         IVON03 = +0                                                    01130201
C                                                                       01140201
C     REFERENCE   X3.9-1978, SECTION 4.1.3, DATA TYPE PROPERTIES        01150201
C                                                                       01160201
C     ****  FCVS PROGRAM 201  -  TEST 014  ****                         01170201
C                                                                       01180201
C        COMPARE 0 TO -0                                                01190201
C                                                                       01200201
      IVTNUM =  14                                                      01210201
      IF (ICZERO) 30140, 0140, 30140                                    01220201
 0140 CONTINUE                                                          01230201
      IVCOMP = 1                                                        01240201
      IVCORR = 0                                                        01250201
      IF (IVON01 .EQ. IVON02) IVCOMP = 0                                01260201
40140 IF (IVCOMP) 20140, 10140, 20140                                   01270201
30140 IVDELE = IVDELE + 1                                               01280201
      WRITE (I02,80000) IVTNUM                                          01290201
      IF (ICZERO) 10140, 0151, 20140                                    01300201
10140 IVPASS = IVPASS + 1                                               01310201
      WRITE (I02,80002) IVTNUM                                          01320201
      GO TO 0151                                                        01330201
20140 IVFAIL = IVFAIL + 1                                               01340201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01350201
 0151 CONTINUE                                                          01360201
C                                                                       01370201
C     ****  FCVS PROGRAM 201  -  TEST 015  ****                         01380201
C                                                                       01390201
C        COMPARE 0 TO +0                                                01400201
C                                                                       01410201
      IVTNUM =  15                                                      01420201
      IF (ICZERO) 30150, 0150, 30150                                    01430201
 0150 CONTINUE                                                          01440201
      IVCOMP = 1                                                        01450201
      IVCORR = 0                                                        01460201
      IF (IVON01 .EQ. IVON03) IVCOMP = 0                                01470201
40150 IF (IVCOMP) 20150, 10150, 20150                                   01480201
30150 IVDELE = IVDELE + 1                                               01490201
      WRITE (I02,80000) IVTNUM                                          01500201
      IF (ICZERO) 10150, 0161, 20150                                    01510201
10150 IVPASS = IVPASS + 1                                               01520201
      WRITE (I02,80002) IVTNUM                                          01530201
      GO TO 0161                                                        01540201
20150 IVFAIL = IVFAIL + 1                                               01550201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01560201
 0161 CONTINUE                                                          01570201
C                                                                       01580201
C     ****  FCVS PROGRAM 201  -  TEST 016  ****                         01590201
C                                                                       01600201
C        COMPARE -0 TO +0                                               01610201
C                                                                       01620201
      IVTNUM =  16                                                      01630201
      IF (ICZERO) 30160, 0160, 30160                                    01640201
 0160 CONTINUE                                                          01650201
      IVCOMP = 1                                                        01660201
      IVCORR = 0                                                        01670201
      IF (IVON02 .EQ. IVON03) IVCOMP = 0                                01680201
40160 IF (IVCOMP) 20160, 10160, 20160                                   01690201
30160 IVDELE = IVDELE + 1                                               01700201
      WRITE (I02,80000) IVTNUM                                          01710201
      IF (ICZERO) 10160, 0171, 20160                                    01720201
10160 IVPASS = IVPASS + 1                                               01730201
      WRITE (I02,80002) IVTNUM                                          01740201
      GO TO 0171                                                        01750201
20160 IVFAIL = IVFAIL + 1                                               01760201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01770201
 0171 CONTINUE                                                          01780201
C                                                                       01790201
C     ****  FCVS PROGRAM 201  -  TEST 017  ****                         01800201
C                                                                       01810201
C        MINUS ZERO (-0) SHOULD NOT BE LESS THAN PLUS ZERO (+0)         01820201
C                                                                       01830201
      IVTNUM =  17                                                      01840201
      IF (ICZERO) 30170, 0170, 30170                                    01850201
 0170 CONTINUE                                                          01860201
      IVCOMP = 1                                                        01870201
      IVCORR = 0                                                        01880201
      IF (IVON02 .LT. IVON03) GO TO 20170                               01890201
      IVCOMP = 0                                                        01900201
      GO TO 10170                                                       01910201
30170 IVDELE = IVDELE + 1                                               01920201
      WRITE (I02,80000) IVTNUM                                          01930201
      IF (ICZERO) 10170, 0181, 20170                                    01940201
10170 IVPASS = IVPASS + 1                                               01950201
      WRITE (I02,80002) IVTNUM                                          01960201
      GO TO 0181                                                        01970201
20170 IVFAIL = IVFAIL + 1                                               01980201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01990201
 0181 CONTINUE                                                          02000201
C                                                                       02010201
C        TEST 18 THROUGH TEST 24 COMPARE REAL VARIABLES WHICH ARE SET   02020201
C     TO SIGNED ZERO AND UNSIGNED ZERO VALUES BY THE FOLLOWING          02030201
C     STATEMENTS                                                        02040201
C                                                                       02050201
         RVON01 = 0.0                                                   02060201
         RVON02 = -0.0                                                  02070201
         RVON03 = +0.0                                                  02080201
         RVON04 = -0.0E+01                                              02090201
         RVON05 = -0E+10                                                02100201
C                                                                       02110201
C     REFERENCE   X3.9-1978, SECTION 4.1.3, DATA TYPE PROPERTIES        02120201
C                                                                       02130201
C     ****  FCVS PROGRAM 201  -  TEST 018  ****                         02140201
C                                                                       02150201
C        COMPARE 0.0 TO -0.0                                            02160201
C                                                                       02170201
      IVTNUM =  18                                                      02180201
      IF (ICZERO) 30180, 0180, 30180                                    02190201
 0180 CONTINUE                                                          02200201
      IVCOMP = 1                                                        02210201
      IVCORR = 0                                                        02220201
      IF (RVON01 .EQ. RVON02) IVCOMP = 0                                02230201
40180 IF (IVCOMP) 20180, 10180, 20180                                   02240201
30180 IVDELE = IVDELE + 1                                               02250201
      WRITE (I02,80000) IVTNUM                                          02260201
      IF (ICZERO) 10180, 0191, 20180                                    02270201
10180 IVPASS = IVPASS + 1                                               02280201
      WRITE (I02,80002) IVTNUM                                          02290201
      GO TO 0191                                                        02300201
20180 IVFAIL = IVFAIL + 1                                               02310201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02320201
 0191 CONTINUE                                                          02330201
C                                                                       02340201
C     ****  FCVS PROGRAM 201  -  TEST 019  ****                         02350201
C                                                                       02360201
C        COMPARE 0.0 TO +0.0                                            02370201
C                                                                       02380201
      IVTNUM =  19                                                      02390201
      IF (ICZERO) 30190, 0190, 30190                                    02400201
 0190 CONTINUE                                                          02410201
      IVCOMP = 1                                                        02420201
      IVCORR = 0                                                        02430201
      IF (RVON01 .EQ. RVON03)  IVCOMP = 0                               02440201
40190 IF (IVCOMP) 20190, 10190, 20190                                   02450201
30190 IVDELE = IVDELE + 1                                               02460201
      WRITE (I02,80000) IVTNUM                                          02470201
      IF (ICZERO) 10190, 0201, 20190                                    02480201
10190 IVPASS = IVPASS + 1                                               02490201
      WRITE (I02,80002) IVTNUM                                          02500201
      GO TO 0201                                                        02510201
20190 IVFAIL = IVFAIL + 1                                               02520201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02530201
 0201 CONTINUE                                                          02540201
C                                                                       02550201
C     ****  FCVS PROGRAM 201  -  TEST 020  ****                         02560201
C                                                                       02570201
C        COMPARE -0.0 TO +0.0                                           02580201
C                                                                       02590201
      IVTNUM =  20                                                      02600201
      IF (ICZERO) 30200, 0200, 30200                                    02610201
 0200 CONTINUE                                                          02620201
      IVCOMP = 1                                                        02630201
      IVCORR = 0                                                        02640201
      IF (RVON02 .EQ. RVON03) IVCOMP = 0                                02650201
40200 IF (IVCOMP) 20200, 10200, 20200                                   02660201
30200 IVDELE = IVDELE + 1                                               02670201
      WRITE (I02,80000) IVTNUM                                          02680201
      IF (ICZERO) 10200, 0211, 20200                                    02690201
10200 IVPASS = IVPASS + 1                                               02700201
      WRITE (I02,80002) IVTNUM                                          02710201
      GO TO 0211                                                        02720201
20200 IVFAIL = IVFAIL + 1                                               02730201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02740201
 0211 CONTINUE                                                          02750201
C                                                                       02760201
C     ****  FCVS PROGRAM 201  -  TEST 021  ****                         02770201
C                                                                       02780201
C        MINUS ZERO (-0.0) SHOULD NOT BE LESS THAN PLUS ZERO (+0.0)     02790201
C                                                                       02800201
      IVTNUM =  21                                                      02810201
      IF (ICZERO) 30210, 0210, 30210                                    02820201
 0210 CONTINUE                                                          02830201
      IVCOMP = 1                                                        02840201
      IVCORR = 0                                                        02850201
      IF (RVON02 .LT. RVON03) GO TO 20210                               02860201
      IVCOMP = 0                                                        02870201
      GO TO 10210                                                       02880201
30210 IVDELE = IVDELE + 1                                               02890201
      WRITE (I02,80000) IVTNUM                                          02900201
      IF (ICZERO) 10210, 0221, 20210                                    02910201
10210 IVPASS = IVPASS + 1                                               02920201
      WRITE (I02,80002) IVTNUM                                          02930201
      GO TO 0221                                                        02940201
20210 IVFAIL = IVFAIL + 1                                               02950201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02960201
 0221 CONTINUE                                                          02970201
C                                                                       02980201
C     ****  FCVS PROGRAM 201  -  TEST 022  ****                         02990201
C                                                                       03000201
C        COMPARE -0.0E+01 TO 0.0                                        03010201
C                                                                       03020201
      IVTNUM =  22                                                      03030201
      IF (ICZERO) 30220, 0220, 30220                                    03040201
 0220 CONTINUE                                                          03050201
      IVCOMP = 1                                                        03060201
      IVCORR = 0                                                        03070201
      IF (RVON04 .EQ. RVON01) IVCOMP = 0                                03080201
40220 IF (IVCOMP) 20220, 10220, 20220                                   03090201
30220 IVDELE = IVDELE + 1                                               03100201
      WRITE (I02,80000) IVTNUM                                          03110201
      IF (ICZERO) 10220, 0231, 20220                                    03120201
10220 IVPASS = IVPASS + 1                                               03130201
      WRITE (I02,80002) IVTNUM                                          03140201
      GO TO 0231                                                        03150201
20220 IVFAIL = IVFAIL + 1                                               03160201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03170201
 0231 CONTINUE                                                          03180201
C                                                                       03190201
C     ****  FCVS PROGRAM 201  -  TEST 023  ****                         03200201
C                                                                       03210201
C        COMPARE -0E+10 TO 0.0                                          03220201
C                                                                       03230201
      IVTNUM =  23                                                      03240201
      IF (ICZERO) 30230, 0230, 30230                                    03250201
 0230 CONTINUE                                                          03260201
      IVCOMP = 1                                                        03270201
      IVCORR = 0                                                        03280201
      IF (RVON05 .EQ. RVON01) IVCOMP = 0                                03290201
40230 IF (IVCOMP) 20230, 10230, 20230                                   03300201
30230 IVDELE = IVDELE + 1                                               03310201
      WRITE (I02,80000) IVTNUM                                          03320201
      IF (ICZERO) 10230, 0241, 20230                                    03330201
10230 IVPASS = IVPASS + 1                                               03340201
      WRITE (I02,80002) IVTNUM                                          03350201
      GO TO 0241                                                        03360201
20230 IVFAIL = IVFAIL + 1                                               03370201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03380201
 0241 CONTINUE                                                          03390201
C                                                                       03400201
C     ****  FCVS PROGRAM 201  -  TEST 024  ****                         03410201
C                                                                       03420201
C        COMPARE -0E+10 TO +0.0                                         03430201
C                                                                       03440201
      IVTNUM =  24                                                      03450201
      IF (ICZERO) 30240, 0240, 30240                                    03460201
 0240 CONTINUE                                                          03470201
      IVCOMP = 1                                                        03480201
      IVCORR = 0                                                        03490201
      IF (RVON05 .NE. RVON03) GO TO 20240                               03500201
      IVCOMP = 0                                                        03510201
      GO TO 10240                                                       03520201
30240 IVDELE = IVDELE + 1                                               03530201
      WRITE (I02,80000) IVTNUM                                          03540201
      IF (ICZERO) 10240, 0251, 20240                                    03550201
10240 IVPASS = IVPASS + 1                                               03560201
      WRITE (I02,80002) IVTNUM                                          03570201
      GO TO 0251                                                        03580201
20240 IVFAIL = IVFAIL + 1                                               03590201
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03600201
 0251 CONTINUE                                                          03610201
C                                                                       03620201
C        TEST 25 THROUGH TEST 28 VERIFY THAT A BASIC REAL CONSTANT MAY  03630201
C     BE WRITTEN WITH MORE DIGITS THAN A PROCESSOR WILL USE TO APPROXI- 03640201
C     MATE THE VALUE OF THE CONSTANT.                                   03650201
C                                                                       03660201
C     REFERENCE   X3.9-1978, SECTION 4.4.1, BASIC REAL CONSTANT         03670201
C                                                                       03680201
C                                                                       03690201
C     ****  FCVS PROGRAM 201  -  TEST 025  ****                         03700201
C                                                                       03710201
C        EIGHT DIGITS IN BASIC REAL CONSTANT                            03720201
C                                                                       03730201
      IVTNUM =  25                                                      03740201
      IF (ICZERO) 30250, 0250, 30250                                    03750201
 0250 CONTINUE                                                          03760201
      RVON06 = 0.0                                                      03770201
      RVCOMP = 0.0                                                      03780201
      RVON06 = 3.1561234                                                03790201
      RVCOMP = RVON06                                                   03800201
      RVCORR = 3.1561                                                   03810201
40250 IF (RVCOMP - 3.1556) 20250, 10250, 40251                          03820201
40251 IF (RVCOMP - 3.1566) 10250, 10250, 20250                          03830201
30250 IVDELE = IVDELE + 1                                               03840201
      WRITE (I02,80000) IVTNUM                                          03850201
      IF (ICZERO) 10250, 0261, 20250                                    03860201
10250 IVPASS = IVPASS + 1                                               03870201
      WRITE (I02,80002) IVTNUM                                          03880201
      GO TO 0261                                                        03890201
20250 IVFAIL = IVFAIL + 1                                               03900201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03910201
 0261 CONTINUE                                                          03920201
C                                                                       03930201
C     ****  FCVS PROGRAM 201  -  TEST 026  ****                         03940201
C                                                                       03950201
C        EIGHT DIGITS IN BASIC REAL CONSTANT PLUS A REAL EXPONENT.      03960201
C                                                                       03970201
      IVTNUM =  26                                                      03980201
      IF (ICZERO) 30260, 0260, 30260                                    03990201
 0260 CONTINUE                                                          04000201
      RVON06 = 0.0                                                      04010201
      RVCOMP = 0.0                                                      04020201
      RVON06 = .31561234E+01                                            04030201
      RVCOMP = RVON06                                                   04040201
      RVCORR = 3.1561                                                   04050201
40260 IF (RVCOMP - 3.1556) 20260, 10260, 40261                          04060201
40261 IF (RVCOMP - 3.1566) 10260, 10260, 20260                          04070201
30260 IVDELE = IVDELE + 1                                               04080201
      WRITE (I02,80000) IVTNUM                                          04090201
      IF (ICZERO) 10260, 0271, 20260                                    04100201
10260 IVPASS = IVPASS + 1                                               04110201
      WRITE (I02,80002) IVTNUM                                          04120201
      GO TO 0271                                                        04130201
20260 IVFAIL = IVFAIL + 1                                               04140201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04150201
 0271 CONTINUE                                                          04160201
C                                                                       04170201
C     ****  FCVS PROGRAM 201  -  TEST 027  ****                         04180201
C                                                                       04190201
C        TWELVE DIGITS IN BASIC REAL CONSTANT.                          04200201
C                                                                       04210201
      IVTNUM =  27                                                      04220201
      IF (ICZERO) 30270, 0270, 30270                                    04230201
 0270 CONTINUE                                                          04240201
      RVON06 = 0.0                                                      04250201
      RVCOMP = 0.0                                                      04260201
      RVON06 = 315612347833 E-11                                        04270201
      RVCOMP = RVON06                                                   04280201
      RVCORR = 3.1561                                                   04290201
40270 IF (RVCOMP - 3.1556) 20270, 10270, 40271                          04300201
40271 IF (RVCOMP - 3.1566) 10270, 10270, 20270                          04310201
30270 IVDELE = IVDELE + 1                                               04320201
      WRITE (I02,80000) IVTNUM                                          04330201
      IF (ICZERO) 10270, 0281, 20270                                    04340201
10270 IVPASS = IVPASS + 1                                               04350201
      WRITE (I02,80002) IVTNUM                                          04360201
      GO TO 0281                                                        04370201
20270 IVFAIL = IVFAIL + 1                                               04380201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04390201
 0281 CONTINUE                                                          04400201
C                                                                       04410201
C     ****  FCVS PROGRAM 201  -  TEST 028  ****                         04420201
C                                                                       04430201
C        TWENTY-FIVE DIGITS IN BASIC REAL CONSTANT.                     04440201
C                                                                       04450201
      IVTNUM =  28                                                      04460201
      IF (ICZERO) 30280, 0280, 30280                                    04470201
 0280 CONTINUE                                                          04480201
      RVON06 = 0.0                                                      04490201
      RVCOMP = 0.0                                                      04500201
      RVON06 = 31.56123478334867532834672E-1                            04510201
      RVCOMP = RVON06                                                   04520201
      RVCORR = 3.1561                                                   04530201
40280 IF (RVCOMP - 3.1556) 20280, 10280, 40281                          04540201
40281 IF (RVCOMP - 3.1566) 10280, 10280, 20280                          04550201
30280 IVDELE = IVDELE + 1                                               04560201
      WRITE (I02,80000) IVTNUM                                          04570201
      IF (ICZERO) 10280, 0291, 20280                                    04580201
10280 IVPASS = IVPASS + 1                                               04590201
      WRITE (I02,80002) IVTNUM                                          04600201
      GO TO 0291                                                        04610201
20280 IVFAIL = IVFAIL + 1                                               04620201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04630201
 0291 CONTINUE                                                          04640201
C                                                                       04650201
C        TEST 29 THROUGH TEST 33 REFERENCE VARIABLES WHOSE TYPE WAS     04660201
C     SPECIFIED BY AN IMPLICIT STATEMENT.  DIVISION IS USED TO VERIFY   04670201
C     THAT THE TYPE IS INTEGER OR REAL.                                 04680201
C                                                                       04690201
C     REFERENCE   X3.9-1978, SECTION 8.5, IMPLICIT STATEMENT            04700201
C                                                                       04710201
C                                                                       04720201
C     ****  FCVS PROGRAM 201  -  TEST 029  ****                         04730201
C                                                                       04740201
C        VERIFY YVIN01 IS AN INTEGER VARIABLE.                          04750201
C                                                                       04760201
      IVTNUM =  29                                                      04770201
      IF (ICZERO) 30290, 0290, 30290                                    04780201
 0290 CONTINUE                                                          04790201
      RVCOMP = 10.0                                                     04800201
      YVIN01 = 4.0                                                      04810201
      RVCOMP = YVIN01/5                                                 04820201
      RVCORR = 0.0                                                      04830201
40290 IF (RVCOMP) 20290, 10290, 20290                                   04840201
30290 IVDELE = IVDELE + 1                                               04850201
      WRITE (I02,80000) IVTNUM                                          04860201
      IF (ICZERO) 10290, 0301, 20290                                    04870201
10290 IVPASS = IVPASS + 1                                               04880201
      WRITE (I02,80002) IVTNUM                                          04890201
      GO TO 0301                                                        04900201
20290 IVFAIL = IVFAIL + 1                                               04910201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04920201
 0301 CONTINUE                                                          04930201
C                                                                       04940201
C     ****  FCVS PROGRAM 201  -  TEST 030  ****                         04950201
C                                                                       04960201
C        VERIFY VVIN01 IS AN INTEGER VARIABLE                           04970201
C                                                                       04980201
      IVTNUM =  30                                                      04990201
      IF (ICZERO) 30300, 0300, 30300                                    05000201
 0300 CONTINUE                                                          05010201
      RVCOMP = 10.0                                                     05020201
      VVIN01 = 4.0                                                      05030201
      RVCOMP = VVIN01/5                                                 05040201
      RVCORR = 0.0                                                      05050201
40300 IF (RVCOMP) 20300, 10300, 20300                                   05060201
30300 IVDELE = IVDELE + 1                                               05070201
      WRITE (I02,80000) IVTNUM                                          05080201
      IF (ICZERO) 10300, 0311, 20300                                    05090201
10300 IVPASS = IVPASS + 1                                               05100201
      WRITE (I02,80002) IVTNUM                                          05110201
      GO TO 0311                                                        05120201
20300 IVFAIL = IVFAIL + 1                                               05130201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05140201
 0311 CONTINUE                                                          05150201
C                                                                       05160201
C     ****  FCVS PROGRAM 201  -  TEST 031  ****                         05170201
C                                                                       05180201
C        VERIFY WVIN01 IS AN INTEGER VARIABLE.                          05190201
C                                                                       05200201
      IVTNUM =  31                                                      05210201
      IF (ICZERO) 30310, 0310, 30310                                    05220201
 0310 CONTINUE                                                          05230201
      RVCOMP = 10.0                                                     05240201
      WVIN01 = 4.0                                                      05250201
      RVCOMP = WVIN01/5                                                 05260201
      RVCORR = 0.0                                                      05270201
40310 IF (RVCOMP) 20310, 10310, 20310                                   05280201
30310 IVDELE = IVDELE + 1                                               05290201
      WRITE (I02,80000) IVTNUM                                          05300201
      IF (ICZERO) 10310, 0321, 20310                                    05310201
10310 IVPASS = IVPASS + 1                                               05320201
      WRITE (I02,80002) IVTNUM                                          05330201
      GO TO 0321                                                        05340201
20310 IVFAIL = IVFAIL + 1                                               05350201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05360201
 0321 CONTINUE                                                          05370201
C                                                                       05380201
C     ****  FCVS PROGRAM 201  -  TEST 032  ****                         05390201
C                                                                       05400201
C        VERIFY XVIN01 IS AN INTEGER VARIABLE.                          05410201
C                                                                       05420201
      IVTNUM =  32                                                      05430201
      IF (ICZERO) 30320, 0320, 30320                                    05440201
 0320 CONTINUE                                                          05450201
      XVIN01 = 4                                                        05460201
      RVCOMP = 10.0                                                     05470201
      RVCOMP = XVIN01/5                                                 05480201
      RVCORR = 0.0                                                      05490201
40320 IF (RVCOMP) 20320, 10320, 20320                                   05500201
30320 IVDELE = IVDELE + 1                                               05510201
      WRITE (I02,80000) IVTNUM                                          05520201
      IF (ICZERO) 10320, 0331, 20320                                    05530201
10320 IVPASS = IVPASS + 1                                               05540201
      WRITE (I02,80002) IVTNUM                                          05550201
      GO TO 0331                                                        05560201
20320 IVFAIL = IVFAIL + 1                                               05570201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05580201
 0331 CONTINUE                                                          05590201
C                                                                       05600201
C     ****  FCVS PROGRAM 201  -  TEST 033  ****                         05610201
C                                                                       05620201
C        VERIFY MVIN01 IS A REAL VARIABLE.                              05630201
C                                                                       05640201
      IVTNUM =  33                                                      05650201
      IF (ICZERO) 30330, 0330, 30330                                    05660201
 0330 CONTINUE                                                          05670201
      RVCOMP = 10.0                                                     05680201
      MVIN01 = 4                                                        05690201
      RVCOMP = MVIN01/5                                                 05700201
      RVCORR = 0.8                                                      05710201
40330 IF (RVCOMP - 0.79995) 20330, 10330, 40331                         05720201
40331 IF (RVCOMP - 0.80005) 10330, 10330, 20330                         05730201
30330 IVDELE = IVDELE + 1                                               05740201
      WRITE (I02,80000) IVTNUM                                          05750201
      IF (ICZERO) 10330, 0341, 20330                                    05760201
10330 IVPASS = IVPASS + 1                                               05770201
      WRITE (I02,80002) IVTNUM                                          05780201
      GO TO 0341                                                        05790201
20330 IVFAIL = IVFAIL + 1                                               05800201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05810201
 0341 CONTINUE                                                          05820201
C                                                                       05830201
C        TEST 34 AND TEST 35 VERIFY THAT THE IMPLICIT TYPE SPECIFICA-   05840201
C     TION FOR A VARIABLE IS OVERRIDDEN BY THE APPEARANCE OF THAT       05850201
C     VARIABLE NAME IN A TYPE-STATEMENT.                                05860201
C                                                                       05870201
C     REFERENCE   X3.9-1977, SECTION 8.4, TYPE-STATEMENTS               05880201
C                            SECTION 8.5, IMPLICIT STATEMENT            05890201
C                                                                       05900201
C                                                                       05910201
C     ****  FCVS PROGRAM 201  -  TEST 034  ****                         05920201
C                                                                       05930201
C        VERIFY YVTN02 IS A REAL VARIABLE.                              05940201
C                                                                       05950201
      IVTNUM =  34                                                      05960201
      IF (ICZERO) 30340, 0340, 30340                                    05970201
 0340 CONTINUE                                                          05980201
      RVCOMP = 10.0                                                     05990201
      YVTN02 = 4                                                        06000201
      RVCOMP = YVTN02/5                                                 06010201
      RVCORR = 0.8                                                      06020201
40340 IF (RVCOMP - 0.79995) 20340, 10340, 40341                         06030201
40341 IF (RVCOMP - 0.80005) 10340, 10340, 20340                         06040201
30340 IVDELE = IVDELE + 1                                               06050201
      WRITE (I02,80000) IVTNUM                                          06060201
      IF (ICZERO) 10340, 0351, 20340                                    06070201
10340 IVPASS = IVPASS + 1                                               06080201
      WRITE (I02,80002) IVTNUM                                          06090201
      GO TO 0351                                                        06100201
20340 IVFAIL = IVFAIL + 1                                               06110201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06120201
 0351 CONTINUE                                                          06130201
C                                                                       06140201
C     ****  FCVS PROGRAM 201  -  TEST 035  ****                         06150201
C                                                                       06160201
C        VERIFY MVTN02 IS AN INTEGER VARIABLE.                          06170201
C                                                                       06180201
      IVTNUM =  35                                                      06190201
      IF (ICZERO) 30350, 0350, 30350                                    06200201
 0350 CONTINUE                                                          06210201
      RVCOMP = 10.0                                                     06220201
      MVTN02 = 4.0                                                      06230201
      RVCOMP = MVTN02/5                                                 06240201
      RVCORR = 0.0                                                      06250201
40350 IF (RVCOMP) 20350, 10350, 20350                                   06260201
30350 IVDELE = IVDELE + 1                                               06270201
      WRITE (I02,80000) IVTNUM                                          06280201
      IF (ICZERO) 10350, 0361, 20350                                    06290201
10350 IVPASS = IVPASS + 1                                               06300201
      WRITE (I02,80002) IVTNUM                                          06310201
      GO TO 0361                                                        06320201
20350 IVFAIL = IVFAIL + 1                                               06330201
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06340201
 0361 CONTINUE                                                          06350201
C                                                                       06360201
C                                                                       06370201
C     WRITE OUT TEST SUMMARY                                            06380201
C                                                                       06390201
      WRITE (I02,90004)                                                 06400201
      WRITE (I02,90014)                                                 06410201
      WRITE (I02,90004)                                                 06420201
      WRITE (I02,90000)                                                 06430201
      WRITE (I02,90004)                                                 06440201
      WRITE (I02,90020) IVFAIL                                          06450201
      WRITE (I02,90022) IVPASS                                          06460201
      WRITE (I02,90024) IVDELE                                          06470201
      STOP                                                              06480201
90001 FORMAT (1H ,24X,5HFM201)                                          06490201
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM201)                          06500201
C                                                                       06510201
C     FORMATS FOR TEST DETAIL LINES                                     06520201
C                                                                       06530201
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   06540201
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      06550201
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         06560201
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    06570201
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        06580201
C                                                                       06590201
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06600201
C                                                                       06610201
90002 FORMAT (1H1)                                                      06620201
90004 FORMAT (1H )                                                      06630201
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06640201
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   06650201
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         06660201
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  06670201
90014 FORMAT (1H ,5X,46H----------------------------------------------) 06680201
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             06690201
C                                                                       06700201
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 06710201
C                                                                       06720201
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              06730201
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              06740201
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             06750201
      END                                                               06760201
