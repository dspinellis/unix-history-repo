C     COMMENT SECTION                                                   00010062
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020062
C     FM062                                                             00030062
C                                                                       00040062
C          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS WHERE    00050062
C     AN ARITHMETIC EXPRESSION FORMED FROM REAL VARIABLES AND           00060062
C     CONSTANTS CONNECTED BY ARITHMETIC OPERATORS IS ASSIGNED TO        00070062
C     A REAL VARIABLE.  IN CASES INVOLVING THE EXPONENTIATION           00080062
C     OPERATOR, REAL VALUES ARE RAISED TO INTEGER POWERS ONLY.          00090062
C                                                                       00100062
C           A REAL DATUM IS A PROCESSOR APPROXIMATION TO THE VALUE OF A 00110062
C     REAL NUMBER.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES.   00120062
C                                                                       00130062
C          A BASIC REAL CONSTANT IS WRITTEN AS AN INTEGER PART, A       00140062
C     DECIMAL POINT, AND A DECIMAL FRACTION PART IN THAT ORDER.  BOTH   00150062
C     THE INTEGER PART AND THE DECIMAL PART ARE STRINGS OF DIGITS;      00160062
C     EITHER ONE OF THESE STRINGS MAY BE EMPTY BUT NOT BOTH.  THE       00170062
C     CONSTANT IS AN APPROXIMATION TO THE DIGIT STRING INTERPRETED AS A 00180062
C     DECIMAL NUMERAL.                                                  00190062
C                                                                       00200062
C         A DECIMAL EXPONENT IS WRITTEN AS THE LETTER E, FOLLOWED BY AN 00210062
C     OPTIONALLY SIGNED INTEGER CONSTANT.                               00220062
C                                                                       00230062
C         A REAL CONSTANT IS INDICATED BY WRITING A BASIC REAL CONSTANT,00240062
C     A BASIC REAL CONSTANT FOLLOWED BY A DECIMAL EXPONENT, OR AN       00250062
C     INTEGER CONSTANT FOLLOWED BY A DECIMAL EXPONENT.                  00260062
C                                                                       00270062
C      REFERENCES                                                       00280062
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00290062
C              X3.9-1978                                                00300062
C                                                                       00310062
C        SECTION 4.4, REAL TYPE                                         00320062
C        SECTION 4.4.1, REAL CONSTANT                                   00330062
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00340062
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00350062
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00360062
C                                                                       00370062
C      **********************************************************       00380062
C                                                                       00390062
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00400062
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00410062
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00420062
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00430062
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00440062
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00450062
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00460062
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00470062
C     OF EXECUTING THESE TESTS.                                         00480062
C                                                                       00490062
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00500062
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00510062
C                                                                       00520062
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00530062
C                                                                       00540062
C                  DEPARTMENT OF THE NAVY                               00550062
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00560062
C                  WASHINGTON, D.C.  20376                              00570062
C                                                                       00580062
C      **********************************************************       00590062
C                                                                       00600062
C                                                                       00610062
C                                                                       00620062
C     INITIALIZATION SECTION                                            00630062
C                                                                       00640062
C     INITIALIZE CONSTANTS                                              00650062
C      **************                                                   00660062
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00670062
      I01 = 5                                                           00680062
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00690062
      I02 = 6                                                           00700062
C     SYSTEM ENVIRONMENT SECTION                                        00710062
C                                                                       00720062
      I01 = 5                                                           00730062
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740062
C     (UNIT NUMBER FOR CARD READER).                                    00750062
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00760062
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00770062
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00780062
C                                                                       00790062
      I02 = 6                                                           00800062
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00810062
C     (UNIT NUMBER FOR PRINTER).                                        00820062
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00830062
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00840062
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00850062
C                                                                       00860062
      IVPASS=0                                                          00870062
      IVFAIL=0                                                          00880062
      IVDELE=0                                                          00890062
      ICZERO=0                                                          00900062
C                                                                       00910062
C     WRITE PAGE HEADERS                                                00920062
      WRITE (I02,90000)                                                 00930062
      WRITE (I02,90001)                                                 00940062
      WRITE (I02,90002)                                                 00950062
      WRITE (I02, 90002)                                                00960062
      WRITE (I02,90003)                                                 00970062
      WRITE (I02,90002)                                                 00980062
      WRITE (I02,90004)                                                 00990062
      WRITE (I02,90002)                                                 01000062
      WRITE (I02,90011)                                                 01010062
      WRITE (I02,90002)                                                 01020062
      WRITE (I02,90002)                                                 01030062
      WRITE (I02,90005)                                                 01040062
      WRITE (I02,90006)                                                 01050062
      WRITE (I02,90002)                                                 01060062
C                                                                       01070062
C     TEST SECTION                                                      01080062
C                                                                       01090062
C          ARITHMETIC ASSIGNMENT STATEMENT                              01100062
C                                                                       01110062
C                                                                       01120062
C     TESTS 62 THROUGH 70 USE A MIXTURE OF REAL VARIABLES AND REAL      01130062
C     CONSTANTS CONNECTED BY TWO IDENTICAL ARITHMETIC OPERATORS.        01140062
C     TESTS OCCUR IN PAIRS, ONE WITHOUT PARENTHESES AND ONE WITH        01150062
C     PARENTHESES TO ALTER THE NORMAL ORDER OF EVALUATION.              01160062
C                                                                       01170062
C     TESTS 71 THROUGH 90 USE THREE REAL VARIABLES CONNECTED BY A       01180062
C     PAIR OF DISSIMILAR OPERATORS.  ALL COMBINATIONS AND ORDERINGS     01190062
C     OF OPERATORS ARE EXERCIZED.  WHERE EXPONENTIATION IS TESTED,      01200062
C     INTEGER VARIABLES ARE USED FOR THE POWER PRIMARIES.               01210062
C                                                                       01220062
C     TESTS 91 AND 92 USE A SERIES OF REAL VARIABLES CONNECTED BY ONE   01230062
C     EACH OF THE ARITHMETIC OPERTORS.  PARENTHETICAL NOTATIONS ARE     01240062
C     ALSO TESTED.                                                      01250062
C                                                                       01260062
C                                                                       01270062
C                                                                       01280062
C                                                                       01290062
C                                                                       01300062
      IVTNUM =  62                                                      01310062
C                                                                       01320062
C      ****  TEST  62  ****                                             01330062
C                                                                       01340062
      IF (ICZERO) 30620,  620, 30620                                    01350062
  620 CONTINUE                                                          01360062
      RVON01 = 7.5                                                      01370062
      RVON02 = 5E2                                                      01380062
      RVCOMP = RVON01 + RVON02 + 33E-1                                  01390062
      GO TO 40620                                                       01400062
30620 IVDELE = IVDELE + 1                                               01410062
      WRITE (I02,80003) IVTNUM                                          01420062
      IF (ICZERO) 40620,  631, 40620                                    01430062
40620 IF (RVCOMP - 510.75) 20620,10620,40621                            01440062
40621 IF (RVCOMP - 510.85) 10620,10620,20620                            01450062
10620 IVPASS = IVPASS + 1                                               01460062
      WRITE (I02,80001) IVTNUM                                          01470062
      GO TO  631                                                        01480062
20620 IVFAIL = IVFAIL + 1                                               01490062
      RVCORR = 510.8                                                    01500062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01510062
  631 CONTINUE                                                          01520062
      IVTNUM =  63                                                      01530062
C                                                                       01540062
C      ****  TEST  63  ****                                             01550062
C                                                                       01560062
      IF (ICZERO) 30630,  630, 30630                                    01570062
  630 CONTINUE                                                          01580062
      RVON01 = 75E-1                                                    01590062
      RVON02 = 500.0                                                    01600062
      RVCOMP = RVON01 + (RVON02 + 3.3)                                  01610062
      GO TO 40630                                                       01620062
30630 IVDELE = IVDELE + 1                                               01630062
      WRITE (I02,80003) IVTNUM                                          01640062
      IF (ICZERO) 40630,  641, 40630                                    01650062
40630 IF (RVCOMP - 510.75) 20630,10630,40631                            01660062
40631 IF (RVCOMP - 510.85) 10630,10630,20630                            01670062
10630 IVPASS = IVPASS + 1                                               01680062
      WRITE (I02,80001) IVTNUM                                          01690062
      GO TO  641                                                        01700062
20630 IVFAIL = IVFAIL + 1                                               01710062
      RVCORR = 510.8                                                    01720062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01730062
  641 CONTINUE                                                          01740062
      IVTNUM =  64                                                      01750062
C                                                                       01760062
C      ****  TEST  64  ****                                             01770062
C                                                                       01780062
      IF (ICZERO) 30640,  640, 30640                                    01790062
  640 CONTINUE                                                          01800062
      RVCOMP = 7.5 - 500. - 3.3                                         01810062
      GO TO 40640                                                       01820062
30640 IVDELE = IVDELE + 1                                               01830062
      WRITE (I02,80003) IVTNUM                                          01840062
      IF (ICZERO) 40640,  651, 40640                                    01850062
40640 IF (RVCOMP + 495.85) 20640,10640,40641                            01860062
40641 IF (RVCOMP + 495.75) 10640,10640,20640                            01870062
10640 IVPASS = IVPASS + 1                                               01880062
      WRITE (I02,80001) IVTNUM                                          01890062
      GO TO  651                                                        01900062
20640 IVFAIL = IVFAIL + 1                                               01910062
      RVCORR = -495.8                                                   01920062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01930062
  651 CONTINUE                                                          01940062
      IVTNUM =  65                                                      01950062
C                                                                       01960062
C      ****  TEST  65  ****                                             01970062
C                                                                       01980062
      IF (ICZERO) 30650,  650, 30650                                    01990062
  650 CONTINUE                                                          02000062
      RVON01 = 7.5                                                      02010062
      RVON02 = 5E2                                                      02020062
      RVCOMP = RVON01 - (33E-1 - RVON02)                                02030062
      GO TO 40650                                                       02040062
30650 IVDELE = IVDELE + 1                                               02050062
      WRITE (I02,80003) IVTNUM                                          02060062
      IF (ICZERO) 40650,  661, 40650                                    02070062
40650 IF (RVCOMP - 504.15) 20650,10650,40651                            02080062
40651 IF (RVCOMP - 504.25) 10650,10650,20650                            02090062
10650 IVPASS = IVPASS + 1                                               02100062
      WRITE (I02,80001) IVTNUM                                          02110062
      GO TO  661                                                        02120062
20650 IVFAIL = IVFAIL + 1                                               02130062
      RVCORR = 504.2                                                    02140062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02150062
  661 CONTINUE                                                          02160062
      IVTNUM =  66                                                      02170062
C                                                                       02180062
C      ****  TEST  66  ****                                             02190062
C                                                                       02200062
      IF (ICZERO) 30660,  660, 30660                                    02210062
  660 CONTINUE                                                          02220062
      RVON01 = 7.5                                                      02230062
      RVCOMP = 5E2 * 33E-1 * RVON01                                     02240062
      GO TO 40660                                                       02250062
30660 IVDELE = IVDELE + 1                                               02260062
      WRITE (I02,80003) IVTNUM                                          02270062
      IF (ICZERO) 40660,  671, 40660                                    02280062
40660 IF (RVCOMP - 12370) 20660,10660,40661                             02290062
40661 IF (RVCOMP - 12380) 10660,10660,20660                             02300062
10660 IVPASS = IVPASS + 1                                               02310062
      WRITE (I02,80001) IVTNUM                                          02320062
      GO TO  671                                                        02330062
20660 IVFAIL = IVFAIL + 1                                               02340062
      RVCORR = 12375.                                                   02350062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02360062
  671 CONTINUE                                                          02370062
      IVTNUM =  67                                                      02380062
C                                                                       02390062
C      ****  TEST  67  ****                                             02400062
C                                                                       02410062
      IF (ICZERO) 30670,  670, 30670                                    02420062
  670 CONTINUE                                                          02430062
      RVON01 = 7.5                                                      02440062
      RVCOMP = 5E2 * (RVON01 * 33E-1)                                   02450062
      GO TO 40670                                                       02460062
30670 IVDELE = IVDELE + 1                                               02470062
      WRITE (I02,80003) IVTNUM                                          02480062
      IF (ICZERO) 40670,  681, 40670                                    02490062
40670 IF (RVCOMP - 12370) 20670,10670,40671                             02500062
40671 IF (RVCOMP - 12380) 10670,10670,20670                             02510062
10670 IVPASS = IVPASS + 1                                               02520062
      WRITE (I02,80001) IVTNUM                                          02530062
      GO TO  681                                                        02540062
20670 IVFAIL = IVFAIL + 1                                               02550062
      RVCORR = 12375.                                                   02560062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02570062
  681 CONTINUE                                                          02580062
      IVTNUM =  68                                                      02590062
C                                                                       02600062
C      ****  TEST  68  ****                                             02610062
C                                                                       02620062
      IF (ICZERO) 30680,  680, 30680                                    02630062
  680 CONTINUE                                                          02640062
      RVON01 = 7.5                                                      02650062
      RVON02 = 33E-1                                                    02660062
      RVON03 = -5E+2                                                    02670062
      RVCOMP = RVON01 / RVON02 / RVON03                                 02680062
      GO TO 40680                                                       02690062
30680 IVDELE = IVDELE + 1                                               02700062
      WRITE (I02,80003) IVTNUM                                          02710062
      IF (ICZERO) 40680,  691, 40680                                    02720062
40680 IF (RVCOMP + .00459) 20680,10680,40681                            02730062
40681 IF (RVCOMP + .00449) 10680,10680,20680                            02740062
10680 IVPASS = IVPASS + 1                                               02750062
      WRITE (I02,80001) IVTNUM                                          02760062
      GO TO  691                                                        02770062
20680 IVFAIL = IVFAIL + 1                                               02780062
      RVCORR = -.0045454                                                02790062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02800062
  691 CONTINUE                                                          02810062
      IVTNUM =  69                                                      02820062
C                                                                       02830062
C      ****  TEST  69  ****                                             02840062
C                                                                       02850062
      IF (ICZERO) 30690,  690, 30690                                    02860062
  690 CONTINUE                                                          02870062
      RVON01 = 7.5                                                      02880062
      RVON02 = 33E-1                                                    02890062
      RVON03 = -5E+2                                                    02900062
      RVCOMP = RVON01 / (RVON02 / RVON03)                               02910062
      GO TO 40690                                                       02920062
30690 IVDELE = IVDELE + 1                                               02930062
      WRITE (I02,80003) IVTNUM                                          02940062
      IF (ICZERO) 40690,  701, 40690                                    02950062
40690 IF (RVCOMP + 1180.) 20690,10690,40691                             02960062
40691 IF (RVCOMP + 1080.) 10690,10690,20690                             02970062
10690 IVPASS = IVPASS + 1                                               02980062
      WRITE (I02,80001) IVTNUM                                          02990062
      GO TO  701                                                        03000062
20690 IVFAIL = IVFAIL + 1                                               03010062
      RVCORR = -1136.4                                                  03020062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03030062
  701 CONTINUE                                                          03040062
      IVTNUM =  70                                                      03050062
C                                                                       03060062
C      ****  TEST  70  ****                                             03070062
C                                                                       03080062
      IF (ICZERO) 30700,  700, 30700                                    03090062
  700 CONTINUE                                                          03100062
      RVON01 = 3.835E3                                                  03110062
      IVON01 =  5                                                       03120062
      RVCOMP = RVON01 ** IVON01                                         03130062
      GO TO 40700                                                       03140062
30700 IVDELE = IVDELE + 1                                               03150062
      WRITE (I02,80003) IVTNUM                                          03160062
      IF (ICZERO) 40700,  711, 40700                                    03170062
40700 IF (RVCOMP - 8.29E17) 20700,10700,40701                           03180062
40701 IF (RVCOMP - 8.30E17) 10700,10700,20700                           03190062
10700 IVPASS = IVPASS + 1                                               03200062
      WRITE (I02,80001) IVTNUM                                          03210062
      GO TO  711                                                        03220062
20700 IVFAIL = IVFAIL + 1                                               03230062
      RVCORR = 8.295E17                                                 03240062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03250062
  711 CONTINUE                                                          03260062
C                                                                       03270062
C     TESTS 71 THROUGH 74 TEST  RV1 + RV2 <OP2> RV3                     03280062
C                                                                       03290062
      IVTNUM =  71                                                      03300062
C                                                                       03310062
C      ****  TEST  71  ****                                             03320062
C                                                                       03330062
      IF (ICZERO) 30710,  710, 30710                                    03340062
  710 CONTINUE                                                          03350062
      RVON01 = 524.87                                                   03360062
      RVON02 = 3.35                                                     03370062
      RVON03 = .005679                                                  03380062
      RVCOMP = RVON01 + RVON02 - RVON03                                 03390062
      GO TO 40710                                                       03400062
30710 IVDELE = IVDELE + 1                                               03410062
      WRITE (I02,80003) IVTNUM                                          03420062
      IF (ICZERO) 40710,  721, 40710                                    03430062
40710 IF (RVCOMP - 528.16) 20710,10710,40711                            03440062
40711 IF (RVCOMP - 528.26) 10710,10710,20710                            03450062
10710 IVPASS = IVPASS + 1                                               03460062
      WRITE (I02,80001) IVTNUM                                          03470062
      GO TO  721                                                        03480062
20710 IVFAIL = IVFAIL + 1                                               03490062
      RVCORR = 528.21                                                   03500062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03510062
  721 CONTINUE                                                          03520062
      IVTNUM =  72                                                      03530062
C                                                                       03540062
C      ****  TEST  72  ****                                             03550062
C                                                                       03560062
      IF (ICZERO) 30720,  720, 30720                                    03570062
  720 CONTINUE                                                          03580062
      RVON01 = 524.87                                                   03590062
      RVON02 = 3.35                                                     03600062
      RVON03 = .005679                                                  03610062
      RVCOMP = RVON01 + RVON02 * RVON03                                 03620062
      GO TO 40720                                                       03630062
30720 IVDELE = IVDELE + 1                                               03640062
      WRITE (I02,80003) IVTNUM                                          03650062
      IF (ICZERO) 40720,  731, 40720                                    03660062
40720 IF (RVCOMP - 524.84) 20720,10720,40721                            03670062
40721 IF (RVCOMP - 524.94) 10720,10720,20720                            03680062
10720 IVPASS = IVPASS + 1                                               03690062
      WRITE (I02,80001) IVTNUM                                          03700062
      GO TO  731                                                        03710062
20720 IVFAIL = IVFAIL + 1                                               03720062
      RVCORR = 524.89                                                   03730062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03740062
  731 CONTINUE                                                          03750062
      IVTNUM =  73                                                      03760062
C                                                                       03770062
C      ****  TEST  73  ****                                             03780062
C                                                                       03790062
      IF (ICZERO) 30730,  730, 30730                                    03800062
  730 CONTINUE                                                          03810062
      RVON01 = 524.87                                                   03820062
      RVON02 = 3.35                                                     03830062
      RVON03 = .005679                                                  03840062
      RVCOMP = RVON01 + RVON02 / RVON03                                 03850062
      GO TO 40730                                                       03860062
30730 IVDELE = IVDELE + 1                                               03870062
      WRITE (I02,80003) IVTNUM                                          03880062
      IF (ICZERO) 40730,  741, 40730                                    03890062
40730 IF (RVCOMP - 1114.2) 20730,10730,40731                            03900062
40731 IF (RVCOMP - 1115.2) 10730,10730,20730                            03910062
10730 IVPASS = IVPASS + 1                                               03920062
      WRITE (I02,80001) IVTNUM                                          03930062
      GO TO  741                                                        03940062
20730 IVFAIL = IVFAIL + 1                                               03950062
      RVCORR = 1114.8                                                   03960062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03970062
  741 CONTINUE                                                          03980062
      IVTNUM =  74                                                      03990062
C                                                                       04000062
C      ****  TEST  74  ****                                             04010062
C                                                                       04020062
      IF (ICZERO) 30740,  740, 30740                                    04030062
  740 CONTINUE                                                          04040062
      RVON01 = 524.87                                                   04050062
      RVON02 = 3.35                                                     04060062
      IVON01 = 7                                                        04070062
      RVCOMP = RVON01 + RVON02 ** IVON01                                04080062
      GO TO 40740                                                       04090062
30740 IVDELE = IVDELE + 1                                               04100062
      WRITE (I02,80003) IVTNUM                                          04110062
      IF (ICZERO) 40740,  751, 40740                                    04120062
40740 IF (RVCOMP - 5259.3) 20740,10740,40741                            04130062
40741 IF (RVCOMP - 5260.3) 10740,10740,20740                            04140062
10740 IVPASS = IVPASS + 1                                               04150062
      WRITE (I02,80001) IVTNUM                                          04160062
      GO TO  751                                                        04170062
20740 IVFAIL = IVFAIL + 1                                               04180062
      RVCORR = 5259.8                                                   04190062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04200062
  751 CONTINUE                                                          04210062
C                                                                       04220062
C     TESTS 75 THROUGH 78 CHECK     RV1 - RV2 <OP2> RV3                 04230062
C                                                                       04240062
      IVTNUM =  75                                                      04250062
C                                                                       04260062
C      ****  TEST  75  ****                                             04270062
C                                                                       04280062
      IF (ICZERO) 30750,  750, 30750                                    04290062
  750 CONTINUE                                                          04300062
      RVON01 = 524.87                                                   04310062
      RVON02 = 3.35                                                     04320062
      RVON03 = .5679                                                    04330062
      RVCOMP = RVON01 - RVON02 + RVON03                                 04340062
      GO TO 40750                                                       04350062
30750 IVDELE = IVDELE + 1                                               04360062
      WRITE (I02,80003) IVTNUM                                          04370062
      IF (ICZERO) 40750,  761, 40750                                    04380062
40750 IF (RVCOMP - 522.03) 20750,10750,40751                            04390062
40751 IF (RVCOMP - 522.13) 10750,10750,20750                            04400062
10750 IVPASS = IVPASS + 1                                               04410062
      WRITE (I02,80001) IVTNUM                                          04420062
      GO TO  761                                                        04430062
20750 IVFAIL = IVFAIL + 1                                               04440062
      RVCORR = 522.09                                                   04450062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04460062
  761 CONTINUE                                                          04470062
      IVTNUM =  76                                                      04480062
C                                                                       04490062
C      ****  TEST  76  ****                                             04500062
C                                                                       04510062
      IF (ICZERO) 30760,  760, 30760                                    04520062
  760 CONTINUE                                                          04530062
      RVON01 = 524.87                                                   04540062
      RVON02 =   3.35                                                   04550062
      RVON03 =    .5679                                                 04560062
      RVCOMP = RVON01 - RVON02 * RVON03                                 04570062
      GO TO 40760                                                       04580062
30760 IVDELE = IVDELE + 1                                               04590062
      WRITE (I02,80003) IVTNUM                                          04600062
      IF (ICZERO) 40760,  771, 40760                                    04610062
40760 IF (RVCOMP - 522.92) 20760,10760,40761                            04620062
40761 IF (RVCOMP - 523.02) 10760,10760,20760                            04630062
10760 IVPASS = IVPASS + 1                                               04640062
      WRITE (I02,80001) IVTNUM                                          04650062
      GO TO  771                                                        04660062
20760 IVFAIL = IVFAIL + 1                                               04670062
      RVCORR = 522.97                                                   04680062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04690062
  771 CONTINUE                                                          04700062
      IVTNUM =  77                                                      04710062
C                                                                       04720062
C      ****  TEST  77  ****                                             04730062
C                                                                       04740062
      IF (ICZERO) 30770,  770, 30770                                    04750062
  770 CONTINUE                                                          04760062
      RVON01 = 524.87                                                   04770062
      RVON02 =   3.35                                                   04780062
      RVON03 =    .5679                                                 04790062
      RVCOMP = RVON01 - RVON02 / RVON03                                 04800062
      GO TO 40770                                                       04810062
30770 IVDELE = IVDELE + 1                                               04820062
      WRITE (I02,80003) IVTNUM                                          04830062
      IF (ICZERO) 40770,  781, 40770                                    04840062
40770 IF (RVCOMP - 518.92) 20770,10770,40771                            04850062
40771 IF (RVCOMP - 519.02) 10770,10770,20770                            04860062
10770 IVPASS = IVPASS + 1                                               04870062
      WRITE (I02,80001) IVTNUM                                          04880062
      GO TO  781                                                        04890062
20770 IVFAIL = IVFAIL + 1                                               04900062
      RVCORR = 518.97                                                   04910062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04920062
  781 CONTINUE                                                          04930062
      IVTNUM =  78                                                      04940062
C                                                                       04950062
C      ****  TEST  78  ****                                             04960062
C                                                                       04970062
      IF (ICZERO) 30780,  780, 30780                                    04980062
  780 CONTINUE                                                          04990062
      RVON01 = 524.87                                                   05000062
      RVON02 =   3.35                                                   05010062
      IVON01 =   7                                                      05020062
      RVCOMP = RVON01 - RVON02 ** IVON01                                05030062
      GO TO 40780                                                       05040062
30780 IVDELE = IVDELE + 1                                               05050062
      WRITE (I02,80003) IVTNUM                                          05060062
      IF (ICZERO) 40780,  791, 40780                                    05070062
40780 IF (RVCOMP + 4210.6) 20780,10780,40781                            05080062
40781 IF (RVCOMP + 4209.6) 10780,10780,20780                            05090062
10780 IVPASS = IVPASS + 1                                               05100062
      WRITE (I02,80001) IVTNUM                                          05110062
      GO TO  791                                                        05120062
20780 IVFAIL = IVFAIL + 1                                               05130062
      RVCORR = -4210.1                                                  05140062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05150062
  791 CONTINUE                                                          05160062
C                                                                       05170062
C     TESTS 79 THROUGH 82 CHECK     RV1 * RV2 <OP2> RV3                 05180062
C                                                                       05190062
      IVTNUM =  79                                                      05200062
C                                                                       05210062
C      ****  TEST  79  ****                                             05220062
C                                                                       05230062
      IF (ICZERO) 30790,  790, 30790                                    05240062
  790 CONTINUE                                                          05250062
      RVON01 = 524.87                                                   05260062
      RVON02 =   .5679                                                  05270062
      RVON03 =   3.35                                                   05280062
      RVCOMP = RVON01 * RVON02 + RVON03                                 05290062
      GO TO 40790                                                       05300062
30790 IVDELE = IVDELE + 1                                               05310062
      WRITE (I02,80003) IVTNUM                                          05320062
      IF (ICZERO) 40790,  801, 40790                                    05330062
40790 IF (RVCOMP - 301.37) 20790,10790,40791                            05340062
40791 IF (RVCOMP - 301.47) 10790,10790,20790                            05350062
10790 IVPASS = IVPASS + 1                                               05360062
      WRITE (I02,80001) IVTNUM                                          05370062
      GO TO  801                                                        05380062
20790 IVFAIL = IVFAIL + 1                                               05390062
      RVCORR = 301.42                                                   05400062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05410062
  801 CONTINUE                                                          05420062
      IVTNUM =  80                                                      05430062
C                                                                       05440062
C      ****  TEST  80  ****                                             05450062
C                                                                       05460062
      IF (ICZERO) 30800,  800, 30800                                    05470062
  800 CONTINUE                                                          05480062
      RVON01 = 524.87                                                   05490062
      RVON02 =    .5679                                                 05500062
      RVON03 =   3.35                                                   05510062
      RVCOMP = RVON01 * RVON02 - RVON03                                 05520062
      GO TO 40800                                                       05530062
30800 IVDELE = IVDELE + 1                                               05540062
      WRITE (I02,80003) IVTNUM                                          05550062
      IF (ICZERO) 40800,  811, 40800                                    05560062
40800 IF (RVCOMP - 294.67) 20800,10800,40801                            05570062
40801 IF (RVCOMP - 294.77) 10800,10800,20800                            05580062
10800 IVPASS = IVPASS + 1                                               05590062
      WRITE (I02,80001) IVTNUM                                          05600062
      GO TO  811                                                        05610062
20800 IVFAIL = IVFAIL + 1                                               05620062
      RVCORR = 294.72                                                   05630062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05640062
  811 CONTINUE                                                          05650062
      IVTNUM =  81                                                      05660062
C                                                                       05670062
C      ****  TEST  81  ****                                             05680062
C                                                                       05690062
      IF (ICZERO) 30810,  810, 30810                                    05700062
  810 CONTINUE                                                          05710062
      RVON01 = 524.87                                                   05720062
      RVON02 =    .5679                                                 05730062
      RVON03 =   3.35                                                   05740062
      RVCOMP = RVON01 * RVON02 / RVON03                                 05750062
      GO TO 40810                                                       05760062
30810 IVDELE = IVDELE + 1                                               05770062
      WRITE (I02,80003) IVTNUM                                          05780062
      IF (ICZERO) 40810,  821, 40810                                    05790062
40810 IF (RVCOMP - 88.92) 20810,10810,40811                             05800062
40811 IF (RVCOMP - 89.02) 10810,10810,20810                             05810062
10810 IVPASS = IVPASS + 1                                               05820062
      WRITE (I02,80001) IVTNUM                                          05830062
      GO TO  821                                                        05840062
20810 IVFAIL = IVFAIL + 1                                               05850062
      RVCORR = 88.977                                                   05860062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05870062
  821 CONTINUE                                                          05880062
      IVTNUM =  82                                                      05890062
C                                                                       05900062
C      ****  TEST  82  ****                                             05910062
C                                                                       05920062
      IF (ICZERO) 30820,  820, 30820                                    05930062
  820 CONTINUE                                                          05940062
      RVON01 = 524.87                                                   05950062
      RVON02 =    .5679                                                 05960062
      IVON01 =   7                                                      05970062
      RVCOMP = RVON01 * RVON02 ** IVON01                                05980062
      GO TO 40820                                                       05990062
30820 IVDELE = IVDELE + 1                                               06000062
      WRITE (I02,80003) IVTNUM                                          06010062
      IF (ICZERO) 40820,  831, 40820                                    06020062
40820 IF (RVCOMP -  9.94) 20820,10820,40821                             06030062
40821 IF (RVCOMP - 10.04) 10820,10820,20820                             06040062
10820 IVPASS = IVPASS + 1                                               06050062
      WRITE (I02,80001) IVTNUM                                          06060062
      GO TO  831                                                        06070062
20820 IVFAIL = IVFAIL + 1                                               06080062
      RVCORR = 9.999                                                    06090062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06100062
  831 CONTINUE                                                          06110062
C                                                                       06120062
C     TESTS 83 THROUGH 86 CHECK     RV1 / RV2 <OP2> RV3                 06130062
C                                                                       06140062
      IVTNUM =  83                                                      06150062
C                                                                       06160062
C      ****  TEST  83  ****                                             06170062
C                                                                       06180062
      IF (ICZERO) 30830,  830, 30830                                    06190062
  830 CONTINUE                                                          06200062
      RVON01 = 524.87                                                   06210062
      RVON02 =   3.35                                                   06220062
      RVON03 =    .5679                                                 06230062
      RVCOMP = RVON01 / RVON02 + RVON03                                 06240062
      GO TO 40830                                                       06250062
30830 IVDELE = IVDELE + 1                                               06260062
      WRITE (I02,80003) IVTNUM                                          06270062
      IF (ICZERO) 40830,  841, 40830                                    06280062
40830 IF (RVCOMP - 157.19) 20830,10830,40831                            06290062
40831 IF (RVCOMP - 157.29) 10830,10830,20830                            06300062
10830 IVPASS = IVPASS + 1                                               06310062
      WRITE (I02,80001) IVTNUM                                          06320062
      GO TO  841                                                        06330062
20830 IVFAIL = IVFAIL + 1                                               06340062
      RVCORR = 157.25                                                   06350062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06360062
  841 CONTINUE                                                          06370062
      IVTNUM =  84                                                      06380062
C                                                                       06390062
C      ****  TEST  84  ****                                             06400062
C                                                                       06410062
      IF (ICZERO) 30840,  840, 30840                                    06420062
  840 CONTINUE                                                          06430062
      RVON01 = 524.87                                                   06440062
      RVON02 =   3.35                                                   06450062
      RVON03 =    .8507                                                 06460062
      RVCOMP = RVON01 / RVON02 - RVON03                                 06470062
      GO TO 40840                                                       06480062
30840 IVDELE = IVDELE + 1                                               06490062
      WRITE (I02,80003) IVTNUM                                          06500062
      IF (ICZERO) 40840,  851, 40840                                    06510062
40840 IF (RVCOMP - 155.77) 20840,10840,40841                            06520062
40841 IF (RVCOMP - 155.87) 10840,10840,20840                            06530062
10840 IVPASS = IVPASS + 1                                               06540062
      WRITE (I02,80001) IVTNUM                                          06550062
      GO TO  851                                                        06560062
20840 IVFAIL = IVFAIL + 1                                               06570062
      RVCORR = 155.83                                                   06580062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06590062
  851 CONTINUE                                                          06600062
      IVTNUM =  85                                                      06610062
C                                                                       06620062
C      ****  TEST  85  ****                                             06630062
C                                                                       06640062
      IF (ICZERO) 30850,  850, 30850                                    06650062
  850 CONTINUE                                                          06660062
      RVON01 = 524.87                                                   06670062
      RVON02 =   3.35                                                   06680062
      RVON03 =    .8507                                                 06690062
      RVCOMP = RVON01 / RVON02 * RVON03                                 06700062
      GO TO 40850                                                       06710062
30850 IVDELE = IVDELE + 1                                               06720062
      WRITE (I02,80003) IVTNUM                                          06730062
      IF (ICZERO) 40850,  861, 40850                                    06740062
40850 IF (RVCOMP - 132.7) 20850,10850,40851                             06750062
40851 IF (RVCOMP - 133.7) 10850,10850,20850                             06760062
10850 IVPASS = IVPASS + 1                                               06770062
      WRITE (I02,80001) IVTNUM                                          06780062
      GO TO  861                                                        06790062
20850 IVFAIL = IVFAIL + 1                                               06800062
      RVCORR = 133.29                                                   06810062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06820062
  861 CONTINUE                                                          06830062
      IVTNUM =  86                                                      06840062
C                                                                       06850062
C      ****  TEST  86  ****                                             06860062
C                                                                       06870062
      IF (ICZERO) 30860,  860, 30860                                    06880062
  860 CONTINUE                                                          06890062
      RVON01 = 524.87                                                   06900062
      RVON02 =   3.35                                                   06910062
      IVON01 =   7                                                      06920062
      RVCOMP = RVON01 / RVON02 ** IVON01                                06930062
      GO TO 40860                                                       06940062
30860 IVDELE = IVDELE + 1                                               06950062
      WRITE (I02,80003) IVTNUM                                          06960062
      IF (ICZERO) 40860,  871, 40860                                    06970062
40860 IF (RVCOMP - .106) 20860,10860,40861                              06980062
40861 IF (RVCOMP - .116) 10860,10860,20860                              06990062
10860 IVPASS = IVPASS + 1                                               07000062
      WRITE (I02,80001) IVTNUM                                          07010062
      GO TO  871                                                        07020062
20860 IVFAIL = IVFAIL + 1                                               07030062
      RVCORR = .11085                                                   07040062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07050062
  871 CONTINUE                                                          07060062
C                                                                       07070062
C     TESTS 87 THROUGH 90 CHECK     RV1 ** IV1 <OP2> RV2                07080062
C                                                                       07090062
      IVTNUM =  87                                                      07100062
C                                                                       07110062
C      ****  TEST  87  ****                                             07120062
C                                                                       07130062
      IF (ICZERO) 30870,  870, 30870                                    07140062
  870 CONTINUE                                                          07150062
      RVON01 =   3.35                                                   07160062
      IVON01 =   7                                                      07170062
      RVON02 = 524.87                                                   07180062
      RVCOMP = RVON01 ** IVON01 + RVON02                                07190062
      GO TO 40870                                                       07200062
30870 IVDELE = IVDELE + 1                                               07210062
      WRITE (I02,80003) IVTNUM                                          07220062
      IF (ICZERO) 40870,  881, 40870                                    07230062
40870 IF (RVCOMP - 5210.) 20870,10870,40871                             07240062
40871 IF (RVCOMP - 5310.) 10870,10870,20870                             07250062
10870 IVPASS = IVPASS + 1                                               07260062
      WRITE (I02,80001) IVTNUM                                          07270062
      GO TO  881                                                        07280062
20870 IVFAIL = IVFAIL + 1                                               07290062
      RVCORR = 5259.8                                                   07300062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07310062
  881 CONTINUE                                                          07320062
      IVTNUM =  88                                                      07330062
C                                                                       07340062
C      ****  TEST  88  ****                                             07350062
C                                                                       07360062
      IF (ICZERO) 30880,  880, 30880                                    07370062
  880 CONTINUE                                                          07380062
      RVON01 =   3.35                                                   07390062
      IVON01 =   7                                                      07400062
      RVON02 = 524.87                                                   07410062
      RVCOMP = RVON01 ** IVON01 - RVON02                                07420062
      GO TO 40880                                                       07430062
30880 IVDELE = IVDELE + 1                                               07440062
      WRITE (I02,80003) IVTNUM                                          07450062
      IF (ICZERO) 40880,  891, 40880                                    07460062
40880 IF (RVCOMP - 4160.) 20880,10880,40881                             07470062
40881 IF (RVCOMP - 4260.) 10880,10880,20880                             07480062
10880 IVPASS = IVPASS + 1                                               07490062
      WRITE (I02,80001) IVTNUM                                          07500062
      GO TO  891                                                        07510062
20880 IVFAIL = IVFAIL + 1                                               07520062
      RVCORR = 4210.1                                                   07530062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07540062
  891 CONTINUE                                                          07550062
      IVTNUM =  89                                                      07560062
C                                                                       07570062
C      ****  TEST  89  ****                                             07580062
C                                                                       07590062
      IF (ICZERO) 30890,  890, 30890                                    07600062
  890 CONTINUE                                                          07610062
      RVON01 =   3.35                                                   07620062
      IVON01 =   7                                                      07630062
      RVON02 = 524.87                                                   07640062
      RVCOMP = RVON01 ** IVON01 * RVON02                                07650062
      GO TO 40890                                                       07660062
30890 IVDELE = IVDELE + 1                                               07670062
      WRITE (I02,80003) IVTNUM                                          07680062
      IF (ICZERO) 40890,  901, 40890                                    07690062
40890 IF (RVCOMP - 2.43E6) 20890,10890,40891                            07700062
40891 IF (RVCOMP - 2.53E6) 10890,10890,20890                            07710062
10890 IVPASS = IVPASS + 1                                               07720062
      WRITE (I02,80001) IVTNUM                                          07730062
      GO TO  901                                                        07740062
20890 IVFAIL = IVFAIL + 1                                               07750062
      RVCORR = 2.4852E6                                                 07760062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07770062
  901 CONTINUE                                                          07780062
      IVTNUM =  90                                                      07790062
C                                                                       07800062
C      ****  TEST  90  ****                                             07810062
C                                                                       07820062
      IF (ICZERO) 30900,  900, 30900                                    07830062
  900 CONTINUE                                                          07840062
      RVON01 =   3.35                                                   07850062
      IVON01 =   7                                                      07860062
      RVON02 = 524.87                                                   07870062
      RVCOMP = RVON01 ** IVON01 / RVON02                                07880062
      GO TO 40900                                                       07890062
30900 IVDELE = IVDELE + 1                                               07900062
      WRITE (I02,80003) IVTNUM                                          07910062
      IF (ICZERO) 40900,  911, 40900                                    07920062
40900 IF (RVCOMP - 8.97) 20900,10900,40901                              07930062
40901 IF (RVCOMP - 9.07) 10900,10900,20900                              07940062
10900 IVPASS = IVPASS + 1                                               07950062
      WRITE (I02,80001) IVTNUM                                          07960062
      GO TO  911                                                        07970062
20900 IVFAIL = IVFAIL + 1                                               07980062
      RVCORR = 9.0211                                                   07990062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          08000062
  911 CONTINUE                                                          08010062
C                                                                       08020062
C     TESTS 91 AND 92 CHECK ALL ARITHMETIC OPERATORS USED TOGETHER      08030062
C                                                                       08040062
      IVTNUM =  91                                                      08050062
C                                                                       08060062
C      ****  TEST  91  ****                                             08070062
C                                                                       08080062
      IF (ICZERO) 30910,  910, 30910                                    08090062
  910 CONTINUE                                                          08100062
      RVON01 = 780.56                                                   08110062
      RVON02 =    .803                                                  08120062
      RVON03 =   3.35                                                   08130062
      IVON01 =   7                                                      08140062
      RVON04 =  20.07                                                   08150062
      RVON05 = 511.9                                                    08160062
      RVCOMP = - RVON01 + RVON02 * RVON03 ** IVON01 / RVON04 - RVON05   08170062
      GO TO 40910                                                       08180062
30910 IVDELE = IVDELE + 1                                               08190062
      WRITE (I02,80003) IVTNUM                                          08200062
      IF (ICZERO) 40910,  921, 40910                                    08210062
40910 IF (RVCOMP + 1113.0) 20910,10910,40911                            08220062
40911 IF (RVCOMP + 1093.0) 10910,10910,20910                            08230062
10910 IVPASS = IVPASS + 1                                               08240062
      WRITE (I02,80001) IVTNUM                                          08250062
      GO TO  921                                                        08260062
20910 IVFAIL = IVFAIL + 1                                               08270062
      RVCORR = -1103.0                                                  08280062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          08290062
  921 CONTINUE                                                          08300062
      IVTNUM =  92                                                      08310062
C                                                                       08320062
C      ****  TEST  92  ****                                             08330062
C                                                                       08340062
      IF (ICZERO) 30920,  920, 30920                                    08350062
  920 CONTINUE                                                          08360062
      RVON01 = 780.56                                                   08370062
      RVON02 =    .803                                                  08380062
      RVON03 =   3.35                                                   08390062
      IVON01 =   7                                                      08400062
      RVON04 =  20.07                                                   08410062
      RVON05 = 511.9                                                    08420062
      RVCOMP = (-RVON01) + (RVON02 * RVON03) ** IVON01 / (RVON04-RVON05)08430062
      GO TO 40920                                                       08440062
30920 IVDELE = IVDELE + 1                                               08450062
      WRITE (I02,80003) IVTNUM                                          08460062
      IF (ICZERO) 40920,  931, 40920                                    08470062
40920 IF (RVCOMP + 788.) 20920,10920,40921                              08480062
40921 IF (RVCOMP + 777.) 10920,10920,20920                              08490062
10920 IVPASS = IVPASS + 1                                               08500062
      WRITE (I02,80001) IVTNUM                                          08510062
      GO TO  931                                                        08520062
20920 IVFAIL = IVFAIL + 1                                               08530062
      RVCORR = -782.63                                                  08540062
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          08550062
  931 CONTINUE                                                          08560062
C                                                                       08570062
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08580062
99999 CONTINUE                                                          08590062
      WRITE (I02,90002)                                                 08600062
      WRITE (I02,90006)                                                 08610062
      WRITE (I02,90002)                                                 08620062
      WRITE (I02,90002)                                                 08630062
      WRITE (I02,90007)                                                 08640062
      WRITE (I02,90002)                                                 08650062
      WRITE (I02,90008)  IVFAIL                                         08660062
      WRITE (I02,90009) IVPASS                                          08670062
      WRITE (I02,90010) IVDELE                                          08680062
C                                                                       08690062
C                                                                       08700062
C     TERMINATE ROUTINE EXECUTION                                       08710062
      STOP                                                              08720062
C                                                                       08730062
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08740062
90000 FORMAT (1H1)                                                      08750062
90002 FORMAT (1H )                                                      08760062
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08770062
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08780062
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08790062
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08800062
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08810062
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08820062
C                                                                       08830062
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08840062
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08850062
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08860062
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08870062
C                                                                       08880062
C     FORMAT STATEMENTS FOR TEST RESULTS                                08890062
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08900062
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08910062
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08920062
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08930062
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08940062
C                                                                       08950062
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM062)                          08960062
      END                                                               08970062
