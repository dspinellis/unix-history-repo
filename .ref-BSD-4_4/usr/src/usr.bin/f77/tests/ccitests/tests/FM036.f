C     COMMENT SECTION                                                   00010036
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020036
C     FM036                                                             00030036
C                                                                       00040036
C         THIS ROUTINE TESTS ARITHMETIC ASIGNMENT STATEMENTS OF THE     00050036
C     FORM                                                              00060036
C              INTEGER VARIABLE = ARITHMETIC EXPRESSION                 00070036
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080036
C     OPERATOR / AND INTEGER CONSTANTS.  BOTH POSITIVE AND NEGATIVE     00090036
C     CONSTANTS ARE USED IN THE ARITHMETIC EXPRESSION.                  00100036
C                                                                       00110036
C         THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     00120036
C     AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  00130036
C     IN THE RESULTANT INTEGER VARIABLE.  THE STANDARD STATES 'THE VALUE00140036
C     OF AN INTEGER FACTOR OR TERM IS THE NEAREST INTEGER WHOSE         00150036
C     MAGNITUDE DOES NOT EXCEED THE MAGNITUDE OF THE MATHEMATICAL VALUE 00160036
C     REPRESENTED BY THAT FACTOR OR TERM.'                              00170036
C                                                                       00180036
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00190036
C             (1)  INTEGER CONSTANT/INTEGER CONSTANT                    00200036
C                      NO TRUNCATION REQUIRED,                          00210036
C             (2)  INTEGER CONSTANT/INTEGER CONSTANT                    00220036
C                      TRUNCATION REQUIRED.                             00230036
C                                                                       00240036
C      REFERENCES                                                       00250036
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00260036
C              X3.9-1978                                                00270036
C                                                                       00280036
C        SECTION 4.3, INTEGER TYPE                                      00290036
C        SECTION 4.3.1, INTEGER CONSTANT                                00300036
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00310036
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00320036
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00330036
C                                                                       00340036
C      **********************************************************       00350036
C                                                                       00360036
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00370036
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00380036
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00390036
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00400036
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00410036
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00420036
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00430036
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00440036
C     OF EXECUTING THESE TESTS.                                         00450036
C                                                                       00460036
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00470036
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00480036
C                                                                       00490036
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00500036
C                                                                       00510036
C                  DEPARTMENT OF THE NAVY                               00520036
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00530036
C                  WASHINGTON, D.C.  20376                              00540036
C                                                                       00550036
C      **********************************************************       00560036
C                                                                       00570036
C                                                                       00580036
C                                                                       00590036
C     INITIALIZATION SECTION                                            00600036
C                                                                       00610036
C     INITIALIZE CONSTANTS                                              00620036
C      **************                                                   00630036
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640036
      I01 = 5                                                           00650036
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660036
      I02 = 6                                                           00670036
C     SYSTEM ENVIRONMENT SECTION                                        00680036
C                                                                       00690036
      I01 = 5                                                           00700036
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710036
C     (UNIT NUMBER FOR CARD READER).                                    00720036
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00730036
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00740036
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00750036
C                                                                       00760036
      I02 = 6                                                           00770036
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00780036
C     (UNIT NUMBER FOR PRINTER).                                        00790036
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00800036
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00810036
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00820036
C                                                                       00830036
      IVPASS=0                                                          00840036
      IVFAIL=0                                                          00850036
      IVDELE=0                                                          00860036
      ICZERO=0                                                          00870036
C                                                                       00880036
C     WRITE PAGE HEADERS                                                00890036
      WRITE (I02,90000)                                                 00900036
      WRITE (I02,90001)                                                 00910036
      WRITE (I02,90002)                                                 00920036
      WRITE (I02, 90002)                                                00930036
      WRITE (I02,90003)                                                 00940036
      WRITE (I02,90002)                                                 00950036
      WRITE (I02,90004)                                                 00960036
      WRITE (I02,90002)                                                 00970036
      WRITE (I02,90011)                                                 00980036
      WRITE (I02,90002)                                                 00990036
      WRITE (I02,90002)                                                 01000036
      WRITE (I02,90005)                                                 01010036
      WRITE (I02,90006)                                                 01020036
      WRITE (I02,90002)                                                 01030036
C                                                                       01040036
C     TEST SECTION                                                      01050036
C         ARITHMETIC ASSIGNMENT STATEMENT                               01060036
C                                                                       01070036
C     TEST 462 THROUGH TEST 490 CONTAIN TWO INTEGER CONSTANTS AND       01080036
C     OPERATOR / IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS       01090036
C            INTEGER VARIABLE = INTEGER CONSTANT/INTEGER CONSTANT       01100036
C                                                                       01110036
C     TEST 462 THROUGH TEST 469 - POSITIVE CONSTANTS                    01120036
C              NO TRUNCATION REQUIRED                                   01130036
C                                                                       01140036
 4621 CONTINUE                                                          01150036
      IVTNUM = 462                                                      01160036
C                                                                       01170036
C      ****  TEST 462  ****                                             01180036
C                                                                       01190036
      IF (ICZERO) 34620, 4620, 34620                                    01200036
 4620 CONTINUE                                                          01210036
      IVCOMP = 4/2                                                      01220036
      GO TO 44620                                                       01230036
34620 IVDELE = IVDELE + 1                                               01240036
      WRITE (I02,80003) IVTNUM                                          01250036
      IF (ICZERO) 44620, 4631, 44620                                    01260036
44620 IF (IVCOMP - 2) 24620,14620,24620                                 01270036
14620 IVPASS = IVPASS + 1                                               01280036
      WRITE (I02,80001) IVTNUM                                          01290036
      GO TO 4631                                                        01300036
24620 IVFAIL = IVFAIL + 1                                               01310036
      IVCORR = 2                                                        01320036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01330036
 4631 CONTINUE                                                          01340036
      IVTNUM = 463                                                      01350036
C                                                                       01360036
C      ****  TEST 463  ****                                             01370036
C                                                                       01380036
      IF (ICZERO) 34630, 4630, 34630                                    01390036
 4630 CONTINUE                                                          01400036
      IVCOMP = 75 / 25                                                  01410036
      GO TO 44630                                                       01420036
34630 IVDELE = IVDELE + 1                                               01430036
      WRITE (I02,80003) IVTNUM                                          01440036
      IF (ICZERO) 44630, 4641, 44630                                    01450036
44630 IF (IVCOMP - 3) 24630,14630,24630                                 01460036
14630 IVPASS = IVPASS + 1                                               01470036
      WRITE (I02,80001) IVTNUM                                          01480036
      GO TO 4641                                                        01490036
24630 IVFAIL = IVFAIL + 1                                               01500036
      IVCORR = 3                                                        01510036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01520036
 4641 CONTINUE                                                          01530036
      IVTNUM = 464                                                      01540036
C                                                                       01550036
C      ****  TEST 464  ****                                             01560036
C                                                                       01570036
      IF (ICZERO) 34640, 4640, 34640                                    01580036
 4640 CONTINUE                                                          01590036
      IVCOMP = 3575/143                                                 01600036
      GO TO 44640                                                       01610036
34640 IVDELE = IVDELE + 1                                               01620036
      WRITE (I02,80003) IVTNUM                                          01630036
      IF (ICZERO) 44640, 4651, 44640                                    01640036
44640 IF (IVCOMP - 25) 24640,14640,24640                                01650036
14640 IVPASS = IVPASS + 1                                               01660036
      WRITE (I02,80001) IVTNUM                                          01670036
      GO TO 4651                                                        01680036
24640 IVFAIL = IVFAIL + 1                                               01690036
      IVCORR = 25                                                       01700036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01710036
 4651 CONTINUE                                                          01720036
      IVTNUM = 465                                                      01730036
C                                                                       01740036
C      ****  TEST 465  ****                                             01750036
C                                                                       01760036
      IF (ICZERO) 34650, 4650, 34650                                    01770036
 4650 CONTINUE                                                          01780036
      IVCOMP = 3575/25                                                  01790036
      GO TO 44650                                                       01800036
34650 IVDELE = IVDELE + 1                                               01810036
      WRITE (I02,80003) IVTNUM                                          01820036
      IF (ICZERO) 44650, 4661, 44650                                    01830036
44650 IF (IVCOMP - 143) 24650,14650,24650                               01840036
14650 IVPASS = IVPASS + 1                                               01850036
      WRITE (I02,80001) IVTNUM                                          01860036
      GO TO 4661                                                        01870036
24650 IVFAIL = IVFAIL + 1                                               01880036
      IVCORR = 143                                                      01890036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01900036
 4661 CONTINUE                                                          01910036
      IVTNUM = 466                                                      01920036
C                                                                       01930036
C      ****  TEST 466  ****                                             01940036
C                                                                       01950036
      IF (ICZERO) 34660, 4660, 34660                                    01960036
 4660 CONTINUE                                                          01970036
      IVCOMP = 6170/1234                                                01980036
      GO TO 44660                                                       01990036
34660 IVDELE = IVDELE + 1                                               02000036
      WRITE (I02,80003) IVTNUM                                          02010036
      IF (ICZERO) 44660, 4671, 44660                                    02020036
44660 IF (IVCOMP - 5) 24660,14660,24660                                 02030036
14660 IVPASS = IVPASS + 1                                               02040036
      WRITE (I02,80001) IVTNUM                                          02050036
      GO TO 4671                                                        02060036
24660 IVFAIL = IVFAIL + 1                                               02070036
      IVCORR = 5                                                        02080036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02090036
 4671 CONTINUE                                                          02100036
      IVTNUM = 467                                                      02110036
C                                                                       02120036
C      ****  TEST 467  ****                                             02130036
C                                                                       02140036
      IF (ICZERO) 34670, 4670, 34670                                    02150036
 4670 CONTINUE                                                          02160036
      IVCOMP = 28600/8                                                  02170036
      GO TO 44670                                                       02180036
34670 IVDELE = IVDELE + 1                                               02190036
      WRITE (I02,80003) IVTNUM                                          02200036
      IF (ICZERO) 44670, 4681, 44670                                    02210036
44670 IF (IVCOMP - 3575) 24670,14670,24670                              02220036
14670 IVPASS = IVPASS + 1                                               02230036
      WRITE (I02,80001) IVTNUM                                          02240036
      GO TO 4681                                                        02250036
24670 IVFAIL = IVFAIL + 1                                               02260036
      IVCORR = 3575                                                     02270036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02280036
 4681 CONTINUE                                                          02290036
      IVTNUM = 468                                                      02300036
C                                                                       02310036
C      ****  TEST 468  ****                                             02320036
C                                                                       02330036
      IF (ICZERO) 34680, 4680, 34680                                    02340036
 4680 CONTINUE                                                          02350036
      IVCOMP = 32766/2                                                  02360036
      GO TO 44680                                                       02370036
34680 IVDELE = IVDELE + 1                                               02380036
      WRITE (I02,80003) IVTNUM                                          02390036
      IF (ICZERO) 44680, 4691, 44680                                    02400036
44680 IF (IVCOMP - 16383) 24680,14680,24680                             02410036
14680 IVPASS = IVPASS + 1                                               02420036
      WRITE (I02,80001) IVTNUM                                          02430036
      GO TO 4691                                                        02440036
24680 IVFAIL = IVFAIL + 1                                               02450036
      IVCORR = 16383                                                    02460036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02470036
 4691 CONTINUE                                                          02480036
      IVTNUM = 469                                                      02490036
C                                                                       02500036
C      ****  TEST 469  ****                                             02510036
C                                                                       02520036
      IF (ICZERO) 34690, 4690, 34690                                    02530036
 4690 CONTINUE                                                          02540036
      IVCOMP = 32767/1                                                  02550036
      GO TO 44690                                                       02560036
34690 IVDELE = IVDELE + 1                                               02570036
      WRITE (I02,80003) IVTNUM                                          02580036
      IF (ICZERO) 44690, 4701, 44690                                    02590036
44690 IF (IVCOMP - 32767) 24690,14690,24690                             02600036
14690 IVPASS = IVPASS + 1                                               02610036
      WRITE (I02,80001) IVTNUM                                          02620036
      GO TO 4701                                                        02630036
24690 IVFAIL = IVFAIL + 1                                               02640036
      IVCORR = 32767                                                    02650036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02660036
C                                                                       02670036
C     TEST 470 THROUGH TEST 478 - POSITIVE CONSTANTS                    02680036
C               TRUNCATION REQUIRED                                     02690036
C                                                                       02700036
 4701 CONTINUE                                                          02710036
      IVTNUM = 470                                                      02720036
C                                                                       02730036
C      ****  TEST 470  ****                                             02740036
C                                                                       02750036
      IF (ICZERO) 34700, 4700, 34700                                    02760036
 4700 CONTINUE                                                          02770036
      IVCOMP = 5/2                                                      02780036
      GO TO 44700                                                       02790036
34700 IVDELE = IVDELE + 1                                               02800036
      WRITE (I02,80003) IVTNUM                                          02810036
      IF (ICZERO) 44700, 4711, 44700                                    02820036
44700 IF (IVCOMP - 2) 24700,14700,24700                                 02830036
14700 IVPASS = IVPASS + 1                                               02840036
      WRITE (I02,80001) IVTNUM                                          02850036
      GO TO 4711                                                        02860036
24700 IVFAIL = IVFAIL + 1                                               02870036
      IVCORR = 2                                                        02880036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02890036
 4711 CONTINUE                                                          02900036
      IVTNUM = 471                                                      02910036
C                                                                       02920036
C      ****  TEST 471  ****                                             02930036
C                                                                       02940036
      IF (ICZERO) 34710, 4710, 34710                                    02950036
 4710 CONTINUE                                                          02960036
      IVCOMP = 2/3                                                      02970036
      GO TO 44710                                                       02980036
34710 IVDELE = IVDELE + 1                                               02990036
      WRITE (I02,80003) IVTNUM                                          03000036
      IF (ICZERO) 44710, 4721, 44710                                    03010036
44710 IF (IVCOMP - 0) 24710,14710,24710                                 03020036
14710 IVPASS = IVPASS + 1                                               03030036
      WRITE (I02,80001) IVTNUM                                          03040036
      GO TO 4721                                                        03050036
24710 IVFAIL = IVFAIL + 1                                               03060036
      IVCORR = 0                                                        03070036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03080036
 4721 CONTINUE                                                          03090036
      IVTNUM = 472                                                      03100036
C                                                                       03110036
C      ****  TEST 472  ****                                             03120036
C                                                                       03130036
      IF (ICZERO) 34720, 4720, 34720                                    03140036
 4720 CONTINUE                                                          03150036
      IVCOMP = 80/15                                                    03160036
      GO TO 44720                                                       03170036
34720 IVDELE = IVDELE + 1                                               03180036
      WRITE (I02,80003) IVTNUM                                          03190036
      IF (ICZERO) 44720, 4731, 44720                                    03200036
44720 IF (IVCOMP - 5) 24720,14720,24720                                 03210036
14720 IVPASS = IVPASS + 1                                               03220036
      WRITE (I02,80001) IVTNUM                                          03230036
      GO TO 4731                                                        03240036
24720 IVFAIL = IVFAIL + 1                                               03250036
      IVCORR = 5                                                        03260036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03270036
 4731 CONTINUE                                                          03280036
      IVTNUM = 473                                                      03290036
C                                                                       03300036
C      ****  TEST 473  ****                                             03310036
C                                                                       03320036
      IF (ICZERO) 34730, 4730, 34730                                    03330036
 4730 CONTINUE                                                          03340036
      IVCOMP = 959/120                                                  03350036
      GO TO 44730                                                       03360036
34730 IVDELE = IVDELE + 1                                               03370036
      WRITE (I02,80003) IVTNUM                                          03380036
      IF (ICZERO) 44730, 4741, 44730                                    03390036
44730 IF (IVCOMP - 7) 24730,14730,24730                                 03400036
14730 IVPASS = IVPASS + 1                                               03410036
      WRITE (I02,80001) IVTNUM                                          03420036
      GO TO 4741                                                        03430036
24730 IVFAIL = IVFAIL + 1                                               03440036
      IVCORR = 7                                                        03450036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03460036
 4741 CONTINUE                                                          03470036
      IVTNUM = 474                                                      03480036
C                                                                       03490036
C      ****  TEST 474  ****                                             03500036
C                                                                       03510036
      IF (ICZERO) 34740, 4740, 34740                                    03520036
 4740 CONTINUE                                                          03530036
      IVCOMP = 959 / 12                                                 03540036
      GO TO 44740                                                       03550036
34740 IVDELE = IVDELE + 1                                               03560036
      WRITE (I02,80003) IVTNUM                                          03570036
      IF (ICZERO) 44740, 4751, 44740                                    03580036
44740 IF (IVCOMP - 79) 24740,14740,24740                                03590036
14740 IVPASS = IVPASS + 1                                               03600036
      WRITE (I02,80001) IVTNUM                                          03610036
      GO TO 4751                                                        03620036
24740 IVFAIL = IVFAIL + 1                                               03630036
      IVCORR = 79                                                       03640036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03650036
 4751 CONTINUE                                                          03660036
      IVTNUM = 475                                                      03670036
C                                                                       03680036
C      ****  TEST 475  ****                                             03690036
C                                                                       03700036
      IF (ICZERO) 34750, 4750, 34750                                    03710036
 4750 CONTINUE                                                          03720036
      IVCOMP = 959/6                                                    03730036
      GO TO 44750                                                       03740036
34750 IVDELE = IVDELE + 1                                               03750036
      WRITE (I02,80003) IVTNUM                                          03760036
      IF (ICZERO) 44750, 4761, 44750                                    03770036
44750 IF (IVCOMP - 159) 24750,14750,24750                               03780036
14750 IVPASS = IVPASS + 1                                               03790036
      WRITE (I02,80001) IVTNUM                                          03800036
      GO TO 4761                                                        03810036
24750 IVFAIL = IVFAIL + 1                                               03820036
      IVCORR = 159                                                      03830036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03840036
 4761 CONTINUE                                                          03850036
      IVTNUM = 476                                                      03860036
C                                                                       03870036
C      ****  TEST 476  ****                                             03880036
C                                                                       03890036
      IF (ICZERO) 34760, 4760, 34760                                    03900036
 4760 CONTINUE                                                          03910036
      IVCOMP = 28606/8                                                  03920036
      GO TO 44760                                                       03930036
34760 IVDELE = IVDELE + 1                                               03940036
      WRITE (I02,80003) IVTNUM                                          03950036
      IF (ICZERO) 44760, 4771, 44760                                    03960036
44760 IF (IVCOMP - 3575) 24760,14760,24760                              03970036
14760 IVPASS = IVPASS + 1                                               03980036
      WRITE (I02,80001) IVTNUM                                          03990036
      GO TO 4771                                                        04000036
24760 IVFAIL = IVFAIL + 1                                               04010036
      IVCORR = 3575                                                     04020036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04030036
 4771 CONTINUE                                                          04040036
      IVTNUM = 477                                                      04050036
C                                                                       04060036
C      ****  TEST 477  ****                                             04070036
C                                                                       04080036
      IF (ICZERO) 34770, 4770, 34770                                    04090036
 4770 CONTINUE                                                          04100036
      IVCOMP = 25603/2                                                  04110036
      GO TO 44770                                                       04120036
34770 IVDELE = IVDELE + 1                                               04130036
      WRITE (I02,80003) IVTNUM                                          04140036
      IF (ICZERO) 44770, 4781, 44770                                    04150036
44770 IF (IVCOMP - 12801) 24770,14770,24770                             04160036
14770 IVPASS = IVPASS + 1                                               04170036
      WRITE (I02,80001) IVTNUM                                          04180036
      GO TO 4781                                                        04190036
24770 IVFAIL = IVFAIL + 1                                               04200036
      IVCORR = 12801                                                    04210036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04220036
 4781 CONTINUE                                                          04230036
      IVTNUM = 478                                                      04240036
C                                                                       04250036
C      ****  TEST 478  ****                                             04260036
C                                                                       04270036
      IF (ICZERO) 34780, 4780, 34780                                    04280036
 4780 CONTINUE                                                          04290036
      IVCOMP = 25603/10354                                              04300036
      GO TO 44780                                                       04310036
34780 IVDELE = IVDELE + 1                                               04320036
      WRITE (I02,80003) IVTNUM                                          04330036
      IF (ICZERO) 44780, 4791, 44780                                    04340036
44780 IF (IVCOMP - 2) 24780,14780,24780                                 04350036
14780 IVPASS = IVPASS + 1                                               04360036
      WRITE (I02,80001) IVTNUM                                          04370036
      GO TO 4791                                                        04380036
24780 IVFAIL = IVFAIL + 1                                               04390036
      IVCORR = 2                                                        04400036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04410036
C                                                                       04420036
C     TEST 479 THROUGH TEST 482 - NEGATIVE CONSTANTS INCLUDED           04430036
C                NO TRUNCATION REQUIRED                                 04440036
C                                                                       04450036
 4791 CONTINUE                                                          04460036
      IVTNUM = 479                                                      04470036
C                                                                       04480036
C      ****  TEST 479  ****                                             04490036
C                                                                       04500036
      IF (ICZERO) 34790, 4790, 34790                                    04510036
 4790 CONTINUE                                                          04520036
      IVCOMP = -4/2                                                     04530036
      GO TO 44790                                                       04540036
34790 IVDELE = IVDELE + 1                                               04550036
      WRITE (I02,80003) IVTNUM                                          04560036
      IF (ICZERO) 44790, 4801, 44790                                    04570036
44790 IF (IVCOMP + 2) 24790,14790,24790                                 04580036
14790 IVPASS = IVPASS + 1                                               04590036
      WRITE (I02,80001) IVTNUM                                          04600036
      GO TO 4801                                                        04610036
24790 IVFAIL = IVFAIL + 1                                               04620036
      IVCORR = -2                                                       04630036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04640036
 4801 CONTINUE                                                          04650036
      IVTNUM = 480                                                      04660036
C                                                                       04670036
C      ****  TEST 480  ****                                             04680036
C                                                                       04690036
      IF (ICZERO) 34800, 4800, 34800                                    04700036
 4800 CONTINUE                                                          04710036
      IVCOMP = 75 / (-25)                                               04720036
      GO TO 44800                                                       04730036
34800 IVDELE = IVDELE + 1                                               04740036
      WRITE (I02,80003) IVTNUM                                          04750036
      IF (ICZERO) 44800, 4811, 44800                                    04760036
44800 IF (IVCOMP + 3) 24800,14800,24800                                 04770036
14800 IVPASS = IVPASS + 1                                               04780036
      WRITE (I02,80001) IVTNUM                                          04790036
      GO TO 4811                                                        04800036
24800 IVFAIL = IVFAIL + 1                                               04810036
      IVCORR = -3                                                       04820036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04830036
 4811 CONTINUE                                                          04840036
      IVTNUM = 481                                                      04850036
C                                                                       04860036
C      ****  TEST 481  ****                                             04870036
C                                                                       04880036
      IF (ICZERO) 34810, 4810, 34810                                    04890036
 4810 CONTINUE                                                          04900036
      IVCOMP= (-6170) / (-1234)                                         04910036
      GO TO 44810                                                       04920036
34810 IVDELE = IVDELE + 1                                               04930036
      WRITE (I02,80003) IVTNUM                                          04940036
      IF (ICZERO) 44810, 4821, 44810                                    04950036
44810 IF (IVCOMP - 5) 24810,14810,24810                                 04960036
14810 IVPASS = IVPASS + 1                                               04970036
      WRITE (I02,80001) IVTNUM                                          04980036
      GO TO 4821                                                        04990036
24810 IVFAIL = IVFAIL + 1                                               05000036
      IVCORR = 5                                                        05010036
                                                                        05020036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05030036
 4821 CONTINUE                                                          05040036
      IVTNUM = 482                                                      05050036
C                                                                       05060036
C      ****  TEST 482  ****                                             05070036
C                                                                       05080036
      IF (ICZERO) 34820, 4820, 34820                                    05090036
 4820 CONTINUE                                                          05100036
      IVCOMP = -32766/(-2)                                              05110036
      GO TO 44820                                                       05120036
34820 IVDELE = IVDELE + 1                                               05130036
      WRITE (I02,80003) IVTNUM                                          05140036
      IF (ICZERO) 44820, 4831, 44820                                    05150036
44820 IF (IVCOMP - 16383) 24820,14820,24820                             05160036
14820 IVPASS = IVPASS + 1                                               05170036
      WRITE (I02,80001) IVTNUM                                          05180036
      GO TO 4831                                                        05190036
24820 IVFAIL = IVFAIL + 1                                               05200036
      IVCORR = 16383                                                    05210036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05220036
C                                                                       05230036
C     TEST 483 THROUGH TEST 490 - NEGATIVE CONSTANTS INCLUDED           05240036
C                TRUNCATION REQUIRED                                    05250036
C                                                                       05260036
 4831 CONTINUE                                                          05270036
      IVTNUM = 483                                                      05280036
C                                                                       05290036
C      ****  TEST 483  ****                                             05300036
C                                                                       05310036
      IF (ICZERO) 34830, 4830, 34830                                    05320036
 4830 CONTINUE                                                          05330036
      IVCOMP = -5/2                                                     05340036
      GO TO 44830                                                       05350036
34830 IVDELE = IVDELE + 1                                               05360036
      WRITE (I02,80003) IVTNUM                                          05370036
      IF (ICZERO) 44830, 4841, 44830                                    05380036
44830 IF (IVCOMP +2) 24830,14830,24830                                  05390036
14830 IVPASS = IVPASS + 1                                               05400036
      WRITE (I02,80001) IVTNUM                                          05410036
      GO TO 4841                                                        05420036
24830 IVFAIL = IVFAIL + 1                                               05430036
      IVCORR = -2                                                       05440036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05450036
 4841 CONTINUE                                                          05460036
      IVTNUM = 484                                                      05470036
C                                                                       05480036
C      ****  TEST 484  ****                                             05490036
C                                                                       05500036
      IF (ICZERO) 34840, 4840, 34840                                    05510036
 4840 CONTINUE                                                          05520036
      IVCOMP = -2/3                                                     05530036
      GO TO 44840                                                       05540036
34840 IVDELE = IVDELE + 1                                               05550036
      WRITE (I02,80003) IVTNUM                                          05560036
      IF (ICZERO) 44840, 4851, 44840                                    05570036
44840 IF (IVCOMP) 24840,14840,24840                                     05580036
14840 IVPASS = IVPASS + 1                                               05590036
      WRITE (I02,80001) IVTNUM                                          05600036
      GO TO 4851                                                        05610036
24840 IVFAIL = IVFAIL + 1                                               05620036
      IVCORR = 0                                                        05630036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05640036
 4851 CONTINUE                                                          05650036
      IVTNUM = 485                                                      05660036
C                                                                       05670036
C      ****  TEST 485  ****                                             05680036
C                                                                       05690036
      IF (ICZERO) 34850, 4850, 34850                                    05700036
 4850 CONTINUE                                                          05710036
      IVCOMP = 80/(-15)                                                 05720036
      GO TO 44850                                                       05730036
34850 IVDELE = IVDELE + 1                                               05740036
      WRITE (I02,80003) IVTNUM                                          05750036
      IF (ICZERO) 44850, 4861, 44850                                    05760036
44850 IF (IVCOMP +5) 24850,14850,24850                                  05770036
14850 IVPASS = IVPASS + 1                                               05780036
      WRITE (I02,80001) IVTNUM                                          05790036
      GO TO 4861                                                        05800036
24850 IVFAIL = IVFAIL + 1                                               05810036
      IVCORR = -5                                                       05820036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05830036
 4861 CONTINUE                                                          05840036
      IVTNUM = 486                                                      05850036
C                                                                       05860036
C      ****  TEST 486  ****                                             05870036
C                                                                       05880036
      IF (ICZERO) 34860, 4860, 34860                                    05890036
 4860 CONTINUE                                                          05900036
      IVCOMP = -959/(-120)                                              05910036
      GO TO 44860                                                       05920036
34860 IVDELE = IVDELE + 1                                               05930036
      WRITE (I02,80003) IVTNUM                                          05940036
      IF (ICZERO) 44860, 4871, 44860                                    05950036
44860 IF (IVCOMP - 7) 24860,14860,24860                                 05960036
14860 IVPASS = IVPASS + 1                                               05970036
      WRITE (I02,80001) IVTNUM                                          05980036
      GO TO 4871                                                        05990036
24860 IVFAIL = IVFAIL + 1                                               06000036
      IVCORR = 7                                                        06010036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06020036
 4871 CONTINUE                                                          06030036
      IVTNUM = 487                                                      06040036
C                                                                       06050036
C      ****  TEST 487  ****                                             06060036
C                                                                       06070036
      IF (ICZERO) 34870, 4870, 34870                                    06080036
 4870 CONTINUE                                                          06090036
      IVCOMP = -959/6                                                   06100036
      GO TO 44870                                                       06110036
34870 IVDELE = IVDELE + 1                                               06120036
      WRITE (I02,80003) IVTNUM                                          06130036
      IF (ICZERO) 44870, 4881, 44870                                    06140036
44870 IF (IVCOMP + 159) 24870,14870,24870                               06150036
14870 IVPASS = IVPASS + 1                                               06160036
      WRITE (I02,80001) IVTNUM                                          06170036
      GO TO 4881                                                        06180036
24870 IVFAIL = IVFAIL + 1                                               06190036
      IVCORR = -159                                                     06200036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06210036
 4881 CONTINUE                                                          06220036
      IVTNUM = 488                                                      06230036
C                                                                       06240036
C      ****  TEST 488  ****                                             06250036
C                                                                       06260036
      IF (ICZERO) 34880, 4880, 34880                                    06270036
 4880 CONTINUE                                                          06280036
      IVCOMP = -28606/(-8)                                              06290036
      GO TO 44880                                                       06300036
34880 IVDELE = IVDELE + 1                                               06310036
      WRITE (I02,80003) IVTNUM                                          06320036
      IF (ICZERO) 44880, 4891, 44880                                    06330036
44880 IF (IVCOMP - 3575) 24880,14880,24880                              06340036
14880 IVPASS = IVPASS + 1                                               06350036
      WRITE (I02,80001) IVTNUM                                          06360036
      GO TO 4891                                                        06370036
24880 IVFAIL = IVFAIL + 1                                               06380036
      IVCORR = 3575                                                     06390036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06400036
 4891 CONTINUE                                                          06410036
      IVTNUM = 489                                                      06420036
C                                                                       06430036
C      ****  TEST 489  ****                                             06440036
C                                                                       06450036
      IF (ICZERO) 34890, 4890, 34890                                    06460036
 4890 CONTINUE                                                          06470036
      IVCOMP = -25603/2                                                 06480036
      GO TO 44890                                                       06490036
34890 IVDELE = IVDELE + 1                                               06500036
      WRITE (I02,80003) IVTNUM                                          06510036
      IF (ICZERO) 44890, 4901, 44890                                    06520036
44890 IF (IVCOMP + 12801) 24890,14890,24890                             06530036
14890 IVPASS = IVPASS + 1                                               06540036
      WRITE (I02,80001) IVTNUM                                          06550036
      GO TO 4901                                                        06560036
24890 IVFAIL = IVFAIL + 1                                               06570036
      IVCORR = -12801                                                   06580036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06590036
 4901 CONTINUE                                                          06600036
      IVTNUM = 490                                                      06610036
C                                                                       06620036
C      ****  TEST 490  ****                                             06630036
C                                                                       06640036
      IF (ICZERO) 34900, 4900, 34900                                    06650036
 4900 CONTINUE                                                          06660036
      IVCOMP = -25603/(-10354)                                          06670036
      GO TO 44900                                                       06680036
34900 IVDELE = IVDELE + 1                                               06690036
      WRITE (I02,80003) IVTNUM                                          06700036
      IF (ICZERO) 44900, 4911, 44900                                    06710036
44900 IF (IVCOMP - 2) 24900,14900,24900                                 06720036
14900 IVPASS = IVPASS + 1                                               06730036
      WRITE (I02,80001) IVTNUM                                          06740036
      GO TO 4911                                                        06750036
24900 IVFAIL = IVFAIL + 1                                               06760036
      IVCORR = 2                                                        06770036
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06780036
C                                                                       06790036
C      ****    END OF TESTS    ****                                     06800036
 4911 CONTINUE                                                          06810036
C                                                                       06820036
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             06830036
99999 CONTINUE                                                          06840036
      WRITE (I02,90002)                                                 06850036
      WRITE (I02,90006)                                                 06860036
      WRITE (I02,90002)                                                 06870036
      WRITE (I02,90002)                                                 06880036
      WRITE (I02,90007)                                                 06890036
      WRITE (I02,90002)                                                 06900036
      WRITE (I02,90008)  IVFAIL                                         06910036
      WRITE (I02,90009) IVPASS                                          06920036
      WRITE (I02,90010) IVDELE                                          06930036
C                                                                       06940036
C                                                                       06950036
C     TERMINATE ROUTINE EXECUTION                                       06960036
      STOP                                                              06970036
C                                                                       06980036
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06990036
90000 FORMAT (1H1)                                                      07000036
90002 FORMAT (1H )                                                      07010036
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07020036
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07030036
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07040036
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07050036
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07060036
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07070036
C                                                                       07080036
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07090036
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07100036
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07110036
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07120036
C                                                                       07130036
C     FORMAT STATEMENTS FOR TEST RESULTS                                07140036
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07150036
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07160036
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07170036
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07180036
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07190036
C                                                                       07200036
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM036)                          07210036
      END                                                               07220036
