C     COMMENT SECTION                                                   00010040
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020040
C     FM040                                                             00030040
C                                                                       00040040
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050040
C         FORM      INTEGER VARIABLE = ARITHMETIC EXPRESSION            00060040
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00070040
C     OPERATOR /, INTEGER VARIABLES AND AN INTEGER CONSTANT.  BOTH      00080040
C     POSITIVE AND NEGATIVE VALUES ARE USED FOR THE INTEGER VARIABLES   00090040
C     AND THE INTEGER CONSTANT.                                         00100040
C                                                                       00110040
C         THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT AND 00120040
C     TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED IN   00130040
C     THE RESULTANT INTEGER VARIABLE.  SOME OF THE TESTS USE PARENS     00140040
C     TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSION.                   00150040
C                                                                       00160040
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00170040
C             (1) INTEGER VARIABLE/INTEGER VARIABLE                     00180040
C             (2) INTEGER VARIABLE/INTEGER VARIABLE/INTEGER CONSTANT    00190040
C                 INTEGER VARIABLE/INTEGER CONSTANT/INTEGER VARIABLE    00200040
C                 INTEGER CONSTANT/INTEGER VARIABLE/INTEGER VARIABLE    00210040
C             (3) SAME AS (2) BUT WITH PARENTHESES TO GROUP ELEMENTS    00220040
C                   IN THE ARITHMETIC EXPRESSION.                       00230040
C                                                                       00240040
C      REFERENCES                                                       00250040
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00260040
C              X3.9-1978                                                00270040
C                                                                       00280040
C        SECTION 4.3, INTEGER TYPE                                      00290040
C        SECTION 4.3.1, INTEGER CONSTANT                                00300040
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00310040
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00320040
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00330040
C                                                                       00340040
C      **********************************************************       00350040
C                                                                       00360040
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00370040
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00380040
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00390040
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00400040
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00410040
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00420040
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00430040
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00440040
C     OF EXECUTING THESE TESTS.                                         00450040
C                                                                       00460040
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00470040
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00480040
C                                                                       00490040
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00500040
C                                                                       00510040
C                  DEPARTMENT OF THE NAVY                               00520040
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00530040
C                  WASHINGTON, D.C.  20376                              00540040
C                                                                       00550040
C      **********************************************************       00560040
C                                                                       00570040
C                                                                       00580040
C                                                                       00590040
C     INITIALIZATION SECTION                                            00600040
C                                                                       00610040
C     INITIALIZE CONSTANTS                                              00620040
C      **************                                                   00630040
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640040
      I01 = 5                                                           00650040
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660040
      I02 = 6                                                           00670040
C     SYSTEM ENVIRONMENT SECTION                                        00680040
C                                                                       00690040
      I01 = 5                                                           00700040
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710040
C     (UNIT NUMBER FOR CARD READER).                                    00720040
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00730040
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00740040
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00750040
C                                                                       00760040
      I02 = 6                                                           00770040
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00780040
C     (UNIT NUMBER FOR PRINTER).                                        00790040
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00800040
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00810040
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00820040
C                                                                       00830040
      IVPASS=0                                                          00840040
      IVFAIL=0                                                          00850040
      IVDELE=0                                                          00860040
      ICZERO=0                                                          00870040
C                                                                       00880040
C     WRITE PAGE HEADERS                                                00890040
      WRITE (I02,90000)                                                 00900040
      WRITE (I02,90001)                                                 00910040
      WRITE (I02,90002)                                                 00920040
      WRITE (I02, 90002)                                                00930040
      WRITE (I02,90003)                                                 00940040
      WRITE (I02,90002)                                                 00950040
      WRITE (I02,90004)                                                 00960040
      WRITE (I02,90002)                                                 00970040
      WRITE (I02,90011)                                                 00980040
      WRITE (I02,90002)                                                 00990040
      WRITE (I02,90002)                                                 01000040
      WRITE (I02,90005)                                                 01010040
      WRITE (I02,90006)                                                 01020040
      WRITE (I02,90002)                                                 01030040
C                                                                       01040040
C     TEST SECTION                                                      01050040
C                                                                       01060040
C         ARITHMETIC ASSIGNMENT STATEMENT                               01070040
C                                                                       01080040
C     TEST 582 THROUGH TEST 597 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS01090040
C     OF THE FORM       INTEGER VARIABLE=INTEGER VARIABLE/INTEGER VAR.  01100040
C                                                                       01110040
C     TEST 582 THROUGH TEST 585 - POSITIVE VALUES                       01120040
C                   NO TRUNCATION REQUIRED                              01130040
C                                                                       01140040
 5821 CONTINUE                                                          01150040
      IVTNUM = 582                                                      01160040
C                                                                       01170040
C      ****  TEST 582  ****                                             01180040
C                                                                       01190040
      IF (ICZERO) 35820, 5820, 35820                                    01200040
 5820 CONTINUE                                                          01210040
      IVON01 = 4                                                        01220040
      IVON02 = 2                                                        01230040
      IVCOMP = IVON01 / IVON02                                          01240040
      GO TO 45820                                                       01250040
35820 IVDELE = IVDELE + 1                                               01260040
      WRITE (I02,80003) IVTNUM                                          01270040
      IF (ICZERO) 45820, 5831, 45820                                    01280040
45820 IF (IVCOMP -2) 25820,15820,25820                                  01290040
15820 IVPASS = IVPASS + 1                                               01300040
      WRITE (I02,80001) IVTNUM                                          01310040
      GO TO 5831                                                        01320040
25820 IVFAIL = IVFAIL + 1                                               01330040
      IVCORR = 2                                                        01340040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01350040
 5831 CONTINUE                                                          01360040
      IVTNUM = 583                                                      01370040
C                                                                       01380040
C      ****  TEST 583  ****                                             01390040
C                                                                       01400040
      IF (ICZERO) 35830, 5830, 35830                                    01410040
 5830 CONTINUE                                                          01420040
      IVON01 = 3575                                                     01430040
      IVON02 = 25                                                       01440040
      IVCOMP = IVON01/IVON02                                            01450040
      GO TO 45830                                                       01460040
35830 IVDELE = IVDELE + 1                                               01470040
      WRITE (I02,80003) IVTNUM                                          01480040
      IF (ICZERO) 45830, 5841, 45830                                    01490040
45830 IF (IVCOMP - 143) 25830,15830,25830                               01500040
15830 IVPASS = IVPASS + 1                                               01510040
      WRITE (I02,80001) IVTNUM                                          01520040
      GO TO 5841                                                        01530040
25830 IVFAIL = IVFAIL + 1                                               01540040
      IVCORR = 143                                                      01550040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01560040
 5841 CONTINUE                                                          01570040
      IVTNUM = 584                                                      01580040
C                                                                       01590040
C      ****  TEST 584  ****                                             01600040
C                                                                       01610040
      IF (ICZERO) 35840, 5840, 35840                                    01620040
 5840 CONTINUE                                                          01630040
      IVON01 = 6170                                                     01640040
      IVON02 = 1234                                                     01650040
      IVCOMP = IVON01/IVON02                                            01660040
      GO TO 45840                                                       01670040
35840 IVDELE = IVDELE + 1                                               01680040
      WRITE (I02,80003) IVTNUM                                          01690040
      IF (ICZERO) 45840, 5851, 45840                                    01700040
45840 IF (IVCOMP - 5) 25840,15840,25840                                 01710040
15840 IVPASS = IVPASS + 1                                               01720040
      WRITE (I02,80001) IVTNUM                                          01730040
      GO TO 5851                                                        01740040
25840 IVFAIL = IVFAIL + 1                                               01750040
      IVCORR = 5                                                        01760040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01770040
 5851 CONTINUE                                                          01780040
      IVTNUM = 585                                                      01790040
C                                                                       01800040
C      ****  TEST 585  ****                                             01810040
C                                                                       01820040
      IF (ICZERO) 35850, 5850, 35850                                    01830040
 5850 CONTINUE                                                          01840040
      IVON01 = 32767                                                    01850040
      IVON02 = 1                                                        01860040
      IVCOMP = IVON01/IVON02                                            01870040
      GO TO 45850                                                       01880040
35850 IVDELE = IVDELE + 1                                               01890040
      WRITE (I02,80003) IVTNUM                                          01900040
      IF (ICZERO) 45850, 5861, 45850                                    01910040
45850 IF (IVCOMP - 32767) 25850,15850,25850                             01920040
15850 IVPASS = IVPASS + 1                                               01930040
      WRITE (I02,80001) IVTNUM                                          01940040
      GO TO 5861                                                        01950040
25850 IVFAIL = IVFAIL + 1                                               01960040
      IVCORR = 32767                                                    01970040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01980040
C                                                                       01990040
C     TEST 586 THROUGH TEST 589  -  POSITIVE VALUES                     02000040
C                   TRUNCATION OF RESULT REQUIRED                       02010040
C                                                                       02020040
 5861 CONTINUE                                                          02030040
      IVTNUM = 586                                                      02040040
C                                                                       02050040
C      ****  TEST 586  ****                                             02060040
C                                                                       02070040
      IF (ICZERO) 35860, 5860, 35860                                    02080040
 5860 CONTINUE                                                          02090040
      IVON01 = 2                                                        02100040
      IVON02 = 3                                                        02110040
      IVCOMP = IVON01/IVON02                                            02120040
      GO TO 45860                                                       02130040
35860 IVDELE = IVDELE + 1                                               02140040
      WRITE (I02,80003) IVTNUM                                          02150040
      IF (ICZERO) 45860, 5871, 45860                                    02160040
45860 IF (IVCOMP) 25860,15860,25860                                     02170040
15860 IVPASS = IVPASS + 1                                               02180040
      WRITE (I02,80001) IVTNUM                                          02190040
      GO TO 5871                                                        02200040
25860 IVFAIL = IVFAIL + 1                                               02210040
      IVCORR = 0                                                        02220040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02230040
 5871 CONTINUE                                                          02240040
      IVTNUM = 587                                                      02250040
C                                                                       02260040
C      ****  TEST 587  ****                                             02270040
C                                                                       02280040
      IF (ICZERO) 35870, 5870, 35870                                    02290040
 5870 CONTINUE                                                          02300040
      IVON01 = 959                                                      02310040
      IVON02 = 120                                                      02320040
      IVCOMP = IVON01/IVON02                                            02330040
      GO TO 45870                                                       02340040
35870 IVDELE = IVDELE + 1                                               02350040
      WRITE (I02,80003) IVTNUM                                          02360040
      IF (ICZERO) 45870, 5881, 45870                                    02370040
45870 IF (IVCOMP - 7) 25870,15870,25870                                 02380040
15870 IVPASS = IVPASS + 1                                               02390040
      WRITE (I02,80001) IVTNUM                                          02400040
      GO TO 5881                                                        02410040
25870 IVFAIL = IVFAIL + 1                                               02420040
      IVCORR = 7                                                        02430040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02440040
 5881 CONTINUE                                                          02450040
      IVTNUM = 588                                                      02460040
C                                                                       02470040
C      ****  TEST 588  ****                                             02480040
C                                                                       02490040
      IF (ICZERO) 35880, 5880, 35880                                    02500040
 5880 CONTINUE                                                          02510040
      IVON01 = 26606                                                    02520040
      IVON02 = 8                                                        02530040
      IVCOMP = IVON01/IVON02                                            02540040
      GO TO 45880                                                       02550040
35880 IVDELE = IVDELE + 1                                               02560040
      WRITE (I02,80003) IVTNUM                                          02570040
      IF (ICZERO) 45880, 5891, 45880                                    02580040
45880 IF (IVCOMP - 3325) 25880,15880,25880                              02590040
15880 IVPASS = IVPASS + 1                                               02600040
      WRITE (I02,80001) IVTNUM                                          02610040
      GO TO 5891                                                        02620040
25880 IVFAIL = IVFAIL + 1                                               02630040
      IVCORR = 3325                                                     02640040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02650040
 5891 CONTINUE                                                          02660040
      IVTNUM = 589                                                      02670040
C                                                                       02680040
C      ****  TEST 589  ****                                             02690040
C                                                                       02700040
      IF (ICZERO) 35890, 5890, 35890                                    02710040
 5890 CONTINUE                                                          02720040
      IVON01 = 25603                                                    02730040
      IVON02 = 10354                                                    02740040
      IVCOMP = IVON01/IVON02                                            02750040
      GO TO 45890                                                       02760040
35890 IVDELE = IVDELE + 1                                               02770040
      WRITE (I02,80003) IVTNUM                                          02780040
      IF (ICZERO) 45890, 5901, 45890                                    02790040
45890 IF (IVCOMP - 2) 25890,15890,25890                                 02800040
15890 IVPASS = IVPASS + 1                                               02810040
      WRITE (I02,80001) IVTNUM                                          02820040
      GO TO 5901                                                        02830040
25890 IVFAIL = IVFAIL + 1                                               02840040
      IVCORR = 2                                                        02850040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02860040
C                                                                       02870040
C     TEST 590 THROUGH TEST 593  - NEGATIVE VALUES INCLUDED             02880040
C               NO TRUNCATION REQUIRED                                  02890040
C                                                                       02900040
 5901 CONTINUE                                                          02910040
      IVTNUM = 590                                                      02920040
C                                                                       02930040
C      ****  TEST 590  ****                                             02940040
C                                                                       02950040
      IF (ICZERO) 35900, 5900, 35900                                    02960040
 5900 CONTINUE                                                          02970040
      IVON01 = 75                                                       02980040
      IVON02 = -25                                                      02990040
      IVCOMP = IVON01/IVON02                                            03000040
      GO TO 45900                                                       03010040
35900 IVDELE = IVDELE + 1                                               03020040
      WRITE (I02,80003) IVTNUM                                          03030040
      IF (ICZERO) 45900, 5911, 45900                                    03040040
45900 IF (IVCOMP + 3) 25900,15900,25900                                 03050040
15900 IVPASS = IVPASS + 1                                               03060040
      WRITE (I02,80001) IVTNUM                                          03070040
      GO TO 5911                                                        03080040
25900 IVFAIL = IVFAIL + 1                                               03090040
      IVCORR = -3                                                       03100040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03110040
 5911 CONTINUE                                                          03120040
      IVTNUM = 591                                                      03130040
C                                                                       03140040
C      ****  TEST 591  ****                                             03150040
C                                                                       03160040
      IF (ICZERO) 35910, 5910, 35910                                    03170040
 5910 CONTINUE                                                          03180040
      IVON01 = -6170                                                    03190040
      IVON02 = -1234                                                    03200040
      IVCOMP = IVON01/IVON02                                            03210040
      GO TO 45910                                                       03220040
35910 IVDELE = IVDELE + 1                                               03230040
      WRITE (I02,80003) IVTNUM                                          03240040
      IF (ICZERO) 45910, 5921, 45910                                    03250040
45910 IF (IVCOMP -5) 25910,15910,25910                                  03260040
15910 IVPASS = IVPASS + 1                                               03270040
      WRITE (I02,80001) IVTNUM                                          03280040
      GO TO 5921                                                        03290040
25910 IVFAIL = IVFAIL + 1                                               03300040
      IVCORR = 5                                                        03310040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03320040
 5921 CONTINUE                                                          03330040
      IVTNUM = 592                                                      03340040
C                                                                       03350040
C      ****  TEST 592  ****                                             03360040
C                                                                       03370040
      IF (ICZERO) 35920, 5920, 35920                                    03380040
 5920 CONTINUE                                                          03390040
      IVON01 = 32766                                                    03400040
      IVON02 = -2                                                       03410040
      IVCOMP =-IVON01/IVON02                                            03420040
      GO TO 45920                                                       03430040
35920 IVDELE = IVDELE + 1                                               03440040
      WRITE (I02,80003) IVTNUM                                          03450040
      IF (ICZERO) 45920, 5931, 45920                                    03460040
45920 IF (IVCOMP - 16383) 25920,15920,25920                             03470040
15920 IVPASS = IVPASS + 1                                               03480040
      WRITE (I02,80001) IVTNUM                                          03490040
      GO TO 5931                                                        03500040
25920 IVFAIL = IVFAIL + 1                                               03510040
      IVCORR = 16383                                                    03520040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03530040
 5931 CONTINUE                                                          03540040
      IVTNUM = 593                                                      03550040
C                                                                       03560040
C      ****  TEST 593  ****                                             03570040
C                                                                       03580040
      IF (ICZERO) 35930, 5930, 35930                                    03590040
 5930 CONTINUE                                                          03600040
      IVON01 = 4                                                        03610040
      IVON02 = 2                                                        03620040
      IVCOMP = IVON01/(-IVON02)                                         03630040
      GO TO 45930                                                       03640040
35930 IVDELE = IVDELE + 1                                               03650040
      WRITE (I02,80003) IVTNUM                                          03660040
      IF (ICZERO) 45930, 5941, 45930                                    03670040
45930 IF (IVCOMP + 2) 25930,15930,25930                                 03680040
15930 IVPASS = IVPASS + 1                                               03690040
      WRITE (I02,80001) IVTNUM                                          03700040
      GO TO 5941                                                        03710040
25930 IVFAIL = IVFAIL + 1                                               03720040
      IVCORR = -2                                                       03730040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03740040
C                                                                       03750040
C     TEST 594 THROUGH TEST 597  -  NEGATIVE VALUES INCLUDED            03760040
C                      TRUNCATION OF RESULT REQUIRED                    03770040
C                                                                       03780040
 5941 CONTINUE                                                          03790040
      IVTNUM = 594                                                      03800040
C                                                                       03810040
C      ****  TEST 594  ****                                             03820040
C                                                                       03830040
      IF (ICZERO) 35940, 5940, 35940                                    03840040
 5940 CONTINUE                                                          03850040
      IVON01 = -5                                                       03860040
      IVON02 = 2                                                        03870040
      IVCOMP = IVON01/IVON02                                            03880040
      GO TO 45940                                                       03890040
35940 IVDELE = IVDELE + 1                                               03900040
      WRITE (I02,80003) IVTNUM                                          03910040
      IF (ICZERO) 45940, 5951, 45940                                    03920040
45940 IF (IVCOMP + 2) 25940,15940,25940                                 03930040
15940 IVPASS = IVPASS + 1                                               03940040
      WRITE (I02,80001) IVTNUM                                          03950040
      GO TO 5951                                                        03960040
25940 IVFAIL = IVFAIL + 1                                               03970040
      IVCORR = -2                                                       03980040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03990040
 5951 CONTINUE                                                          04000040
      IVTNUM = 595                                                      04010040
C                                                                       04020040
C      ****  TEST 595  ****                                             04030040
C                                                                       04040040
      IF (ICZERO) 35950, 5950, 35950                                    04050040
 5950 CONTINUE                                                          04060040
      IVON01 = -25603                                                   04070040
      IVON02 = -10354                                                   04080040
      IVCOMP = IVON01/IVON02                                            04090040
      GO TO 45950                                                       04100040
35950 IVDELE = IVDELE + 1                                               04110040
      WRITE (I02,80003) IVTNUM                                          04120040
      IF (ICZERO) 45950, 5961, 45950                                    04130040
45950 IF (IVCOMP -2) 25950,15950,25950                                  04140040
15950 IVPASS = IVPASS + 1                                               04150040
      WRITE (I02,80001) IVTNUM                                          04160040
      GO TO 5961                                                        04170040
25950 IVFAIL = IVFAIL + 1                                               04180040
      IVCORR =2                                                         04190040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04200040
 5961 CONTINUE                                                          04210040
      IVTNUM = 596                                                      04220040
C                                                                       04230040
C      ****  TEST 596  ****                                             04240040
C                                                                       04250040
      IF (ICZERO) 35960, 5960, 35960                                    04260040
 5960 CONTINUE                                                          04270040
      IVON01 = 25603                                                    04280040
      IVON02 = 10354                                                    04290040
      IVCOMP = -IVON01/IVON02                                           04300040
      GO TO 45960                                                       04310040
35960 IVDELE = IVDELE + 1                                               04320040
      WRITE (I02,80003) IVTNUM                                          04330040
      IF (ICZERO) 45960, 5971, 45960                                    04340040
45960 IF (IVCOMP +2) 25960,15960,25960                                  04350040
15960 IVPASS = IVPASS + 1                                               04360040
      WRITE (I02,80001) IVTNUM                                          04370040
      GO TO 5971                                                        04380040
25960 IVFAIL = IVFAIL + 1                                               04390040
      IVCORR = -2                                                       04400040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04410040
 5971 CONTINUE                                                          04420040
      IVTNUM = 597                                                      04430040
C                                                                       04440040
C      ****  TEST 597  ****                                             04450040
C                                                                       04460040
      IF (ICZERO) 35970, 5970, 35970                                    04470040
 5970 CONTINUE                                                          04480040
      IVON01 = 25603                                                    04490040
      IVON02 = -2                                                       04500040
      IVCOMP = -(IVON01/IVON02)                                         04510040
      GO TO 45970                                                       04520040
35970 IVDELE = IVDELE + 1                                               04530040
      WRITE (I02,80003) IVTNUM                                          04540040
      IF (ICZERO) 45970, 5981, 45970                                    04550040
45970 IF (IVCOMP - 12801) 25970,15970,25970                             04560040
15970 IVPASS = IVPASS + 1                                               04570040
      WRITE (I02,80001) IVTNUM                                          04580040
      GO TO 5981                                                        04590040
25970 IVFAIL = IVFAIL + 1                                               04600040
      IVCORR = 12801                                                    04610040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04620040
C                                                                       04630040
C     TEST 598 THROUGH TEST 614 CONTAIN TWO INTEGER VARIABLES, AN       04640040
C     INTEGER CONSTANT AND OPERATOR / IN AN ARITHMETIC EXPRESSION.      04650040
C                                                                       04660040
C         TEST 598 THROUGH TEST 603  -  NO PARENS TO GROUP ELEMENTS BUT 04670040
C                   THERE ARE PARENS SURROUNDING NEGATIVE CONSTANTS     04680040
C                                                                       04690040
C     TEST 598 AND TEST 599  -  IV = IV/IV/IC.                          04700040
C                                                                       04710040
 5981 CONTINUE                                                          04720040
      IVTNUM = 598                                                      04730040
C                                                                       04740040
C      ****  TEST 598  ****                                             04750040
C                                                                       04760040
      IF (ICZERO) 35980, 5980, 35980                                    04770040
 5980 CONTINUE                                                          04780040
      IVON01 = 32766                                                    04790040
      IVON02 = 2                                                        04800040
      IVCOMP = IVON01/IVON02/3                                          04810040
      GO TO 45980                                                       04820040
35980 IVDELE = IVDELE + 1                                               04830040
      WRITE (I02,80003) IVTNUM                                          04840040
      IF (ICZERO) 45980, 5991, 45980                                    04850040
45980 IF (IVCOMP - 5461) 25980,15980,25980                              04860040
15980 IVPASS = IVPASS + 1                                               04870040
      WRITE (I02,80001) IVTNUM                                          04880040
      GO TO 5991                                                        04890040
25980 IVFAIL = IVFAIL + 1                                               04900040
      IVCORR = 5461                                                     04910040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04920040
 5991 CONTINUE                                                          04930040
      IVTNUM = 599                                                      04940040
C                                                                       04950040
C      ****  TEST 599  ****                                             04960040
C                                                                       04970040
      IF (ICZERO) 35990, 5990, 35990                                    04980040
 5990 CONTINUE                                                          04990040
      IVON01 = 7151                                                     05000040
      IVON02 = 3                                                        05010040
      IVCOMP = IVON01/IVON02/10                                         05020040
      GO TO 45990                                                       05030040
35990 IVDELE = IVDELE + 1                                               05040040
      WRITE (I02,80003) IVTNUM                                          05050040
      IF (ICZERO) 45990, 6001, 45990                                    05060040
45990 IF (IVCOMP -238) 25990,15990,25990                                05070040
15990 IVPASS = IVPASS + 1                                               05080040
      WRITE (I02,80001) IVTNUM                                          05090040
      GO TO 6001                                                        05100040
25990 IVFAIL = IVFAIL + 1                                               05110040
      IVCORR = 238                                                      05120040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05130040
C                                                                       05140040
C     TEST 600 AND TEST 601   -  IV= IV/IC/IV.                          05150040
C                                                                       05160040
 6001 CONTINUE                                                          05170040
      IVTNUM = 600                                                      05180040
C                                                                       05190040
C      ****  TEST 600  ****                                             05200040
C                                                                       05210040
      IF (ICZERO) 36000, 6000, 36000                                    05220040
 6000 CONTINUE                                                          05230040
      IVON01 = -7150                                                    05240040
      IVON03 = -25                                                      05250040
      IVCOMP = IVON01/(-2)/IVON03                                       05260040
      GO TO 46000                                                       05270040
36000 IVDELE = IVDELE + 1                                               05280040
      WRITE (I02,80003) IVTNUM                                          05290040
      IF (ICZERO) 46000, 6011, 46000                                    05300040
46000 IF (IVCOMP + 143)  26000,16000,26000                              05310040
16000 IVPASS = IVPASS + 1                                               05320040
      WRITE (I02,80001) IVTNUM                                          05330040
      GO TO 6011                                                        05340040
26000 IVFAIL = IVFAIL + 1                                               05350040
      IVCORR = -143                                                     05360040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05370040
 6011 CONTINUE                                                          05380040
      IVTNUM = 601                                                      05390040
C                                                                       05400040
C      ****  TEST 601  ****                                             05410040
C                                                                       05420040
      IF (ICZERO) 36010, 6010, 36010                                    05430040
 6010 CONTINUE                                                          05440040
      IVON01 = 32767                                                    05450040
      IVON03 = -1                                                       05460040
      IVCOMP = IVON01/2/IVON03                                          05470040
      GO TO 46010                                                       05480040
36010 IVDELE = IVDELE + 1                                               05490040
      WRITE (I02,80003) IVTNUM                                          05500040
      IF (ICZERO) 46010, 6021, 46010                                    05510040
46010 IF (IVCOMP + 16383) 26010,16010,26010                             05520040
16010 IVPASS = IVPASS + 1                                               05530040
      WRITE (I02,80001) IVTNUM                                          05540040
      GO TO 6021                                                        05550040
26010 IVFAIL = IVFAIL + 1                                               05560040
      IVCORR = -16383                                                   05570040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05580040
 6021 CONTINUE                                                          05590040
      IVTNUM = 602                                                      05600040
C                                                                       05610040
C      ****  TEST 602  ****                                             05620040
C                                                                       05630040
C     TEST 602 AND TEST 603   -  IV=IC/IV/IV                            05640040
C                                                                       05650040
C                                                                       05660040
      IF (ICZERO) 36020, 6020, 36020                                    05670040
 6020 CONTINUE                                                          05680040
      IVON02 = 13                                                       05690040
      IVON03 = 51                                                       05700040
      IVCOMP = 15249/IVON02/IVON03                                      05710040
      GO TO 46020                                                       05720040
36020 IVDELE = IVDELE + 1                                               05730040
      WRITE (I02,80003) IVTNUM                                          05740040
      IF (ICZERO) 46020, 6031, 46020                                    05750040
46020 IF (IVCOMP - 23) 26020,16020,26020                                05760040
16020 IVPASS = IVPASS + 1                                               05770040
      WRITE (I02,80001) IVTNUM                                          05780040
      GO TO 6031                                                        05790040
26020 IVFAIL = IVFAIL + 1                                               05800040
      IVCORR = 23                                                       05810040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05820040
 6031 CONTINUE                                                          05830040
      IVTNUM = 603                                                      05840040
C                                                                       05850040
C      ****  TEST 603  ****                                             05860040
C                                                                       05870040
      IF (ICZERO) 36030, 6030, 36030                                    05880040
 6030 CONTINUE                                                          05890040
      IVON02 = -13                                                      05900040
      IVON03 = -51                                                      05910040
      IVCOMP = -15249/IVON02/IVON03                                     05920040
      GO TO 46030                                                       05930040
36030 IVDELE = IVDELE + 1                                               05940040
      WRITE (I02,80003) IVTNUM                                          05950040
      IF (ICZERO) 46030, 6041, 46030                                    05960040
46030 IF (IVCOMP +23) 26030,16030,26030                                 05970040
16030 IVPASS = IVPASS + 1                                               05980040
      WRITE (I02,80001) IVTNUM                                          05990040
      GO TO 6041                                                        06000040
26030 IVFAIL = IVFAIL + 1                                               06010040
      IVCORR = -23                                                      06020040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06030040
C                                                                       06040040
C     TEST 604 THROUGH TEST 614  - PARENTHESES ARE USED TO GROUP        06050040
C     ELEMENTS IN THE ARITHMETIC EXPRESSIONS.                           06060040
C                                                                       06070040
C     TEST 604 AND TEST 605  -  IV=(IV/IV)/IC.                          06080040
C                                                                       06090040
 6041 CONTINUE                                                          06100040
      IVTNUM = 604                                                      06110040
C                                                                       06120040
C      ****  TEST 604  ****                                             06130040
C                                                                       06140040
      IF (ICZERO) 36040, 6040, 36040                                    06150040
 6040 CONTINUE                                                          06160040
      IVON01 = 32766                                                    06170040
      IVON02 = 2                                                        06180040
      IVCOMP =(IVON01/IVON02)/3                                         06190040
      GO TO 46040                                                       06200040
36040 IVDELE = IVDELE + 1                                               06210040
      WRITE (I02,80003) IVTNUM                                          06220040
      IF (ICZERO) 46040, 6051, 46040                                    06230040
46040 IF (IVCOMP -5461) 26040,16040,26040                               06240040
16040 IVPASS = IVPASS + 1                                               06250040
      WRITE (I02,80001) IVTNUM                                          06260040
      GO TO 6051                                                        06270040
26040 IVFAIL = IVFAIL + 1                                               06280040
      IVCORR = 5461                                                     06290040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06300040
 6051 CONTINUE                                                          06310040
      IVTNUM = 605                                                      06320040
C                                                                       06330040
C      ****  TEST 605  ****                                             06340040
C                                                                       06350040
      IF (ICZERO) 36050, 6050, 36050                                    06360040
 6050 CONTINUE                                                          06370040
      IVON01 = 7151                                                     06380040
      IVON02 =  3                                                       06390040
      IVCOMP = (IVON01/IVON02)/10                                       06400040
      GO TO 46050                                                       06410040
36050 IVDELE = IVDELE + 1                                               06420040
      WRITE (I02,80003) IVTNUM                                          06430040
      IF (ICZERO) 46050, 6061, 46050                                    06440040
46050 IF (IVCOMP - 238) 26050,16050,26050                               06450040
16050 IVPASS = IVPASS + 1                                               06460040
      WRITE (I02,80001) IVTNUM                                          06470040
      GO TO 6061                                                        06480040
26050 IVFAIL = IVFAIL + 1                                               06490040
      IVCORR = 238                                                      06500040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06510040
C                                                                       06520040
C     TEST 606 AND TEST 607  -  IV=IV/(IV/IC).                          06530040
C                                                                       06540040
 6061 CONTINUE                                                          06550040
      IVTNUM = 606                                                      06560040
C                                                                       06570040
C      ****  TEST 606  ****                                             06580040
C                                                                       06590040
      IF (ICZERO) 36060, 6060, 36060                                    06600040
 6060 CONTINUE                                                          06610040
      IVON01 = -7154                                                    06620040
      IVON02 =  26                                                      06630040
      IVCOMP = IVON01/(IVON02/5)                                        06640040
      GO TO 46060                                                       06650040
36060 IVDELE = IVDELE + 1                                               06660040
      WRITE (I02,80003) IVTNUM                                          06670040
      IF (ICZERO) 46060, 6071, 46060                                    06680040
46060 IF (IVCOMP + 1430) 26060,16060,26060                              06690040
16060 IVPASS = IVPASS + 1                                               06700040
      WRITE (I02,80001) IVTNUM                                          06710040
      GO TO 6071                                                        06720040
26060 IVFAIL = IVFAIL + 1                                               06730040
      IVCORR = -1430                                                    06740040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06750040
 6071 CONTINUE                                                          06760040
      IVTNUM = 607                                                      06770040
C                                                                       06780040
C      ****  TEST 607  ****                                             06790040
C                                                                       06800040
      IF (ICZERO) 36070, 6070, 36070                                    06810040
 6070 CONTINUE                                                          06820040
      IVON01 = 29                                                       06830040
      IVON02 = -5                                                       06840040
      IVCOMP = IVON01/(IVON02/2)                                        06850040
      GO TO 46070                                                       06860040
36070 IVDELE = IVDELE + 1                                               06870040
      WRITE (I02,80003) IVTNUM                                          06880040
      IF (ICZERO) 46070, 6081, 46070                                    06890040
46070 IF (IVCOMP + 14) 26070,16070,26070                                06900040
16070 IVPASS = IVPASS + 1                                               06910040
      WRITE (I02,80001) IVTNUM                                          06920040
      GO TO 6081                                                        06930040
26070 IVFAIL = IVFAIL + 1                                               06940040
      IVCORR = -14                                                      06950040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06960040
C                                                                       06970040
C     TEST 608 AND TEST 609  -  IV = (IV/IC)/IV.                        06980040
C                                                                       06990040
 6081 CONTINUE                                                          07000040
      IVTNUM = 608                                                      07010040
C                                                                       07020040
C      ****  TEST 608  ****                                             07030040
C                                                                       07040040
      IF (ICZERO) 36080, 6080, 36080                                    07050040
 6080 CONTINUE                                                          07060040
      IVON01 = 24                                                       07070040
      IVON03 =  3                                                       07080040
      IVCOMP = (IVON01/3)/IVON03                                        07090040
      GO TO 46080                                                       07100040
36080 IVDELE = IVDELE + 1                                               07110040
      WRITE (I02,80003) IVTNUM                                          07120040
      IF (ICZERO) 46080, 6091, 46080                                    07130040
46080 IF (IVCOMP -2) 26080,16080,26080                                  07140040
16080 IVPASS = IVPASS + 1                                               07150040
      WRITE (I02,80001) IVTNUM                                          07160040
      GO TO 6091                                                        07170040
26080 IVFAIL = IVFAIL + 1                                               07180040
      IVCORR = 2                                                        07190040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07200040
 6091 CONTINUE                                                          07210040
      IVTNUM = 609                                                      07220040
C                                                                       07230040
C      ****  TEST 609  ****                                             07240040
C                                                                       07250040
      IF (ICZERO) 36090, 6090, 36090                                    07260040
 6090 CONTINUE                                                          07270040
      IVON01 = 7151                                                     07280040
      IVON03 = 10                                                       07290040
      IVCOMP = (IVON01/(-3))/IVON03                                     07300040
      GO TO 46090                                                       07310040
36090 IVDELE = IVDELE + 1                                               07320040
      WRITE (I02,80003) IVTNUM                                          07330040
      IF (ICZERO) 46090, 6101, 46090                                    07340040
46090 IF (IVCOMP + 238) 26090,16090,26090                               07350040
16090 IVPASS = IVPASS + 1                                               07360040
      WRITE (I02,80001) IVTNUM                                          07370040
      GO TO 6101                                                        07380040
26090 IVFAIL = IVFAIL + 1                                               07390040
      IVCORR = -238                                                     07400040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07410040
C                                                                       07420040
C     TEST 610 AND TEST 611  -  IV=IV(IC/IV)                            07430040
C                                                                       07440040
 6101 CONTINUE                                                          07450040
      IVTNUM = 610                                                      07460040
C                                                                       07470040
C      ****  TEST 610  ****                                             07480040
C                                                                       07490040
      IF (ICZERO) 36100, 6100, 36100                                    07500040
 6100 CONTINUE                                                          07510040
      IVON01 = -7154                                                    07520040
      IVON03 = -5                                                       07530040
      IVCOMP = IVON01/((-26)/IVON03)                                    07540040
      GO TO 46100                                                       07550040
36100 IVDELE = IVDELE + 1                                               07560040
      WRITE (I02,80003) IVTNUM                                          07570040
      IF (ICZERO) 46100, 6111, 46100                                    07580040
46100 IF (IVCOMP + 1430) 26100,16100,26100                              07590040
16100 IVPASS = IVPASS + 1                                               07600040
      WRITE (I02,80001) IVTNUM                                          07610040
      GO TO 6111                                                        07620040
26100 IVFAIL = IVFAIL + 1                                               07630040
      IVCORR = -1430                                                    07640040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07650040
 6111 CONTINUE                                                          07660040
      IVTNUM = 611                                                      07670040
C                                                                       07680040
C      ****  TEST 611  ****                                             07690040
C                                                                       07700040
      IF (ICZERO) 36110, 6110, 36110                                    07710040
 6110 CONTINUE                                                          07720040
      IVON01 = 7150                                                     07730040
      IVON03 = 5                                                        07740040
      IVCOMP = IVON01/((+25)/IVON03)                                    07750040
      GO TO 46110                                                       07760040
36110 IVDELE = IVDELE + 1                                               07770040
      WRITE (I02,80003) IVTNUM                                          07780040
      IF (ICZERO) 46110, 6121, 46110                                    07790040
46110 IF (IVCOMP -1430) 26110,16110,26110                               07800040
16110 IVPASS = IVPASS + 1                                               07810040
      WRITE (I02,80001) IVTNUM                                          07820040
      GO TO 6121                                                        07830040
26110 IVFAIL = IVFAIL + 1                                               07840040
      IVCORR = 1430                                                     07850040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07860040
 6121 CONTINUE                                                          07870040
      IVTNUM = 612                                                      07880040
C                                                                       07890040
C      ****  TEST 612  ****                                             07900040
C     TEST 612  -  IV= (IC/IV)/IV                                       07910040
C                                                                       07920040
      IF (ICZERO) 36120, 6120, 36120                                    07930040
 6120 CONTINUE                                                          07940040
      IVON02 = -3                                                       07950040
      IVON03 = -10                                                      07960040
      IVCOMP = (-7154/IVON02)/IVON03                                    07970040
      GO TO 46120                                                       07980040
36120 IVDELE = IVDELE + 1                                               07990040
      WRITE (I02,80003) IVTNUM                                          08000040
      IF (ICZERO) 46120, 6131, 46120                                    08010040
46120 IF (IVCOMP + 238) 26120,16120,26120                               08020040
16120 IVPASS = IVPASS + 1                                               08030040
      WRITE (I02,80001) IVTNUM                                          08040040
      GO TO 6131                                                        08050040
26120 IVFAIL = IVFAIL + 1                                               08060040
      IVCORR = -238                                                     08070040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08080040
C                                                                       08090040
C     TEST 613 AND TEST 614  -  IV=IC/(IV/IV)                           08100040
C                                                                       08110040
 6131 CONTINUE                                                          08120040
      IVTNUM = 613                                                      08130040
C                                                                       08140040
C      ****  TEST 613  ****                                             08150040
C                                                                       08160040
      IF (ICZERO) 36130, 6130, 36130                                    08170040
 6130 CONTINUE                                                          08180040
      IVON02 = 8                                                        08190040
      IVON03 = 4                                                        08200040
      IVCOMP = 24/(IVON02/IVON03)                                       08210040
      GO TO 46130                                                       08220040
36130 IVDELE = IVDELE + 1                                               08230040
      WRITE (I02,80003) IVTNUM                                          08240040
      IF (ICZERO) 46130, 6141, 46130                                    08250040
46130 IF (IVCOMP - 12) 26130,16130,26130                                08260040
16130 IVPASS = IVPASS + 1                                               08270040
      WRITE (I02,80001) IVTNUM                                          08280040
      GO TO 6141                                                        08290040
26130 IVFAIL = IVFAIL + 1                                               08300040
      IVCORR = 12                                                       08310040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08320040
 6141 CONTINUE                                                          08330040
      IVTNUM = 614                                                      08340040
C                                                                       08350040
C      ****  TEST 614  ****                                             08360040
C                                                                       08370040
      IF (ICZERO) 36140, 6140, 36140                                    08380040
 6140 CONTINUE                                                          08390040
      IVON02 = 25                                                       08400040
      IVON03 = 5                                                        08410040
      IVCOMP = 7150/(-(IVON02/IVON03))                                  08420040
      GO TO 46140                                                       08430040
36140 IVDELE = IVDELE + 1                                               08440040
      WRITE (I02,80003) IVTNUM                                          08450040
      IF (ICZERO) 46140, 6151, 46140                                    08460040
46140 IF (IVCOMP + 1430) 26140,16140,26140                              08470040
16140 IVPASS = IVPASS + 1                                               08480040
      WRITE (I02,80001) IVTNUM                                          08490040
      GO TO 6151                                                        08500040
26140 IVFAIL = IVFAIL + 1                                               08510040
      IVCORR = -1430                                                    08520040
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08530040
C      ****    END OF TESTS    ****                                     08540040
 6151 CONTINUE                                                          08550040
C                                                                       08560040
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08570040
99999 CONTINUE                                                          08580040
      WRITE (I02,90002)                                                 08590040
      WRITE (I02,90006)                                                 08600040
      WRITE (I02,90002)                                                 08610040
      WRITE (I02,90002)                                                 08620040
      WRITE (I02,90007)                                                 08630040
      WRITE (I02,90002)                                                 08640040
      WRITE (I02,90008)  IVFAIL                                         08650040
      WRITE (I02,90009) IVPASS                                          08660040
      WRITE (I02,90010) IVDELE                                          08670040
C                                                                       08680040
C                                                                       08690040
C     TERMINATE ROUTINE EXECUTION                                       08700040
      STOP                                                              08710040
C                                                                       08720040
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08730040
90000 FORMAT (1H1)                                                      08740040
90002 FORMAT (1H )                                                      08750040
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08760040
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08770040
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08780040
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08790040
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08800040
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08810040
C                                                                       08820040
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08830040
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08840040
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08850040
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08860040
C                                                                       08870040
C     FORMAT STATEMENTS FOR TEST RESULTS                                08880040
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08890040
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08900040
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08910040
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08920040
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08930040
C                                                                       08940040
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM040)                          08950040
      END                                                               08960040
