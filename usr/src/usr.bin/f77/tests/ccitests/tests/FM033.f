C     COMMENT SECTION                                                   00010033
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020033
C     FM033                                                             00030033
C                                                                       00040033
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050033
C     FORM                                                              00060033
C             INTEGER VARIABLE = ARITHMETIC EXPRESSION                  00070033
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080033
C     OPERATOR * AND INTEGER CONSTANTS.  SOME OF THE TESTS USE PARENS   00090033
C     TO GROUP ELEMENTS IN THE EXPRESSION AND TO ALLOW THE USE OF       00100033
C     NEGATIVE CONSTANTS FOLLOWING THE * OPERATOR.                      00110033
C                                                                       00120033
C     THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS          00130033
C         (1)  INTEGER CONSTANT * INTEGER CONSTANT                      00140033
C         (2)  INTEGER CONSTANT * INTEGER CONSTANT * INTEGER CONSTANT   00150033
C         (3)  SAME AS (2) BUT WITH PARENS TO GROUP ELEMENTS            00160033
C                                                                       00170033
C      REFERENCES                                                       00180033
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00190033
C              X3.9-1978                                                00200033
C                                                                       00210033
C        SECTION 4.3, INTEGER TYPE                                      00220033
C        SECTION 4.3.1, INTEGER CONSTANT                                00230033
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00240033
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00250033
C                                                                       00260033
C      **********************************************************       00270033
C                                                                       00280033
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00290033
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00300033
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00310033
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00320033
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00330033
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00340033
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00350033
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00360033
C     OF EXECUTING THESE TESTS.                                         00370033
C                                                                       00380033
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00390033
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00400033
C                                                                       00410033
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00420033
C                                                                       00430033
C                  DEPARTMENT OF THE NAVY                               00440033
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00450033
C                  WASHINGTON, D.C.  20376                              00460033
C                                                                       00470033
C      **********************************************************       00480033
C                                                                       00490033
C                                                                       00500033
C                                                                       00510033
C     INITIALIZATION SECTION                                            00520033
C                                                                       00530033
C     INITIALIZE CONSTANTS                                              00540033
C      **************                                                   00550033
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00560033
      I01 = 5                                                           00570033
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00580033
      I02 = 6                                                           00590033
C     SYSTEM ENVIRONMENT SECTION                                        00600033
C                                                                       00610033
      I01 = 5                                                           00620033
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00630033
C     (UNIT NUMBER FOR CARD READER).                                    00640033
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00650033
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00660033
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00670033
C                                                                       00680033
      I02 = 6                                                           00690033
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00700033
C     (UNIT NUMBER FOR PRINTER).                                        00710033
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00720033
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00730033
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00740033
C                                                                       00750033
      IVPASS=0                                                          00760033
      IVFAIL=0                                                          00770033
      IVDELE=0                                                          00780033
      ICZERO=0                                                          00790033
C                                                                       00800033
C     WRITE PAGE HEADERS                                                00810033
      WRITE (I02,90000)                                                 00820033
      WRITE (I02,90001)                                                 00830033
      WRITE (I02,90002)                                                 00840033
      WRITE (I02, 90002)                                                00850033
      WRITE (I02,90003)                                                 00860033
      WRITE (I02,90002)                                                 00870033
      WRITE (I02,90004)                                                 00880033
      WRITE (I02,90002)                                                 00890033
      WRITE (I02,90011)                                                 00900033
      WRITE (I02,90002)                                                 00910033
      WRITE (I02,90002)                                                 00920033
      WRITE (I02,90005)                                                 00930033
      WRITE (I02,90006)                                                 00940033
      WRITE (I02,90002)                                                 00950033
C                                                                       00960033
C     TEST SECTION                                                      00970033
C                                                                       00980033
C         ARITHMETIC ASSIGNMENT STATEMENT                               00990033
C                                                                       01000033
C     TEST 360 THROUGH TEST 376 CONTAIN TWO INTEGER CONSTANTS AND       01010033
C     OPERATOR * IN AN ARITHMETIC EXPRESSION.                           01020033
C              IV = IC * IC                                             01030033
C                                                                       01040033
C     TEST 360 THROUGH TEST 365  - INTEGER CONSTANTS ARE POSITIVE       01050033
C                                                                       01060033
 3601 CONTINUE                                                          01070033
      IVTNUM = 360                                                      01080033
C                                                                       01090033
C       ****  TEST 360  ****                                            01100033
C                                                                       01110033
      IF (ICZERO) 33600, 3600, 33600                                    01120033
 3600 CONTINUE                                                          01130033
      IVCOMP = 2 * 3                                                    01140033
      GO TO 43600                                                       01150033
33600 IVDELE = IVDELE + 1                                               01160033
      WRITE (I02,80003) IVTNUM                                          01170033
      IF (ICZERO) 43600, 3611, 43600                                    01180033
43600 IF (IVCOMP - 6) 23600,13600,23600                                 01190033
13600 IVPASS = IVPASS + 1                                               01200033
      WRITE (I02,80001) IVTNUM                                          01210033
      GO TO 3611                                                        01220033
23600 IVFAIL = IVFAIL + 1                                               01230033
      IVCORR=6                                                          01240033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01250033
 3611 CONTINUE                                                          01260033
      IVTNUM = 361                                                      01270033
C                                                                       01280033
C      ****  TEST 361  ****                                             01290033
C                                                                       01300033
      IF (ICZERO) 33610, 3610, 33610                                    01310033
 3610 CONTINUE                                                          01320033
      IVCOMP = 3*2                                                      01330033
      GO TO 43610                                                       01340033
33610 IVDELE = IVDELE + 1                                               01350033
      WRITE (I02,80003) IVTNUM                                          01360033
      IF (ICZERO) 43610, 3621, 43610                                    01370033
43610 IF (IVCOMP-6) 23610,13610,23610                                   01380033
13610 IVPASS = IVPASS + 1                                               01390033
      WRITE (I02,80001) IVTNUM                                          01400033
      GO TO 3621                                                        01410033
23610 IVFAIL = IVFAIL + 1                                               01420033
      IVCORR=6                                                          01430033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01440033
 3621 CONTINUE                                                          01450033
      IVTNUM = 362                                                      01460033
C                                                                       01470033
C      ****  TEST 362  ****                                             01480033
C                                                                       01490033
      IF (ICZERO) 33620, 3620, 33620                                    01500033
 3620 CONTINUE                                                          01510033
      IVCOMP=13*11                                                      01520033
      GO TO 43620                                                       01530033
33620 IVDELE = IVDELE + 1                                               01540033
      WRITE (I02,80003) IVTNUM                                          01550033
      IF (ICZERO) 43620, 3631, 43620                                    01560033
43620 IF (IVCOMP-143) 23620,13620,23620                                 01570033
13620 IVPASS = IVPASS + 1                                               01580033
      WRITE (I02,80001) IVTNUM                                          01590033
      GO TO 3631                                                        01600033
23620 IVFAIL = IVFAIL + 1                                               01610033
      IVCORR=143                                                        01620033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01630033
 3631 CONTINUE                                                          01640033
      IVTNUM = 363                                                      01650033
C                                                                       01660033
C      ****  TEST 363  ****                                             01670033
C                                                                       01680033
      IF (ICZERO) 33630, 3630, 33630                                    01690033
 3630 CONTINUE                                                          01700033
      IVCOMP = 223*99                                                   01710033
      GO TO 43630                                                       01720033
33630 IVDELE = IVDELE + 1                                               01730033
      WRITE (I02,80003) IVTNUM                                          01740033
      IF (ICZERO) 43630, 3641, 43630                                    01750033
43630 IF (IVCOMP-22077) 23630,13630,23630                               01760033
13630 IVPASS = IVPASS + 1                                               01770033
      WRITE (I02,80001) IVTNUM                                          01780033
      GO TO 3641                                                        01790033
23630 IVFAIL = IVFAIL + 1                                               01800033
      IVCORR=22077                                                      01810033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01820033
 3641 CONTINUE                                                          01830033
      IVTNUM = 364                                                      01840033
C                                                                       01850033
C      ****  TEST 364  ****                                             01860033
C                                                                       01870033
      IF (ICZERO) 33640, 3640, 33640                                    01880033
 3640 CONTINUE                                                          01890033
      IVCOMP=11235*2                                                    01900033
      GO TO 43640                                                       01910033
33640 IVDELE = IVDELE + 1                                               01920033
      WRITE (I02,80003) IVTNUM                                          01930033
      IF (ICZERO) 43640, 3651, 43640                                    01940033
43640 IF (IVCOMP-22470) 23640,13640,23640                               01950033
13640 IVPASS = IVPASS + 1                                               01960033
      WRITE (I02,80001) IVTNUM                                          01970033
      GO TO 3651                                                        01980033
23640 IVFAIL = IVFAIL + 1                                               01990033
      IVCORR=22470                                                      02000033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02010033
 3651 CONTINUE                                                          02020033
      IVTNUM = 365                                                      02030033
C                                                                       02040033
C      ****  TEST 365  ****                                             02050033
C                                                                       02060033
      IF (ICZERO) 33650, 3650, 33650                                    02070033
 3650 CONTINUE                                                          02080033
      IVCOMP = 2*16383                                                  02090033
      GO TO 43650                                                       02100033
33650 IVDELE = IVDELE + 1                                               02110033
      WRITE (I02,80003) IVTNUM                                          02120033
      IF (ICZERO) 43650, 3661, 43650                                    02130033
43650 IF (IVCOMP-32766) 23650,13650,23650                               02140033
13650 IVPASS = IVPASS + 1                                               02150033
      WRITE (I02,80001) IVTNUM                                          02160033
      GO TO 3661                                                        02170033
23650 IVFAIL = IVFAIL + 1                                               02180033
      IVCORR = 32766                                                    02190033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02200033
C                                                                       02210033
C     TEST 366 THROUGH TEST 371                                         02220033
C         ONE POSITIVE AND ONE NEGATIVE CONSTANT                        02230033
C                                                                       02240033
 3661 CONTINUE                                                          02250033
      IVTNUM = 366                                                      02260033
C                                                                       02270033
C      ****  TEST 366  ****                                             02280033
C                                                                       02290033
      IF (ICZERO) 33660, 3660, 33660                                    02300033
 3660 CONTINUE                                                          02310033
      IVCOMP =2*(-3)                                                    02320033
      GO TO 43660                                                       02330033
33660 IVDELE = IVDELE + 1                                               02340033
      WRITE (I02,80003) IVTNUM                                          02350033
      IF (ICZERO) 43660, 3671, 43660                                    02360033
43660 IF (IVCOMP+6) 23660,13660,23660                                   02370033
13660 IVPASS = IVPASS + 1                                               02380033
      WRITE (I02,80001) IVTNUM                                          02390033
      GO TO 3671                                                        02400033
23660 IVFAIL = IVFAIL + 1                                               02410033
      IVCORR = -6                                                       02420033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02430033
 3671 CONTINUE                                                          02440033
      IVTNUM = 367                                                      02450033
C                                                                       02460033
C      ****  TEST 367  ****                                             02470033
C                                                                       02480033
      IF (ICZERO) 33670, 3670, 33670                                    02490033
 3670 CONTINUE                                                          02500033
      IVCOMP=(-2)*3                                                     02510033
      GO TO 43670                                                       02520033
33670 IVDELE = IVDELE + 1                                               02530033
      WRITE (I02,80003) IVTNUM                                          02540033
      IF (ICZERO) 43670, 3681, 43670                                    02550033
43670 IF (IVCOMP+6)23670,13670,23670                                    02560033
13670 IVPASS = IVPASS + 1                                               02570033
      WRITE (I02,80001) IVTNUM                                          02580033
      GO TO 3681                                                        02590033
23670 IVFAIL = IVFAIL + 1                                               02600033
      IVCORR =-6                                                        02610033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02620033
 3681 CONTINUE                                                          02630033
      IVTNUM = 368                                                      02640033
C                                                                       02650033
C      ****  TEST 368  ****                                             02660033
C                                                                       02670033
      IF (ICZERO) 33680, 3680, 33680                                    02680033
 3680 CONTINUE                                                          02690033
      IVCOMP= -2*3                                                      02700033
      GO TO 43680                                                       02710033
33680 IVDELE = IVDELE + 1                                               02720033
      WRITE (I02,80003) IVTNUM                                          02730033
      IF (ICZERO) 43680, 3691, 43680                                    02740033
43680 IF (IVCOMP +6) 23680,13680,23680                                  02750033
13680 IVPASS = IVPASS + 1                                               02760033
      WRITE (I02,80001) IVTNUM                                          02770033
      GO TO 3691                                                        02780033
23680 IVFAIL = IVFAIL + 1                                               02790033
      IVCORR=-6                                                         02800033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02810033
 3691 CONTINUE                                                          02820033
      IVTNUM = 369                                                      02830033
C                                                                       02840033
C      ****  TEST 369  ****                                             02850033
C                                                                       02860033
      IF (ICZERO) 33690, 3690, 33690                                    02870033
 3690 CONTINUE                                                          02880033
      IVCOMP = (-13)*11                                                 02890033
      GO TO 43690                                                       02900033
33690 IVDELE = IVDELE + 1                                               02910033
      WRITE (I02,80003) IVTNUM                                          02920033
      IF (ICZERO) 43690, 3701, 43690                                    02930033
43690 IF (IVCOMP+143) 23690,13690,23690                                 02940033
13690 IVPASS = IVPASS + 1                                               02950033
      WRITE (I02,80001) IVTNUM                                          02960033
      GO TO 3701                                                        02970033
23690 IVFAIL = IVFAIL + 1                                               02980033
      IVCORR=-143                                                       02990033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03000033
 3701 CONTINUE                                                          03010033
      IVTNUM = 370                                                      03020033
C                                                                       03030033
C      ****  TEST 370  ****                                             03040033
C                                                                       03050033
      IF (ICZERO) 33700, 3700, 33700                                    03060033
 3700 CONTINUE                                                          03070033
      IVCOMP = 223 * (-99)                                              03080033
      GO TO 43700                                                       03090033
33700 IVDELE = IVDELE + 1                                               03100033
      WRITE (I02,80003) IVTNUM                                          03110033
      IF (ICZERO) 43700, 3711, 43700                                    03120033
43700 IF (IVCOMP + 22077) 23700,13700,23700                             03130033
13700 IVPASS = IVPASS + 1                                               03140033
      WRITE (I02,80001) IVTNUM                                          03150033
      GO TO 3711                                                        03160033
23700 IVFAIL = IVFAIL + 1                                               03170033
      IVCORR =-22077                                                    03180033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03190033
 3711 CONTINUE                                                          03200033
      IVTNUM = 371                                                      03210033
C                                                                       03220033
C      ****  TEST 371  ****                                             03230033
C                                                                       03240033
      IF (ICZERO) 33710, 3710, 33710                                    03250033
 3710 CONTINUE                                                          03260033
      IVCOMP= -2 * 16383                                                03270033
      GO TO 43710                                                       03280033
33710 IVDELE = IVDELE + 1                                               03290033
      WRITE (I02,80003) IVTNUM                                          03300033
      IF (ICZERO) 43710, 3721, 43710                                    03310033
43710 IF (IVCOMP+32766) 23710,13710,23710                               03320033
13710 IVPASS = IVPASS + 1                                               03330033
      WRITE (I02,80001) IVTNUM                                          03340033
      GO TO 3721                                                        03350033
23710 IVFAIL = IVFAIL + 1                                               03360033
      IVCORR= -32766                                                    03370033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03380033
C                                                                       03390033
C     TEST 372 THROUGH TEST 376 - TWO NEGATIVE CONSTANTS                03400033
C                                                                       03410033
 3721 CONTINUE                                                          03420033
      IVTNUM = 372                                                      03430033
C                                                                       03440033
C      ****  TEST 372  ****                                             03450033
C                                                                       03460033
      IF (ICZERO) 33720, 3720, 33720                                    03470033
 3720 CONTINUE                                                          03480033
      IVCOMP=(-2)*(-3)                                                  03490033
      GO TO 43720                                                       03500033
33720 IVDELE = IVDELE + 1                                               03510033
      WRITE (I02,80003) IVTNUM                                          03520033
      IF (ICZERO) 43720, 3731, 43720                                    03530033
43720 IF (IVCOMP-6) 23720,13720,23720                                   03540033
13720 IVPASS = IVPASS + 1                                               03550033
      WRITE (I02,80001) IVTNUM                                          03560033
      GO TO 3731                                                        03570033
23720 IVFAIL = IVFAIL + 1                                               03580033
      IVCORR=6                                                          03590033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03600033
 3731 CONTINUE                                                          03610033
      IVTNUM = 373                                                      03620033
C                                                                       03630033
C      ****  TEST 373  ****                                             03640033
C                                                                       03650033
      IF (ICZERO) 33730, 3730, 33730                                    03660033
 3730 CONTINUE                                                          03670033
      IVCOMP = -2*(-3)                                                  03680033
      GO TO 43730                                                       03690033
33730 IVDELE = IVDELE + 1                                               03700033
      WRITE (I02,80003) IVTNUM                                          03710033
      IF (ICZERO) 43730, 3741, 43730                                    03720033
43730 IF (IVCOMP-6) 23730,13730,23730                                   03730033
13730 IVPASS = IVPASS + 1                                               03740033
      WRITE (I02,80001) IVTNUM                                          03750033
      GO TO 3741                                                        03760033
23730 IVFAIL = IVFAIL + 1                                               03770033
      IVCORR=6                                                          03780033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03790033
 3741 CONTINUE                                                          03800033
      IVTNUM = 374                                                      03810033
C                                                                       03820033
C      ****  TEST 374  ****                                             03830033
C                                                                       03840033
      IF (ICZERO) 33740, 3740, 33740                                    03850033
 3740 CONTINUE                                                          03860033
      IVCOMP=(-13)*(-11)                                                03870033
      GO TO 43740                                                       03880033
33740 IVDELE = IVDELE + 1                                               03890033
      WRITE (I02,80003) IVTNUM                                          03900033
      IF (ICZERO) 43740, 3751, 43740                                    03910033
43740 IF (IVCOMP-143) 23740,13740,23740                                 03920033
13740 IVPASS = IVPASS + 1                                               03930033
      WRITE (I02,80001) IVTNUM                                          03940033
      GO TO 3751                                                        03950033
23740 IVFAIL = IVFAIL + 1                                               03960033
      IVCORR = 143                                                      03970033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03980033
 3751 CONTINUE                                                          03990033
      IVTNUM = 375                                                      04000033
C                                                                       04010033
C      ****  TEST 375  ****                                             04020033
C                                                                       04030033
      IF (ICZERO) 33750, 3750, 33750                                    04040033
 3750 CONTINUE                                                          04050033
      IVCOMP= -223 *(-99)                                               04060033
      GO TO 43750                                                       04070033
33750 IVDELE = IVDELE + 1                                               04080033
      WRITE (I02,80003) IVTNUM                                          04090033
      IF (ICZERO) 43750, 3761, 43750                                    04100033
43750 IF (IVCOMP - 22077) 23750,13750,23750                             04110033
13750 IVPASS = IVPASS + 1                                               04120033
      WRITE (I02,80001) IVTNUM                                          04130033
      GO TO 3761                                                        04140033
23750 IVFAIL = IVFAIL + 1                                               04150033
      IVCORR = 22077                                                    04160033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04170033
 3761 CONTINUE                                                          04180033
      IVTNUM = 376                                                      04190033
C                                                                       04200033
C      ****  TEST 376  ****                                             04210033
C                                                                       04220033
      IF (ICZERO) 33760, 3760, 33760                                    04230033
 3760 CONTINUE                                                          04240033
      IVCOMP = (-16383)*(-2)                                            04250033
      GO TO 43760                                                       04260033
33760 IVDELE = IVDELE + 1                                               04270033
      WRITE (I02,80003) IVTNUM                                          04280033
      IF (ICZERO) 43760, 3771, 43760                                    04290033
43760 IF (IVCOMP - 32766) 23760,13760,23760                             04300033
13760 IVPASS = IVPASS + 1                                               04310033
      WRITE (I02,80001) IVTNUM                                          04320033
      GO TO 3771                                                        04330033
23760 IVFAIL = IVFAIL + 1                                               04340033
      IVCORR =32766                                                     04350033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04360033
C                                                                       04370033
C     TEST 377 THROUGH TEST 394 CONTAIN THREE INTEGER CONSTANTS AND     04380033
C     OPERATOR * IN AN ARITHMETIC EXPRESSION.                           04390033
C               IV = IC * IC * IC                                       04400033
C                                                                       04410033
C     TEST 377 THROUGH TEST 382   - CONSTANTS ARE POSITIVE              04420033
C                                                                       04430033
 3771 CONTINUE                                                          04440033
      IVTNUM = 377                                                      04450033
C                                                                       04460033
C      ****  TEST 377  ****                                             04470033
C                                                                       04480033
      IF (ICZERO) 33770, 3770, 33770                                    04490033
 3770 CONTINUE                                                          04500033
      IVCOMP =2*3*4                                                     04510033
      GO TO 43770                                                       04520033
33770 IVDELE = IVDELE + 1                                               04530033
      WRITE (I02,80003) IVTNUM                                          04540033
      IF (ICZERO) 43770, 3781, 43770                                    04550033
43770 IF (IVCOMP-24) 23770,13770,23770                                  04560033
13770 IVPASS = IVPASS + 1                                               04570033
      WRITE (I02,80001) IVTNUM                                          04580033
      GO TO 3781                                                        04590033
23770 IVFAIL = IVFAIL + 1                                               04600033
      IVCORR = 24                                                       04610033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04620033
 3781 CONTINUE                                                          04630033
      IVTNUM = 378                                                      04640033
C                                                                       04650033
C      ****  TEST 378  ****                                             04660033
C                                                                       04670033
      IF (ICZERO) 33780, 3780, 33780                                    04680033
 3780 CONTINUE                                                          04690033
      IVCOMP = 2*3*55                                                   04700033
      GO TO 43780                                                       04710033
33780 IVDELE = IVDELE + 1                                               04720033
      WRITE (I02,80003) IVTNUM                                          04730033
      IF (ICZERO) 43780, 3791, 43780                                    04740033
43780 IF (IVCOMP-330) 23780,13780,23780                                 04750033
13780 IVPASS = IVPASS + 1                                               04760033
      WRITE (I02,80001) IVTNUM                                          04770033
      GO TO 3791                                                        04780033
23780 IVFAIL = IVFAIL + 1                                               04790033
      IVCORR = 330                                                      04800033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04810033
 3791 CONTINUE                                                          04820033
      IVTNUM = 379                                                      04830033
C                                                                       04840033
C      ****  TEST 379  ****                                             04850033
C                                                                       04860033
      IF (ICZERO) 33790, 3790, 33790                                    04870033
 3790 CONTINUE                                                          04880033
      IVCOMP = 23*51*13                                                 04890033
      GO TO 43790                                                       04900033
33790 IVDELE = IVDELE + 1                                               04910033
      WRITE (I02,80003) IVTNUM                                          04920033
      IF (ICZERO) 43790, 3801, 43790                                    04930033
43790 IF (IVCOMP-15249) 23790,13790,23790                               04940033
13790 IVPASS = IVPASS + 1                                               04950033
      WRITE (I02,80001) IVTNUM                                          04960033
      GO TO 3801                                                        04970033
23790 IVFAIL = IVFAIL + 1                                               04980033
      IVCORR = 15249                                                    04990033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05000033
 3801 CONTINUE                                                          05010033
      IVTNUM = 380                                                      05020033
C                                                                       05030033
C      ****  TEST 380  ****                                             05040033
C                                                                       05050033
      IF (ICZERO) 33800, 3800, 33800                                    05060033
 3800 CONTINUE                                                          05070033
      IVCOMP = 3* 5461* 2                                               05080033
      GO TO 43800                                                       05090033
33800 IVDELE = IVDELE + 1                                               05100033
      WRITE (I02,80003) IVTNUM                                          05110033
      IF (ICZERO) 43800, 3811, 43800                                    05120033
43800 IF (IVCOMP - 32766) 23800,13800,23800                             05130033
13800 IVPASS = IVPASS + 1                                               05140033
      WRITE (I02,80001) IVTNUM                                          05150033
      GO TO 3811                                                        05160033
23800 IVFAIL = IVFAIL + 1                                               05170033
      IVCORR = 32766                                                    05180033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05190033
 3811 CONTINUE                                                          05200033
      IVTNUM = 381                                                      05210033
C                                                                       05220033
C      ****  TEST 381  ****                                             05230033
C                                                                       05240033
      IF (ICZERO) 33810, 3810, 33810                                    05250033
 3810 CONTINUE                                                          05260033
      IVCOMP = 16383*2*1                                                05270033
      GO TO 43810                                                       05280033
33810 IVDELE = IVDELE + 1                                               05290033
      WRITE (I02,80003) IVTNUM                                          05300033
      IF (ICZERO) 43810, 3821, 43810                                    05310033
43810 IF (IVCOMP-32766) 23810,13810,23810                               05320033
13810 IVPASS = IVPASS + 1                                               05330033
      WRITE (I02,80001) IVTNUM                                          05340033
      GO TO 3821                                                        05350033
23810 IVFAIL = IVFAIL + 1                                               05360033
      IVCORR = 32766                                                    05370033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05380033
 3821 CONTINUE                                                          05390033
      IVTNUM = 382                                                      05400033
C                                                                       05410033
C      ****  TEST 382  ****                                             05420033
C                                                                       05430033
      IF (ICZERO) 33820, 3820, 33820                                    05440033
 3820 CONTINUE                                                          05450033
      IVCOMP = 3*53*157                                                 05460033
      GO TO 43820                                                       05470033
33820 IVDELE = IVDELE + 1                                               05480033
      WRITE (I02,80003) IVTNUM                                          05490033
      IF (ICZERO) 43820, 3831, 43820                                    05500033
43820 IF (IVCOMP-24963) 23820,13820,23820                               05510033
13820 IVPASS = IVPASS + 1                                               05520033
      WRITE (I02,80001) IVTNUM                                          05530033
      GO TO 3831                                                        05540033
23820 IVFAIL = IVFAIL + 1                                               05550033
      IVCORR = 24963                                                    05560033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05570033
C                                                                       05580033
C     TEST 383 THROUGH TEST 386                                         05590033
C         THREE POSITIVE INTEGER CONSTANTS GROUPED WITH PARENS.         05600033
C                                                                       05610033
 3831 CONTINUE                                                          05620033
      IVTNUM = 383                                                      05630033
C                                                                       05640033
C      ****  TEST 383  ****                                             05650033
C                                                                       05660033
      IF (ICZERO) 33830, 3830, 33830                                    05670033
 3830 CONTINUE                                                          05680033
      IVCOMP = (2*3)*4                                                  05690033
      GO TO 43830                                                       05700033
33830 IVDELE = IVDELE + 1                                               05710033
      WRITE (I02,80003) IVTNUM                                          05720033
      IF (ICZERO) 43830, 3841, 43830                                    05730033
43830 IF (IVCOMP-24) 23830,13830,23830                                  05740033
13830 IVPASS = IVPASS + 1                                               05750033
      WRITE (I02,80001) IVTNUM                                          05760033
      GO TO 3841                                                        05770033
23830 IVFAIL = IVFAIL + 1                                               05780033
      IVCORR = 24                                                       05790033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05800033
 3841 CONTINUE                                                          05810033
      IVTNUM = 384                                                      05820033
C                                                                       05830033
C      ****  TEST 384  ****                                             05840033
C                                                                       05850033
      IF (ICZERO) 33840, 3840, 33840                                    05860033
 3840 CONTINUE                                                          05870033
      IVCOMP = 2*(3*4)                                                  05880033
      GO TO 43840                                                       05890033
33840 IVDELE = IVDELE + 1                                               05900033
      WRITE (I02,80003) IVTNUM                                          05910033
      IF (ICZERO) 43840, 3851, 43840                                    05920033
43840 IF (IVCOMP-24) 23840,13840,23840                                  05930033
13840 IVPASS = IVPASS + 1                                               05940033
      WRITE (I02,80001) IVTNUM                                          05950033
      GO TO 3851                                                        05960033
23840 IVFAIL = IVFAIL + 1                                               05970033
      IVCORR = 24                                                       05980033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05990033
 3851 CONTINUE                                                          06000033
      IVTNUM = 385                                                      06010033
C                                                                       06020033
C      ****  TEST 385 ****                                              06030033
C                                                                       06040033
      IF (ICZERO) 33850, 3850, 33850                                    06050033
 3850 CONTINUE                                                          06060033
      IVCOMP = (3*(+53)) * (+157)                                       06070033
      GO TO 43850                                                       06080033
33850 IVDELE = IVDELE + 1                                               06090033
      WRITE (I02,80003) IVTNUM                                          06100033
      IF (ICZERO) 43850, 3861, 43850                                    06110033
43850 IF (IVCOMP-24963) 23850,13850,23850                               06120033
13850 IVPASS = IVPASS + 1                                               06130033
      WRITE (I02,80001) IVTNUM                                          06140033
      GO TO 3861                                                        06150033
23850 IVFAIL = IVFAIL + 1                                               06160033
      IVCORR = 24963                                                    06170033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06180033
 3861 CONTINUE                                                          06190033
      IVTNUM = 386                                                      06200033
C                                                                       06210033
C      ****  TEST 386  ****                                             06220033
C                                                                       06230033
      IF (ICZERO) 33860, 3860, 33860                                    06240033
 3860 CONTINUE                                                          06250033
      IVCOMP = 3 *((+53)*157)                                           06260033
      GO TO 43860                                                       06270033
33860 IVDELE = IVDELE + 1                                               06280033
      WRITE (I02,80003) IVTNUM                                          06290033
      IF (ICZERO) 43860, 3871, 43860                                    06300033
43860 IF (IVCOMP-24963) 23860,13860,23860                               06310033
13860 IVPASS = IVPASS + 1                                               06320033
      WRITE (I02,80001) IVTNUM                                          06330033
      GO TO 3871                                                        06340033
23860 IVFAIL = IVFAIL + 1                                               06350033
      IVCORR=24963                                                      06360033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06370033
C                                                                       06380033
C     TEST 387 THROUGH TEST 391                                         06390033
C         BOTH POSITIVE AND NEGATIVE CONSTANTS IN ARITHMETIC EXPRESSION.06400033
C                                                                       06410033
 3871 CONTINUE                                                          06420033
      IVTNUM = 387                                                      06430033
C                                                                       06440033
C      ****  TEST 387  ****                                             06450033
C                                                                       06460033
      IF (ICZERO) 33870, 3870, 33870                                    06470033
 3870 CONTINUE                                                          06480033
      IVCOMP = 2*3*(-4)                                                 06490033
      GO TO 43870                                                       06500033
33870 IVDELE = IVDELE + 1                                               06510033
      WRITE (I02,80003) IVTNUM                                          06520033
      IF (ICZERO) 43870, 3881, 43870                                    06530033
43870 IF (IVCOMP + 24) 23870,13870,23870                                06540033
13870 IVPASS = IVPASS + 1                                               06550033
      WRITE (I02,80001) IVTNUM                                          06560033
      GO TO 3881                                                        06570033
23870 IVFAIL = IVFAIL + 1                                               06580033
      IVCORR = -24                                                      06590033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06600033
 3881 CONTINUE                                                          06610033
      IVTNUM = 388                                                      06620033
C                                                                       06630033
C      ****  TEST 388  ****                                             06640033
C                                                                       06650033
      IF (ICZERO) 33880, 3880, 33880                                    06660033
 3880 CONTINUE                                                          06670033
      IVCOMP = 2*(-3)*(+4)                                              06680033
      GO TO 43880                                                       06690033
33880 IVDELE = IVDELE + 1                                               06700033
      WRITE (I02,80003) IVTNUM                                          06710033
      IF (ICZERO) 43880, 3891, 43880                                    06720033
43880 IF (IVCOMP + 24) 23880,13880,23880                                06730033
13880 IVPASS = IVPASS + 1                                               06740033
      WRITE (I02,80001) IVTNUM                                          06750033
      GO TO 3891                                                        06760033
23880 IVFAIL = IVFAIL + 1                                               06770033
      IVCORR = -24                                                      06780033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06790033
 3891 CONTINUE                                                          06800033
      IVTNUM = 389                                                      06810033
C                                                                       06820033
C      ****  TEST 389  ****                                             06830033
C                                                                       06840033
      IF (ICZERO) 33890, 3890, 33890                                    06850033
 3890 CONTINUE                                                          06860033
      IVCOMP = (-2)*3*4                                                 06870033
      GO TO 43890                                                       06880033
33890 IVDELE = IVDELE + 1                                               06890033
      WRITE (I02,80003) IVTNUM                                          06900033
      IF (ICZERO) 43890, 3901, 43890                                    06910033
43890 IF (IVCOMP+24) 23890,13890,23890                                  06920033
13890 IVPASS = IVPASS + 1                                               06930033
      WRITE (I02,80001) IVTNUM                                          06940033
      GO TO 3901                                                        06950033
23890 IVFAIL = IVFAIL + 1                                               06960033
      IVCORR = -24                                                      06970033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06980033
 3901 CONTINUE                                                          06990033
      IVTNUM = 390                                                      07000033
C                                                                       07010033
C      ****  TEST 390  ****                                             07020033
C                                                                       07030033
      IF (ICZERO) 33900, 3900, 33900                                    07040033
 3900 CONTINUE                                                          07050033
      IVCOMP = -2*3*4                                                   07060033
      GO TO 43900                                                       07070033
33900 IVDELE = IVDELE + 1                                               07080033
      WRITE (I02,80003) IVTNUM                                          07090033
      IF (ICZERO) 43900, 3911, 43900                                    07100033
43900 IF (IVCOMP+24) 23900,13900,23900                                  07110033
13900 IVPASS = IVPASS + 1                                               07120033
      WRITE (I02,80001) IVTNUM                                          07130033
      GO TO 3911                                                        07140033
23900 IVFAIL = IVFAIL + 1                                               07150033
      IVCORR = -24                                                      07160033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07170033
 3911 CONTINUE                                                          07180033
      IVTNUM = 391                                                      07190033
C                                                                       07200033
C      ****  TEST 391  ****                                             07210033
C                                                                       07220033
      IF (ICZERO) 33910, 3910, 33910                                    07230033
 3910 CONTINUE                                                          07240033
      IVCOMP = +2 * (-3) * (-4)                                         07250033
      GO TO 43910                                                       07260033
33910 IVDELE = IVDELE + 1                                               07270033
      WRITE (I02,80003) IVTNUM                                          07280033
      IF (ICZERO) 43910, 3921, 43910                                    07290033
43910 IF (IVCOMP - 24) 23910,13910,23910                                07300033
13910 IVPASS = IVPASS + 1                                               07310033
      WRITE (I02,80001) IVTNUM                                          07320033
      GO TO 3921                                                        07330033
23910 IVFAIL = IVFAIL + 1                                               07340033
      IVCORR = 24                                                       07350033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07360033
C                                                                       07370033
C     TEST 392 THROUGH TEST 394                                         07380033
C         ALL CONSTANTS ARE NEGATIVE.                                   07390033
C                                                                       07400033
 3921 CONTINUE                                                          07410033
      IVTNUM = 392                                                      07420033
C                                                                       07430033
C      ****  TEST 392  ****                                             07440033
C                                                                       07450033
      IF (ICZERO) 33920, 3920, 33920                                    07460033
 3920 CONTINUE                                                          07470033
      IVCOMP = (-2)*(-3)*(-4)                                           07480033
      GO TO 43920                                                       07490033
33920 IVDELE = IVDELE + 1                                               07500033
      WRITE (I02,80003) IVTNUM                                          07510033
      IF (ICZERO) 43920, 3931, 43920                                    07520033
43920 IF (IVCOMP+24) 23920,13920,23920                                  07530033
13920 IVPASS = IVPASS + 1                                               07540033
      WRITE (I02,80001) IVTNUM                                          07550033
      GO TO 3931                                                        07560033
23920 IVFAIL = IVFAIL + 1                                               07570033
      IVCORR = -24                                                      07580033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07590033
 3931 CONTINUE                                                          07600033
      IVTNUM = 393                                                      07610033
C                                                                       07620033
C      ****  TEST 393  ****                                             07630033
C                                                                       07640033
      IF (ICZERO) 33930, 3930, 33930                                    07650033
 3930 CONTINUE                                                          07660033
      IVCOMP = (-23)*(-51)*(-13)                                        07670033
      GO TO 43930                                                       07680033
33930 IVDELE = IVDELE + 1                                               07690033
      WRITE (I02,80003) IVTNUM                                          07700033
      IF (ICZERO) 43930, 3941, 43930                                    07710033
43930 IF (IVCOMP + 15249) 23930,13930,23930                             07720033
13930 IVPASS = IVPASS + 1                                               07730033
      WRITE (I02,80001) IVTNUM                                          07740033
      GO TO 3941                                                        07750033
23930 IVFAIL = IVFAIL + 1                                               07760033
      IVCORR = -15249                                                   07770033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07780033
 3941 CONTINUE                                                          07790033
      IVTNUM = 394                                                      07800033
C                                                                       07810033
C      ****  TEST 394  ****                                             07820033
C                                                                       07830033
      IF (ICZERO) 33940, 3940, 33940                                    07840033
 3940 CONTINUE                                                          07850033
      IVCOMP = -3 * (-53)*( -157)                                       07860033
      GO TO 43940                                                       07870033
33940 IVDELE = IVDELE + 1                                               07880033
      WRITE (I02,80003) IVTNUM                                          07890033
      IF (ICZERO) 43940, 3951, 43940                                    07900033
43940 IF (IVCOMP +24963) 23940,13940,23940                              07910033
13940 IVPASS = IVPASS + 1                                               07920033
      WRITE (I02,80001) IVTNUM                                          07930033
      GO TO 3951                                                        07940033
23940 IVFAIL = IVFAIL + 1                                               07950033
      IVCORR = -24963                                                   07960033
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07970033
C      ****   END OF TESTS   ****                                       07980033
 3951 CONTINUE                                                          07990033
C                                                                       08000033
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08010033
99999 CONTINUE                                                          08020033
      WRITE (I02,90002)                                                 08030033
      WRITE (I02,90006)                                                 08040033
      WRITE (I02,90002)                                                 08050033
      WRITE (I02,90002)                                                 08060033
      WRITE (I02,90007)                                                 08070033
      WRITE (I02,90002)                                                 08080033
      WRITE (I02,90008)  IVFAIL                                         08090033
      WRITE (I02,90009) IVPASS                                          08100033
      WRITE (I02,90010) IVDELE                                          08110033
C                                                                       08120033
C                                                                       08130033
C     TERMINATE ROUTINE EXECUTION                                       08140033
      STOP                                                              08150033
C                                                                       08160033
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08170033
90000 FORMAT (1H1)                                                      08180033
90002 FORMAT (1H )                                                      08190033
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08200033
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08210033
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08220033
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08230033
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08240033
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08250033
C                                                                       08260033
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08270033
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08280033
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08290033
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08300033
C                                                                       08310033
C     FORMAT STATEMENTS FOR TEST RESULTS                                08320033
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08330033
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08340033
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08350033
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08360033
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08370033
C                                                                       08380033
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM033)                          08390033
      END                                                               08400033
