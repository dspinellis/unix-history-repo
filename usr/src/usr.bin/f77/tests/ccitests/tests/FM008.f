C     COMMENT SECTION.                                                  00010008
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020008
C     FM008                                                             00030008
C                                                                       00040008
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050008
C     FORM          INTEGER VARIABLE = ARITHMETIC EXPRESSION            00060008
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00070008
C     OPERATOR + INTEGER CONSTANTS AND POSITIVE INTEGER VARIABLES.      00080008
C     SOME OF THE TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE        00090008
C     ARITHMETIC EXPRESSION.                                            00100008
C                                                                       00110008
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00120008
C            (1) TWO INTEGER CONSTANTS,                                 00130008
C            (2) THREE INTEGER CONSTANTS,                               00140008
C            (3) THREE INTEGER CONSTANTS WITH PARENTHESES TO GROUP      00150008
C                   ELEMENTS,                                           00160008
C            (4) ONE INTEGER VARIABLE AND ONE INTEGER CONSTANT,         00170008
C            (5) ONE INTEGER VARIABLE AND TWO INTEGER CONSTANTS,        00180008
C            (6) ONE INTEGER VARIABLE AND TWO INTEGER CONSTANTS WITH    00190008
C                   PARENTHESES TO GROUP ELEMENTS.                      00200008
C                                                                       00210008
C                                                                       00220008
C      REFERENCES                                                       00230008
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00240008
C              X3.9-1978                                                00250008
C                                                                       00260008
C        SECTION 4.3, INTEGER TYPE                                      00270008
C        SECTION 4.3.1, INTEGER CONSTANT                                00280008
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00290008
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENTS                 00300008
C                                                                       00310008
C                                                                       00320008
C      **********************************************************       00330008
C                                                                       00340008
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00350008
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00360008
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00370008
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00380008
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00390008
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00400008
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00410008
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00420008
C     OF EXECUTING THESE TESTS.                                         00430008
C                                                                       00440008
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00450008
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00460008
C                                                                       00470008
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00480008
C                                                                       00490008
C                  DEPARTMENT OF THE NAVY                               00500008
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00510008
C                  WASHINGTON, D.C.  20376                              00520008
C                                                                       00530008
C      **********************************************************       00540008
C                                                                       00550008
C                                                                       00560008
C                                                                       00570008
C     INITIALIZATION SECTION                                            00580008
C                                                                       00590008
C     INITIALIZE CONSTANTS                                              00600008
C      **************                                                   00610008
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620008
      I01 = 5                                                           00630008
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640008
      I02 = 6                                                           00650008
C     SYSTEM ENVIRONMENT SECTION                                        00660008
C                                                                       00670008
      I01 = 5                                                           00680008
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690008
C     (UNIT NUMBER FOR CARD READER).                                    00700008
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00710008
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00720008
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00730008
C                                                                       00740008
      I02 = 6                                                           00750008
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00760008
C     (UNIT NUMBER FOR PRINTER).                                        00770008
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00780008
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00790008
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00800008
C                                                                       00810008
      IVPASS=0                                                          00820008
      IVFAIL=0                                                          00830008
      IVDELE=0                                                          00840008
      ICZERO=0                                                          00850008
C                                                                       00860008
C     WRITE PAGE HEADERS                                                00870008
      WRITE (I02,90000)                                                 00880008
      WRITE (I02,90001)                                                 00890008
      WRITE (I02,90002)                                                 00900008
      WRITE (I02, 90002)                                                00910008
      WRITE (I02,90003)                                                 00920008
      WRITE (I02,90002)                                                 00930008
      WRITE (I02,90004)                                                 00940008
      WRITE (I02,90002)                                                 00950008
      WRITE (I02,90011)                                                 00960008
      WRITE (I02,90002)                                                 00970008
      WRITE (I02,90002)                                                 00980008
      WRITE (I02,90005)                                                 00990008
      WRITE (I02,90006)                                                 01000008
      WRITE (I02,90002)                                                 01010008
C     TEST SECTION                                                      01020008
C                                                                       01030008
C         ARITHMETIC ASSIGNMENT STATEMENT                               01040008
C                                                                       01050008
C     TEST 200 THROUGH TEST 214 CONTAIN INTEGER CONSTANTS AND OPERATOR +01060008
C     IN ARITHMETIC EXPRESSION.                                         01070008
C                                                                       01080008
C     TEST 200 THROUGH TEST 206 - TWO INTEGER CONSTANTS                 01090008
C                                                                       01100008
 2001 CONTINUE                                                          01110008
      IVTNUM = 200                                                      01120008
C                                                                       01130008
C      ****  TEST 200  ****                                             01140008
C                                                                       01150008
      IF (ICZERO) 32000, 2000, 32000                                    01160008
 2000 CONTINUE                                                          01170008
      IVCOMP = 2+3                                                      01180008
      GO TO 42000                                                       01190008
32000 IVDELE = IVDELE + 1                                               01200008
      WRITE (I02,80003) IVTNUM                                          01210008
      IF (ICZERO) 42000, 2011, 42000                                    01220008
42000 IF (IVCOMP - 5) 22000,12000,22000                                 01230008
12000 IVPASS = IVPASS + 1                                               01240008
      WRITE (I02,80001) IVTNUM                                          01250008
      GO TO 2011                                                        01260008
22000 IVFAIL = IVFAIL + 1                                               01270008
      IVCORR = 5                                                        01280008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01290008
 2011 CONTINUE                                                          01300008
      IVTNUM = 201                                                      01310008
C                                                                       01320008
C      ****  TEST 201  ****                                             01330008
C                                                                       01340008
      IF (ICZERO) 32010, 2010, 32010                                    01350008
 2010 CONTINUE                                                          01360008
      IVCOMP = 51 + 52                                                  01370008
      GO TO 42010                                                       01380008
32010 IVDELE = IVDELE + 1                                               01390008
      WRITE (I02,80003) IVTNUM                                          01400008
      IF (ICZERO) 42010, 2021, 42010                                    01410008
42010 IF (IVCOMP - 103) 22010,12010,22010                               01420008
12010 IVPASS = IVPASS + 1                                               01430008
      WRITE (I02,80001) IVTNUM                                          01440008
      GO TO 2021                                                        01450008
22010 IVFAIL = IVFAIL + 1                                               01460008
      IVCORR = 103                                                      01470008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01480008
 2021 CONTINUE                                                          01490008
      IVTNUM = 202                                                      01500008
C                                                                       01510008
C      ****  TEST 202  ****                                             01520008
C                                                                       01530008
      IF (ICZERO) 32020, 2020, 32020                                    01540008
 2020 CONTINUE                                                          01550008
      IVCOMP = 189 + 676                                                01560008
      GO TO 42020                                                       01570008
32020 IVDELE = IVDELE + 1                                               01580008
      WRITE (I02,80003) IVTNUM                                          01590008
      IF (ICZERO) 42020, 2031, 42020                                    01600008
42020 IF (IVCOMP - 865) 22020,12020,22020                               01610008
12020 IVPASS = IVPASS + 1                                               01620008
      WRITE (I02,80001) IVTNUM                                          01630008
      GO TO 2031                                                        01640008
22020 IVFAIL = IVFAIL + 1                                               01650008
      IVCORR = 865                                                      01660008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01670008
 2031 CONTINUE                                                          01680008
      IVTNUM = 203                                                      01690008
C                                                                       01700008
C      ****  TEST 203  ****                                             01710008
C                                                                       01720008
      IF (ICZERO) 32030, 2030, 32030                                    01730008
 2030 CONTINUE                                                          01740008
      IVCOMP = 1358 + 8001                                              01750008
      GO TO 42030                                                       01760008
32030 IVDELE = IVDELE + 1                                               01770008
      WRITE (I02,80003) IVTNUM                                          01780008
      IF (ICZERO) 42030, 2041, 42030                                    01790008
42030 IF (IVCOMP - 9359) 22030, 12030, 22030                            01800008
12030 IVPASS = IVPASS + 1                                               01810008
      WRITE (I02,80001) IVTNUM                                          01820008
      GO TO 2041                                                        01830008
22030 IVFAIL = IVFAIL + 1                                               01840008
      IVCORR = 9359                                                     01850008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01860008
 2041 CONTINUE                                                          01870008
      IVTNUM = 204                                                      01880008
C                                                                       01890008
C      ****  TEST 204  ****                                             01900008
C                                                                       01910008
      IF (ICZERO) 32040, 2040, 32040                                    01920008
 2040 CONTINUE                                                          01930008
      IVCOMP = 11112 + 10001                                            01940008
      GO TO 42040                                                       01950008
32040 IVDELE = IVDELE + 1                                               01960008
      WRITE (I02,80003) IVTNUM                                          01970008
      IF (ICZERO) 42040, 2051, 42040                                    01980008
42040 IF (IVCOMP - 21113) 22040, 12040, 22040                           01990008
12040 IVPASS = IVPASS + 1                                               02000008
      WRITE (I02,80001) IVTNUM                                          02010008
      GO TO 2051                                                        02020008
22040 IVFAIL = IVFAIL + 1                                               02030008
      IVCORR=21113                                                      02040008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02050008
 2051 CONTINUE                                                          02060008
      IVTNUM = 205                                                      02070008
C                                                                       02080008
C      ****  TEST 205  ****                                             02090008
C                                                                       02100008
      IF (ICZERO) 32050, 2050, 32050                                    02110008
 2050 CONTINUE                                                          02120008
      IVCOMP = 189 + 9876                                               02130008
      GO TO 42050                                                       02140008
32050 IVDELE = IVDELE + 1                                               02150008
      WRITE (I02,80003) IVTNUM                                          02160008
      IF (ICZERO) 42050, 2061, 42050                                    02170008
42050 IF (IVCOMP - 10065) 22050,12050,22050                             02180008
12050 IVPASS = IVPASS + 1                                               02190008
      WRITE (I02,80001) IVTNUM                                          02200008
      GO TO 2061                                                        02210008
22050 IVFAIL = IVFAIL + 1                                               02220008
      IVCORR = 10065                                                    02230008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02240008
 2061 CONTINUE                                                          02250008
      IVTNUM = 206                                                      02260008
C                                                                       02270008
C      ****  TEST 206  ****                                             02280008
C          REQUIRES 32767                                               02290008
C                                                                       02300008
      IF (ICZERO) 32060, 2060, 32060                                    02310008
 2060 CONTINUE                                                          02320008
      IVCOMP = 32752 + 15                                               02330008
      GO TO 42060                                                       02340008
32060 IVDELE = IVDELE + 1                                               02350008
      WRITE (I02,80003) IVTNUM                                          02360008
      IF (ICZERO) 42060, 2071, 42060                                    02370008
42060 IF (IVCOMP - 32767) 22060,12060,22060                             02380008
12060 IVPASS = IVPASS + 1                                               02390008
      WRITE (I02,80001) IVTNUM                                          02400008
      GO TO 2071                                                        02410008
22060 IVFAIL = IVFAIL + 1                                               02420008
      IVCORR = 32767                                                    02430008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02440008
C                                                                       02450008
C     TEST 207 THROUGH TEST 210 - THREE INTEGER CONSTANTS               02460008
C                                                                       02470008
 2071 CONTINUE                                                          02480008
      IVTNUM = 207                                                      02490008
C                                                                       02500008
C      ****  TEST 207  ****                                             02510008
C                                                                       02520008
      IF (ICZERO) 32070, 2070, 32070                                    02530008
 2070 CONTINUE                                                          02540008
      IVCOMP = 2+3+4                                                    02550008
      GO TO 42070                                                       02560008
32070 IVDELE = IVDELE + 1                                               02570008
      WRITE (I02,80003) IVTNUM                                          02580008
      IF (ICZERO) 42070, 2081, 42070                                    02590008
42070 IF (IVCOMP - 9) 22070,12070,22070                                 02600008
12070 IVPASS = IVPASS + 1                                               02610008
      WRITE (I02,80001) IVTNUM                                          02620008
      GO TO 2081                                                        02630008
22070 IVFAIL = IVFAIL + 1                                               02640008
      IVCORR = 9                                                        02650008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02660008
 2081 CONTINUE                                                          02670008
      IVTNUM = 208                                                      02680008
C                                                                       02690008
C      ****  TEST 208  ****                                             02700008
C                                                                       02710008
      IF (ICZERO) 32080, 2080, 32080                                    02720008
 2080 CONTINUE                                                          02730008
      IVCOMP = 51 + 52 + 53                                             02740008
      GO TO 42080                                                       02750008
32080 IVDELE = IVDELE + 1                                               02760008
      WRITE (I02,80003) IVTNUM                                          02770008
      IF (ICZERO) 42080, 2091, 42080                                    02780008
42080 IF (IVCOMP - 156) 22080,12080,22080                               02790008
12080 IVPASS = IVPASS + 1                                               02800008
      WRITE (I02,80001) IVTNUM                                          02810008
      GO TO 2091                                                        02820008
22080 IVFAIL = IVFAIL + 1                                               02830008
      IVCORR = 156                                                      02840008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02850008
 2091 CONTINUE                                                          02860008
      IVTNUM = 209                                                      02870008
C                                                                       02880008
C      ****  TEST 209  ****                                             02890008
C                                                                       02900008
      IF (ICZERO) 32090, 2090, 32090                                    02910008
 2090 CONTINUE                                                          02920008
      IVCOMP = 189 +676+101                                             02930008
      GO TO 42090                                                       02940008
32090 IVDELE = IVDELE + 1                                               02950008
      WRITE (I02,80003) IVTNUM                                          02960008
      IF (ICZERO) 42090, 2101, 42090                                    02970008
42090 IF (IVCOMP - 966) 22090,12090,22090                               02980008
12090 IVPASS = IVPASS + 1                                               02990008
      WRITE (I02,80001) IVTNUM                                          03000008
      GO TO 2101                                                        03010008
22090 IVFAIL = IVFAIL + 1                                               03020008
      IVCORR = 966                                                      03030008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03040008
 2101 CONTINUE                                                          03050008
      IVTNUM = 210                                                      03060008
C                                                                       03070008
C      ****  TEST 210  ****                                             03080008
C                                                                       03090008
      IF (ICZERO) 32100, 2100, 32100                                    03100008
 2100 CONTINUE                                                          03110008
      IVCOMP = 1358 + 8001 + 2189                                       03120008
      GO TO 42100                                                       03130008
32100 IVDELE = IVDELE + 1                                               03140008
      WRITE (I02,80003) IVTNUM                                          03150008
      IF (ICZERO) 42100, 2111, 42100                                    03160008
42100 IF (IVCOMP - 11548) 22100,12100,22100                             03170008
12100 IVPASS = IVPASS + 1                                               03180008
      WRITE (I02,80001) IVTNUM                                          03190008
      GO TO 2111                                                        03200008
22100 IVFAIL = IVFAIL + 1                                               03210008
      IVCORR = 11548                                                    03220008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03230008
C                                                                       03240008
C     TESTS 211 THROUGH 214 ARE THE SAME AS 207 THROUGH 210 EXCEPT      03250008
C     PARENTHESES ARE USED TO GROUP THE CONSTANTS.                      03260008
C                                                                       03270008
 2111 CONTINUE                                                          03280008
      IVTNUM = 211                                                      03290008
C                                                                       03300008
C      ****  TEST 211  ****                                             03310008
C                                                                       03320008
      IF (ICZERO) 32110, 2110, 32110                                    03330008
 2110 CONTINUE                                                          03340008
      IVCOMP = (2+3)+4                                                  03350008
      GO TO 42110                                                       03360008
32110 IVDELE = IVDELE + 1                                               03370008
      WRITE (I02,80003) IVTNUM                                          03380008
      IF (ICZERO) 42110, 2121, 42110                                    03390008
42110 IF (IVCOMP -9) 22110,12110,22110                                  03400008
12110 IVPASS = IVPASS + 1                                               03410008
      WRITE (I02,80001) IVTNUM                                          03420008
      GO TO 2121                                                        03430008
22110 IVFAIL = IVFAIL + 1                                               03440008
      IVCORR = 9                                                        03450008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03460008
 2121 CONTINUE                                                          03470008
      IVTNUM = 212                                                      03480008
C                                                                       03490008
C      ****  TEST 212  ****                                             03500008
C                                                                       03510008
      IF (ICZERO) 32120, 2120, 32120                                    03520008
 2120 CONTINUE                                                          03530008
      IVCOMP = 51+(52+53)                                               03540008
      GO TO 42120                                                       03550008
32120 IVDELE = IVDELE + 1                                               03560008
      WRITE (I02,80003) IVTNUM                                          03570008
      IF (ICZERO) 42120, 2131, 42120                                    03580008
42120 IF (IVCOMP - 156) 22120,12120,22120                               03590008
12120 IVPASS = IVPASS + 1                                               03600008
      WRITE (I02,80001) IVTNUM                                          03610008
      GO TO 2131                                                        03620008
22120 IVFAIL = IVFAIL + 1                                               03630008
      IVCORR = 156                                                      03640008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03650008
 2131 CONTINUE                                                          03660008
      IVTNUM = 213                                                      03670008
C                                                                       03680008
C      ****  TEST 213  ****                                             03690008
C                                                                       03700008
      IF (ICZERO) 32130, 2130, 32130                                    03710008
 2130 CONTINUE                                                          03720008
      IVCOMP = 189 +(676+101)                                           03730008
      GO TO 42130                                                       03740008
32130 IVDELE = IVDELE + 1                                               03750008
      WRITE (I02,80003) IVTNUM                                          03760008
      IF (ICZERO) 42130, 2141, 42130                                    03770008
42130 IF (IVCOMP - 966) 22130,12130,22130                               03780008
12130 IVPASS = IVPASS + 1                                               03790008
      WRITE (I02,80001) IVTNUM                                          03800008
      GO TO 2141                                                        03810008
22130 IVFAIL = IVFAIL + 1                                               03820008
      IVCORR = 966                                                      03830008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03840008
 2141 CONTINUE                                                          03850008
      IVTNUM = 214                                                      03860008
C                                                                       03870008
C      ****  TEST 214  ****                                             03880008
C                                                                       03890008
      IF (ICZERO) 32140, 2140, 32140                                    03900008
 2140 CONTINUE                                                          03910008
      IVCOMP = (1358+2189) + 8001                                       03920008
      GO TO 42140                                                       03930008
32140 IVDELE = IVDELE + 1                                               03940008
      WRITE (I02,80003) IVTNUM                                          03950008
      IF (ICZERO) 42140, 2151, 42140                                    03960008
42140 IF (IVCOMP - 11548) 22140,12140,22140                             03970008
12140 IVPASS = IVPASS + 1                                               03980008
      WRITE (I02,80001) IVTNUM                                          03990008
      GO TO 2151                                                        04000008
22140 IVFAIL = IVFAIL + 1                                               04010008
      IVCORR = 11548                                                    04020008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04030008
C                                                                       04040008
C     TEST 215 THROUGH TEST 234 CONTAIN INTEGER VARIABLES, INTEGER      04050008
C     CONSTANTS AND THE OPERATOR + IN ARITHMETIC EXPRESSION.            04060008
C                                                                       04070008
C     TEST 215 THROUGH TEST 219 - ONE INTEGER VARIABLE AND ONE INTEGER  04080008
C     CONSTANT IN ARITHMETIC EXPRESSION.                                04090008
C                                                                       04100008
 2151 CONTINUE                                                          04110008
      IVTNUM = 215                                                      04120008
C                                                                       04130008
C      ****  TEST 215  ****                                             04140008
C                                                                       04150008
      IF (ICZERO) 32150, 2150, 32150                                    04160008
 2150 CONTINUE                                                          04170008
      IVON01 = 2                                                        04180008
      IVCOMP = IVON01 + 3                                               04190008
      GO TO 42150                                                       04200008
32150 IVDELE = IVDELE + 1                                               04210008
      WRITE (I02,80003) IVTNUM                                          04220008
      IF (ICZERO) 42150, 2161, 42150                                    04230008
42150 IF (IVCOMP - 5) 22150,12150,22150                                 04240008
12150 IVPASS = IVPASS + 1                                               04250008
      WRITE (I02,80001) IVTNUM                                          04260008
      GO TO 2161                                                        04270008
22150 IVFAIL = IVFAIL + 1                                               04280008
      IVCORR=5                                                          04290008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04300008
 2161 CONTINUE                                                          04310008
      IVTNUM = 216                                                      04320008
C                                                                       04330008
C      ****  TEST 216  ****                                             04340008
C                                                                       04350008
      IF (ICZERO) 32160, 2160, 32160                                    04360008
 2160 CONTINUE                                                          04370008
      IVON01 = 3                                                        04380008
      IVCOMP = 2 + IVON01                                               04390008
      GO TO 42160                                                       04400008
32160 IVDELE = IVDELE + 1                                               04410008
      WRITE (I02,80003) IVTNUM                                          04420008
      IF (ICZERO) 42160, 2171, 42160                                    04430008
42160 IF (IVCOMP - 5) 22160,12160,22160                                 04440008
12160 IVPASS = IVPASS + 1                                               04450008
      WRITE (I02,80001) IVTNUM                                          04460008
      GO TO 2171                                                        04470008
22160 IVFAIL = IVFAIL + 1                                               04480008
      IVCORR = 5                                                        04490008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04500008
 2171 CONTINUE                                                          04510008
      IVTNUM = 217                                                      04520008
C                                                                       04530008
C      ****  TEST 217  ****                                             04540008
C                                                                       04550008
      IF (ICZERO) 32170, 2170, 32170                                    04560008
 2170 CONTINUE                                                          04570008
      IVON01 = 51                                                       04580008
      IVCOMP = IVON01 +52                                               04590008
      GO TO 42170                                                       04600008
32170 IVDELE = IVDELE + 1                                               04610008
      WRITE (I02,80003) IVTNUM                                          04620008
      IF (ICZERO) 42170, 2181, 42170                                    04630008
42170 IF (IVCOMP - 103) 22170,12170,22170                               04640008
12170 IVPASS = IVPASS + 1                                               04650008
      WRITE (I02,80001) IVTNUM                                          04660008
      GO TO 2181                                                        04670008
22170 IVFAIL = IVFAIL + 1                                               04680008
      IVCORR = 103                                                      04690008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04700008
 2181 CONTINUE                                                          04710008
      IVTNUM = 218                                                      04720008
C                                                                       04730008
C      ****  TEST 218  ****                                             04740008
C                                                                       04750008
      IF (ICZERO) 32180, 2180, 32180                                    04760008
 2180 CONTINUE                                                          04770008
      IVON01 = 676                                                      04780008
      IVCOMP = 189 + IVON01                                             04790008
      GO TO 42180                                                       04800008
32180 IVDELE = IVDELE + 1                                               04810008
      WRITE (I02,80003) IVTNUM                                          04820008
      IF (ICZERO) 42180, 2191, 42180                                    04830008
42180 IF (IVCOMP - 865) 22180,12180,22180                               04840008
12180 IVPASS = IVPASS + 1                                               04850008
      WRITE (I02,80001) IVTNUM                                          04860008
      GO TO 2191                                                        04870008
22180 IVFAIL = IVFAIL + 1                                               04880008
      IVCORR = 865                                                      04890008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04900008
 2191 CONTINUE                                                          04910008
      IVTNUM = 219                                                      04920008
C                                                                       04930008
C      ****  TEST 219  ****                                             04940008
C                                                                       04950008
      IF (ICZERO) 32190, 2190, 32190                                    04960008
 2190 CONTINUE                                                          04970008
      IVON01 = 1358                                                     04980008
      IVCOMP = IVON01 + 8001                                            04990008
      GO TO 42190                                                       05000008
32190 IVDELE = IVDELE + 1                                               05010008
      WRITE (I02,80003) IVTNUM                                          05020008
      IF (ICZERO) 42190, 2201, 42190                                    05030008
42190 IF (IVCOMP - 9359) 22190,12190,22190                              05040008
12190 IVPASS = IVPASS + 1                                               05050008
      WRITE (I02,80001) IVTNUM                                          05060008
      GO TO 2201                                                        05070008
22190 IVFAIL = IVFAIL + 1                                               05080008
      IVCORR = 9359                                                     05090008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05100008
C                                                                       05110008
C     TEST 220 THROUGH TEST 224 - ONE INTEGER VARIABLE, TWO INTEGER     05120008
C     CONSTANTS IN ARITHMETIC EXPRESSION.                               05130008
C                                                                       05140008
 2201 CONTINUE                                                          05150008
      IVTNUM = 220                                                      05160008
C                                                                       05170008
C      ****  TEST 220  ****                                             05180008
C                                                                       05190008
      IF (ICZERO) 32200, 2200, 32200                                    05200008
 2200 CONTINUE                                                          05210008
      IVON01 = 2                                                        05220008
      IVCOMP = IVON01 +3 +4                                             05230008
      GO TO 42200                                                       05240008
32200 IVDELE = IVDELE + 1                                               05250008
      WRITE (I02,80003) IVTNUM                                          05260008
      IF (ICZERO) 42200, 2211, 42200                                    05270008
42200 IF (IVCOMP - 9) 22200,12200,22200                                 05280008
12200 IVPASS = IVPASS + 1                                               05290008
      WRITE (I02,80001) IVTNUM                                          05300008
      GO TO 2211                                                        05310008
22200 IVFAIL = IVFAIL + 1                                               05320008
      IVCORR = 9                                                        05330008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05340008
 2211 CONTINUE                                                          05350008
      IVTNUM = 221                                                      05360008
C                                                                       05370008
C      ****  TEST 221  ****                                             05380008
C                                                                       05390008
      IF (ICZERO) 32210, 2210, 32210                                    05400008
 2210 CONTINUE                                                          05410008
      IVON01 = 3                                                        05420008
      IVCOMP = 2+IVON01+4                                               05430008
      GO TO 42210                                                       05440008
32210 IVDELE = IVDELE + 1                                               05450008
      WRITE (I02,80003) IVTNUM                                          05460008
      IF (ICZERO) 42210, 2221, 42210                                    05470008
42210 IF (IVCOMP - 9) 22210,12210,22210                                 05480008
12210 IVPASS = IVPASS + 1                                               05490008
      WRITE (I02,80001) IVTNUM                                          05500008
      GO TO 2221                                                        05510008
22210 IVFAIL = IVFAIL + 1                                               05520008
      IVCORR = 9                                                        05530008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05540008
 2221 CONTINUE                                                          05550008
      IVTNUM = 222                                                      05560008
C                                                                       05570008
C      ****  TEST 222  ****                                             05580008
C                                                                       05590008
      IF (ICZERO) 32220, 2220, 32220                                    05600008
 2220 CONTINUE                                                          05610008
      IVON01 = 4                                                        05620008
      IVCOMP= 2+3+IVON01                                                05630008
      GO TO 42220                                                       05640008
32220 IVDELE = IVDELE + 1                                               05650008
      WRITE (I02,80003) IVTNUM                                          05660008
      IF (ICZERO) 42220, 2231, 42220                                    05670008
42220 IF (IVCOMP - 9) 22220,12220,22220                                 05680008
12220 IVPASS = IVPASS + 1                                               05690008
      WRITE (I02,80001) IVTNUM                                          05700008
      GO TO 2231                                                        05710008
22220 IVFAIL = IVFAIL + 1                                               05720008
      IVCORR = 9                                                        05730008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05740008
 2231 CONTINUE                                                          05750008
      IVTNUM = 223                                                      05760008
C                                                                       05770008
C      ****  TEST 223  ****                                             05780008
C                                                                       05790008
      IF (ICZERO) 32230, 2230, 32230                                    05800008
 2230 CONTINUE                                                          05810008
      IVON01 = 2189                                                     05820008
      IVCOMP = 1358+IVON01+8001                                         05830008
      GO TO 42230                                                       05840008
32230 IVDELE = IVDELE + 1                                               05850008
      WRITE (I02,80003) IVTNUM                                          05860008
      IF (ICZERO) 42230, 2241, 42230                                    05870008
42230 IF (IVCOMP - 11548) 22230,12230,22230                             05880008
12230 IVPASS = IVPASS + 1                                               05890008
      WRITE (I02,80001) IVTNUM                                          05900008
      GO TO 2241                                                        05910008
22230 IVFAIL = IVFAIL + 1                                               05920008
      IVCORR=11548                                                      05930008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05940008
 2241 CONTINUE                                                          05950008
      IVTNUM = 224                                                      05960008
C                                                                       05970008
C      ****  TEST 224  ****                                             05980008
C                                                                       05990008
      IF (ICZERO) 32240, 2240, 32240                                    06000008
 2240 CONTINUE                                                          06010008
      IVON01 = 11111                                                    06020008
      IVCOMP = 11111 + IVON01 + 10111                                   06030008
      GO TO 42240                                                       06040008
32240 IVDELE = IVDELE + 1                                               06050008
      WRITE (I02,80003) IVTNUM                                          06060008
      IF (ICZERO) 42240, 2251, 42240                                    06070008
42240 IF (IVCOMP - 32333) 22240,12240,22240                             06080008
12240 IVPASS = IVPASS + 1                                               06090008
      WRITE (I02,80001) IVTNUM                                          06100008
      GO TO 2251                                                        06110008
22240 IVFAIL = IVFAIL + 1                                               06120008
      IVCORR = 32333                                                    06130008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06140008
C                                                                       06150008
C     TEST 225 THROUGH TEST 234 USE PARENTHESES TO GROUP ELEMENTS IN    06160008
C     AN ARITHMETIC EXPRESSION. THE RESULTS ARE THE SAME AS TESTS       06170008
C     220 THROUGH 224.                                                  06180008
C                                                                       06190008
 2251 CONTINUE                                                          06200008
      IVTNUM = 225                                                      06210008
C                                                                       06220008
C      ****  TEST 225  ****                                             06230008
C                                                                       06240008
      IF (ICZERO) 32250, 2250, 32250                                    06250008
 2250 CONTINUE                                                          06260008
       IVON01 = 2                                                       06270008
      IVCOMP = (IVON01 +3) + 4                                          06280008
      GO TO 42250                                                       06290008
32250 IVDELE = IVDELE + 1                                               06300008
      WRITE (I02,80003) IVTNUM                                          06310008
      IF (ICZERO) 42250, 2261, 42250                                    06320008
42250 IF (IVCOMP -9) 22250,12250,22250                                  06330008
12250 IVPASS = IVPASS + 1                                               06340008
      WRITE (I02,80001) IVTNUM                                          06350008
      GO TO 2261                                                        06360008
22250 IVFAIL = IVFAIL + 1                                               06370008
      IVCORR = 9                                                        06380008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06390008
 2261 CONTINUE                                                          06400008
      IVTNUM = 226                                                      06410008
C                                                                       06420008
C      ****  TEST 226  ****                                             06430008
C                                                                       06440008
      IF (ICZERO) 32260, 2260, 32260                                    06450008
 2260 CONTINUE                                                          06460008
      IVON01 = 2                                                        06470008
      IVCOMP = IVON01 + (3+4)                                           06480008
      GO TO 42260                                                       06490008
32260 IVDELE = IVDELE + 1                                               06500008
      WRITE (I02,80003) IVTNUM                                          06510008
      IF (ICZERO) 42260, 2271, 42260                                    06520008
42260 IF (IVCOMP - 9) 22260,12260,22260                                 06530008
12260 IVPASS = IVPASS + 1                                               06540008
      WRITE (I02,80001) IVTNUM                                          06550008
      GO TO 2271                                                        06560008
22260 IVFAIL = IVFAIL + 1                                               06570008
      IVCORR = 9                                                        06580008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06590008
 2271 CONTINUE                                                          06600008
      IVTNUM = 227                                                      06610008
C                                                                       06620008
C      ****  TEST 227  ****                                             06630008
C                                                                       06640008
      IF (ICZERO) 32270, 2270, 32270                                    06650008
 2270 CONTINUE                                                          06660008
      IVON01 = 3                                                        06670008
      IVCOMP = (2+IVON01) + 4                                           06680008
      GO TO 42270                                                       06690008
32270 IVDELE = IVDELE + 1                                               06700008
      WRITE (I02,80003) IVTNUM                                          06710008
      IF (ICZERO) 42270, 2281, 42270                                    06720008
42270 IF (IVCOMP - 9) 22270,12270,22270                                 06730008
12270 IVPASS = IVPASS + 1                                               06740008
      WRITE (I02,80001) IVTNUM                                          06750008
      GO TO 2281                                                        06760008
22270 IVFAIL = IVFAIL + 1                                               06770008
      IVCORR = 9                                                        06780008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06790008
 2281 CONTINUE                                                          06800008
      IVTNUM = 228                                                      06810008
C                                                                       06820008
C      ****  TEST 228  ****                                             06830008
C                                                                       06840008
      IF (ICZERO) 32280, 2280, 32280                                    06850008
 2280 CONTINUE                                                          06860008
      IVON01 = 3                                                        06870008
      IVCOMP = 2 +(IVON01+4)                                            06880008
      GO TO 42280                                                       06890008
32280 IVDELE = IVDELE + 1                                               06900008
      WRITE (I02,80003) IVTNUM                                          06910008
      IF (ICZERO) 42280, 2291, 42280                                    06920008
42280 IF (IVCOMP - 9) 22280, 12280, 22280                               06930008
12280 IVPASS = IVPASS + 1                                               06940008
      WRITE (I02,80001) IVTNUM                                          06950008
      GO TO 2291                                                        06960008
22280 IVFAIL = IVFAIL + 1                                               06970008
      IVCORR = 9                                                        06980008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06990008
 2291 CONTINUE                                                          07000008
      IVTNUM = 229                                                      07010008
C                                                                       07020008
C      ****  TEST 229  ****                                             07030008
C                                                                       07040008
      IF (ICZERO) 32290, 2290, 32290                                    07050008
 2290 CONTINUE                                                          07060008
      IVON01 = 4                                                        07070008
      IVCOMP = (2+3)+IVON01                                             07080008
      GO TO 42290                                                       07090008
32290 IVDELE = IVDELE + 1                                               07100008
      WRITE (I02,80003) IVTNUM                                          07110008
      IF (ICZERO) 42290, 2301, 42290                                    07120008
42290 IF (IVCOMP - 9) 22290,12290,22290                                 07130008
12290 IVPASS = IVPASS + 1                                               07140008
      WRITE (I02,80001) IVTNUM                                          07150008
      GO TO 2301                                                        07160008
22290 IVFAIL = IVFAIL + 1                                               07170008
      IVCORR = 9                                                        07180008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07190008
 2301 CONTINUE                                                          07200008
      IVTNUM = 230                                                      07210008
C                                                                       07220008
C      ****  TEST 230  ****                                             07230008
C                                                                       07240008
      IF (ICZERO) 32300, 2300, 32300                                    07250008
 2300 CONTINUE                                                          07260008
      IVON01 = 2189                                                     07270008
      IVCOMP = 1358 + (IVON01+8001)                                     07280008
      GO TO 42300                                                       07290008
32300 IVDELE = IVDELE + 1                                               07300008
      WRITE (I02,80003) IVTNUM                                          07310008
      IF (ICZERO) 42300, 2311, 42300                                    07320008
42300 IF (IVCOMP - 11548) 22300,12300,22300                             07330008
12300 IVPASS = IVPASS + 1                                               07340008
      WRITE (I02,80001) IVTNUM                                          07350008
      GO TO 2311                                                        07360008
22300 IVFAIL = IVFAIL + 1                                               07370008
      IVCORR = 11548                                                    07380008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07390008
 2311 CONTINUE                                                          07400008
      IVTNUM = 231                                                      07410008
C                                                                       07420008
C      ****  TEST 231  ****                                             07430008
C                                                                       07440008
      IF (ICZERO) 32310, 2310, 32310                                    07450008
 2310 CONTINUE                                                          07460008
      IVON01 = 2189                                                     07470008
      IVCOMP = (1358+IVON01) + 8001                                     07480008
      GO TO 42310                                                       07490008
32310 IVDELE = IVDELE + 1                                               07500008
      WRITE (I02,80003) IVTNUM                                          07510008
      IF (ICZERO) 42310, 2321, 42310                                    07520008
42310 IF (IVCOMP - 11548) 22310,12310,22310                             07530008
12310 IVPASS = IVPASS + 1                                               07540008
      WRITE (I02,80001) IVTNUM                                          07550008
      GO TO 2321                                                        07560008
22310 IVFAIL = IVFAIL + 1                                               07570008
      IVCORR = 11548                                                    07580008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07590008
 2321 CONTINUE                                                          07600008
      IVTNUM = 232                                                      07610008
C                                                                       07620008
C      ****  TEST 232  ****                                             07630008
C                                                                       07640008
      IF (ICZERO) 32320, 2320, 32320                                    07650008
 2320 CONTINUE                                                          07660008
      IVON01 = 11111                                                    07670008
      IVCOMP = (11111 + IVON01) + 10111                                 07680008
      GO TO 42320                                                       07690008
32320 IVDELE = IVDELE + 1                                               07700008
      WRITE (I02,80003) IVTNUM                                          07710008
      IF (ICZERO) 42320, 2331, 42320                                    07720008
42320 IF (IVCOMP - 32333) 22320,12320,22320                             07730008
12320 IVPASS = IVPASS + 1                                               07740008
      WRITE (I02,80001) IVTNUM                                          07750008
      GO TO 2331                                                        07760008
22320 IVFAIL = IVFAIL + 1                                               07770008
      IVCORR = 32333                                                    07780008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07790008
 2331 CONTINUE                                                          07800008
      IVTNUM = 233                                                      07810008
C                                                                       07820008
C      ****  TEST 233  ****                                             07830008
C                                                                       07840008
      IF (ICZERO) 32330, 2330, 32330                                    07850008
 2330 CONTINUE                                                          07860008
      IVON01 = 11111                                                    07870008
      IVCOMP = (IVON01 + 10111) + 11111                                 07880008
      GO TO 42330                                                       07890008
32330 IVDELE = IVDELE + 1                                               07900008
      WRITE (I02,80003) IVTNUM                                          07910008
      IF (ICZERO) 42330, 2341, 42330                                    07920008
42330 IF (IVCOMP - 32333) 22330,12330,22330                             07930008
12330 IVPASS = IVPASS + 1                                               07940008
      WRITE (I02,80001) IVTNUM                                          07950008
      GO TO 2341                                                        07960008
22330 IVFAIL = IVFAIL + 1                                               07970008
      IVCORR = 32333                                                    07980008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07990008
 2341 CONTINUE                                                          08000008
      IVTNUM = 234                                                      08010008
C                                                                       08020008
C      ****  TEST 234  ****                                             08030008
C                                                                       08040008
      IF (ICZERO) 32340, 2340, 32340                                    08050008
 2340 CONTINUE                                                          08060008
      IVON01 = 10111                                                    08070008
      IVCOMP = 11111 + (11111+IVON01)                                   08080008
      GO TO 42340                                                       08090008
32340 IVDELE = IVDELE + 1                                               08100008
      WRITE (I02,80003) IVTNUM                                          08110008
      IF (ICZERO) 42340, 2351, 42340                                    08120008
42340 IF (IVCOMP - 32333) 22340,12340,22340                             08130008
12340 IVPASS = IVPASS + 1                                               08140008
      WRITE (I02,80001) IVTNUM                                          08150008
      GO TO 2351                                                        08160008
22340 IVFAIL = IVFAIL + 1                                               08170008
      IVCORR = 32333                                                    08180008
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08190008
 2351 CONTINUE                                                          08200008
C                                                                       08210008
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08220008
99999 CONTINUE                                                          08230008
      WRITE (I02,90002)                                                 08240008
      WRITE (I02,90006)                                                 08250008
      WRITE (I02,90002)                                                 08260008
      WRITE (I02,90002)                                                 08270008
      WRITE (I02,90007)                                                 08280008
      WRITE (I02,90002)                                                 08290008
      WRITE (I02,90008)  IVFAIL                                         08300008
      WRITE (I02,90009) IVPASS                                          08310008
      WRITE (I02,90010) IVDELE                                          08320008
C                                                                       08330008
C                                                                       08340008
C     TERMINATE ROUTINE EXECUTION                                       08350008
      STOP                                                              08360008
C                                                                       08370008
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08380008
90000 FORMAT (1H1)                                                      08390008
90002 FORMAT (1H )                                                      08400008
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08410008
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08420008
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08430008
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08440008
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08450008
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08460008
C                                                                       08470008
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08480008
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08490008
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08500008
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08510008
C                                                                       08520008
C     FORMAT STATEMENTS FOR TEST RESULTS                                08530008
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08540008
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08550008
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08560008
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08570008
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08580008
C                                                                       08590008
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM008)                          08600008
      END                                                               08610008
