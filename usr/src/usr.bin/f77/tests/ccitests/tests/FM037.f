C     COMMENT SECTION                                                   00010037
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020037
C     FM037                                                             00030037
C                                                                       00040037
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050037
C     FORM                                                              00060037
C              INTEGER VARIABLE = ARITHMETIC EXPRESSION                 00070037
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THREE INTEGER      00080037
C     CONSTANTS AND THE ARITHMETIC OPERATOR /.  BOTH POSITIVE AND NEGA- 00090037
C     TIVE CONSTANTS ARE USED IN THE ARITHMETIC EXPRESSION.             00100037
C                                                                       00110037
C         THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     00120037
C     AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  00130037
C     IN THE RESULTANT INTEGER VARIABLE.  THE STANDARD STATES 'THE VALUE00140037
C     OF AN INTEGER FACTOR OR TERM IS THE NEAREST INTEGER WHOSE MAGNI-  00150037
C     TUDE DOES NOT EXCEED THE MAGNITUDE OF THE MATHEMATICAL VALUE      00160037
C     REPRESENTED BY THAT FACTOR OR TERM.  THE ASSOCIATIVE AND COMMUTA- 00170037
C     TIVE LAWS DO NOT APPLY IN THE EVALUATION OF INTEGER TERMS CON-    00180037
C     TAINING DIVISION, HENCE THE EVALUATION OF SUCH TERMS MUST EFFEC-  00190037
C     TIVELY PROCEED FROM LEFT TO RIGHT.'                               00200037
C                                                                       00210037
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00220037
C             (1)  INTEGER CONSTANT/INTEGER CONSTANT/INTEGER CONSTANT   00230037
C                      NO TRUNCATION REQUIRED                           00240037
C             (2)  INTEGER CONSTANT/INTEGER CONSTANT/INTEGER CONSTANT   00250037
C                      TRUNCATION REQUIRED                              00260037
C                                                                       00270037
C      REFERENCES                                                       00280037
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00290037
C              X3.9-1978                                                00300037
C                                                                       00310037
C        SECTION 4.3, INTEGER TYPE                                      00320037
C        SECTION 4.3.1, INTEGER CONSTANT                                00330037
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00340037
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00350037
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00360037
C                                                                       00370037
C      **********************************************************       00380037
C                                                                       00390037
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00400037
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00410037
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00420037
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00430037
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00440037
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00450037
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00460037
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00470037
C     OF EXECUTING THESE TESTS.                                         00480037
C                                                                       00490037
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00500037
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00510037
C                                                                       00520037
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00530037
C                                                                       00540037
C                  DEPARTMENT OF THE NAVY                               00550037
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00560037
C                  WASHINGTON, D.C.  20376                              00570037
C                                                                       00580037
C      **********************************************************       00590037
C                                                                       00600037
C                                                                       00610037
C                                                                       00620037
C     INITIALIZATION SECTION                                            00630037
C                                                                       00640037
C     INITIALIZE CONSTANTS                                              00650037
C      **************                                                   00660037
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00670037
      I01 = 5                                                           00680037
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00690037
      I02 = 6                                                           00700037
C     SYSTEM ENVIRONMENT SECTION                                        00710037
C                                                                       00720037
      I01 = 5                                                           00730037
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740037
C     (UNIT NUMBER FOR CARD READER).                                    00750037
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00760037
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00770037
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00780037
C                                                                       00790037
      I02 = 6                                                           00800037
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00810037
C     (UNIT NUMBER FOR PRINTER).                                        00820037
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00830037
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00840037
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00850037
C                                                                       00860037
      IVPASS=0                                                          00870037
      IVFAIL=0                                                          00880037
      IVDELE=0                                                          00890037
      ICZERO=0                                                          00900037
C                                                                       00910037
C     WRITE PAGE HEADERS                                                00920037
      WRITE (I02,90000)                                                 00930037
      WRITE (I02,90001)                                                 00940037
      WRITE (I02,90002)                                                 00950037
      WRITE (I02, 90002)                                                00960037
      WRITE (I02,90003)                                                 00970037
      WRITE (I02,90002)                                                 00980037
      WRITE (I02,90004)                                                 00990037
      WRITE (I02,90002)                                                 01000037
      WRITE (I02,90011)                                                 01010037
      WRITE (I02,90002)                                                 01020037
      WRITE (I02,90002)                                                 01030037
      WRITE (I02,90005)                                                 01040037
      WRITE (I02,90006)                                                 01050037
      WRITE (I02,90002)                                                 01060037
C                                                                       01070037
C     TEST SECTION                                                      01080037
C                                                                       01090037
C         ARITHMETIC ASSIGNMENT STATEMENT                               01100037
C                                                                       01110037
C     TEST 491 THROUGH TEST 519 CONTAIN THREE INTEGER CONSTANTS AND     01120037
C     OPERATOR / IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS       01130037
C         INTEGER VARIABLE = INTEGER CONSTANT/INTEGER CONSTANT/INT.CON. 01140037
C                                                                       01150037
C                                                                       01160037
C     TEST 491 THROUGH TEST 496 - POSITIVE INTEGER CONSTANTS            01170037
C                       NO TRUNCATION REQUIRED                          01180037
C                                                                       01190037
 4911 CONTINUE                                                          01200037
      IVTNUM = 491                                                      01210037
C                                                                       01220037
C      ****  TEST 491  ****                                             01230037
C                                                                       01240037
      IF (ICZERO) 34910, 4910, 34910                                    01250037
 4910 CONTINUE                                                          01260037
      IVCOMP = 24/3/4                                                   01270037
      GO TO 44910                                                       01280037
34910 IVDELE = IVDELE + 1                                               01290037
      WRITE (I02,80003) IVTNUM                                          01300037
      IF (ICZERO) 44910, 4921, 44910                                    01310037
44910 IF (IVCOMP - 2) 24910,14910,24910                                 01320037
14910 IVPASS = IVPASS + 1                                               01330037
      WRITE (I02,80001) IVTNUM                                          01340037
      GO TO 4921                                                        01350037
24910 IVFAIL = IVFAIL + 1                                               01360037
      IVCORR = 2                                                        01370037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01380037
 4921 CONTINUE                                                          01390037
      IVTNUM = 492                                                      01400037
C                                                                       01410037
C      ****  TEST 492  ****                                             01420037
C                                                                       01430037
      IF (ICZERO) 34920, 4920, 34920                                    01440037
 4920 CONTINUE                                                          01450037
      IVCOMP = 330/3/2                                                  01460037
      GO TO 44920                                                       01470037
34920 IVDELE = IVDELE + 1                                               01480037
      WRITE (I02,80003) IVTNUM                                          01490037
      IF (ICZERO) 44920, 4931, 44920                                    01500037
44920 IF (IVCOMP - 55) 24920,14920,24920                                01510037
14920 IVPASS = IVPASS + 1                                               01520037
      WRITE (I02,80001) IVTNUM                                          01530037
      GO TO 4931                                                        01540037
24920 IVFAIL = IVFAIL + 1                                               01550037
      IVCORR = 55                                                       01560037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01570037
 4931 CONTINUE                                                          01580037
      IVTNUM = 493                                                      01590037
C                                                                       01600037
C      ****  TEST 493  ****                                             01610037
C                                                                       01620037
      IF (ICZERO) 34930, 4930, 34930                                    01630037
 4930 CONTINUE                                                          01640037
      IVCOMP = 15249/13/51                                              01650037
      GO TO 44930                                                       01660037
34930 IVDELE = IVDELE + 1                                               01670037
      WRITE (I02,80003) IVTNUM                                          01680037
      IF (ICZERO) 44930, 4941, 44930                                    01690037
44930 IF (IVCOMP - 23) 24930,14930,24930                                01700037
14930 IVPASS = IVPASS + 1                                               01710037
      WRITE (I02,80001) IVTNUM                                          01720037
      GO TO 4941                                                        01730037
24930 IVFAIL = IVFAIL + 1                                               01740037
      IVCORR = 23                                                       01750037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01760037
 4941 CONTINUE                                                          01770037
      IVTNUM = 494                                                      01780037
C                                                                       01790037
C      ****  TEST 494  ****                                             01800037
C                                                                       01810037
      IF (ICZERO) 34940, 4940, 34940                                    01820037
 4940 CONTINUE                                                          01830037
      IVCOMP = 7150/2/25                                                01840037
      GO TO 44940                                                       01850037
34940 IVDELE = IVDELE + 1                                               01860037
      WRITE (I02,80003) IVTNUM                                          01870037
      IF (ICZERO) 44940, 4951, 44940                                    01880037
44940 IF (IVCOMP - 143) 24940,14940,24940                               01890037
14940 IVPASS = IVPASS + 1                                               01900037
      WRITE (I02,80001) IVTNUM                                          01910037
      GO TO 4951                                                        01920037
24940 IVFAIL = IVFAIL + 1                                               01930037
      IVCORR = 143                                                      01940037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01950037
 4951 CONTINUE                                                          01960037
      IVTNUM = 495                                                      01970037
C                                                                       01980037
C      ****  TEST 495  ****                                             01990037
C                                                                       02000037
      IF (ICZERO) 34950, 4950, 34950                                    02010037
 4950 CONTINUE                                                          02020037
      IVCOMP = 32766/2/3                                                02030037
      GO TO 44950                                                       02040037
34950 IVDELE = IVDELE + 1                                               02050037
      WRITE (I02,80003) IVTNUM                                          02060037
      IF (ICZERO) 44950, 4961, 44950                                    02070037
44950 IF (IVCOMP - 5461) 24950,14950,24950                              02080037
14950 IVPASS = IVPASS + 1                                               02090037
      WRITE (I02,80001) IVTNUM                                          02100037
      GO TO 4961                                                        02110037
24950 IVFAIL = IVFAIL + 1                                               02120037
      IVCORR = 5461                                                     02130037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02140037
 4961 CONTINUE                                                          02150037
      IVTNUM = 496                                                      02160037
C                                                                       02170037
C      ****  TEST 496  ****                                             02180037
C                                                                       02190037
      IF (ICZERO) 34960, 4960, 34960                                    02200037
 4960 CONTINUE                                                          02210037
      IVCOMP = 32766/1/1                                                02220037
      GO TO 44960                                                       02230037
34960 IVDELE = IVDELE + 1                                               02240037
      WRITE (I02,80003) IVTNUM                                          02250037
      IF (ICZERO) 44960, 4971, 44960                                    02260037
44960 IF (IVCOMP - 32766) 24960,14960,24960                             02270037
14960 IVPASS = IVPASS + 1                                               02280037
      WRITE (I02,80001) IVTNUM                                          02290037
      GO TO 4971                                                        02300037
24960 IVFAIL = IVFAIL + 1                                               02310037
      IVCORR = 32766                                                    02320037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02330037
C                                                                       02340037
C     TEST 497 THROUGH TEST 502 - POSITIVE INTEGER CONSTANTS            02350037
C                  TRUNCATION REQUIRED                                  02360037
C                                                                       02370037
 4971 CONTINUE                                                          02380037
      IVTNUM = 497                                                      02390037
C                                                                       02400037
C      ****  TEST 497  ****                                             02410037
C                                                                       02420037
      IF (ICZERO) 34970, 4970, 34970                                    02430037
 4970 CONTINUE                                                          02440037
      IVCOMP = 24/3/3                                                   02450037
      GO TO 44970                                                       02460037
34970 IVDELE = IVDELE + 1                                               02470037
      WRITE (I02,80003) IVTNUM                                          02480037
      IF (ICZERO) 44970, 4981, 44970                                    02490037
44970 IF (IVCOMP -2) 24970,14970,24970                                  02500037
14970 IVPASS = IVPASS + 1                                               02510037
      WRITE (I02,80001) IVTNUM                                          02520037
      GO TO 4981                                                        02530037
24970 IVFAIL = IVFAIL + 1                                               02540037
      IVCORR = 2                                                        02550037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02560037
 4981 CONTINUE                                                          02570037
      IVTNUM = 498                                                      02580037
C                                                                       02590037
C      ****  TEST 498  ****                                             02600037
C                                                                       02610037
      IF (ICZERO) 34980, 4980, 34980                                    02620037
 4980 CONTINUE                                                          02630037
      IVCOMP = 230/2/3                                                  02640037
      GO TO 44980                                                       02650037
34980 IVDELE = IVDELE + 1                                               02660037
      WRITE (I02,80003) IVTNUM                                          02670037
      IF (ICZERO) 44980, 4991, 44980                                    02680037
44980 IF (IVCOMP - 38) 24980,14980,24980                                02690037
14980 IVPASS = IVPASS + 1                                               02700037
      WRITE (I02,80001) IVTNUM                                          02710037
      GO TO 4991                                                        02720037
24980 IVFAIL = IVFAIL + 1                                               02730037
      IVCORR = 38                                                       02740037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02750037
 4991 CONTINUE                                                          02760037
      IVTNUM = 499                                                      02770037
C                                                                       02780037
C      ****  TEST 499  ****                                             02790037
C                                                                       02800037
      IF (ICZERO) 34990, 4990, 34990                                    02810037
 4990 CONTINUE                                                          02820037
      IVCOMP = 7151/3/10                                                02830037
      GO TO 44990                                                       02840037
34990 IVDELE = IVDELE + 1                                               02850037
      WRITE (I02,80003) IVTNUM                                          02860037
      IF (ICZERO) 44990, 5001, 44990                                    02870037
44990 IF (IVCOMP - 238) 24990,14990,24990                               02880037
14990 IVPASS = IVPASS + 1                                               02890037
      WRITE (I02,80001) IVTNUM                                          02900037
      GO TO 5001                                                        02910037
24990 IVFAIL = IVFAIL + 1                                               02920037
      IVCORR = 238                                                      02930037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02940037
 5001 CONTINUE                                                          02950037
      IVTNUM = 500                                                      02960037
C                                                                       02970037
C      ****  TEST 500  ****                                             02980037
C                                                                       02990037
      IF (ICZERO) 35000, 5000, 35000                                    03000037
 5000 CONTINUE                                                          03010037
      IVCOMP = 15248/51/13                                              03020037
      GO TO 45000                                                       03030037
35000 IVDELE = IVDELE + 1                                               03040037
      WRITE (I02,80003) IVTNUM                                          03050037
      IF (ICZERO) 45000, 5011, 45000                                    03060037
45000 IF (IVCOMP - 22) 25000,15000,25000                                03070037
15000 IVPASS = IVPASS + 1                                               03080037
      WRITE (I02,80001) IVTNUM                                          03090037
      GO TO 5011                                                        03100037
25000 IVFAIL = IVFAIL + 1                                               03110037
      IVCORR = 22                                                       03120037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03130037
 5011 CONTINUE                                                          03140037
      IVTNUM = 501                                                      03150037
C                                                                       03160037
C      ****  TEST 501  ****                                             03170037
C                                                                       03180037
      IF (ICZERO) 35010, 5010, 35010                                    03190037
 5010 CONTINUE                                                          03200037
      IVCOMP = 27342/4/3                                                03210037
      GO TO 45010                                                       03220037
35010 IVDELE = IVDELE + 1                                               03230037
      WRITE (I02,80003) IVTNUM                                          03240037
      IF (ICZERO) 45010, 5021, 45010                                    03250037
45010 IF (IVCOMP - 2278) 25010,15010,25010                              03260037
15010 IVPASS = IVPASS + 1                                               03270037
      WRITE (I02,80001) IVTNUM                                          03280037
      GO TO 5021                                                        03290037
25010 IVFAIL = IVFAIL + 1                                               03300037
      IVCORR = 2278                                                     03310037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03320037
 5021 CONTINUE                                                          03330037
      IVTNUM = 502                                                      03340037
C                                                                       03350037
C      ****  TEST 502  ****                                             03360037
C                                                                       03370037
      IF (ICZERO) 35020, 5020, 35020                                    03380037
 5020 CONTINUE                                                          03390037
      IVCOMP = 32767/2/1                                                03400037
      GO TO 45020                                                       03410037
35020 IVDELE = IVDELE + 1                                               03420037
      WRITE (I02,80003) IVTNUM                                          03430037
      IF (ICZERO) 45020, 5031, 45020                                    03440037
45020 IF (IVCOMP - 16383) 25020,15020,25020                             03450037
15020 IVPASS = IVPASS + 1                                               03460037
      WRITE (I02,80001) IVTNUM                                          03470037
      GO TO 5031                                                        03480037
25020 IVFAIL = IVFAIL + 1                                               03490037
      IVCORR = 16383                                                    03500037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03510037
C                                                                       03520037
C     TEST 503 THROUGH TEST 507 - NEGATIVE INTEGER CONSTANTS INCLUDED   03530037
C                  NO TRUNCATION REQUIRED                               03540037
C                                                                       03550037
 5031 CONTINUE                                                          03560037
      IVTNUM = 503                                                      03570037
C                                                                       03580037
C      ****  TEST 503  ****                                             03590037
C                                                                       03600037
      IF (ICZERO) 35030, 5030, 35030                                    03610037
 5030 CONTINUE                                                          03620037
      IVCOMP = -24/3/4                                                  03630037
      GO TO 45030                                                       03640037
35030 IVDELE = IVDELE + 1                                               03650037
      WRITE (I02,80003) IVTNUM                                          03660037
      IF (ICZERO) 45030, 5041, 45030                                    03670037
45030 IF (IVCOMP +2) 25030,15030,25030                                  03680037
15030 IVPASS = IVPASS + 1                                               03690037
      WRITE (I02,80001) IVTNUM                                          03700037
      GO TO 5041                                                        03710037
25030 IVFAIL = IVFAIL + 1                                               03720037
      IVCORR = -2                                                       03730037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03740037
 5041 CONTINUE                                                          03750037
      IVTNUM = 504                                                      03760037
C                                                                       03770037
C      ****  TEST 504  ****                                             03780037
C                                                                       03790037
      IF (ICZERO) 35040, 5040, 35040                                    03800037
 5040 CONTINUE                                                          03810037
      IVCOMP = 330/(-3)/2                                               03820037
      GO TO 45040                                                       03830037
35040 IVDELE = IVDELE + 1                                               03840037
      WRITE (I02,80003) IVTNUM                                          03850037
      IF (ICZERO) 45040, 5051, 45040                                    03860037
45040 IF (IVCOMP + 55) 25040,15040,25040                                03870037
15040 IVPASS = IVPASS + 1                                               03880037
      WRITE (I02,80001) IVTNUM                                          03890037
      GO TO 5051                                                        03900037
25040 IVFAIL = IVFAIL + 1                                               03910037
      IVCORR = -55                                                      03920037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03930037
 5051 CONTINUE                                                          03940037
      IVTNUM = 505                                                      03950037
C                                                                       03960037
C      ****  TEST 505  ****                                             03970037
C                                                                       03980037
      IF (ICZERO) 35050, 5050, 35050                                    03990037
 5050 CONTINUE                                                          04000037
      IVCOMP = 15249/(-13)/(-51)                                        04010037
      GO TO 45050                                                       04020037
35050 IVDELE = IVDELE + 1                                               04030037
      WRITE (I02,80003) IVTNUM                                          04040037
      IF (ICZERO) 45050, 5061, 45050                                    04050037
45050 IF (IVCOMP - 23) 25050,15050,25050                                04060037
15050 IVPASS = IVPASS + 1                                               04070037
      WRITE (I02,80001) IVTNUM                                          04080037
      GO TO 5061                                                        04090037
25050 IVFAIL = IVFAIL + 1                                               04100037
      IVCORR = 23                                                       04110037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04120037
 5061 CONTINUE                                                          04130037
      IVTNUM = 506                                                      04140037
C                                                                       04150037
C      ****  TEST 506  ****                                             04160037
C                                                                       04170037
      IF (ICZERO) 35060, 5060, 35060                                    04180037
 5060 CONTINUE                                                          04190037
      IVCOMP = -7150/(-2)/(-25)                                         04200037
      GO TO 45060                                                       04210037
35060 IVDELE = IVDELE + 1                                               04220037
      WRITE (I02,80003) IVTNUM                                          04230037
      IF (ICZERO) 45060, 5071, 45060                                    04240037
45060 IF (IVCOMP + 143) 25060,15060,25060                               04250037
15060 IVPASS = IVPASS + 1                                               04260037
      WRITE (I02,80001) IVTNUM                                          04270037
      GO TO 5071                                                        04280037
25060 IVFAIL = IVFAIL + 1                                               04290037
      IVCORR = -143                                                     04300037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04310037
 5071 CONTINUE                                                          04320037
      IVTNUM = 507                                                      04330037
C                                                                       04340037
C      ****  TEST 507  ****                                             04350037
C                                                                       04360037
      IF (ICZERO) 35070, 5070, 35070                                    04370037
 5070 CONTINUE                                                          04380037
      IVCOMP = (-32766)/(-2)/(-3)                                       04390037
      GO TO 45070                                                       04400037
35070 IVDELE = IVDELE + 1                                               04410037
      WRITE (I02,80003) IVTNUM                                          04420037
      IF (ICZERO) 45070, 5081, 45070                                    04430037
45070 IF (IVCOMP + 5461) 25070,15070,25070                              04440037
15070 IVPASS = IVPASS + 1                                               04450037
      WRITE (I02,80001) IVTNUM                                          04460037
      GO TO 5081                                                        04470037
25070 IVFAIL = IVFAIL + 1                                               04480037
      IVCORR = -5461                                                    04490037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04500037
C                                                                       04510037
C     TEST 508 THROUGH TEST 513 - NEGATIVE INTEGER CONSTANTS INCLUDED   04520037
C                       TRUNCATION REQUIRED                             04530037
C                                                                       04540037
 5081 CONTINUE                                                          04550037
      IVTNUM = 508                                                      04560037
C                                                                       04570037
C      ****  TEST 508  ****                                             04580037
C                                                                       04590037
      IF (ICZERO) 35080, 5080, 35080                                    04600037
 5080 CONTINUE                                                          04610037
      IVCOMP = -24/3/3                                                  04620037
      GO TO 45080                                                       04630037
35080 IVDELE = IVDELE + 1                                               04640037
      WRITE (I02,80003) IVTNUM                                          04650037
      IF (ICZERO) 45080, 5091, 45080                                    04660037
45080 IF (IVCOMP + 2) 25080,15080,25080                                 04670037
15080 IVPASS = IVPASS + 1                                               04680037
      WRITE (I02,80001) IVTNUM                                          04690037
      GO TO 5091                                                        04700037
25080 IVFAIL = IVFAIL + 1                                               04710037
      IVCORR = -2                                                       04720037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04730037
 5091 CONTINUE                                                          04740037
      IVTNUM = 509                                                      04750037
C                                                                       04760037
C      ****  TEST 509  ****                                             04770037
C                                                                       04780037
      IF (ICZERO) 35090, 5090, 35090                                    04790037
 5090 CONTINUE                                                          04800037
      IVCOMP = 230/(-2)/3                                               04810037
      GO TO 45090                                                       04820037
35090 IVDELE = IVDELE + 1                                               04830037
      WRITE (I02,80003) IVTNUM                                          04840037
      IF (ICZERO) 45090, 5101, 45090                                    04850037
45090 IF (IVCOMP + 38) 25090,15090,25090                                04860037
15090 IVPASS = IVPASS + 1                                               04870037
      WRITE (I02,80001) IVTNUM                                          04880037
      GO TO 5101                                                        04890037
25090 IVFAIL = IVFAIL + 1                                               04900037
      IVCORR = -38                                                      04910037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04920037
 5101 CONTINUE                                                          04930037
      IVTNUM = 510                                                      04940037
C                                                                       04950037
C      ****  TEST 510  ****                                             04960037
C                                                                       04970037
      IF (ICZERO) 35100, 5100, 35100                                    04980037
 5100 CONTINUE                                                          04990037
      IVCOMP = 7151/(-3)/(-10)                                          05000037
      GO TO 45100                                                       05010037
35100 IVDELE = IVDELE + 1                                               05020037
      WRITE (I02,80003) IVTNUM                                          05030037
      IF (ICZERO) 45100, 5111, 45100                                    05040037
45100 IF (IVCOMP - 238) 25100,15100,25100                               05050037
15100 IVPASS = IVPASS + 1                                               05060037
      WRITE (I02,80001) IVTNUM                                          05070037
      GO TO 5111                                                        05080037
25100 IVFAIL = IVFAIL + 1                                               05090037
      IVCORR = 238                                                      05100037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05110037
 5111 CONTINUE                                                          05120037
      IVTNUM = 511                                                      05130037
C                                                                       05140037
C      ****  TEST 511  ****                                             05150037
C                                                                       05160037
      IF (ICZERO) 35110, 5110, 35110                                    05170037
 5110 CONTINUE                                                          05180037
      IVCOMP = -15248/(-51)/(-13)                                       05190037
      GO TO 45110                                                       05200037
35110 IVDELE = IVDELE + 1                                               05210037
      WRITE (I02,80003) IVTNUM                                          05220037
      IF (ICZERO) 45110, 5121, 45110                                    05230037
45110 IF (IVCOMP + 22) 25110,15110,25110                                05240037
15110 IVPASS = IVPASS + 1                                               05250037
      WRITE (I02,80001) IVTNUM                                          05260037
      GO TO 5121                                                        05270037
25110 IVFAIL = IVFAIL + 1                                               05280037
      IVCORR = -22                                                      05290037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05300037
 5121 CONTINUE                                                          05310037
      IVTNUM = 512                                                      05320037
C                                                                       05330037
C      ****  TEST 512  ****                                             05340037
C                                                                       05350037
      IF (ICZERO) 35120, 5120, 35120                                    05360037
 5120 CONTINUE                                                          05370037
      IVCOMP = (-27342)/(-4)/(-3)                                       05380037
      GO TO 45120                                                       05390037
35120 IVDELE = IVDELE + 1                                               05400037
      WRITE (I02,80003) IVTNUM                                          05410037
      IF (ICZERO) 45120, 5131, 45120                                    05420037
45120 IF (IVCOMP + 2278) 25120,15120,25120                              05430037
15120 IVPASS = IVPASS + 1                                               05440037
      WRITE (I02,80001) IVTNUM                                          05450037
      GO TO 5131                                                        05460037
25120 IVFAIL = IVFAIL + 1                                               05470037
      IVCORR = -2278                                                    05480037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05490037
 5131 CONTINUE                                                          05500037
      IVTNUM = 513                                                      05510037
C                                                                       05520037
C      ****  TEST 513  ****                                             05530037
C                                                                       05540037
      IF (ICZERO) 35130, 5130, 35130                                    05550037
 5130 CONTINUE                                                          05560037
      IVCOMP = 32767/2/(-1)                                             05570037
      GO TO 45130                                                       05580037
35130 IVDELE = IVDELE + 1                                               05590037
      WRITE (I02,80003) IVTNUM                                          05600037
      IF (ICZERO) 45130, 5141, 45130                                    05610037
45130 IF (IVCOMP + 16383) 25130,15130,25130                             05620037
15130 IVPASS = IVPASS + 1                                               05630037
      WRITE (I02,80001) IVTNUM                                          05640037
      GO TO 5141                                                        05650037
25130 IVFAIL = IVFAIL + 1                                               05660037
      IVCORR = -16383                                                   05670037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05680037
C                                                                       05690037
C     TEST 514 THROUGH TEST 519 - POSITIVE AND NEGATIVE SIGNED INTEGER  05700037
C           CONSTANTS IN ARITHMETIC EXPRESSION.                         05710037
C                                                                       05720037
 5141 CONTINUE                                                          05730037
      IVTNUM = 514                                                      05740037
C                                                                       05750037
C      ****  TEST 514  ****                                             05760037
C                                                                       05770037
      IF (ICZERO) 35140, 5140, 35140                                    05780037
 5140 CONTINUE                                                          05790037
      IVCOMP = +24/(-3)/4                                               05800037
      GO TO 45140                                                       05810037
35140 IVDELE = IVDELE + 1                                               05820037
      WRITE (I02,80003) IVTNUM                                          05830037
      IF (ICZERO) 45140, 5151, 45140                                    05840037
45140 IF (IVCOMP +2) 25140,15140,25140                                  05850037
15140 IVPASS = IVPASS + 1                                               05860037
      WRITE (I02,80001) IVTNUM                                          05870037
      GO TO 5151                                                        05880037
25140 IVFAIL = IVFAIL + 1                                               05890037
      IVCORR = -2                                                       05900037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05910037
 5151 CONTINUE                                                          05920037
      IVTNUM = 515                                                      05930037
C                                                                       05940037
C      ****  TEST 515  ****                                             05950037
C                                                                       05960037
      IF (ICZERO) 35150, 5150, 35150                                    05970037
 5150 CONTINUE                                                          05980037
      IVCOMP = 24/(+3)/(-4)                                             05990037
      GO TO 45150                                                       06000037
35150 IVDELE = IVDELE + 1                                               06010037
      WRITE (I02,80003) IVTNUM                                          06020037
      IF (ICZERO) 45150, 5161, 45150                                    06030037
45150 IF (IVCOMP +2) 25150,15150,25150                                  06040037
15150 IVPASS = IVPASS + 1                                               06050037
      WRITE (I02,80001) IVTNUM                                          06060037
      GO TO 5161                                                        06070037
25150 IVFAIL = IVFAIL + 1                                               06080037
      IVCORR = -2                                                       06090037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06100037
 5161 CONTINUE                                                          06110037
      IVTNUM = 516                                                      06120037
C                                                                       06130037
C      ****  TEST 516  ****                                             06140037
C                                                                       06150037
      IF (ICZERO) 35160, 5160, 35160                                    06160037
 5160 CONTINUE                                                          06170037
      IVCOMP = -24/(-3)/(+4)                                            06180037
      GO TO 45160                                                       06190037
35160 IVDELE = IVDELE + 1                                               06200037
      WRITE (I02,80003) IVTNUM                                          06210037
      IF (ICZERO) 45160, 5171, 45160                                    06220037
45160 IF (IVCOMP -2) 25160,15160,25160                                  06230037
15160 IVPASS = IVPASS + 1                                               06240037
      WRITE (I02,80001) IVTNUM                                          06250037
      GO TO 5171                                                        06260037
25160 IVFAIL = IVFAIL + 1                                               06270037
      IVCORR = 2                                                        06280037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06290037
 5171 CONTINUE                                                          06300037
      IVTNUM = 517                                                      06310037
C                                                                       06320037
C      ****  TEST 517  ****                                             06330037
C                                                                       06340037
      IF (ICZERO) 35170, 5170, 35170                                    06350037
 5170 CONTINUE                                                          06360037
      IVCOMP = -16811/(-16812)/(+1)                                     06370037
      GO TO 45170                                                       06380037
35170 IVDELE = IVDELE + 1                                               06390037
      WRITE (I02,80003) IVTNUM                                          06400037
      IF (ICZERO) 45170, 5181, 45170                                    06410037
45170 IF (IVCOMP - 0) 25170,15170,25170                                 06420037
15170 IVPASS = IVPASS + 1                                               06430037
      WRITE (I02,80001) IVTNUM                                          06440037
      GO TO 5181                                                        06450037
25170 IVFAIL = IVFAIL + 1                                               06460037
      IVCORR = 0                                                        06470037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06480037
 5181 CONTINUE                                                          06490037
      IVTNUM = 518                                                      06500037
C                                                                       06510037
C      ****  TEST 518  ****                                             06520037
C                                                                       06530037
      IF (ICZERO) 35180, 5180, 35180                                    06540037
 5180 CONTINUE                                                          06550037
      IVCOMP = (-16811) / (+16811) / (+1)                               06560037
      GO TO 45180                                                       06570037
35180 IVDELE = IVDELE + 1                                               06580037
      WRITE (I02,80003) IVTNUM                                          06590037
      IF (ICZERO) 45180, 5191, 45180                                    06600037
45180 IF (IVCOMP +1) 25180,15180,25180                                  06610037
15180 IVPASS = IVPASS + 1                                               06620037
      WRITE (I02,80001) IVTNUM                                          06630037
      GO TO 5191                                                        06640037
25180 IVFAIL = IVFAIL + 1                                               06650037
      IVCORR = -1                                                       06660037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06670037
 5191 CONTINUE                                                          06680037
      IVTNUM = 519                                                      06690037
C                                                                       06700037
C      ****  TEST 519  ****                                             06710037
C                                                                       06720037
      IF (ICZERO) 35190, 5190, 35190                                    06730037
 5190 CONTINUE                                                          06740037
      IVCOMP = (-335)/(+168)/(+1)                                       06750037
      GO TO 45190                                                       06760037
35190 IVDELE = IVDELE + 1                                               06770037
      WRITE (I02,80003) IVTNUM                                          06780037
      IF (ICZERO) 45190, 5201, 45190                                    06790037
45190 IF (IVCOMP + 1) 25190,15190,25190                                 06800037
15190 IVPASS = IVPASS + 1                                               06810037
      WRITE (I02,80001) IVTNUM                                          06820037
      GO TO 5201                                                        06830037
25190 IVFAIL = IVFAIL + 1                                               06840037
      IVCORR = -1                                                       06850037
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06860037
C      ****    END OF TESTS    ****                                     06870037
 5201 CONTINUE                                                          06880037
C                                                                       06890037
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             06900037
99999 CONTINUE                                                          06910037
      WRITE (I02,90002)                                                 06920037
      WRITE (I02,90006)                                                 06930037
      WRITE (I02,90002)                                                 06940037
      WRITE (I02,90002)                                                 06950037
      WRITE (I02,90007)                                                 06960037
      WRITE (I02,90002)                                                 06970037
      WRITE (I02,90008)  IVFAIL                                         06980037
      WRITE (I02,90009) IVPASS                                          06990037
      WRITE (I02,90010) IVDELE                                          07000037
C                                                                       07010037
C                                                                       07020037
C     TERMINATE ROUTINE EXECUTION                                       07030037
      STOP                                                              07040037
C                                                                       07050037
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07060037
90000 FORMAT (1H1)                                                      07070037
90002 FORMAT (1H )                                                      07080037
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07090037
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07100037
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07110037
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07120037
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07130037
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07140037
C                                                                       07150037
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07160037
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07170037
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07180037
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07190037
C                                                                       07200037
C     FORMAT STATEMENTS FOR TEST RESULTS                                07210037
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07220037
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07230037
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07240037
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07250037
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07260037
C                                                                       07270037
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM037)                          07280037
      END                                                               07290037
