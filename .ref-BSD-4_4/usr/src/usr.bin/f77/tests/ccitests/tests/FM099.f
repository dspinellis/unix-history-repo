C     COMMENT SECTION                                                   00010099
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020099
C     FM099                                                             00030099
C                                                                       00040099
C     THIS ROUTINE TESTS VARIOUS MATHEMATICAL FUNCTIONS WHERE BOTH THE  00050099
C     FUNCTION TYPE AND ARGUMENTS ARE REAL.  THE REAL VARIABLES AND     00060099
C     CONSTANTS CONTAIN BOTH POSITIVE AND NEGATIVE VALUES.  THE         00070099
C     FUNCTIONS TESTED IN FM099 INCLUDE                                 00080099
C                                                                       00090099
C                                                     TYPE OF           00100099
C       FUNCTION                    NAME       ARGUMENT     FUNCTION    00110099
C       ----------------            ----        --------    --------    00120099
C         EXPONENTIAL               EXP        REAL         REAL        00130099
C         NATURAL LOGARITHM         ALOG       REAL         REAL        00140099
C         COMMON LOGARITHM          ALOG10     REAL         REAL        00150099
C         SQUARE ROOT               SQRT       REAL         REAL        00160099
C         TRIGONOMETRIC SINE        SIN        REAL         REAL        00170099
C         TRIGONOMETRIC COSINE      COS        REAL         REAL        00180099
C         HYPERBOLIC TANGENT        TANH       REAL         REAL        00190099
C         ARCTANGENT                ATAN       REAL         REAL        00200099
C                                   ATAN2      REAL         REAL        00210099
C                                                                       00220099
C      REFERENCES                                                       00230099
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00240099
C              X3.9-1978                                                00250099
C                                                                       00260099
C        SECTION 8.7, EXTERNAL STATEMENT                                00270099
C        SECTION 15.5.2, FUNCTION REFERENCE                             00280099
C                                                                       00290099
C                                                                       00300099
C      **********************************************************       00310099
C                                                                       00320099
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00330099
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00340099
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00350099
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00360099
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00370099
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00380099
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00390099
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00400099
C     OF EXECUTING THESE TESTS.                                         00410099
C                                                                       00420099
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00430099
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00440099
C                                                                       00450099
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00460099
C                                                                       00470099
C                  DEPARTMENT OF THE NAVY                               00480099
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00490099
C                  WASHINGTON, D.C.  20376                              00500099
C                                                                       00510099
C      **********************************************************       00520099
C                                                                       00530099
C                                                                       00540099
C                                                                       00550099
C     INITIALIZATION SECTION                                            00560099
C                                                                       00570099
C     INITIALIZE CONSTANTS                                              00580099
C      **************                                                   00590099
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00600099
      I01 = 5                                                           00610099
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00620099
      I02 = 6                                                           00630099
C     SYSTEM ENVIRONMENT SECTION                                        00640099
C                                                                       00650099
      I01 = 5                                                           00660099
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670099
C     (UNIT NUMBER FOR CARD READER).                                    00680099
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00690099
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00700099
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00710099
C                                                                       00720099
      I02 = 6                                                           00730099
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00740099
C     (UNIT NUMBER FOR PRINTER).                                        00750099
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00760099
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00770099
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00780099
C                                                                       00790099
      IVPASS=0                                                          00800099
      IVFAIL=0                                                          00810099
      IVDELE=0                                                          00820099
      ICZERO=0                                                          00830099
C                                                                       00840099
C     WRITE PAGE HEADERS                                                00850099
      WRITE (I02,90000)                                                 00860099
      WRITE (I02,90001)                                                 00870099
      WRITE (I02,90002)                                                 00880099
      WRITE (I02, 90002)                                                00890099
      WRITE (I02,90003)                                                 00900099
      WRITE (I02,90002)                                                 00910099
      WRITE (I02,90004)                                                 00920099
      WRITE (I02,90002)                                                 00930099
      WRITE (I02,90011)                                                 00940099
      WRITE (I02,90002)                                                 00950099
      WRITE (I02,90002)                                                 00960099
      WRITE (I02,90005)                                                 00970099
      WRITE (I02,90006)                                                 00980099
      WRITE (I02,90002)                                                 00990099
C                                                                       01000099
C     TEST SECTION                                                      01010099
C                                                                       01020099
C     TEST 939 THROUGH TEST 942 CONTAIN FUNCTION TESTS FOR EXPONENTIAL  01030099
C     FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL                01040099
C                                                                       01050099
      IVTNUM = 939                                                      01060099
C                                                                       01070099
C      ****  TEST 939  ****                                             01080099
C                                                                       01090099
      IF (ICZERO) 39390, 9390, 39390                                    01100099
 9390 CONTINUE                                                          01110099
      RVON01 = 0.0                                                      01120099
      RVCOMP = EXP (RVON01)                                             01130099
      GO TO 49390                                                       01140099
39390 IVDELE = IVDELE + 1                                               01150099
      WRITE (I02,80003) IVTNUM                                          01160099
      IF (ICZERO) 49390, 9401, 49390                                    01170099
49390 IF (RVCOMP - 0.95) 29390,19390,49391                              01180099
49391 IF (RVCOMP - 1.05) 19390,19390,29390                              01190099
19390 IVPASS = IVPASS + 1                                               01200099
      WRITE (I02,80001) IVTNUM                                          01210099
      GO TO 9401                                                        01220099
29390 IVFAIL = IVFAIL + 1                                               01230099
      RVCORR = 1.00                                                     01240099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01250099
 9401 CONTINUE                                                          01260099
      IVTNUM = 940                                                      01270099
C                                                                       01280099
C      ****  TEST 940  ****                                             01290099
C                                                                       01300099
      IF (ICZERO) 39400, 9400, 39400                                    01310099
 9400 CONTINUE                                                          01320099
      RVCOMP = EXP (0.5)                                                01330099
      GO TO 49400                                                       01340099
39400 IVDELE = IVDELE + 1                                               01350099
      WRITE (I02,80003) IVTNUM                                          01360099
      IF (ICZERO) 49400, 9411, 49400                                    01370099
49400 IF (RVCOMP - 1.60) 29400,19400,49401                              01380099
49401 IF (RVCOMP - 1.70) 19400,19400,29400                              01390099
19400 IVPASS = IVPASS + 1                                               01400099
      WRITE (I02,80001) IVTNUM                                          01410099
      GO TO 9411                                                        01420099
29400 IVFAIL = IVFAIL + 1                                               01430099
      RVCORR = 1.65                                                     01440099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01450099
 9411 CONTINUE                                                          01460099
      IVTNUM = 941                                                      01470099
C                                                                       01480099
C      ****  TEST 941  ****                                             01490099
C                                                                       01500099
      IF (ICZERO) 39410, 9410, 39410                                    01510099
 9410 CONTINUE                                                          01520099
      RVON01 = .1E1                                                     01530099
      RVCOMP = EXP (RVON01)                                             01540099
      GO TO 49410                                                       01550099
39410 IVDELE = IVDELE + 1                                               01560099
      WRITE (I02,80003) IVTNUM                                          01570099
      IF (ICZERO) 49410, 9421, 49410                                    01580099
49410 IF (RVCOMP - 2.67) 29410,19410,49411                              01590099
49411 IF (RVCOMP - 2.77) 19410,19410,29410                              01600099
19410 IVPASS = IVPASS + 1                                               01610099
      WRITE (I02,80001) IVTNUM                                          01620099
      GO TO 9421                                                        01630099
29410 IVFAIL = IVFAIL + 1                                               01640099
      RVCORR = 2.72                                                     01650099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01660099
 9421 CONTINUE                                                          01670099
      IVTNUM = 942                                                      01680099
C                                                                       01690099
C      ****  TEST 942  ****                                             01700099
C                                                                       01710099
      IF (ICZERO) 39420, 9420, 39420                                    01720099
 9420 CONTINUE                                                          01730099
      RVON01 = -1.0                                                     01740099
      RVCOMP = EXP (RVON01)                                             01750099
      GO TO 49420                                                       01760099
39420 IVDELE = IVDELE + 1                                               01770099
      WRITE (I02,80003) IVTNUM                                          01780099
      IF (ICZERO) 49420, 9431, 49420                                    01790099
49420 IF (RVCOMP - 0.363) 29420,19420,49421                             01800099
49421 IF (RVCOMP - 0.373) 19420,19420,29420                             01810099
19420 IVPASS = IVPASS + 1                                               01820099
      WRITE (I02,80001) IVTNUM                                          01830099
      GO TO 9431                                                        01840099
29420 IVFAIL = IVFAIL + 1                                               01850099
      RVCORR = 0.368                                                    01860099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01870099
 9431 CONTINUE                                                          01880099
C                                                                       01890099
C     TEST 943 THROUGH TEST 945 CONTAIN FUNCTION TESTS FOR NATURAL      01900099
C     LOGARITHM FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL      01910099
C                                                                       01920099
      IVTNUM = 943                                                      01930099
C                                                                       01940099
C      ****  TEST 943  ****                                             01950099
C                                                                       01960099
      IF (ICZERO) 39430, 9430, 39430                                    01970099
 9430 CONTINUE                                                          01980099
      RVON01 = 5E1                                                      01990099
      RVCOMP = ALOG (RVON01)                                            02000099
      GO TO 49430                                                       02010099
39430 IVDELE = IVDELE + 1                                               02020099
      WRITE (I02,80003) IVTNUM                                          02030099
      IF (ICZERO) 49430, 9441, 49430                                    02040099
49430 IF (RVCOMP - 3.9115) 29430,19430,49431                            02050099
49431 IF (RVCOMP - 3.9125) 19430,19430,29430                            02060099
19430 IVPASS = IVPASS + 1                                               02070099
      WRITE (I02,80001) IVTNUM                                          02080099
      GO TO 9441                                                        02090099
29430 IVFAIL = IVFAIL + 1                                               02100099
      RVCORR = 3.9120                                                   02110099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02120099
 9441 CONTINUE                                                          02130099
      IVTNUM = 944                                                      02140099
C                                                                       02150099
C      ****  TEST 944  ****                                             02160099
C                                                                       02170099
      IF (ICZERO) 39440, 9440, 39440                                    02180099
 9440 CONTINUE                                                          02190099
      RVON01 = 1.0                                                      02200099
      RVCOMP = ALOG (RVON01)                                            02210099
      GO TO 49440                                                       02220099
39440 IVDELE = IVDELE + 1                                               02230099
      WRITE (I02,80003) IVTNUM                                          02240099
      IF (ICZERO) 49440, 9451, 49440                                    02250099
49440 IF (RVCOMP + .00005) 29440,19440,49441                            02260099
49441 IF (RVCOMP - .00005) 19440,19440,29440                            02270099
19440 IVPASS = IVPASS + 1                                               02280099
      WRITE (I02,80001) IVTNUM                                          02290099
      GO TO 9451                                                        02300099
29440 IVFAIL = IVFAIL + 1                                               02310099
      RVCORR = 0.00000                                                  02320099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02330099
 9451 CONTINUE                                                          02340099
      IVTNUM = 945                                                      02350099
C                                                                       02360099
C      ****  TEST 945  ****                                             02370099
C                                                                       02380099
      IF (ICZERO) 39450, 9450, 39450                                    02390099
 9450 CONTINUE                                                          02400099
      RVCOMP = ALOG (2.0)                                               02410099
      GO TO 49450                                                       02420099
39450 IVDELE = IVDELE + 1                                               02430099
      WRITE (I02,80003) IVTNUM                                          02440099
      IF (ICZERO) 49450, 9461, 49450                                    02450099
49450 IF (RVCOMP - 0.688) 29450,19450,49451                             02460099
49451 IF (RVCOMP - 0.698) 19450,19450,29450                             02470099
19450 IVPASS = IVPASS + 1                                               02480099
      WRITE (I02,80001) IVTNUM                                          02490099
      GO TO 9461                                                        02500099
29450 IVFAIL = IVFAIL + 1                                               02510099
      RVCORR = 0.693                                                    02520099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02530099
 9461 CONTINUE                                                          02540099
C                                                                       02550099
C     TEST 946 THROUGH TEST 948 CONTAIN FUNCTION TESTS FOR COMMON       02560099
C     LOGARITHM FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL      02570099
C                                                                       02580099
      IVTNUM = 946                                                      02590099
C                                                                       02600099
C      ****  TEST 946  ****                                             02610099
C                                                                       02620099
      IF (ICZERO) 39460, 9460, 39460                                    02630099
 9460 CONTINUE                                                          02640099
      RVON01 = 2E2                                                      02650099
      RVCOMP = ALOG10 (RVON01)                                          02660099
      GO TO 49460                                                       02670099
39460 IVDELE = IVDELE + 1                                               02680099
      WRITE (I02,80003) IVTNUM                                          02690099
      IF (ICZERO) 49460, 9471, 49460                                    02700099
49460 IF (RVCOMP - 2.296) 29460,19460,49461                             02710099
49461 IF (RVCOMP - 2.306) 19460,19460,29460                             02720099
19460 IVPASS = IVPASS + 1                                               02730099
      WRITE (I02,80001) IVTNUM                                          02740099
      GO TO 9471                                                        02750099
29460 IVFAIL = IVFAIL + 1                                               02760099
      RVCORR = 2.301                                                    02770099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02780099
 9471 CONTINUE                                                          02790099
      IVTNUM = 947                                                      02800099
C                                                                       02810099
C      ****  TEST 947  ****                                             02820099
C                                                                       02830099
      IF (ICZERO) 39470, 9470, 39470                                    02840099
 9470 CONTINUE                                                          02850099
      RVON01 = .3E+3                                                    02860099
      RVCOMP = ALOG10 (RVON01)                                          02870099
      GO TO 49470                                                       02880099
39470 IVDELE = IVDELE + 1                                               02890099
      WRITE (I02,80003) IVTNUM                                          02900099
      IF (ICZERO) 49470, 9481, 49470                                    02910099
49470 IF (RVCOMP - 2.472) 29470,19470,49471                             02920099
49471 IF (RVCOMP - 2.482) 19470,19470,29470                             02930099
19470 IVPASS = IVPASS + 1                                               02940099
      WRITE (I02,80001) IVTNUM                                          02950099
      GO TO 9481                                                        02960099
29470 IVFAIL = IVFAIL + 1                                               02970099
      RVCORR = 2.477                                                    02980099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02990099
 9481 CONTINUE                                                          03000099
      IVTNUM = 948                                                      03010099
C                                                                       03020099
C      ****  TEST 948  ****                                             03030099
C                                                                       03040099
      IF (ICZERO) 39480, 9480, 39480                                    03050099
 9480 CONTINUE                                                          03060099
      RVON01 = 1350.0                                                   03070099
      RVCOMP = ALOG10 (RVON01)                                          03080099
      GO TO 49480                                                       03090099
39480 IVDELE = IVDELE + 1                                               03100099
      WRITE (I02,80003) IVTNUM                                          03110099
      IF (ICZERO) 49480, 9491, 49480                                    03120099
49480 IF (RVCOMP - 3.125) 29480,19480,49481                             03130099
49481 IF (RVCOMP - 3.135) 19480,19480,29480                             03140099
19480 IVPASS = IVPASS + 1                                               03150099
      WRITE (I02,80001) IVTNUM                                          03160099
      GO TO 9491                                                        03170099
29480 IVFAIL = IVFAIL + 1                                               03180099
      RVCORR = 3.130                                                    03190099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03200099
 9491 CONTINUE                                                          03210099
C                                                                       03220099
C     TEST 949 THROUGH TEST 951 CONTAIN FUNCTION TESTS FOR SQUARE ROOT  03230099
C     FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL                03240099
C                                                                       03250099
      IVTNUM = 949                                                      03260099
C                                                                       03270099
C      ****  TEST 949  ****                                             03280099
C                                                                       03290099
      IF (ICZERO) 39490, 9490, 39490                                    03300099
 9490 CONTINUE                                                          03310099
      RVON01 = 1.0                                                      03320099
      RVCOMP = SQRT (RVON01)                                            03330099
      GO TO 49490                                                       03340099
39490 IVDELE = IVDELE + 1                                               03350099
      WRITE (I02,80003) IVTNUM                                          03360099
      IF (ICZERO) 49490, 9501, 49490                                    03370099
49490 IF (RVCOMP - 0.95) 29490,19490,49491                              03380099
49491 IF (RVCOMP - 1.05) 19490,19490,29490                              03390099
19490 IVPASS = IVPASS + 1                                               03400099
      WRITE (I02,80001) IVTNUM                                          03410099
      GO TO 9501                                                        03420099
29490 IVFAIL = IVFAIL + 1                                               03430099
      RVCORR = 1.00                                                     03440099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03450099
 9501 CONTINUE                                                          03460099
      IVTNUM = 950                                                      03470099
C                                                                       03480099
C      ****  TEST 950  ****                                             03490099
C                                                                       03500099
      IF (ICZERO) 39500, 9500, 39500                                    03510099
 9500 CONTINUE                                                          03520099
      RVCOMP = SQRT (2.0)                                               03530099
      GO TO 49500                                                       03540099
39500 IVDELE = IVDELE + 1                                               03550099
      WRITE (I02,80003) IVTNUM                                          03560099
      IF (ICZERO) 49500, 9511, 49500                                    03570099
49500 IF (RVCOMP - 1.36) 29500,19500,49501                              03580099
49501 IF (RVCOMP - 1.46) 19500,19500,29500                              03590099
19500 IVPASS = IVPASS + 1                                               03600099
      WRITE (I02,80001) IVTNUM                                          03610099
      GO TO 9511                                                        03620099
29500 IVFAIL = IVFAIL + 1                                               03630099
      RVCORR = 1.41                                                     03640099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03650099
 9511 CONTINUE                                                          03660099
      IVTNUM = 951                                                      03670099
C                                                                       03680099
C      ****  TEST 951  ****                                             03690099
C                                                                       03700099
      IF (ICZERO) 39510, 9510, 39510                                    03710099
 9510 CONTINUE                                                          03720099
      RVON01 = .229E1                                                   03730099
      RVCOMP = SQRT (RVON01)                                            03740099
      GO TO 49510                                                       03750099
39510 IVDELE = IVDELE + 1                                               03760099
      WRITE (I02,80003) IVTNUM                                          03770099
      IF (ICZERO) 49510, 9521, 49510                                    03780099
49510 IF (RVCOMP - 1.46) 29510,19510,49511                              03790099
49511 IF (RVCOMP - 1.56) 19510,19510,29510                              03800099
19510 IVPASS = IVPASS + 1                                               03810099
      WRITE (I02,80001) IVTNUM                                          03820099
      GO TO 9521                                                        03830099
29510 IVFAIL = IVFAIL + 1                                               03840099
      RVCORR = 1.51                                                     03850099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03860099
 9521 CONTINUE                                                          03870099
C                                                                       03880099
C     TEST 952 THROUGH TEST 953 CONTAIN FUNCTION TESTS FOR TRIGONOMETRIC03890099
C     SINE FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL           03900099
C                                                                       03910099
      IVTNUM = 952                                                      03920099
C                                                                       03930099
C      ****  TEST 952  ****                                             03940099
C                                                                       03950099
      IF (ICZERO) 39520, 9520, 39520                                    03960099
 9520 CONTINUE                                                          03970099
      RVON01 = 0.00000                                                  03980099
      RVCOMP = SIN (RVON01)                                             03990099
      GO TO 49520                                                       04000099
39520 IVDELE = IVDELE + 1                                               04010099
      WRITE (I02,80003) IVTNUM                                          04020099
      IF (ICZERO) 49520, 9531, 49520                                    04030099
49520 IF (RVCOMP + .00005) 29520,19520,49521                            04040099
49521 IF (RVCOMP - .00005) 19520,19520,29520                            04050099
19520 IVPASS = IVPASS + 1                                               04060099
      WRITE (I02,80001) IVTNUM                                          04070099
      GO TO 9531                                                        04080099
29520 IVFAIL = IVFAIL + 1                                               04090099
      RVCORR = 0.00000                                                  04100099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04110099
 9531 CONTINUE                                                          04120099
      IVTNUM = 953                                                      04130099
C                                                                       04140099
C      ****  TEST 953  ****                                             04150099
C                                                                       04160099
      IF (ICZERO) 39530, 9530, 39530                                    04170099
 9530 CONTINUE                                                          04180099
      RVON01 = 0.5                                                      04190099
      RVCOMP = SIN (RVON01)                                             04200099
      GO TO 49530                                                       04210099
39530 IVDELE = IVDELE + 1                                               04220099
      WRITE (I02,80003) IVTNUM                                          04230099
      IF (ICZERO) 49530, 9541, 49530                                    04240099
49530 IF (RVCOMP - .474) 29530,19530,49531                              04250099
49531 IF (RVCOMP - .484) 19530,19530,29530                              04260099
19530 IVPASS = IVPASS + 1                                               04270099
      WRITE (I02,80001) IVTNUM                                          04280099
      GO TO 9541                                                        04290099
29530 IVFAIL = IVFAIL + 1                                               04300099
      RVCORR = .479                                                     04310099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04320099
 9541 CONTINUE                                                          04330099
      IVTNUM = 954                                                      04340099
C                                                                       04350099
C      ****  TEST 954  ****                                             04360099
C                                                                       04370099
      IF (ICZERO) 39540, 9540, 39540                                    04380099
 9540 CONTINUE                                                          04390099
      RVON01 = 4E0                                                      04400099
      RVCOMP = SIN (RVON01)                                             04410099
      GO TO 49540                                                       04420099
39540 IVDELE = IVDELE + 1                                               04430099
      WRITE (I02,80003) IVTNUM                                          04440099
      IF (ICZERO) 49540, 9551, 49540                                    04450099
49540 IF (RVCOMP + .762) 29540,19540,49541                              04460099
49541 IF (RVCOMP + .752) 19540,19540,29540                              04470099
19540 IVPASS = IVPASS + 1                                               04480099
      WRITE (I02,80001) IVTNUM                                          04490099
      GO TO 9551                                                        04500099
29540 IVFAIL = IVFAIL + 1                                               04510099
      RVCORR = -.757                                                    04520099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04530099
 9551 CONTINUE                                                          04540099
C                                                                       04550099
C     TEST 955 THROUGH TEST 957 CONTAIN FUNCTION TESTS FOR TRIGONOMETRIC04560099
C     COSINE FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL         04570099
C                                                                       04580099
      IVTNUM = 955                                                      04590099
C                                                                       04600099
C      ****  TEST 955  ****                                             04610099
C                                                                       04620099
      IF (ICZERO) 39550, 9550, 39550                                    04630099
 9550 CONTINUE                                                          04640099
      RVON01 = 0.00000                                                  04650099
      RVCOMP = COS (RVON01)                                             04660099
      GO TO 49550                                                       04670099
39550 IVDELE = IVDELE + 1                                               04680099
      WRITE (I02,80003) IVTNUM                                          04690099
      IF (ICZERO) 49550, 9561, 49550                                    04700099
49550 IF (RVCOMP - .995) 29550,19550,49551                              04710099
49551 IF (RVCOMP - 1.005) 19550,19550,29550                             04720099
19550 IVPASS = IVPASS + 1                                               04730099
      WRITE (I02,80001) IVTNUM                                          04740099
      GO TO 9561                                                        04750099
29550 IVFAIL = IVFAIL + 1                                               04760099
      RVCORR = 1.000                                                    04770099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04780099
 9561 CONTINUE                                                          04790099
      IVTNUM = 956                                                      04800099
C                                                                       04810099
C      ****  TEST 956  ****                                             04820099
C                                                                       04830099
      IF (ICZERO) 39560, 9560, 39560                                    04840099
 9560 CONTINUE                                                          04850099
      RVON01 = 1.0E0                                                    04860099
      RVCOMP = COS (RVON01)                                             04870099
      GO TO 49560                                                       04880099
39560 IVDELE = IVDELE + 1                                               04890099
      WRITE (I02,80003) IVTNUM                                          04900099
      IF (ICZERO) 49560, 9571, 49560                                    04910099
49560 IF (RVCOMP - .535) 29560,19560,49561                              04920099
49561 IF (RVCOMP - .545) 19560,19560,29560                              04930099
19560 IVPASS = IVPASS + 1                                               04940099
      WRITE (I02,80001) IVTNUM                                          04950099
      GO TO 9571                                                        04960099
29560 IVFAIL = IVFAIL + 1                                               04970099
      RVCORR = 0.540                                                    04980099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04990099
 9571 CONTINUE                                                          05000099
      IVTNUM = 957                                                      05010099
C                                                                       05020099
C      ****  TEST 957  ****                                             05030099
C                                                                       05040099
      IF (ICZERO) 39570, 9570, 39570                                    05050099
 9570 CONTINUE                                                          05060099
      RVCOMP = COS (4.0)                                                05070099
      GO TO 49570                                                       05080099
39570 IVDELE = IVDELE + 1                                               05090099
      WRITE (I02,80003) IVTNUM                                          05100099
      IF (ICZERO) 49570, 9581, 49570                                    05110099
49570 IF (RVCOMP + .659) 29570,19570,49571                              05120099
49571 IF (RVCOMP + .649) 19570,19570,29570                              05130099
19570 IVPASS = IVPASS + 1                                               05140099
      WRITE (I02,80001) IVTNUM                                          05150099
      GO TO 9581                                                        05160099
29570 IVFAIL = IVFAIL + 1                                               05170099
      RVCORR = -0.654                                                   05180099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05190099
 9581 CONTINUE                                                          05200099
C                                                                       05210099
C     TEST 958 THROUGH TEST 960 CONTAIN FUNCTION TESTS FOR HYPERBOLIC   05220099
C     TANGENT FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL        05230099
C                                                                       05240099
      IVTNUM = 958                                                      05250099
C                                                                       05260099
C      ****  TEST 958  ****                                             05270099
C                                                                       05280099
      IF (ICZERO) 39580, 9580, 39580                                    05290099
 9580 CONTINUE                                                          05300099
      RVCOMP = TANH (0.0)                                               05310099
      GO TO 49580                                                       05320099
39580 IVDELE = IVDELE + 1                                               05330099
      WRITE (I02,80003) IVTNUM                                          05340099
      IF (ICZERO) 49580, 9591, 49580                                    05350099
49580 IF (RVCOMP + .00005) 29580,19580,49581                            05360099
49581 IF (RVCOMP - .00005) 19580,19580,29580                            05370099
19580 IVPASS = IVPASS + 1                                               05380099
      WRITE (I02,80001) IVTNUM                                          05390099
      GO TO 9591                                                        05400099
29580 IVFAIL = IVFAIL + 1                                               05410099
      RVCORR = 0.00000                                                  05420099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05430099
 9591 CONTINUE                                                          05440099
      IVTNUM = 959                                                      05450099
C                                                                       05460099
C      ****  TEST 959  ****                                             05470099
C                                                                       05480099
      IF (ICZERO) 39590, 9590, 39590                                    05490099
 9590 CONTINUE                                                          05500099
      RVON01 = .5E0                                                     05510099
      RVCOMP = TANH (RVON01)                                            05520099
      GO TO 49590                                                       05530099
39590 IVDELE = IVDELE + 1                                               05540099
      WRITE (I02,80003) IVTNUM                                          05550099
      IF (ICZERO) 49590, 9601, 49590                                    05560099
49590 IF (RVCOMP - .457) 29590,19590,49591                              05570099
49591 IF (RVCOMP - .467) 19590,19590,29590                              05580099
19590 IVPASS = IVPASS + 1                                               05590099
      WRITE (I02,80001) IVTNUM                                          05600099
      GO TO 9601                                                        05610099
29590 IVFAIL = IVFAIL + 1                                               05620099
      RVCORR = 0.462                                                    05630099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05640099
 9601 CONTINUE                                                          05650099
      IVTNUM = 960                                                      05660099
C                                                                       05670099
C      ****  TEST 960  ****                                             05680099
C                                                                       05690099
      IF (ICZERO) 39600, 9600, 39600                                    05700099
 9600 CONTINUE                                                          05710099
      RVON01 = .25                                                      05720099
      RVCOMP = TANH (RVON01)                                            05730099
      GO TO 49600                                                       05740099
39600 IVDELE = IVDELE + 1                                               05750099
      WRITE (I02,80003) IVTNUM                                          05760099
      IF (ICZERO) 49600, 9611, 49600                                    05770099
49600 IF (RVCOMP - .240) 29600,19600,49601                              05780099
49601 IF (RVCOMP - .250) 19600,19600,29600                              05790099
19600 IVPASS = IVPASS + 1                                               05800099
      WRITE (I02,80001) IVTNUM                                          05810099
      GO TO 9611                                                        05820099
29600 IVFAIL = IVFAIL + 1                                               05830099
      RVCORR = 0.245                                                    05840099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05850099
 9611 CONTINUE                                                          05860099
C                                                                       05870099
C     TESTS 961 AND 962 CONTAIN TESTS FOR ARCTANGENT OF THE FORM        05880099
C     ATAN (A) WHERE THE ARGUMENT AND FUNCTION ARE REAL                 05890099
C                                                                       05900099
      IVTNUM = 961                                                      05910099
C                                                                       05920099
C      ****  TEST 961  ****                                             05930099
C                                                                       05940099
      IF (ICZERO) 39610, 9610, 39610                                    05950099
 9610 CONTINUE                                                          05960099
      RVCOMP = ATAN (0.0)                                               05970099
      GO TO 49610                                                       05980099
39610 IVDELE = IVDELE + 1                                               05990099
      WRITE (I02,80003) IVTNUM                                          06000099
      IF (ICZERO) 49610, 9621, 49610                                    06010099
49610 IF (RVCOMP + .00005) 29610,19610,49611                            06020099
49611 IF (RVCOMP - .00005) 19610,19610,29610                            06030099
19610 IVPASS = IVPASS + 1                                               06040099
      WRITE (I02,80001) IVTNUM                                          06050099
      GO TO 9621                                                        06060099
29610 IVFAIL = IVFAIL + 1                                               06070099
      RVCORR = 0.00000                                                  06080099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06090099
 9621 CONTINUE                                                          06100099
      IVTNUM = 962                                                      06110099
C                                                                       06120099
C      ****  TEST 962  ****                                             06130099
C                                                                       06140099
      IF (ICZERO) 39620, 9620, 39620                                    06150099
 9620 CONTINUE                                                          06160099
      RVON01 = 5E-1                                                     06170099
      RVCOMP = ATAN (RVON01)                                            06180099
      GO TO 49620                                                       06190099
39620 IVDELE = IVDELE + 1                                               06200099
      WRITE (I02,80003) IVTNUM                                          06210099
      IF (ICZERO) 49620, 9631, 49620                                    06220099
49620 IF (RVCOMP - .459) 29620,19620,49621                              06230099
49621 IF (RVCOMP - .469) 19620,19620,29620                              06240099
19620 IVPASS = IVPASS + 1                                               06250099
      WRITE (I02,80001) IVTNUM                                          06260099
      GO TO 9631                                                        06270099
29620 IVFAIL = IVFAIL + 1                                               06280099
      RVCORR = 0.464                                                    06290099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06300099
 9631 CONTINUE                                                          06310099
C                                                                       06320099
C     TESTS 963 AND 964 CONTAIN TESTS FOR ARCTANGENT OF THE FORM        06330099
C     ATAN2 (A1,A2) WHERE THE ARGUMENTS AND FUNCTION ARE REAL           06340099
C                                                                       06350099
      IVTNUM = 963                                                      06360099
C                                                                       06370099
C      ****  TEST 963  ****                                             06380099
C                                                                       06390099
      IF (ICZERO) 39630, 9630, 39630                                    06400099
 9630 CONTINUE                                                          06410099
      RVON01 = 0.0                                                      06420099
      RVON02 = 1E0                                                      06430099
      RVCOMP = ATAN2 (RVON01,RVON02)                                    06440099
      GO TO 49630                                                       06450099
39630 IVDELE = IVDELE + 1                                               06460099
      WRITE (I02,80003) IVTNUM                                          06470099
      IF (ICZERO) 49630, 9641, 49630                                    06480099
49630 IF (RVCOMP + .00005) 29630,19630,49631                            06490099
49631 IF (RVCOMP - .00005) 19630,19630,29630                            06500099
19630 IVPASS = IVPASS + 1                                               06510099
      WRITE (I02,80001) IVTNUM                                          06520099
      GO TO 9641                                                        06530099
29630 IVFAIL = IVFAIL + 1                                               06540099
      RVCORR = 0.00000                                                  06550099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06560099
 9641 CONTINUE                                                          06570099
      IVTNUM = 964                                                      06580099
C                                                                       06590099
C      ****  TEST 964  ****                                             06600099
C                                                                       06610099
      IF (ICZERO) 39640, 9640, 39640                                    06620099
 9640 CONTINUE                                                          06630099
      RVON01 = 2E1                                                      06640099
      RVCOMP = ATAN2 (-1.0,RVON01)                                      06650099
      GO TO 49640                                                       06660099
39640 IVDELE = IVDELE + 1                                               06670099
      WRITE (I02,80003) IVTNUM                                          06680099
      IF (ICZERO) 49640, 9651, 49640                                    06690099
49640 IF (RVCOMP + .05001) 29640,19640,49641                            06700099
49641 IF (RVCOMP + .04991) 19640,19640,29640                            06710099
19640 IVPASS = IVPASS + 1                                               06720099
      WRITE (I02,80001) IVTNUM                                          06730099
      GO TO 9651                                                        06740099
29640 IVFAIL = IVFAIL + 1                                               06750099
      RVCORR = -.04996                                                  06760099
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06770099
 9651 CONTINUE                                                          06780099
C                                                                       06790099
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             06800099
99999 CONTINUE                                                          06810099
      WRITE (I02,90002)                                                 06820099
      WRITE (I02,90006)                                                 06830099
      WRITE (I02,90002)                                                 06840099
      WRITE (I02,90002)                                                 06850099
      WRITE (I02,90007)                                                 06860099
      WRITE (I02,90002)                                                 06870099
      WRITE (I02,90008)  IVFAIL                                         06880099
      WRITE (I02,90009) IVPASS                                          06890099
      WRITE (I02,90010) IVDELE                                          06900099
C                                                                       06910099
C                                                                       06920099
C     TERMINATE ROUTINE EXECUTION                                       06930099
      STOP                                                              06940099
C                                                                       06950099
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06960099
90000 FORMAT (1H1)                                                      06970099
90002 FORMAT (1H )                                                      06980099
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06990099
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07000099
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07010099
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07020099
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07030099
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07040099
C                                                                       07050099
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07060099
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07070099
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07080099
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07090099
C                                                                       07100099
C     FORMAT STATEMENTS FOR TEST RESULTS                                07110099
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07120099
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07130099
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07140099
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07150099
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07160099
C                                                                       07170099
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM099)                          07180099
      END                                                               07190099
