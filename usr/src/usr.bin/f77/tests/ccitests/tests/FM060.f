C     COMMENT SECTION                                                   00010060
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020060
C     FM060                                                             00030060
C                                                                       00040060
C         THIS ROUTINE CONTAINS BASIC ARITHMETIC IF STATEMENT TESTS FOR 00050060
C     THE FORMAT                                                        00060060
C                                                                       00070060
C                   IF (E) K1,K2,K3                                     00080060
C                                                                       00090060
C     WHERE E IS A SIMPLE REAL EXPRESSION OF THE FORM                   00100060
C                                                                       00110060
C            REAL VARIABLE                                              00120060
C            REAL VARIABLE - REAL CONSTANT                              00130060
C            REAL VARIABLE + REAL CONSTANT                              00140060
C                                                                       00150060
C     AND K1, K2 AND K3 ARE STATEMENT LABELS.                           00160060
C                                                                       00170060
C         THIS ROUTINE ALSO TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF   00180060
C     THE FORM                                                          00190060
C                  REAL VARIABLE = REAL CONSTANT                        00200060
C                  REAL VARIABLE = REAL VARIABLE                        00210060
C                  REAL VARIABLE = -REAL VARIABLE                       00220060
C                                                                       00230060
C     THE REAL CONSTANTS AND REAL VARIABLES CONTAIN BOTH POSITIVE AND   00240060
C     NEGATIVE VALUES.                                                  00250060
C                                                                       00260060
C         A REAL DATUM IS A PROCESSOR APPROXIMATION TO THE VALUE OF A   00270060
C     REAL NUMBER.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES.   00280060
C                                                                       00290060
C         A BASIC REAL CONSTANT IS WRITTEN AS AN INTEGER PART, A DECIMAL00300060
C     POINT, AND A DECIMAL FRACTION PART IN THAT ORDER.  BOTH THE       00310060
C     INTEGER PART AND THE DECIMAL PART ARE STRINGS OF DIGITS; EITHER   00320060
C     ONE OF THESE STRINGS MAY BE EMPTY BUT NOT BOTH.  THE CONSTANT IS  00330060
C     AN APPROXIMATION TO THE DIGIT STRING INTERPRETED AS A DECIMAL     00340060
C     NUMERAL.                                                          00350060
C                                                                       00360060
C         A DECIMAL EXPONENT IS WRITTEN AS THE LETTER E, FOLLOWED BY AN 00370060
C     OPTIONALLY SIGNED INTEGER CONSTANT.                               00380060
C                                                                       00390060
C         A REAL CONSTANT IS INDICATED BY WRITING A BASIC REAL CONSTANT,00400060
C     A BASIC REAL CONSTANT FOLLOWED BY A DECIMAL EXPONENT, OR AN       00410060
C     INTEGER CONSTANT FOLLOWED BY A DECIMAL EXPONENT.                  00420060
C                                                                       00430060
C      REFERENCES                                                       00440060
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00450060
C              X3.9-1978                                                00460060
C                                                                       00470060
C        SECTION 4.4, REAL TYPE                                         00480060
C        SECTION 4.4.1, REAL CONSTANT                                   00490060
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00500060
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00510060
C        SECTION 11.4, ARITHMETIC IF STATEMENT                          00520060
C                                                                       00530060
C      **********************************************************       00540060
C                                                                       00550060
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00560060
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00570060
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00580060
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00590060
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00600060
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00610060
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00620060
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00630060
C     OF EXECUTING THESE TESTS.                                         00640060
C                                                                       00650060
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00660060
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00670060
C                                                                       00680060
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00690060
C                                                                       00700060
C                  DEPARTMENT OF THE NAVY                               00710060
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00720060
C                  WASHINGTON, D.C.  20376                              00730060
C                                                                       00740060
C      **********************************************************       00750060
C                                                                       00760060
C                                                                       00770060
C                                                                       00780060
C     INITIALIZATION SECTION                                            00790060
C                                                                       00800060
C     INITIALIZE CONSTANTS                                              00810060
C      **************                                                   00820060
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00830060
      I01 = 5                                                           00840060
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00850060
      I02 = 6                                                           00860060
C     SYSTEM ENVIRONMENT SECTION                                        00870060
C                                                                       00880060
      I01 = 5                                                           00890060
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00900060
C     (UNIT NUMBER FOR CARD READER).                                    00910060
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00920060
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00930060
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00940060
C                                                                       00950060
      I02 = 6                                                           00960060
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00970060
C     (UNIT NUMBER FOR PRINTER).                                        00980060
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00990060
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01000060
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01010060
C                                                                       01020060
      IVPASS=0                                                          01030060
      IVFAIL=0                                                          01040060
      IVDELE=0                                                          01050060
      ICZERO=0                                                          01060060
C                                                                       01070060
C     WRITE PAGE HEADERS                                                01080060
      WRITE (I02,90000)                                                 01090060
      WRITE (I02,90001)                                                 01100060
      WRITE (I02,90002)                                                 01110060
      WRITE (I02, 90002)                                                01120060
      WRITE (I02,90003)                                                 01130060
      WRITE (I02,90002)                                                 01140060
      WRITE (I02,90004)                                                 01150060
      WRITE (I02,90002)                                                 01160060
      WRITE (I02,90011)                                                 01170060
      WRITE (I02,90002)                                                 01180060
      WRITE (I02,90002)                                                 01190060
      WRITE (I02,90005)                                                 01200060
      WRITE (I02,90006)                                                 01210060
      WRITE (I02,90002)                                                 01220060
C                                                                       01230060
C     TEST SECTION                                                      01240060
C                                                                       01250060
C         ARITHMETIC IF STATEMENT                                       01260060
C                                                                       01270060
C     TEST 1 THROUGH TEST 3 CONTAIN BASIC ARITHMETIC IF STATEMENT TESTS 01280060
C     WITH A REAL VARIABLE AS ARITHMETIC EXPRESSION.                    01290060
C                                                                       01300060
   11 CONTINUE                                                          01310060
      IVTNUM =   1                                                      01320060
C                                                                       01330060
C      ****  TEST   1  ****                                             01340060
C         TEST 001  - LESS THAN ZERO BRANCH EXPECTED                    01350060
C                                                                       01360060
      IF (ICZERO) 30010,   10, 30010                                    01370060
   10 CONTINUE                                                          01380060
      RVCOMP = 0.0                                                      01390060
      RVON01 = -1.0                                                     01400060
      IF (RVON01)  12,40010, 40010                                      01410060
   12 RVCOMP = RVON01                                                   01420060
      GO TO 40010                                                       01430060
30010 IVDELE = IVDELE + 1                                               01440060
      WRITE (I02,80003) IVTNUM                                          01450060
      IF (ICZERO) 40010,   21, 40010                                    01460060
40010 IF (RVCOMP) 10010,20010,20010                                     01470060
10010 IVPASS = IVPASS + 1                                               01480060
      WRITE (I02,80001) IVTNUM                                          01490060
      GO TO   21                                                        01500060
20010 IVFAIL = IVFAIL + 1                                               01510060
      RVCORR = -1.0                                                     01520060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01530060
   21 CONTINUE                                                          01540060
      IVTNUM =   2                                                      01550060
C                                                                       01560060
C      ****  TEST   2  ****                                             01570060
C         TEST 002  -  EQUAL TO ZERO BRANCH EXPECTED                    01580060
C                                                                       01590060
      IF (ICZERO) 30020,   20, 30020                                    01600060
   20 CONTINUE                                                          01610060
      RVCOMP = 1.0                                                      01620060
      RVON01 = 0.0                                                      01630060
      IF (RVON01) 40020,22,40020                                        01640060
   22 RVCOMP = RVON01                                                   01650060
      GO TO 40020                                                       01660060
30020 IVDELE = IVDELE + 1                                               01670060
      WRITE (I02,80003) IVTNUM                                          01680060
      IF (ICZERO) 40020,   31, 40020                                    01690060
40020 IF (RVCOMP)  20020,10020,20020                                    01700060
10020 IVPASS = IVPASS + 1                                               01710060
      WRITE (I02,80001) IVTNUM                                          01720060
      GO TO   31                                                        01730060
20020 IVFAIL = IVFAIL + 1                                               01740060
      RVCORR = 0.0                                                      01750060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01760060
   31 CONTINUE                                                          01770060
      IVTNUM =   3                                                      01780060
C                                                                       01790060
C      ****  TEST   3  ****                                             01800060
C         TEST 003  -  GREATER THAN ZERO BRANCH EXPECTED                01810060
C                                                                       01820060
      IF (ICZERO) 30030,   30, 30030                                    01830060
   30 CONTINUE                                                          01840060
      RVCOMP = 0.0                                                      01850060
      RVON01 = 1.0                                                      01860060
      IF (RVON01) 40030,40030,32                                        01870060
   32 RVCOMP = RVON01                                                   01880060
      GO TO 40030                                                       01890060
30030 IVDELE = IVDELE + 1                                               01900060
      WRITE (I02,80003) IVTNUM                                          01910060
      IF (ICZERO) 40030,   41, 40030                                    01920060
40030 IF (RVCOMP)  20030,20030,10030                                    01930060
10030 IVPASS = IVPASS + 1                                               01940060
      WRITE (I02,80001) IVTNUM                                          01950060
      GO TO   41                                                        01960060
20030 IVFAIL = IVFAIL + 1                                               01970060
      RVCORR = 1.0                                                      01980060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01990060
   41 CONTINUE                                                          02000060
      IVTNUM =   4                                                      02010060
C                                                                       02020060
C      ****  TEST   4  ****                                             02030060
C     TEST 004  - BASIC IF STATEMENTS TEST                              02040060
C           THESE IF STATEMENTS ARE USED IN REAL VARIABLE TEST          02050060
C           VERIFICATION.  THE ARITHMETIC EXPRESSIONS ARE OF THE FORM   02060060
C                   REAL VARIABLE - REAL CONSTANT                       02070060
C                                                                       02080060
      IF (ICZERO) 30040,   40, 30040                                    02090060
   40 CONTINUE                                                          02100060
      RVCOMP = 4.0                                                      02110060
      RVON01 = 1.0                                                      02120060
      IF (RVON01 - .99995) 40040,42,42                                  02130060
   42 IF (RVON01 - 1.0005) 43,43,40040                                  02140060
   43 RVCOMP = 0.0                                                      02150060
      GO TO 40040                                                       02160060
30040 IVDELE = IVDELE + 1                                               02170060
      WRITE (I02,80003) IVTNUM                                          02180060
      IF (ICZERO) 40040,   51, 40040                                    02190060
40040 IF (RVCOMP) 20040,10040,20040                                     02200060
10040 IVPASS = IVPASS + 1                                               02210060
      WRITE (I02,80001) IVTNUM                                          02220060
      GO TO   51                                                        02230060
20040 IVFAIL = IVFAIL + 1                                               02240060
      RVCORR = 0.0                                                      02250060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02260060
   51 CONTINUE                                                          02270060
      IVTNUM =   5                                                      02280060
C                                                                       02290060
C      ****  TEST   5  ****                                             02300060
C     TEST 005  -  BASIC IF STATEMENTS TEST                             02310060
C           THESE IF STATEMENTS ARE USED IN REAL VARIABLE TEST          02320060
C           VERIFICATION.  THE ARITHMETIC EXPRESSIONS ARE OF THE FORM   02330060
C                   REAL VARIABLE + REAL CONSTANT                       02340060
C                                                                       02350060
      IF (ICZERO) 30050,   50, 30050                                    02360060
   50 CONTINUE                                                          02370060
      RVCOMP = -1.0                                                     02380060
      RVON01 = -1.0                                                     02390060
      IF (RVON01 + 1.0005) 40050,52,52                                  02400060
   52 IF (RVON01 + .99995) 53,53,40050                                  02410060
   53 RVCOMP = 0.0                                                      02420060
      GO TO 40050                                                       02430060
30050 IVDELE = IVDELE + 1                                               02440060
      WRITE (I02,80003) IVTNUM                                          02450060
      IF (ICZERO) 40050,   61, 40050                                    02460060
40050 IF (RVCOMP) 20050,10050,20050                                     02470060
10050 IVPASS = IVPASS + 1                                               02480060
      WRITE (I02,80001) IVTNUM                                          02490060
      GO TO   61                                                        02500060
20050 IVFAIL = IVFAIL + 1                                               02510060
      RVCORR = 0.0                                                      02520060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02530060
C                                                                       02540060
C        ARITHMETIC ASSIGNMENT STATEMENT                                02550060
C                                                                       02560060
C                                                                       02570060
C     TEST 006 THROUGH TEST 025 CONTAIN ARITHMETIC ASSIGNMENT           02580060
C     STATEMENTS OF THE FORM                                            02590060
C              REAL VARIABLE = REAL CONSTANT                            02600060
C                                                                       02610060
C          THE THREE TYPES OF REAL CONSTANTS ARE TESTED WITH POSITIVE   02620060
C     AND NEGATIVE VALUES FOR THE CONSTANTS, AND POSITIVE AND NEGATIVE  02630060
C     EXPONENTS.                                                        02640060
C                                                                       02650060
C     TEST 006 THROUGH TEST 011 - CONSTANT IS BASIC REAL CONSTANT       02660060
C                                                                       02670060
   61 CONTINUE                                                          02680060
      IVTNUM =   6                                                      02690060
C                                                                       02700060
C      ****  TEST   6  ****                                             02710060
C                                                                       02720060
      IF (ICZERO) 30060,   60, 30060                                    02730060
   60 CONTINUE                                                          02740060
      RVCOMP = 2.0                                                      02750060
      GO TO 40060                                                       02760060
30060 IVDELE = IVDELE + 1                                               02770060
      WRITE (I02,80003) IVTNUM                                          02780060
      IF (ICZERO) 40060,   71, 40060                                    02790060
40060 IF (RVCOMP - 1.9995) 20060,10060,40061                            02800060
40061 IF (RVCOMP - 2.0005) 10060,10060,20060                            02810060
10060 IVPASS = IVPASS + 1                                               02820060
      WRITE (I02,80001) IVTNUM                                          02830060
      GO TO   71                                                        02840060
20060 IVFAIL = IVFAIL + 1                                               02850060
      RVCORR = 2.0                                                      02860060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02870060
   71 CONTINUE                                                          02880060
      IVTNUM =   7                                                      02890060
C                                                                       02900060
C      ****  TEST   7  ****                                             02910060
C                                                                       02920060
      IF (ICZERO) 30070,   70, 30070                                    02930060
   70 CONTINUE                                                          02940060
      RVCOMP = 44.5                                                     02950060
      GO TO 40070                                                       02960060
30070 IVDELE = IVDELE + 1                                               02970060
      WRITE (I02,80003) IVTNUM                                          02980060
      IF (ICZERO) 40070,   81, 40070                                    02990060
40070 IF (RVCOMP - 44.495) 20070,10070,40071                            03000060
40071 IF (RVCOMP - 45.505) 10070,10070,20070                            03010060
10070 IVPASS = IVPASS + 1                                               03020060
      WRITE (I02,80001) IVTNUM                                          03030060
      GO TO   81                                                        03040060
20070 IVFAIL = IVFAIL + 1                                               03050060
      RVCORR = 44.5                                                     03060060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03070060
   81 CONTINUE                                                          03080060
      IVTNUM =   8                                                      03090060
C                                                                       03100060
C      ****  TEST   8  ****                                             03110060
C                                                                       03120060
      IF (ICZERO) 30080,   80, 30080                                    03130060
   80 CONTINUE                                                          03140060
      RVCOMP = -2.0                                                     03150060
      GO TO 40080                                                       03160060
30080 IVDELE = IVDELE + 1                                               03170060
      WRITE (I02,80003) IVTNUM                                          03180060
      IF (ICZERO) 40080,   91, 40080                                    03190060
40080 IF (RVCOMP + 2.0005) 20080,10080,40081                            03200060
40081 IF (RVCOMP + 1.9995) 10080,10080,20080                            03210060
10080 IVPASS = IVPASS + 1                                               03220060
      WRITE (I02,80001) IVTNUM                                          03230060
      GO TO   91                                                        03240060
20080 IVFAIL = IVFAIL + 1                                               03250060
      RVCORR = -2.0                                                     03260060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03270060
   91 CONTINUE                                                          03280060
      IVTNUM =   9                                                      03290060
C                                                                       03300060
C      ****  TEST   9  ****                                             03310060
C                                                                       03320060
      IF (ICZERO) 30090,   90, 30090                                    03330060
   90 CONTINUE                                                          03340060
      RVCOMP = 65001.                                                   03350060
      GO TO 40090                                                       03360060
30090 IVDELE = IVDELE + 1                                               03370060
      WRITE (I02,80003) IVTNUM                                          03380060
      IF (ICZERO) 40090,  101, 40090                                    03390060
40090 IF (RVCOMP - 64996.) 20090,10090,40091                            03400060
40091 IF (RVCOMP - 65006.) 10090,10090,20090                            03410060
10090 IVPASS = IVPASS + 1                                               03420060
      WRITE (I02,80001) IVTNUM                                          03430060
      GO TO  101                                                        03440060
20090 IVFAIL = IVFAIL + 1                                               03450060
      RVCORR = 65001.                                                   03460060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03470060
  101 CONTINUE                                                          03480060
      IVTNUM =  10                                                      03490060
C                                                                       03500060
C      ****  TEST  10  ****                                             03510060
C                                                                       03520060
      IF (ICZERO) 30100,  100, 30100                                    03530060
  100 CONTINUE                                                          03540060
      RVCOMP = .65001                                                   03550060
      GO TO 40100                                                       03560060
30100 IVDELE = IVDELE + 1                                               03570060
      WRITE (I02,80003) IVTNUM                                          03580060
      IF (ICZERO) 40100,  111, 40100                                    03590060
40100 IF (RVCOMP - .64996) 20100,10100,40101                            03600060
40101 IF (RVCOMP - .65006) 10100,10100,20100                            03610060
10100 IVPASS = IVPASS + 1                                               03620060
      WRITE (I02,80001) IVTNUM                                          03630060
      GO TO  111                                                        03640060
20100 IVFAIL = IVFAIL + 1                                               03650060
      RVCORR = .65001                                                   03660060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03670060
  111 CONTINUE                                                          03680060
      IVTNUM =  11                                                      03690060
C                                                                       03700060
C      ****  TEST  11  ****                                             03710060
C                                                                       03720060
      IF (ICZERO) 30110,  110, 30110                                    03730060
  110 CONTINUE                                                          03740060
      RVCOMP = -.33333                                                  03750060
      GO TO 40110                                                       03760060
30110 IVDELE = IVDELE + 1                                               03770060
      WRITE (I02,80003) IVTNUM                                          03780060
      IF (ICZERO) 40110,  121, 40110                                    03790060
40110 IF (RVCOMP + .33338) 20110,10110,40111                            03800060
40111 IF (RVCOMP + .33328) 10110,10110,20110                            03810060
10110 IVPASS = IVPASS + 1                                               03820060
      WRITE (I02,80001) IVTNUM                                          03830060
      GO TO  121                                                        03840060
20110 IVFAIL = IVFAIL + 1                                               03850060
      RVCORR = -.33333                                                  03860060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03870060
C                                                                       03880060
C     TEST 012 THROUGH TEST 19 - REAL CONSTANT IS BASIC REAL CONSTANT   03890060
C                              - FOLLOWED BY DECIMAL EXPONENT           03900060
C                                                                       03910060
  121 CONTINUE                                                          03920060
      IVTNUM =  12                                                      03930060
C                                                                       03940060
C      ****  TEST  12  ****                                             03950060
C                                                                       03960060
      IF (ICZERO) 30120,  120, 30120                                    03970060
  120 CONTINUE                                                          03980060
      RVCOMP = .2E+1                                                    03990060
      GO TO 40120                                                       04000060
30120 IVDELE = IVDELE + 1                                               04010060
      WRITE (I02,80003) IVTNUM                                          04020060
      IF (ICZERO) 40120,  131, 40120                                    04030060
40120 IF (RVCOMP - 1.9995) 20120,10120,40121                            04040060
40121 IF (RVCOMP - 2.0005) 10120,10120,20120                            04050060
10120 IVPASS = IVPASS + 1                                               04060060
      WRITE (I02,80001) IVTNUM                                          04070060
      GO TO  131                                                        04080060
20120 IVFAIL = IVFAIL + 1                                               04090060
      RVCORR = 2.0                                                      04100060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04110060
  131 CONTINUE                                                          04120060
      IVTNUM =  13                                                      04130060
C                                                                       04140060
C      ****  TEST  13  ****                                             04150060
C                                                                       04160060
      IF (ICZERO) 30130,  130, 30130                                    04170060
  130 CONTINUE                                                          04180060
      RVCOMP = 2.0E+0                                                   04190060
      GO TO 40130                                                       04200060
30130 IVDELE = IVDELE + 1                                               04210060
      WRITE (I02,80003) IVTNUM                                          04220060
      IF (ICZERO) 40130,  141, 40130                                    04230060
40130 IF (RVCOMP - 1.9995) 20130,10130,40131                            04240060
40131 IF (RVCOMP - 2.0005) 10130,10130,20130                            04250060
10130 IVPASS = IVPASS + 1                                               04260060
      WRITE (I02,80001) IVTNUM                                          04270060
      GO TO  141                                                        04280060
20130 IVFAIL = IVFAIL + 1                                               04290060
      RVCORR = 2.0                                                      04300060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04310060
  141 CONTINUE                                                          04320060
      IVTNUM =  14                                                      04330060
C                                                                       04340060
C      ****  TEST  14  ****                                             04350060
C                                                                       04360060
      IF (ICZERO) 30140,  140, 30140                                    04370060
  140 CONTINUE                                                          04380060
      RVCOMP = 445.0E-01                                                04390060
      GO TO 40140                                                       04400060
30140 IVDELE = IVDELE + 1                                               04410060
      WRITE (I02,80003) IVTNUM                                          04420060
      IF (ICZERO) 40140,  151, 40140                                    04430060
40140 IF (RVCOMP - 44.495) 20140,10140,40141                            04440060
40141 IF (RVCOMP - 44.505) 10140,10140,20140                            04450060
10140 IVPASS = IVPASS + 1                                               04460060
      WRITE (I02,80001) IVTNUM                                          04470060
      GO TO  151                                                        04480060
20140 IVFAIL = IVFAIL + 1                                               04490060
      RVCORR = 44.5                                                     04500060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04510060
  151 CONTINUE                                                          04520060
      IVTNUM =  15                                                      04530060
C                                                                       04540060
C      ****  TEST  15  ****                                             04550060
C                                                                       04560060
      IF (ICZERO) 30150,  150, 30150                                    04570060
  150 CONTINUE                                                          04580060
      RVCOMP = 4.450E1                                                  04590060
      GO TO 40150                                                       04600060
30150 IVDELE = IVDELE + 1                                               04610060
      WRITE (I02,80003) IVTNUM                                          04620060
      IF (ICZERO) 40150,  161, 40150                                    04630060
40150 IF (RVCOMP - 44.495) 20150,10150,40151                            04640060
40151 IF (RVCOMP - 44.505) 10150,10150,20150                            04650060
10150 IVPASS = IVPASS + 1                                               04660060
      WRITE (I02,80001) IVTNUM                                          04670060
      GO TO  161                                                        04680060
20150 IVFAIL = IVFAIL + 1                                               04690060
      RVCORR = 44.5                                                     04700060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04710060
  161 CONTINUE                                                          04720060
      IVTNUM =  16                                                      04730060
C                                                                       04740060
C      ****  TEST  16  ****                                             04750060
C                                                                       04760060
      IF (ICZERO) 30160,  160, 30160                                    04770060
  160 CONTINUE                                                          04780060
      RVCOMP = 2.E+15                                                   04790060
      GO TO 40160                                                       04800060
30160 IVDELE = IVDELE + 1                                               04810060
      WRITE (I02,80003) IVTNUM                                          04820060
      IF (ICZERO) 40160,  171, 40160                                    04830060
40160 IF (RVCOMP - 1.9995E+15) 20160,10160,40161                        04840060
40161 IF (RVCOMP - 2.0005E+15) 10160,10160,20160                        04850060
10160 IVPASS = IVPASS + 1                                               04860060
      WRITE (I02,80001) IVTNUM                                          04870060
      GO TO  171                                                        04880060
20160 IVFAIL = IVFAIL + 1                                               04890060
      RVCORR = 2.0E+15                                                  04900060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04910060
  171 CONTINUE                                                          04920060
      IVTNUM =  17                                                      04930060
C                                                                       04940060
C      ****  TEST  17  ****                                             04950060
C                                                                       04960060
      IF (ICZERO) 30170,  170, 30170                                    04970060
  170 CONTINUE                                                          04980060
      RVCOMP = 44.5E-15                                                 04990060
      GO TO 40170                                                       05000060
30170 IVDELE = IVDELE + 1                                               05010060
      WRITE (I02,80003) IVTNUM                                          05020060
      IF (ICZERO) 40170,  181, 40170                                    05030060
40170 IF (RVCOMP - 44.495E-15) 20170,10170,40171                        05040060
40171 IF (RVCOMP - 44.505E-15) 10170,10170,20170                        05050060
10170 IVPASS = IVPASS + 1                                               05060060
      WRITE (I02,80001) IVTNUM                                          05070060
      GO TO  181                                                        05080060
20170 IVFAIL = IVFAIL + 1                                               05090060
      RVCORR = 44.5E-15                                                 05100060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05110060
  181 CONTINUE                                                          05120060
      IVTNUM =  18                                                      05130060
C                                                                       05140060
C      ****  TEST  18  ****                                             05150060
C                                                                       05160060
      IF (ICZERO) 30180,  180, 30180                                    05170060
  180 CONTINUE                                                          05180060
      RVCOMP = -4.45E0                                                  05190060
      GO TO 40180                                                       05200060
30180 IVDELE = IVDELE + 1                                               05210060
      WRITE (I02,80003) IVTNUM                                          05220060
      IF (ICZERO) 40180,  191, 40180                                    05230060
40180 IF (RVCOMP + 4.4505) 20180,10180,40181                            05240060
40181 IF (RVCOMP + 4.4495) 10180,10180,20180                            05250060
10180 IVPASS = IVPASS + 1                                               05260060
      WRITE (I02,80001) IVTNUM                                          05270060
      GO TO  191                                                        05280060
20180 IVFAIL = IVFAIL + 1                                               05290060
      RVCORR = -4.45                                                    05300060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05310060
  191 CONTINUE                                                          05320060
      IVTNUM =  19                                                      05330060
C                                                                       05340060
C      ****  TEST  19  ****                                             05350060
C                                                                       05360060
      IF (ICZERO) 30190,  190, 30190                                    05370060
  190 CONTINUE                                                          05380060
      RVCOMP = -6511.8E-0                                               05390060
      GO TO 40190                                                       05400060
30190 IVDELE = IVDELE + 1                                               05410060
      WRITE (I02,80003) IVTNUM                                          05420060
      IF (ICZERO) 40190,  201, 40190                                    05430060
40190 IF (RVCOMP + 6512.3) 20190,10190,40191                            05440060
40191 IF (RVCOMP + 6511.3) 10190,10190,20190                            05450060
10190 IVPASS = IVPASS + 1                                               05460060
      WRITE (I02,80001) IVTNUM                                          05470060
      GO TO  201                                                        05480060
20190 IVFAIL = IVFAIL + 1                                               05490060
      RVCORR = -6511.8                                                  05500060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05510060
C                                                                       05520060
C     TEST 020 THROUGH TEST 025 - INTEGER CONSTANT FOLLOWED             05530060
C                               - BY A DECIMAL EXPONENT                 05540060
C                                                                       05550060
  201 CONTINUE                                                          05560060
      IVTNUM =  20                                                      05570060
C                                                                       05580060
C      ****  TEST  20  ****                                             05590060
C                                                                       05600060
      IF (ICZERO) 30200,  200, 30200                                    05610060
  200 CONTINUE                                                          05620060
      RVCOMP = 2E+1                                                     05630060
      GO TO 40200                                                       05640060
30200 IVDELE = IVDELE + 1                                               05650060
      WRITE (I02,80003) IVTNUM                                          05660060
      IF (ICZERO) 40200,  211, 40200                                    05670060
40200 IF (RVCOMP - 19.995) 20200,10200,40201                            05680060
40201 IF (RVCOMP - 20.005) 10200,10200,20200                            05690060
10200 IVPASS = IVPASS + 1                                               05700060
      WRITE (I02,80001) IVTNUM                                          05710060
      GO TO  211                                                        05720060
20200 IVFAIL = IVFAIL + 1                                               05730060
      RVCORR = 20.0                                                     05740060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05750060
  211 CONTINUE                                                          05760060
      IVTNUM =  21                                                      05770060
C                                                                       05780060
C      ****  TEST  21  ****                                             05790060
C                                                                       05800060
      IF (ICZERO) 30210,  210, 30210                                    05810060
  210 CONTINUE                                                          05820060
      RVCOMP = 445E-02                                                  05830060
      GO TO 40210                                                       05840060
30210 IVDELE = IVDELE + 1                                               05850060
      WRITE (I02,80003) IVTNUM                                          05860060
      IF (ICZERO) 40210,  221, 40210                                    05870060
40210 IF (RVCOMP - 4.4495) 20210,10210,40211                            05880060
40211 IF (RVCOMP - 4.4505) 10210,10210,20210                            05890060
10210 IVPASS = IVPASS + 1                                               05900060
      WRITE (I02,80001) IVTNUM                                          05910060
      GO TO  221                                                        05920060
20210 IVFAIL = IVFAIL + 1                                               05930060
      RVCORR = 4.45                                                     05940060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05950060
  221 CONTINUE                                                          05960060
      IVTNUM =  22                                                      05970060
C                                                                       05980060
C      ****  TEST  22  ****                                             05990060
C                                                                       06000060
      IF (ICZERO) 30220,  220, 30220                                    06010060
  220 CONTINUE                                                          06020060
      RVCOMP = 7E3                                                      06030060
      GO TO 40220                                                       06040060
30220 IVDELE = IVDELE + 1                                               06050060
      WRITE (I02,80003) IVTNUM                                          06060060
      IF (ICZERO) 40220,  231, 40220                                    06070060
40220 IF (RVCOMP - 6999.0) 20220,10220,40221                            06080060
40221 IF (RVCOMP - 7001.0) 10220,10220,20220                            06090060
10220 IVPASS = IVPASS + 1                                               06100060
      WRITE (I02,80001) IVTNUM                                          06110060
      GO TO  231                                                        06120060
20220 IVFAIL = IVFAIL + 1                                               06130060
      RVCORR = 7000.0                                                   06140060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06150060
  231 CONTINUE                                                          06160060
      IVTNUM =  23                                                      06170060
C                                                                       06180060
C      ****  TEST  23  ****                                             06190060
C                                                                       06200060
      IF (ICZERO) 30230,  230, 30230                                    06210060
  230 CONTINUE                                                          06220060
      RVCOMP = 214 E 0                                                  06230060
      GO TO 40230                                                       06240060
30230 IVDELE = IVDELE + 1                                               06250060
      WRITE (I02,80003) IVTNUM                                          06260060
      IF (ICZERO) 40230,  241, 40230                                    06270060
40230 IF (RVCOMP - 213.95) 20230,10230,40231                            06280060
40231 IF (RVCOMP - 214.05) 10230,10230,20230                            06290060
10230 IVPASS = IVPASS + 1                                               06300060
      WRITE (I02,80001) IVTNUM                                          06310060
      GO TO  241                                                        06320060
20230 IVFAIL = IVFAIL + 1                                               06330060
      RVCORR = 214.0                                                    06340060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06350060
  241 CONTINUE                                                          06360060
      IVTNUM =  24                                                      06370060
C                                                                       06380060
C      ****  TEST  24  ****                                             06390060
C                                                                       06400060
      IF (ICZERO) 30240,  240, 30240                                    06410060
  240 CONTINUE                                                          06420060
      RVCOMP = -3276E+6                                                 06430060
      GO TO 40240                                                       06440060
30240 IVDELE = IVDELE + 1                                               06450060
      WRITE (I02,80003) IVTNUM                                          06460060
      IF (ICZERO) 40240,  251, 40240                                    06470060
40240 IF (RVCOMP + .32765E+10) 20240,10240,40241                        06480060
40241 IF (RVCOMP + .32755E+10) 10240,10240,20240                        06490060
10240 IVPASS = IVPASS + 1                                               06500060
      WRITE (I02,80001) IVTNUM                                          06510060
      GO TO  251                                                        06520060
20240 IVFAIL = IVFAIL + 1                                               06530060
      RVCORR = -3276E+6                                                 06540060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06550060
  251 CONTINUE                                                          06560060
      IVTNUM =  25                                                      06570060
C                                                                       06580060
C      ****  TEST  25  ****                                             06590060
C                                                                       06600060
      IF (ICZERO) 30250,  250, 30250                                    06610060
  250 CONTINUE                                                          06620060
      RVCOMP = -7E3                                                     06630060
      GO TO 40250                                                       06640060
30250 IVDELE = IVDELE + 1                                               06650060
      WRITE (I02,80003) IVTNUM                                          06660060
      IF (ICZERO) 40250,  261, 40250                                    06670060
40250 IF (RVCOMP + 7001.)  20250,10250,40251                            06680060
40251 IF (RVCOMP + 6999.) 10250,10250,20250                             06690060
10250 IVPASS = IVPASS + 1                                               06700060
      WRITE (I02,80001) IVTNUM                                          06710060
      GO TO  261                                                        06720060
20250 IVFAIL = IVFAIL + 1                                               06730060
      RVCORR = -7000.0                                                  06740060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06750060
C                                                                       06760060
C     TEST 026 THROUGH TEST 028 CONTAIN ARITHMETIC ASSIGNMENT STATEMENT 06770060
C     OF THE FORM            REAL VARIABLE = REAL VARIABLE              06780060
C                                                                       06790060
  261 CONTINUE                                                          06800060
      IVTNUM =  26                                                      06810060
C                                                                       06820060
C      ****  TEST  26  ****                                             06830060
C                                                                       06840060
      IF (ICZERO) 30260,  260, 30260                                    06850060
  260 CONTINUE                                                          06860060
      RVON01 = .2E+1                                                    06870060
      RVCOMP = RVON01                                                   06880060
      GO TO 40260                                                       06890060
30260 IVDELE = IVDELE + 1                                               06900060
      WRITE (I02,80003) IVTNUM                                          06910060
      IF (ICZERO) 40260,  271, 40260                                    06920060
40260 IF (RVCOMP - 1.9995) 20260,10260,40261                            06930060
40261 IF (RVCOMP - 2.0005) 10260,10260,20260                            06940060
10260 IVPASS = IVPASS + 1                                               06950060
      WRITE (I02,80001) IVTNUM                                          06960060
      GO TO  271                                                        06970060
20260 IVFAIL = IVFAIL + 1                                               06980060
      RVCORR = 20.0                                                     06990060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07000060
  271 CONTINUE                                                          07010060
      IVTNUM =  27                                                      07020060
C                                                                       07030060
C      ****  TEST  27  ****                                             07040060
C                                                                       07050060
      IF (ICZERO) 30270,  270, 30270                                    07060060
  270 CONTINUE                                                          07070060
      RVON01 = -445.E-01                                                07080060
      RVCOMP = RVON01                                                   07090060
      GO TO 40270                                                       07100060
30270 IVDELE = IVDELE + 1                                               07110060
      WRITE (I02,80003) IVTNUM                                          07120060
      IF (ICZERO) 40270,  281, 40270                                    07130060
40270 IF (RVCOMP + 44.505) 20270,10270,40271                            07140060
40271 IF (RVCOMP + 44.495) 10270,10270,20270                            07150060
10270 IVPASS = IVPASS + 1                                               07160060
      WRITE (I02,80001) IVTNUM                                          07170060
      GO TO  281                                                        07180060
20270 IVFAIL = IVFAIL + 1                                               07190060
      RVCORR = -44.5                                                    07200060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07210060
  281 CONTINUE                                                          07220060
      IVTNUM =  28                                                      07230060
C                                                                       07240060
C      ****  TEST  28  ****                                             07250060
C                                                                       07260060
      IF (ICZERO) 30280,  280, 30280                                    07270060
  280 CONTINUE                                                          07280060
      RVON01 = 7E3                                                      07290060
      RVCOMP = RVON01                                                   07300060
      GO TO 40280                                                       07310060
30280 IVDELE = IVDELE + 1                                               07320060
      WRITE (I02,80003) IVTNUM                                          07330060
      IF (ICZERO) 40280,  291, 40280                                    07340060
40280 IF (RVCOMP - 6999.0) 20280,10280,40281                            07350060
40281 IF (RVCOMP-7001.0) 10280,10280,20280                              07360060
10280 IVPASS = IVPASS + 1                                               07370060
      WRITE (I02,80001) IVTNUM                                          07380060
      GO TO  291                                                        07390060
20280 IVFAIL = IVFAIL + 1                                               07400060
      RVCORR = 7000.0                                                   07410060
C                                                                       07420060
C     TEST 029 THROUGH TEST 031 CONTAIN ARITHMETIC ASSIGNMENT STATEMENT 07430060
C     OF THE FORM            REAL VARIABLE = - REAL VARIABLE            07440060
C                                                                       07450060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07460060
  291 CONTINUE                                                          07470060
      IVTNUM =  29                                                      07480060
C                                                                       07490060
C      ****  TEST  29  ****                                             07500060
C                                                                       07510060
      IF (ICZERO) 30290,  290, 30290                                    07520060
  290 CONTINUE                                                          07530060
      RVON01 = .2E+1                                                    07540060
      RVCOMP = -RVON01                                                  07550060
      GO TO 40290                                                       07560060
30290 IVDELE = IVDELE + 1                                               07570060
      WRITE (I02,80003) IVTNUM                                          07580060
      IF (ICZERO) 40290,  301, 40290                                    07590060
40290 IF (RVCOMP + 2.0005) 20290,10290,40291                            07600060
40291 IF (RVCOMP + 1.9995) 10290,10290,20290                            07610060
10290 IVPASS = IVPASS + 1                                               07620060
      WRITE (I02,80001) IVTNUM                                          07630060
      GO TO  301                                                        07640060
20290 IVFAIL = IVFAIL + 1                                               07650060
      RVCORR = -2.0                                                     07660060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07670060
  301 CONTINUE                                                          07680060
      IVTNUM =  30                                                      07690060
C                                                                       07700060
C      ****  TEST  30  ****                                             07710060
C                                                                       07720060
      IF (ICZERO) 30300,  300, 30300                                    07730060
  300 CONTINUE                                                          07740060
      RVON01 = -445.E-01                                                07750060
      RVCOMP = -RVON01                                                  07760060
      GO TO 40300                                                       07770060
30300 IVDELE = IVDELE + 1                                               07780060
      WRITE (I02,80003) IVTNUM                                          07790060
      IF (ICZERO) 40300,  311, 40300                                    07800060
40300 IF (RVCOMP - 44.495) 20300,10300,40301                            07810060
40301 IF (RVCOMP - 44.505) 10300,10300,20300                            07820060
10300 IVPASS = IVPASS + 1                                               07830060
      WRITE (I02,80001) IVTNUM                                          07840060
      GO TO  311                                                        07850060
20300 IVFAIL = IVFAIL + 1                                               07860060
      RVCORR = 44.5                                                     07870060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07880060
  311 CONTINUE                                                          07890060
      IVTNUM =  31                                                      07900060
C                                                                       07910060
C      ****  TEST  31  ****                                             07920060
C                                                                       07930060
      IF (ICZERO) 30310,  310, 30310                                    07940060
  310 CONTINUE                                                          07950060
      RVON01 = -.44559E1                                                07960060
      RVCOMP = -RVON01                                                  07970060
      GO TO 40310                                                       07980060
30310 IVDELE = IVDELE + 1                                               07990060
      WRITE (I02,80003) IVTNUM                                          08000060
      IF (ICZERO) 40310,  321, 40310                                    08010060
40310 IF (RVCOMP - 4.4554) 20310,10310,40311                            08020060
40311 IF (RVCOMP - 4.4564) 10310,10310,20310                            08030060
10310 IVPASS = IVPASS + 1                                               08040060
      WRITE (I02,80001) IVTNUM                                          08050060
      GO TO  321                                                        08060060
20310 IVFAIL = IVFAIL + 1                                               08070060
      RVCORR = 4.4559                                                   08080060
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          08090060
C      ****    END OF TESTS    ****                                     08100060
  321 CONTINUE                                                          08110060
C                                                                       08120060
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08130060
99999 CONTINUE                                                          08140060
      WRITE (I02,90002)                                                 08150060
      WRITE (I02,90006)                                                 08160060
      WRITE (I02,90002)                                                 08170060
      WRITE (I02,90002)                                                 08180060
      WRITE (I02,90007)                                                 08190060
      WRITE (I02,90002)                                                 08200060
      WRITE (I02,90008)  IVFAIL                                         08210060
      WRITE (I02,90009) IVPASS                                          08220060
      WRITE (I02,90010) IVDELE                                          08230060
C                                                                       08240060
C                                                                       08250060
C     TERMINATE ROUTINE EXECUTION                                       08260060
      STOP                                                              08270060
C                                                                       08280060
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08290060
90000 FORMAT (1H1)                                                      08300060
90002 FORMAT (1H )                                                      08310060
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08320060
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08330060
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08340060
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08350060
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08360060
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08370060
C                                                                       08380060
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08390060
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08400060
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08410060
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08420060
C                                                                       08430060
C     FORMAT STATEMENTS FOR TEST RESULTS                                08440060
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08450060
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08460060
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08470060
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08480060
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08490060
C                                                                       08500060
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM060)                          08510060
      END                                                               08520060
