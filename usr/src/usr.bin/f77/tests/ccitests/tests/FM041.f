C     COMMENT SECTION                                                   00010041
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020041
C     FM041                                                             00030041
C                                                                       00040041
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE              00050041
C     FORM      INTEGER VARIABLE =  PRIMARY ** PRIMARY                  00060041
C     WHERE THE FIRST OF TWO PRIMARIES IS AN INTEGER VARIABLE OR AN     00070041
C     INTEGER CONSTANT AND THE SECOND PRIMARY IS AN INTEGER CONSTANT.   00080041
C                                                                       00090041
C      REFERENCES                                                       00100041
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110041
C              X3.9-1978                                                00120041
C                                                                       00130041
C        SECTION 4.3, INTEGER TYPE                                      00140041
C        SECTION 4.3.1, INTEGER CONSTANT                                00150041
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00160041
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00170041
C                                                                       00180041
C                                                                       00190041
C      **********************************************************       00200041
C                                                                       00210041
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00220041
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00230041
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00240041
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00250041
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00260041
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00270041
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00280041
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00290041
C     OF EXECUTING THESE TESTS.                                         00300041
C                                                                       00310041
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00320041
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00330041
C                                                                       00340041
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00350041
C                                                                       00360041
C                  DEPARTMENT OF THE NAVY                               00370041
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00380041
C                  WASHINGTON, D.C.  20376                              00390041
C                                                                       00400041
C      **********************************************************       00410041
C                                                                       00420041
C                                                                       00430041
C                                                                       00440041
C     INITIALIZATION SECTION                                            00450041
C                                                                       00460041
C     INITIALIZE CONSTANTS                                              00470041
C      **************                                                   00480041
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00490041
      I01 = 5                                                           00500041
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00510041
      I02 = 6                                                           00520041
C     SYSTEM ENVIRONMENT SECTION                                        00530041
C                                                                       00540041
      I01 = 5                                                           00550041
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00560041
C     (UNIT NUMBER FOR CARD READER).                                    00570041
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00580041
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00590041
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00600041
C                                                                       00610041
      I02 = 6                                                           00620041
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00630041
C     (UNIT NUMBER FOR PRINTER).                                        00640041
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00650041
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00660041
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00670041
C                                                                       00680041
      IVPASS=0                                                          00690041
      IVFAIL=0                                                          00700041
      IVDELE=0                                                          00710041
      ICZERO=0                                                          00720041
C                                                                       00730041
C     WRITE PAGE HEADERS                                                00740041
      WRITE (I02,90000)                                                 00750041
      WRITE (I02,90001)                                                 00760041
      WRITE (I02,90002)                                                 00770041
      WRITE (I02, 90002)                                                00780041
      WRITE (I02,90003)                                                 00790041
      WRITE (I02,90002)                                                 00800041
      WRITE (I02,90004)                                                 00810041
      WRITE (I02,90002)                                                 00820041
      WRITE (I02,90011)                                                 00830041
      WRITE (I02,90002)                                                 00840041
      WRITE (I02,90002)                                                 00850041
      WRITE (I02,90005)                                                 00860041
      WRITE (I02,90006)                                                 00870041
      WRITE (I02,90002)                                                 00880041
C                                                                       00890041
C     TEST SECTION                                                      00900041
C                                                                       00910041
C         ARITHMETIC ASSIGNMENT STATEMENT                               00920041
C                                                                       00930041
C     TEST 615 THROUGH TEST 631 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS00940041
C     OF THE FORM    INTEGER VARIABLE = INTEGER CONSTANT ** INTEGER CON.00950041
C                                                                       00960041
C     TEST 632 THROUGH TEST 648 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS00970041
C     OF THE FORM    INTEGER VARIABLE = INTEGER VARIABLE ** INTEGER CON.00980041
C                                                                       00990041
C                                                                       01000041
      IVTNUM = 615                                                      01010041
C                                                                       01020041
C      ****  TEST 615  ****                                             01030041
C     TEST 615  - SMALL NUMBER BASE; ZERO EXPONENT                      01040041
C                                                                       01050041
      IF (ICZERO) 36150, 6150, 36150                                    01060041
 6150 CONTINUE                                                          01070041
      IVCOMP = 1 ** 0                                                   01080041
      GO TO 46150                                                       01090041
36150 IVDELE = IVDELE + 1                                               01100041
      WRITE (I02,80003) IVTNUM                                          01110041
      IF (ICZERO) 46150, 6161, 46150                                    01120041
46150 IF (IVCOMP - 1) 26150,16150,26150                                 01130041
16150 IVPASS = IVPASS + 1                                               01140041
      WRITE (I02,80001) IVTNUM                                          01150041
      GO TO 6161                                                        01160041
26150 IVFAIL = IVFAIL + 1                                               01170041
      IVCORR = 1                                                        01180041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01190041
 6161 CONTINUE                                                          01200041
      IVTNUM = 616                                                      01210041
C                                                                       01220041
C      ****  TEST 616  ****                                             01230041
C     TEST 616  - ZERO BASE TO FIRST POWER                              01240041
C                                                                       01250041
      IF (ICZERO) 36160, 6160, 36160                                    01260041
 6160 CONTINUE                                                          01270041
      IVCOMP = 0 ** 1                                                   01280041
      GO TO 46160                                                       01290041
36160 IVDELE = IVDELE + 1                                               01300041
      WRITE (I02,80003) IVTNUM                                          01310041
      IF (ICZERO) 46160, 6171, 46160                                    01320041
46160 IF (IVCOMP) 26160,16160,26160                                     01330041
16160 IVPASS = IVPASS + 1                                               01340041
      WRITE (I02,80001) IVTNUM                                          01350041
      GO TO 6171                                                        01360041
26160 IVFAIL = IVFAIL + 1                                               01370041
      IVCORR = 0                                                        01380041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01390041
 6171 CONTINUE                                                          01400041
      IVTNUM = 617                                                      01410041
C                                                                       01420041
C      ****  TEST 617  ****                                             01430041
C     TEST 617  - BASE =1; EXPONENT = 1                                 01440041
C                                                                       01450041
      IF (ICZERO) 36170, 6170, 36170                                    01460041
 6170 CONTINUE                                                          01470041
      IVCOMP = 1 ** 1                                                   01480041
      GO TO 46170                                                       01490041
36170 IVDELE = IVDELE + 1                                               01500041
      WRITE (I02,80003) IVTNUM                                          01510041
      IF (ICZERO) 46170, 6181, 46170                                    01520041
46170 IF (IVCOMP - 1) 26170,16170,26170                                 01530041
16170 IVPASS = IVPASS + 1                                               01540041
      WRITE (I02,80001) IVTNUM                                          01550041
      GO TO 6181                                                        01560041
26170 IVFAIL = IVFAIL + 1                                               01570041
      IVCORR = 1                                                        01580041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01590041
 6181 CONTINUE                                                          01600041
      IVTNUM = 618                                                      01610041
C                                                                       01620041
C      ****  TEST 618  ****                                             01630041
C     TEST 618  - LARGE NUMBER BASE; EXPONENT = 1                       01640041
C                                                                       01650041
      IF (ICZERO) 36180, 6180, 36180                                    01660041
 6180 CONTINUE                                                          01670041
      IVCOMP = 32767 ** 1                                               01680041
      GO TO 46180                                                       01690041
36180 IVDELE = IVDELE + 1                                               01700041
      WRITE (I02,80003) IVTNUM                                          01710041
      IF (ICZERO) 46180, 6191, 46180                                    01720041
46180 IF (IVCOMP - 32767) 26180,16180,26180                             01730041
16180 IVPASS = IVPASS + 1                                               01740041
      WRITE (I02,80001) IVTNUM                                          01750041
      GO TO 6191                                                        01760041
26180 IVFAIL = IVFAIL + 1                                               01770041
      IVCORR = 32767                                                    01780041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01790041
 6191 CONTINUE                                                          01800041
      IVTNUM = 619                                                      01810041
C                                                                       01820041
C      ****  TEST 619  ****                                             01830041
C     TEST 619  - LARGE EXPONENT                                        01840041
C                                                                       01850041
      IF (ICZERO) 36190, 6190, 36190                                    01860041
 6190 CONTINUE                                                          01870041
      IVCOMP = 1 ** 32767                                               01880041
      GO TO 46190                                                       01890041
36190 IVDELE = IVDELE + 1                                               01900041
      WRITE (I02,80003) IVTNUM                                          01910041
      IF (ICZERO) 46190, 6201, 46190                                    01920041
46190 IF (IVCOMP - 1) 26190,16190,26190                                 01930041
16190 IVPASS = IVPASS + 1                                               01940041
      WRITE (I02,80001) IVTNUM                                          01950041
      GO TO 6201                                                        01960041
26190 IVFAIL = IVFAIL + 1                                               01970041
      IVCORR = 1                                                        01980041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01990041
 6201 CONTINUE                                                          02000041
      IVTNUM = 620                                                      02010041
C                                                                       02020041
C      ****  TEST 620  ****                                             02030041
C     TEST 620  - ZERO BASE; LARGE NUMBER EXPONENT                      02040041
C                                                                       02050041
      IF (ICZERO) 36200, 6200, 36200                                    02060041
 6200 CONTINUE                                                          02070041
      IVCOMP = 0 ** 32767                                               02080041
      GO TO 46200                                                       02090041
36200 IVDELE = IVDELE + 1                                               02100041
      WRITE (I02,80003) IVTNUM                                          02110041
      IF (ICZERO) 46200, 6211, 46200                                    02120041
46200 IF (IVCOMP) 26200,16200,26200                                     02130041
16200 IVPASS = IVPASS + 1                                               02140041
      WRITE (I02,80001) IVTNUM                                          02150041
      GO TO 6211                                                        02160041
26200 IVFAIL = IVFAIL + 1                                               02170041
      IVCORR = 0                                                        02180041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02190041
 6211 CONTINUE                                                          02200041
      IVTNUM = 621                                                      02210041
C                                                                       02220041
C      ****  TEST 621  ****                                             02230041
C     TEST 621  -LARGE NUMBER BASE; ZERO EXPONENT                       02240041
C                                                                       02250041
      IF (ICZERO) 36210, 6210, 36210                                    02260041
 6210 CONTINUE                                                          02270041
      IVCOMP = 32767 ** 0                                               02280041
      GO TO 46210                                                       02290041
36210 IVDELE = IVDELE + 1                                               02300041
      WRITE (I02,80003) IVTNUM                                          02310041
      IF (ICZERO) 46210, 6221, 46210                                    02320041
46210 IF (IVCOMP - 1) 26210,16210,26210                                 02330041
16210 IVPASS = IVPASS + 1                                               02340041
      WRITE (I02,80001) IVTNUM                                          02350041
      GO TO 6221                                                        02360041
26210 IVFAIL = IVFAIL + 1                                               02370041
      IVCORR = 1                                                        02380041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02390041
 6221 CONTINUE                                                          02400041
      IVTNUM = 622                                                      02410041
C                                                                       02420041
C      ****  TEST 622  ****                                             02430041
C     TEST 622  -EXPONENT IS POWER OF TWO                               02440041
C                                                                       02450041
      IF (ICZERO) 36220, 6220, 36220                                    02460041
 6220 CONTINUE                                                          02470041
      IVCOMP = 181 ** 2                                                 02480041
      GO TO 46220                                                       02490041
36220 IVDELE = IVDELE + 1                                               02500041
      WRITE (I02,80003) IVTNUM                                          02510041
      IF (ICZERO) 46220, 6231, 46220                                    02520041
46220 IF (IVCOMP - 32761) 26220,16220,26220                             02530041
16220 IVPASS = IVPASS + 1                                               02540041
      WRITE (I02,80001) IVTNUM                                          02550041
      GO TO 6231                                                        02560041
26220 IVFAIL = IVFAIL + 1                                               02570041
      IVCORR = 32761                                                    02580041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02590041
 6231 CONTINUE                                                          02600041
      IVTNUM = 623                                                      02610041
C                                                                       02620041
C      ****  TEST 623  ****                                             02630041
C     TEST 623  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              02640041
C                                                                       02650041
      IF (ICZERO) 36230, 6230, 36230                                    02660041
 6230 CONTINUE                                                          02670041
      IVCOMP = 2 ** 8                                                   02680041
      GO TO 46230                                                       02690041
36230 IVDELE = IVDELE + 1                                               02700041
      WRITE (I02,80003) IVTNUM                                          02710041
      IF (ICZERO) 46230, 6241, 46230                                    02720041
46230 IF (IVCOMP - 256) 26230,16230,26230                               02730041
16230 IVPASS = IVPASS + 1                                               02740041
      WRITE (I02,80001) IVTNUM                                          02750041
      GO TO 6241                                                        02760041
26230 IVFAIL = IVFAIL + 1                                               02770041
      IVCORR = 256                                                      02780041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02790041
 6241 CONTINUE                                                          02800041
C                                                                       02810041
C     TESTS 624 AND 625 TEST TO ENSURE EXPONENTIATION OPERATOR IS       02820041
C                       NOT COMMUTATIVE                                 02830041
C                                                                       02840041
      IVTNUM = 624                                                      02850041
C                                                                       02860041
C      ****  TEST 624  ****                                             02870041
C                                                                       02880041
      IF (ICZERO) 36240, 6240, 36240                                    02890041
 6240 CONTINUE                                                          02900041
      IVCOMP = 3 ** 9                                                   02910041
      GO TO 46240                                                       02920041
36240 IVDELE = IVDELE + 1                                               02930041
      WRITE (I02,80003) IVTNUM                                          02940041
      IF (ICZERO) 46240, 6251, 46240                                    02950041
46240 IF (IVCOMP - 19683) 26240,16240,26240                             02960041
16240 IVPASS = IVPASS + 1                                               02970041
      WRITE (I02,80001) IVTNUM                                          02980041
      GO TO 6251                                                        02990041
26240 IVFAIL = IVFAIL + 1                                               03000041
      IVCORR = 19683                                                    03010041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03020041
 6251 CONTINUE                                                          03030041
      IVTNUM = 625                                                      03040041
C                                                                       03050041
C      ****  TEST 625  ****                                             03060041
C                                                                       03070041
      IF (ICZERO) 36250, 6250, 36250                                    03080041
 6250 CONTINUE                                                          03090041
      IVCOMP = 9 ** 3                                                   03100041
      GO TO 46250                                                       03110041
36250 IVDELE = IVDELE + 1                                               03120041
      WRITE (I02,80003) IVTNUM                                          03130041
      IF (ICZERO) 46250, 6261, 46250                                    03140041
46250 IF (IVCOMP - 729) 26250,16250,26250                               03150041
16250 IVPASS = IVPASS + 1                                               03160041
      WRITE (I02,80001) IVTNUM                                          03170041
      GO TO 6261                                                        03180041
26250 IVFAIL = IVFAIL + 1                                               03190041
      IVCORR = 729                                                      03200041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03210041
 6261 CONTINUE                                                          03220041
C                                                                       03230041
C     TESTS 626 THROUGH 631 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE03240041
C                           ODD AND EVEN NUMBER POWERS CHECKING THE SIGN03250041
C                           OF THE RESULTS                              03260041
C                                                                       03270041
      IVTNUM = 626                                                      03280041
C                                                                       03290041
C      ****  TEST 626  ****                                             03300041
C                                                                       03310041
      IF (ICZERO) 36260, 6260, 36260                                    03320041
 6260 CONTINUE                                                          03330041
      IVCOMP = 1 ** 2                                                   03340041
      GO TO 46260                                                       03350041
36260 IVDELE = IVDELE + 1                                               03360041
      WRITE (I02,80003) IVTNUM                                          03370041
      IF (ICZERO) 46260, 6271, 46260                                    03380041
46260 IF (IVCOMP - 1) 26260,16260,26260                                 03390041
16260 IVPASS = IVPASS + 1                                               03400041
      WRITE (I02,80001) IVTNUM                                          03410041
      GO TO 6271                                                        03420041
26260 IVFAIL = IVFAIL + 1                                               03430041
      IVCORR = 1                                                        03440041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03450041
 6271 CONTINUE                                                          03460041
      IVTNUM = 627                                                      03470041
C                                                                       03480041
C      ****  TEST 627  ****                                             03490041
C                                                                       03500041
      IF (ICZERO) 36270, 6270, 36270                                    03510041
 6270 CONTINUE                                                          03520041
      IVCOMP= (-1) ** 2                                                 03530041
      GO TO 46270                                                       03540041
36270 IVDELE = IVDELE + 1                                               03550041
      WRITE (I02,80003) IVTNUM                                          03560041
      IF (ICZERO) 46270, 6281, 46270                                    03570041
46270 IF (IVCOMP - 1) 26270,16270,26270                                 03580041
16270 IVPASS = IVPASS + 1                                               03590041
      WRITE (I02,80001) IVTNUM                                          03600041
      GO TO 6281                                                        03610041
26270 IVFAIL = IVFAIL + 1                                               03620041
      IVCORR = 1                                                        03630041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03640041
 6281 CONTINUE                                                          03650041
      IVTNUM = 628                                                      03660041
C                                                                       03670041
C      ****  TEST 628  ****                                             03680041
C                                                                       03690041
      IF (ICZERO) 36280, 6280, 36280                                    03700041
 6280 CONTINUE                                                          03710041
      IVCOMP = 7 ** 3                                                   03720041
      GO TO 46280                                                       03730041
36280 IVDELE = IVDELE + 1                                               03740041
      WRITE (I02,80003) IVTNUM                                          03750041
      IF (ICZERO) 46280, 6291, 46280                                    03760041
46280 IF (IVCOMP - 343) 26280,16280,26280                               03770041
16280 IVPASS = IVPASS + 1                                               03780041
      WRITE (I02,80001) IVTNUM                                          03790041
      GO TO 6291                                                        03800041
26280 IVFAIL = IVFAIL + 1                                               03810041
      IVCORR = 343                                                      03820041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03830041
 6291 CONTINUE                                                          03840041
      IVTNUM = 629                                                      03850041
C                                                                       03860041
C      ****  TEST 629  ****                                             03870041
C                                                                       03880041
      IF (ICZERO) 36290, 6290, 36290                                    03890041
 6290 CONTINUE                                                          03900041
      IVCOMP = (-7) ** 3                                                03910041
      GO TO 46290                                                       03920041
36290 IVDELE = IVDELE + 1                                               03930041
      WRITE (I02,80003) IVTNUM                                          03940041
      IF (ICZERO) 46290, 6301, 46290                                    03950041
46290 IF (IVCOMP + 343) 26290,16290,26290                               03960041
16290 IVPASS = IVPASS + 1                                               03970041
      WRITE (I02,80001) IVTNUM                                          03980041
      GO TO 6301                                                        03990041
26290 IVFAIL = IVFAIL + 1                                               04000041
      IVCORR = -343                                                     04010041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04020041
 6301 CONTINUE                                                          04030041
      IVTNUM = 630                                                      04040041
C                                                                       04050041
C      ****  TEST 630  ****                                             04060041
C                                                                       04070041
      IF (ICZERO) 36300, 6300, 36300                                    04080041
 6300 CONTINUE                                                          04090041
      IVCOMP = 7 ** 4                                                   04100041
      GO TO 46300                                                       04110041
36300 IVDELE = IVDELE + 1                                               04120041
      WRITE (I02,80003) IVTNUM                                          04130041
      IF (ICZERO) 46300, 6311, 46300                                    04140041
46300 IF (IVCOMP - 2401) 26300,16300,26300                              04150041
16300 IVPASS = IVPASS + 1                                               04160041
      WRITE (I02,80001) IVTNUM                                          04170041
      GO TO 6311                                                        04180041
26300 IVFAIL = IVFAIL + 1                                               04190041
      IVCORR = 2401                                                     04200041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04210041
 6311 CONTINUE                                                          04220041
      IVTNUM = 631                                                      04230041
C                                                                       04240041
C      ****  TEST 631  ****                                             04250041
C                                                                       04260041
      IF (ICZERO) 36310, 6310, 36310                                    04270041
 6310 CONTINUE                                                          04280041
      IVCOMP = (-7) ** 4                                                04290041
      GO TO 46310                                                       04300041
36310 IVDELE = IVDELE + 1                                               04310041
      WRITE (I02,80003) IVTNUM                                          04320041
      IF (ICZERO) 46310, 6321, 46310                                    04330041
46310 IF (IVCOMP - 2401) 26310,16310,26310                              04340041
16310 IVPASS = IVPASS + 1                                               04350041
      WRITE (I02,80001) IVTNUM                                          04360041
      GO TO 6321                                                        04370041
26310 IVFAIL = IVFAIL + 1                                               04380041
      IVCORR = 2401                                                     04390041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04400041
 6321 CONTINUE                                                          04410041
      IVTNUM = 632                                                      04420041
C                                                                       04430041
C      ****  TEST 632  ****                                             04440041
C     TEST 632  - SMALL NUMBER BASE; ZERO EXPONENT                      04450041
C                                                                       04460041
      IF (ICZERO) 36320, 6320, 36320                                    04470041
 6320 CONTINUE                                                          04480041
      IVON01 = 1                                                        04490041
      IVCOMP = IVON01 ** 1                                              04500041
      GO TO 46320                                                       04510041
36320 IVDELE = IVDELE + 1                                               04520041
      WRITE (I02,80003) IVTNUM                                          04530041
      IF (ICZERO) 46320, 6331, 46320                                    04540041
46320 IF (IVCOMP - 1) 26320,16320,26320                                 04550041
16320 IVPASS = IVPASS + 1                                               04560041
      WRITE (I02,80001) IVTNUM                                          04570041
      GO TO 6331                                                        04580041
26320 IVFAIL = IVFAIL + 1                                               04590041
      IVCORR = 1                                                        04600041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04610041
 6331 CONTINUE                                                          04620041
      IVTNUM = 633                                                      04630041
C                                                                       04640041
C      ****  TEST 633  ****                                             04650041
C     TEST 633  - ZERO BASE TO FIRST POWER                              04660041
C                                                                       04670041
      IF (ICZERO) 36330, 6330, 36330                                    04680041
 6330 CONTINUE                                                          04690041
      IVON01 = 0                                                        04700041
      IVCOMP = IVON01 ** 1                                              04710041
      GO TO 46330                                                       04720041
36330 IVDELE = IVDELE + 1                                               04730041
      WRITE (I02,80003) IVTNUM                                          04740041
      IF (ICZERO) 46330, 6341, 46330                                    04750041
46330 IF (IVCOMP) 26330,16330,26330                                     04760041
16330 IVPASS = IVPASS + 1                                               04770041
      WRITE (I02,80001) IVTNUM                                          04780041
      GO TO 6341                                                        04790041
26330 IVFAIL = IVFAIL + 1                                               04800041
      IVCORR = 0                                                        04810041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04820041
 6341 CONTINUE                                                          04830041
      IVTNUM = 634                                                      04840041
C                                                                       04850041
C      ****  TEST 634  ****                                             04860041
C     TEST 634  - BASE =1; EXPONENT = 1                                 04870041
C                                                                       04880041
      IF (ICZERO) 36340, 6340, 36340                                    04890041
 6340 CONTINUE                                                          04900041
      IVON01 = 1                                                        04910041
      IVCOMP = IVON01 ** 1                                              04920041
      GO TO 46340                                                       04930041
36340 IVDELE = IVDELE + 1                                               04940041
      WRITE (I02,80003) IVTNUM                                          04950041
      IF (ICZERO) 46340, 6351, 46340                                    04960041
46340 IF (IVCOMP - 1) 26340,16340,26340                                 04970041
16340 IVPASS = IVPASS + 1                                               04980041
      WRITE (I02,80001) IVTNUM                                          04990041
      GO TO 6351                                                        05000041
26340 IVFAIL = IVFAIL + 1                                               05010041
      IVCORR = 1                                                        05020041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05030041
 6351 CONTINUE                                                          05040041
      IVTNUM = 635                                                      05050041
C                                                                       05060041
C      ****  TEST 635  ****                                             05070041
C     TEST 635  - LARGE EXPONENT                                        05080041
C                                                                       05090041
      IF (ICZERO) 36350, 6350, 36350                                    05100041
 6350 CONTINUE                                                          05110041
      IVON01 = 1                                                        05120041
      IVCOMP = IVON01 ** 32767                                          05130041
      GO TO 46350                                                       05140041
36350 IVDELE = IVDELE + 1                                               05150041
      WRITE (I02,80003) IVTNUM                                          05160041
      IF (ICZERO) 46350, 6361, 46350                                    05170041
46350 IF (IVCOMP - 1) 26350,16350,26350                                 05180041
16350 IVPASS = IVPASS + 1                                               05190041
      WRITE (I02,80001) IVTNUM                                          05200041
      GO TO 6361                                                        05210041
26350 IVFAIL = IVFAIL + 1                                               05220041
      IVCORR = 1                                                        05230041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05240041
 6361 CONTINUE                                                          05250041
      IVTNUM = 636                                                      05260041
C                                                                       05270041
C      ****  TEST 636  ****                                             05280041
C     TEST 636  - LARGE NUMBER BASE; EXPONENT = 1                       05290041
C                                                                       05300041
      IF (ICZERO) 36360, 6360, 36360                                    05310041
 6360 CONTINUE                                                          05320041
      IVON01 = 32767                                                    05330041
      IVCOMP = IVON01 ** 1                                              05340041
      GO TO 46360                                                       05350041
36360 IVDELE = IVDELE + 1                                               05360041
      WRITE (I02,80003) IVTNUM                                          05370041
      IF (ICZERO) 46360, 6371, 46360                                    05380041
46360 IF (IVCOMP - 32767) 26360,16360,26360                             05390041
16360 IVPASS = IVPASS + 1                                               05400041
      WRITE (I02,80001) IVTNUM                                          05410041
      GO TO 6371                                                        05420041
26360 IVFAIL = IVFAIL + 1                                               05430041
      IVCORR = 32767                                                    05440041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05450041
 6371 CONTINUE                                                          05460041
      IVTNUM = 637                                                      05470041
C                                                                       05480041
C      ****  TEST 637  ****                                             05490041
C     TEST 637  - ZERO BASE; LARGE NUMBER EXPONENT                      05500041
C                                                                       05510041
      IF (ICZERO) 36370, 6370, 36370                                    05520041
 6370 CONTINUE                                                          05530041
      IVON01 = 0                                                        05540041
      IVCOMP = IVON01 ** 32767                                          05550041
      GO TO 46370                                                       05560041
36370 IVDELE = IVDELE + 1                                               05570041
      WRITE (I02,80003) IVTNUM                                          05580041
      IF (ICZERO) 46370, 6381, 46370                                    05590041
46370 IF (IVCOMP) 26370,16370,26370                                     05600041
16370 IVPASS = IVPASS + 1                                               05610041
      WRITE (I02,80001) IVTNUM                                          05620041
      GO TO 6381                                                        05630041
26370 IVFAIL = IVFAIL +1                                                05640041
      IVCORR = 0                                                        05650041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05660041
 6381 CONTINUE                                                          05670041
      IVTNUM = 638                                                      05680041
C                                                                       05690041
C      ****  TEST 638  ****                                             05700041
C     TEST 638  -LARGE NUMBER BASE; ZERO EXPONENT                       05710041
C                                                                       05720041
      IF (ICZERO) 36380, 6380, 36380                                    05730041
 6380 CONTINUE                                                          05740041
      IVON01 = 32767                                                    05750041
      IVCOMP = IVON01 ** 0                                              05760041
      GO TO 46380                                                       05770041
36380 IVDELE = IVDELE + 1                                               05780041
      WRITE (I02,80003) IVTNUM                                          05790041
      IF (ICZERO) 46380, 6391, 46380                                    05800041
46380 IF (IVCOMP - 1) 26380,16380,26380                                 05810041
16380 IVPASS = IVPASS + 1                                               05820041
      WRITE (I02,80001) IVTNUM                                          05830041
      GO TO 6391                                                        05840041
26380 IVFAIL = IVFAIL + 1                                               05850041
      IVCORR = 1                                                        05860041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05870041
 6391 CONTINUE                                                          05880041
      IVTNUM = 639                                                      05890041
C                                                                       05900041
C      ****  TEST 639  ****                                             05910041
C     TEST 639  -EXPONENT IS POWER OF TWO                               05920041
C                                                                       05930041
      IF (ICZERO) 36390, 6390, 36390                                    05940041
 6390 CONTINUE                                                          05950041
      IVON01 = 181                                                      05960041
      IVCOMP = IVON01 ** 2                                              05970041
      GO TO 46390                                                       05980041
36390 IVDELE = IVDELE + 1                                               05990041
      WRITE (I02,80003) IVTNUM                                          06000041
      IF (ICZERO) 46390, 6401, 46390                                    06010041
46390 IF (IVCOMP - 32761) 26390,16390,26390                             06020041
16390 IVPASS = IVPASS + 1                                               06030041
      WRITE (I02,80001) IVTNUM                                          06040041
      GO TO 6401                                                        06050041
26390 IVFAIL = IVFAIL + 1                                               06060041
      IVCORR = 32761                                                    06070041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06080041
 6401 CONTINUE                                                          06090041
      IVTNUM = 640                                                      06100041
C                                                                       06110041
C      ****  TEST 640  ****                                             06120041
C     TEST 640  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              06130041
C                                                                       06140041
      IF (ICZERO) 36400, 6400, 36400                                    06150041
 6400 CONTINUE                                                          06160041
      IVON01 = 2                                                        06170041
      IVCOMP = IVON01 ** 8                                              06180041
      GO TO 46400                                                       06190041
36400 IVDELE = IVDELE + 1                                               06200041
      WRITE (I02,80003) IVTNUM                                          06210041
      IF (ICZERO) 46400, 6411, 46400                                    06220041
46400 IF (IVCOMP - 256) 26400,16400,26400                               06230041
16400 IVPASS = IVPASS + 1                                               06240041
      WRITE (I02,80001) IVTNUM                                          06250041
      GO TO 6411                                                        06260041
26400 IVFAIL = IVFAIL + 1                                               06270041
      IVCORR = 256                                                      06280041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06290041
 6411 CONTINUE                                                          06300041
C                                                                       06310041
C     TESTS 641 AND 642 TEST TO ENSURE EXPONENTIATION OPERATOR IS       06320041
C                       NOT COMMUTATIVE                                 06330041
C                                                                       06340041
      IVTNUM = 641                                                      06350041
C                                                                       06360041
C      ****  TEST 641  ****                                             06370041
C                                                                       06380041
      IF (ICZERO) 36410, 6410, 36410                                    06390041
 6410 CONTINUE                                                          06400041
      IVON01 = 3                                                        06410041
      IVCOMP = IVON01 ** 9                                              06420041
      GO TO 46410                                                       06430041
36410 IVDELE = IVDELE + 1                                               06440041
      WRITE (I02,80003) IVTNUM                                          06450041
      IF (ICZERO) 46410, 6421, 46410                                    06460041
46410 IF (IVCOMP - 19683) 26410,16410,26410                             06470041
16410 IVPASS = IVPASS + 1                                               06480041
      WRITE (I02,80001) IVTNUM                                          06490041
      GO TO 6421                                                        06500041
26410 IVFAIL = IVFAIL + 1                                               06510041
      IVCORR = 19683                                                    06520041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06530041
 6421 CONTINUE                                                          06540041
      IVTNUM = 642                                                      06550041
C                                                                       06560041
C      ****  TEST 642  ****                                             06570041
C                                                                       06580041
      IF (ICZERO) 36420, 6420, 36420                                    06590041
 6420 CONTINUE                                                          06600041
      IVON01 = 9                                                        06610041
      IVCOMP = IVON01 ** 3                                              06620041
      GO TO 46420                                                       06630041
36420 IVDELE = IVDELE + 1                                               06640041
      WRITE (I02,80003) IVTNUM                                          06650041
      IF (ICZERO) 46420, 6431, 46420                                    06660041
46420 IF (IVCOMP - 729) 26420,16420,26420                               06670041
16420 IVPASS = IVPASS + 1                                               06680041
      WRITE (I02,80001) IVTNUM                                          06690041
      GO TO 6431                                                        06700041
26420 IVFAIL = IVFAIL + 1                                               06710041
      IVCORR = 729                                                      06720041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06730041
 6431 CONTINUE                                                          06740041
C                                                                       06750041
C     TESTS 643 THROUGH 648 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE06760041
C                           ODD AND EVEN NUMBER POWERS CHECKING THE SIGN06770041
C                           OF THE RESULTS                              06780041
C                                                                       06790041
      IVTNUM = 643                                                      06800041
C                                                                       06810041
C      ****  TEST 643  ****                                             06820041
C                                                                       06830041
      IF (ICZERO) 36430, 6430, 36430                                    06840041
 6430 CONTINUE                                                          06850041
      IVON01 = 1                                                        06860041
      IVCOMP = IVON01 ** 2                                              06870041
      GO TO 46430                                                       06880041
36430 IVDELE = IVDELE + 1                                               06890041
      WRITE (I02,80003) IVTNUM                                          06900041
      IF (ICZERO) 46430, 6441, 46430                                    06910041
46430 IF (IVCOMP - 1) 26430,16430,26430                                 06920041
16430 IVPASS = IVPASS + 1                                               06930041
      WRITE (I02,80001) IVTNUM                                          06940041
      GO TO 6441                                                        06950041
26430 IVFAIL = IVFAIL + 1                                               06960041
      IVCORR = 1                                                        06970041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06980041
 6441 CONTINUE                                                          06990041
      IVTNUM = 644                                                      07000041
C                                                                       07010041
C      ****  TEST 644  ****                                             07020041
C                                                                       07030041
      IF (ICZERO) 36440, 6440, 36440                                    07040041
 6440 CONTINUE                                                          07050041
      IVON01 = -1                                                       07060041
      IVCOMP = IVON01 ** 2                                              07070041
      GO TO 46440                                                       07080041
36440 IVDELE = IVDELE + 1                                               07090041
      WRITE (I02,80003) IVTNUM                                          07100041
      IF (ICZERO) 46440, 6451, 46440                                    07110041
46440 IF (IVCOMP - 1) 26440,16440,26440                                 07120041
16440 IVPASS = IVPASS + 1                                               07130041
      WRITE (I02,80001) IVTNUM                                          07140041
      GO TO 6451                                                        07150041
26440 IVFAIL = IVFAIL + 1                                               07160041
      IVCORR = 1                                                        07170041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07180041
 6451 CONTINUE                                                          07190041
      IVTNUM = 645                                                      07200041
C                                                                       07210041
C      ****  TEST 645  ****                                             07220041
C                                                                       07230041
      IF (ICZERO) 36450, 6450, 36450                                    07240041
 6450 CONTINUE                                                          07250041
      IVON01 = 7                                                        07260041
      IVCOMP = IVON01 ** 3                                              07270041
      GO TO 46450                                                       07280041
36450 IVDELE = IVDELE + 1                                               07290041
      WRITE (I02,80003) IVTNUM                                          07300041
      IF (ICZERO) 46450, 6461, 46450                                    07310041
46450 IF (IVCOMP - 343) 26450,16450,26450                               07320041
16450 IVPASS = IVPASS + 1                                               07330041
      WRITE (I02,80001) IVTNUM                                          07340041
      GO TO 6461                                                        07350041
26450 IVFAIL = IVFAIL + 1                                               07360041
      IVCORR = 343                                                      07370041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07380041
 6461 CONTINUE                                                          07390041
      IVTNUM = 646                                                      07400041
C                                                                       07410041
C      ****  TEST 646  ****                                             07420041
C                                                                       07430041
      IF (ICZERO) 36460, 6460, 36460                                    07440041
 6460 CONTINUE                                                          07450041
      IVON01 = -7                                                       07460041
      IVCOMP = IVON01 ** 3                                              07470041
      GO TO 46460                                                       07480041
36460 IVDELE = IVDELE + 1                                               07490041
      WRITE (I02,80003) IVTNUM                                          07500041
      IF (ICZERO) 46460, 6471, 46460                                    07510041
46460 IF (IVCOMP + 343) 26460,16460,26460                               07520041
16460 IVPASS = IVPASS + 1                                               07530041
      WRITE (I02,80001) IVTNUM                                          07540041
      GO TO 6471                                                        07550041
26460 IVFAIL = IVFAIL + 1                                               07560041
      IVCORR = -343                                                     07570041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07580041
 6471 CONTINUE                                                          07590041
      IVTNUM = 647                                                      07600041
C                                                                       07610041
C      ****  TEST 647  ****                                             07620041
C                                                                       07630041
      IF (ICZERO) 36470, 6470, 36470                                    07640041
 6470 CONTINUE                                                          07650041
      IVON01 = 7                                                        07660041
      IVCOMP = IVON01 ** 4                                              07670041
      GO TO 46470                                                       07680041
36470 IVDELE = IVDELE + 1                                               07690041
      WRITE (I02,80003) IVTNUM                                          07700041
      IF (ICZERO) 46470, 6481, 46470                                    07710041
46470 IF (IVCOMP - 2401) 26470,16470,26470                              07720041
16470 IVPASS = IVPASS + 1                                               07730041
      WRITE (I02,80001) IVTNUM                                          07740041
      GO TO 6481                                                        07750041
26470 IVFAIL = IVFAIL + 1                                               07760041
      IVCORR = 2401                                                     07770041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07780041
 6481 CONTINUE                                                          07790041
      IVTNUM = 648                                                      07800041
C                                                                       07810041
C      ****  TEST 648  ****                                             07820041
C                                                                       07830041
      IF (ICZERO) 36480, 6480, 36480                                    07840041
 6480 CONTINUE                                                          07850041
      IVON01 = -7                                                       07860041
      IVCOMP = IVON01 ** 4                                              07870041
      GO TO 46480                                                       07880041
36480 IVDELE = IVDELE + 1                                               07890041
      WRITE (I02,80003) IVTNUM                                          07900041
      IF (ICZERO) 46480, 6491, 46480                                    07910041
46480 IF (IVCOMP - 2401) 26480,16480,26480                              07920041
16480 IVPASS = IVPASS + 1                                               07930041
      WRITE (I02,80001) IVTNUM                                          07940041
      GO TO 6491                                                        07950041
26480 IVFAIL = IVFAIL + 1                                               07960041
      IVCORR = 2401                                                     07970041
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07980041
 6491 CONTINUE                                                          07990041
C      ***    END OF TESTS    ***                                       08000041
C                                                                       08010041
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08020041
99999 CONTINUE                                                          08030041
      WRITE (I02,90002)                                                 08040041
      WRITE (I02,90006)                                                 08050041
      WRITE (I02,90002)                                                 08060041
      WRITE (I02,90002)                                                 08070041
      WRITE (I02,90007)                                                 08080041
      WRITE (I02,90002)                                                 08090041
      WRITE (I02,90008)  IVFAIL                                         08100041
      WRITE (I02,90009) IVPASS                                          08110041
      WRITE (I02,90010) IVDELE                                          08120041
C                                                                       08130041
C                                                                       08140041
C     TERMINATE ROUTINE EXECUTION                                       08150041
      STOP                                                              08160041
C                                                                       08170041
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08180041
90000 FORMAT (1H1)                                                      08190041
90002 FORMAT (1H )                                                      08200041
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08210041
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08220041
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08230041
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08240041
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08250041
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08260041
C                                                                       08270041
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08280041
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08290041
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08300041
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08310041
C                                                                       08320041
C     FORMAT STATEMENTS FOR TEST RESULTS                                08330041
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08340041
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08350041
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08360041
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08370041
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08380041
C                                                                       08390041
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM041)                          08400041
      END                                                               08410041
