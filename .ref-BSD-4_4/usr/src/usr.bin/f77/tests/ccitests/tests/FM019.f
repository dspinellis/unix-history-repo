C                                                                       00010019
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020019
C                                                                       00030019
C     FM019                                                             00040019
C                                                                       00050019
C           THIS ROUTINE CONTINUES TESTS OF THE FORTRAN LOGICAL IF STATE00060019
C     BY TESTING VARIOUS FORMS OF RELATIONAL EXPRESSIONS WITH ARITHMETIC00070019
C     EXPRESSIONS .  POSITIVE AND NEGATIVE SIGNS ARE USED IN CONJUNCTION00080019
C     WITH PARENTHESES. COMBINATIONS OF LOGICAL  .AND.    .OR.          00090019
C     .NOT. ARE USED TO TEST THE MORE COMPLEX EXPRESSIONS.              00100019
C                                                                       00110019
C      REFERENCES                                                       00120019
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00130019
C              X3.9-1978                                                00140019
C                                                                       00150019
C        SECTION 4.7.1, LOGICAL CONSTANT                                00160019
C        SECTION 6, EXPRESSIONS                                         00170019
C        SECTION 11.5, LOGICAL IF STATEMENT                             00180019
C                                                                       00190019
      LOGICAL LCTNT1, LCTNT2                                            00200019
C                                                                       00210019
C      **********************************************************       00220019
C                                                                       00230019
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00240019
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00250019
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00260019
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00270019
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00280019
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00290019
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00300019
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00310019
C     OF EXECUTING THESE TESTS.                                         00320019
C                                                                       00330019
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00340019
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00350019
C                                                                       00360019
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00370019
C                                                                       00380019
C                  DEPARTMENT OF THE NAVY                               00390019
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00400019
C                  WASHINGTON, D.C.  20376                              00410019
C                                                                       00420019
C      **********************************************************       00430019
C                                                                       00440019
C                                                                       00450019
C                                                                       00460019
C     INITIALIZATION SECTION                                            00470019
C                                                                       00480019
C     INITIALIZE CONSTANTS                                              00490019
C      **************                                                   00500019
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00510019
      I01 = 5                                                           00520019
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00530019
      I02 = 6                                                           00540019
C     SYSTEM ENVIRONMENT SECTION                                        00550019
C                                                                       00560019
      I01 = 5                                                           00570019
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00580019
C     (UNIT NUMBER FOR CARD READER).                                    00590019
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00600019
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00610019
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00620019
C                                                                       00630019
      I02 = 6                                                           00640019
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00650019
C     (UNIT NUMBER FOR PRINTER).                                        00660019
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00670019
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00680019
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00690019
C                                                                       00700019
      IVPASS=0                                                          00710019
      IVFAIL=0                                                          00720019
      IVDELE=0                                                          00730019
      ICZERO=0                                                          00740019
C                                                                       00750019
C     WRITE PAGE HEADERS                                                00760019
      WRITE (I02,90000)                                                 00770019
      WRITE (I02,90001)                                                 00780019
      WRITE (I02,90002)                                                 00790019
      WRITE (I02, 90002)                                                00800019
      WRITE (I02,90003)                                                 00810019
      WRITE (I02,90002)                                                 00820019
      WRITE (I02,90004)                                                 00830019
      WRITE (I02,90002)                                                 00840019
      WRITE (I02,90011)                                                 00850019
      WRITE (I02,90002)                                                 00860019
      WRITE (I02,90002)                                                 00870019
      WRITE (I02,90005)                                                 00880019
      WRITE (I02,90006)                                                 00890019
      WRITE (I02,90002)                                                 00900019
      IVTNUM = 530                                                      00910019
C                                                                       00920019
C      ****  TEST 530  ****                                             00930019
C     TEST 530  - TEST OF POSITIVELY SIGNED TERM   +(IC) (RO) -(IC)     00940019
C           .LT.  FALSE PATH                                            00950019
C                                                                       00960019
      IF (ICZERO) 35300, 5300, 35300                                    00970019
 5300 CONTINUE                                                          00980019
      IVON01 = 1                                                        00990019
      IF ( +3 .LT. -3)  IVON01 = 0                                      01000019
      GO TO 45300                                                       01010019
35300 IVDELE = IVDELE + 1                                               01020019
      WRITE (I02,80003) IVTNUM                                          01030019
      IF (ICZERO) 45300, 5311, 45300                                    01040019
45300 IF ( IVON01 - 1 )  25300, 15300, 25300                            01050019
15300 IVPASS = IVPASS + 1                                               01060019
      WRITE (I02,80001) IVTNUM                                          01070019
      GO TO 5311                                                        01080019
25300 IVFAIL = IVFAIL + 1                                               01090019
      IVCOMP = IVON01                                                   01100019
      IVCORR = 1                                                        01110019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01120019
 5311 CONTINUE                                                          01130019
      IVTNUM = 531                                                      01140019
C                                                                       01150019
C      ****  TEST 531  ****                                             01160019
C     TEST 531  -  TEST OF SIGNED ZERO     .LT.  FALSE PATH             01170019
C                                                                       01180019
C                                                                       01190019
      IF (ICZERO) 35310, 5310, 35310                                    01200019
 5310 CONTINUE                                                          01210019
      IVON01 = 1                                                        01220019
      IF ( +0 .LT. -0 )  IVON01 = 0                                     01230019
      GO TO 45310                                                       01240019
35310 IVDELE = IVDELE + 1                                               01250019
      WRITE (I02,80003) IVTNUM                                          01260019
      IF (ICZERO) 45310, 5321, 45310                                    01270019
45310 IF ( IVON01 - 1 )  25310, 15310, 25310                            01280019
15310 IVPASS = IVPASS + 1                                               01290019
      WRITE (I02,80001) IVTNUM                                          01300019
      GO TO 5321                                                        01310019
25310 IVFAIL = IVFAIL + 1                                               01320019
      IVCOMP = IVON01                                                   01330019
      IVCORR = 1                                                        01340019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01350019
 5321 CONTINUE                                                          01360019
      IVTNUM = 532                                                      01370019
C                                                                       01380019
C      ****  TEST 532  ****                                             01390019
C     TEST 532  -  TEST OF SIGNED ZERO  .LE.  TRUE PATH                 01400019
C                                                                       01410019
C                                                                       01420019
      IF (ICZERO) 35320, 5320, 35320                                    01430019
 5320 CONTINUE                                                          01440019
      IVON01 = 0                                                        01450019
      IF ( +0 .LE. -0 )  IVON01 = 1                                     01460019
      GO TO 45320                                                       01470019
35320 IVDELE = IVDELE + 1                                               01480019
      WRITE (I02,80003) IVTNUM                                          01490019
      IF (ICZERO) 45320, 5331, 45320                                    01500019
45320 IF ( IVON01 - 1 )  25320, 15320, 25320                            01510019
15320 IVPASS = IVPASS + 1                                               01520019
      WRITE (I02,80001) IVTNUM                                          01530019
      GO TO 5331                                                        01540019
25320 IVFAIL = IVFAIL + 1                                               01550019
      IVCOMP = IVON01                                                   01560019
      IVCORR = 1                                                        01570019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01580019
 5331 CONTINUE                                                          01590019
      IVTNUM = 533                                                      01600019
C                                                                       01610019
C      ****  TEST 533  ****                                             01620019
C     TEST 533  -  TEST OF SIGNED ZERO  .EQ.  TRUE PATH                 01630019
C                                                                       01640019
C                                                                       01650019
      IF (ICZERO) 35330, 5330, 35330                                    01660019
 5330 CONTINUE                                                          01670019
      IVON01 = 0                                                        01680019
      IF ( +0 .EQ. -0 )  IVON01 = 1                                     01690019
      GO TO 45330                                                       01700019
35330 IVDELE = IVDELE + 1                                               01710019
      WRITE (I02,80003) IVTNUM                                          01720019
      IF (ICZERO) 45330, 5341, 45330                                    01730019
45330 IF ( IVON01 - 1 )  25330, 15330, 25330                            01740019
15330 IVPASS = IVPASS + 1                                               01750019
      WRITE (I02,80001) IVTNUM                                          01760019
      GO TO 5341                                                        01770019
25330 IVFAIL = IVFAIL + 1                                               01780019
      IVCOMP = IVON01                                                   01790019
      IVCORR = 1                                                        01800019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01810019
 5341 CONTINUE                                                          01820019
      IVTNUM = 534                                                      01830019
C                                                                       01840019
C      ****  TEST 534  ****                                             01850019
C     TEST 534  -  TEST OF SIGNED ZERO  .NE.  FALSE PATH                01860019
C                                                                       01870019
C                                                                       01880019
      IF (ICZERO) 35340, 5340, 35340                                    01890019
 5340 CONTINUE                                                          01900019
      IVON01 = 1                                                        01910019
      IF ( +0 .NE. -0 )  IVON01 = 0                                     01920019
      GO TO 45340                                                       01930019
35340 IVDELE = IVDELE + 1                                               01940019
      WRITE (I02,80003) IVTNUM                                          01950019
      IF (ICZERO) 45340, 5351, 45340                                    01960019
45340 IF ( IVON01 - 1 )  25340, 15340, 25340                            01970019
15340 IVPASS = IVPASS + 1                                               01980019
      WRITE (I02,80001) IVTNUM                                          01990019
      GO TO 5351                                                        02000019
25340 IVFAIL = IVFAIL + 1                                               02010019
      IVCOMP = IVON01                                                   02020019
      IVCORR = 1                                                        02030019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02040019
 5351 CONTINUE                                                          02050019
      IVTNUM = 535                                                      02060019
C                                                                       02070019
C      ****  TEST 535  ****                                             02080019
C     TEST 535  -  TEST OF SIGNED ZERO  .GE.  TRUE PATH                 02090019
C                                                                       02100019
C                                                                       02110019
      IF (ICZERO) 35350, 5350, 35350                                    02120019
 5350 CONTINUE                                                          02130019
      IVON01 = 0                                                        02140019
      IF ( +0 .GE. -0 )  IVON01 = 1                                     02150019
      GO TO 45350                                                       02160019
35350 IVDELE = IVDELE + 1                                               02170019
      WRITE (I02,80003) IVTNUM                                          02180019
      IF (ICZERO) 45350, 5361, 45350                                    02190019
45350 IF ( IVON01 - 1 )  25350, 15350, 25350                            02200019
15350 IVPASS = IVPASS + 1                                               02210019
      WRITE (I02,80001) IVTNUM                                          02220019
      GO TO 5361                                                        02230019
25350 IVFAIL = IVFAIL + 1                                               02240019
      IVCOMP = IVON01                                                   02250019
      IVCORR = 1                                                        02260019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02270019
 5361 CONTINUE                                                          02280019
      IVTNUM = 536                                                      02290019
C                                                                       02300019
C      ****  TEST 536  ****                                             02310019
C     TEST 536  -  TEST OF SIGNED ZERO  .GT.  FALSE PATH                02320019
C                                                                       02330019
C                                                                       02340019
      IF (ICZERO) 35360, 5360, 35360                                    02350019
 5360 CONTINUE                                                          02360019
      IVON01 = 1                                                        02370019
      IF ( +0 .GT. -0 )  IVON01 = 0                                     02380019
      GO TO 45360                                                       02390019
35360 IVDELE = IVDELE + 1                                               02400019
      WRITE (I02,80003) IVTNUM                                          02410019
      IF (ICZERO) 45360, 5371, 45360                                    02420019
45360 IF ( IVON01 - 1 )  25360, 15360, 25360                            02430019
15360 IVPASS = IVPASS + 1                                               02440019
      WRITE (I02,80001) IVTNUM                                          02450019
      GO TO 5371                                                        02460019
25360 IVFAIL = IVFAIL + 1                                               02470019
      IVCOMP = IVON01                                                   02480019
      IVCORR = 1                                                        02490019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02500019
 5371 CONTINUE                                                          02510019
      IVTNUM = 537                                                      02520019
C                                                                       02530019
C      ****  TEST 537  ****                                             02540019
C     TEST 537  -  TEST OF +32767 .EQ. -32766  FALSE PATH               02550019
C                                                                       02560019
C                                                                       02570019
      IF (ICZERO) 35370, 5370, 35370                                    02580019
 5370 CONTINUE                                                          02590019
      IVON01 = 1                                                        02600019
      IF ( +32767 .EQ. -32766 )  IVON01 = 0                             02610019
      GO TO 45370                                                       02620019
35370 IVDELE = IVDELE + 1                                               02630019
      WRITE (I02,80003) IVTNUM                                          02640019
      IF (ICZERO) 45370, 5381, 45370                                    02650019
45370 IF ( IVON01 - 1 )  25370, 15370, 25370                            02660019
15370 IVPASS = IVPASS + 1                                               02670019
      WRITE (I02,80001) IVTNUM                                          02680019
      GO TO 5381                                                        02690019
25370 IVFAIL = IVFAIL + 1                                               02700019
      IVCOMP = IVON01                                                   02710019
      IVCORR = 1                                                        02720019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02730019
 5381 CONTINUE                                                          02740019
      IVTNUM = 538                                                      02750019
C                                                                       02760019
C      ****  TEST 538  ****                                             02770019
C     TEST 538  -  TESTS MINUS SIGN WITH INTEGER VARIABLES              02780019
C           RELATIONAL EXPRESSION USES  .LE.  TRUE PATH                 02790019
C                                                                       02800019
C                                                                       02810019
      IF (ICZERO) 35380, 5380, 35380                                    02820019
 5380 CONTINUE                                                          02830019
      IVON01 = 0                                                        02840019
      IVON02 = 3                                                        02850019
      IF ( -IVON02 .LE. -IVON02 )  IVON01 = 1                           02860019
      GO TO 45380                                                       02870019
35380 IVDELE = IVDELE + 1                                               02880019
      WRITE (I02,80003) IVTNUM                                          02890019
      IF (ICZERO) 45380, 5391, 45380                                    02900019
45380 IF ( IVON01 - 1 )  25380, 15380, 25380                            02910019
15380 IVPASS = IVPASS + 1                                               02920019
      WRITE (I02,80001) IVTNUM                                          02930019
      GO TO 5391                                                        02940019
25380 IVFAIL = IVFAIL + 1                                               02950019
      IVCOMP = IVON01                                                   02960019
      IVCORR = 1                                                        02970019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02980019
 5391 CONTINUE                                                          02990019
      IVTNUM = 539                                                      03000019
C                                                                       03010019
C      ****  TEST 539  ****                                             03020019
C     TEST 539  -  TEST IS LIKE TEST 538   USES  .GE.  TRUE PATH        03030019
C                                                                       03040019
C                                                                       03050019
      IF (ICZERO) 35390, 5390, 35390                                    03060019
 5390 CONTINUE                                                          03070019
      IVON01 = 0                                                        03080019
      IVON02 = 32766                                                    03090019
      IF ( -IVON02 .GE. -IVON02 )  IVON01 = 1                           03100019
      GO TO 45390                                                       03110019
35390 IVDELE = IVDELE + 1                                               03120019
      WRITE (I02,80003) IVTNUM                                          03130019
      IF (ICZERO) 45390, 5401, 45390                                    03140019
45390 IF ( IVON01 - 1 )  25390, 15390, 25390                            03150019
15390 IVPASS = IVPASS + 1                                               03160019
      WRITE (I02,80001) IVTNUM                                          03170019
      GO TO 5401                                                        03180019
25390 IVFAIL = IVFAIL + 1                                               03190019
      IVCOMP = IVON01                                                   03200019
      IVCORR = 1                                                        03210019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03220019
 5401 CONTINUE                                                          03230019
      IVTNUM = 540                                                      03240019
C                                                                       03250019
C      ****  TEST 540  ****                                             03260019
C     TEST 540  -  INTEGER EXPONIENTIATION AND MINUS SIGN  USES .NE.    03270019
C           FALSE PATH                                                  03280019
C                                                                       03290019
C                                                                       03300019
      IF (ICZERO) 35400, 5400, 35400                                    03310019
 5400 CONTINUE                                                          03320019
      IVON01 = 1                                                        03330019
      IVON02 = 3                                                        03340019
      IF ( -IVON02 ** 3 .NE. -27 )  IVON01 = 0                          03350019
      GO TO 45400                                                       03360019
35400 IVDELE = IVDELE + 1                                               03370019
      WRITE (I02,80003) IVTNUM                                          03380019
      IF (ICZERO) 45400, 5411, 45400                                    03390019
45400 IF ( IVON01 - 1 )  25400, 15400, 25400                            03400019
15400 IVPASS = IVPASS + 1                                               03410019
      WRITE (I02,80001) IVTNUM                                          03420019
      GO TO 5411                                                        03430019
25400 IVFAIL = IVFAIL + 1                                               03440019
      IVCOMP = IVON01                                                   03450019
      IVCORR = 1                                                        03460019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03470019
 5411 CONTINUE                                                          03480019
      IVTNUM = 541                                                      03490019
C                                                                       03500019
C      ****  TEST 541  ****                                             03510019
C     TEST 541  -  LIKE TEST 540  USES  .LE.  TRUE PATH                 03520019
C                                                                       03530019
C                                                                       03540019
      IF (ICZERO) 35410, 5410, 35410                                    03550019
 5410 CONTINUE                                                          03560019
      IVON01 = 0                                                        03570019
      IVON02 = 3                                                        03580019
      IF ( -3 ** IVON02  .LE. -27 )  IVON01 = 1                         03590019
      GO TO 45410                                                       03600019
35410 IVDELE = IVDELE + 1                                               03610019
      WRITE (I02,80003) IVTNUM                                          03620019
      IF (ICZERO) 45410, 5421, 45410                                    03630019
45410 IF ( IVON01 - 1 )  25410, 15410, 25410                            03640019
15410 IVPASS = IVPASS + 1                                               03650019
      WRITE (I02,80001) IVTNUM                                          03660019
      GO TO 5421                                                        03670019
25410 IVFAIL = IVFAIL + 1                                               03680019
      IVCOMP = IVON01                                                   03690019
      IVCORR = 1                                                        03700019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03710019
 5421 CONTINUE                                                          03720019
      IVTNUM = 542                                                      03730019
C                                                                       03740019
C      ****  TEST 542  ****                                             03750019
C     TEST 542  -  INTEGER EXPONIENTIATION AND MULTIPLICATION           03760019
C           USES  .EQ.  TRUE PATH                                       03770019
C                                                                       03780019
C                                                                       03790019
      IF (ICZERO) 35420, 5420, 35420                                    03800019
 5420 CONTINUE                                                          03810019
      IVON01 = 0                                                        03820019
      IVON02 = 3                                                        03830019
      IVON03 = 27                                                       03840019
      IF ( -IVON02 ** 2 * IVON02 .EQ. -IVON03 )  IVON01 = 1             03850019
      GO TO 45420                                                       03860019
35420 IVDELE = IVDELE + 1                                               03870019
      WRITE (I02,80003) IVTNUM                                          03880019
      IF (ICZERO) 45420, 5431, 45420                                    03890019
45420 IF ( IVON01 - 1 )  25420, 15420, 25420                            03900019
15420 IVPASS = IVPASS + 1                                               03910019
      WRITE (I02,80001) IVTNUM                                          03920019
      GO TO 5431                                                        03930019
25420 IVFAIL = IVFAIL + 1                                               03940019
      IVCOMP = IVON01                                                   03950019
      IVCORR = 1                                                        03960019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03970019
 5431 CONTINUE                                                          03980019
      IVTNUM = 543                                                      03990019
C                                                                       04000019
C      ****  TEST 543  ****                                             04010019
C     TEST 543  -  INTEGER EXPONIENTIATION AND DIVISION                 04020019
C           USES  .LT.  TRUE PATH                                       04030019
C                                                                       04040019
C                                                                       04050019
      IF (ICZERO) 35430, 5430, 35430                                    04060019
 5430 CONTINUE                                                          04070019
      IVON01 = 0                                                        04080019
      IVON02 = 587                                                      04090019
      IVON03 = 3                                                        04100019
      IVON04 = 3                                                        04110019
      IF ( -IVON02/IVON04 ** 3 .LT. -3 ** IVON03/IVON02 )  IVON01 = 1   04120019
      GO TO 45430                                                       04130019
35430 IVDELE = IVDELE + 1                                               04140019
      WRITE (I02,80003) IVTNUM                                          04150019
      IF (ICZERO) 45430, 5441, 45430                                    04160019
45430 IF ( IVON01 - 1 )  25430, 15430, 25430                            04170019
15430 IVPASS = IVPASS + 1                                               04180019
      WRITE (I02,80001) IVTNUM                                          04190019
      GO TO 5441                                                        04200019
25430 IVFAIL = IVFAIL + 1                                               04210019
      IVCOMP = IVON01                                                   04220019
      IVCORR = 1                                                        04230019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04240019
 5441 CONTINUE                                                          04250019
      IVTNUM = 544                                                      04260019
C                                                                       04270019
C      ****  TEST 544  ****                                             04280019
C     TEST 544  -  INTEGER ADDITION AND SUBTRACTION                     04290019
C           USES  .EQ.  TRUE PATH                                       04300019
C                                                                       04310019
C                                                                       04320019
      IF (ICZERO) 35440, 5440, 35440                                    04330019
 5440 CONTINUE                                                          04340019
      IVON01 = 0                                                        04350019
      IVON02 = 3                                                        04360019
      IVON03 = 587                                                      04370019
      IF ( IVON02 - IVON03 .EQ. -IVON03 + IVON02 )  IVON01 = 1          04380019
      GO TO 45440                                                       04390019
35440 IVDELE = IVDELE + 1                                               04400019
      WRITE (I02,80003) IVTNUM                                          04410019
      IF (ICZERO) 45440, 5451, 45440                                    04420019
45440 IF ( IVON01 - 1 )  25440, 15440, 25440                            04430019
15440 IVPASS = IVPASS + 1                                               04440019
      WRITE (I02,80001) IVTNUM                                          04450019
      GO TO 5451                                                        04460019
25440 IVFAIL = IVFAIL + 1                                               04470019
      IVCOMP = IVON01                                                   04480019
      IVCORR = 1                                                        04490019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04500019
 5451 CONTINUE                                                          04510019
      IVTNUM = 545                                                      04520019
C                                                                       04530019
C      ****  TEST 545  ****                                             04540019
C     TEST 545  -  INTEGER ADDITION AND SUBTRACTION WITH PARENTHESES    04550019
C           USES  .EQ.  TRUE PATH  LIKE TEST 544                        04560019
C                                                                       04570019
C                                                                       04580019
      IF (ICZERO) 35450, 5450, 35450                                    04590019
 5450 CONTINUE                                                          04600019
      IVON01 = 0                                                        04610019
      IVON02 = 3                                                        04620019
      IVON03 = 587                                                      04630019
      IF ( (IVON02 - IVON03) .EQ. (-IVON03 + IVON02) )  IVON01 = 1      04640019
      GO TO 45450                                                       04650019
35450 IVDELE = IVDELE + 1                                               04660019
      WRITE (I02,80003) IVTNUM                                          04670019
      IF (ICZERO) 45450, 5461, 45450                                    04680019
45450 IF ( IVON01 - 1 ) 25450, 15450, 25450                             04690019
15450 IVPASS = IVPASS + 1                                               04700019
      WRITE (I02,80001) IVTNUM                                          04710019
      GO TO 5461                                                        04720019
25450 IVFAIL = IVFAIL + 1                                               04730019
      IVCOMP = IVON01                                                   04740019
      IVCORR = 1                                                        04750019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04760019
 5461 CONTINUE                                                          04770019
      IVTNUM = 546                                                      04780019
C                                                                       04790019
C      ****  TEST 546  ****                                             04800019
C     TEST 546  -  INTEGER EXPONIENTIATION AND DIVISION WITH PARENS     04810019
C           USES  .LT.  TRUE PATH                                       04820019
C                                                                       04830019
C                                                                       04840019
      IF (ICZERO) 35460, 5460, 35460                                    04850019
 5460 CONTINUE                                                          04860019
      IVON01 = 0                                                        04870019
      IVON02 = 587                                                      04880019
      IVON03 = 3                                                        04890019
      IVON04 = 3                                                        04900019
      IF ((-IVON02/(IVON04**3)).LT.((-3**IVON03)/IVON02))IVON01=1       04910019
      GO TO 45460                                                       04920019
35460 IVDELE = IVDELE + 1                                               04930019
      WRITE (I02,80003) IVTNUM                                          04940019
      IF (ICZERO) 45460, 5471, 45460                                    04950019
45460 IF ( IVON01 - 1 )  25460, 15460, 25460                            04960019
15460 IVPASS = IVPASS + 1                                               04970019
      WRITE (I02,80001) IVTNUM                                          04980019
      GO TO 5471                                                        04990019
25460 IVFAIL = IVFAIL + 1                                               05000019
      IVCOMP = IVON01                                                   05010019
      IVCORR = 1                                                        05020019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05030019
 5471 CONTINUE                                                          05040019
      IVTNUM = 547                                                      05050019
C                                                                       05060019
C      ****  TEST 547  ****                                             05070019
C     TEST 547  -  INTEGER MULTIPLICATION WITH PARENTHESES  .LT.  FALSE 05080019
C                                                                       05090019
C                                                                       05100019
      IF (ICZERO) 35470, 5470, 35470                                    05110019
 5470 CONTINUE                                                          05120019
      IVON01 = 1                                                        05130019
      IVON02 = 587                                                      05140019
      IF ((-3)*(-3).LT.(-IVON02))IVON01=0                               05150019
      GO TO 45470                                                       05160019
35470 IVDELE = IVDELE + 1                                               05170019
      WRITE (I02,80003) IVTNUM                                          05180019
      IF (ICZERO) 45470, 5481, 45470                                    05190019
45470 IF ( IVON01 - 1 )  25470, 15470, 25470                            05200019
15470 IVPASS = IVPASS + 1                                               05210019
      WRITE (I02,80001) IVTNUM                                          05220019
      GO TO 5481                                                        05230019
25470 IVFAIL = IVFAIL + 1                                               05240019
      IVCOMP = IVON01                                                   05250019
      IVCORR = 1                                                        05260019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05270019
 5481 CONTINUE                                                          05280019
      IVTNUM = 548                                                      05290019
C                                                                       05300019
C      ****  TEST 548  ****                                             05310019
C     TEST 548  -  INTEGER EXPONIENTIATION, MINUS SIGNS, AND PARENTHESES05320019
C           USES  .LE.  TRUE PATH                                       05330019
C                                                                       05340019
C                                                                       05350019
      IF (ICZERO) 35480, 5480, 35480                                    05360019
 5480 CONTINUE                                                          05370019
      IVON01 = 0                                                        05380019
      IVON02 = 3                                                        05390019
      IVON03 = 27                                                       05400019
      IF ( ((-IVON02) ** IVON02 .LE. (-IVON03)))  IVON01 = 1            05410019
      GO TO 45480                                                       05420019
35480 IVDELE = IVDELE + 1                                               05430019
      WRITE (I02,80003) IVTNUM                                          05440019
      IF (ICZERO) 45480, 5491, 45480                                    05450019
45480 IF ( IVON01 - 1 )  25480, 15480, 25480                            05460019
15480 IVPASS = IVPASS + 1                                               05470019
      WRITE (I02,80001) IVTNUM                                          05480019
      GO TO 5491                                                        05490019
25480 IVFAIL = IVFAIL + 1                                               05500019
      IVCOMP = IVON01                                                   05510019
      IVCORR = 1                                                        05520019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05530019
 5491 CONTINUE                                                          05540019
      IVTNUM = 549                                                      05550019
C                                                                       05560019
C      ****  TEST 549  ****                                             05570019
C     TEST 549  -  TEST THE ORDER OF INTEGER ARITHMETIC OPERATIONS      05580019
C           USES INTEGER EXPONIENTIATION, ADDITION, MULTIPLICATION,     05590019
C           AND PARENTHESES.  ALSO USES  .EQ.  TRUE PATH                05600019
C           SEE SECTION 6.1, ARITHMETIC EXPRESSIONS.                    05610019
C                                                                       05620019
C                                                                       05630019
      IF (ICZERO) 35490, 5490, 35490                                    05640019
 5490 CONTINUE                                                          05650019
      IVON01 = 0                                                        05660019
      IVON02 = 3                                                        05670019
      IF(IVON02 * IVON02/(IVON02+IVON02)**IVON02+IVON02 .EQ. 3) IVON01=105680019
      GO TO 45490                                                       05690019
35490 IVDELE = IVDELE + 1                                               05700019
      WRITE (I02,80003) IVTNUM                                          05710019
      IF (ICZERO) 45490, 5501, 45490                                    05720019
45490 IF ( IVON01 - 1 )  25490, 15490, 25490                            05730019
15490 IVPASS = IVPASS + 1                                               05740019
      WRITE (I02,80001) IVTNUM                                          05750019
      GO TO 5501                                                        05760019
25490 IVFAIL = IVFAIL + 1                                               05770019
      IVCOMP = IVON01                                                   05780019
      IVCORR = 1                                                        05790019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05800019
 5501 CONTINUE                                                          05810019
      IVTNUM = 550                                                      05820019
C                                                                       05830019
C      ****  TEST 550  ****                                             05840019
C     TEST 550  -  COMBINATION OF LOGICAL  .NOT. AND  .AND.             05850019
C           .NOT. (LP) .AND. .NOT. (LP)                                 05860019
C           TRUE PATH                                                   05870019
C                                                                       05880019
C                                                                       05890019
      IF (ICZERO) 35500, 5500, 35500                                    05900019
 5500 CONTINUE                                                          05910019
      IVON01 = 0                                                        05920019
      LCTNT1 = .FALSE.                                                  05930019
      IF ( .NOT. .FALSE. .AND. .NOT. LCTNT1 )  IVON01 = 1               05940019
      GO TO 45500                                                       05950019
35500 IVDELE = IVDELE + 1                                               05960019
      WRITE (I02,80003) IVTNUM                                          05970019
      IF (ICZERO) 45500, 5511, 45500                                    05980019
45500 IF ( IVON01 - 1 )  25500, 15500, 25500                            05990019
15500 IVPASS = IVPASS + 1                                               06000019
      WRITE (I02,80001) IVTNUM                                          06010019
      GO TO 5511                                                        06020019
25500 IVFAIL = IVFAIL + 1                                               06030019
      IVCOMP = IVON01                                                   06040019
      IVCORR = 1                                                        06050019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06060019
 5511 CONTINUE                                                          06070019
      IVTNUM = 551                                                      06080019
C                                                                       06090019
C      ****  TEST 551  ****                                             06100019
C     TEST 551  -  COMBINATION OF LOGICAL .OR. AND .NOT.                06110019
C           .NOT. (LP) .OR. .NOT. (LP)                                  06120019
C           TRUE PATH                                                   06130019
C                                                                       06140019
C                                                                       06150019
      IF (ICZERO) 35510, 5510, 35510                                    06160019
 5510 CONTINUE                                                          06170019
      IVON01 = 0                                                        06180019
      LCTNT1 = .TRUE.                                                   06190019
      LCTNT2 = .FALSE.                                                  06200019
      IF ( .NOT. LCTNT1 .OR. .NOT. LCTNT2 )  IVON01 = 1                 06210019
      GO TO 45510                                                       06220019
35510 IVDELE = IVDELE + 1                                               06230019
      WRITE (I02,80003) IVTNUM                                          06240019
      IF (ICZERO) 45510, 5521, 45510                                    06250019
45510 IF ( IVON01 - 1 )  25510, 15510, 25510                            06260019
15510 IVPASS = IVPASS + 1                                               06270019
      WRITE (I02,80001) IVTNUM                                          06280019
      GO TO 5521                                                        06290019
25510 IVFAIL = IVFAIL + 1                                               06300019
      IVCOMP = IVON01                                                   06310019
      IVCORR = 1                                                        06320019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06330019
 5521 CONTINUE                                                          06340019
      IVTNUM = 552                                                      06350019
C                                                                       06360019
C      ****  TEST 552  ****                                             06370019
C     TEST 552  -  COMBINATION OF LOGICAL .AND.  .OR.  AND  .NOT.       06380019
C           .NOT. ( (LE) .OR. (LT) ) .AND. .NOT. ( (LT) .AND. (LF) )    06390019
C           .NOT. IS APPLIED TO A LOGICAL EXPRESSION INCLOSED IN PARENS 06400019
C           FALSE PATH                                                  06410019
C                                                                       06420019
      IF (ICZERO) 35520, 5520, 35520                                    06430019
 5520 CONTINUE                                                          06440019
      IVON01 = 1                                                        06450019
      LCTNT1 = .FALSE.                                                  06460019
      LCTNT2 = .TRUE.                                                   06470019
      IF(.NOT.(LCTNT1.OR.LCTNT2).AND..NOT.(LCTNT1.AND.LCTNT2))IVON01 = 006480019
      GO TO 45520                                                       06490019
35520 IVDELE = IVDELE + 1                                               06500019
      WRITE (I02,80003) IVTNUM                                          06510019
      IF (ICZERO) 45520, 5531, 45520                                    06520019
45520 IF ( IVON01 - 1 )  25520, 15520, 25520                            06530019
15520 IVPASS = IVPASS + 1                                               06540019
      WRITE (I02,80001) IVTNUM                                          06550019
      GO TO 5531                                                        06560019
25520 IVFAIL = IVFAIL + 1                                               06570019
      IVCOMP = IVON01                                                   06580019
      IVCORR = 1                                                        06590019
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06600019
 5531 CONTINUE                                                          06610019
C                                                                       06620019
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             06630019
99999 CONTINUE                                                          06640019
      WRITE (I02,90002)                                                 06650019
      WRITE (I02,90006)                                                 06660019
      WRITE (I02,90002)                                                 06670019
      WRITE (I02,90002)                                                 06680019
      WRITE (I02,90007)                                                 06690019
      WRITE (I02,90002)                                                 06700019
      WRITE (I02,90008)  IVFAIL                                         06710019
      WRITE (I02,90009) IVPASS                                          06720019
      WRITE (I02,90010) IVDELE                                          06730019
C                                                                       06740019
C                                                                       06750019
C     TERMINATE ROUTINE EXECUTION                                       06760019
      STOP                                                              06770019
C                                                                       06780019
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06790019
90000 FORMAT (1H1)                                                      06800019
90002 FORMAT (1H )                                                      06810019
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06820019
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   06830019
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        06840019
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 06850019
90006 FORMAT (1H ,5X,46H----------------------------------------------) 06860019
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             06870019
C                                                                       06880019
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               06890019
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        06900019
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              06910019
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             06920019
C                                                                       06930019
C     FORMAT STATEMENTS FOR TEST RESULTS                                06940019
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      06950019
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      06960019
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   06970019
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         06980019
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    06990019
C                                                                       07000019
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM019)                          07010019
      END                                                               07020019
