C     COMMENT SECTION                                                   00010004
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020004
C     FM004                                                             00030004
C                                                                       00040004
C         THIS ROUTINE CONTAINS BASIC ARITHMETIC IF STATEMENT TESTS.    00050004
C     THE STATEMENT FORMAT IS                                           00060004
C                IF  (E)  K1, K2, K3                                    00070004
C     WHERE E IS A SIMPLE INTEGER EXPRESSION OF FORM                    00080004
C                VARIABLE - CONSTANT                                    00090004
C                VARIABLE + CONSTANT                                    00100004
C     AND K1, K2 AND K3 ARE STATEMENT LABELS.  ONLY THE STATEMENTS IN   00110004
C     THE BASIC ASSUMPTIONS ARE INCLUDED IN THESE TESTS.                00120004
C         EXECUTION OF AN IF STATEMENT CAUSES EVALUATION OF THE         00130004
C     EXPRESSION E FOLLOWING WHICH THE STATEMENT LABEL K1, K2 OR K3     00140004
C     IS EXECUTED NEXT AS THE VALUE OF E IS LESS THAN ZERO, ZERO, OR    00150004
C     GREATER THAN ZERO, RESPECTIVELY.                                  00160004
C                                                                       00170004
C         THE BASIC UNCONDITIONAL GO TO STATEMENT IS TESTED IN THIS     00180004
C     ROUTINE. THE STATEMENT IS OF THE FORM                             00190004
C               GO TO K                                                 00200004
C     WHERE K IS A STATEMENT LABEL.                                     00210004
C         EXECUTION OF AN UNCONDITIONAL GO TO STATEMENT CAUSES THE      00220004
C     STATEMENT IDENTIFIED BY STATEMENT LABEL K TO BE EXECUTED NEXT.    00230004
C                                                                       00240004
C      REFERENCES                                                       00250004
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00260004
C              X3.9-1978                                                00270004
C                                                                       00280004
C        SECTION 3.6, NORMAL EXECUTION SEQUENCE AND TRANSFER OF CONTROL 00290004
C        SECTION 11.1, GO TO STATEMENT                                  00300004
C        SECTION 11.4, ARITHMETIC IF STATEMENT                          00310004
C                                                                       00320004
C      **********************************************************       00330004
C                                                                       00340004
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00350004
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00360004
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00370004
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00380004
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00390004
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00400004
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00410004
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00420004
C     OF EXECUTING THESE TESTS.                                         00430004
C                                                                       00440004
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00450004
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00460004
C                                                                       00470004
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00480004
C                                                                       00490004
C                  DEPARTMENT OF THE NAVY                               00500004
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00510004
C                  WASHINGTON, D.C.  20376                              00520004
C                                                                       00530004
C      **********************************************************       00540004
C                                                                       00550004
C                                                                       00560004
C                                                                       00570004
C     INITIALIZATION SECTION                                            00580004
C                                                                       00590004
C     INITIALIZE CONSTANTS                                              00600004
C      **************                                                   00610004
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620004
      I01 = 5                                                           00630004
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640004
      I02 = 6                                                           00650004
C     SYSTEM ENVIRONMENT SECTION                                        00660004
C                                                                       00670004
      I01 = 5                                                           00680004
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690004
C     (UNIT NUMBER FOR CARD READER).                                    00700004
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00710004
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00720004
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00730004
C                                                                       00740004
      I02 = 6                                                           00750004
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00760004
C     (UNIT NUMBER FOR PRINTER).                                        00770004
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00780004
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00790004
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00800004
C                                                                       00810004
      IVPASS=0                                                          00820004
      IVFAIL=0                                                          00830004
      IVDELE=0                                                          00840004
      ICZERO=0                                                          00850004
C                                                                       00860004
C     WRITE PAGE HEADERS                                                00870004
      WRITE (I02,90000)                                                 00880004
      WRITE (I02,90001)                                                 00890004
      WRITE (I02,90002)                                                 00900004
      WRITE (I02, 90002)                                                00910004
      WRITE (I02,90003)                                                 00920004
      WRITE (I02,90002)                                                 00930004
      WRITE (I02,90004)                                                 00940004
      WRITE (I02,90002)                                                 00950004
      WRITE (I02,90011)                                                 00960004
      WRITE (I02,90002)                                                 00970004
      WRITE (I02,90002)                                                 00980004
      WRITE (I02,90005)                                                 00990004
      WRITE (I02,90006)                                                 01000004
      WRITE (I02,90002)                                                 01010004
C          TEST SECTION                                                 01020004
C                                                                       01030004
C         TESTS 21, 22, AND 23 CONTAIN THE SAME IF STATEMENT BUT THE    01040004
C     EXPECTED BRANCH IS TO THE FIRST, SECOND OR THIRD STATEMENT LABEL  01050004
C     AS THE INTEGER EXPRESSION IS LESS THAN ZERO, EQUAL TO ZERO, OR    01060004
C     GREATER THAN ZERO RESPECTIVELY.                                   01070004
C                                                                       01080004
  211 CONTINUE                                                          01090004
      IVTNUM =  21                                                      01100004
C                                                                       01110004
C      ****  TEST 021  ****                                             01120004
C     TEST 21 - ARITHMETIC IF STATEMENT TEST                            01130004
C         LESS THAN ZERO BRANCH EXPECTED.                               01140004
C                                                                       01150004
      IF (ICZERO) 30210,  210, 30210                                    01160004
  210 CONTINUE                                                          01170004
      IVON01=2                                                          01180004
      IF (IVON01 - 3) 212,213,214                                       01190004
  212 IVON02 = -1                                                       01200004
      GO TO 40210                                                       01210004
  213 IVON02 = 0                                                        01220004
      GO TO 40210                                                       01230004
  214 IVON02 = 1                                                        01240004
      GO TO 40210                                                       01250004
30210 IVDELE = IVDELE + 1                                               01260004
      WRITE (I02,80003) IVTNUM                                          01270004
      IF (ICZERO) 40210,  221, 40210                                    01280004
40210 IF (IVON02) 10210, 20210, 20210                                   01290004
10210 IVPASS = IVPASS + 1                                               01300004
      WRITE (I02,80001) IVTNUM                                          01310004
      GO TO  221                                                        01320004
20210 IVFAIL = IVFAIL + 1                                               01330004
      IVCOMP=IVON02                                                     01340004
      IVCORR=-1                                                         01350004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01360004
  221 CONTINUE                                                          01370004
      IVTNUM =  22                                                      01380004
C                                                                       01390004
C      ****  TEST 022  ****                                             01400004
C     TEST 22 - ARITHMETIC IF STATEMENT TEST                            01410004
C         EQUAL TO ZERO BRANCH EXPECTED                                 01420004
C                                                                       01430004
      IF (ICZERO) 30220,  220, 30220                                    01440004
  220 CONTINUE                                                          01450004
      IVON01 = 3                                                        01460004
      IF (IVON01 - 3) 222,223,224                                       01470004
  222 IVON02 = -1                                                       01480004
      GO TO 40220                                                       01490004
  223 IVON02 = 0                                                        01500004
      GO TO 40220                                                       01510004
  224 IVON02 = 1                                                        01520004
      GO TO 40220                                                       01530004
30220 IVDELE = IVDELE + 1                                               01540004
      WRITE (I02,80003) IVTNUM                                          01550004
      IF (ICZERO) 40220,  231, 40220                                    01560004
40220 IF (IVON02) 20220, 10220, 20220                                   01570004
10220 IVPASS = IVPASS + 1                                               01580004
      WRITE (I02,80001) IVTNUM                                          01590004
      GO TO  231                                                        01600004
20220 IVFAIL = IVFAIL + 1                                               01610004
      IVCOMP=IVON02                                                     01620004
      IVCORR= 0                                                         01630004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01640004
  231 CONTINUE                                                          01650004
      IVTNUM =  23                                                      01660004
C                                                                       01670004
C      ****  TEST 023  ****                                             01680004
C     TEST 23 - ARITHMETIC IF STATEMENT TEST                            01690004
C         GREATER THAN ZERO BRANCH EXPECTED                             01700004
C                                                                       01710004
      IF (ICZERO) 30230,  230, 30230                                    01720004
  230 CONTINUE                                                          01730004
      IVON01 = 4                                                        01740004
      IF (IVON01 - 3) 232,233,234                                       01750004
  232 IVON02 = -1                                                       01760004
      GO TO 40230                                                       01770004
  233 IVON02 = 0                                                        01780004
      GO TO 40230                                                       01790004
  234 IVON02 = 1                                                        01800004
      GO TO 40230                                                       01810004
30230 IVDELE = IVDELE + 1                                               01820004
      WRITE (I02,80003) IVTNUM                                          01830004
      IF (ICZERO) 40230,  241, 40230                                    01840004
40230 IF (IVON02) 20230, 20230, 10230                                   01850004
10230 IVPASS = IVPASS + 1                                               01860004
      WRITE (I02,80001) IVTNUM                                          01870004
      GO TO  241                                                        01880004
20230 IVFAIL = IVFAIL + 1                                               01890004
      IVCOMP=IVON02                                                     01900004
      IVCORR = 1                                                        01910004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01920004
C                                                                       01930004
C         TESTS 24 THROUGH 29 CONTAIN AN IF STATEMENT WITH TWO OF THE   01940004
C     THREE BRANCH STATEMENT LABELS EQUAL.                              01950004
C                                                                       01960004
  241 CONTINUE                                                          01970004
      IVTNUM =  24                                                      01980004
C                                                                       01990004
C      ****  TEST 024  ****                                             02000004
C     TEST 24 - ARITHMETIC IF STATEMENT TEST                            02010004
C         LESS THAN ZERO BRANCH EXPECTED                                02020004
C                                                                       02030004
      IF (ICZERO) 30240,  240, 30240                                    02040004
  240 CONTINUE                                                          02050004
      IVON01=2                                                          02060004
      IF (IVON01 - 3) 242,243,242                                       02070004
  242 IVON02=-1                                                         02080004
      GO TO 40240                                                       02090004
  243 IVON02=0                                                          02100004
      GO TO 40240                                                       02110004
30240 IVDELE = IVDELE + 1                                               02120004
      WRITE (I02,80003) IVTNUM                                          02130004
      IF (ICZERO) 40240,  251, 40240                                    02140004
40240 IF (IVON02) 10240, 20240, 20240                                   02150004
10240 IVPASS = IVPASS + 1                                               02160004
      WRITE (I02,80001) IVTNUM                                          02170004
      GO TO  251                                                        02180004
20240 IVFAIL = IVFAIL + 1                                               02190004
      IVCOMP=IVON02                                                     02200004
      IVCORR=-1                                                         02210004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02220004
  251 CONTINUE                                                          02230004
      IVTNUM =  25                                                      02240004
C                                                                       02250004
C      ****  TEST 025  ****                                             02260004
C     TEST 25 - ARITHMETIC IF STATEMENT TEST                            02270004
C         EQUAL TO ZERO BRANCH EXPECTED                                 02280004
C                                                                       02290004
      IF (ICZERO) 30250,  250, 30250                                    02300004
  250 CONTINUE                                                          02310004
      IVON01=3                                                          02320004
      IF (IVON01 - 3) 252,253,252                                       02330004
  252 IVON02= -1                                                        02340004
      GO TO 40250                                                       02350004
  253 IVON02 = 0                                                        02360004
      GO TO 40250                                                       02370004
30250 IVDELE = IVDELE + 1                                               02380004
      WRITE (I02,80003) IVTNUM                                          02390004
      IF (ICZERO) 40250,  261, 40250                                    02400004
40250 IF (IVON02) 20250,10250,20250                                     02410004
10250 IVPASS = IVPASS + 1                                               02420004
      WRITE (I02,80001) IVTNUM                                          02430004
      GO TO  261                                                        02440004
20250 IVFAIL = IVFAIL + 1                                               02450004
      IVCOMP=IVON02                                                     02460004
      IVCORR=0                                                          02470004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02480004
  261 CONTINUE                                                          02490004
      IVTNUM =  26                                                      02500004
C                                                                       02510004
C      ****  TEST 026  ****                                             02520004
C     TEST 26 - ARITHMETIC IF STATEMENT TEST                            02530004
C         GREATER THAN ZERO BRANCH EXPECTED                             02540004
C                                                                       02550004
      IF (ICZERO) 30260,  260, 30260                                    02560004
  260 CONTINUE                                                          02570004
      IVON01=4                                                          02580004
      IF (IVON01-3) 262, 263, 262                                       02590004
  262 IVON02= 1                                                         02600004
      GO TO 40260                                                       02610004
  263 IVON02 = 0                                                        02620004
      GO TO 40260                                                       02630004
30260 IVDELE = IVDELE + 1                                               02640004
      WRITE (I02,80003) IVTNUM                                          02650004
      IF (ICZERO) 40260,  271, 40260                                    02660004
40260 IF (IVON02) 20260, 20260, 10260                                   02670004
10260 IVPASS = IVPASS + 1                                               02680004
      WRITE (I02,80001) IVTNUM                                          02690004
      GO TO  271                                                        02700004
20260 IVFAIL = IVFAIL + 1                                               02710004
      IVCOMP=IVON02                                                     02720004
      IVCORR = 1                                                        02730004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02740004
  271 CONTINUE                                                          02750004
      IVTNUM =  27                                                      02760004
C                                                                       02770004
C      ****  TEST 027  ****                                             02780004
C     TEST 27 - ARITHMETIC IF STATEMENT TEST                            02790004
C         LESS THAN ZERO BRANCH EXPECTED                                02800004
C                                                                       02810004
      IF (ICZERO) 30270,  270, 30270                                    02820004
  270 CONTINUE                                                          02830004
      IVON01 = -4                                                       02840004
      IF (IVON01 + 3) 272, 272, 273                                     02850004
  272 IVON02= -1                                                        02860004
      GO TO 40270                                                       02870004
  273 IVON02 = 1                                                        02880004
      GO TO 40270                                                       02890004
30270 IVDELE = IVDELE + 1                                               02900004
      WRITE (I02,80003) IVTNUM                                          02910004
      IF (ICZERO) 40270,  281, 40270                                    02920004
40270 IF (IVON02) 10270, 20270, 20270                                   02930004
10270 IVPASS = IVPASS + 1                                               02940004
      WRITE (I02,80001) IVTNUM                                          02950004
      GO TO  281                                                        02960004
20270 IVFAIL = IVFAIL + 1                                               02970004
      IVCOMP=IVON02                                                     02980004
      IVCORR= -1                                                        02990004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03000004
  281 CONTINUE                                                          03010004
      IVTNUM =  28                                                      03020004
C                                                                       03030004
C      ****  TEST 028  ****                                             03040004
C     TEST 28 - ARITHMETIC IF STATEMENT TEST                            03050004
C         EQUAL TO ZERO BRANCH EXPECTED                                 03060004
C                                                                       03070004
      IF (ICZERO) 30280,  280, 30280                                    03080004
  280 CONTINUE                                                          03090004
      IVON01 = -3                                                       03100004
      IF (IVON01 + 3) 282, 282, 283                                     03110004
  282 IVON02 = 0                                                        03120004
      GO TO 40280                                                       03130004
  283 IVON02 = 1                                                        03140004
      GO TO 40280                                                       03150004
30280 IVDELE = IVDELE + 1                                               03160004
      WRITE (I02,80003) IVTNUM                                          03170004
      IF (ICZERO) 40280,  291, 40280                                    03180004
40280 IF (IVON02) 20280, 10280, 20280                                   03190004
10280 IVPASS = IVPASS + 1                                               03200004
      WRITE (I02,80001) IVTNUM                                          03210004
      GO TO  291                                                        03220004
20280 IVFAIL = IVFAIL + 1                                               03230004
      IVCOMP=IVON02                                                     03240004
      IVCORR= 0                                                         03250004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03260004
  291 CONTINUE                                                          03270004
      IVTNUM =  29                                                      03280004
C                                                                       03290004
C      ****  TEST 029  ****                                             03300004
C     TEST 29 - ARITHMETIC IF STATEMENT TEST                            03310004
C         GREATER THAN ZERO BRANCH EXPECTED                             03320004
C                                                                       03330004
      IF (ICZERO) 30290,  290, 30290                                    03340004
  290 CONTINUE                                                          03350004
      IVON01 = -2                                                       03360004
      IF (IVON01 + 3) 292,292,293                                       03370004
  292 IVON02 = -1                                                       03380004
      GO TO 40290                                                       03390004
  293 IVON02 = 1                                                        03400004
      GO TO 40290                                                       03410004
30290 IVDELE = IVDELE + 1                                               03420004
      WRITE (I02,80003) IVTNUM                                          03430004
      IF (ICZERO) 40290,  301, 40290                                    03440004
40290 IF (IVON02) 20290, 20290, 10290                                   03450004
10290 IVPASS = IVPASS + 1                                               03460004
      WRITE (I02,80001) IVTNUM                                          03470004
      GO TO  301                                                        03480004
20290 IVFAIL = IVFAIL + 1                                               03490004
      IVCOMP= IVON02                                                    03500004
      IVCORR = 1                                                        03510004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03520004
C                                                                       03530004
C         TESTS 30 AND 31 CONTAIN THE BASIC GO TO STATEMENT TESTS.      03540004
C                                                                       03550004
  301 CONTINUE                                                          03560004
      IVTNUM =  30                                                      03570004
C                                                                       03580004
C      ****  TEST 030  ****                                             03590004
C     TEST 30 - UNCONDITIONAL GO TO STATEMENT TEST                      03600004
C                                                                       03610004
      IF (ICZERO) 30300,  300, 30300                                    03620004
  300 CONTINUE                                                          03630004
      IVON01 = 1                                                        03640004
      GO TO 302                                                         03650004
  303 IVON01 = 2                                                        03660004
      GO TO 304                                                         03670004
  302 IVON01 = 3                                                        03680004
      GO TO 303                                                         03690004
  304 GO TO 40300                                                       03700004
30300 IVDELE = IVDELE + 1                                               03710004
      WRITE (I02,80003) IVTNUM                                          03720004
      IF (ICZERO) 40300,  311, 40300                                    03730004
40300 IF (IVON01 - 2) 20300,10300,20300                                 03740004
10300 IVPASS = IVPASS + 1                                               03750004
      WRITE (I02,80001) IVTNUM                                          03760004
      GO TO  311                                                        03770004
20300 IVFAIL = IVFAIL + 1                                               03780004
      IVCOMP = IVON01                                                   03790004
      IVCORR = 2                                                        03800004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03810004
  311 CONTINUE                                                          03820004
      IVTNUM =  31                                                      03830004
C                                                                       03840004
C      ****  TEST 031  ****                                             03850004
C     TEST 31 - UNCONDITIONAL GO TO STATEMENT TEST                      03860004
C                                                                       03870004
      IF (ICZERO) 30310,  310, 30310                                    03880004
  310 CONTINUE                                                          03890004
      IVON01 = 1                                                        03900004
      GO TO 316                                                         03910004
  313 GO TO 317                                                         03920004
  314 IVON01 = 3                                                        03930004
      GO TO 40310                                                       03940004
  315 GO TO 313                                                         03950004
  316 GO TO 315                                                         03960004
  317 GO TO 314                                                         03970004
30310 IVDELE = IVDELE + 1                                               03980004
      WRITE (I02,80003) IVTNUM                                          03990004
      IF (ICZERO) 40310,  321, 40310                                    04000004
40310 IF (IVON01 - 3) 20310, 10310, 20310                               04010004
10310 IVPASS = IVPASS + 1                                               04020004
      WRITE (I02,80001) IVTNUM                                          04030004
      GO TO  321                                                        04040004
20310 IVFAIL = IVFAIL + 1                                               04050004
      IVCOMP=IVON01                                                     04060004
      IVCORR = 3                                                        04070004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04080004
  321 CONTINUE                                                          04090004
      IVTNUM =  32                                                      04100004
C                                                                       04110004
C      ****  TEST 032  ****                                             04120004
C         TEST 32 - ARITHMETIC IF STATEMENT AND UNCONDITIONAL GO TO     04130004
C                   STATEMENT                                           04140004
C     THIS TEST COMBINES THE BASIC ARITHMETIC IF STATEMENTS AND         04150004
C     UNCONDITIONAL GO TO STATEMENTS IN ONE TEST.                       04160004
C                                                                       04170004
      IF (ICZERO) 30320,  320, 30320                                    04180004
  320 CONTINUE                                                          04190004
      IVON01 = 1                                                        04200004
      GO TO 322                                                         04210004
  324 IVON01 = 2                                                        04220004
      IF (IVON01 -1) 323, 323, 325                                      04230004
  327 IVON01 = 5                                                        04240004
      GO TO 328                                                         04250004
  326 IVON01 = -4                                                       04260004
      IF (IVON01 + 4) 323, 327, 323                                     04270004
  322 IF (IVON01 - 1) 323, 324, 323                                     04280004
  323 GO TO 20320                                                       04290004
  325 IVON01 = 3                                                        04300004
      IF (IVON01 -4) 326,323,323                                        04310004
  328 GO TO 40320                                                       04320004
30320 IVDELE = IVDELE + 1                                               04330004
      WRITE (I02,80003) IVTNUM                                          04340004
      IF (ICZERO) 40320,  331, 40320                                    04350004
40320 IF (IVON01 - 5) 20320, 10320, 20320                               04360004
10320 IVPASS = IVPASS + 1                                               04370004
      WRITE (I02,80001) IVTNUM                                          04380004
      GO TO  331                                                        04390004
20320 IVFAIL = IVFAIL + 1                                               04400004
      IVCOMP=IVON01                                                     04410004
      IVCORR=5                                                          04420004
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04430004
  331 CONTINUE                                                          04440004
C                                                                       04450004
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             04460004
99999 CONTINUE                                                          04470004
      WRITE (I02,90002)                                                 04480004
      WRITE (I02,90006)                                                 04490004
      WRITE (I02,90002)                                                 04500004
      WRITE (I02,90002)                                                 04510004
      WRITE (I02,90007)                                                 04520004
      WRITE (I02,90002)                                                 04530004
      WRITE (I02,90008)  IVFAIL                                         04540004
      WRITE (I02,90009) IVPASS                                          04550004
      WRITE (I02,90010) IVDELE                                          04560004
C                                                                       04570004
C                                                                       04580004
C     TERMINATE ROUTINE EXECUTION                                       04590004
      STOP                                                              04600004
C                                                                       04610004
C     FORMAT STATEMENTS FOR PAGE HEADERS                                04620004
90000 FORMAT (1H1)                                                      04630004
90002 FORMAT (1H )                                                      04640004
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04650004
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   04660004
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        04670004
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 04680004
90006 FORMAT (1H ,5X,46H----------------------------------------------) 04690004
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             04700004
C                                                                       04710004
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               04720004
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        04730004
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              04740004
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             04750004
C                                                                       04760004
C     FORMAT STATEMENTS FOR TEST RESULTS                                04770004
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04780004
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      04790004
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   04800004
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04810004
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    04820004
C                                                                       04830004
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM004)                          04840004
      END                                                               04850004
