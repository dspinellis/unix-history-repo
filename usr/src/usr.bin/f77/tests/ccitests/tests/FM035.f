C     COMMENT SECTION                                                   00010035
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020035
C     FM035                                                             00030035
C                                                                       00040035
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050035
C     FORM                                                              00060035
C              INTEGER VARIABLE = ARITHMETIC EXPRESSION                 00070035
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080035
C     OPERATOR *, INTEGER VARIABLES AND INTEGER CONSTANT.  SOME OF THE  00090035
C     TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE EXPRESSION AND TO  00100035
C     ALLOW THE USE OF NEGATIVE CONSTANTS FOLLOWING THE * OPERATOR.     00110035
C     THE INTEGER VARIABLES CONTAIN POSITIVE AND NEGATIVE VALUES.       00120035
C                                                                       00130035
C     THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS          00140035
C         (1)  INTEGER VARIABLE * INTEGER VARIABLE                      00150035
C         (2)  INTEGER VARIABLE * INTEGER VARIABLE * INTEGER CONSTANT   00160035
C              INTEGER VARIABLE * INTEGER CONSTANT * INTEGER VARIABLE   00170035
C              INTEGER CONSTANT * INTEGER VARIABLE * INTEGER VARIABLE   00180035
C         (3)  SAME AS (2) BUT WITH PARENTHESES TO GROUP ELEMENTS.      00190035
C                                                                       00200035
C      REFERENCES                                                       00210035
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00220035
C              X3.9-1978                                                00230035
C                                                                       00240035
C        SECTION 4.3, INTEGER TYPE                                      00250035
C        SECTION 4.3.1, INTEGER CONSTANT                                00260035
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00270035
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00280035
C                                                                       00290035
C      **********************************************************       00300035
C                                                                       00310035
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00320035
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00330035
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00340035
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00350035
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00360035
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00370035
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00380035
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00390035
C     OF EXECUTING THESE TESTS.                                         00400035
C                                                                       00410035
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00420035
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00430035
C                                                                       00440035
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00450035
C                                                                       00460035
C                  DEPARTMENT OF THE NAVY                               00470035
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00480035
C                  WASHINGTON, D.C.  20376                              00490035
C                                                                       00500035
C      **********************************************************       00510035
C                                                                       00520035
C                                                                       00530035
C                                                                       00540035
C     INITIALIZATION SECTION                                            00550035
C                                                                       00560035
C     INITIALIZE CONSTANTS                                              00570035
C      **************                                                   00580035
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00590035
      I01 = 5                                                           00600035
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00610035
      I02 = 6                                                           00620035
C     SYSTEM ENVIRONMENT SECTION                                        00630035
C                                                                       00640035
      I01 = 5                                                           00650035
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00660035
C     (UNIT NUMBER FOR CARD READER).                                    00670035
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00680035
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00690035
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00700035
C                                                                       00710035
      I02 = 6                                                           00720035
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00730035
C     (UNIT NUMBER FOR PRINTER).                                        00740035
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00750035
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760035
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00770035
C                                                                       00780035
      IVPASS=0                                                          00790035
      IVFAIL=0                                                          00800035
      IVDELE=0                                                          00810035
      ICZERO=0                                                          00820035
C                                                                       00830035
C     WRITE PAGE HEADERS                                                00840035
      WRITE (I02,90000)                                                 00850035
      WRITE (I02,90001)                                                 00860035
      WRITE (I02,90002)                                                 00870035
      WRITE (I02, 90002)                                                00880035
      WRITE (I02,90003)                                                 00890035
      WRITE (I02,90002)                                                 00900035
      WRITE (I02,90004)                                                 00910035
      WRITE (I02,90002)                                                 00920035
      WRITE (I02,90011)                                                 00930035
      WRITE (I02,90002)                                                 00940035
      WRITE (I02,90002)                                                 00950035
      WRITE (I02,90005)                                                 00960035
      WRITE (I02,90006)                                                 00970035
      WRITE (I02,90002)                                                 00980035
C                                                                       00990035
C     TEST SECTION                                                      01000035
C                                                                       01010035
C         ARITHMETIC ASSIGNMENT STATEMENT                               01020035
C                                                                       01030035
C     TEST 430 THROUGH TEST 441 CONTAIN TWO INTEGER VARIABLES AND       01040035
C     OPERATOR * IN AN ARITHMETIC EXPRESSION.                           01050035
C         THE FORM IS   IV = IV * IV                                    01060035
C                                                                       01070035
C     TEST 430 THROUGH TEST 433  -  TWO POSITIVE VARIABLES              01080035
C                                                                       01090035
 4301 CONTINUE                                                          01100035
      IVTNUM = 430                                                      01110035
C                                                                       01120035
C      ****  TEST 430  ****                                             01130035
C                                                                       01140035
      IF (ICZERO) 34300, 4300, 34300                                    01150035
 4300 CONTINUE                                                          01160035
      IVON01 = 2                                                        01170035
      IVON02 = 3                                                        01180035
      IVCOMP = IVON01 * IVON02                                          01190035
      GO TO 44300                                                       01200035
34300 IVDELE = IVDELE + 1                                               01210035
      WRITE (I02,80003) IVTNUM                                          01220035
      IF (ICZERO) 44300, 4311, 44300                                    01230035
44300 IF (IVCOMP - 6) 24300,14300,24300                                 01240035
14300 IVPASS = IVPASS + 1                                               01250035
      WRITE (I02,80001) IVTNUM                                          01260035
      GO TO 4311                                                        01270035
24300 IVFAIL = IVFAIL + 1                                               01280035
      IVCORR = 6                                                        01290035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01300035
 4311 CONTINUE                                                          01310035
      IVTNUM = 431                                                      01320035
C                                                                       01330035
C      ****  TEST 431  ****                                             01340035
C                                                                       01350035
      IF (ICZERO) 34310, 4310, 34310                                    01360035
 4310 CONTINUE                                                          01370035
      IVON01 = 13                                                       01380035
      IVON02 = 11                                                       01390035
      IVCOMP = IVON01 * IVON02                                          01400035
      GO TO 44310                                                       01410035
34310 IVDELE = IVDELE + 1                                               01420035
      WRITE (I02,80003) IVTNUM                                          01430035
      IF (ICZERO) 44310, 4321, 44310                                    01440035
44310 IF (IVCOMP - 143) 24310,14310,24310                               01450035
14310 IVPASS = IVPASS + 1                                               01460035
      WRITE (I02,80001) IVTNUM                                          01470035
      GO TO 4321                                                        01480035
24310 IVFAIL = IVFAIL + 1                                               01490035
      IVCORR = 143                                                      01500035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01510035
 4321 CONTINUE                                                          01520035
      IVTNUM = 432                                                      01530035
C                                                                       01540035
C      ****  TEST 432  ****                                             01550035
C                                                                       01560035
      IF (ICZERO) 34320, 4320, 34320                                    01570035
 4320 CONTINUE                                                          01580035
      IVON01 = 223                                                      01590035
      IVON02 = 99                                                       01600035
      IVCOMP = IVON01 * IVON02                                          01610035
      GO TO 44320                                                       01620035
34320 IVDELE = IVDELE + 1                                               01630035
      WRITE (I02,80003) IVTNUM                                          01640035
      IF (ICZERO) 44320, 4331, 44320                                    01650035
44320 IF (IVCOMP - 22077) 24320,14320,24320                             01660035
14320 IVPASS = IVPASS + 1                                               01670035
      WRITE (I02,80001) IVTNUM                                          01680035
      GO TO 4331                                                        01690035
24320 IVFAIL = IVFAIL + 1                                               01700035
      IVCORR = 22077                                                    01710035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01720035
 4331 CONTINUE                                                          01730035
      IVTNUM = 433                                                      01740035
C                                                                       01750035
C      ****  TEST 433  ****                                             01760035
C                                                                       01770035
      IF (ICZERO) 34330, 4330, 34330                                    01780035
 4330 CONTINUE                                                          01790035
      IVON01 = 11235                                                    01800035
      IVON02 = 2                                                        01810035
      IVCOMP = IVON01*IVON02                                            01820035
      GO TO 44330                                                       01830035
34330 IVDELE = IVDELE + 1                                               01840035
      WRITE (I02,80003) IVTNUM                                          01850035
      IF (ICZERO) 44330, 4341, 44330                                    01860035
44330 IF (IVCOMP - 22470) 24330,14330,24330                             01870035
14330 IVPASS = IVPASS + 1                                               01880035
      WRITE (I02,80001) IVTNUM                                          01890035
      GO TO 4341                                                        01900035
24330 IVFAIL = IVFAIL + 1                                               01910035
      IVCORR = 22470                                                    01920035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01930035
C                                                                       01940035
C     TEST 434 THROUGH TEST 437                                         01950035
C          ONE NEGATIVE VARIABLE, ONE POSITIVE VARIABLE                 01960035
C                                                                       01970035
 4341 CONTINUE                                                          01980035
      IVTNUM = 434                                                      01990035
C                                                                       02000035
C      ****  TEST 434  ****                                             02010035
C                                                                       02020035
      IF (ICZERO) 34340, 4340, 34340                                    02030035
 4340 CONTINUE                                                          02040035
      IVON01 = -2                                                       02050035
      IVON02 = 3                                                        02060035
      IVCOMP = IVON01 * IVON02                                          02070035
      GO TO 44340                                                       02080035
34340 IVDELE = IVDELE + 1                                               02090035
      WRITE (I02,80003) IVTNUM                                          02100035
      IF (ICZERO) 44340, 4351, 44340                                    02110035
44340 IF (IVCOMP +6) 24340,14340,24340                                  02120035
14340 IVPASS = IVPASS + 1                                               02130035
      WRITE (I02,80001) IVTNUM                                          02140035
      GO TO 4351                                                        02150035
24340 IVFAIL = IVFAIL + 1                                               02160035
      IVCORR = -6                                                       02170035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02180035
 4351 CONTINUE                                                          02190035
      IVTNUM = 435                                                      02200035
C                                                                       02210035
C      ****  TEST 435  ****                                             02220035
C                                                                       02230035
      IF (ICZERO) 34350, 4350, 34350                                    02240035
 4350 CONTINUE                                                          02250035
      IVON01 = -13                                                      02260035
      IVON02 = +11                                                      02270035
      IVCOMP = IVON01*IVON02                                            02280035
      GO TO 44350                                                       02290035
34350 IVDELE = IVDELE + 1                                               02300035
      WRITE (I02,80003) IVTNUM                                          02310035
      IF (ICZERO) 44350, 4361, 44350                                    02320035
44350 IF (IVCOMP + 143) 24350,14350,24350                               02330035
14350 IVPASS = IVPASS + 1                                               02340035
      WRITE (I02,80001) IVTNUM                                          02350035
      GO TO 4361                                                        02360035
24350 IVFAIL = IVFAIL + 1                                               02370035
      IVCORR = -143                                                     02380035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02390035
 4361 CONTINUE                                                          02400035
      IVTNUM = 436                                                      02410035
C                                                                       02420035
C      ****  TEST 436  ****                                             02430035
C                                                                       02440035
      IF (ICZERO) 34360, 4360, 34360                                    02450035
 4360 CONTINUE                                                          02460035
      IVON01 = -223                                                     02470035
      IVON02 = 99                                                       02480035
      IVCOMP = IVON01 * IVON02                                          02490035
      GO TO 44360                                                       02500035
34360 IVDELE = IVDELE + 1                                               02510035
      WRITE (I02,80003) IVTNUM                                          02520035
      IF (ICZERO) 44360, 4371, 44360                                    02530035
44360 IF (IVCOMP + 22077) 24360,14360,24360                             02540035
14360 IVPASS = IVPASS + 1                                               02550035
      WRITE (I02,80001) IVTNUM                                          02560035
      GO TO 4371                                                        02570035
24360 IVFAIL = IVFAIL + 1                                               02580035
      IVCORR = -22077                                                   02590035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02600035
 4371 CONTINUE                                                          02610035
      IVTNUM = 437                                                      02620035
C                                                                       02630035
C      ****  TEST 437  ****                                             02640035
C                                                                       02650035
      IF (ICZERO) 34370, 4370, 34370                                    02660035
 4370 CONTINUE                                                          02670035
      IVON01 = -11235                                                   02680035
      IVON02 =  2                                                       02690035
      IVCOMP = IVON01 * IVON02                                          02700035
      GO TO 44370                                                       02710035
34370 IVDELE = IVDELE + 1                                               02720035
      WRITE (I02,80003) IVTNUM                                          02730035
      IF (ICZERO) 44370, 4381, 44370                                    02740035
44370 IF (IVCOMP + 22470) 24370,14370,24370                             02750035
14370 IVPASS = IVPASS + 1                                               02760035
      WRITE (I02,80001) IVTNUM                                          02770035
      GO TO 4381                                                        02780035
24370 IVFAIL = IVFAIL + 1                                               02790035
      IVCORR = -22470                                                   02800035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02810035
C                                                                       02820035
C     TEST 438 THROUGH TEST 441  -  TWO NEGATIVE VARIABLES              02830035
 4381 CONTINUE                                                          02840035
      IVTNUM = 438                                                      02850035
C                                                                       02860035
C      ****  TEST 438  ****                                             02870035
C                                                                       02880035
      IF (ICZERO) 34380, 4380, 34380                                    02890035
 4380 CONTINUE                                                          02900035
      IVON01 = -2                                                       02910035
      IVON02 = -3                                                       02920035
      IVCOMP = IVON01 * IVON02                                          02930035
      GO TO 44380                                                       02940035
34380 IVDELE = IVDELE + 1                                               02950035
      WRITE (I02,80003) IVTNUM                                          02960035
      IF (ICZERO) 44380, 4391, 44380                                    02970035
44380 IF (IVCOMP - 6) 24380,14380,24380                                 02980035
14380 IVPASS = IVPASS + 1                                               02990035
      WRITE (I02,80001) IVTNUM                                          03000035
      GO TO 4391                                                        03010035
24380 IVFAIL = IVFAIL + 1                                               03020035
      IVCORR = 6                                                        03030035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03040035
 4391 CONTINUE                                                          03050035
      IVTNUM = 439                                                      03060035
C                                                                       03070035
C      ****  TEST 439  ****                                             03080035
C                                                                       03090035
      IF (ICZERO) 34390, 4390, 34390                                    03100035
 4390 CONTINUE                                                          03110035
      IVON01 = -13                                                      03120035
      IVON02 = -11                                                      03130035
      IVCOMP = IVON01 * IVON02                                          03140035
      GO TO 44390                                                       03150035
34390 IVDELE = IVDELE + 1                                               03160035
      WRITE (I02,80003) IVTNUM                                          03170035
      IF (ICZERO) 44390, 4401, 44390                                    03180035
44390 IF (IVCOMP - 143) 24390,14390,24390                               03190035
14390 IVPASS = IVPASS + 1                                               03200035
      WRITE (I02,80001) IVTNUM                                          03210035
      GO TO 4401                                                        03220035
24390 IVFAIL = IVFAIL + 1                                               03230035
      IVCORR = 143                                                      03240035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03250035
 4401 CONTINUE                                                          03260035
      IVTNUM = 440                                                      03270035
C                                                                       03280035
C      ****  TEST 440  ****                                             03290035
C                                                                       03300035
      IF (ICZERO) 34400, 4400, 34400                                    03310035
 4400 CONTINUE                                                          03320035
      IVON01 = -223                                                     03330035
      IVON02 = -99                                                      03340035
      IVCOMP = IVON01*IVON02                                            03350035
      GO TO 44400                                                       03360035
34400 IVDELE = IVDELE + 1                                               03370035
      WRITE (I02,80003) IVTNUM                                          03380035
      IF (ICZERO) 44400, 4411, 44400                                    03390035
44400 IF (IVCOMP - 22077) 24400,14400,24400                             03400035
14400 IVPASS = IVPASS + 1                                               03410035
      WRITE (I02,80001) IVTNUM                                          03420035
      GO TO 4411                                                        03430035
24400 IVFAIL = IVFAIL + 1                                               03440035
      IVCORR = 22077                                                    03450035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03460035
 4411 CONTINUE                                                          03470035
      IVTNUM = 441                                                      03480035
C                                                                       03490035
C      ****  TEST 441  ****                                             03500035
C                                                                       03510035
      IF (ICZERO) 34410, 4410, 34410                                    03520035
 4410 CONTINUE                                                          03530035
      IVON01 = -5461                                                    03540035
      IVON02 = -6                                                       03550035
      IVCOMP = IVON01 * IVON02                                          03560035
      GO TO 44410                                                       03570035
34410 IVDELE = IVDELE + 1                                               03580035
      WRITE (I02,80003) IVTNUM                                          03590035
      IF (ICZERO) 44410, 4421, 44410                                    03600035
44410 IF (IVCOMP - 32766) 24410, 14410, 24410                           03610035
14410 IVPASS = IVPASS + 1                                               03620035
      WRITE (I02,80001) IVTNUM                                          03630035
      GO TO 4421                                                        03640035
24410 IVFAIL = IVFAIL + 1                                               03650035
      IVCORR = 32766                                                    03660035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03670035
C                                                                       03680035
C     TEST 442 THROUGH TEST 445 CONTAIN SIGNED INTEGER VARIABLES AND    03690035
C     OPERATOR * IN AN ARITHMETIC EXPRESSION.                           03700035
 4421 CONTINUE                                                          03710035
      IVTNUM = 442                                                      03720035
C                                                                       03730035
C      ****  TEST 442  ****                                             03740035
C        FORM IS  IV = -IV*IV                                           03750035
C                                                                       03760035
      IF (ICZERO) 34420, 4420, 34420                                    03770035
 4420 CONTINUE                                                          03780035
      IVON01 = 2                                                        03790035
      IVON02 = 3                                                        03800035
      IVCOMP = -IVON01 * IVON02                                         03810035
      GO TO 44420                                                       03820035
34420 IVDELE = IVDELE + 1                                               03830035
      WRITE (I02,80003) IVTNUM                                          03840035
      IF (ICZERO) 44420, 4431, 44420                                    03850035
44420 IF (IVCOMP + 6) 24420,14420,24420                                 03860035
14420 IVPASS = IVPASS + 1                                               03870035
      WRITE (I02,80001) IVTNUM                                          03880035
      GO TO 4431                                                        03890035
24420 IVFAIL = IVFAIL + 1                                               03900035
      IVCORR = -6                                                       03910035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03920035
 4431 CONTINUE                                                          03930035
      IVTNUM = 443                                                      03940035
C                                                                       03950035
C      ****  TEST 443  ****                                             03960035
C        FORM IS  IV = IV*(-IV)                                         03970035
C                                                                       03980035
      IF (ICZERO) 34430, 4430, 34430                                    03990035
 4430 CONTINUE                                                          04000035
      IVON01 = 2                                                        04010035
      IVON02 = 3                                                        04020035
      IVCOMP = IVON01 * (-IVON02)                                       04030035
      GO TO 44430                                                       04040035
34430 IVDELE = IVDELE + 1                                               04050035
      WRITE (I02,80003) IVTNUM                                          04060035
      IF (ICZERO) 44430, 4441, 44430                                    04070035
44430 IF (IVCOMP +6) 24430,14430,24430                                  04080035
14430 IVPASS = IVPASS + 1                                               04090035
      WRITE (I02,80001) IVTNUM                                          04100035
      GO TO 4441                                                        04110035
24430 IVFAIL = IVFAIL + 1                                               04120035
      IVCORR = -6                                                       04130035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04140035
 4441 CONTINUE                                                          04150035
      IVTNUM = 444                                                      04160035
C                                                                       04170035
C      ****  TEST 444  ****                                             04180035
C        FORM IS  IV = (-IV)*(-IV)                                      04190035
C                                                                       04200035
      IF (ICZERO) 34440, 4440, 34440                                    04210035
 4440 CONTINUE                                                          04220035
      IVON01 = 2                                                        04230035
      IVON02 = 3                                                        04240035
      IVCOMP = (-IVON01) * (-IVON02)                                    04250035
      GO TO 44440                                                       04260035
34440 IVDELE = IVDELE + 1                                               04270035
      WRITE (I02,80003) IVTNUM                                          04280035
      IF (ICZERO) 44440, 4451, 44440                                    04290035
44440 IF (IVCOMP - 6) 24440,14440,24440                                 04300035
14440 IVPASS = IVPASS + 1                                               04310035
      WRITE (I02,80001) IVTNUM                                          04320035
      GO TO 4451                                                        04330035
24440 IVFAIL = IVFAIL + 1                                               04340035
      IVCORR =  6                                                       04350035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04360035
 4451 CONTINUE                                                          04370035
      IVTNUM = 445                                                      04380035
C                                                                       04390035
C      ****  TEST 445  ****                                             04400035
C        FORM IS   IV = -IV * IV                                        04410035
C                                                                       04420035
      IF (ICZERO) 34450, 4450, 34450                                    04430035
 4450 CONTINUE                                                          04440035
      IVON01 = -11235                                                   04450035
      IVON02 =  -2                                                      04460035
      IVCOMP = -IVON01 * IVON02                                         04470035
      GO TO 44450                                                       04480035
34450 IVDELE = IVDELE + 1                                               04490035
      WRITE (I02,80003) IVTNUM                                          04500035
      IF (ICZERO) 44450, 4461, 44450                                    04510035
44450 IF (IVCOMP + 22470) 24450,14450,24450                             04520035
14450 IVPASS = IVPASS + 1                                               04530035
      WRITE (I02,80001) IVTNUM                                          04540035
      GO TO 4461                                                        04550035
24450 IVFAIL = IVFAIL + 1                                               04560035
      IVCORR = -22470                                                   04570035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04580035
C                                                                       04590035
C     TEST 446 THROUGH TEST 452 CONTAIN TWO INTEGER VARIABLES, AN       04600035
C     INTEGER CONSTANT AND OPERATOR * IN AN ARITHMETIC EXPRESSION.      04610035
C                                                                       04620035
 4461 CONTINUE                                                          04630035
      IVTNUM = 446                                                      04640035
C                                                                       04650035
C      ****  TEST 446  ****                                             04660035
C                                                                       04670035
      IF (ICZERO) 34460, 4460, 34460                                    04680035
 4460 CONTINUE                                                          04690035
      IVON01 = 2                                                        04700035
      IVON02 = 3                                                        04710035
      IVCOMP = IVON01 * IVON02 * 4                                      04720035
      GO TO 44460                                                       04730035
34460 IVDELE = IVDELE + 1                                               04740035
      WRITE (I02,80003) IVTNUM                                          04750035
      IF (ICZERO) 44460, 4471, 44460                                    04760035
44460 IF (IVCOMP -24) 24460,14460,24460                                 04770035
14460 IVPASS = IVPASS + 1                                               04780035
      WRITE (I02,80001) IVTNUM                                          04790035
      GO TO 4471                                                        04800035
24460 IVFAIL = IVFAIL + 1                                               04810035
      IVCORR = 24                                                       04820035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04830035
 4471 CONTINUE                                                          04840035
      IVTNUM = 447                                                      04850035
C                                                                       04860035
C      ****  TEST 447  ****                                             04870035
C                                                                       04880035
      IF (ICZERO) 34470, 4470, 34470                                    04890035
 4470 CONTINUE                                                          04900035
      IVON01 = -2                                                       04910035
      IVON02 = 3                                                        04920035
      IVCOMP = IVON01 * IVON02 * 4                                      04930035
      GO TO 44470                                                       04940035
34470 IVDELE = IVDELE + 1                                               04950035
      WRITE (I02,80003) IVTNUM                                          04960035
      IF (ICZERO) 44470, 4481, 44470                                    04970035
44470 IF (IVCOMP +24) 24470,14470,24470                                 04980035
14470 IVPASS = IVPASS + 1                                               04990035
      WRITE (I02,80001) IVTNUM                                          05000035
      GO TO 4481                                                        05010035
24470 IVFAIL = IVFAIL + 1                                               05020035
      IVCORR = -24                                                      05030035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05040035
 4481 CONTINUE                                                          05050035
      IVTNUM = 448                                                      05060035
C                                                                       05070035
C      ****  TEST 448  ****                                             05080035
C                                                                       05090035
      IF (ICZERO) 34480, 4480, 34480                                    05100035
 4480 CONTINUE                                                          05110035
      IVON01 = -2                                                       05120035
      IVON02 = 3                                                        05130035
      IVCOMP = IVON01 * IVON02 * (-4)                                   05140035
      GO TO 44480                                                       05150035
34480 IVDELE = IVDELE + 1                                               05160035
      WRITE (I02,80003) IVTNUM                                          05170035
      IF (ICZERO) 44480, 4491, 44480                                    05180035
44480 IF (IVCOMP -24) 24480,14480,24480                                 05190035
14480 IVPASS = IVPASS + 1                                               05200035
      WRITE (I02,80001) IVTNUM                                          05210035
      GO TO 4491                                                        05220035
24480 IVFAIL = IVFAIL + 1                                               05230035
      IVCORR = 24                                                       05240035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05250035
 4491 CONTINUE                                                          05260035
      IVTNUM = 449                                                      05270035
C                                                                       05280035
C      ****  TEST 449  ****                                             05290035
C                                                                       05300035
      IF (ICZERO) 34490, 4490, 34490                                    05310035
 4490 CONTINUE                                                          05320035
      IVON01 = 51                                                       05330035
      IVON03 = 13                                                       05340035
      IVCOMP = IVON01 * 23 * IVON03                                     05350035
      GO TO 44490                                                       05360035
34490 IVDELE = IVDELE + 1                                               05370035
      WRITE (I02,80003) IVTNUM                                          05380035
      IF (ICZERO) 44490, 4501, 44490                                    05390035
44490 IF (IVCOMP - 15249) 24490,14490,24490                             05400035
14490 IVPASS = IVPASS + 1                                               05410035
      WRITE (I02,80001) IVTNUM                                          05420035
      GO TO 4501                                                        05430035
24490 IVFAIL = IVFAIL + 1                                               05440035
      IVCORR = 15249                                                    05450035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05460035
 4501 CONTINUE                                                          05470035
      IVTNUM = 450                                                      05480035
C                                                                       05490035
C      ****  TEST 450  ****                                             05500035
C                                                                       05510035
      IF (ICZERO) 34500, 4500, 34500                                    05520035
 4500 CONTINUE                                                          05530035
      IVON02 = 2                                                        05540035
      IVON03 = 5461                                                     05550035
      IVCOMP = 3 * IVON02 * IVON03                                      05560035
      GO TO 44500                                                       05570035
34500 IVDELE = IVDELE + 1                                               05580035
      WRITE (I02,80003) IVTNUM                                          05590035
      IF (ICZERO) 44500, 4511, 44500                                    05600035
44500 IF (IVCOMP -32766) 24500,14500,24500                              05610035
14500 IVPASS = IVPASS + 1                                               05620035
      WRITE (I02,80001) IVTNUM                                          05630035
      GO TO 4511                                                        05640035
24500 IVFAIL = IVFAIL + 1                                               05650035
      IVCORR = 32766                                                    05660035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05670035
 4511 CONTINUE                                                          05680035
      IVTNUM = 451                                                      05690035
C                                                                       05700035
C      ****  TEST 451  ****                                             05710035
C                                                                       05720035
      IF (ICZERO) 34510, 4510, 34510                                    05730035
 4510 CONTINUE                                                          05740035
      IVON01 = -51                                                      05750035
      IVON03 = 13                                                       05760035
      IVCOMP = IVON01 * 23 * (-IVON03)                                  05770035
      GO TO 44510                                                       05780035
34510 IVDELE = IVDELE + 1                                               05790035
      WRITE (I02,80003) IVTNUM                                          05800035
      IF (ICZERO) 44510, 4521, 44510                                    05810035
44510 IF (IVCOMP - 15249) 24510,14510,24510                             05820035
14510 IVPASS = IVPASS + 1                                               05830035
      WRITE (I02,80001) IVTNUM                                          05840035
      GO TO 4521                                                        05850035
24510 IVFAIL = IVFAIL + 1                                               05860035
      IVCORR = 15249                                                    05870035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05880035
 4521 CONTINUE                                                          05890035
      IVTNUM = 452                                                      05900035
C                                                                       05910035
C      ****  TEST 452  ****                                             05920035
C                                                                       05930035
      IF (ICZERO) 34520, 4520, 34520                                    05940035
 4520 CONTINUE                                                          05950035
      IVON01 = -5461                                                    05960035
      IVON03 = 2                                                        05970035
      IVCOMP = IVON01 * (-3) * IVON03                                   05980035
      GO TO 44520                                                       05990035
34520 IVDELE = IVDELE + 1                                               06000035
      WRITE (I02,80003) IVTNUM                                          06010035
      IF (ICZERO) 44520, 4531, 44520                                    06020035
44520 IF (IVCOMP - 32766) 24520,14520,24520                             06030035
14520 IVPASS = IVPASS + 1                                               06040035
      WRITE (I02,80001) IVTNUM                                          06050035
      GO TO 4531                                                        06060035
24520 IVFAIL = IVFAIL + 1                                               06070035
      IVCORR = 32766                                                    06080035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06090035
C                                                                       06100035
C     TEST 453 THROUGH TEST 461 CONTAIN TWO INTEGER VARIABLES AND ONE   06110035
C     INTEGER CONSTANT IN AN ARITHMETIC EXPRESSION.  PARENTHESES ARE    06120035
C     USED TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSIONS IN THESE     06130035
C     TESTS.                                                            06140035
C                                                                       06150035
 4531 CONTINUE                                                          06160035
      IVTNUM = 453                                                      06170035
C                                                                       06180035
C      ****  TEST 453  ****                                             06190035
C                                                                       06200035
      IF (ICZERO) 34530, 4530, 34530                                    06210035
 4530 CONTINUE                                                          06220035
      IVON01 = 2                                                        06230035
      IVON02 = 3                                                        06240035
      IVCOMP = IVON01 * (IVON02 * 4)                                    06250035
      GO TO 44530                                                       06260035
34530 IVDELE = IVDELE + 1                                               06270035
      WRITE (I02,80003) IVTNUM                                          06280035
      IF (ICZERO) 44530, 4541, 44530                                    06290035
44530 IF (IVCOMP - 24) 24530,14530,24530                                06300035
14530 IVPASS = IVPASS + 1                                               06310035
      WRITE (I02,80001) IVTNUM                                          06320035
      GO TO 4541                                                        06330035
24530 IVFAIL = IVFAIL + 1                                               06340035
      IVCORR = 24                                                       06350035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06360035
 4541 CONTINUE                                                          06370035
      IVTNUM = 454                                                      06380035
C                                                                       06390035
C      ****  TEST 454  ****                                             06400035
C                                                                       06410035
      IF (ICZERO) 34540, 4540, 34540                                    06420035
 4540 CONTINUE                                                          06430035
      IVON01 = 2                                                        06440035
      IVON02 = 3                                                        06450035
      IVCOMP = (IVON01 * IVON02) * 4                                    06460035
      GO TO 44540                                                       06470035
34540 IVDELE = IVDELE + 1                                               06480035
      WRITE (I02,80003) IVTNUM                                          06490035
      IF (ICZERO) 44540, 4551, 44540                                    06500035
44540 IF (IVCOMP -24) 24540,14540,24540                                 06510035
14540 IVPASS = IVPASS + 1                                               06520035
      WRITE (I02,80001) IVTNUM                                          06530035
      GO TO 4551                                                        06540035
24540 IVFAIL = IVFAIL + 1                                               06550035
      IVCORR = 24                                                       06560035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06570035
 4551 CONTINUE                                                          06580035
      IVTNUM = 455                                                      06590035
C                                                                       06600035
C      ****  TEST 455  ****                                             06610035
C                                                                       06620035
      IF (ICZERO) 34550, 4550, 34550                                    06630035
 4550 CONTINUE                                                          06640035
      IVON01 = -2                                                       06650035
      IVON02 = 3                                                        06660035
      IVCOMP = IVON01 *(IVON02 * (-4))                                  06670035
      GO TO 44550                                                       06680035
34550 IVDELE = IVDELE + 1                                               06690035
      WRITE (I02,80003) IVTNUM                                          06700035
      IF (ICZERO) 44550, 4561, 44550                                    06710035
44550 IF (IVCOMP - 24) 24550,14550,24550                                06720035
14550 IVPASS = IVPASS + 1                                               06730035
      WRITE (I02,80001) IVTNUM                                          06740035
      GO TO 4561                                                        06750035
24550 IVFAIL = IVFAIL + 1                                               06760035
      IVCORR = 24                                                       06770035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06780035
 4561 CONTINUE                                                          06790035
      IVTNUM = 456                                                      06800035
C                                                                       06810035
C      ****  TEST 456  ****                                             06820035
C                                                                       06830035
      IF (ICZERO) 34560, 4560, 34560                                    06840035
 4560 CONTINUE                                                          06850035
      IVON01 = -2                                                       06860035
      IVON02 = -3                                                       06870035
      IVCOMP = IVON01 * (IVON02 * 4)                                    06880035
      GO TO 44560                                                       06890035
34560 IVDELE = IVDELE + 1                                               06900035
      WRITE (I02,80003) IVTNUM                                          06910035
      IF (ICZERO) 44560, 4571, 44560                                    06920035
44560 IF (IVCOMP -24) 24560,14560,24560                                 06930035
14560 IVPASS = IVPASS + 1                                               06940035
      WRITE (I02,80001) IVTNUM                                          06950035
      GO TO 4571                                                        06960035
24560 IVFAIL = IVFAIL + 1                                               06970035
      IVCORR = 24                                                       06980035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06990035
 4571 CONTINUE                                                          07000035
      IVTNUM = 457                                                      07010035
C                                                                       07020035
C      ****  TEST 457  ****                                             07030035
C                                                                       07040035
      IF (ICZERO) 34570, 4570, 34570                                    07050035
 4570 CONTINUE                                                          07060035
      IVON01 = -2                                                       07070035
      IVON02 = -3                                                       07080035
      IVCOMP = (IVON01*IVON02) * (-4)                                   07090035
      GO TO 44570                                                       07100035
34570 IVDELE = IVDELE + 1                                               07110035
      WRITE (I02,80003) IVTNUM                                          07120035
      IF (ICZERO) 44570, 4581, 44570                                    07130035
44570 IF (IVCOMP +24) 24570,14570,24570                                 07140035
14570 IVPASS = IVPASS + 1                                               07150035
      WRITE (I02,80001) IVTNUM                                          07160035
      GO TO 4581                                                        07170035
24570 IVFAIL = IVFAIL + 1                                               07180035
      IVCORR = -24                                                      07190035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07200035
 4581 CONTINUE                                                          07210035
      IVTNUM = 458                                                      07220035
C                                                                       07230035
C      ****  TEST 458  ****                                             07240035
C                                                                       07250035
      IF (ICZERO) 34580, 4580, 34580                                    07260035
 4580 CONTINUE                                                          07270035
      IVON01 = 23                                                       07280035
      IVON03 = 13                                                       07290035
      IVCOMP = IVON01 * (51 * IVON03)                                   07300035
      GO TO 44580                                                       07310035
34580 IVDELE = IVDELE + 1                                               07320035
      WRITE (I02,80003) IVTNUM                                          07330035
      IF (ICZERO) 44580, 4591, 44580                                    07340035
44580 IF (IVCOMP -15249) 24580,14580,24580                              07350035
14580 IVPASS = IVPASS + 1                                               07360035
      WRITE (I02,80001) IVTNUM                                          07370035
      GO TO 4591                                                        07380035
24580 IVFAIL = IVFAIL + 1                                               07390035
      IVCORR = 15249                                                    07400035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07410035
 4591 CONTINUE                                                          07420035
      IVTNUM = 459                                                      07430035
C                                                                       07440035
C      ****  TEST 459  ****                                             07450035
C                                                                       07460035
      IF (ICZERO) 34590, 4590, 34590                                    07470035
 4590 CONTINUE                                                          07480035
      IVON02 = 51                                                       07490035
      IVON03 = 13                                                       07500035
      IVCOMP = (23 * IVON02) * IVON03                                   07510035
      GO TO 44590                                                       07520035
34590 IVDELE = IVDELE + 1                                               07530035
      WRITE (I02,80003) IVTNUM                                          07540035
      IF (ICZERO) 44590, 4601, 44590                                    07550035
44590 IF (IVCOMP - 15249) 24590,14590,24590                             07560035
14590 IVPASS = IVPASS + 1                                               07570035
      WRITE (I02,80001) IVTNUM                                          07580035
      GO TO 4601                                                        07590035
24590 IVFAIL = IVFAIL + 1                                               07600035
      IVCORR = 15249                                                    07610035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07620035
 4601 CONTINUE                                                          07630035
      IVTNUM = 460                                                      07640035
C                                                                       07650035
C      ****  TEST 460  ****                                             07660035
C                                                                       07670035
      IF (ICZERO) 34600, 4600, 34600                                    07680035
 4600 CONTINUE                                                          07690035
      IVON01 = -23                                                      07700035
      IVON03 = 13                                                       07710035
      IVCOMP = (IVON01 * (-51)) * (-IVON03)                             07720035
      GO TO 44600                                                       07730035
34600 IVDELE = IVDELE + 1                                               07740035
      WRITE (I02,80003) IVTNUM                                          07750035
      IF (ICZERO) 44600, 4611, 44600                                    07760035
44600 IF (IVCOMP + 15249) 24600,14600,24600                             07770035
14600 IVPASS = IVPASS + 1                                               07780035
      WRITE (I02,80001) IVTNUM                                          07790035
      GO TO 4611                                                        07800035
24600 IVFAIL = IVFAIL + 1                                               07810035
      IVCORR = -15249                                                   07820035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07830035
 4611 CONTINUE                                                          07840035
      IVTNUM = 461                                                      07850035
C                                                                       07860035
C      ****  TEST 461  ****                                             07870035
C                                                                       07880035
      IF (ICZERO) 34610, 4610, 34610                                    07890035
 4610 CONTINUE                                                          07900035
      IVON02 = 51                                                       07910035
      IVON03 = 13                                                       07920035
      IVCOMP = -23 * (IVON02*IVON03)                                    07930035
      GO TO 44610                                                       07940035
34610 IVDELE = IVDELE + 1                                               07950035
      WRITE (I02,80003) IVTNUM                                          07960035
      IF (ICZERO) 44610, 4621, 44610                                    07970035
44610 IF (IVCOMP + 15249) 24610,14610,24610                             07980035
14610 IVPASS = IVPASS + 1                                               07990035
      WRITE (I02,80001) IVTNUM                                          08000035
      GO TO 4621                                                        08010035
24610 IVFAIL = IVFAIL + 1                                               08020035
      IVCORR = -15249                                                   08030035
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08040035
C      ****    END OF TESTS    ****                                     08050035
 4621 CONTINUE                                                          08060035
C                                                                       08070035
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08080035
99999 CONTINUE                                                          08090035
      WRITE (I02,90002)                                                 08100035
      WRITE (I02,90006)                                                 08110035
      WRITE (I02,90002)                                                 08120035
      WRITE (I02,90002)                                                 08130035
      WRITE (I02,90007)                                                 08140035
      WRITE (I02,90002)                                                 08150035
      WRITE (I02,90008)  IVFAIL                                         08160035
      WRITE (I02,90009) IVPASS                                          08170035
      WRITE (I02,90010) IVDELE                                          08180035
C                                                                       08190035
C                                                                       08200035
C     TERMINATE ROUTINE EXECUTION                                       08210035
      STOP                                                              08220035
C                                                                       08230035
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08240035
90000 FORMAT (1H1)                                                      08250035
90002 FORMAT (1H )                                                      08260035
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08270035
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08280035
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08290035
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08300035
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08310035
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08320035
C                                                                       08330035
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08340035
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08350035
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08360035
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08370035
C                                                                       08380035
C     FORMAT STATEMENTS FOR TEST RESULTS                                08390035
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08400035
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08410035
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08420035
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08430035
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08440035
C                                                                       08450035
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM035)                          08460035
      END                                                               08470035
