C     COMMENT SECTION                                                   00010034
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020034
C     FM034                                                             00030034
C                                                                       00040034
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050034
C     FORM                                                              00060034
C               INTEGER VARIABLE = ARITHMETIC EXPRESSION                00070034
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080034
C     OPERATOR *, INTEGER VARIABLE AND INTEGER CONSTANT.  SOME OF THE   00090034
C     TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE EXPRESSION AND TO  00100034
C     ALLOW THE USE OF NEGATIVE CONSTANTS FOLLOWING THE * OPERATOR.     00110034
C     THE INTEGER VARIABLES CONTAIN POSITIVE AND NEGATIVE VALUES.       00120034
C                                                                       00130034
C     THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS          00140034
C         (1)  INTEGER VARIABLE * INTEGER CONSTANT                      00150034
C              INTEGER CONSTANT * INTEGER VARIABLE                      00160034
C         (2)  INTEGER CONSTANT * INTEGER VARIABLE * INTEGER CONSTANT   00170034
C         (3)  SAME AS (2) BUT WITH PARENS TO GROUP ELEMENTS.           00180034
C                                                                       00190034
C      REFERENCES                                                       00200034
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00210034
C              X3.9-1978                                                00220034
C                                                                       00230034
C        SECTION 4.3, INTEGER TYPE                                      00240034
C        SECTION 4.3.1, INTEGER CONSTANT                                00250034
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00260034
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00270034
C                                                                       00280034
C      **********************************************************       00290034
C                                                                       00300034
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00310034
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00320034
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00330034
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00340034
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00350034
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00360034
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00370034
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00380034
C     OF EXECUTING THESE TESTS.                                         00390034
C                                                                       00400034
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00410034
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00420034
C                                                                       00430034
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00440034
C                                                                       00450034
C                  DEPARTMENT OF THE NAVY                               00460034
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00470034
C                  WASHINGTON, D.C.  20376                              00480034
C                                                                       00490034
C      **********************************************************       00500034
C                                                                       00510034
C                                                                       00520034
C                                                                       00530034
C     INITIALIZATION SECTION                                            00540034
C                                                                       00550034
C     INITIALIZE CONSTANTS                                              00560034
C      **************                                                   00570034
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00580034
      I01 = 5                                                           00590034
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00600034
      I02 = 6                                                           00610034
C     SYSTEM ENVIRONMENT SECTION                                        00620034
C                                                                       00630034
      I01 = 5                                                           00640034
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00650034
C     (UNIT NUMBER FOR CARD READER).                                    00660034
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00670034
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00680034
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00690034
C                                                                       00700034
      I02 = 6                                                           00710034
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00720034
C     (UNIT NUMBER FOR PRINTER).                                        00730034
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00740034
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00750034
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00760034
C                                                                       00770034
      IVPASS=0                                                          00780034
      IVFAIL=0                                                          00790034
      IVDELE=0                                                          00800034
      ICZERO=0                                                          00810034
C                                                                       00820034
C     WRITE PAGE HEADERS                                                00830034
      WRITE (I02,90000)                                                 00840034
      WRITE (I02,90001)                                                 00850034
      WRITE (I02,90002)                                                 00860034
      WRITE (I02, 90002)                                                00870034
      WRITE (I02,90003)                                                 00880034
      WRITE (I02,90002)                                                 00890034
      WRITE (I02,90004)                                                 00900034
      WRITE (I02,90002)                                                 00910034
      WRITE (I02,90011)                                                 00920034
      WRITE (I02,90002)                                                 00930034
      WRITE (I02,90002)                                                 00940034
      WRITE (I02,90005)                                                 00950034
      WRITE (I02,90006)                                                 00960034
      WRITE (I02,90002)                                                 00970034
C                                                                       00980034
C     TEST SECTION                                                      00990034
C                                                                       01000034
C         ARITHMETIC ASSIGNMENT STATEMENT                               01010034
C                                                                       01020034
C     TEST 395 THROUGH TEST 414 CONTAIN AN INTEGER VARIABLE, AN INTEGER 01030034
C     CONSTANT, AND OPERATOR * IN AN ARITHMETIC EXPRESSION.             01040034
C                                                                       01050034
C     TEST 395 THROUGH TEST 406     -  IV= IV * IC                      01060034
C                                                                       01070034
C         TEST 395 THROUGH TEST 398                                     01080034
C              POSITIVE INTEGER VARIABLE, POSITIVE INTEGER CONSTANT     01090034
C                                                                       01100034
 3951 CONTINUE                                                          01110034
      IVTNUM = 395                                                      01120034
C                                                                       01130034
C      ****  TEST 395  ****                                             01140034
C                                                                       01150034
      IF (ICZERO) 33950, 3950, 33950                                    01160034
 3950 CONTINUE                                                          01170034
      IVON01 = 2                                                        01180034
      IVCOMP = IVON01 * 3                                               01190034
      GO TO 43950                                                       01200034
33950 IVDELE = IVDELE + 1                                               01210034
      WRITE (I02,80003) IVTNUM                                          01220034
      IF (ICZERO) 43950, 3961, 43950                                    01230034
43950 IF (IVCOMP -6) 23950,13950,23950                                  01240034
13950 IVPASS = IVPASS + 1                                               01250034
      WRITE (I02,80001) IVTNUM                                          01260034
      GO TO 3961                                                        01270034
23950 IVFAIL = IVFAIL + 1                                               01280034
      IVCORR =6                                                         01290034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01300034
 3961 CONTINUE                                                          01310034
      IVTNUM = 396                                                      01320034
C                                                                       01330034
C      ****  TEST 396  ****                                             01340034
C                                                                       01350034
      IF (ICZERO) 33960, 3960, 33960                                    01360034
 3960 CONTINUE                                                          01370034
      IVON01 = 13                                                       01380034
      IVCOMP = IVON01 * 11                                              01390034
      GO TO 43960                                                       01400034
33960 IVDELE = IVDELE + 1                                               01410034
      WRITE (I02,80003) IVTNUM                                          01420034
      IF (ICZERO) 43960, 3971, 43960                                    01430034
43960 IF (IVCOMP - 143) 23960,13960,23960                               01440034
13960 IVPASS = IVPASS + 1                                               01450034
      WRITE (I02,80001) IVTNUM                                          01460034
      GO TO 3971                                                        01470034
23960 IVFAIL = IVFAIL + 1                                               01480034
      IVCORR = 143                                                      01490034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01500034
 3971 CONTINUE                                                          01510034
      IVTNUM = 397                                                      01520034
C                                                                       01530034
C      ****  TEST 397  ****                                             01540034
C                                                                       01550034
      IF (ICZERO) 33970, 3970, 33970                                    01560034
 3970 CONTINUE                                                          01570034
      IVON01 = 223                                                      01580034
      IVCOMP = IVON01 * 99                                              01590034
      GO TO 43970                                                       01600034
33970 IVDELE = IVDELE + 1                                               01610034
      WRITE (I02,80003) IVTNUM                                          01620034
      IF (ICZERO) 43970, 3981, 43970                                    01630034
43970 IF (IVCOMP - 22077) 23970,13970,23970                             01640034
13970 IVPASS = IVPASS + 1                                               01650034
      WRITE (I02,80001) IVTNUM                                          01660034
      GO TO 3981                                                        01670034
23970 IVFAIL = IVFAIL + 1                                               01680034
      IVCORR = 22077                                                    01690034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01700034
 3981 CONTINUE                                                          01710034
      IVTNUM = 398                                                      01720034
C                                                                       01730034
C      ****  TEST 398  ****                                             01740034
C                                                                       01750034
      IF (ICZERO) 33980, 3980, 33980                                    01760034
 3980 CONTINUE                                                          01770034
      IVON01 = 11235                                                    01780034
      IVCOMP = IVON01 * 2                                               01790034
      GO TO 43980                                                       01800034
33980 IVDELE = IVDELE + 1                                               01810034
      WRITE (I02,80003) IVTNUM                                          01820034
      IF (ICZERO) 43980, 3991, 43980                                    01830034
43980 IF (IVCOMP - 22470) 23980,13980,23980                             01840034
13980 IVPASS = IVPASS + 1                                               01850034
      WRITE (I02,80001) IVTNUM                                          01860034
      GO TO 3991                                                        01870034
23980 IVFAIL = IVFAIL + 1                                               01880034
      IVCORR = 22470                                                    01890034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01900034
C                                                                       01910034
C         TEST 399 THROUGH TEST 402                                     01920034
C             NEGATIVE INTEGER VARIABLE, POSITIVE INTEGER CONSTANT      01930034
C                                                                       01940034
 3991 CONTINUE                                                          01950034
      IVTNUM = 399                                                      01960034
C                                                                       01970034
C       ****  TEST 399  ****                                            01980034
C                                                                       01990034
      IF (ICZERO) 33990, 3990, 33990                                    02000034
 3990 CONTINUE                                                          02010034
      IVON01 = -2                                                       02020034
      IVCOMP = IVON01 * 3                                               02030034
      GO TO 43990                                                       02040034
33990 IVDELE = IVDELE + 1                                               02050034
      WRITE (I02,80003) IVTNUM                                          02060034
      IF (ICZERO) 43990, 4001, 43990                                    02070034
43990 IF (IVCOMP +6) 23990,13990,23990                                  02080034
13990 IVPASS = IVPASS + 1                                               02090034
      WRITE (I02,80001) IVTNUM                                          02100034
      GO TO 4001                                                        02110034
23990 IVFAIL = IVFAIL + 1                                               02120034
      IVCORR = -6                                                       02130034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02140034
 4001 CONTINUE                                                          02150034
      IVTNUM = 400                                                      02160034
C                                                                       02170034
C      ****  TEST 400  ****                                             02180034
C                                                                       02190034
      IF (ICZERO) 34000, 4000, 34000                                    02200034
 4000 CONTINUE                                                          02210034
      IVON01 = -13                                                      02220034
      IVCOMP =IVON01*11                                                 02230034
      GO TO 44000                                                       02240034
34000 IVDELE = IVDELE + 1                                               02250034
      WRITE (I02,80003) IVTNUM                                          02260034
      IF (ICZERO) 44000, 4011, 44000                                    02270034
44000 IF (IVCOMP +143) 24000,14000,24000                                02280034
14000 IVPASS = IVPASS + 1                                               02290034
      WRITE (I02,80001) IVTNUM                                          02300034
      GO TO 4011                                                        02310034
24000 IVFAIL = IVFAIL + 1                                               02320034
      IVCORR = -143                                                     02330034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02340034
 4011 CONTINUE                                                          02350034
      IVTNUM = 401                                                      02360034
C                                                                       02370034
C       ****  TEST 401  ****                                            02380034
C                                                                       02390034
      IF (ICZERO) 34010, 4010, 34010                                    02400034
 4010 CONTINUE                                                          02410034
      IVON01 = -223                                                     02420034
      IVCOMP = IVON01*99                                                02430034
      GO TO 44010                                                       02440034
34010 IVDELE = IVDELE + 1                                               02450034
      WRITE (I02,80003) IVTNUM                                          02460034
      IF (ICZERO) 44010, 4021, 44010                                    02470034
44010 IF (IVCOMP + 22077) 24010,14010,24010                             02480034
14010 IVPASS = IVPASS + 1                                               02490034
      WRITE (I02,80001) IVTNUM                                          02500034
      GO TO 4021                                                        02510034
24010 IVFAIL = IVFAIL + 1                                               02520034
      IVCORR = -22077                                                   02530034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02540034
 4021 CONTINUE                                                          02550034
      IVTNUM = 402                                                      02560034
C                                                                       02570034
C       ****  TEST 402  ****                                            02580034
C                                                                       02590034
      IF (ICZERO) 34020, 4020, 34020                                    02600034
 4020 CONTINUE                                                          02610034
      IVON01 = -11235                                                   02620034
      IVCOMP = IVON01*2                                                 02630034
      GO TO 44020                                                       02640034
34020 IVDELE = IVDELE + 1                                               02650034
      WRITE (I02,80003) IVTNUM                                          02660034
      IF (ICZERO) 44020, 4031, 44020                                    02670034
44020 IF (IVCOMP+22470) 24020,14020,24020                               02680034
14020 IVPASS = IVPASS + 1                                               02690034
      WRITE (I02,80001) IVTNUM                                          02700034
      GO TO 4031                                                        02710034
24020 IVFAIL = IVFAIL + 1                                               02720034
      IVCORR = -22470                                                   02730034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02740034
C                                                                       02750034
C          TEST 403 AND TEST 404                                        02760034
C              NEGATIVE INTEGER VARIABLE, NEGATIVE INTEGER CONSTANT     02770034
C                                                                       02780034
 4031 CONTINUE                                                          02790034
      IVTNUM = 403                                                      02800034
C                                                                       02810034
C       ****  TEST 403  ****                                            02820034
C                                                                       02830034
      IF (ICZERO) 34030, 4030, 34030                                    02840034
 4030 CONTINUE                                                          02850034
      IVON01=-2                                                         02860034
      IVCOMP = IVON01*(-3)                                              02870034
      GO TO 44030                                                       02880034
34030 IVDELE = IVDELE + 1                                               02890034
      WRITE (I02,80003) IVTNUM                                          02900034
      IF (ICZERO) 44030, 4041, 44030                                    02910034
44030 IF (IVCOMP -6) 24030,14030,24030                                  02920034
14030 IVPASS = IVPASS + 1                                               02930034
      WRITE (I02,80001) IVTNUM                                          02940034
      GO TO 4041                                                        02950034
24030 IVFAIL = IVFAIL + 1                                               02960034
      IVCORR =6                                                         02970034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02980034
 4041 CONTINUE                                                          02990034
      IVTNUM = 404                                                      03000034
C                                                                       03010034
C       ****  TEST 404  ****                                            03020034
C                                                                       03030034
      IF (ICZERO) 34040, 4040, 34040                                    03040034
 4040 CONTINUE                                                          03050034
      IVON01 = -13                                                      03060034
      IVCOMP = IVON01 * (-11)                                           03070034
      GO TO 44040                                                       03080034
34040 IVDELE = IVDELE + 1                                               03090034
      WRITE (I02,80003) IVTNUM                                          03100034
      IF (ICZERO) 44040, 4051, 44040                                    03110034
44040 IF (IVCOMP -143) 24040,14040,24040                                03120034
14040 IVPASS = IVPASS + 1                                               03130034
      WRITE (I02,80001) IVTNUM                                          03140034
      GO TO 4051                                                        03150034
24040 IVFAIL = IVFAIL + 1                                               03160034
      IVCORR = 143                                                      03170034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03180034
C                                                                       03190034
C          TEST 405 AND TEST 406                                        03200034
C              POSITIVE INTEGER VARIABLE, NEGATIVE INTEGER CONSTANT     03210034
C                                                                       03220034
 4051 CONTINUE                                                          03230034
      IVTNUM = 405                                                      03240034
C                                                                       03250034
C       ****  TEST 405  ****                                            03260034
C                                                                       03270034
      IF (ICZERO) 34050, 4050, 34050                                    03280034
 4050 CONTINUE                                                          03290034
      IVON01 = 223                                                      03300034
      IVCOMP = IVON01 * (-99)                                           03310034
      GO TO 44050                                                       03320034
34050 IVDELE = IVDELE + 1                                               03330034
      WRITE (I02,80003) IVTNUM                                          03340034
      IF (ICZERO) 44050, 4061, 44050                                    03350034
44050 IF (IVCOMP + 22077) 24050,14050,24050                             03360034
14050 IVPASS = IVPASS + 1                                               03370034
      WRITE (I02,80001) IVTNUM                                          03380034
      GO TO 4061                                                        03390034
24050 IVFAIL = IVFAIL + 1                                               03400034
      IVCORR = -22077                                                   03410034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03420034
 4061 CONTINUE                                                          03430034
      IVTNUM = 406                                                      03440034
C                                                                       03450034
C       ****  TEST 406  ****                                            03460034
C                                                                       03470034
      IF (ICZERO) 34060, 4060, 34060                                    03480034
 4060 CONTINUE                                                          03490034
      IVON01 = 11235                                                    03500034
      IVCOMP = IVON01 * (-2)                                            03510034
      GO TO 44060                                                       03520034
34060 IVDELE = IVDELE + 1                                               03530034
      WRITE (I02,80003) IVTNUM                                          03540034
      IF (ICZERO) 44060, 4071, 44060                                    03550034
44060 IF (IVCOMP + 22470) 24060,14060,24060                             03560034
14060 IVPASS = IVPASS + 1                                               03570034
      WRITE (I02,80001) IVTNUM                                          03580034
      GO TO 4071                                                        03590034
24060 IVFAIL = IVFAIL + 1                                               03600034
      IVCORR = -22470                                                   03610034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03620034
C                                                                       03630034
C      TEST 407 THROUGH TEST 414    -   IV = IC * IV                    03640034
C                                                                       03650034
C          TEST 407 AND TEST 408                                        03660034
C               POSITIVE INTEGER CONSTANT, POSITIVE INTEGER VARIABLE    03670034
C                                                                       03680034
 4071 CONTINUE                                                          03690034
      IVTNUM = 407                                                      03700034
C                                                                       03710034
C       ****  TEST 407  ****                                            03720034
C                                                                       03730034
      IF (ICZERO) 34070, 4070, 34070                                    03740034
 4070 CONTINUE                                                          03750034
      IVON02 = 11                                                       03760034
      IVCOMP = 13*IVON02                                                03770034
      GO TO 44070                                                       03780034
34070 IVDELE = IVDELE + 1                                               03790034
      WRITE (I02,80003) IVTNUM                                          03800034
      IF (ICZERO) 44070, 4081, 44070                                    03810034
44070 IF (IVCOMP - 143) 24070,14070,24070                               03820034
14070 IVPASS = IVPASS + 1                                               03830034
      WRITE (I02,80001) IVTNUM                                          03840034
      GO TO 4081                                                        03850034
24070 IVFAIL = IVFAIL + 1                                               03860034
      IVCORR = 143                                                      03870034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03880034
 4081 CONTINUE                                                          03890034
      IVTNUM = 408                                                      03900034
C                                                                       03910034
C       ****  TEST 408  ****                                            03920034
C                                                                       03930034
      IF (ICZERO) 34080, 4080, 34080                                    03940034
 4080 CONTINUE                                                          03950034
      IVON02 = +11                                                      03960034
      IVCOMP = +13 * IVON02                                             03970034
      GO TO 44080                                                       03980034
34080 IVDELE = IVDELE + 1                                               03990034
      WRITE (I02,80003) IVTNUM                                          04000034
      IF (ICZERO) 44080, 4091, 44080                                    04010034
44080 IF (IVCOMP - 143) 24080,14080,24080                               04020034
14080 IVPASS = IVPASS + 1                                               04030034
      WRITE (I02,80001) IVTNUM                                          04040034
      GO TO 4091                                                        04050034
24080 IVFAIL = IVFAIL + 1                                               04060034
      IVCORR = 143                                                      04070034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04080034
C                                                                       04090034
C          TEST 409 AND TEST 410                                        04100034
C               POSITIVE INTEGER CONSTANT, NEGATIVE INTEGER VARIABLE    04110034
C                                                                       04120034
 4091 CONTINUE                                                          04130034
      IVTNUM = 409                                                      04140034
C                                                                       04150034
C       ****  TEST 409  ****                                            04160034
C                                                                       04170034
      IF (ICZERO) 34090, 4090, 34090                                    04180034
 4090 CONTINUE                                                          04190034
      IVON02 = -99                                                      04200034
      IVCOMP = 223 * IVON02                                             04210034
      GO TO 44090                                                       04220034
34090 IVDELE = IVDELE + 1                                               04230034
      WRITE (I02,80003) IVTNUM                                          04240034
      IF (ICZERO) 44090, 4101, 44090                                    04250034
44090 IF (IVCOMP + 22077) 24090,14090,24090                             04260034
14090 IVPASS = IVPASS + 1                                               04270034
      WRITE (I02,80001) IVTNUM                                          04280034
      GO TO 4101                                                        04290034
24090 IVFAIL = IVFAIL + 1                                               04300034
      IVCORR =-22077                                                    04310034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04320034
 4101 CONTINUE                                                          04330034
      IVTNUM = 410                                                      04340034
C                                                                       04350034
C       ****  TEST 410  ****                                            04360034
C                                                                       04370034
      IF (ICZERO) 34100, 4100, 34100                                    04380034
 4100 CONTINUE                                                          04390034
      IVON02 = -99                                                      04400034
      IVCOMP = +223*IVON02                                              04410034
      GO TO 44100                                                       04420034
34100 IVDELE = IVDELE + 1                                               04430034
      WRITE (I02,80003) IVTNUM                                          04440034
      IF (ICZERO) 44100, 4111, 44100                                    04450034
44100 IF (IVCOMP + 22077) 24100,14100,24100                             04460034
14100 IVPASS = IVPASS + 1                                               04470034
      WRITE (I02,80001) IVTNUM                                          04480034
      GO TO 4111                                                        04490034
24100 IVFAIL = IVFAIL + 1                                               04500034
      IVCORR = -22077                                                   04510034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04520034
C                                                                       04530034
C          TEST 411 AND TEST 412                                        04540034
C              NEGATIVE INTEGER CONSTANT, POSITIVE INTEGER VARIABLE     04550034
C                                                                       04560034
 4111 CONTINUE                                                          04570034
      IVTNUM = 411                                                      04580034
C                                                                       04590034
C       ****  TEST 411  ****                                            04600034
C                                                                       04610034
      IF (ICZERO) 34110, 4110, 34110                                    04620034
 4110 CONTINUE                                                          04630034
      IVON02 = 2                                                        04640034
      IVCOMP = (-11235) * IVON02                                        04650034
      GO TO 44110                                                       04660034
34110 IVDELE = IVDELE + 1                                               04670034
      WRITE (I02,80003) IVTNUM                                          04680034
      IF (ICZERO) 44110, 4121, 44110                                    04690034
44110 IF (IVCOMP + 22470) 24110,14110,24110                             04700034
14110 IVPASS = IVPASS + 1                                               04710034
      WRITE (I02,80001) IVTNUM                                          04720034
      GO TO 4121                                                        04730034
24110 IVFAIL = IVFAIL + 1                                               04740034
      IVCORR = -22470                                                   04750034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04760034
 4121 CONTINUE                                                          04770034
      IVTNUM = 412                                                      04780034
C                                                                       04790034
C       ****  TEST 412  ****                                            04800034
C                                                                       04810034
      IF (ICZERO) 34120, 4120, 34120                                    04820034
 4120 CONTINUE                                                          04830034
      IVON02 = +2                                                       04840034
      IVCOMP = -11235 * IVON02                                          04850034
      GO TO 44120                                                       04860034
34120 IVDELE = IVDELE + 1                                               04870034
      WRITE (I02,80003) IVTNUM                                          04880034
      IF (ICZERO) 44120, 4131, 44120                                    04890034
44120 IF (IVCOMP + 22470) 24120,14120,24120                             04900034
14120 IVPASS=IVPASS + 1                                                 04910034
      WRITE (I02,80001) IVTNUM                                          04920034
      GO TO 4131                                                        04930034
24120 IVFAIL = IVFAIL + 1                                               04940034
      IVCORR = -22470                                                   04950034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04960034
C                                                                       04970034
C          TEST 413 AND TEST 414                                        04980034
C                NEGATIVE INTEGER CONSTANT, NEGATIVE INTEGER VARIABLE   04990034
C                                                                       05000034
 4131 CONTINUE                                                          05010034
      IVTNUM = 413                                                      05020034
C                                                                       05030034
C       ****  TEST 413  ****                                            05040034
C                                                                       05050034
      IF (ICZERO) 34130, 4130, 34130                                    05060034
 4130 CONTINUE                                                          05070034
      IVON02 = -3                                                       05080034
      IVCOMP = (-2) * IVON02                                            05090034
      GO TO 44130                                                       05100034
34130 IVDELE = IVDELE + 1                                               05110034
      WRITE (I02,80003) IVTNUM                                          05120034
      IF (ICZERO) 44130, 4141, 44130                                    05130034
44130 IF (IVCOMP - 6) 24130,14130,24130                                 05140034
14130 IVPASS = IVPASS + 1                                               05150034
      WRITE (I02,80001) IVTNUM                                          05160034
      GO TO 4141                                                        05170034
24130 IVFAIL = IVFAIL + 1                                               05180034
      IVCORR = 6                                                        05190034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05200034
 4141 CONTINUE                                                          05210034
      IVTNUM = 414                                                      05220034
C                                                                       05230034
C       ****  TEST 414  ****                                            05240034
C                                                                       05250034
      IF (ICZERO) 34140, 4140, 34140                                    05260034
 4140 CONTINUE                                                          05270034
      IVON02 = -3                                                       05280034
      IVCOMP = -2 * IVON02                                              05290034
      GO TO 44140                                                       05300034
34140 IVDELE = IVDELE + 1                                               05310034
      WRITE (I02,80003) IVTNUM                                          05320034
      IF (ICZERO) 44140, 4151, 44140                                    05330034
44140 IF (IVCOMP - 6) 24140,14140,24140                                 05340034
14140 IVPASS = IVPASS + 1                                               05350034
      WRITE (I02,80001) IVTNUM                                          05360034
      GO TO 4151                                                        05370034
24140 IVFAIL = IVFAIL + 1                                               05380034
      IVCORR = 6                                                        05390034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05400034
C                                                                       05410034
C      TEST 415  THROUGH TEST 429 CONTAIN TWO INTEGER CONSTANTS,        05420034
C      ONE INTEGER VARIABLE AND OPERATOR * IN ARITHMETIC EXPRESSION.    05430034
C                                                                       05440034
 4151 CONTINUE                                                          05450034
      IVTNUM = 415                                                      05460034
C                                                                       05470034
C       ****  TEST 415  ****                                            05480034
C                                                                       05490034
      IF (ICZERO) 34150, 4150, 34150                                    05500034
 4150 CONTINUE                                                          05510034
      IVON01 = 2                                                        05520034
      IVCOMP = IVON01 * 3 * 4                                           05530034
      GO TO 44150                                                       05540034
34150 IVDELE = IVDELE + 1                                               05550034
      WRITE (I02,80003) IVTNUM                                          05560034
      IF (ICZERO) 44150, 4161, 44150                                    05570034
44150 IF (IVCOMP - 24) 24150,14150,24150                                05580034
14150 IVPASS = IVPASS + 1                                               05590034
      WRITE (I02,80001) IVTNUM                                          05600034
      GO TO 4161                                                        05610034
24150 IVFAIL = IVFAIL + 1                                               05620034
      IVCORR = 24                                                       05630034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05640034
 4161 CONTINUE                                                          05650034
      IVTNUM = 416                                                      05660034
C                                                                       05670034
C       ****  TEST 416  ****                                            05680034
C                                                                       05690034
      IF (ICZERO) 34160, 4160, 34160                                    05700034
 4160 CONTINUE                                                          05710034
      IVON01 = -2                                                       05720034
      IVCOMP = IVON01 *3*4                                              05730034
      GO TO 44160                                                       05740034
34160 IVDELE = IVDELE + 1                                               05750034
      WRITE (I02,80003) IVTNUM                                          05760034
      IF (ICZERO) 44160, 4171, 44160                                    05770034
44160 IF (IVCOMP +24) 24160,14160,24160                                 05780034
14160 IVPASS = IVPASS + 1                                               05790034
      WRITE (I02,80001) IVTNUM                                          05800034
      GO TO 4171                                                        05810034
24160 IVFAIL = IVFAIL + 1                                               05820034
      IVCORR = -24                                                      05830034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05840034
 4171 CONTINUE                                                          05850034
      IVTNUM = 417                                                      05860034
C                                                                       05870034
C       ****  TEST 417  ****                                            05880034
C                                                                       05890034
      IF (ICZERO) 34170, 4170, 34170                                    05900034
 4170 CONTINUE                                                          05910034
      IVON01 = -2                                                       05920034
      IVCOMP = IVON01*3*(-4)                                            05930034
      GO TO 44170                                                       05940034
34170 IVDELE = IVDELE + 1                                               05950034
      WRITE (I02,80003) IVTNUM                                          05960034
      IF (ICZERO) 44170, 4181, 44170                                    05970034
44170 IF (IVCOMP -24) 24170,14170,24170                                 05980034
14170 IVPASS = IVPASS + 1                                               05990034
      WRITE (I02,80001) IVTNUM                                          06000034
      GO TO 4181                                                        06010034
24170 IVFAIL = IVFAIL + 1                                               06020034
      IVCORR = 24                                                       06030034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06040034
 4181 CONTINUE                                                          06050034
      IVTNUM = 418                                                      06060034
C                                                                       06070034
C       ****  TEST 418  ****                                            06080034
C                                                                       06090034
      IF (ICZERO) 34180, 4180, 34180                                    06100034
 4180 CONTINUE                                                          06110034
      IVON01 = -2                                                       06120034
      IVCOMP = IVON01*(-3)*(-4)                                         06130034
      GO TO 44180                                                       06140034
34180 IVDELE = IVDELE + 1                                               06150034
      WRITE (I02,80003) IVTNUM                                          06160034
      IF (ICZERO) 44180, 4191, 44180                                    06170034
44180 IF (IVCOMP +24) 24180,14180,24180                                 06180034
14180 IVPASS = IVPASS + 1                                               06190034
      WRITE (I02,80001) IVTNUM                                          06200034
      GO TO 4191                                                        06210034
24180 IVFAIL = IVFAIL + 1                                               06220034
      IVCORR = -24                                                      06230034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06240034
 4191 CONTINUE                                                          06250034
      IVTNUM = 419                                                      06260034
C                                                                       06270034
C       ****  TEST 419  ****                                            06280034
C                                                                       06290034
      IF (ICZERO) 34190, 4190, 34190                                    06300034
 4190 CONTINUE                                                          06310034
      IVON02 = 51                                                       06320034
      IVCOMP = 23*IVON02*13                                             06330034
      GO TO 44190                                                       06340034
34190 IVDELE = IVDELE + 1                                               06350034
      WRITE (I02,80003) IVTNUM                                          06360034
      IF (ICZERO) 44190, 4201, 44190                                    06370034
44190 IF (IVCOMP-15249) 24190,14190,24190                               06380034
14190 IVPASS = IVPASS + 1                                               06390034
      WRITE (I02,80001) IVTNUM                                          06400034
      GO TO 4201                                                        06410034
24190 IVFAIL = IVFAIL + 1                                               06420034
      IVCORR = 15249                                                    06430034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06440034
 4201 CONTINUE                                                          06450034
      IVTNUM = 420                                                      06460034
C                                                                       06470034
C       ****  TEST 420  ****                                            06480034
C                                                                       06490034
      IF (ICZERO) 34200, 4200, 34200                                    06500034
 4200 CONTINUE                                                          06510034
      IVON02 = -51                                                      06520034
      IVCOMP = 23*IVON02*(-13)                                          06530034
      GO TO 44200                                                       06540034
34200 IVDELE = IVDELE + 1                                               06550034
      WRITE (I02,80003) IVTNUM                                          06560034
      IF (ICZERO) 44200, 4211, 44200                                    06570034
44200 IF (IVCOMP - 15249) 24200,14200,24200                             06580034
14200 IVPASS = IVPASS + 1                                               06590034
      WRITE (I02,80001) IVTNUM                                          06600034
      GO TO 4211                                                        06610034
24200 IVFAIL = IVFAIL + 1                                               06620034
      IVCORR = 15249                                                    06630034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06640034
 4211 CONTINUE                                                          06650034
      IVTNUM = 421                                                      06660034
C                                                                       06670034
C       ****  TEST 421  ****                                            06680034
C                                                                       06690034
      IF (ICZERO) 34210, 4210, 34210                                    06700034
 4210 CONTINUE                                                          06710034
      IVON02 = -51                                                      06720034
      IVCOMP = 23*IVON02*13                                             06730034
      GO TO 44210                                                       06740034
34210 IVDELE = IVDELE + 1                                               06750034
      WRITE (I02,80003) IVTNUM                                          06760034
      IF (ICZERO) 44210, 4221, 44210                                    06770034
44210 IF (IVCOMP+15249) 24210,14210,24210                               06780034
14210 IVPASS = IVPASS + 1                                               06790034
      WRITE (I02,80001) IVTNUM                                          06800034
      GO TO 4221                                                        06810034
24210 IVFAIL = IVFAIL + 1                                               06820034
      IVCORR = -15249                                                   06830034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06840034
 4221 CONTINUE                                                          06850034
      IVTNUM = 422                                                      06860034
C                                                                       06870034
C       ****  TEST 422  ****                                            06880034
C                                                                       06890034
      IF (ICZERO) 34220, 4220, 34220                                    06900034
 4220 CONTINUE                                                          06910034
      IVON02 = -51                                                      06920034
      IVCOMP =(-23)*IVON02*(-13)                                        06930034
      GO TO 44220                                                       06940034
34220 IVDELE = IVDELE + 1                                               06950034
      WRITE (I02,80003) IVTNUM                                          06960034
      IF (ICZERO) 44220, 4231, 44220                                    06970034
44220 IF (IVCOMP+15249) 24220,14220,24220                               06980034
14220 IVPASS = IVPASS + 1                                               06990034
      WRITE (I02,80001) IVTNUM                                          07000034
      GO TO 4231                                                        07010034
24220 IVFAIL = IVFAIL + 1                                               07020034
      IVCORR = -15249                                                   07030034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07040034
 4231 CONTINUE                                                          07050034
      IVTNUM = 423                                                      07060034
C                                                                       07070034
C       ****  TEST 423  ****                                            07080034
C                                                                       07090034
      IF (ICZERO) 34230, 4230, 34230                                    07100034
 4230 CONTINUE                                                          07110034
      IVON03 = 5461                                                     07120034
      IVCOMP = 2*3*IVON03                                               07130034
      GO TO 44230                                                       07140034
34230 IVDELE = IVDELE + 1                                               07150034
      WRITE (I02,80003) IVTNUM                                          07160034
      IF (ICZERO) 44230, 4241, 44230                                    07170034
44230 IF (IVCOMP - 32766) 24230,14230,24230                             07180034
14230 IVPASS = IVPASS + 1                                               07190034
      WRITE (I02,80001) IVTNUM                                          07200034
      GO TO 4241                                                        07210034
24230 IVFAIL = IVFAIL + 1                                               07220034
      IVCORR = 32766                                                    07230034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07240034
 4241 CONTINUE                                                          07250034
      IVTNUM = 424                                                      07260034
C                                                                       07270034
C       ****  TEST 424  ****                                            07280034
C                                                                       07290034
      IF (ICZERO) 34240, 4240, 34240                                    07300034
 4240 CONTINUE                                                          07310034
      IVON03 = -5461                                                    07320034
      IVCOMP = 2*3*IVON03                                               07330034
      GO TO 44240                                                       07340034
34240 IVDELE = IVDELE + 1                                               07350034
      WRITE (I02,80003) IVTNUM                                          07360034
      IF (ICZERO) 44240, 4251, 44240                                    07370034
44240 IF (IVCOMP +32766) 24240,14240,24240                              07380034
14240 IVPASS = IVPASS + 1                                               07390034
      WRITE (I02,80001) IVTNUM                                          07400034
      GO TO 4251                                                        07410034
24240 IVFAIL = IVFAIL + 1                                               07420034
      IVCORR = -32766                                                   07430034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07440034
 4251 CONTINUE                                                          07450034
      IVTNUM = 425                                                      07460034
C                                                                       07470034
C       ****  TEST 425  ****                                            07480034
C                                                                       07490034
      IF (ICZERO) 34250, 4250, 34250                                    07500034
 4250 CONTINUE                                                          07510034
      IVON03 = -5461                                                    07520034
      IVCOMP = -2*3*IVON03                                              07530034
      GO TO 44250                                                       07540034
34250 IVDELE = IVDELE + 1                                               07550034
      WRITE (I02,80003) IVTNUM                                          07560034
      IF (ICZERO) 44250, 4261, 44250                                    07570034
44250 IF (IVCOMP - 32766) 24250,14250,24250                             07580034
14250 IVPASS = IVPASS + 1                                               07590034
      WRITE (I02,80001) IVTNUM                                          07600034
      GO TO 4261                                                        07610034
24250 IVFAIL = IVFAIL + 1                                               07620034
      IVCORR = 32766                                                    07630034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07640034
C                                                                       07650034
C      TEST 426 THROUGH TEST 429 USE PARENTHESES TO GROUP ELEMENTS      07660034
C      IN ARITHMETIC EXPRESSION.                                        07670034
C                                                                       07680034
 4261 CONTINUE                                                          07690034
      IVTNUM = 426                                                      07700034
C                                                                       07710034
C       ****  TEST 426  ****                                            07720034
C                                                                       07730034
      IF (ICZERO) 34260, 4260, 34260                                    07740034
 4260 CONTINUE                                                          07750034
      IVON02 = 51                                                       07760034
      IVCOMP = (23*IVON02)*13                                           07770034
      GO TO 44260                                                       07780034
34260 IVDELE = IVDELE + 1                                               07790034
      WRITE (I02,80003) IVTNUM                                          07800034
      IF (ICZERO) 44260, 4271, 44260                                    07810034
44260 IF (IVCOMP -15249) 24260,14260,24260                              07820034
14260 IVPASS = IVPASS + 1                                               07830034
      WRITE (I02,80001) IVTNUM                                          07840034
      GO TO 4271                                                        07850034
24260 IVFAIL = IVFAIL + 1                                               07860034
      IVCORR = 15249                                                    07870034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07880034
 4271 CONTINUE                                                          07890034
      IVTNUM = 427                                                      07900034
C                                                                       07910034
C       ****  TEST 427  ****                                            07920034
C                                                                       07930034
      IF (ICZERO) 34270, 4270, 34270                                    07940034
 4270 CONTINUE                                                          07950034
      IVON02 = 51                                                       07960034
      IVCOMP = 23*(IVON02*13)                                           07970034
      GO TO 44270                                                       07980034
34270 IVDELE = IVDELE + 1                                               07990034
      WRITE (I02,80003) IVTNUM                                          08000034
      IF (ICZERO) 44270, 4281, 44270                                    08010034
44270 IF (IVCOMP-15249) 24270,14270,24270                               08020034
14270 IVPASS = IVPASS + 1                                               08030034
      WRITE (I02,80001) IVTNUM                                          08040034
      GO TO 4281                                                        08050034
24270 IVFAIL = IVFAIL + 1                                               08060034
      IVCORR = 15249                                                    08070034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08080034
 4281 CONTINUE                                                          08090034
      IVTNUM = 428                                                      08100034
C                                                                       08110034
C       ****  TEST 428  ****                                            08120034
C                                                                       08130034
      IF (ICZERO) 34280, 4280, 34280                                    08140034
 4280 CONTINUE                                                          08150034
      IVON02 = -51                                                      08160034
      IVCOMP = -23 * (IVON02*(+13))                                     08170034
      GO TO 44280                                                       08180034
34280 IVDELE = IVDELE + 1                                               08190034
      WRITE (I02,80003) IVTNUM                                          08200034
      IF (ICZERO) 44280, 4291, 44280                                    08210034
44280 IF (IVCOMP - 15249)24280,14280,24280                              08220034
14280 IVPASS = IVPASS + 1                                               08230034
      WRITE (I02,80001) IVTNUM                                          08240034
      GO TO 4291                                                        08250034
24280 IVFAIL = IVFAIL + 1                                               08260034
      IVCORR = 15249                                                    08270034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08280034
 4291 CONTINUE                                                          08290034
      IVTNUM = 429                                                      08300034
C                                                                       08310034
C       ****  TEST 429  ****                                            08320034
C                                                                       08330034
      IF (ICZERO) 34290, 4290, 34290                                    08340034
 4290 CONTINUE                                                          08350034
      IVON02 = -51                                                      08360034
      IVCOMP = (-23)*(IVON02*(-13))                                     08370034
      GO TO 44290                                                       08380034
34290 IVDELE = IVDELE + 1                                               08390034
      WRITE (I02,80003) IVTNUM                                          08400034
      IF (ICZERO) 44290, 4301, 44290                                    08410034
44290 IF (IVCOMP + 15249) 24290,14290,24290                             08420034
14290 IVPASS = IVPASS + 1                                               08430034
      WRITE (I02,80001) IVTNUM                                          08440034
      GO TO 4301                                                        08450034
24290 IVFAIL = IVFAIL + 1                                               08460034
      IVCORR = -15249                                                   08470034
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08480034
C     ****   END OF TESTS   ****                                        08490034
 4301 CONTINUE                                                          08500034
C                                                                       08510034
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08520034
99999 CONTINUE                                                          08530034
      WRITE (I02,90002)                                                 08540034
      WRITE (I02,90006)                                                 08550034
      WRITE (I02,90002)                                                 08560034
      WRITE (I02,90002)                                                 08570034
      WRITE (I02,90007)                                                 08580034
      WRITE (I02,90002)                                                 08590034
      WRITE (I02,90008)  IVFAIL                                         08600034
      WRITE (I02,90009) IVPASS                                          08610034
      WRITE (I02,90010) IVDELE                                          08620034
C                                                                       08630034
C                                                                       08640034
C     TERMINATE ROUTINE EXECUTION                                       08650034
      STOP                                                              08660034
C                                                                       08670034
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08680034
90000 FORMAT (1H1)                                                      08690034
90002 FORMAT (1H )                                                      08700034
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08710034
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08720034
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08730034
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08740034
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08750034
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08760034
C                                                                       08770034
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08780034
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08790034
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08800034
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08810034
C                                                                       08820034
C     FORMAT STATEMENTS FOR TEST RESULTS                                08830034
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08840034
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08850034
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08860034
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08870034
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08880034
C                                                                       08890034
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM034)                          08900034
      END                                                               08910034
