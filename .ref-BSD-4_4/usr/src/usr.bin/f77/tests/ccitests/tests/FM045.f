C     COMMENT SECTION                                                   00010045
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020045
C     FM045                                                             00030045
C                                                                       00040045
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS USING INTEGER       00050045
C     VARIABLES CONNECTED BY A SERIES OF ARITHMETIC OPERATORS.          00060045
C     DIFFERENT COMBINATIONS OF PARENTHETICAL NOTATION ARE EXERCIZED.   00070045
C                                                                       00080045
C                                                                       00090045
C      REFERENCES                                                       00100045
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110045
C              X3.9-1978                                                00120045
C                                                                       00130045
C        SECTION 4.3, INTEGER TYPE                                      00140045
C        SECTION 4.3.1, INTEGER CONSTANT                                00150045
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00160045
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00170045
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00180045
C                                                                       00190045
C                                                                       00200045
C                                                                       00210045
C      **********************************************************       00220045
C                                                                       00230045
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00240045
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00250045
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00260045
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00270045
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00280045
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00290045
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00300045
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00310045
C     OF EXECUTING THESE TESTS.                                         00320045
C                                                                       00330045
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00340045
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00350045
C                                                                       00360045
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00370045
C                                                                       00380045
C                  DEPARTMENT OF THE NAVY                               00390045
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00400045
C                  WASHINGTON, D.C.  20376                              00410045
C                                                                       00420045
C      **********************************************************       00430045
C                                                                       00440045
C                                                                       00450045
C                                                                       00460045
C     INITIALIZATION SECTION                                            00470045
C                                                                       00480045
C     INITIALIZE CONSTANTS                                              00490045
C      **************                                                   00500045
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00510045
      I01 = 5                                                           00520045
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00530045
      I02 = 6                                                           00540045
C     SYSTEM ENVIRONMENT SECTION                                        00550045
C                                                                       00560045
      I01 = 5                                                           00570045
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00580045
C     (UNIT NUMBER FOR CARD READER).                                    00590045
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00600045
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00610045
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00620045
C                                                                       00630045
      I02 = 6                                                           00640045
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00650045
C     (UNIT NUMBER FOR PRINTER).                                        00660045
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00670045
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00680045
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00690045
C                                                                       00700045
      IVPASS=0                                                          00710045
      IVFAIL=0                                                          00720045
      IVDELE=0                                                          00730045
      ICZERO=0                                                          00740045
C                                                                       00750045
C     WRITE PAGE HEADERS                                                00760045
      WRITE (I02,90000)                                                 00770045
      WRITE (I02,90001)                                                 00780045
      WRITE (I02,90002)                                                 00790045
      WRITE (I02, 90002)                                                00800045
      WRITE (I02,90003)                                                 00810045
      WRITE (I02,90002)                                                 00820045
      WRITE (I02,90004)                                                 00830045
      WRITE (I02,90002)                                                 00840045
      WRITE (I02,90011)                                                 00850045
      WRITE (I02,90002)                                                 00860045
      WRITE (I02,90002)                                                 00870045
      WRITE (I02,90005)                                                 00880045
      WRITE (I02,90006)                                                 00890045
      WRITE (I02,90002)                                                 00900045
C                                                                       00910045
C                                                                       00920045
C     TEST SECTION                                                      00930045
C                                                                       00940045
C         ARITHMETIC ASSIGNMENT STATEMENT                               00950045
C                                                                       00960045
C                                                                       00970045
C     TESTS 747 THROUGH 755 USE THE SAME STRING OF VARIABLES AND        00980045
C     OPERATORS, BUT USE DIFFERENT COMBINATIONS OF PARENTHETICAL        00990045
C     NOTATION  TO ALTER PRIORITIES IN ORDER OF EVALUATION.             01000045
C                                                                       01010045
C     TESTS 756 THROUGH 759 CHECK THE CAPABILITY TO ENCLOSE THE ENTIRE  01020045
C     RIGHT HAND SIDE OF AN ASSIGNMENT STATEMENT IN PARENTHESES OR SETS 01030045
C     OF NESTED PARENTHESES.                                            01040045
C                                                                       01050045
C                                                                       01060045
C                                                                       01070045
C                                                                       01080045
C                                                                       01090045
C                                                                       01100045
C                                                                       01110045
      IVTNUM = 747                                                      01120045
C                                                                       01130045
C      ****  TEST 747  ****                                             01140045
C                                                                       01150045
      IF (ICZERO) 37470, 7470, 37470                                    01160045
 7470 CONTINUE                                                          01170045
      IVON01 = 15                                                       01180045
      IVON02 =  9                                                       01190045
      IVON03 =  4                                                       01200045
      IVON04 = 18                                                       01210045
      IVON05 =  6                                                       01220045
      IVON06 =  2                                                       01230045
      IVCOMP = IVON01 + IVON02 - IVON03 * IVON04 / IVON05 ** IVON06     01240045
      GO TO 47470                                                       01250045
37470 IVDELE = IVDELE + 1                                               01260045
      WRITE (I02,80003) IVTNUM                                          01270045
      IF (ICZERO) 47470, 7481, 47470                                    01280045
47470 IF (IVCOMP - 22) 27470,17470,27470                                01290045
17470 IVPASS = IVPASS + 1                                               01300045
      WRITE (I02,80001) IVTNUM                                          01310045
      GO TO 7481                                                        01320045
27470 IVFAIL = IVFAIL + 1                                               01330045
      IVCORR = 22                                                       01340045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01350045
 7481 CONTINUE                                                          01360045
      IVTNUM = 748                                                      01370045
C                                                                       01380045
C      ****  TEST 748  ****                                             01390045
C                                                                       01400045
      IF (ICZERO) 37480, 7480, 37480                                    01410045
 7480 CONTINUE                                                          01420045
      IVON01 = 15                                                       01430045
      IVON02 =  9                                                       01440045
      IVON03 =  4                                                       01450045
      IVON04 = 18                                                       01460045
      IVON05 =  6                                                       01470045
      IVON06 =  2                                                       01480045
      IVCOMP = ((((IVON01 + IVON02) - IVON03) * IVON04) / IVON05)       01490045
     *         ** IVON06                                                01500045
      GO TO 47480                                                       01510045
37480 IVDELE = IVDELE + 1                                               01520045
      WRITE (I02,80003) IVTNUM                                          01530045
      IF (ICZERO) 47480, 7491, 47480                                    01540045
47480 IF (IVCOMP - 3600) 27480,17480,27480                              01550045
17480 IVPASS = IVPASS + 1                                               01560045
      WRITE (I02,80001) IVTNUM                                          01570045
      GO TO 7491                                                        01580045
27480 IVFAIL = IVFAIL + 1                                               01590045
      IVCORR = 3600                                                     01600045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01610045
 7491 CONTINUE                                                          01620045
      IVTNUM = 749                                                      01630045
C                                                                       01640045
C      ****  TEST 749  ****                                             01650045
C                                                                       01660045
      IF (ICZERO) 37490, 7490, 37490                                    01670045
 7490 CONTINUE                                                          01680045
      IVON01 = 15                                                       01690045
      IVON02 =  9                                                       01700045
      IVON03 =  4                                                       01710045
      IVON04 = 36                                                       01720045
      IVON05 =  6                                                       01730045
      IVON06 =  2                                                       01740045
      IVCOMP = (IVON01 + IVON02 - IVON03) * (IVON04 / IVON05 ** IVON06) 01750045
      GO TO 47490                                                       01760045
37490 IVDELE = IVDELE + 1                                               01770045
      WRITE (I02,80003) IVTNUM                                          01780045
      IF (ICZERO) 47490, 7501, 47490                                    01790045
47490 IF (IVCOMP - 20) 27490,17490,27490                                01800045
17490 IVPASS = IVPASS + 1                                               01810045
      WRITE (I02,80001) IVTNUM                                          01820045
      GO TO 7501                                                        01830045
27490 IVFAIL = IVFAIL + 1                                               01840045
      IVCORR = 20                                                       01850045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01860045
 7501 CONTINUE                                                          01870045
      IVTNUM = 750                                                      01880045
C                                                                       01890045
C      ****  TEST 750  ****                                             01900045
C                                                                       01910045
      IF (ICZERO) 37500, 7500, 37500                                    01920045
 7500 CONTINUE                                                          01930045
      IVON01 = 15                                                       01940045
      IVON02 =  9                                                       01950045
      IVON03 =  4                                                       01960045
      IVON04 = 36                                                       01970045
      IVON05 =  6                                                       01980045
      IVON06 =  2                                                       01990045
      IVCOMP = (IVON01 + IVON02) - (IVON03 * IVON04) / (IVON05 **       02000045
     *         IVON06)                                                  02010045
      GO TO 47500                                                       02020045
37500 IVDELE = IVDELE + 1                                               02030045
      WRITE (I02,80003) IVTNUM                                          02040045
      IF (ICZERO) 47500, 7511, 47500                                    02050045
47500 IF (IVCOMP - 20) 27500,17500,27500                                02060045
17500 IVPASS = IVPASS + 1                                               02070045
      WRITE (I02,80001) IVTNUM                                          02080045
      GO TO 7511                                                        02090045
27500 IVFAIL = IVFAIL + 1                                               02100045
      IVCORR = 20                                                       02110045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02120045
 7511 CONTINUE                                                          02130045
      IVTNUM = 751                                                      02140045
C                                                                       02150045
C      ****  TEST 751  ****                                             02160045
C                                                                       02170045
      IF (ICZERO) 37510, 7510, 37510                                    02180045
 7510 CONTINUE                                                          02190045
      IVON01 = 15                                                       02200045
      IVON02 =  9                                                       02210045
      IVON03 =  4                                                       02220045
      IVON04 = 36                                                       02230045
      IVON05 =  6                                                       02240045
      IVON06 =  2                                                       02250045
      IVCOMP = ((IVON01 + IVON02) - (IVON03 * IVON04)) / (IVON05 **     02260045
     *         IVON06)                                                  02270045
      GO TO 47510                                                       02280045
37510 IVDELE = IVDELE + 1                                               02290045
      WRITE (I02,80003) IVTNUM                                          02300045
      IF (ICZERO)  47510, 7521, 47510                                   02310045
47510 IF (IVCOMP + 3)  27510,17510,27510                                02320045
17510 IVPASS = IVPASS + 1                                               02330045
      WRITE (I02,80001) IVTNUM                                          02340045
      GO TO 7521                                                        02350045
27510 IVFAIL = IVFAIL + 1                                               02360045
      IVCORR = -3                                                       02370045
C     ACTUAL ANSWER IS  -3.333333...     TRUNCATION IS NECESSARY        02380045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02390045
 7521 CONTINUE                                                          02400045
      IVTNUM = 752                                                      02410045
C                                                                       02420045
C      ****  TEST 752  ****                                             02430045
C                                                                       02440045
      IF (ICZERO) 37520, 7520, 37520                                    02450045
 7520 CONTINUE                                                          02460045
      IVON01 = 15                                                       02470045
      IVON02 =  9                                                       02480045
      IVON03 =  4                                                       02490045
      IVON04 = 36                                                       02500045
      IVON05 =  6                                                       02510045
      IVON06 =  2                                                       02520045
      IVCOMP = (IVON01 + IVON02) - (IVON03 * IVON04 / IVON05) ** IVON06 02530045
      GO TO 47520                                                       02540045
37520 IVDELE = IVDELE + 1                                               02550045
      WRITE (I02,80003) IVTNUM                                          02560045
      IF (ICZERO) 47520, 7531, 47520                                    02570045
47520 IF (IVCOMP + 552) 27520,17520,27520                               02580045
17520 IVPASS = IVPASS + 1                                               02590045
      WRITE (I02,80001) IVTNUM                                          02600045
      GO TO 7531                                                        02610045
27520 IVFAIL = IVFAIL + 1                                               02620045
      IVCORR = -552                                                     02630045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02640045
 7531 CONTINUE                                                          02650045
      IVTNUM = 753                                                      02660045
C                                                                       02670045
C      ****  TEST 753  ****                                             02680045
C                                                                       02690045
      IF (ICZERO) 37530, 7530, 37530                                    02700045
 7530 CONTINUE                                                          02710045
      IVON01 = 15                                                       02720045
      IVON02 =  9                                                       02730045
      IVON03 =  4                                                       02740045
      IVON04 = 36                                                       02750045
      IVON05 =  6                                                       02760045
      IVON06 =  2                                                       02770045
      IVCOMP = IVON01 + (IVON02 - IVON03 * IVON04) / IVON05 ** IVON06   02780045
      GO TO 47530                                                       02790045
37530 IVDELE = IVDELE + 1                                               02800045
      WRITE (I02,80003) IVTNUM                                          02810045
      IF (ICZERO) 47530, 7541, 47530                                    02820045
47530 IF (IVCOMP - 12) 27530,17530,27530                                02830045
17530 IVPASS = IVPASS + 1                                               02840045
      WRITE (I02,80001) IVTNUM                                          02850045
      GO TO 7541                                                        02860045
27530 IVFAIL = IVFAIL + 1                                               02870045
      IVCORR = 12                                                       02880045
C     ACTUAL ANSWER IS  11.25            TRUNCATION IS NECESSARY        02890045
C                                        DURING AN INTERMEDIATE STEP    02900045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02910045
 7541 CONTINUE                                                          02920045
      IVTNUM = 754                                                      02930045
C                                                                       02940045
C      ****  TEST 754  ****                                             02950045
C                                                                       02960045
      IF (ICZERO) 37540, 7540, 37540                                    02970045
 7540 CONTINUE                                                          02980045
      IVON01 = 15                                                       02990045
      IVON02 =  9                                                       03000045
      IVON03 =  4                                                       03010045
      IVON04 = 36                                                       03020045
      IVON05 =  6                                                       03030045
      IVON06 =  2                                                       03040045
      IVCOMP = IVON01 + (IVON02 - IVON03) * (IVON04 / IVON05) ** IVON06 03050045
      GO TO 47540                                                       03060045
37540 IVDELE = IVDELE + 1                                               03070045
      WRITE (I02,80003) IVTNUM                                          03080045
      IF (ICZERO) 47540, 7551, 47540                                    03090045
47540 IF (IVCOMP - 195) 27540,17540,27540                               03100045
17540 IVPASS = IVPASS + 1                                               03110045
      WRITE (I02,80001) IVTNUM                                          03120045
      GO TO 7551                                                        03130045
27540 IVFAIL = IVFAIL + 1                                               03140045
      IVCORR = 195                                                      03150045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03160045
 7551 CONTINUE                                                          03170045
      IVTNUM = 755                                                      03180045
C                                                                       03190045
C      ****  TEST 755  ****                                             03200045
C                                                                       03210045
      IF (ICZERO) 37550, 7550, 37550                                    03220045
 7550 CONTINUE                                                          03230045
      IVON01 = 15                                                       03240045
      IVON02 =  9                                                       03250045
      IVON03 =  4                                                       03260045
      IVON04 = 36                                                       03270045
      IVON05 =  6                                                       03280045
      IVON06 =  2                                                       03290045
      IVCOMP = ((IVON01 + (IVON02 - IVON03) * IVON04) / IVON05) **      03300045
     *         IVON06                                                   03310045
      GO TO 47550                                                       03320045
37550 IVDELE = IVDELE + 1                                               03330045
      WRITE (I02,80003) IVTNUM                                          03340045
      IF (ICZERO) 47550, 7561, 47550                                    03350045
47550 IF (IVCOMP - 1024)  27550,17550,27550                             03360045
17550 IVPASS = IVPASS + 1                                               03370045
      WRITE (I02,80001) IVTNUM                                          03380045
      GO TO 7561                                                        03390045
27550 IVFAIL = IVFAIL + 1                                               03400045
      IVCORR = 1024                                                     03410045
C     ACTUAL ANSWER IS  1056.25         TRUNCATION IS NECESSARY         03420045
C                                       DURING AN INTERMEDIATE STEP     03430045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03440045
 7561 CONTINUE                                                          03450045
      IVTNUM = 756                                                      03460045
C                                                                       03470045
C      ****  TEST 756  ****                                             03480045
C          SINGLE PARENTHESES                                           03490045
C                                                                       03500045
      IF (ICZERO) 37560, 7560, 37560                                    03510045
 7560 CONTINUE                                                          03520045
      IVON01 = 13                                                       03530045
      IVON02 = 37                                                       03540045
      IVCOMP = (IVON01 + IVON02)                                        03550045
      GO TO 47560                                                       03560045
37560 IVDELE = IVDELE + 1                                               03570045
      WRITE (I02,80003) IVTNUM                                          03580045
      IF (ICZERO) 47560, 7571, 47560                                    03590045
47560 IF (IVCOMP - 50) 27560,17560,27560                                03600045
17560 IVPASS = IVPASS + 1                                               03610045
      WRITE (I02,80001) IVTNUM                                          03620045
      GO TO 7571                                                        03630045
27560 IVFAIL = IVFAIL + 1                                               03640045
      IVCORR = 50                                                       03650045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03660045
 7571 CONTINUE                                                          03670045
      IVTNUM = 757                                                      03680045
C                                                                       03690045
C      ****  TEST 757  ****                                             03700045
C          NESTED PARENTHESES (TWO SETS)                                03710045
C                                                                       03720045
      IF (ICZERO) 37570, 7570, 37570                                    03730045
 7570 CONTINUE                                                          03740045
      IVON01 = 13                                                       03750045
      IVON02 = 37                                                       03760045
      IVCOMP = ((IVON01 - IVON02))                                      03770045
      GO TO 47570                                                       03780045
37570 IVDELE = IVDELE + 1                                               03790045
      WRITE (I02,80003) IVTNUM                                          03800045
      IF (ICZERO) 47570, 7581, 47570                                    03810045
47570 IF (IVCOMP + 24) 27570,17570,27570                                03820045
17570 IVPASS = IVPASS + 1                                               03830045
      WRITE (I02,80001) IVTNUM                                          03840045
      GO TO 7581                                                        03850045
27570 IVFAIL = IVFAIL + 1                                               03860045
      IVCORR = -24                                                      03870045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03880045
 7581 CONTINUE                                                          03890045
      IVTNUM = 758                                                      03900045
C                                                                       03910045
C      ****  TEST 758  ****                                             03920045
C          NESTED PARENTHESES (21 SETS - SAME LINE)                     03930045
C                                                                       03940045
      IF (ICZERO) 37580, 7580, 37580                                    03950045
 7580 CONTINUE                                                          03960045
      IVON01 = 13                                                       03970045
      IVON02 = 37                                                       03980045
      IVCOMP = (((((((((((((((((((((IVON01 * IVON02)))))))))))))))))))))03990045
      GO TO 47580                                                       04000045
37580 IVDELE = IVDELE + 1                                               04010045
      WRITE (I02,80003) IVTNUM                                          04020045
      IF (ICZERO) 47580, 7591, 47580                                    04030045
47580 IF (IVCOMP - 481) 27580,17580,27580                               04040045
17580 IVPASS = IVPASS + 1                                               04050045
      WRITE (I02,80001) IVTNUM                                          04060045
      GO TO 7591                                                        04070045
27580 IVFAIL = IVFAIL + 1                                               04080045
      IVCORR = 481                                                      04090045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04100045
 7591 CONTINUE                                                          04110045
      IVTNUM = 759                                                      04120045
C                                                                       04130045
C      ****  TEST 759  ****                                             04140045
C          NESTED PARENTHESES (57 SETS - MULTIPLE LINES)                04150045
C                                                                       04160045
      IF (ICZERO) 37590, 7590, 37590                                    04170045
 7590 CONTINUE                                                          04180045
      IVON01 = 13                                                       04190045
      IVON02 = 37                                                       04200045
      IVCOMP = (((((((((((((((((((((((((((((((((((((((((((((((((((((((((04210045
     *         IVON01 / IVON02                                          04220045
     *         )))))))))))))))))))))))))))))))))))))))))))))))))))))))))04230045
      GO TO 47590                                                       04240045
37590 IVDELE = IVDELE + 1                                               04250045
      WRITE (I02,80003) IVTNUM                                          04260045
      IF (ICZERO) 47590, 7601, 47590                                    04270045
47590 IF (IVCOMP) 27590,17590,27590                                     04280045
17590 IVPASS = IVPASS + 1                                               04290045
      WRITE (I02,80001) IVTNUM                                          04300045
      GO TO 7601                                                        04310045
27590 IVFAIL = IVFAIL + 1                                               04320045
      IVCORR = 0                                                        04330045
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04340045
 7601 CONTINUE                                                          04350045
C                                                                       04360045
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             04370045
99999 CONTINUE                                                          04380045
      WRITE (I02,90002)                                                 04390045
      WRITE (I02,90006)                                                 04400045
      WRITE (I02,90002)                                                 04410045
      WRITE (I02,90002)                                                 04420045
      WRITE (I02,90007)                                                 04430045
      WRITE (I02,90002)                                                 04440045
      WRITE (I02,90008)  IVFAIL                                         04450045
      WRITE (I02,90009) IVPASS                                          04460045
      WRITE (I02,90010) IVDELE                                          04470045
C                                                                       04480045
C                                                                       04490045
C     TERMINATE ROUTINE EXECUTION                                       04500045
      STOP                                                              04510045
C                                                                       04520045
C     FORMAT STATEMENTS FOR PAGE HEADERS                                04530045
90000 FORMAT (1H1)                                                      04540045
90002 FORMAT (1H )                                                      04550045
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04560045
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   04570045
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        04580045
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 04590045
90006 FORMAT (1H ,5X,46H----------------------------------------------) 04600045
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             04610045
C                                                                       04620045
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               04630045
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        04640045
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              04650045
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             04660045
C                                                                       04670045
C     FORMAT STATEMENTS FOR TEST RESULTS                                04680045
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04690045
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      04700045
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   04710045
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04720045
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    04730045
C                                                                       04740045
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM045)                          04750045
      END                                                               04760045
