C     COMMENT SECTION.                                                  00010030
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020030
C     FM030                                                             00030030
C                                                                       00040030
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050030
C     FORM                                                              00060030
C               INTEGER VARIABLE = ARITHMETIC EXPRESSION                00070030
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080030
C     OPERATOR -, INTEGER CONSTANTS AND INTEGER VARIABLES.              00090030
C     SOME OF THE TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE        00100030
C     ARITHMETIC EXPRESSION.                                            00110030
C                                                                       00120030
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00130030
C            (1)  INTEGER CONSTANT - INTEGER CONSTANT                   00140030
C            (2)  INTEGER CONSTANT - INTEGER CONSTANT - INTEGER CONSTANT00150030
C            (3)  SAME AS (2) BUT WITH PARENTHESES TO GROUP ELEMENTS    00160030
C            (4)  INTEGER VARIABLE - INTEGER CONSTANT                   00170030
C                 INTEGER CONSTANT - INTEGER VARIABLE                   00180030
C                                                                       00190030
C      REFERENCES                                                       00200030
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00210030
C              X3.9-1978                                                00220030
C                                                                       00230030
C        SECTION 4.3, INTEGER TYPE                                      00240030
C        SECTION 4.3.1, INTEGER CONSTANT                                00250030
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00260030
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00270030
C                                                                       00280030
C                                                                       00290030
C      **********************************************************       00300030
C                                                                       00310030
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00320030
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00330030
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00340030
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00350030
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00360030
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00370030
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00380030
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00390030
C     OF EXECUTING THESE TESTS.                                         00400030
C                                                                       00410030
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00420030
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00430030
C                                                                       00440030
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00450030
C                                                                       00460030
C                  DEPARTMENT OF THE NAVY                               00470030
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00480030
C                  WASHINGTON, D.C.  20376                              00490030
C                                                                       00500030
C      **********************************************************       00510030
C                                                                       00520030
C                                                                       00530030
C                                                                       00540030
C     INITIALIZATION SECTION                                            00550030
C                                                                       00560030
C     INITIALIZE CONSTANTS                                              00570030
C      **************                                                   00580030
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00590030
      I01 = 5                                                           00600030
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00610030
      I02 = 6                                                           00620030
C     SYSTEM ENVIRONMENT SECTION                                        00630030
C                                                                       00640030
      I01 = 5                                                           00650030
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00660030
C     (UNIT NUMBER FOR CARD READER).                                    00670030
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00680030
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00690030
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00700030
C                                                                       00710030
      I02 = 6                                                           00720030
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00730030
C     (UNIT NUMBER FOR PRINTER).                                        00740030
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00750030
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760030
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00770030
C                                                                       00780030
      IVPASS=0                                                          00790030
      IVFAIL=0                                                          00800030
      IVDELE=0                                                          00810030
      ICZERO=0                                                          00820030
C                                                                       00830030
C     WRITE PAGE HEADERS                                                00840030
      WRITE (I02,90000)                                                 00850030
      WRITE (I02,90001)                                                 00860030
      WRITE (I02,90002)                                                 00870030
      WRITE (I02, 90002)                                                00880030
      WRITE (I02,90003)                                                 00890030
      WRITE (I02,90002)                                                 00900030
      WRITE (I02,90004)                                                 00910030
      WRITE (I02,90002)                                                 00920030
      WRITE (I02,90011)                                                 00930030
      WRITE (I02,90002)                                                 00940030
      WRITE (I02,90002)                                                 00950030
      WRITE (I02,90005)                                                 00960030
      WRITE (I02,90006)                                                 00970030
      WRITE (I02,90002)                                                 00980030
C     TEST SECTION                                                      00990030
C                                                                       01000030
C         ARITHMETIC ASSIGNMENT STATEMENT                               01010030
C                                                                       01020030
C         TEST 265 THROUGH TEST 270 CONTAIN TWO INTEGER CONSTANTS AND   01030030
C     OPERATOR - IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS       01040030
C          INTEGER VARIABLE = INTEGER CONSTANT - INTEGER CONSTANT       01050030
C                                                                       01060030
 2651 CONTINUE                                                          01070030
      IVTNUM = 265                                                      01080030
C                                                                       01090030
C      ****  TEST 265  ****                                             01100030
C                                                                       01110030
      IF (ICZERO) 32650, 2650, 32650                                    01120030
 2650 CONTINUE                                                          01130030
      IVCOMP = 3-2                                                      01140030
      GO TO 42650                                                       01150030
32650 IVDELE = IVDELE + 1                                               01160030
      WRITE (I02,80003) IVTNUM                                          01170030
      IF (ICZERO) 42650, 2661, 42650                                    01180030
42650 IF (IVCOMP - 1) 22650,12650,22650                                 01190030
12650 IVPASS = IVPASS + 1                                               01200030
      WRITE (I02,80001) IVTNUM                                          01210030
      GO TO 2661                                                        01220030
22650 IVFAIL = IVFAIL + 1                                               01230030
      IVCORR = 1                                                        01240030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01250030
 2661 CONTINUE                                                          01260030
      IVTNUM = 266                                                      01270030
C                                                                       01280030
C      ****  TEST 266  ****                                             01290030
C                                                                       01300030
      IF (ICZERO) 32660, 2660, 32660                                    01310030
 2660 CONTINUE                                                          01320030
      IVCOMP = 51 - 52                                                  01330030
      GO TO 42660                                                       01340030
32660 IVDELE = IVDELE + 1                                               01350030
      WRITE (I02,80003) IVTNUM                                          01360030
      IF (ICZERO) 42660, 2671, 42660                                    01370030
42660 IF (IVCOMP +1) 22660,12660,22660                                  01380030
12660 IVPASS = IVPASS + 1                                               01390030
      WRITE (I02,80001) IVTNUM                                          01400030
      GO TO 2671                                                        01410030
22660 IVFAIL = IVFAIL + 1                                               01420030
      IVCORR = -1                                                       01430030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01440030
 2671 CONTINUE                                                          01450030
      IVTNUM = 267                                                      01460030
C                                                                       01470030
C      ****  TEST 267  ***                                              01480030
C                                                                       01490030
      IF (ICZERO) 32670, 2670, 32670                                    01500030
 2670 CONTINUE                                                          01510030
      IVCOMP = 865 - 189                                                01520030
      GO TO 42670                                                       01530030
32670 IVDELE = IVDELE + 1                                               01540030
      WRITE (I02,80003) IVTNUM                                          01550030
      IF (ICZERO) 42670, 2681, 42670                                    01560030
42670 IF (IVCOMP -676) 22670,12670,22670                                01570030
12670 IVPASS = IVPASS + 1                                               01580030
      WRITE (I02,80001) IVTNUM                                          01590030
      GO TO 2681                                                        01600030
22670 IVFAIL = IVFAIL + 1                                               01610030
      IVCORR = 676                                                      01620030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01630030
 2681 CONTINUE                                                          01640030
      IVTNUM = 268                                                      01650030
C                                                                       01660030
C      ****  TEST 268  ****                                             01670030
C                                                                       01680030
      IF (ICZERO) 32680, 2680, 32680                                    01690030
 2680 CONTINUE                                                          01700030
      IVCOMP =1358-9359                                                 01710030
      GO TO 42680                                                       01720030
32680 IVDELE = IVDELE + 1                                               01730030
      WRITE (I02,80003) IVTNUM                                          01740030
      IF (ICZERO) 42680, 2691, 42680                                    01750030
42680 IF (IVCOMP+8001) 22680,12680,22680                                01760030
12680 IVPASS = IVPASS + 1                                               01770030
      WRITE (I02,80001) IVTNUM                                          01780030
      GO TO 2691                                                        01790030
22680 IVFAIL = IVFAIL + 1                                               01800030
      IVCORR = -8001                                                    01810030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01820030
 2691 CONTINUE                                                          01830030
      IVTNUM = 269                                                      01840030
C                                                                       01850030
C      ****  TEST 269  ****                                             01860030
C                                                                       01870030
      IF (ICZERO) 32690, 2690, 32690                                    01880030
 2690 CONTINUE                                                          01890030
      IVCOMP =21113-10001                                               01900030
      GO TO 42690                                                       01910030
32690 IVDELE = IVDELE + 1                                               01920030
      WRITE (I02,80003) IVTNUM                                          01930030
      IF (ICZERO) 42690, 2701, 42690                                    01940030
42690 IF (IVCOMP-11112) 22690,12690,22690                               01950030
12690 IVPASS = IVPASS + 1                                               01960030
      WRITE (I02,80001) IVTNUM                                          01970030
      GO TO 2701                                                        01980030
22690 IVFAIL = IVFAIL + 1                                               01990030
      IVCORR=11112                                                      02000030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02010030
 2701 CONTINUE                                                          02020030
      IVTNUM = 270                                                      02030030
C                                                                       02040030
C      ****  TEST 270  ****                                             02050030
C                                                                       02060030
      IF (ICZERO) 32700, 2700, 32700                                    02070030
 2700 CONTINUE                                                          02080030
      IVCOMP = 32767-1                                                  02090030
      GO TO 42700                                                       02100030
32700 IVDELE = IVDELE + 1                                               02110030
      WRITE (I02,80003) IVTNUM                                          02120030
      IF (ICZERO) 42700, 2711, 42700                                    02130030
42700 IF (IVCOMP -32766) 22700,12700,22700                              02140030
12700 IVPASS = IVPASS + 1                                               02150030
      WRITE (I02,80001) IVTNUM                                          02160030
      GO TO 2711                                                        02170030
22700 IVFAIL = IVFAIL + 1                                               02180030
      IVCORR = 32766                                                    02190030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02200030
C                                                                       02210030
C         TEST 271 THROUGH TEST 274 CONTAIN THREE INTEGER CONSTANTS     02220030
C     AND OPERATOR - IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS   02230030
C                       IV = IC - IC - IC                               02240030
C                                                                       02250030
 2711 CONTINUE                                                          02260030
      IVTNUM = 271                                                      02270030
C                                                                       02280030
C      ****  TEST 271  ****                                             02290030
C                                                                       02300030
      IF (ICZERO) 32710, 2710, 32710                                    02310030
 2710 CONTINUE                                                          02320030
      IVCOMP=9-4-3                                                      02330030
      GO TO 42710                                                       02340030
32710 IVDELE = IVDELE + 1                                               02350030
      WRITE (I02,80003) IVTNUM                                          02360030
      IF (ICZERO) 42710, 2721, 42710                                    02370030
42710 IF (IVCOMP -2) 22710,12710,22710                                  02380030
12710 IVPASS = IVPASS + 1                                               02390030
      WRITE (I02,80001) IVTNUM                                          02400030
      GO TO 2721                                                        02410030
22710 IVFAIL = IVFAIL + 1                                               02420030
      IVCORR =2                                                         02430030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02440030
 2721 CONTINUE                                                          02450030
      IVTNUM = 272                                                      02460030
C                                                                       02470030
C      ****  TEST 272 ****                                              02480030
C                                                                       02490030
      IF (ICZERO) 32720, 2720, 32720                                    02500030
 2720 CONTINUE                                                          02510030
      IVCOMP = 51-52-53                                                 02520030
      GO TO 42720                                                       02530030
32720 IVDELE = IVDELE + 1                                               02540030
      WRITE (I02,80003) IVTNUM                                          02550030
      IF (ICZERO) 42720, 2731, 42720                                    02560030
42720 IF (IVCOMP +54) 22720,12720,22720                                 02570030
12720 IVPASS = IVPASS + 1                                               02580030
      WRITE (I02,80001) IVTNUM                                          02590030
      GO TO 2731                                                        02600030
22720 IVFAIL = IVFAIL + 1                                               02610030
      IVCORR = -54                                                      02620030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02630030
 2731 CONTINUE                                                          02640030
      IVTNUM = 273                                                      02650030
C                                                                       02660030
C      ****  TEST 273  ****                                             02670030
C                                                                       02680030
      IF (ICZERO) 32730, 2730, 32730                                    02690030
 2730 CONTINUE                                                          02700030
      IVCOMP = 966 -676 -189                                            02710030
      GO TO 42730                                                       02720030
32730 IVDELE = IVDELE + 1                                               02730030
      WRITE (I02,80003) IVTNUM                                          02740030
      IF (ICZERO) 42730, 2741, 42730                                    02750030
42730 IF (IVCOMP -101) 22730,12730,22730                                02760030
12730 IVPASS = IVPASS + 1                                               02770030
      WRITE (I02,80001) IVTNUM                                          02780030
      GO TO 2741                                                        02790030
22730 IVFAIL = IVFAIL + 1                                               02800030
      IVCORR = 101                                                      02810030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02820030
 2741 CONTINUE                                                          02830030
      IVTNUM = 274                                                      02840030
C                                                                       02850030
C      ****  TEST 274  ****                                             02860030
C                                                                       02870030
      IF (ICZERO) 32740, 2740, 32740                                    02880030
 2740 CONTINUE                                                          02890030
      IVCOMP = 1358-8001-2188                                           02900030
      GO TO 42740                                                       02910030
32740 IVDELE = IVDELE + 1                                               02920030
      WRITE (I02,80003) IVTNUM                                          02930030
      IF (ICZERO) 42740, 2751, 42740                                    02940030
42740 IF (IVCOMP + 8831) 22740,12740,22740                              02950030
12740 IVPASS = IVPASS + 1                                               02960030
      WRITE (I02,80001) IVTNUM                                          02970030
      GO TO 2751                                                        02980030
22740 IVFAIL = IVFAIL + 1                                               02990030
      IVCORR = -8831                                                    03000030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03010030
C                                                                       03020030
C     TEST 275 THROUGH TEST 282 ARE THE SAME AS TESTS 271-274 EXCEPT    03030030
C     PARENTHESES ARE USED TO GROUP THE CONSTANTS.                      03040030
C                                                                       03050030
 2751 CONTINUE                                                          03060030
      IVTNUM = 275                                                      03070030
C                                                                       03080030
C      ****  TEST 275  ****                                             03090030
C                                                                       03100030
      IF (ICZERO) 32750, 2750, 32750                                    03110030
 2750 CONTINUE                                                          03120030
      IVCOMP =(9-4)-3                                                   03130030
      GO TO 42750                                                       03140030
32750 IVDELE = IVDELE + 1                                               03150030
      WRITE (I02,80003) IVTNUM                                          03160030
      IF (ICZERO) 42750, 2761, 42750                                    03170030
42750 IF (IVCOMP -2) 22750,12750,22750                                  03180030
12750 IVPASS = IVPASS + 1                                               03190030
      WRITE (I02,80001) IVTNUM                                          03200030
      GO TO 2761                                                        03210030
22750 IVFAIL = IVFAIL + 1                                               03220030
      IVCORR = 2                                                        03230030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03240030
 2761 CONTINUE                                                          03250030
      IVTNUM = 276                                                      03260030
C                                                                       03270030
C      ****  TEST 276  ****                                             03280030
C                                                                       03290030
      IF (ICZERO) 32760, 2760, 32760                                    03300030
 2760 CONTINUE                                                          03310030
      IVCOMP =9-(4-3)                                                   03320030
      GO TO 42760                                                       03330030
32760 IVDELE = IVDELE + 1                                               03340030
      WRITE (I02,80003) IVTNUM                                          03350030
      IF (ICZERO) 42760, 2771, 42760                                    03360030
42760 IF (IVCOMP -8) 22760,12760,22760                                  03370030
12760 IVPASS = IVPASS + 1                                               03380030
      WRITE (I02,80001) IVTNUM                                          03390030
      GO TO 2771                                                        03400030
22760 IVFAIL = IVFAIL + 1                                               03410030
      IVCORR =8                                                         03420030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03430030
 2771 CONTINUE                                                          03440030
      IVTNUM = 277                                                      03450030
C                                                                       03460030
C      ****  TEST 277  ****                                             03470030
C                                                                       03480030
      IF (ICZERO) 32770, 2770, 32770                                    03490030
 2770 CONTINUE                                                          03500030
      IVCOMP =(51-52)-53                                                03510030
      GO TO 42770                                                       03520030
32770 IVDELE = IVDELE + 1                                               03530030
      WRITE (I02,80003) IVTNUM                                          03540030
      IF (ICZERO) 42770, 2781, 42770                                    03550030
42770 IF (IVCOMP +54) 22770,12770,22770                                 03560030
12770 IVPASS = IVPASS + 1                                               03570030
      WRITE (I02,80001) IVTNUM                                          03580030
      GO TO 2781                                                        03590030
22770 IVFAIL = IVFAIL + 1                                               03600030
      IVCORR = -54                                                      03610030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03620030
 2781 CONTINUE                                                          03630030
      IVTNUM = 278                                                      03640030
C                                                                       03650030
C      ****  TEST 278  ****                                             03660030
C                                                                       03670030
      IF (ICZERO) 32780, 2780, 32780                                    03680030
 2780 CONTINUE                                                          03690030
      IVCOMP=51-(52-53)                                                 03700030
      GO TO 42780                                                       03710030
32780 IVDELE = IVDELE + 1                                               03720030
      WRITE (I02,80003) IVTNUM                                          03730030
      IF (ICZERO) 42780, 2791, 42780                                    03740030
42780 IF (IVCOMP-52) 22780,12780,22780                                  03750030
12780 IVPASS = IVPASS + 1                                               03760030
      WRITE (I02,80001) IVTNUM                                          03770030
      GO TO 2791                                                        03780030
22780 IVFAIL = IVFAIL + 1                                               03790030
      IVCORR = 52                                                       03800030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03810030
 2791 CONTINUE                                                          03820030
      IVTNUM = 279                                                      03830030
C                                                                       03840030
C      ****  TEST 279  ****                                             03850030
C                                                                       03860030
      IF (ICZERO) 32790, 2790, 32790                                    03870030
 2790 CONTINUE                                                          03880030
      IVCOMP =(966-676)-189                                             03890030
      GO TO 42790                                                       03900030
32790 IVDELE = IVDELE + 1                                               03910030
      WRITE (I02,80003) IVTNUM                                          03920030
      IF (ICZERO) 42790, 2801, 42790                                    03930030
42790 IF (IVCOMP - 101) 22790,12790,22790                               03940030
12790 IVPASS = IVPASS + 1                                               03950030
      WRITE (I02,80001) IVTNUM                                          03960030
      GO TO 2801                                                        03970030
22790 IVFAIL = IVFAIL + 1                                               03980030
      IVCORR = 101                                                      03990030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04000030
 2801 CONTINUE                                                          04010030
      IVTNUM = 280                                                      04020030
C                                                                       04030030
C      ****  TEST 280  ****                                             04040030
C                                                                       04050030
      IF (ICZERO) 32800, 2800, 32800                                    04060030
 2800 CONTINUE                                                          04070030
      IVCOMP =966-(676-189)                                             04080030
      GO TO 42800                                                       04090030
32800 IVDELE = IVDELE + 1                                               04100030
      WRITE (I02,80003) IVTNUM                                          04110030
      IF (ICZERO) 42800, 2811, 42800                                    04120030
42800 IF (IVCOMP - 479) 22800,12800,22800                               04130030
12800 IVPASS = IVPASS + 1                                               04140030
      WRITE (I02,80001) IVTNUM                                          04150030
      GO TO 2811                                                        04160030
22800 IVFAIL = IVFAIL + 1                                               04170030
      IVCORR = 479                                                      04180030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04190030
 2811 CONTINUE                                                          04200030
      IVTNUM = 281                                                      04210030
C                                                                       04220030
C      ****  TEST 281  ****                                             04230030
C                                                                       04240030
      IF (ICZERO) 32810, 2810, 32810                                    04250030
 2810 CONTINUE                                                          04260030
      IVCOMP = (1358-8001)-2188                                         04270030
      GO TO 42810                                                       04280030
32810 IVDELE = IVDELE + 1                                               04290030
      WRITE (I02,80003) IVTNUM                                          04300030
      IF (ICZERO) 42810, 2821, 42810                                    04310030
42810 IF (IVCOMP + 8831) 22810,12810,22810                              04320030
12810 IVPASS = IVPASS + 1                                               04330030
      WRITE (I02,80001) IVTNUM                                          04340030
      GO TO 2821                                                        04350030
22810 IVFAIL = IVFAIL + 1                                               04360030
      IVCORR = -8831                                                    04370030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04380030
 2821 CONTINUE                                                          04390030
      IVTNUM = 282                                                      04400030
C                                                                       04410030
C      ****  TEST 282  ****                                             04420030
C                                                                       04430030
      IF (ICZERO) 32820, 2820, 32820                                    04440030
 2820 CONTINUE                                                          04450030
      IVCOMP = 1358-(8001-2188)                                         04460030
      GO TO 42820                                                       04470030
32820 IVDELE = IVDELE + 1                                               04480030
      WRITE (I02,80003) IVTNUM                                          04490030
      IF (ICZERO) 42820, 2831, 42820                                    04500030
42820 IF (IVCOMP + 4455) 22820,12820,22820                              04510030
12820 IVPASS = IVPASS + 1                                               04520030
      WRITE (I02,80001) IVTNUM                                          04530030
      GO TO 2831                                                        04540030
22820 IVFAIL = IVFAIL + 1                                               04550030
      IVCORR = -4455                                                    04560030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04570030
C                                                                       04580030
C     TEST 283 THROUGH TEST 299 CONTAIN INTEGER VARIABLE, INTEGER       04590030
C     CONSTANT AND OPERATOR - IN ARITHMETIC EXPRESSION. THE INTEGER     04600030
C     VARIABLE CONTAINS BOTH POSITIVE AND NEGATIVE VALUES.              04610030
C     THE FORMS TESTED ARE                                              04620030
C             INTEGER VARIABLE = INTEGER VARIABLE - INTEGER CONSTANT    04630030
C             INTEGER VARIABLE = INTEGER CONSTANT - INTEGER VARIABLE    04640030
C                                                                       04650030
 2831 CONTINUE                                                          04660030
      IVTNUM = 283                                                      04670030
C                                                                       04680030
C      ****  TEST 283  ****                                             04690030
C                                                                       04700030
      IF (ICZERO) 32830, 2830, 32830                                    04710030
 2830 CONTINUE                                                          04720030
      IVON01 = 3                                                        04730030
      IVCOMP = IVON01 - 2                                               04740030
      GO TO 42830                                                       04750030
32830 IVDELE = IVDELE + 1                                               04760030
      WRITE (I02,80003) IVTNUM                                          04770030
      IF (ICZERO) 42830, 2841, 42830                                    04780030
42830 IF (IVCOMP - 1) 22830,12830,22830                                 04790030
12830 IVPASS = IVPASS + 1                                               04800030
      WRITE (I02,80001) IVTNUM                                          04810030
      GO TO 2841                                                        04820030
22830 IVFAIL = IVFAIL + 1                                               04830030
      IVCORR = 1                                                        04840030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04850030
 2841 CONTINUE                                                          04860030
      IVTNUM = 284                                                      04870030
C                                                                       04880030
C      ****  TEST 284  ****                                             04890030
C                                                                       04900030
      IF (ICZERO) 32840, 2840, 32840                                    04910030
 2840 CONTINUE                                                          04920030
      IVON01 = 2                                                        04930030
      IVCOMP = IVON01 -3                                                04940030
      GO TO 42840                                                       04950030
32840 IVDELE = IVDELE + 1                                               04960030
      WRITE (I02,80003) IVTNUM                                          04970030
      IF (ICZERO) 42840, 2851, 42840                                    04980030
42840 IF (IVCOMP +1) 22840,12840,22840                                  04990030
12840 IVPASS = IVPASS + 1                                               05000030
      WRITE (I02,80001) IVTNUM                                          05010030
      GO TO 2851                                                        05020030
22840 IVFAIL = IVFAIL + 1                                               05030030
      IVCORR = -1                                                       05040030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05050030
 2851 CONTINUE                                                          05060030
      IVTNUM = 285                                                      05070030
C                                                                       05080030
C      ****  TEST 285  ****                                             05090030
C                                                                       05100030
      IF (ICZERO) 32850, 2850, 32850                                    05110030
 2850 CONTINUE                                                          05120030
      IVON01 =-3                                                        05130030
      IVCOMP = IVON01 -2                                                05140030
      GO TO 42850                                                       05150030
32850 IVDELE = IVDELE + 1                                               05160030
      WRITE (I02,80003) IVTNUM                                          05170030
      IF (ICZERO) 42850, 2861, 42850                                    05180030
42850 IF (IVCOMP +5) 22850,12850,22850                                  05190030
12850 IVPASS = IVPASS + 1                                               05200030
      WRITE (I02,80001) IVTNUM                                          05210030
      GO TO 2861                                                        05220030
22850 IVFAIL = IVFAIL + 1                                               05230030
      IVCORR =-5                                                        05240030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05250030
 2861 CONTINUE                                                          05260030
      IVTNUM = 286                                                      05270030
C                                                                       05280030
C      ****  TEST 286  ****                                             05290030
C                                                                       05300030
      IF (ICZERO) 32860, 2860, 32860                                    05310030
 2860 CONTINUE                                                          05320030
      IVON02 =2                                                         05330030
      IVCOMP = 3 - IVON02                                               05340030
      GO TO 42860                                                       05350030
32860 IVDELE = IVDELE + 1                                               05360030
      WRITE (I02,80003) IVTNUM                                          05370030
      IF (ICZERO) 42860, 2871, 42860                                    05380030
42860 IF (IVCOMP -1) 22860,12860,22860                                  05390030
12860 IVPASS = IVPASS + 1                                               05400030
      WRITE (I02,80001) IVTNUM                                          05410030
      GO TO 2871                                                        05420030
22860 IVFAIL = IVFAIL + 1                                               05430030
      IVCORR = 1                                                        05440030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05450030
 2871 CONTINUE                                                          05460030
      IVTNUM = 287                                                      05470030
C                                                                       05480030
C      ****  TEST 287  ****                                             05490030
C                                                                       05500030
      IF (ICZERO) 32870, 2870, 32870                                    05510030
 2870 CONTINUE                                                          05520030
      IVON02 =3                                                         05530030
      IVCOMP = 2 -IVON02                                                05540030
      GO TO 42870                                                       05550030
32870 IVDELE = IVDELE + 1                                               05560030
      WRITE (I02,80003) IVTNUM                                          05570030
      IF (ICZERO) 42870, 2881, 42870                                    05580030
42870 IF (IVCOMP +1) 22870,12870,22870                                  05590030
12870 IVPASS = IVPASS + 1                                               05600030
      WRITE (I02,80001) IVTNUM                                          05610030
      GO TO 2881                                                        05620030
22870 IVFAIL = IVFAIL + 1                                               05630030
      IVCORR =-1                                                        05640030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05650030
 2881 CONTINUE                                                          05660030
      IVTNUM = 288                                                      05670030
C                                                                       05680030
C      ****  TEST 288  ****                                             05690030
C                                                                       05700030
      IF (ICZERO) 32880, 2880, 32880                                    05710030
 2880 CONTINUE                                                          05720030
      IVON02 = -2                                                       05730030
      IVCOMP = 3 - IVON02                                               05740030
      GO TO 42880                                                       05750030
32880 IVDELE = IVDELE + 1                                               05760030
      WRITE (I02,80003) IVTNUM                                          05770030
      IF (ICZERO) 42880, 2891, 42880                                    05780030
42880 IF (IVCOMP -5) 22880,12880,22880                                  05790030
12880 IVPASS = IVPASS + 1                                               05800030
      WRITE (I02,80001) IVTNUM                                          05810030
      GO TO 2891                                                        05820030
22880 IVFAIL = IVFAIL + 1                                               05830030
      IVCORR =5                                                         05840030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05850030
 2891 CONTINUE                                                          05860030
      IVTNUM = 289                                                      05870030
C                                                                       05880030
C      ****  TEST 289  ****                                             05890030
C                                                                       05900030
      IF (ICZERO) 32890, 2890, 32890                                    05910030
 2890 CONTINUE                                                          05920030
      IVON01 =51                                                        05930030
      IVCOMP = IVON01 - 52                                              05940030
      GO TO 42890                                                       05950030
32890 IVDELE = IVDELE + 1                                               05960030
      WRITE (I02,80003) IVTNUM                                          05970030
      IF (ICZERO) 42890, 2901, 42890                                    05980030
42890 IF (IVCOMP + 1) 22890,12890,22890                                 05990030
12890 IVPASS = IVPASS + 1                                               06000030
      WRITE (I02,80001) IVTNUM                                          06010030
      GO TO 2901                                                        06020030
22890 IVFAIL = IVFAIL + 1                                               06030030
      IVCORR = -1                                                       06040030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06050030
 2901 CONTINUE                                                          06060030
      IVTNUM = 290                                                      06070030
C                                                                       06080030
C      ****  TEST 290  ****                                             06090030
C                                                                       06100030
      IF (ICZERO) 32900, 2900, 32900                                    06110030
 2900 CONTINUE                                                          06120030
      IVON01 =51                                                        06130030
      IVCOMP = IVON01 -51                                               06140030
      GO TO 42900                                                       06150030
32900 IVDELE = IVDELE + 1                                               06160030
      WRITE (I02,80003) IVTNUM                                          06170030
      IF (ICZERO) 42900, 2911, 42900                                    06180030
42900 IF (IVCOMP) 22900,12900,22900                                     06190030
12900 IVPASS = IVPASS + 1                                               06200030
      WRITE (I02,80001) IVTNUM                                          06210030
      GO TO 2911                                                        06220030
22900 IVFAIL = IVFAIL + 1                                               06230030
      IVCORR =0                                                         06240030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06250030
 2911 CONTINUE                                                          06260030
      IVTNUM = 291                                                      06270030
C                                                                       06280030
C      ****  TEST 291  ****                                             06290030
C                                                                       06300030
      IF (ICZERO) 32910, 2910, 32910                                    06310030
 2910 CONTINUE                                                          06320030
      IVON01 =53                                                        06330030
      IVCOMP =IVON01 -52                                                06340030
      GO TO 42910                                                       06350030
32910 IVDELE = IVDELE + 1                                               06360030
      WRITE (I02,80003) IVTNUM                                          06370030
      IF (ICZERO) 42910, 2921, 42910                                    06380030
42910 IF (IVCOMP -1) 22910,12910,22910                                  06390030
12910 IVPASS = IVPASS + 1                                               06400030
      WRITE (I02,80001) IVTNUM                                          06410030
      GO TO 2921                                                        06420030
22910 IVFAIL = IVFAIL + 1                                               06430030
      IVCORR = 1                                                        06440030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06450030
 2921 CONTINUE                                                          06460030
      IVTNUM = 292                                                      06470030
C                                                                       06480030
C      ****  TEST 292  ****                                             06490030
C                                                                       06500030
      IF (ICZERO) 32920, 2920, 32920                                    06510030
 2920 CONTINUE                                                          06520030
      IVON02 = 676                                                      06530030
      IVCOMP = 189 - IVON02                                             06540030
      GO TO 42920                                                       06550030
32920 IVDELE = IVDELE + 1                                               06560030
      WRITE (I02,80003) IVTNUM                                          06570030
      IF (ICZERO) 42920, 2931, 42920                                    06580030
42920 IF (IVCOMP + 487) 22920,12920,22920                               06590030
12920 IVPASS = IVPASS + 1                                               06600030
      WRITE (I02,80001) IVTNUM                                          06610030
      GO TO 2931                                                        06620030
22920 IVFAIL = IVFAIL + 1                                               06630030
      IVCORR = -487                                                     06640030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06650030
 2931 CONTINUE                                                          06660030
      IVTNUM = 293                                                      06670030
C                                                                       06680030
C      ****  TEST 293  ****                                             06690030
C                                                                       06700030
      IF (ICZERO) 32930, 2930, 32930                                    06710030
 2930 CONTINUE                                                          06720030
      IVON02 = -676                                                     06730030
      IVCOMP = 189 - IVON02                                             06740030
      GO TO 42930                                                       06750030
32930 IVDELE = IVDELE + 1                                               06760030
      WRITE (I02,80003) IVTNUM                                          06770030
      IF (ICZERO) 42930, 2941, 42930                                    06780030
42930 IF (IVCOMP - 865) 22930,12930,22930                               06790030
12930 IVPASS = IVPASS + 1                                               06800030
      WRITE (I02,80001) IVTNUM                                          06810030
      GO TO 2941                                                        06820030
22930 IVFAIL = IVFAIL + 1                                               06830030
      IVCORR = 865                                                      06840030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06850030
 2941 CONTINUE                                                          06860030
      IVTNUM = 294                                                      06870030
C                                                                       06880030
C      ****  TEST 294  ****                                             06890030
C                                                                       06900030
      IF (ICZERO) 32940, 2940, 32940                                    06910030
 2940 CONTINUE                                                          06920030
      IVON01 = 1358                                                     06930030
      IVCOMP = IVON01 - 8001                                            06940030
      GO TO 42940                                                       06950030
32940 IVDELE = IVDELE + 1                                               06960030
      WRITE (I02,80003) IVTNUM                                          06970030
      IF (ICZERO) 42940, 2951, 42940                                    06980030
42940 IF (IVCOMP + 6643) 22940,12940,22940                              06990030
12940 IVPASS = IVPASS + 1                                               07000030
      WRITE (I02,80001) IVTNUM                                          07010030
      GO TO 2951                                                        07020030
22940 IVFAIL = IVFAIL + 1                                               07030030
      IVCORR = -6643                                                    07040030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07050030
 2951 CONTINUE                                                          07060030
      IVTNUM = 295                                                      07070030
C                                                                       07080030
C      ****  TEST 295  ****                                             07090030
C                                                                       07100030
      IF (ICZERO) 32950, 2950, 32950                                    07110030
 2950 CONTINUE                                                          07120030
      IVON01 = -1358                                                    07130030
      IVCOMP = IVON01 - 8001                                            07140030
      GO TO 42950                                                       07150030
32950 IVDELE = IVDELE + 1                                               07160030
      WRITE (I02,80003) IVTNUM                                          07170030
      IF (ICZERO) 42950, 2961, 42950                                    07180030
42950 IF (IVCOMP + 9359) 22950,12950,22950                              07190030
12950 IVPASS = IVPASS + 1                                               07200030
      WRITE (I02,80001) IVTNUM                                          07210030
      GO TO 2961                                                        07220030
22950 IVFAIL = IVFAIL + 1                                               07230030
      IVCORR = -9359                                                    07240030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07250030
 2961 CONTINUE                                                          07260030
      IVTNUM = 296                                                      07270030
C                                                                       07280030
C      ****  TEST 296  ****                                             07290030
C                                                                       07300030
      IF (ICZERO) 32960, 2960, 32960                                    07310030
 2960 CONTINUE                                                          07320030
      IVON01 = 15                                                       07330030
      IVCOMP = IVON01 - 32752                                           07340030
      GO TO 42960                                                       07350030
32960 IVDELE = IVDELE + 1                                               07360030
      WRITE (I02,80003) IVTNUM                                          07370030
      IF (ICZERO) 42960, 2971, 42960                                    07380030
42960 IF (IVCOMP + 32737) 22960,12960,22960                             07390030
12960 IVPASS = IVPASS + 1                                               07400030
      WRITE (I02,80001) IVTNUM                                          07410030
      GO TO 2971                                                        07420030
22960 IVFAIL = IVFAIL + 1                                               07430030
      IVCORR = -32737                                                   07440030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07450030
 2971 CONTINUE                                                          07460030
      IVTNUM = 297                                                      07470030
C                                                                       07480030
C      ****  TEST 297  ****                                             07490030
C                                                                       07500030
      IF (ICZERO) 32970, 2970, 32970                                    07510030
 2970 CONTINUE                                                          07520030
      IVON01 =-32751                                                    07530030
      IVCOMP = IVON01 - 15                                              07540030
      GO TO 42970                                                       07550030
32970 IVDELE = IVDELE + 1                                               07560030
      WRITE (I02,80003) IVTNUM                                          07570030
      IF (ICZERO) 42970, 2981, 42970                                    07580030
42970 IF (IVCOMP + 32766) 22970,12970,22970                             07590030
12970 IVPASS = IVPASS + 1                                               07600030
      WRITE (I02,80001) IVTNUM                                          07610030
      GO TO 2981                                                        07620030
22970 IVFAIL = IVFAIL + 1                                               07630030
      IVCORR = -32766                                                   07640030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07650030
 2981 CONTINUE                                                          07660030
      IVTNUM = 298                                                      07670030
C                                                                       07680030
C      ****  TEST 298  ****                                             07690030
C                                                                       07700030
      IF (ICZERO) 32980, 2980, 32980                                    07710030
 2980 CONTINUE                                                          07720030
      IVON02 = -32752                                                   07730030
      IVCOMP = 15 - IVON02                                              07740030
      GO TO 42980                                                       07750030
32980 IVDELE = IVDELE + 1                                               07760030
      WRITE (I02,80003) IVTNUM                                          07770030
      IF (ICZERO) 42980, 2991, 42980                                    07780030
42980 IF (IVCOMP - 32767) 22980,12980,22980                             07790030
12980 IVPASS = IVPASS + 1                                               07800030
      WRITE (I02,80001) IVTNUM                                          07810030
      GO TO 2991                                                        07820030
22980 IVFAIL = IVFAIL + 1                                               07830030
      IVCORR = 32767                                                    07840030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07850030
 2991 CONTINUE                                                          07860030
      IVTNUM = 299                                                      07870030
C                                                                       07880030
C      ****  TEST 299  ****                                             07890030
C                                                                       07900030
      IF (ICZERO) 32990, 2990, 32990                                    07910030
 2990 CONTINUE                                                          07920030
      IVON02 = 15                                                       07930030
      IVCOMP = 32752 - IVON02                                           07940030
      GO TO 42990                                                       07950030
32990 IVDELE = IVDELE + 1                                               07960030
      WRITE (I02,80003) IVTNUM                                          07970030
      IF (ICZERO) 42990, 3001, 42990                                    07980030
42990 IF (IVCOMP - 32737) 22990,12990,22990                             07990030
12990 IVPASS = IVPASS + 1                                               08000030
      WRITE (I02,80001) IVTNUM                                          08010030
      GO TO 3001                                                        08020030
22990 IVFAIL = IVFAIL + 1                                               08030030
      IVCORR = 32737                                                    08040030
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08050030
 3001  CONTINUE                                                         08060030
C                                                                       08070030
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08080030
99999 CONTINUE                                                          08090030
      WRITE (I02,90002)                                                 08100030
      WRITE (I02,90006)                                                 08110030
      WRITE (I02,90002)                                                 08120030
      WRITE (I02,90002)                                                 08130030
      WRITE (I02,90007)                                                 08140030
      WRITE (I02,90002)                                                 08150030
      WRITE (I02,90008)  IVFAIL                                         08160030
      WRITE (I02,90009) IVPASS                                          08170030
      WRITE (I02,90010) IVDELE                                          08180030
C                                                                       08190030
C                                                                       08200030
C     TERMINATE ROUTINE EXECUTION                                       08210030
      STOP                                                              08220030
C                                                                       08230030
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08240030
90000 FORMAT (1H1)                                                      08250030
90002 FORMAT (1H )                                                      08260030
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08270030
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08280030
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08290030
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08300030
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08310030
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08320030
C                                                                       08330030
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08340030
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08350030
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08360030
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08370030
C                                                                       08380030
C     FORMAT STATEMENTS FOR TEST RESULTS                                08390030
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08400030
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08410030
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08420030
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08430030
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08440030
C                                                                       08450030
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM030)                          08460030
      END                                                               08470030
