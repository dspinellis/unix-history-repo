C     COMMENT SECTION.                                                  00010009
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020009
C     FM009                                                             00030009
C                                                                       00040009
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050009
C     FORM                                                              00060009
C             INTEGER VARIABLE = ARITHMETIC EXPRESSION                  00070009
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080009
C     OPERATOR +, INTEGER CONSTANTS AND POSITIVE INTEGER VARIABLES.     00090009
C     SOME OF THE TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE        00100009
C     ARITHMETIC EXPRESSION.                                            00110009
C                                                                       00120009
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00130009
C            (1)  TWO INTEGER VARIABLES,                                00140009
C            (2)  TWO INTEGER VARIABLES AND ONE INTEGER CONSTANT,       00150009
C            (3)  TWO INTEGER VARIABLES AND ONE INTEGER CONSTANT WITH   00160009
C                   PARENTHESES TO GROUP ELEMENTS.                      00170009
C                                                                       00180009
C      REFERENCES                                                       00190009
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00200009
C              X3.9-1978                                                00210009
C                                                                       00220009
C        SECTION 4.3, INTEGER TYPE                                      00230009
C        SECTION 4.3.1, INTEGER CONSTANT                                00240009
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00250009
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENTS                 00260009
C                                                                       00270009
C      **********************************************************       00280009
C                                                                       00290009
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00300009
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00310009
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00320009
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00330009
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00340009
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00350009
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00360009
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00370009
C     OF EXECUTING THESE TESTS.                                         00380009
C                                                                       00390009
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00400009
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00410009
C                                                                       00420009
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00430009
C                                                                       00440009
C                  DEPARTMENT OF THE NAVY                               00450009
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00460009
C                  WASHINGTON, D.C.  20376                              00470009
C                                                                       00480009
C      **********************************************************       00490009
C                                                                       00500009
C                                                                       00510009
C                                                                       00520009
C     INITIALIZATION SECTION                                            00530009
C                                                                       00540009
C     INITIALIZE CONSTANTS                                              00550009
C      **************                                                   00560009
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00570009
      I01 = 5                                                           00580009
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00590009
      I02 = 6                                                           00600009
C     SYSTEM ENVIRONMENT SECTION                                        00610009
C                                                                       00620009
      I01 = 5                                                           00630009
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00640009
C     (UNIT NUMBER FOR CARD READER).                                    00650009
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00660009
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00670009
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00680009
C                                                                       00690009
      I02 = 6                                                           00700009
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00710009
C     (UNIT NUMBER FOR PRINTER).                                        00720009
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00730009
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00740009
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00750009
C                                                                       00760009
      IVPASS=0                                                          00770009
      IVFAIL=0                                                          00780009
      IVDELE=0                                                          00790009
      ICZERO=0                                                          00800009
C                                                                       00810009
C     WRITE PAGE HEADERS                                                00820009
      WRITE (I02,90000)                                                 00830009
      WRITE (I02,90001)                                                 00840009
      WRITE (I02,90002)                                                 00850009
      WRITE (I02, 90002)                                                00860009
      WRITE (I02,90003)                                                 00870009
      WRITE (I02,90002)                                                 00880009
      WRITE (I02,90004)                                                 00890009
      WRITE (I02,90002)                                                 00900009
      WRITE (I02,90011)                                                 00910009
      WRITE (I02,90002)                                                 00920009
      WRITE (I02,90002)                                                 00930009
      WRITE (I02,90005)                                                 00940009
      WRITE (I02,90006)                                                 00950009
      WRITE (I02,90002)                                                 00960009
C                                                                       00970009
C     TEST SECTION                                                      00980009
C                                                                       00990009
C         ARITHMETIC ASSIGNMENT STATEMENT                               01000009
C                                                                       01010009
C     TEST 235 THROUGH TEST 243 CONTAIN TWO POSITIVE INTEGER VARIABLES  01020009
C     AND OPERATOR + IN ARITHMETIC EXPRESSION.                          01030009
C                                                                       01040009
 2351 CONTINUE                                                          01050009
      IVTNUM = 235                                                      01060009
C                                                                       01070009
C      ****  TEST 235  ****                                             01080009
C                                                                       01090009
      IF (ICZERO) 32350, 2350, 32350                                    01100009
 2350 CONTINUE                                                          01110009
      IVON01 = 2                                                        01120009
      IVON02 = 3                                                        01130009
      IVCOMP = IVON01 + IVON02                                          01140009
      GO TO 42350                                                       01150009
32350 IVDELE = IVDELE + 1                                               01160009
      WRITE (I02,80003) IVTNUM                                          01170009
      IF (ICZERO) 42350, 2361, 42350                                    01180009
42350 IF (IVCOMP - 5) 22350,12350,22350                                 01190009
12350 IVPASS = IVPASS + 1                                               01200009
      WRITE (I02,80001) IVTNUM                                          01210009
      GO TO 2361                                                        01220009
22350 IVFAIL = IVFAIL + 1                                               01230009
      IVCORR = 5                                                        01240009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01250009
 2361 CONTINUE                                                          01260009
      IVTNUM = 236                                                      01270009
C                                                                       01280009
C      ****  TEST 236  ****                                             01290009
C                                                                       01300009
      IF (ICZERO) 32360, 2360, 32360                                    01310009
 2360 CONTINUE                                                          01320009
      IVON01 = 2                                                        01330009
      IVON02 = 3                                                        01340009
      IVCOMP = IVON02 + IVON01                                          01350009
      GO TO 42360                                                       01360009
32360 IVDELE = IVDELE + 1                                               01370009
      WRITE (I02,80003) IVTNUM                                          01380009
      IF (ICZERO) 42360, 2371, 42360                                    01390009
42360 IF (IVCOMP - 5) 22360, 12360, 22360                               01400009
12360 IVPASS = IVPASS + 1                                               01410009
      WRITE (I02,80001) IVTNUM                                          01420009
      GO TO 2371                                                        01430009
22360 IVFAIL = IVFAIL + 1                                               01440009
      IVCORR = 5                                                        01450009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01460009
 2371 CONTINUE                                                          01470009
      IVTNUM = 237                                                      01480009
C                                                                       01490009
C      ****  TEST 237  ****                                             01500009
C                                                                       01510009
      IF (ICZERO) 32370, 2370, 32370                                    01520009
 2370 CONTINUE                                                          01530009
      IVON01 = 51                                                       01540009
      IVON02 = 52                                                       01550009
      IVCOMP = IVON01 + IVON02                                          01560009
      GO TO 42370                                                       01570009
32370 IVDELE = IVDELE + 1                                               01580009
      WRITE (I02,80003) IVTNUM                                          01590009
      IF (ICZERO) 42370, 2381, 42370                                    01600009
42370 IF (IVCOMP - 103) 22370, 12370, 22370                             01610009
12370 IVPASS = IVPASS + 1                                               01620009
      WRITE (I02,80001) IVTNUM                                          01630009
      GO TO 2381                                                        01640009
22370 IVFAIL = IVFAIL + 1                                               01650009
      IVCORR = 103                                                      01660009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01670009
 2381 CONTINUE                                                          01680009
      IVTNUM = 238                                                      01690009
C                                                                       01700009
C      ****  TEST  238 ****                                             01710009
C                                                                       01720009
      IF (ICZERO) 32380, 2380, 32380                                    01730009
 2380 CONTINUE                                                          01740009
      IVON01 = 189                                                      01750009
      IVON02 = 676                                                      01760009
      IVCOMP = IVON01 + IVON02                                          01770009
      GO TO 42380                                                       01780009
32380 IVDELE = IVDELE + 1                                               01790009
      WRITE (I02,80003) IVTNUM                                          01800009
      IF (ICZERO) 42380, 2391, 42380                                    01810009
42380 IF (IVCOMP - 865) 22380, 12380, 22380                             01820009
12380 IVPASS = IVPASS + 1                                               01830009
      WRITE (I02,80001) IVTNUM                                          01840009
      GO TO 2391                                                        01850009
22380 IVFAIL = IVFAIL + 1                                               01860009
      IVCORR = 865                                                      01870009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01880009
 2391 CONTINUE                                                          01890009
      IVTNUM = 239                                                      01900009
C                                                                       01910009
C      ****  TEST 239  ****                                             01920009
C                                                                       01930009
      IF (ICZERO) 32390, 2390, 32390                                    01940009
 2390 CONTINUE                                                          01950009
      IVON01 = 1358                                                     01960009
      IVON02 = 8001                                                     01970009
      IVCOMP = IVON01 + IVON02                                          01980009
      GO TO 42390                                                       01990009
32390 IVDELE = IVDELE + 1                                               02000009
      WRITE (I02,80003) IVTNUM                                          02010009
      IF (ICZERO) 42390, 2401, 42390                                    02020009
42390 IF (IVCOMP - 9359) 22390, 12390, 22390                            02030009
12390 IVPASS = IVPASS + 1                                               02040009
      WRITE (I02,80001) IVTNUM                                          02050009
      GO TO 2401                                                        02060009
22390 IVFAIL = IVFAIL + 1                                               02070009
      IVCORR = 9359                                                     02080009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02090009
 2401 CONTINUE                                                          02100009
      IVTNUM = 240                                                      02110009
C                                                                       02120009
C      ****  TEST 240  ****                                             02130009
C                                                                       02140009
      IF (ICZERO) 32400, 2400, 32400                                    02150009
 2400 CONTINUE                                                          02160009
      IVON01 = 1358                                                     02170009
      IVON02 = 8001                                                     02180009
      IVCOMP = IVON02 + IVON01                                          02190009
      GO TO 42400                                                       02200009
32400 IVDELE = IVDELE + 1                                               02210009
      WRITE (I02,80003) IVTNUM                                          02220009
      IF (ICZERO) 42400, 2411, 42400                                    02230009
42400 IF (IVCOMP - 9359) 22400, 12400, 22400                            02240009
12400 IVPASS = IVPASS + 1                                               02250009
      WRITE (I02,80001) IVTNUM                                          02260009
      GO TO 2411                                                        02270009
22400 IVFAIL = IVFAIL + 1                                               02280009
      IVCORR = 9359                                                     02290009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02300009
 2411 CONTINUE                                                          02310009
      IVTNUM = 241                                                      02320009
C                                                                       02330009
C      ****  TEST 241  ****                                             02340009
C                                                                       02350009
      IF (ICZERO) 32410, 2410, 32410                                    02360009
 2410 CONTINUE                                                          02370009
      IVON01 = 11112                                                    02380009
      IVON02 = 10001                                                    02390009
      IVCOMP = IVON01 + IVON02                                          02400009
      GO TO 42410                                                       02410009
32410 IVDELE = IVDELE + 1                                               02420009
      WRITE (I02,80003) IVTNUM                                          02430009
      IF (ICZERO) 42410, 2421, 42410                                    02440009
42410 IF (IVCOMP - 21113) 22410, 12410, 22410                           02450009
12410 IVPASS = IVPASS + 1                                               02460009
      WRITE (I02,80001) IVTNUM                                          02470009
      GO TO 2421                                                        02480009
22410 IVFAIL = IVFAIL + 1                                               02490009
      IVCORR = 21113                                                    02500009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02510009
 2421 CONTINUE                                                          02520009
      IVTNUM = 242                                                      02530009
C                                                                       02540009
C      **** TEST 242  ****                                              02550009
C                                                                       02560009
      IF (ICZERO) 32420, 2420, 32420                                    02570009
 2420 CONTINUE                                                          02580009
      IVON01 = 189                                                      02590009
      IVON02 = 9876                                                     02600009
      IVCOMP = IVON01 + IVON02                                          02610009
      GO TO 42420                                                       02620009
32420 IVDELE = IVDELE + 1                                               02630009
      WRITE (I02,80003) IVTNUM                                          02640009
      IF (ICZERO) 42420, 2431, 42420                                    02650009
42420 IF (IVCOMP - 10065) 22420, 12420, 22420                           02660009
12420 IVPASS = IVPASS + 1                                               02670009
      WRITE (I02,80001) IVTNUM                                          02680009
      GO TO 2431                                                        02690009
22420 IVFAIL = IVFAIL + 1                                               02700009
      IVCORR = 10065                                                    02710009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02720009
 2431 CONTINUE                                                          02730009
      IVTNUM = 243                                                      02740009
C                                                                       02750009
C      **** TEST 243  ****                                              02760009
C         REQUIRES 32767                                                02770009
C                                                                       02780009
      IF (ICZERO) 32430, 2430, 32430                                    02790009
 2430 CONTINUE                                                          02800009
      IVON01 = 16383                                                    02810009
      IVON02 = 16384                                                    02820009
      IVCOMP = IVON01 + IVON02                                          02830009
      GO TO 42430                                                       02840009
32430 IVDELE = IVDELE + 1                                               02850009
      WRITE (I02,80003) IVTNUM                                          02860009
      IF (ICZERO) 42430, 2441, 42430                                    02870009
42430 IF (IVCOMP - 32767) 22430, 12430, 22430                           02880009
12430 IVPASS = IVPASS + 1                                               02890009
      WRITE (I02,80001) IVTNUM                                          02900009
      GO TO 2441                                                        02910009
22430 IVFAIL = IVFAIL + 1                                               02920009
      IVCORR = 32767                                                    02930009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02940009
C                                                                       02950009
C     TEST 244 THROUGH TEST 250 CONTAIN TWO POSITIVE INTEGER VARIABLES, 02960009
C     ONE INTEGER CONSTANT, AND OPERATOR + IN ARITHMETIC EXPRESSION.    02970009
C                                                                       02980009
 2441 CONTINUE                                                          02990009
      IVTNUM = 244                                                      03000009
C                                                                       03010009
C      ****  TEST 244  ****                                             03020009
C                                                                       03030009
      IF (ICZERO) 32440, 2440, 32440                                    03040009
 2440 CONTINUE                                                          03050009
      IVON01 = 2                                                        03060009
      IVON02 = 3                                                        03070009
      IVCOMP = IVON01 + IVON02 + 4                                      03080009
      GO TO 42440                                                       03090009
32440 IVDELE = IVDELE + 1                                               03100009
      WRITE (I02,80003) IVTNUM                                          03110009
      IF (ICZERO) 42440, 2451, 42440                                    03120009
42440 IF (IVCOMP - 9) 22440, 12440, 22440                               03130009
12440 IVPASS = IVPASS + 1                                               03140009
      WRITE (I02,80001) IVTNUM                                          03150009
      GO TO 2451                                                        03160009
22440 IVFAIL = IVFAIL + 1                                               03170009
      IVCORR = 9                                                        03180009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03190009
 2451 CONTINUE                                                          03200009
      IVTNUM = 245                                                      03210009
C                                                                       03220009
C      ****  TEST 245  ****                                             03230009
C                                                                       03240009
      IF (ICZERO) 32450, 2450, 32450                                    03250009
 2450 CONTINUE                                                          03260009
      IVON01 = 2                                                        03270009
      IVON03 = 4                                                        03280009
      IVCOMP = IVON01 +3 + IVON03                                       03290009
      GO TO 42450                                                       03300009
32450 IVDELE = IVDELE + 1                                               03310009
      WRITE (I02,80003) IVTNUM                                          03320009
      IF (ICZERO) 42450, 2461, 42450                                    03330009
42450 IF (IVCOMP - 9) 22450, 12450, 22450                               03340009
12450 IVPASS = IVPASS + 1                                               03350009
      WRITE (I02,80001) IVTNUM                                          03360009
      GO TO 2461                                                        03370009
22450 IVFAIL = IVFAIL + 1                                               03380009
      IVCORR = 9                                                        03390009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03400009
 2461 CONTINUE                                                          03410009
      IVTNUM = 246                                                      03420009
C                                                                       03430009
C      ****  TEST 246  ****                                             03440009
C                                                                       03450009
      IF (ICZERO) 32460, 2460, 32460                                    03460009
 2460 CONTINUE                                                          03470009
      IVON02 = 3                                                        03480009
      IVON03 = 4                                                        03490009
      IVCOMP = 2 + IVON02 + IVON03                                      03500009
      GO TO 42460                                                       03510009
32460 IVDELE = IVDELE + 1                                               03520009
      WRITE (I02,80003) IVTNUM                                          03530009
      IF (ICZERO) 42460, 2471, 42460                                    03540009
42460 IF (IVCOMP - 9) 22460, 12460, 22460                               03550009
12460 IVPASS = IVPASS + 1                                               03560009
      WRITE (I02,80001) IVTNUM                                          03570009
      GO TO 2471                                                        03580009
22460 IVFAIL = IVFAIL + 1                                               03590009
      IVCORR = 9                                                        03600009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03610009
 2471 CONTINUE                                                          03620009
      IVTNUM = 247                                                      03630009
C                                                                       03640009
C      ****  TEST 247  ****                                             03650009
C                                                                       03660009
      IF (ICZERO) 32470, 2470, 32470                                    03670009
 2470 CONTINUE                                                          03680009
      IVON01 = 51                                                       03690009
      IVON03 = 53                                                       03700009
      IVCOMP = IVON01 +52 + IVON03                                      03710009
      GO TO 42470                                                       03720009
32470 IVDELE = IVDELE + 1                                               03730009
      WRITE (I02,80003) IVTNUM                                          03740009
      IF (ICZERO) 42470, 2481, 42470                                    03750009
42470 IF (IVCOMP - 156) 22470, 12470, 22470                             03760009
12470 IVPASS = IVPASS + 1                                               03770009
      WRITE (I02,80001) IVTNUM                                          03780009
      GO TO 2481                                                        03790009
22470 IVFAIL = IVFAIL + 1                                               03800009
      IVCORR = 156                                                      03810009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03820009
 2481 CONTINUE                                                          03830009
      IVTNUM = 248                                                      03840009
C                                                                       03850009
C      ****  TEST 248  ****                                             03860009
C                                                                       03870009
      IF (ICZERO) 32480, 2480, 32480                                    03880009
 2480 CONTINUE                                                          03890009
      IVON02 = 676                                                      03900009
      IVON03 = 101                                                      03910009
      IVCOMP = 189 + IVON02 + IVON03                                    03920009
      GO TO 42480                                                       03930009
32480 IVDELE = IVDELE + 1                                               03940009
      WRITE (I02,80003) IVTNUM                                          03950009
      IF (ICZERO) 42480, 2491, 42480                                    03960009
42480 IF (IVCOMP - 966) 22480, 12480, 22480                             03970009
12480 IVPASS = IVPASS + 1                                               03980009
      WRITE (I02,80001) IVTNUM                                          03990009
      GO TO 2491                                                        04000009
22480 IVFAIL = IVFAIL + 1                                               04010009
      IVCORR = 966                                                      04020009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04030009
 2491 CONTINUE                                                          04040009
      IVTNUM = 249                                                      04050009
C                                                                       04060009
C      ****  TEST 249  ****                                             04070009
C                                                                       04080009
      IF (ICZERO) 32490, 2490, 32490                                    04090009
 2490 CONTINUE                                                          04100009
      IVON01 = 1358                                                     04110009
      IVON02 = 8001                                                     04120009
      IVCOMP = IVON01 + IVON02 + 2189                                   04130009
      GO TO 42490                                                       04140009
32490 IVDELE = IVDELE + 1                                               04150009
      WRITE (I02,80003) IVTNUM                                          04160009
      IF (ICZERO) 42490, 2501, 42490                                    04170009
42490 IF (IVCOMP - 11548) 22490, 12490, 22490                           04180009
12490 IVPASS = IVPASS + 1                                               04190009
      WRITE (I02,80001) IVTNUM                                          04200009
      GO TO 2501                                                        04210009
22490 IVFAIL = IVFAIL + 1                                               04220009
      IVCORR = 11548                                                    04230009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04240009
 2501 CONTINUE                                                          04250009
      IVTNUM = 250                                                      04260009
C                                                                       04270009
C      ****  TEST 250  ****                                             04280009
C         REQUIRES 32767                                                04290009
C                                                                       04300009
      IF (ICZERO) 32500, 2500, 32500                                    04310009
 2500 CONTINUE                                                          04320009
      IVON01 = 16383                                                    04330009
      IVON03 = 4                                                        04340009
      IVCOMP = IVON01 + 16380 + IVON03                                  04350009
      GO TO 42500                                                       04360009
32500 IVDELE = IVDELE + 1                                               04370009
      WRITE (I02,80003) IVTNUM                                          04380009
      IF (ICZERO) 42500, 2511, 42500                                    04390009
42500 IF (IVCOMP - 32767) 22500,12500,22500                             04400009
12500 IVPASS = IVPASS + 1                                               04410009
      WRITE (I02,80001) IVTNUM                                          04420009
      GO TO 2511                                                        04430009
22500 IVFAIL = IVFAIL + 1                                               04440009
      IVCORR = 32767                                                    04450009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04460009
C                                                                       04470009
C     TEST 251 THROUGH TEST 264 CONTAIN TWO POSITIVE INTEGER VARIABLES, 04480009
C     ONE INTEGER CONSTANT, AND OPERATOR + IN ARITHMETIC EXPRESSION.    04490009
C     PARENTHESES ARE USED TO GROUP ELEMENTS.  THE RESULTS ARE THE SAME 04500009
C     AS TESTS 244 THROUGH 250.                                         04510009
C                                                                       04520009
 2511 CONTINUE                                                          04530009
      IVTNUM = 251                                                      04540009
C                                                                       04550009
C      ****  TEST 251  ****                                             04560009
C                                                                       04570009
      IF (ICZERO) 32510, 2510, 32510                                    04580009
 2510 CONTINUE                                                          04590009
      IVON01 = 2                                                        04600009
      IVON02 = 3                                                        04610009
      IVCOMP = (IVON01 + IVON02) + 4                                    04620009
      GO TO 42510                                                       04630009
32510 IVDELE = IVDELE + 1                                               04640009
      WRITE (I02,80003) IVTNUM                                          04650009
      IF (ICZERO) 42510, 2521, 42510                                    04660009
42510 IF (IVCOMP - 9) 22510,12510,22510                                 04670009
12510 IVPASS = IVPASS + 1                                               04680009
      WRITE (I02,80001) IVTNUM                                          04690009
      GO TO 2521                                                        04700009
22510 IVFAIL = IVFAIL + 1                                               04710009
      IVCORR = 9                                                        04720009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04730009
 2521 CONTINUE                                                          04740009
      IVTNUM = 252                                                      04750009
C                                                                       04760009
C      ****  TEST 252  ****                                             04770009
C                                                                       04780009
      IF (ICZERO) 32520, 2520, 32520                                    04790009
 2520 CONTINUE                                                          04800009
      IVON02 = 3                                                        04810009
      IVON03 = 4                                                        04820009
      IVCOMP = 2 + (IVON02 + IVON03)                                    04830009
      GO TO 42520                                                       04840009
32520 IVDELE = IVDELE + 1                                               04850009
      WRITE (I02,80003) IVTNUM                                          04860009
      IF (ICZERO) 42520, 2531, 42520                                    04870009
42520 IF (IVCOMP - 9) 22520,12520,22520                                 04880009
12520 IVPASS = IVPASS + 1                                               04890009
      WRITE (I02,80001) IVTNUM                                          04900009
      GO TO 2531                                                        04910009
22520 IVFAIL = IVFAIL + 1                                               04920009
      IVCORR = 9                                                        04930009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04940009
 2531 CONTINUE                                                          04950009
      IVTNUM = 253                                                      04960009
C                                                                       04970009
C      **** TEST 253  ****                                              04980009
C                                                                       04990009
      IF (ICZERO) 32530, 2530, 32530                                    05000009
 2530 CONTINUE                                                          05010009
      IVON02 =3                                                         05020009
      IVON03 =4                                                         05030009
      IVCOMP = (2+IVON02)+IVON03                                        05040009
      GO TO 42530                                                       05050009
32530 IVDELE = IVDELE + 1                                               05060009
      WRITE (I02,80003) IVTNUM                                          05070009
      IF (ICZERO) 42530, 2541, 42530                                    05080009
42530 IF (IVCOMP -9) 22530,12530,22530                                  05090009
12530 IVPASS = IVPASS + 1                                               05100009
      WRITE (I02,80001) IVTNUM                                          05110009
      GO TO 2541                                                        05120009
22530 IVFAIL = IVFAIL + 1                                               05130009
      IVCORR =9                                                         05140009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05150009
 2541 CONTINUE                                                          05160009
      IVTNUM = 254                                                      05170009
C                                                                       05180009
C      ****  TEST 254  ****                                             05190009
C                                                                       05200009
      IF (ICZERO) 32540, 2540, 32540                                    05210009
 2540 CONTINUE                                                          05220009
      IVON01 = 2                                                        05230009
      IVON02 = 3                                                        05240009
      IVCOMP = IVON01 + (IVON02 + 4)                                    05250009
      GO TO 42540                                                       05260009
32540 IVDELE = IVDELE + 1                                               05270009
      WRITE (I02,80003) IVTNUM                                          05280009
      IF (ICZERO) 42540, 2551, 42540                                    05290009
42540 IF (IVCOMP-9)22540,12540,22540                                    05300009
12540 IVPASS = IVPASS + 1                                               05310009
      WRITE (I02,80001) IVTNUM                                          05320009
      GO TO 2551                                                        05330009
22540 IVFAIL = IVFAIL + 1                                               05340009
      IVCORR=9                                                          05350009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05360009
 2551 CONTINUE                                                          05370009
      IVTNUM = 255                                                      05380009
C                                                                       05390009
C      ****  TEST 255  ****                                             05400009
C                                                                       05410009
      IF (ICZERO) 32550, 2550, 32550                                    05420009
 2550 CONTINUE                                                          05430009
      IVON01 = 2                                                        05440009
      IVON03 = 4                                                        05450009
      IVCOMP = IVON01 +(3+IVON03)                                       05460009
      GO TO 42550                                                       05470009
32550 IVDELE = IVDELE + 1                                               05480009
      WRITE (I02,80003) IVTNUM                                          05490009
      IF (ICZERO) 42550, 2561, 42550                                    05500009
42550 IF (IVCOMP-9)22550,12550,22550                                    05510009
12550 IVPASS = IVPASS + 1                                               05520009
      WRITE (I02,80001) IVTNUM                                          05530009
      GO TO 2561                                                        05540009
22550 IVFAIL = IVFAIL + 1                                               05550009
      IVCORR =9                                                         05560009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05570009
 2561 CONTINUE                                                          05580009
      IVTNUM = 256                                                      05590009
C                                                                       05600009
C      ****  TEST 256  ****                                             05610009
C                                                                       05620009
      IF (ICZERO) 32560, 2560, 32560                                    05630009
 2560 CONTINUE                                                          05640009
      IVON01 = 2                                                        05650009
      IVON03 = 4                                                        05660009
      IVCOMP =(IVON01+3)+IVON03                                         05670009
      GO TO 42560                                                       05680009
32560 IVDELE = IVDELE + 1                                               05690009
      WRITE (I02,80003) IVTNUM                                          05700009
      IF (ICZERO) 42560, 2571, 42560                                    05710009
42560 IF (IVCOMP-9) 22560,12560,22560                                   05720009
12560 IVPASS = IVPASS + 1                                               05730009
      WRITE (I02,80001) IVTNUM                                          05740009
      GO TO 2571                                                        05750009
22560 IVFAIL = IVFAIL + 1                                               05760009
      IVCORR =9                                                         05770009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05780009
 2571 CONTINUE                                                          05790009
      IVTNUM = 257                                                      05800009
C                                                                       05810009
C      ****  TEST 257  ****                                             05820009
C                                                                       05830009
      IF (ICZERO) 32570, 2570, 32570                                    05840009
 2570 CONTINUE                                                          05850009
      IVON01 = 51                                                       05860009
      IVON03 = 53                                                       05870009
      IVCOMP=IVON01+(52+IVON03)                                         05880009
      GO TO 42570                                                       05890009
32570 IVDELE = IVDELE + 1                                               05900009
      WRITE (I02,80003) IVTNUM                                          05910009
      IF (ICZERO) 42570, 2581, 42570                                    05920009
42570 IF (IVCOMP -156) 22570,12570,22570                                05930009
12570 IVPASS = IVPASS + 1                                               05940009
      WRITE (I02,80001) IVTNUM                                          05950009
      GO TO 2581                                                        05960009
22570 IVFAIL = IVFAIL + 1                                               05970009
      IVCORR = 156                                                      05980009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05990009
 2581 CONTINUE                                                          06000009
      IVTNUM = 258                                                      06010009
C                                                                       06020009
C      ****  TEST 258  ****                                             06030009
C                                                                       06040009
      IF (ICZERO) 32580, 2580, 32580                                    06050009
 2580 CONTINUE                                                          06060009
      IVON01 = 51                                                       06070009
      IVON03 = 53                                                       06080009
      IVCOMP =(IVON01+52)+IVON03                                        06090009
      GO TO 42580                                                       06100009
32580 IVDELE = IVDELE + 1                                               06110009
      WRITE (I02,80003) IVTNUM                                          06120009
      IF (ICZERO) 42580, 2591, 42580                                    06130009
42580 IF (IVCOMP-156) 22580,12580,22580                                 06140009
12580 IVPASS = IVPASS + 1                                               06150009
      WRITE (I02,80001) IVTNUM                                          06160009
      GO TO 2591                                                        06170009
22580 IVFAIL = IVFAIL + 1                                               06180009
      IVCORR = 156                                                      06190009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06200009
 2591 CONTINUE                                                          06210009
      IVTNUM = 259                                                      06220009
C                                                                       06230009
C      ****  TEST 259  ****                                             06240009
C                                                                       06250009
      IF (ICZERO) 32590, 2590, 32590                                    06260009
 2590 CONTINUE                                                          06270009
      IVON02 = 676                                                      06280009
      IVON03 = 101                                                      06290009
      IVCOMP = 189+(IVON02+IVON03)                                      06300009
      GO TO 42590                                                       06310009
32590 IVDELE = IVDELE + 1                                               06320009
      WRITE (I02,80003) IVTNUM                                          06330009
      IF (ICZERO) 42590, 2601, 42590                                    06340009
42590 IF (IVCOMP -966) 22590,12590,22590                                06350009
12590 IVPASS = IVPASS + 1                                               06360009
      WRITE (I02,80001) IVTNUM                                          06370009
      GO TO 2601                                                        06380009
22590 IVFAIL = IVFAIL + 1                                               06390009
      IVCORR =966                                                       06400009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06410009
 2601 CONTINUE                                                          06420009
      IVTNUM = 260                                                      06430009
C                                                                       06440009
C      ****  TEST 260  ****                                             06450009
C                                                                       06460009
      IF (ICZERO) 32600, 2600, 32600                                    06470009
 2600 CONTINUE                                                          06480009
      IVON02 = 676                                                      06490009
      IVON03 = 101                                                      06500009
      IVCOMP = (189 + IVON02) + IVON03                                  06510009
      GO TO 42600                                                       06520009
32600 IVDELE = IVDELE + 1                                               06530009
      WRITE (I02,80003) IVTNUM                                          06540009
      IF (ICZERO) 42600, 2611, 42600                                    06550009
42600 IF (IVCOMP-966) 22600,12600,22600                                 06560009
12600 IVPASS = IVPASS + 1                                               06570009
      WRITE (I02,80001) IVTNUM                                          06580009
      GO TO 2611                                                        06590009
22600 IVFAIL = IVFAIL + 1                                               06600009
      IVCORR=966                                                        06610009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06620009
 2611 CONTINUE                                                          06630009
      IVTNUM = 261                                                      06640009
C                                                                       06650009
C      ****  TEST 261  ****                                             06660009
C                                                                       06670009
      IF (ICZERO) 32610, 2610, 32610                                    06680009
 2610 CONTINUE                                                          06690009
      IVON01 = 1358                                                     06700009
      IVON02 = 8001                                                     06710009
      IVCOMP = IVON01 + (IVON02 + 2189)                                 06720009
      GO TO 42610                                                       06730009
32610 IVDELE = IVDELE + 1                                               06740009
      WRITE (I02,80003) IVTNUM                                          06750009
      IF (ICZERO) 42610, 2621, 42610                                    06760009
42610 IF (IVCOMP-11548) 22610,12610,22610                               06770009
12610 IVPASS = IVPASS + 1                                               06780009
      WRITE (I02,80001) IVTNUM                                          06790009
      GO TO 2621                                                        06800009
22610 IVFAIL = IVFAIL + 1                                               06810009
      IVCORR=11548                                                      06820009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06830009
 2621 CONTINUE                                                          06840009
      IVTNUM = 262                                                      06850009
C                                                                       06860009
C      ****  TEST 262  ****                                             06870009
C                                                                       06880009
      IF (ICZERO) 32620, 2620, 32620                                    06890009
 2620 CONTINUE                                                          06900009
      IVON01 = 1358                                                     06910009
      IVON02 = 8001                                                     06920009
      IVCOMP =(IVON01+IVON02)+2189                                      06930009
      GO TO 42620                                                       06940009
32620 IVDELE = IVDELE + 1                                               06950009
      WRITE (I02,80003) IVTNUM                                          06960009
      IF (ICZERO) 42620, 2631, 42620                                    06970009
42620 IF (IVCOMP-11548) 22620,12620,22620                               06980009
12620 IVPASS = IVPASS + 1                                               06990009
      WRITE (I02,80001) IVTNUM                                          07000009
      GO TO 2631                                                        07010009
22620 IVFAIL = IVFAIL + 1                                               07020009
      IVCORR=11548                                                      07030009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07040009
 2631 CONTINUE                                                          07050009
      IVTNUM = 263                                                      07060009
C                                                                       07070009
C      ****  TEST 263  ****                                             07080009
C         REQUIRES 32767                                                07090009
C                                                                       07100009
      IF (ICZERO) 32630, 2630, 32630                                    07110009
 2630 CONTINUE                                                          07120009
      IVON01 = 16383                                                    07130009
      IVON03 = 16380                                                    07140009
      IVCOMP = IVON01 + (4+IVON03)                                      07150009
      GO TO 42630                                                       07160009
32630 IVDELE = IVDELE + 1                                               07170009
      WRITE (I02,80003) IVTNUM                                          07180009
      IF (ICZERO) 42630, 2641, 42630                                    07190009
42630 IF (IVCOMP-32767) 22630,12630,22630                               07200009
12630 IVPASS = IVPASS + 1                                               07210009
      WRITE (I02,80001) IVTNUM                                          07220009
      GO TO 2641                                                        07230009
22630 IVFAIL = IVFAIL + 1                                               07240009
      IVCORR =32767                                                     07250009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07260009
 2641 CONTINUE                                                          07270009
      IVTNUM = 264                                                      07280009
C                                                                       07290009
C      ****  TEST 264  ****                                             07300009
C         REQUIRES 32767                                                07310009
C                                                                       07320009
      IF (ICZERO) 32640, 2640, 32640                                    07330009
 2640 CONTINUE                                                          07340009
      IVON01 = 16383                                                    07350009
      IVON02 = 16380                                                    07360009
      IVCOMP = (IVON01+IVON02) +4                                       07370009
      GO TO 42640                                                       07380009
32640 IVDELE = IVDELE + 1                                               07390009
      WRITE (I02,80003) IVTNUM                                          07400009
      IF (ICZERO) 42640, 2651, 42640                                    07410009
42640 IF (IVCOMP - 32767) 22640,12640,22640                             07420009
12640 IVPASS = IVPASS + 1                                               07430009
      WRITE (I02,80001) IVTNUM                                          07440009
      GO TO 2651                                                        07450009
22640 IVFAIL = IVFAIL + 1                                               07460009
      IVCORR = 32767                                                    07470009
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07480009
 2651 CONTINUE                                                          07490009
C                                                                       07500009
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07510009
99999 CONTINUE                                                          07520009
      WRITE (I02,90002)                                                 07530009
      WRITE (I02,90006)                                                 07540009
      WRITE (I02,90002)                                                 07550009
      WRITE (I02,90002)                                                 07560009
      WRITE (I02,90007)                                                 07570009
      WRITE (I02,90002)                                                 07580009
      WRITE (I02,90008)  IVFAIL                                         07590009
      WRITE (I02,90009) IVPASS                                          07600009
      WRITE (I02,90010) IVDELE                                          07610009
C                                                                       07620009
C                                                                       07630009
C     TERMINATE ROUTINE EXECUTION                                       07640009
      STOP                                                              07650009
C                                                                       07660009
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07670009
90000 FORMAT (1H1)                                                      07680009
90002 FORMAT (1H )                                                      07690009
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07700009
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07710009
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07720009
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07730009
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07740009
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07750009
C                                                                       07760009
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07770009
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07780009
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07790009
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07800009
C                                                                       07810009
C     FORMAT STATEMENTS FOR TEST RESULTS                                07820009
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07830009
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07840009
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07850009
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07860009
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07870009
C                                                                       07880009
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM009)                          07890009
      END                                                               07900009
