C     COMMENT SECTION                                                   00010042
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020042
C     FM042                                                             00030042
C                                                                       00040042
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE              00050042
C     FORM      INTEGER VARIABLE =  PRIMARY ** PRIMARY                  00060042
C     WHERE THE FIRST OF TWO PRIMARIES IS AN INTEGER VARIABLE OR AN     00070042
C     INTEGER CONSTANT AND THE SECOND PRIMARY IS AN INTEGER VARIABLE.   00080042
C                                                                       00090042
C      REFERENCES                                                       00100042
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110042
C              X3.9-1978                                                00120042
C                                                                       00130042
C        SECTION 4.3, INTEGER TYPE                                      00140042
C        SECTION 4.3.1, INTEGER CONSTANT                                00150042
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00160042
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00170042
C                                                                       00180042
C                                                                       00190042
C      **********************************************************       00200042
C                                                                       00210042
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00220042
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00230042
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00240042
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00250042
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00260042
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00270042
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00280042
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00290042
C     OF EXECUTING THESE TESTS.                                         00300042
C                                                                       00310042
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00320042
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00330042
C                                                                       00340042
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00350042
C                                                                       00360042
C                  DEPARTMENT OF THE NAVY                               00370042
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00380042
C                  WASHINGTON, D.C.  20376                              00390042
C                                                                       00400042
C      **********************************************************       00410042
C                                                                       00420042
C                                                                       00430042
C                                                                       00440042
C     INITIALIZATION SECTION                                            00450042
C                                                                       00460042
C     INITIALIZE CONSTANTS                                              00470042
C      **************                                                   00480042
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00490042
      I01 = 5                                                           00500042
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00510042
      I02 = 6                                                           00520042
C     SYSTEM ENVIRONMENT SECTION                                        00530042
C                                                                       00540042
      I01 = 5                                                           00550042
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00560042
C     (UNIT NUMBER FOR CARD READER).                                    00570042
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00580042
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00590042
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00600042
C                                                                       00610042
      I02 = 6                                                           00620042
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00630042
C     (UNIT NUMBER FOR PRINTER).                                        00640042
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00650042
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00660042
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00670042
C                                                                       00680042
      IVPASS=0                                                          00690042
      IVFAIL=0                                                          00700042
      IVDELE=0                                                          00710042
      ICZERO=0                                                          00720042
C                                                                       00730042
C     WRITE PAGE HEADERS                                                00740042
      WRITE (I02,90000)                                                 00750042
      WRITE (I02,90001)                                                 00760042
      WRITE (I02,90002)                                                 00770042
      WRITE (I02, 90002)                                                00780042
      WRITE (I02,90003)                                                 00790042
      WRITE (I02,90002)                                                 00800042
      WRITE (I02,90004)                                                 00810042
      WRITE (I02,90002)                                                 00820042
      WRITE (I02,90011)                                                 00830042
      WRITE (I02,90002)                                                 00840042
      WRITE (I02,90002)                                                 00850042
      WRITE (I02,90005)                                                 00860042
      WRITE (I02,90006)                                                 00870042
      WRITE (I02,90002)                                                 00880042
C                                                                       00890042
C     TEST SECTION                                                      00900042
C                                                                       00910042
C         ARITHMETIC ASSIGNMENT STATEMENT                               00920042
C                                                                       00930042
C     TEST 649 THROUGH TEST 665 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS00940042
C     OF THE FORM    INTEGER VARIABLE = INTEGER CONST. ** INTEGER VAR.  00950042
C                                                                       00960042
C     TEST 666 THROUGH TEST 682 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS00970042
C     OF THE FORM    INTEGER VARIABLE = INTEGER VAR. ** INTEGER VAR.    00980042
C                                                                       00990042
C                                                                       01000042
      IVTNUM = 649                                                      01010042
C                                                                       01020042
C      ****  TEST 649  ****                                             01030042
C     TEST 649  - SMALL NUMBER BASE; ZERO EXPONENT                      01040042
C                                                                       01050042
      IF (ICZERO) 36490, 6490, 36490                                    01060042
 6490 CONTINUE                                                          01070042
      IVON01 = 0                                                        01080042
      IVCOMP = 1 ** IVON01                                              01090042
      GO TO 46490                                                       01100042
36490 IVDELE = IVDELE + 1                                               01110042
      WRITE (I02,80003) IVTNUM                                          01120042
      IF (ICZERO) 46490, 6501, 46490                                    01130042
46490 IF (IVCOMP - 1) 26490,16490,26490                                 01140042
16490 IVPASS = IVPASS + 1                                               01150042
      WRITE (I02,80001) IVTNUM                                          01160042
      GO TO 6501                                                        01170042
26490 IVFAIL = IVFAIL + 1                                               01180042
      IVCORR = 1                                                        01190042
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01200042
 6501 CONTINUE                                                          01210042
      IVTNUM = 650                                                      01220042
C                                                                       01230042
C      ****  TEST 650  ****                                             01240042
C     TEST 650  - ZERO BASE TO FIRST POWER                              01250042
C                                                                       01260042
      IF (ICZERO) 36500, 6500, 36500                                    01270042
 6500 CONTINUE                                                          01280042
      IVON01 = 1                                                        01290042
      IVCOMP = 0 ** IVON01                                              01300042
      GO TO 46500                                                       01310042
36500 IVDELE = IVDELE + 1                                               01320042
      WRITE (I02,80003) IVTNUM                                          01330042
      IF (ICZERO) 46500, 6511, 46500                                    01340042
46500 IF (IVCOMP) 26500,16500,26500                                     01350042
16500 IVPASS = IVPASS + 1                                               01360042
      WRITE (I02,80001) IVTNUM                                          01370042
      GO TO 6511                                                        01380042
26500 IVFAIL = IVFAIL + 1                                               01390042
      IVCORR = 0                                                        01400042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01410042
 6511 CONTINUE                                                          01420042
      IVTNUM = 651                                                      01430042
C                                                                       01440042
C      ****  TEST 651  ****                                             01450042
C     TEST 651  - BASE =1; EXPONENT = 1                                 01460042
C                                                                       01470042
      IF (ICZERO) 36510, 6510, 36510                                    01480042
 6510 CONTINUE                                                          01490042
      IVON01 = 1                                                        01500042
      IVCOMP = 1 ** IVON01                                              01510042
      GO TO 46510                                                       01520042
36510 IVDELE = IVDELE + 1                                               01530042
      WRITE (I02,80003) IVTNUM                                          01540042
      IF (ICZERO) 46510, 6521, 46510                                    01550042
46510 IF (IVCOMP - 1) 26510,16510,26510                                 01560042
16510 IVPASS = IVPASS + 1                                               01570042
      WRITE (I02,80001) IVTNUM                                          01580042
      GO TO 6521                                                        01590042
26510 IVFAIL = IVFAIL + 1                                               01600042
      IVCORR = 1                                                        01610042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01620042
 6521 CONTINUE                                                          01630042
      IVTNUM = 652                                                      01640042
C                                                                       01650042
C      ****  TEST 652  ****                                             01660042
C     TEST 652  - LARGE EXPONENT                                        01670042
C                                                                       01680042
      IF (ICZERO) 36520, 6520, 36520                                    01690042
 6520 CONTINUE                                                          01700042
      IVON01 = 32767                                                    01710042
      IVCOMP = 1 ** IVON01                                              01720042
      GO TO 46520                                                       01730042
36520 IVDELE = IVDELE + 1                                               01740042
      WRITE (I02,80003) IVTNUM                                          01750042
      IF (ICZERO) 46520, 6531, 46520                                    01760042
46520 IF (IVCOMP - 1) 26520,16520,26520                                 01770042
16520 IVPASS = IVPASS + 1                                               01780042
      WRITE (I02,80001) IVTNUM                                          01790042
      GO TO 6531                                                        01800042
26520 IVFAIL = IVFAIL + 1                                               01810042
      IVCORR = 1                                                        01820042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01830042
 6531 CONTINUE                                                          01840042
      IVTNUM = 653                                                      01850042
C                                                                       01860042
C      ****  TEST 653  ****                                             01870042
C     TEST 653  - LARGE NUMBER BASE; EXPONENT = 1                       01880042
C                                                                       01890042
      IF (ICZERO) 36530, 6530, 36530                                    01900042
 6530 CONTINUE                                                          01910042
      IVON01 = 1                                                        01920042
      IVCOMP = 32767 ** IVON01                                          01930042
      GO TO 46530                                                       01940042
36530 IVDELE = IVDELE + 1                                               01950042
      WRITE (I02,80003) IVTNUM                                          01960042
      IF (ICZERO) 46530, 6541, 46530                                    01970042
46530 IF (IVCOMP - 32767) 26530,16530,26530                             01980042
16530 IVPASS = IVPASS + 1                                               01990042
      WRITE (I02,80001) IVTNUM                                          02000042
      GO TO 6541                                                        02010042
26530 IVFAIL = IVFAIL + 1                                               02020042
      IVCORR = 32767                                                    02030042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02040042
 6541 CONTINUE                                                          02050042
      IVTNUM = 654                                                      02060042
C                                                                       02070042
C      ****  TEST 654  ****                                             02080042
C     TEST 654  - ZERO BASE; LARGE NUMBER EXPONENT                      02090042
C                                                                       02100042
      IF (ICZERO) 36540, 6540, 36540                                    02110042
 6540 CONTINUE                                                          02120042
      IVON01 = 32767                                                    02130042
      IVCOMP = 0 ** IVON01                                              02140042
      GO TO 46540                                                       02150042
36540 IVDELE = IVDELE + 1                                               02160042
      WRITE (I02,80003) IVTNUM                                          02170042
      IF (ICZERO) 46540, 6551, 46540                                    02180042
46540 IF (IVCOMP) 26540,16540,26540                                     02190042
16540 IVPASS = IVPASS + 1                                               02200042
      WRITE (I02,80001) IVTNUM                                          02210042
      GO TO 6551                                                        02220042
26540 IVFAIL = IVFAIL + 1                                               02230042
      IVCORR = 0                                                        02240042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02250042
 6551 CONTINUE                                                          02260042
      IVTNUM = 655                                                      02270042
C                                                                       02280042
C      ****  TEST 655  ****                                             02290042
C     TEST 655  -LARGE NUMBER BASE; ZERO EXPONENT                       02300042
C                                                                       02310042
      IF (ICZERO) 36550, 6550, 36550                                    02320042
 6550 CONTINUE                                                          02330042
      IVON01 = 0                                                        02340042
      IVCOMP = 32767 ** IVON01                                          02350042
      GO TO 46550                                                       02360042
36550 IVDELE = IVDELE + 1                                               02370042
      WRITE (I02,80003) IVTNUM                                          02380042
      IF (ICZERO) 46550, 6561, 46550                                    02390042
46550 IF (IVCOMP -1) 26550,16550,26550                                  02400042
16550 IVPASS = IVPASS + 1                                               02410042
      WRITE (I02,80001) IVTNUM                                          02420042
      GO TO 6561                                                        02430042
26550 IVFAIL = IVFAIL + 1                                               02440042
      IVCORR = 1                                                        02450042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02460042
 6561 CONTINUE                                                          02470042
      IVTNUM = 656                                                      02480042
C                                                                       02490042
C      ****  TEST 656  ****                                             02500042
C     TEST 656  -EXPONENT IS POWER OF TWO                               02510042
C                                                                       02520042
      IF (ICZERO) 36560, 6560, 36560                                    02530042
 6560 CONTINUE                                                          02540042
      IVON01 = 2                                                        02550042
      IVCOMP = 181 ** IVON01                                            02560042
      GO TO 46560                                                       02570042
36560 IVDELE = IVDELE + 1                                               02580042
      WRITE (I02,80003) IVTNUM                                          02590042
      IF (ICZERO) 46560, 6571, 46560                                    02600042
46560 IF (IVCOMP - 32761) 26560,16560,26560                             02610042
16560 IVPASS = IVPASS + 1                                               02620042
      WRITE (I02,80001) IVTNUM                                          02630042
      GO TO 6571                                                        02640042
26560 IVFAIL = IVFAIL + 1                                               02650042
      IVCORR = 32761                                                    02660042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02670042
 6571 CONTINUE                                                          02680042
      IVTNUM = 657                                                      02690042
C                                                                       02700042
C      ****  TEST 657  ****                                             02710042
C     TEST 657  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              02720042
C                                                                       02730042
      IF (ICZERO) 36570, 6570, 36570                                    02740042
 6570 CONTINUE                                                          02750042
      IVON01 = 8                                                        02760042
      IVCOMP = 2 ** IVON01                                              02770042
      GO TO 46570                                                       02780042
36570 IVDELE = IVDELE + 1                                               02790042
      WRITE (I02,80003) IVTNUM                                          02800042
      IF (ICZERO) 46570, 6581, 46570                                    02810042
46570 IF (IVCOMP - 256) 26570,16570,26560                               02820042
16570 IVPASS = IVPASS + 1                                               02830042
      WRITE (I02,80001) IVTNUM                                          02840042
      GO TO 6581                                                        02850042
26570 IVFAIL = IVFAIL + 1                                               02860042
      IVCORR = 256                                                      02870042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02880042
 6581 CONTINUE                                                          02890042
C                                                                       02900042
C     TESTS 658 AND 659 TEST TO ENSURE EXPONENTIATION OPERATOR IS       02910042
C                       NOT COMMUTATIVE                                 02920042
C                                                                       02930042
      IVTNUM = 658                                                      02940042
C                                                                       02950042
C      ****  TEST 658  ****                                             02960042
C                                                                       02970042
      IF (ICZERO) 36580, 6580, 36580                                    02980042
 6580 CONTINUE                                                          02990042
      IVON01 = 9                                                        03000042
      IVCOMP = 3 ** IVON01                                              03010042
      GO TO 46580                                                       03020042
36580 IVDELE = IVDELE + 1                                               03030042
      WRITE (I02,80003) IVTNUM                                          03040042
      IF (ICZERO) 46580, 6591, 46580                                    03050042
46580 IF (IVCOMP - 19683) 26580,16580,26580                             03060042
16580 IVPASS = IVPASS + 1                                               03070042
      WRITE (I02,80001) IVTNUM                                          03080042
      GO TO 6591                                                        03090042
26580 IVFAIL = IVFAIL + 1                                               03100042
      IVCORR = 19683                                                    03110042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03120042
 6591 CONTINUE                                                          03130042
      IVTNUM = 659                                                      03140042
C                                                                       03150042
C      ****  TEST 659  ****                                             03160042
C                                                                       03170042
      IF (ICZERO) 36590, 6590, 36590                                    03180042
 6590 CONTINUE                                                          03190042
      IVON01 = 3                                                        03200042
      IVCOMP = 9 ** IVON01                                              03210042
      GO TO 46590                                                       03220042
36590 IVDELE = IVDELE + 1                                               03230042
      WRITE (I02,80003) IVTNUM                                          03240042
      IF (ICZERO) 46590, 6601, 46590                                    03250042
46590 IF (IVCOMP - 729) 26590,16590,26590                               03260042
16590 IVPASS = IVPASS + 1                                               03270042
      WRITE (I02,80001) IVTNUM                                          03280042
      GO TO 6601                                                        03290042
26590 IVFAIL = IVFAIL + 1                                               03300042
      IVCORR = 729                                                      03310042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03320042
 6601 CONTINUE                                                          03330042
C                                                                       03340042
C     TESTS 660 THROUGH 665 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE03350042
C                           ODD AND EVEN NUMBER POWERS CHECKING THE SIGN03360042
C                           OF THE RESULTS                              03370042
C                                                                       03380042
      IVTNUM = 660                                                      03390042
C                                                                       03400042
C      ****  TEST 660  ****                                             03410042
C                                                                       03420042
      IF (ICZERO) 36600, 6600, 36600                                    03430042
 6600 CONTINUE                                                          03440042
      IVON01 = 2                                                        03450042
      IVCOMP = 1 ** IVON01                                              03460042
      GO TO 46600                                                       03470042
36600 IVDELE = IVDELE + 1                                               03480042
      WRITE (I02,80003) IVTNUM                                          03490042
      IF (ICZERO) 46600, 6611, 46600                                    03500042
46600 IF (IVCOMP - 1) 26600,16600,26600                                 03510042
16600 IVPASS = IVPASS + 1                                               03520042
      WRITE (I02,80001) IVTNUM                                          03530042
      GO TO 6611                                                        03540042
26600 IVFAIL = IVFAIL + 1                                               03550042
      IVCORR = 1                                                        03560042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03570042
 6611 CONTINUE                                                          03580042
      IVTNUM = 661                                                      03590042
C                                                                       03600042
C      ****  TEST 661  ****                                             03610042
C                                                                       03620042
      IF (ICZERO) 36610, 6610, 36610                                    03630042
 6610 CONTINUE                                                          03640042
      IVON01 = 2                                                        03650042
      IVCOMP = ( -1) ** IVON01                                          03660042
      GO TO 46610                                                       03670042
36610 IVDELE = IVDELE + 1                                               03680042
      WRITE (I02,80003) IVTNUM                                          03690042
      IF (ICZERO) 46610, 6621, 46610                                    03700042
46610 IF (IVCOMP - 1) 26610,16610,26610                                 03710042
16610 IVPASS = IVPASS + 1                                               03720042
      WRITE (I02,80001) IVTNUM                                          03730042
      GO TO 6621                                                        03740042
26610 IVFAIL = IVFAIL + 1                                               03750042
      IVCORR = 1                                                        03760042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03770042
 6621 CONTINUE                                                          03780042
      IVTNUM = 662                                                      03790042
C                                                                       03800042
C      ****  TEST 662  ****                                             03810042
C                                                                       03820042
      IF (ICZERO) 36620, 6620, 36620                                    03830042
 6620 CONTINUE                                                          03840042
      IVON01 = 3                                                        03850042
      IVCOMP = 7 ** IVON01                                              03860042
      GO TO 46620                                                       03870042
36620 IVDELE = IVDELE + 1                                               03880042
      WRITE (I02,80003) IVTNUM                                          03890042
      IF (ICZERO) 46620, 6631, 46620                                    03900042
46620 IF (IVCOMP - 343) 26620,16620,26620                               03910042
16620 IVPASS = IVPASS + 1                                               03920042
      WRITE (I02,80001) IVTNUM                                          03930042
      GO TO 6631                                                        03940042
26620 IVFAIL = IVFAIL + 1                                               03950042
      IVCORR = 343                                                      03960042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03970042
 6631 CONTINUE                                                          03980042
      IVTNUM = 663                                                      03990042
C                                                                       04000042
C      ****  TEST 663  ****                                             04010042
C                                                                       04020042
      IF (ICZERO) 36630, 6630, 36630                                    04030042
 6630 CONTINUE                                                          04040042
      IVON01 = 3                                                        04050042
      IVCOMP = (-7) **IVON01                                            04060042
      GO TO 46630                                                       04070042
36630 IVDELE = IVDELE + 1                                               04080042
      WRITE (I02,80003) IVTNUM                                          04090042
      IF (ICZERO) 46630, 6641, 46630                                    04100042
46630 IF (IVCOMP + 343) 26630,16630,26630                               04110042
16630 IVPASS = IVPASS + 1                                               04120042
      WRITE (I02,80001) IVTNUM                                          04130042
      GO TO 6641                                                        04140042
26630 IVFAIL = IVFAIL + 1                                               04150042
      IVCORR = -343                                                     04160042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04170042
 6641 CONTINUE                                                          04180042
      IVTNUM = 664                                                      04190042
C                                                                       04200042
C      ****  TEST 664  ****                                             04210042
C                                                                       04220042
      IF (ICZERO) 36640, 6640, 36640                                    04230042
 6640 CONTINUE                                                          04240042
      IVON01 = 4                                                        04250042
      IVCOMP = 7 ** IVON01                                              04260042
      GO TO 46640                                                       04270042
36640 IVDELE = IVDELE + 1                                               04280042
      WRITE (I02,80003) IVTNUM                                          04290042
      IF (ICZERO) 46640, 6651, 46640                                    04300042
46640 IF (IVCOMP - 2401) 26640,16640,26640                              04310042
16640 IVPASS = IVPASS + 1                                               04320042
      WRITE (I02,80001) IVTNUM                                          04330042
      GO TO 6651                                                        04340042
26640 IVFAIL = IVFAIL + 1                                               04350042
      IVCORR = 2401                                                     04360042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04370042
 6651 CONTINUE                                                          04380042
      IVTNUM = 665                                                      04390042
C                                                                       04400042
C      ****  TEST 665  ****                                             04410042
C                                                                       04420042
      IF (ICZERO) 36650, 6650, 36650                                    04430042
 6650 CONTINUE                                                          04440042
      IVON01 = 4                                                        04450042
      IVCOMP = (-7) ** IVON01                                           04460042
      GO TO 46650                                                       04470042
36650 IVDELE = IVDELE + 1                                               04480042
      WRITE (I02,80003) IVTNUM                                          04490042
      IF (ICZERO) 46650, 6661, 46650                                    04500042
46650 IF (IVCOMP - 2401) 26650,16650,26650                              04510042
16650 IVPASS = IVPASS + 1                                               04520042
      WRITE (I02,80001) IVTNUM                                          04530042
      GO TO 6661                                                        04540042
26650 IVFAIL = IVFAIL + 1                                               04550042
      IVCORR = 2401                                                     04560042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04570042
 6661 CONTINUE                                                          04580042
      IVTNUM = 666                                                      04590042
C                                                                       04600042
C      ****  TEST 666  ****                                             04610042
C     TEST 666  - SMALL NUMBER BASE; ZERO EXPONENT                      04620042
C                                                                       04630042
      IF (ICZERO) 36660, 6660, 36660                                    04640042
 6660 CONTINUE                                                          04650042
      IVON01 = 1                                                        04660042
      IVON02 = 0                                                        04670042
      IVCOMP = IVON01 ** IVON02                                         04680042
      GO TO 46660                                                       04690042
36660 IVDELE = IVDELE + 1                                               04700042
      WRITE (I02,80003) IVTNUM                                          04710042
      IF (ICZERO) 46660, 6671, 46660                                    04720042
46660 IF (IVCOMP - 1) 26660,16660,26660                                 04730042
16660 IVPASS = IVPASS + 1                                               04740042
      WRITE (I02,80001) IVTNUM                                          04750042
      GO TO 6671                                                        04760042
26660 IVFAIL = IVFAIL + 1                                               04770042
      IVCORR = 1                                                        04780042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04790042
 6671 CONTINUE                                                          04800042
      IVTNUM = 667                                                      04810042
C                                                                       04820042
C      ****  TEST 667  ****                                             04830042
C     TEST 667  - ZERO BASE TO FIRST POWER                              04840042
C                                                                       04850042
      IF (ICZERO) 36670, 6670, 36670                                    04860042
 6670 CONTINUE                                                          04870042
      IVON01 = 0                                                        04880042
      IVON02 = 1                                                        04890042
      IVCOMP = IVON01 ** IVON02                                         04900042
      GO TO 46670                                                       04910042
36670 IVDELE = IVDELE + 1                                               04920042
      WRITE (I02,80003) IVTNUM                                          04930042
      IF (ICZERO) 46670, 6681, 46670                                    04940042
46670 IF (IVCOMP) 26670,16670,26670                                     04950042
16670 IVPASS = IVPASS + 1                                               04960042
      WRITE (I02,80001) IVTNUM                                          04970042
      GO TO 6681                                                        04980042
26670 IVFAIL = IVFAIL + 1                                               04990042
      IVCORR = 0                                                        05000042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05010042
 6681 CONTINUE                                                          05020042
      IVTNUM = 668                                                      05030042
C                                                                       05040042
C      ****  TEST 668  ****                                             05050042
C     TEST 668  - BASE =1; EXPONENT = 1                                 05060042
C                                                                       05070042
      IF (ICZERO) 36680, 6680, 36680                                    05080042
 6680 CONTINUE                                                          05090042
      IVON01 = 1                                                        05100042
      IVON02 = 1                                                        05110042
      IVCOMP = IVON01 ** IVON02                                         05120042
      GO TO 46680                                                       05130042
36680 IVDELE = IVDELE + 1                                               05140042
      WRITE (I02,80003) IVTNUM                                          05150042
      IF (ICZERO) 46680, 6691, 46680                                    05160042
46680 IF (IVCOMP - 1) 26680,16680,26680                                 05170042
16680 IVPASS = IVPASS + 1                                               05180042
      WRITE (I02,80001) IVTNUM                                          05190042
      GO TO 6691                                                        05200042
26680 IVFAIL = IVFAIL + 1                                               05210042
      IVCORR = 1                                                        05220042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05230042
 6691 CONTINUE                                                          05240042
      IVTNUM = 669                                                      05250042
C                                                                       05260042
C      ****  TEST 669  ****                                             05270042
C     TEST 669  - LARGE EXPONENT                                        05280042
C                                                                       05290042
      IF (ICZERO) 36690, 6690, 36690                                    05300042
 6690 CONTINUE                                                          05310042
      IVON01 = 1                                                        05320042
      IVON02 = 32767                                                    05330042
      IVCOMP = IVON01 ** IVON02                                         05340042
      GO TO 46690                                                       05350042
36690 IVDELE = IVDELE + 1                                               05360042
      WRITE (I02,80003) IVTNUM                                          05370042
      IF (ICZERO) 46690, 6701, 46690                                    05380042
46690 IF (IVCOMP - 1) 26690,16690,26690                                 05390042
16690 IVPASS = IVPASS + 1                                               05400042
      WRITE (I02,80001) IVTNUM                                          05410042
      GO TO 6701                                                        05420042
26690 IVFAIL = IVFAIL + 1                                               05430042
      IVCORR = 1                                                        05440042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05450042
 6701 CONTINUE                                                          05460042
      IVTNUM = 670                                                      05470042
C                                                                       05480042
C      ****  TEST 670  ****                                             05490042
C     TEST 670  - LARGE NUMBER BASE; EXPONENT = 1                       05500042
C                                                                       05510042
      IF (ICZERO) 36700, 6700, 36700                                    05520042
 6700 CONTINUE                                                          05530042
      IVON01 = 32767                                                    05540042
      IVON02 = 1                                                        05550042
      IVCOMP = IVON01 ** IVON02                                         05560042
      GO TO 46700                                                       05570042
36700 IVDELE = IVDELE + 1                                               05580042
      WRITE (I02,80003) IVTNUM                                          05590042
      IF (ICZERO) 46700, 6711, 46700                                    05600042
46700 IF (IVCOMP - 32767) 26700,16700,26700                             05610042
16700 IVPASS = IVPASS + 1                                               05620042
      WRITE (I02,80001) IVTNUM                                          05630042
      GO TO 6711                                                        05640042
26700 IVFAIL = IVFAIL + 1                                               05650042
      IVCORR = 32767                                                    05660042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05670042
 6711 CONTINUE                                                          05680042
      IVTNUM = 671                                                      05690042
C                                                                       05700042
C      ****  TEST 671  ****                                             05710042
C     TEST 671  - ZERO BASE; LARGE NUMBER EXPONENT                      05720042
C                                                                       05730042
      IF (ICZERO) 36710, 6710, 36710                                    05740042
 6710 CONTINUE                                                          05750042
      IVON01 = 0                                                        05760042
      IVON02 = 32767                                                    05770042
      IVCOMP = IVON01 ** IVON02                                         05780042
      GO TO 46710                                                       05790042
36710 IVDELE = IVDELE + 1                                               05800042
      WRITE (I02,80003) IVTNUM                                          05810042
      IF (ICZERO) 46710, 6721, 46710                                    05820042
46710 IF (IVCOMP) 26710,16710,26710                                     05830042
16710 IVPASS = IVPASS + 1                                               05840042
      WRITE (I02,80001) IVTNUM                                          05850042
      GO TO 6721                                                        05860042
26710 IVFAIL = IVFAIL + 1                                               05870042
      IVCORR = 0                                                        05880042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05890042
 6721 CONTINUE                                                          05900042
      IVTNUM = 672                                                      05910042
C                                                                       05920042
C      ****  TEST 672  ****                                             05930042
C     TEST 672  -LARGE NUMBER BASE; ZERO EXPONENT                       05940042
C                                                                       05950042
      IF (ICZERO) 36720, 6720, 36720                                    05960042
 6720 CONTINUE                                                          05970042
      IVON01 = 32767                                                    05980042
      IVON02 = 0                                                        05990042
      IVCOMP = IVON01 ** IVON02                                         06000042
      GO TO 46720                                                       06010042
36720 IVDELE = IVDELE + 1                                               06020042
      WRITE (I02,80003) IVTNUM                                          06030042
      IF (ICZERO) 46720, 6731, 46720                                    06040042
46720 IF (IVCOMP -1) 26720,16720,26720                                  06050042
16720 IVPASS = IVPASS + 1                                               06060042
      WRITE (I02,80001) IVTNUM                                          06070042
      GO TO 6731                                                        06080042
26720 IVFAIL = IVFAIL + 1                                               06090042
      IVCORR = 1                                                        06100042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06110042
 6731 CONTINUE                                                          06120042
      IVTNUM = 673                                                      06130042
C                                                                       06140042
C      ****  TEST 673  ****                                             06150042
C     TEST 673  -EXPONENT IS POWER OF TWO                               06160042
C                                                                       06170042
      IF (ICZERO) 36730, 6730, 36730                                    06180042
 6730 CONTINUE                                                          06190042
      IVON01 = 181                                                      06200042
      IVON02 = 2                                                        06210042
      IVCOMP = IVON01 ** IVON02                                         06220042
      GO TO 46730                                                       06230042
36730 IVDELE = IVDELE + 1                                               06240042
      WRITE (I02,80003) IVTNUM                                          06250042
      IF (ICZERO) 46730, 6741, 46730                                    06260042
46730 IF (IVCOMP - 32761) 26730,16730,26730                             06270042
16730 IVPASS = IVPASS + 1                                               06280042
      WRITE (I02,80001) IVTNUM                                          06290042
      GO TO 6741                                                        06300042
26730 IVFAIL = IVFAIL + 1                                               06310042
      IVCORR = 32761                                                    06320042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06330042
 6741 CONTINUE                                                          06340042
      IVTNUM = 674                                                      06350042
C                                                                       06360042
C      ****  TEST 674  ****                                             06370042
C     TEST 674  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              06380042
C                                                                       06390042
      IF (ICZERO) 36740, 6740, 36740                                    06400042
 6740 CONTINUE                                                          06410042
      IVON01 = 2                                                        06420042
      IVON02 = 8                                                        06430042
      IVCOMP = IVON01 ** IVON02                                         06440042
      GO TO 46740                                                       06450042
36740 IVDELE = IVDELE + 1                                               06460042
      WRITE (I02,80003) IVTNUM                                          06470042
      IF (ICZERO) 46740, 6751, 46740                                    06480042
46740 IF (IVCOMP - 256) 26740,16740,26740                               06490042
16740 IVPASS = IVPASS + 1                                               06500042
      WRITE (I02,80001) IVTNUM                                          06510042
      GO TO 6751                                                        06520042
26740 IVFAIL = IVFAIL + 1                                               06530042
      IVCORR = 256                                                      06540042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06550042
 6751 CONTINUE                                                          06560042
C                                                                       06570042
C     TESTS 675 AND 676 TEST TO ENSURE EXPONENTIATION OPERATOR IS       06580042
C                       NOT COMMUTATIVE                                 06590042
C                                                                       06600042
      IVTNUM = 675                                                      06610042
C                                                                       06620042
C      ****  TEST 675  ****                                             06630042
C                                                                       06640042
      IF (ICZERO) 36750, 6750, 36750                                    06650042
 6750 CONTINUE                                                          06660042
      IVON01 = 3                                                        06670042
      IVON02 = 9                                                        06680042
      IVCOMP = IVON01 ** IVON02                                         06690042
      GO TO 46750                                                       06700042
36750 IVDELE = IVDELE + 1                                               06710042
      WRITE (I02,80003) IVTNUM                                          06720042
      IF (ICZERO) 46750, 6761, 46750                                    06730042
46750 IF (IVCOMP - 19683) 26750,16750,26750                             06740042
16750 IVPASS = IVPASS + 1                                               06750042
      WRITE (I02,80001) IVTNUM                                          06760042
      GO TO 6761                                                        06770042
26750 IVFAIL = IVFAIL + 1                                               06780042
      IVCORR = 19683                                                    06790042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06800042
 6761 CONTINUE                                                          06810042
      IVTNUM = 676                                                      06820042
C                                                                       06830042
C      ****  TEST 676  ****                                             06840042
C                                                                       06850042
      IF (ICZERO) 36760, 6760, 36760                                    06860042
 6760 CONTINUE                                                          06870042
      IVON01 = 9                                                        06880042
      IVON02 = 3                                                        06890042
      IVCOMP = IVON01 ** IVON02                                         06900042
      GO TO 46760                                                       06910042
36760 IVDELE = IVDELE + 1                                               06920042
      WRITE (I02,80003) IVTNUM                                          06930042
      IF (ICZERO) 46760, 6771, 46760                                    06940042
46760 IF (IVCOMP - 729) 26760,16760,26760                               06950042
16760 IVPASS = IVPASS + 1                                               06960042
      WRITE (I02,80001) IVTNUM                                          06970042
      GO TO 6771                                                        06980042
26760 IVFAIL = IVFAIL + 1                                               06990042
      IVCORR = 729                                                      07000042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07010042
 6771 CONTINUE                                                          07020042
C                                                                       07030042
C     TESTS 677 THROUGH 682 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE07040042
C                           ODD AND EVEN NUMBER POWERS CHECKING THE SIGN07050042
C                           OF THE RESULTS                              07060042
C                                                                       07070042
      IVTNUM = 677                                                      07080042
C                                                                       07090042
C      ****  TEST 677  ****                                             07100042
C                                                                       07110042
      IF (ICZERO) 36770, 6770, 36770                                    07120042
 6770 CONTINUE                                                          07130042
      IVON01 = 1                                                        07140042
      IVON02 = 2                                                        07150042
      IVCOMP = IVON01 ** IVON02                                         07160042
      GO TO 46770                                                       07170042
36770 IVDELE = IVDELE + 1                                               07180042
      WRITE (I02,80003) IVTNUM                                          07190042
      IF (ICZERO) 46770, 6781, 46770                                    07200042
46770 IF (IVCOMP - 1) 26770,16770,26770                                 07210042
16770 IVPASS = IVPASS + 1                                               07220042
      WRITE (I02,80001) IVTNUM                                          07230042
      GO TO 6781                                                        07240042
26770 IVFAIL = IVFAIL + 1                                               07250042
      IVCORR = 1                                                        07260042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07270042
 6781 CONTINUE                                                          07280042
      IVTNUM = 678                                                      07290042
C                                                                       07300042
C      ****  TEST 678  ****                                             07310042
C                                                                       07320042
      IF (ICZERO) 36780, 6780, 36780                                    07330042
 6780 CONTINUE                                                          07340042
      IVON01 = -1                                                       07350042
      IVON02 = 2                                                        07360042
      IVCOMP = IVON01 ** IVON02                                         07370042
      GO TO 46780                                                       07380042
36780 IVDELE = IVDELE + 1                                               07390042
      WRITE (I02,80003) IVTNUM                                          07400042
      IF (ICZERO) 46780, 6791, 46780                                    07410042
46780 IF (IVCOMP - 1) 26780,16780,26780                                 07420042
16780 IVPASS = IVPASS + 1                                               07430042
      WRITE (I02,80001) IVTNUM                                          07440042
      GO TO 6791                                                        07450042
26780 IVFAIL = IVFAIL + 1                                               07460042
      IVCORR = 1                                                        07470042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07480042
 6791 CONTINUE                                                          07490042
      IVTNUM = 679                                                      07500042
C                                                                       07510042
C      ****  TEST 679  ****                                             07520042
C                                                                       07530042
      IF (ICZERO) 36790, 6790, 36790                                    07540042
 6790 CONTINUE                                                          07550042
      IVON01 = 7                                                        07560042
      IVON02 = 3                                                        07570042
      IVCOMP = IVON01 ** IVON02                                         07580042
      GO TO 46790                                                       07590042
36790 IVDELE = IVDELE + 1                                               07600042
      WRITE (I02,80003) IVTNUM                                          07610042
      IF (ICZERO) 46790, 6801, 46790                                    07620042
46790 IF (IVCOMP - 343) 26790,16790,26790                               07630042
16790 IVPASS = IVPASS + 1                                               07640042
      WRITE (I02,80001) IVTNUM                                          07650042
      GO TO 6801                                                        07660042
26790 IVFAIL = IVFAIL + 1                                               07670042
      IVCORR = 343                                                      07680042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07690042
 6801 CONTINUE                                                          07700042
      IVTNUM = 680                                                      07710042
C                                                                       07720042
C      ****  TEST 680  ****                                             07730042
C                                                                       07740042
      IF (ICZERO) 36800, 6800, 36800                                    07750042
 6800 CONTINUE                                                          07760042
      IVON01 = -7                                                       07770042
      IVON02 = 3                                                        07780042
      IVCOMP = IVON01 ** IVON02                                         07790042
      GO TO 46800                                                       07800042
36800 IVDELE = IVDELE + 1                                               07810042
      WRITE (I02,80003) IVTNUM                                          07820042
      IF (ICZERO) 46800, 6811, 46800                                    07830042
46800 IF (IVCOMP + 343) 26800,16800,26800                               07840042
16800 IVPASS = IVPASS + 1                                               07850042
      WRITE (I02,80001) IVTNUM                                          07860042
      GO TO 6811                                                        07870042
26800 IVFAIL = IVFAIL + 1                                               07880042
      IVCORR = -343                                                     07890042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07900042
 6811 CONTINUE                                                          07910042
      IVTNUM = 681                                                      07920042
C                                                                       07930042
C      ****  TEST 681  ****                                             07940042
C                                                                       07950042
      IF (ICZERO) 36810, 6810, 36810                                    07960042
 6810 CONTINUE                                                          07970042
      IVON01 = 7                                                        07980042
      IVON02 = 4                                                        07990042
      IVCOMP = IVON01 ** IVON02                                         08000042
      GO TO 46810                                                       08010042
36810 IVDELE = IVDELE + 1                                               08020042
      WRITE (I02,80003) IVTNUM                                          08030042
      IF (ICZERO) 46810, 6821, 46810                                    08040042
46810 IF (IVCOMP - 2401) 26810,16810,26810                              08050042
16810 IVPASS = IVPASS + 1                                               08060042
      WRITE (I02,80001) IVTNUM                                          08070042
      GO TO 6821                                                        08080042
26810 IVFAIL = IVFAIL + 1                                               08090042
      IVCORR = 2401                                                     08100042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          08110042
 6821 CONTINUE                                                          08120042
      IVTNUM = 682                                                      08130042
C                                                                       08140042
C      ****  TEST 682  ****                                             08150042
C                                                                       08160042
      IF (ICZERO) 36820, 6820, 36820                                    08170042
 6820 CONTINUE                                                          08180042
      IVON01 = -7                                                       08190042
      IVON02 = 4                                                        08200042
      IVCOMP = IVON01 ** IVON02                                         08210042
      GO TO 46820                                                       08220042
36820 IVDELE = IVDELE + 1                                               08230042
      WRITE (I02,80003) IVTNUM                                          08240042
      IF (ICZERO) 46820, 6831, 46820                                    08250042
46820 IF (IVCOMP - 2401) 26820,16820,26820                              08260042
16820 IVPASS = IVPASS + 1                                               08270042
      WRITE (I02,80001) IVTNUM                                          08280042
      GO TO 6831                                                        08290042
26820 IVFAIL = IVFAIL + 1                                               08300042
      IVCORR = 2401                                                     08310042
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          08320042
 6831 CONTINUE                                                          08330042
C                                                                       08340042
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08350042
99999 CONTINUE                                                          08360042
      WRITE (I02,90002)                                                 08370042
      WRITE (I02,90006)                                                 08380042
      WRITE (I02,90002)                                                 08390042
      WRITE (I02,90002)                                                 08400042
      WRITE (I02,90007)                                                 08410042
      WRITE (I02,90002)                                                 08420042
      WRITE (I02,90008)  IVFAIL                                         08430042
      WRITE (I02,90009) IVPASS                                          08440042
      WRITE (I02,90010) IVDELE                                          08450042
C                                                                       08460042
C                                                                       08470042
C     TERMINATE ROUTINE EXECUTION                                       08480042
      STOP                                                              08490042
C                                                                       08500042
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08510042
90000 FORMAT (1H1)                                                      08520042
90002 FORMAT (1H )                                                      08530042
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08540042
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08550042
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08560042
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08570042
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08580042
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08590042
C                                                                       08600042
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08610042
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08620042
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08630042
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08640042
C                                                                       08650042
C     FORMAT STATEMENTS FOR TEST RESULTS                                08660042
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08670042
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08680042
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08690042
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08700042
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08710042
C                                                                       08720042
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM042)                          08730042
      END                                                               08740042
