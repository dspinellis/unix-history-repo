C        COMMENT SECTION                                                00010006
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020006
C     FM006                                                             00030006
C                                                                       00040006
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF        00050006
C     THE FORM                                                          00060006
C                   INTEGER VARIABLE = INTEGER CONSTANT                 00070006
C                   INTEGER VARIABLE = INTEGER VARIABLE                 00080006
C         THE INTEGER CONSTANT MAY BE UNSIGNED, POSITIVE OR NEGATIVE.   00090006
C                                                                       00100006
C         AN INTEGER DATUM IS ALWAYS AN EXACT REPRESENTATION OF AN      00110006
C     INTEGER VALUE.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES. 00120006
C     IT MAY ONLY ASSUME INTEGRAL VALUES.                               00130006
C                                                                       00140006
C         AN INTEGER CONSTANT IS WRITTEN AS A NONEMPTY STRING OF DIGITS.00150006
C     THE CONSTANT IS THE DIGIT STRING INTERPRETED AS A DECIMAL NUMBER. 00160006
C                                                                       00170006
C         THIS ROUTINE ALSO CONTAINS TESTS WHICH CHECK ON THE USE OF    00180006
C     AT LEAST 16 BITS FOR REPRESENTING INTEGER DATA VALUES.  THE       00190006
C     CONSTANT VALUES 32767 AND -32766 ARE USED IN THESE TESTS.         00200006
C                                                                       00210006
C      REFERENCES                                                       00220006
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00230006
C              X3.9-1978                                                00240006
C                                                                       00250006
C        SECTION 4.3, INTEGER TYPE                                      00260006
C        SECTION 4.3.1, INTEGER CONSTANT                                00270006
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENTS                 00280006
C                                                                       00290006
C                                                                       00300006
C      **********************************************************       00310006
C                                                                       00320006
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00330006
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00340006
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00350006
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00360006
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00370006
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00380006
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00390006
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00400006
C     OF EXECUTING THESE TESTS.                                         00410006
C                                                                       00420006
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00430006
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00440006
C                                                                       00450006
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00460006
C                                                                       00470006
C                  DEPARTMENT OF THE NAVY                               00480006
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00490006
C                  WASHINGTON, D.C.  20376                              00500006
C                                                                       00510006
C      **********************************************************       00520006
C                                                                       00530006
C                                                                       00540006
C                                                                       00550006
C     INITIALIZATION SECTION                                            00560006
C                                                                       00570006
C     INITIALIZE CONSTANTS                                              00580006
C      **************                                                   00590006
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00600006
      I01 = 5                                                           00610006
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00620006
      I02 = 6                                                           00630006
C     SYSTEM ENVIRONMENT SECTION                                        00640006
C                                                                       00650006
      I01 = 5                                                           00660006
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670006
C     (UNIT NUMBER FOR CARD READER).                                    00680006
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00690006
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00700006
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00710006
C                                                                       00720006
      I02 = 6                                                           00730006
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00740006
C     (UNIT NUMBER FOR PRINTER).                                        00750006
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00760006
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00770006
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00780006
C                                                                       00790006
      IVPASS=0                                                          00800006
      IVFAIL=0                                                          00810006
      IVDELE=0                                                          00820006
      ICZERO=0                                                          00830006
C                                                                       00840006
C     WRITE PAGE HEADERS                                                00850006
      WRITE (I02,90000)                                                 00860006
      WRITE (I02,90001)                                                 00870006
      WRITE (I02,90002)                                                 00880006
      WRITE (I02, 90002)                                                00890006
      WRITE (I02,90003)                                                 00900006
      WRITE (I02,90002)                                                 00910006
      WRITE (I02,90004)                                                 00920006
      WRITE (I02,90002)                                                 00930006
      WRITE (I02,90011)                                                 00940006
      WRITE (I02,90002)                                                 00950006
      WRITE (I02,90002)                                                 00960006
      WRITE (I02,90005)                                                 00970006
      WRITE (I02,90006)                                                 00980006
      WRITE (I02,90002)                                                 00990006
C     TEST SECTION                                                      01000006
C                                                                       01010006
C            ARITHMETIC ASSIGNMENT STATEMENT                            01020006
C                                                                       01030006
C     TEST 50 THROUGH TEST 61 CONTAIN STATEMENT OF FORM                 01040006
C              INTEGER VARIABLE = INTEGER CONSTANT                      01050006
C                                                                       01060006
C     TESTS 50 THROUGH 53 CONTAIN UNSIGNED INTEGER CONSTANT.            01070006
C                                                                       01080006
  501 CONTINUE                                                          01090006
      IVTNUM =  50                                                      01100006
C                                                                       01110006
C      ****  TEST 50  ****                                              01120006
C                                                                       01130006
      IF (ICZERO) 30500,  500, 30500                                    01140006
  500 CONTINUE                                                          01150006
      IVCOMP=3                                                          01160006
      GO TO 40500                                                       01170006
30500 IVDELE = IVDELE + 1                                               01180006
      WRITE (I02,80003) IVTNUM                                          01190006
      IF (ICZERO) 40500,  511, 40500                                    01200006
40500 IF (IVCOMP - 3) 20500, 10500, 20500                               01210006
10500 IVPASS = IVPASS + 1                                               01220006
      WRITE (I02,80001) IVTNUM                                          01230006
      GO TO  511                                                        01240006
20500 IVFAIL = IVFAIL + 1                                               01250006
      IVCORR = 3                                                        01260006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01270006
  511 CONTINUE                                                          01280006
      IVTNUM =  51                                                      01290006
C                                                                       01300006
C      ****  TEST 51  ****                                              01310006
C                                                                       01320006
      IF (ICZERO) 30510,  510, 30510                                    01330006
  510 CONTINUE                                                          01340006
      IVCOMP = 76                                                       01350006
      GO TO 40510                                                       01360006
30510 IVDELE = IVDELE + 1                                               01370006
      WRITE (I02,80003) IVTNUM                                          01380006
      IF (ICZERO) 40510,  521, 40510                                    01390006
40510 IF (IVCOMP - 76) 20510, 10510, 20510                              01400006
10510 IVPASS = IVPASS + 1                                               01410006
      WRITE (I02,80001) IVTNUM                                          01420006
      GO TO  521                                                        01430006
20510 IVFAIL = IVFAIL + 1                                               01440006
      IVCORR = 76                                                       01450006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01460006
  521 CONTINUE                                                          01470006
      IVTNUM =  52                                                      01480006
C                                                                       01490006
C      ****  TEST 52  ****                                              01500006
C                                                                       01510006
      IF (ICZERO) 30520,  520, 30520                                    01520006
  520 CONTINUE                                                          01530006
      IVCOMP = 587                                                      01540006
      GO TO 40520                                                       01550006
30520 IVDELE = IVDELE + 1                                               01560006
      WRITE (I02,80003) IVTNUM                                          01570006
      IF (ICZERO) 40520,  531, 40520                                    01580006
40520 IF (IVCOMP - 587) 20520, 10520, 20520                             01590006
10520 IVPASS = IVPASS + 1                                               01600006
      WRITE (I02,80001) IVTNUM                                          01610006
      GO TO  531                                                        01620006
20520 IVFAIL = IVFAIL + 1                                               01630006
      IVCORR = 587                                                      01640006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01650006
  531 CONTINUE                                                          01660006
      IVTNUM =  53                                                      01670006
C                                                                       01680006
C      ****  TEST 53  ****                                              01690006
C                                                                       01700006
      IF (ICZERO) 30530,  530, 30530                                    01710006
  530 CONTINUE                                                          01720006
      IVCOMP = 9999                                                     01730006
      GO TO 40530                                                       01740006
30530 IVDELE = IVDELE + 1                                               01750006
      WRITE (I02,80003) IVTNUM                                          01760006
      IF (ICZERO) 40530,  541, 40530                                    01770006
40530 IF (IVCOMP - 9999) 20530, 10530, 20530                            01780006
10530 IVPASS = IVPASS + 1                                               01790006
      WRITE (I02,80001) IVTNUM                                          01800006
      GO TO  541                                                        01810006
20530 IVFAIL = IVFAIL + 1                                               01820006
      IVCORR = 9999                                                     01830006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01840006
C                                                                       01850006
C         TESTS 54 THROUGH 57 CONTAIN POSITIVE SIGNED INTEGERS          01860006
C                                                                       01870006
  541 CONTINUE                                                          01880006
      IVTNUM =  54                                                      01890006
C                                                                       01900006
C      ****  TEST 54  ****                                              01910006
C                                                                       01920006
      IF (ICZERO) 30540,  540, 30540                                    01930006
  540 CONTINUE                                                          01940006
      IVCOMP = +3                                                       01950006
      GO TO 40540                                                       01960006
30540 IVDELE = IVDELE + 1                                               01970006
      WRITE (I02,80003) IVTNUM                                          01980006
      IF (ICZERO) 40540,  551, 40540                                    01990006
40540 IF (IVCOMP - 3) 20540, 10540, 20540                               02000006
10540 IVPASS = IVPASS + 1                                               02010006
      WRITE (I02,80001) IVTNUM                                          02020006
      GO TO  551                                                        02030006
20540 IVFAIL = IVFAIL + 1                                               02040006
      IVCORR = 3                                                        02050006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02060006
  551 CONTINUE                                                          02070006
      IVTNUM =  55                                                      02080006
C                                                                       02090006
C      ****  TEST 55  ****                                              02100006
C                                                                       02110006
      IF (ICZERO) 30550,  550, 30550                                    02120006
  550 CONTINUE                                                          02130006
      IVCOMP = +76                                                      02140006
      GO TO 40550                                                       02150006
30550 IVDELE = IVDELE + 1                                               02160006
      WRITE (I02,80003) IVTNUM                                          02170006
      IF (ICZERO) 40550,  561, 40550                                    02180006
40550 IF (IVCOMP - 76) 20550, 10550, 20550                              02190006
10550 IVPASS = IVPASS + 1                                               02200006
      WRITE (I02,80001) IVTNUM                                          02210006
      GO TO  561                                                        02220006
20550 IVFAIL = IVFAIL + 1                                               02230006
      IVCORR = 76                                                       02240006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02250006
  561 CONTINUE                                                          02260006
      IVTNUM =  56                                                      02270006
C                                                                       02280006
C      ****  TEST 56  ****                                              02290006
C                                                                       02300006
      IF (ICZERO) 30560,  560, 30560                                    02310006
  560 CONTINUE                                                          02320006
      IVCOMP = +587                                                     02330006
      GO TO 40560                                                       02340006
30560 IVDELE = IVDELE + 1                                               02350006
      WRITE (I02,80003) IVTNUM                                          02360006
      IF (ICZERO) 40560,  571, 40560                                    02370006
40560 IF (IVCOMP - 587) 20560, 10560, 20560                             02380006
10560 IVPASS = IVPASS + 1                                               02390006
      WRITE (I02,80001) IVTNUM                                          02400006
      GO TO  571                                                        02410006
20560 IVFAIL = IVFAIL + 1                                               02420006
      IVCORR = 587                                                      02430006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02440006
  571 CONTINUE                                                          02450006
      IVTNUM =  57                                                      02460006
C                                                                       02470006
C      ****  TEST 57  ****                                              02480006
C                                                                       02490006
      IF (ICZERO) 30570,  570, 30570                                    02500006
  570 CONTINUE                                                          02510006
      IVCOMP = +9999                                                    02520006
      GO TO 40570                                                       02530006
30570 IVDELE = IVDELE + 1                                               02540006
      WRITE (I02,80003) IVTNUM                                          02550006
      IF (ICZERO) 40570,  581, 40570                                    02560006
40570 IF (IVCOMP - 9999) 20570, 10570, 20570                            02570006
10570 IVPASS = IVPASS + 1                                               02580006
      WRITE (I02,80001) IVTNUM                                          02590006
      GO TO  581                                                        02600006
20570 IVFAIL = IVFAIL + 1                                               02610006
      IVCORR = 9999                                                     02620006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02630006
C                                                                       02640006
C         TESTS 58 THROUGH 61 CONTAIN SIGNED NEGATIVE INTEGERS          02650006
C                                                                       02660006
  581 CONTINUE                                                          02670006
      IVTNUM =  58                                                      02680006
C                                                                       02690006
C      ****  TEST 58  ****                                              02700006
C                                                                       02710006
      IF (ICZERO) 30580,  580, 30580                                    02720006
  580 CONTINUE                                                          02730006
      IVCOMP = -3                                                       02740006
      GO TO 40580                                                       02750006
30580 IVDELE = IVDELE + 1                                               02760006
      WRITE (I02,80003) IVTNUM                                          02770006
      IF (ICZERO) 40580,  591, 40580                                    02780006
40580 IF (IVCOMP + 3) 20580, 10580, 20580                               02790006
10580 IVPASS = IVPASS + 1                                               02800006
      WRITE (I02,80001) IVTNUM                                          02810006
      GO TO  591                                                        02820006
20580 IVFAIL = IVFAIL + 1                                               02830006
      IVCORR = -3                                                       02840006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02850006
  591 CONTINUE                                                          02860006
      IVTNUM =  59                                                      02870006
C                                                                       02880006
C      ****  TEST 59  ****                                              02890006
C                                                                       02900006
      IF (ICZERO) 30590,  590, 30590                                    02910006
  590 CONTINUE                                                          02920006
      IVCOMP = -76                                                      02930006
      GO TO 40590                                                       02940006
30590 IVDELE = IVDELE + 1                                               02950006
      WRITE (I02,80003) IVTNUM                                          02960006
      IF (ICZERO) 40590,  601, 40590                                    02970006
40590 IF (IVCOMP + 76) 20590, 10590, 20590                              02980006
10590 IVPASS = IVPASS + 1                                               02990006
      WRITE (I02,80001) IVTNUM                                          03000006
      GO TO  601                                                        03010006
20590 IVFAIL = IVFAIL + 1                                               03020006
      IVCORR = -76                                                      03030006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03040006
  601 CONTINUE                                                          03050006
      IVTNUM =  60                                                      03060006
C                                                                       03070006
C      ****  TEST 60  ****                                              03080006
C                                                                       03090006
      IF (ICZERO) 30600,  600, 30600                                    03100006
  600 CONTINUE                                                          03110006
      IVCOMP = -587                                                     03120006
      GO TO 40600                                                       03130006
30600 IVDELE = IVDELE + 1                                               03140006
      WRITE (I02,80003) IVTNUM                                          03150006
      IF (ICZERO) 40600,  611, 40600                                    03160006
40600 IF (IVCOMP + 587) 20600,10600,20600                               03170006
10600 IVPASS = IVPASS + 1                                               03180006
      WRITE (I02,80001) IVTNUM                                          03190006
      GO TO  611                                                        03200006
20600 IVFAIL = IVFAIL + 1                                               03210006
      IVCORR = -587                                                     03220006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03230006
  611 CONTINUE                                                          03240006
      IVTNUM =  61                                                      03250006
C                                                                       03260006
C      ****  TEST 61  ****                                              03270006
C                                                                       03280006
      IF (ICZERO) 30610,  610, 30610                                    03290006
  610 CONTINUE                                                          03300006
      IVCOMP = -9999                                                    03310006
      GO TO 40610                                                       03320006
30610 IVDELE = IVDELE + 1                                               03330006
      WRITE (I02,80003) IVTNUM                                          03340006
      IF (ICZERO) 40610,  621, 40610                                    03350006
40610 IF (IVCOMP + 9999) 20610, 10610, 20610                            03360006
10610 IVPASS = IVPASS + 1                                               03370006
      WRITE (I02,80001) IVTNUM                                          03380006
      GO TO  621                                                        03390006
20610 IVFAIL = IVFAIL + 1                                               03400006
      IVCORR = -9999                                                    03410006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03420006
C                                                                       03430006
C     TEST 62 THROUGH TEST 73 CONTAIN STATEMENT OF FORM                 03440006
C         INTEGER VARIABLE = INTEGER VARIABLE                           03450006
C                                                                       03460006
C     TESTS 62 THROUGH 65 CONTAIN UNSIGNED VALUES.                      03470006
C                                                                       03480006
  621 CONTINUE                                                          03490006
      IVTNUM =  62                                                      03500006
C                                                                       03510006
C      ****  TEST 62  ****                                              03520006
C                                                                       03530006
      IF (ICZERO) 30620,  620, 30620                                    03540006
  620 CONTINUE                                                          03550006
      IVON01 = 3                                                        03560006
      IVCOMP = IVON01                                                   03570006
      GO TO 40620                                                       03580006
30620 IVDELE = IVDELE + 1                                               03590006
      WRITE (I02,80003) IVTNUM                                          03600006
      IF (ICZERO) 40620,  631, 40620                                    03610006
40620 IF (IVCOMP - 3) 20620, 10620, 20620                               03620006
10620 IVPASS = IVPASS + 1                                               03630006
      WRITE (I02,80001) IVTNUM                                          03640006
      GO TO  631                                                        03650006
20620 IVFAIL = IVFAIL + 1                                               03660006
      IVCORR = 3                                                        03670006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03680006
  631 CONTINUE                                                          03690006
      IVTNUM =  63                                                      03700006
C                                                                       03710006
C      ****  TEST 63  ****                                              03720006
C                                                                       03730006
      IF (ICZERO) 30630,  630, 30630                                    03740006
  630 CONTINUE                                                          03750006
      IVON01 = 76                                                       03760006
      IVCOMP = IVON01                                                   03770006
      GO TO 40630                                                       03780006
30630 IVDELE = IVDELE + 1                                               03790006
      WRITE (I02,80003) IVTNUM                                          03800006
      IF (ICZERO) 40630,  641, 40630                                    03810006
40630 IF (IVCOMP - 76) 20630, 10630, 20630                              03820006
10630 IVPASS = IVPASS + 1                                               03830006
      WRITE (I02,80001) IVTNUM                                          03840006
      GO TO  641                                                        03850006
20630 IVFAIL = IVFAIL + 1                                               03860006
      IVCORR = 76                                                       03870006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03880006
  641 CONTINUE                                                          03890006
      IVTNUM =  64                                                      03900006
C                                                                       03910006
C      ****  TEST 64  ****                                              03920006
C                                                                       03930006
      IF (ICZERO) 30640,  640, 30640                                    03940006
  640 CONTINUE                                                          03950006
      IVON01 = 587                                                      03960006
      IVCOMP = IVON01                                                   03970006
      GO TO 40640                                                       03980006
30640 IVDELE = IVDELE + 1                                               03990006
      WRITE (I02,80003) IVTNUM                                          04000006
      IF (ICZERO) 40640,  651, 40640                                    04010006
40640 IF (IVCOMP - 587) 20640, 10640, 20640                             04020006
10640 IVPASS = IVPASS + 1                                               04030006
      WRITE (I02,80001) IVTNUM                                          04040006
      GO TO  651                                                        04050006
20640 IVFAIL = IVFAIL + 1                                               04060006
      IVCORR = 587                                                      04070006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04080006
  651 CONTINUE                                                          04090006
      IVTNUM =  65                                                      04100006
C                                                                       04110006
C      ****  TEST 65  ****                                              04120006
C                                                                       04130006
      IF (ICZERO) 30650,  650, 30650                                    04140006
  650 CONTINUE                                                          04150006
      IVON01 = 9999                                                     04160006
      IVCOMP = IVON01                                                   04170006
      GO TO 40650                                                       04180006
30650 IVDELE = IVDELE + 1                                               04190006
      WRITE (I02,80003) IVTNUM                                          04200006
      IF (ICZERO) 40650,  661, 40650                                    04210006
40650 IF (IVCOMP - 9999)  20650, 10650, 20650                           04220006
10650 IVPASS = IVPASS + 1                                               04230006
      WRITE (I02,80001) IVTNUM                                          04240006
      GO TO  661                                                        04250006
20650 IVFAIL = IVFAIL + 1                                               04260006
      IVCORR = 9999                                                     04270006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04280006
C                                                                       04290006
C     TESTS 66 THROUGH 69 CONTAIN POSITIVE VALUES.                      04300006
C                                                                       04310006
  661 CONTINUE                                                          04320006
      IVTNUM =  66                                                      04330006
C                                                                       04340006
C      ****  TEST 66  ****                                              04350006
C                                                                       04360006
      IF (ICZERO) 30660,  660, 30660                                    04370006
  660 CONTINUE                                                          04380006
      IVON01 = +3                                                       04390006
      IVCOMP = IVON01                                                   04400006
      GO TO 40660                                                       04410006
30660 IVDELE = IVDELE + 1                                               04420006
      WRITE (I02,80003) IVTNUM                                          04430006
      IF (ICZERO) 40660,  671, 40660                                    04440006
40660 IF (IVCOMP - 3) 20660,10660,20660                                 04450006
10660 IVPASS = IVPASS + 1                                               04460006
      WRITE (I02,80001) IVTNUM                                          04470006
      GO TO  671                                                        04480006
20660 IVFAIL = IVFAIL + 1                                               04490006
      IVCORR = 3                                                        04500006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04510006
  671 CONTINUE                                                          04520006
      IVTNUM =  67                                                      04530006
C                                                                       04540006
C      ****  TEST 67  ****                                              04550006
C                                                                       04560006
      IF (ICZERO) 30670,  670, 30670                                    04570006
  670 CONTINUE                                                          04580006
      IVON01 = +76                                                      04590006
      IVCOMP = IVON01                                                   04600006
      GO TO 40670                                                       04610006
30670 IVDELE = IVDELE + 1                                               04620006
      WRITE (I02,80003) IVTNUM                                          04630006
      IF (ICZERO) 40670,  681, 40670                                    04640006
40670 IF (IVCOMP - 76) 20670, 10670, 20670                              04650006
10670 IVPASS = IVPASS + 1                                               04660006
      WRITE (I02,80001) IVTNUM                                          04670006
      GO TO  681                                                        04680006
20670 IVFAIL = IVFAIL + 1                                               04690006
      IVCORR = 76                                                       04700006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04710006
  681 CONTINUE                                                          04720006
      IVTNUM =  68                                                      04730006
C                                                                       04740006
C      ****  TEST 68  ****                                              04750006
C                                                                       04760006
      IF (ICZERO) 30680,  680, 30680                                    04770006
  680 CONTINUE                                                          04780006
      IVON01 = +587                                                     04790006
      IVCOMP = IVON01                                                   04800006
      GO TO 40680                                                       04810006
30680 IVDELE = IVDELE + 1                                               04820006
      WRITE (I02,80003) IVTNUM                                          04830006
      IF (ICZERO) 40680,  691, 40680                                    04840006
40680 IF (IVCOMP - 587) 20680, 10680, 20680                             04850006
10680 IVPASS = IVPASS + 1                                               04860006
      WRITE (I02,80001) IVTNUM                                          04870006
      GO TO  691                                                        04880006
20680 IVFAIL = IVFAIL + 1                                               04890006
      IVCORR = 587                                                      04900006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04910006
  691 CONTINUE                                                          04920006
      IVTNUM =  69                                                      04930006
C                                                                       04940006
C      ****  TEST 69  ****                                              04950006
C                                                                       04960006
      IF (ICZERO) 30690,  690, 30690                                    04970006
  690 CONTINUE                                                          04980006
      IVON01 = +9999                                                    04990006
      IVCOMP = IVON01                                                   05000006
      GO TO 40690                                                       05010006
30690 IVDELE = IVDELE + 1                                               05020006
      WRITE (I02,80003) IVTNUM                                          05030006
      IF (ICZERO) 40690,  701, 40690                                    05040006
40690 IF (IVCOMP - 9999) 20690, 10690, 20690                            05050006
10690 IVPASS = IVPASS + 1                                               05060006
      WRITE (I02,80001) IVTNUM                                          05070006
      GO TO  701                                                        05080006
20690 IVFAIL = IVFAIL + 1                                               05090006
      IVCORR = 9999                                                     05100006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05110006
C                                                                       05120006
C     TESTS 70 THROUGH 73 CONTAIN NEGATIVE VALUES.                      05130006
C                                                                       05140006
  701 CONTINUE                                                          05150006
      IVTNUM =  70                                                      05160006
C                                                                       05170006
C      ****  TEST 70  ****                                              05180006
C                                                                       05190006
      IF (ICZERO) 30700,  700, 30700                                    05200006
  700 CONTINUE                                                          05210006
      IVON01 = -3                                                       05220006
      IVCOMP = IVON01                                                   05230006
      GO TO 40700                                                       05240006
30700 IVDELE = IVDELE + 1                                               05250006
      WRITE (I02,80003) IVTNUM                                          05260006
      IF (ICZERO) 40700,  711, 40700                                    05270006
40700 IF (IVCOMP + 3) 20700, 10700, 20700                               05280006
10700 IVPASS = IVPASS + 1                                               05290006
      WRITE (I02,80001) IVTNUM                                          05300006
      GO TO  711                                                        05310006
20700 IVFAIL = IVFAIL + 1                                               05320006
      IVCORR = -3                                                       05330006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05340006
  711 CONTINUE                                                          05350006
      IVTNUM =  71                                                      05360006
C                                                                       05370006
C      ****  TEST 71  ****                                              05380006
C                                                                       05390006
      IF (ICZERO) 30710,  710, 30710                                    05400006
  710 CONTINUE                                                          05410006
      IVON01 = -76                                                      05420006
      IVCOMP = IVON01                                                   05430006
      GO TO 40710                                                       05440006
30710 IVDELE = IVDELE + 1                                               05450006
      WRITE (I02,80003) IVTNUM                                          05460006
      IF (ICZERO) 40710,  721, 40710                                    05470006
40710 IF (IVCOMP + 76) 20710, 10710, 20710                              05480006
10710 IVPASS = IVPASS + 1                                               05490006
      WRITE (I02,80001) IVTNUM                                          05500006
      GO TO  721                                                        05510006
20710 IVFAIL = IVFAIL + 1                                               05520006
      IVCORR = -76                                                      05530006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05540006
  721 CONTINUE                                                          05550006
      IVTNUM =  72                                                      05560006
C                                                                       05570006
C      ****  TEST 72  ****                                              05580006
C                                                                       05590006
      IF (ICZERO) 30720,  720, 30720                                    05600006
  720 CONTINUE                                                          05610006
      IVON01 = -587                                                     05620006
      IVCOMP = IVON01                                                   05630006
      GO TO 40720                                                       05640006
30720 IVDELE = IVDELE + 1                                               05650006
      WRITE (I02,80003) IVTNUM                                          05660006
      IF (ICZERO) 40720,  731, 40720                                    05670006
40720 IF (IVCOMP + 587) 20720, 10720, 20720                             05680006
10720 IVPASS = IVPASS + 1                                               05690006
      WRITE (I02,80001) IVTNUM                                          05700006
      GO TO  731                                                        05710006
20720 IVFAIL = IVFAIL + 1                                               05720006
      IVCORR = -587                                                     05730006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05740006
  731 CONTINUE                                                          05750006
      IVTNUM =  73                                                      05760006
C                                                                       05770006
C      ****  TEST 73  ****                                              05780006
C                                                                       05790006
      IF (ICZERO) 30730,  730, 30730                                    05800006
  730 CONTINUE                                                          05810006
      IVON01 = -9999                                                    05820006
      IVCOMP = IVON01                                                   05830006
      GO TO 40730                                                       05840006
30730 IVDELE = IVDELE + 1                                               05850006
      WRITE (I02,80003) IVTNUM                                          05860006
      IF (ICZERO) 40730,  741, 40730                                    05870006
40730 IF (IVCOMP + 9999) 20730, 10730, 20730                            05880006
10730 IVPASS = IVPASS + 1                                               05890006
      WRITE (I02,80001) IVTNUM                                          05900006
      GO TO  741                                                        05910006
20730 IVFAIL = IVFAIL + 1                                               05920006
      IVCORR = -9999                                                    05930006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05940006
C                                                                       05950006
C     TESTS 74 THROUGH 79 CHECK THAT AT LEAST 16 BITS ARE USED IN THE   05960006
C     INTERNAL REPRESENTATION OF AN INTEGER DATUM.  THIS INCLUDES ONE   05970006
C     BIT FOR THE SIGN.  THE LARGEST INTEGER USED IS 32767 =2**15 - 1,  05980006
C     AND THE SMALLEST INTEGER USED IS -32766.                          05990006
C                                                                       06000006
  741 CONTINUE                                                          06010006
      IVTNUM =  74                                                      06020006
C                                                                       06030006
C      ****  TEST 74  ****                                              06040006
C             UNSIGNED CONSTANT 32767                                   06050006
C                                                                       06060006
      IF (ICZERO) 30740,  740, 30740                                    06070006
  740 CONTINUE                                                          06080006
      IVCOMP = 32767                                                    06090006
      GO TO 40740                                                       06100006
30740 IVDELE = IVDELE + 1                                               06110006
      WRITE (I02,80003) IVTNUM                                          06120006
      IF (ICZERO) 40740,  751, 40740                                    06130006
40740 IF (IVCOMP - 32767) 20740, 10740, 20740                           06140006
10740 IVPASS = IVPASS + 1                                               06150006
      WRITE (I02,80001) IVTNUM                                          06160006
      GO TO  751                                                        06170006
20740 IVFAIL = IVFAIL + 1                                               06180006
      IVCORR = 32767                                                    06190006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06200006
  751 CONTINUE                                                          06210006
      IVTNUM =  75                                                      06220006
C                                                                       06230006
C      ****  TEST 75  ****                                              06240006
C             SIGNED POSITIVE CONSTANT +32767                           06250006
C                                                                       06260006
      IF (ICZERO) 30750,  750, 30750                                    06270006
  750 CONTINUE                                                          06280006
      IVCOMP = +32767                                                   06290006
      GO TO 40750                                                       06300006
30750 IVDELE = IVDELE + 1                                               06310006
      WRITE (I02,80003) IVTNUM                                          06320006
      IF (ICZERO) 40750,  761, 40750                                    06330006
40750 IF (IVCOMP - 32767) 20750, 10750, 20750                           06340006
10750 IVPASS = IVPASS + 1                                               06350006
      WRITE (I02,80001) IVTNUM                                          06360006
      GO TO  761                                                        06370006
20750 IVFAIL = IVFAIL + 1                                               06380006
      IVCORR = 32767                                                    06390006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06400006
  761 CONTINUE                                                          06410006
      IVTNUM =  76                                                      06420006
C                                                                       06430006
C      ****  TEST 76  ****                                              06440006
C             SIGNED NEGATIVE CONSTANT -32766                           06450006
C                                                                       06460006
      IF (ICZERO) 30760,  760, 30760                                    06470006
  760 CONTINUE                                                          06480006
      IVCOMP = - 32766                                                  06490006
      GO TO 40760                                                       06500006
30760 IVDELE = IVDELE + 1                                               06510006
      WRITE (I02,80003) IVTNUM                                          06520006
      IF (ICZERO) 40760,  771, 40760                                    06530006
40760 IF (IVCOMP + 32766) 20760, 10760, 20760                           06540006
10760 IVPASS = IVPASS + 1                                               06550006
      WRITE (I02,80001) IVTNUM                                          06560006
      GO TO  771                                                        06570006
20760 IVFAIL = IVFAIL + 1                                               06580006
      IVCORR = -32766                                                   06590006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06600006
  771 CONTINUE                                                          06610006
      IVTNUM =  77                                                      06620006
C                                                                       06630006
C      ****  TEST 77  ****                                              06640006
C                                                                       06650006
      IF (ICZERO) 30770,  770, 30770                                    06660006
  770 CONTINUE                                                          06670006
      IVON01 = 32767                                                    06680006
      IVCOMP = IVON01                                                   06690006
      GO TO 40770                                                       06700006
30770 IVDELE = IVDELE + 1                                               06710006
      WRITE (I02,80003) IVTNUM                                          06720006
      IF (ICZERO) 40770,  781, 40770                                    06730006
40770 IF (IVCOMP - 32767) 20770, 10770, 20770                           06740006
10770 IVPASS = IVPASS + 1                                               06750006
      WRITE (I02,80001) IVTNUM                                          06760006
      GO TO  781                                                        06770006
20770 IVFAIL = IVFAIL + 1                                               06780006
      IVCORR = 32767                                                    06790006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06800006
  781 CONTINUE                                                          06810006
      IVTNUM =  78                                                      06820006
C                                                                       06830006
C      ****  TEST 78  ****                                              06840006
C                                                                       06850006
      IF (ICZERO) 30780,  780, 30780                                    06860006
  780 CONTINUE                                                          06870006
      IVON01 = +32767                                                   06880006
      IVCOMP = IVON01                                                   06890006
      GO TO 40780                                                       06900006
30780 IVDELE = IVDELE + 1                                               06910006
      WRITE (I02,80003) IVTNUM                                          06920006
      IF (ICZERO) 40780,  791, 40780                                    06930006
40780 IF (IVCOMP - 32767) 20780, 10780, 20780                           06940006
10780 IVPASS = IVPASS + 1                                               06950006
      WRITE (I02,80001) IVTNUM                                          06960006
      GO TO  791                                                        06970006
20780 IVFAIL = IVFAIL + 1                                               06980006
      IVCORR = 32767                                                    06990006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07000006
  791 CONTINUE                                                          07010006
      IVTNUM =  79                                                      07020006
C                                                                       07030006
C      ****  TEST 79  ****                                              07040006
C                                                                       07050006
      IF (ICZERO) 30790,  790, 30790                                    07060006
  790 CONTINUE                                                          07070006
      IVON01 = -32766                                                   07080006
      IVCOMP=IVON01                                                     07090006
      GO TO 40790                                                       07100006
30790 IVDELE = IVDELE + 1                                               07110006
      WRITE (I02,80003) IVTNUM                                          07120006
      IF (ICZERO) 40790,  801, 40790                                    07130006
40790 IF (IVCOMP + 32766) 20790, 10790, 20790                           07140006
10790 IVPASS = IVPASS + 1                                               07150006
      WRITE (I02,80001) IVTNUM                                          07160006
      GO TO  801                                                        07170006
20790 IVFAIL = IVFAIL + 1                                               07180006
      IVCORR = -32766                                                   07190006
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07200006
  801 CONTINUE                                                          07210006
C                                                                       07220006
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07230006
99999 CONTINUE                                                          07240006
      WRITE (I02,90002)                                                 07250006
      WRITE (I02,90006)                                                 07260006
      WRITE (I02,90002)                                                 07270006
      WRITE (I02,90002)                                                 07280006
      WRITE (I02,90007)                                                 07290006
      WRITE (I02,90002)                                                 07300006
      WRITE (I02,90008)  IVFAIL                                         07310006
      WRITE (I02,90009) IVPASS                                          07320006
      WRITE (I02,90010) IVDELE                                          07330006
C                                                                       07340006
C                                                                       07350006
C     TERMINATE ROUTINE EXECUTION                                       07360006
      STOP                                                              07370006
C                                                                       07380006
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07390006
90000 FORMAT (1H1)                                                      07400006
90002 FORMAT (1H )                                                      07410006
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07420006
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07430006
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07440006
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07450006
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07460006
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07470006
C                                                                       07480006
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07490006
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07500006
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07510006
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07520006
C                                                                       07530006
C     FORMAT STATEMENTS FOR TEST RESULTS                                07540006
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07550006
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07560006
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07570006
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07580006
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07590006
C                                                                       07600006
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM006)                          07610006
      END                                                               07620006
