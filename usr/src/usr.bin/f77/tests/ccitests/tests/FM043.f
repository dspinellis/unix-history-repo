C     COMMENT SECTION                                                   00010043
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020043
C     FM043                                                             00030043
C                                                                       00040043
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE FORM         00050043
C                                                                       00060043
C     INTEGER VAR. = INTEGER VAR. <OP1> INTEGER VAR. <OP2> INTEGER VAR. 00070043
C                                                                       00080043
C     WHERE <OP1> AND <OP2> ARE ARITHMETIC OPERATORS, BUT <OP1> IS      00090043
C     NOT THE SAME AS <OP2>.                                            00100043
C                                                                       00110043
C      REFERENCES                                                       00120043
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00130043
C              X3.9-1978                                                00140043
C                                                                       00150043
C        SECTION 4.3, INTEGER TYPE                                      00160043
C        SECTION 4.3.1, INTEGER CONSTANT                                00170043
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00180043
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00190043
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00200043
C                                                                       00210043
C                                                                       00220043
C      **********************************************************       00230043
C                                                                       00240043
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00250043
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00260043
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00270043
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00280043
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00290043
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00300043
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00310043
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00320043
C     OF EXECUTING THESE TESTS.                                         00330043
C                                                                       00340043
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00350043
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00360043
C                                                                       00370043
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00380043
C                                                                       00390043
C                  DEPARTMENT OF THE NAVY                               00400043
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00410043
C                  WASHINGTON, D.C.  20376                              00420043
C                                                                       00430043
C      **********************************************************       00440043
C                                                                       00450043
C                                                                       00460043
C                                                                       00470043
C     INITIALIZATION SECTION                                            00480043
C                                                                       00490043
C     INITIALIZE CONSTANTS                                              00500043
C      **************                                                   00510043
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00520043
      I01 = 5                                                           00530043
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00540043
      I02 = 6                                                           00550043
C     SYSTEM ENVIRONMENT SECTION                                        00560043
C                                                                       00570043
      I01 = 5                                                           00580043
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00590043
C     (UNIT NUMBER FOR CARD READER).                                    00600043
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00610043
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00620043
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00630043
C                                                                       00640043
      I02 = 6                                                           00650043
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00660043
C     (UNIT NUMBER FOR PRINTER).                                        00670043
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00680043
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00690043
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00700043
C                                                                       00710043
      IVPASS=0                                                          00720043
      IVFAIL=0                                                          00730043
      IVDELE=0                                                          00740043
      ICZERO=0                                                          00750043
C                                                                       00760043
C     WRITE PAGE HEADERS                                                00770043
      WRITE (I02,90000)                                                 00780043
      WRITE (I02,90001)                                                 00790043
      WRITE (I02,90002)                                                 00800043
      WRITE (I02, 90002)                                                00810043
      WRITE (I02,90003)                                                 00820043
      WRITE (I02,90002)                                                 00830043
      WRITE (I02,90004)                                                 00840043
      WRITE (I02,90002)                                                 00850043
      WRITE (I02,90011)                                                 00860043
      WRITE (I02,90002)                                                 00870043
      WRITE (I02,90002)                                                 00880043
      WRITE (I02,90005)                                                 00890043
      WRITE (I02,90006)                                                 00900043
      WRITE (I02,90002)                                                 00910043
C                                                                       00920043
C     TEST SECTION                                                      00930043
C                                                                       00940043
C         ARITHMETIC ASSIGNMENT STATEMENT                               00950043
C                                                                       00960043
C     TESTS 683 THROUGH 694 TEST STATEMENTS WHERE <OP1> IS '+' AND      00970043
C     <OP2> VARIES.                                                     00980043
C                                                                       00990043
C     TEST 695 THROUGH 706 TEST STATEMENTS WHERE <OP1> IS '-' AND       01000043
C     <OP2> VARIES.                                                     01010043
C                                                                       01020043
C     TESTS 707 THROUGH 718 TEST STATEMENTS WHERE <OP1> IS '*' AND      01030043
C     <OP2> VARIES.                                                     01040043
C                                                                       01050043
C                                                                       01060043
C                                                                       01070043
C     TESTS 683 THROUGH  685 TEST '+' FOLLOWED BY '-'.                  01080043
C                                                                       01090043
      IVTNUM = 683                                                      01100043
C                                                                       01110043
C      ****  TEST 683  ****                                             01120043
C                                                                       01130043
      IF (ICZERO) 36830, 6830, 36830                                    01140043
 6830 CONTINUE                                                          01150043
      IVON01 = 45                                                       01160043
      IVON02 =  9                                                       01170043
      IVON03 =  3                                                       01180043
      IVCOMP = IVON01 + IVON02 - IVON03                                 01190043
      GO TO 46830                                                       01200043
36830 IVDELE = IVDELE + 1                                               01210043
      WRITE (I02,80003) IVTNUM                                          01220043
      IF (ICZERO) 46830, 6841, 46830                                    01230043
46830 IF (IVCOMP - 51) 26830,16830,26830                                01240043
16830 IVPASS = IVPASS + 1                                               01250043
      WRITE (I02,80001) IVTNUM                                          01260043
      GO TO 6841                                                        01270043
26830 IVFAIL = IVFAIL + 1                                               01280043
      IVCORR = 51                                                       01290043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01300043
 6841 CONTINUE                                                          01310043
      IVTNUM = 684                                                      01320043
C                                                                       01330043
C      ****  TEST 684  ****                                             01340043
C                                                                       01350043
      IF (ICZERO) 36840, 6840, 36840                                    01360043
 6840 CONTINUE                                                          01370043
      IVON01 = 45                                                       01380043
      IVON02 =  9                                                       01390043
      IVON03 =  3                                                       01400043
      IVCOMP = (IVON01 + IVON02) - IVON03                               01410043
      GO TO 46840                                                       01420043
36840 IVDELE = IVDELE + 1                                               01430043
      WRITE (I02,80003) IVTNUM                                          01440043
      IF (ICZERO) 46840, 6851, 46840                                    01450043
46840 IF (IVCOMP - 51) 26840,16840,26840                                01460043
16840 IVPASS = IVPASS + 1                                               01470043
      WRITE (I02,80001) IVTNUM                                          01480043
      GO TO 6851                                                        01490043
26840 IVFAIL = IVFAIL + 1                                               01500043
      IVCORR = 51                                                       01510043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01520043
 6851 CONTINUE                                                          01530043
      IVTNUM = 685                                                      01540043
C                                                                       01550043
C      ****  TEST 685  ****                                             01560043
C                                                                       01570043
      IF (ICZERO) 36850, 6850, 36850                                    01580043
 6850 CONTINUE                                                          01590043
      IVON01 = 45                                                       01600043
      IVON02 = 9                                                        01610043
      IVON03 = 3                                                        01620043
      IVCOMP = IVON01 + (IVON02 - IVON03)                               01630043
      GO TO 46850                                                       01640043
36850 IVDELE = IVDELE + 1                                               01650043
      WRITE (I02,80003) IVTNUM                                          01660043
      IF (ICZERO) 46850, 6861, 46850                                    01670043
46850 IF (IVCOMP - 51) 26850,16850,26850                                01680043
16850 IVPASS = IVPASS + 1                                               01690043
      WRITE (I02,80001) IVTNUM                                          01700043
      GO TO 6861                                                        01710043
26850 IVFAIL = IVFAIL + 1                                               01720043
      IVCORR = 51                                                       01730043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01740043
 6861 CONTINUE                                                          01750043
C                                                                       01760043
C     TESTS 686 THROUGH 688 TEST '+' FOLLOWED BY '*'.                   01770043
C                                                                       01780043
      IVTNUM = 686                                                      01790043
C                                                                       01800043
C      ****  TEST 686  ****                                             01810043
C                                                                       01820043
      IF (ICZERO) 36860, 6860, 36860                                    01830043
 6860 CONTINUE                                                          01840043
      IVON01 = 45                                                       01850043
      IVON02 =  9                                                       01860043
      IVON03 =  3                                                       01870043
      IVCOMP =  IVON01 + IVON02 * IVON03                                01880043
      GO TO 46860                                                       01890043
36860 IVDELE = IVDELE + 1                                               01900043
      WRITE (I02,80003) IVTNUM                                          01910043
      IF (ICZERO) 46860, 6871, 46860                                    01920043
46860 IF (IVCOMP - 72) 26860,16860,26860                                01930043
16860 IVPASS = IVPASS + 1                                               01940043
      WRITE (I02,80001) IVTNUM                                          01950043
      GO TO 6871                                                        01960043
26860 IVFAIL = IVFAIL + 1                                               01970043
      IVCORR = 72                                                       01980043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01990043
 6871 CONTINUE                                                          02000043
      IVTNUM = 687                                                      02010043
C                                                                       02020043
C      ****  TEST 687  ****                                             02030043
C                                                                       02040043
      IF (ICZERO) 36870, 6870, 36870                                    02050043
 6870 CONTINUE                                                          02060043
      IVON01 = 45                                                       02070043
      IVON02 =  9                                                       02080043
      IVON03 =  3                                                       02090043
      IVCOMP = (IVON01 + IVON02) * IVON03                               02100043
      GO TO 46870                                                       02110043
36870 IVDELE = IVDELE + 1                                               02120043
      WRITE (I02,80003) IVTNUM                                          02130043
      IF (ICZERO) 46870, 6881, 46870                                    02140043
46870 IF (IVCOMP - 162) 26870,16870,26870                               02150043
16870 IVPASS = IVPASS + 1                                               02160043
      WRITE (I02,80001) IVTNUM                                          02170043
      GO TO 6881                                                        02180043
26870 IVFAIL = IVFAIL + 1                                               02190043
      IVCORR = 162                                                      02200043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02210043
 6881 CONTINUE                                                          02220043
      IVTNUM = 688                                                      02230043
C                                                                       02240043
C      ****  TEST 688  ****                                             02250043
C                                                                       02260043
      IF (ICZERO) 36880, 6880, 36880                                    02270043
 6880 CONTINUE                                                          02280043
      IVON01 = 45                                                       02290043
      IVON02 =  9                                                       02300043
      IVON03 = 3                                                        02310043
      IVCOMP = IVON01 + (IVON02 * IVON03)                               02320043
      GO TO 46880                                                       02330043
36880 IVDELE = IVDELE + 1                                               02340043
      WRITE (I02,80003) IVTNUM                                          02350043
      IF (ICZERO) 46880, 6891, 46880                                    02360043
46880 IF (IVCOMP - 72) 26880,16880,26880                                02370043
16880 IVPASS = IVPASS + 1                                               02380043
      WRITE (I02,80001) IVTNUM                                          02390043
      GO TO 6891                                                        02400043
26880 IVFAIL = IVFAIL + 1                                               02410043
      IVCORR = 72                                                       02420043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02430043
 6891 CONTINUE                                                          02440043
C                                                                       02450043
C     TESTS 689 THROUGH 691 TEST '+' FOLLOWED BY '/'.                   02460043
C                                                                       02470043
      IVTNUM = 689                                                      02480043
C                                                                       02490043
C      ****  TEST 689  ****                                             02500043
C                                                                       02510043
      IF (ICZERO) 36890, 6890, 36890                                    02520043
 6890 CONTINUE                                                          02530043
      IVON01 = 45                                                       02540043
      IVON02 =  9                                                       02550043
      IVON03 = 3                                                        02560043
      IVCOMP = IVON01 + IVON02 / IVON03                                 02570043
      GO TO 46890                                                       02580043
36890 IVDELE = IVDELE + 1                                               02590043
      WRITE (I02,80003) IVTNUM                                          02600043
      IF (ICZERO) 46890, 6901, 46890                                    02610043
46890 IF (IVCOMP - 48) 26890,16890,26890                                02620043
16890 IVPASS = IVPASS + 1                                               02630043
      WRITE (I02,80001) IVTNUM                                          02640043
      GO TO 6901                                                        02650043
26890 IVFAIL = IVFAIL + 1                                               02660043
      IVCORR = 48                                                       02670043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02680043
 6901 CONTINUE                                                          02690043
      IVTNUM = 690                                                      02700043
C                                                                       02710043
C      ****  TEST 690  ****                                             02720043
C                                                                       02730043
      IF (ICZERO) 36900, 6900, 36900                                    02740043
 6900 CONTINUE                                                          02750043
      IVON01 = 45                                                       02760043
      IVON02 =  9                                                       02770043
      IVON03 =  3                                                       02780043
      IVCOMP = (IVON01 + IVON02) / IVON03                               02790043
      GO TO 46900                                                       02800043
36900 IVDELE = IVDELE + 1                                               02810043
      WRITE (I02,80003) IVTNUM                                          02820043
      IF (ICZERO) 46900, 6911, 46900                                    02830043
46900 IF (IVCOMP - 18) 26900,16900,26900                                02840043
16900 IVPASS = IVPASS + 1                                               02850043
      WRITE (I02,80001) IVTNUM                                          02860043
      GO TO 6911                                                        02870043
26900 IVFAIL = IVFAIL + 1                                               02880043
      IVCORR = 18                                                       02890043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02900043
 6911 CONTINUE                                                          02910043
      IVTNUM = 691                                                      02920043
C                                                                       02930043
C      ****  TEST 691  ****                                             02940043
C                                                                       02950043
      IF (ICZERO) 36910, 6910, 36910                                    02960043
 6910 CONTINUE                                                          02970043
      IVON01 = 45                                                       02980043
      IVON02 =  9                                                       02990043
      IVON03 =  3                                                       03000043
      IVCOMP = IVON01 + (IVON02 / IVON03)                               03010043
      GO TO 46910                                                       03020043
36910 IVDELE = IVDELE + 1                                               03030043
      WRITE (I02,80003) IVTNUM                                          03040043
      IF (ICZERO) 46910, 6921, 46910                                    03050043
46910 IF (IVCOMP - 48) 26910,16910,26910                                03060043
16910 IVPASS = IVPASS + 1                                               03070043
      WRITE (I02,80001) IVTNUM                                          03080043
      GO TO 6921                                                        03090043
26910 IVFAIL = IVFAIL + 1                                               03100043
      IVCORR = 48                                                       03110043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03120043
 6921 CONTINUE                                                          03130043
C                                                                       03140043
C     TESTS 692 THROUGH 694 TEST '+' FOLLOWED BY '**'.                  03150043
C                                                                       03160043
      IVTNUM = 692                                                      03170043
C                                                                       03180043
C      ****  TEST 692  ****                                             03190043
C                                                                       03200043
      IF (ICZERO) 36920, 6920, 36920                                    03210043
 6920 CONTINUE                                                          03220043
      IVON01 = 15                                                       03230043
      IVON02 =  9                                                       03240043
      IVON03 =  3                                                       03250043
      IVCOMP = IVON01 + IVON02 ** IVON03                                03260043
      GO TO 46920                                                       03270043
36920 IVDELE = IVDELE + 1                                               03280043
      WRITE (I02,80003) IVTNUM                                          03290043
      IF (ICZERO) 46920, 6931, 46920                                    03300043
46920 IF (IVCOMP - 744) 26920,16920,26920                               03310043
16920 IVPASS = IVPASS + 1                                               03320043
      WRITE (I02,80001) IVTNUM                                          03330043
      GO TO 6931                                                        03340043
26920 IVFAIL = IVFAIL + 1                                               03350043
      IVCORR = 744                                                      03360043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03370043
 6931 CONTINUE                                                          03380043
      IVTNUM = 693                                                      03390043
C                                                                       03400043
C      ****  TEST 693  ****                                             03410043
C                                                                       03420043
      IF (ICZERO) 36930, 6930, 36930                                    03430043
 6930 CONTINUE                                                          03440043
      IVON01 = 15                                                       03450043
      IVON02 =  9                                                       03460043
      IVON03 =  3                                                       03470043
      IVCOMP = (IVON01 + IVON02) ** IVON03                              03480043
      GO TO 46930                                                       03490043
36930 IVDELE = IVDELE + 1                                               03500043
      WRITE (I02,80003) IVTNUM                                          03510043
      IF (ICZERO) 46930, 6941, 46930                                    03520043
46930 IF (IVCOMP - 13824) 26930,16930,26930                             03530043
16930 IVPASS = IVPASS + 1                                               03540043
      WRITE (I02,80001) IVTNUM                                          03550043
      GO TO 6941                                                        03560043
26930 IVFAIL = IVFAIL + 1                                               03570043
      IVCORR = 13824                                                    03580043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03590043
 6941 CONTINUE                                                          03600043
      IVTNUM = 694                                                      03610043
C                                                                       03620043
C      ****  TEST 694  ****                                             03630043
C                                                                       03640043
      IF (ICZERO) 36940, 6940, 36940                                    03650043
 6940 CONTINUE                                                          03660043
      IVON01 = 15                                                       03670043
      IVON02 =  9                                                       03680043
      IVON03 =  3                                                       03690043
      IVCOMP = IVON01 + (IVON02 ** IVON03)                              03700043
      GO TO 46940                                                       03710043
36940 IVDELE = IVDELE + 1                                               03720043
      WRITE (I02,80003) IVTNUM                                          03730043
      IF (ICZERO) 46940, 6951, 46940                                    03740043
46940 IF (IVCOMP - 744) 26940,16940,26940                               03750043
16940 IVPASS = IVPASS + 1                                               03760043
      WRITE (I02,80001) IVTNUM                                          03770043
      GO TO 6951                                                        03780043
26940 IVFAIL = IVFAIL + 1                                               03790043
      IVCORR = 744                                                      03800043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03810043
 6951 CONTINUE                                                          03820043
C                                                                       03830043
C     TESTS 695 THROUGH 697 TEST '-' FOLLOWED BY '+'.                   03840043
C                                                                       03850043
      IVTNUM = 695                                                      03860043
C                                                                       03870043
C      ****  TEST 695  ****                                             03880043
C                                                                       03890043
      IF (ICZERO) 36950, 6950, 36950                                    03900043
 6950 CONTINUE                                                          03910043
      IVON01 =  45                                                      03920043
      IVON02 =   9                                                      03930043
      IVON03 =   3                                                      03940043
      IVCOMP = IVON01 - IVON02 + IVON03                                 03950043
      GO TO 46950                                                       03960043
36950 IVDELE = IVDELE + 1                                               03970043
      WRITE (I02,80003) IVTNUM                                          03980043
      IF (ICZERO) 46950, 6961, 46950                                    03990043
46950 IF (IVCOMP - 39) 26950,16950,26950                                04000043
16950 IVPASS = IVPASS + 1                                               04010043
      WRITE (I02,80001) IVTNUM                                          04020043
      GO TO 6961                                                        04030043
26950 IVFAIL = IVFAIL + 1                                               04040043
      IVCORR = 39                                                       04050043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04060043
 6961 CONTINUE                                                          04070043
      IVTNUM = 696                                                      04080043
C                                                                       04090043
C      ****  TEST 696  ****                                             04100043
C                                                                       04110043
      IF (ICZERO) 36960, 6960, 36960                                    04120043
 6960 CONTINUE                                                          04130043
      IVON01 = 45                                                       04140043
      IVON02 =  9                                                       04150043
      IVON03 =  3                                                       04160043
      IVCOMP = (IVON01 - IVON02) + IVON03                               04170043
      GO TO 46960                                                       04180043
36960 IVDELE = IVDELE + 1                                               04190043
      WRITE (I02,80003) IVTNUM                                          04200043
      IF (ICZERO) 46960, 6971, 46960                                    04210043
46960 IF (IVCOMP - 39) 26960,16960,26960                                04220043
16960 IVPASS = IVPASS + 1                                               04230043
      WRITE (I02,80001) IVTNUM                                          04240043
      GO TO 6971                                                        04250043
26960 IVFAIL = IVFAIL + 1                                               04260043
      IVCORR = 39                                                       04270043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04280043
 6971 CONTINUE                                                          04290043
      IVTNUM = 697                                                      04300043
C                                                                       04310043
C      ****  TEST 697  ****                                             04320043
C                                                                       04330043
      IF (ICZERO) 36970, 6970, 36970                                    04340043
 6970 CONTINUE                                                          04350043
      IVON01 = 45                                                       04360043
      IVON02 =  9                                                       04370043
      IVON03 =  3                                                       04380043
      IVCOMP = IVON01 - (IVON02 + IVON03)                               04390043
      GO TO 46970                                                       04400043
36970 IVDELE = IVDELE + 1                                               04410043
      WRITE (I02,80003) IVTNUM                                          04420043
      IF (ICZERO) 46970, 6981, 46970                                    04430043
46970 IF (IVCOMP - 33) 26970,16970,26970                                04440043
16970 IVPASS = IVPASS + 1                                               04450043
      WRITE (I02,80001) IVTNUM                                          04460043
      GO TO 6981                                                        04470043
26970 IVFAIL = IVFAIL + 1                                               04480043
      IVCORR = 33                                                       04490043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04500043
 6981 CONTINUE                                                          04510043
C                                                                       04520043
C     TESTS 698 THROUGH 700 TEST '-' FOLLOWED BY '*'.                   04530043
C                                                                       04540043
      IVTNUM = 698                                                      04550043
C                                                                       04560043
C      ****  TEST 698  ****                                             04570043
C                                                                       04580043
      IF (ICZERO) 36980, 6980, 36980                                    04590043
 6980 CONTINUE                                                          04600043
      IVON01 = 45                                                       04610043
      IVON02 =  9                                                       04620043
      IVON03 =  3                                                       04630043
      IVCOMP =  IVON01 - IVON02 * IVON03                                04640043
      GO TO 46980                                                       04650043
36980 IVDELE = IVDELE + 1                                               04660043
      WRITE (I02,80003) IVTNUM                                          04670043
      IF (ICZERO) 46980, 6991, 46980                                    04680043
46980 IF (IVCOMP - 18) 26980,16980,26980                                04690043
16980 IVPASS = IVPASS + 1                                               04700043
      WRITE (I02,80001) IVTNUM                                          04710043
      GO TO 6991                                                        04720043
26980 IVFAIL = IVFAIL + 1                                               04730043
      IVCORR = 18                                                       04740043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04750043
 6991 CONTINUE                                                          04760043
      IVTNUM = 699                                                      04770043
C                                                                       04780043
C      ****  TEST 699  ****                                             04790043
C                                                                       04800043
      IF (ICZERO) 36990, 6990, 36990                                    04810043
 6990 CONTINUE                                                          04820043
      IVON01 = 45                                                       04830043
      IVON02 =  9                                                       04840043
      IVON03 =  3                                                       04850043
      IVCOMP = (IVON01 - IVON02) * IVON03                               04860043
      GO TO 46990                                                       04870043
36990 IVDELE = IVDELE + 1                                               04880043
      WRITE (I02,80003) IVTNUM                                          04890043
      IF (ICZERO) 46990, 7001, 46990                                    04900043
46990 IF (IVCOMP - 108) 26990,16990,26990                               04910043
16990 IVPASS = IVPASS + 1                                               04920043
      WRITE (I02,80001) IVTNUM                                          04930043
      GO TO 7001                                                        04940043
26990 IVFAIL = IVFAIL + 1                                               04950043
      IVCORR = 108                                                      04960043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04970043
 7001 CONTINUE                                                          04980043
      IVTNUM = 700                                                      04990043
C                                                                       05000043
C      ****  TEST 700  ****                                             05010043
C                                                                       05020043
      IF (ICZERO) 37000, 7000, 37000                                    05030043
 7000 CONTINUE                                                          05040043
      IVON01 = 45                                                       05050043
      IVON02 =  9                                                       05060043
      IVON03 =  3                                                       05070043
      IVCOMP = IVON01 - (IVON02 * IVON03)                               05080043
      GO TO 47000                                                       05090043
37000 IVDELE = IVDELE + 1                                               05100043
      WRITE (I02,80003) IVTNUM                                          05110043
      IF (ICZERO) 47000, 7011, 47000                                    05120043
47000 IF (IVCOMP - 18) 27000,17000,27000                                05130043
17000 IVPASS = IVPASS + 1                                               05140043
      WRITE (I02,80001) IVTNUM                                          05150043
      GO TO 7011                                                        05160043
27000 IVFAIL = IVFAIL + 1                                               05170043
      IVCORR = 18                                                       05180043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05190043
 7011 CONTINUE                                                          05200043
C                                                                       05210043
C     TESTS 701 THROUGH 703 TEST '-' FOLLOWED BY '/'.                   05220043
C                                                                       05230043
      IVTNUM = 701                                                      05240043
C                                                                       05250043
C      ****  TEST 701  ****                                             05260043
C                                                                       05270043
      IF (ICZERO) 37010, 7010, 37010                                    05280043
 7010 CONTINUE                                                          05290043
      IVON01 = 45                                                       05300043
      IVON02 =  9                                                       05310043
      IVON03 =  3                                                       05320043
      IVCOMP = IVON01 - IVON02 / IVON03                                 05330043
      GO TO 47010                                                       05340043
37010 IVDELE = IVDELE + 1                                               05350043
      WRITE (I02,80003) IVTNUM                                          05360043
      IF (ICZERO) 47010, 7021, 47010                                    05370043
47010 IF (IVCOMP - 42) 27010,17010,27010                                05380043
17010 IVPASS = IVPASS + 1                                               05390043
      WRITE (I02,80001) IVTNUM                                          05400043
      GO TO 7021                                                        05410043
27010 IVFAIL = IVFAIL + 1                                               05420043
      IVCORR = 42                                                       05430043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05440043
 7021 CONTINUE                                                          05450043
      IVTNUM = 702                                                      05460043
C                                                                       05470043
C      ****  TEST 702  ****                                             05480043
C                                                                       05490043
      IF (ICZERO) 37020, 7020, 37020                                    05500043
 7020 CONTINUE                                                          05510043
      IVON01 = 45                                                       05520043
      IVON02 =  9                                                       05530043
      IVON03 =  3                                                       05540043
      IVCOMP = (IVON01 - IVON02) / IVON03                               05550043
      GO TO 47020                                                       05560043
37020 IVDELE = IVDELE + 1                                               05570043
      WRITE (I02,80003) IVTNUM                                          05580043
      IF (ICZERO) 47020, 7031, 47020                                    05590043
47020 IF (IVCOMP - 12) 27020,17020,27020                                05600043
17020 IVPASS = IVPASS + 1                                               05610043
      WRITE (I02,80001) IVTNUM                                          05620043
      GO TO 7031                                                        05630043
27020 IVFAIL = IVFAIL + 1                                               05640043
      IVCORR = 12                                                       05650043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05660043
 7031 CONTINUE                                                          05670043
      IVTNUM = 703                                                      05680043
C                                                                       05690043
C      ****  TEST 703  ****                                             05700043
C                                                                       05710043
      IF (ICZERO) 37030, 7030, 37030                                    05720043
 7030 CONTINUE                                                          05730043
      IVON01 = 45                                                       05740043
      IVON02 =  9                                                       05750043
      IVON03 =  3                                                       05760043
      IVCOMP = IVON01 - (IVON02 / IVON03)                               05770043
      GO TO 47030                                                       05780043
37030 IVDELE = IVDELE + 1                                               05790043
      WRITE (I02,80003) IVTNUM                                          05800043
      IF (ICZERO) 47030, 7041, 47030                                    05810043
47030 IF (IVCOMP - 42) 27030,17030,27030                                05820043
17030 IVPASS = IVPASS + 1                                               05830043
      WRITE (I02,80001) IVTNUM                                          05840043
      GO TO 7041                                                        05850043
27030 IVFAIL = IVFAIL + 1                                               05860043
      IVCORR = 42                                                       05870043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05880043
 7041 CONTINUE                                                          05890043
C                                                                       05900043
C     TESTS 704 THROUGH 706 TEST '-' FOLLOWED BY '**'.                  05910043
C                                                                       05920043
      IVTNUM = 704                                                      05930043
C                                                                       05940043
C      ****  TEST 704  ****                                             05950043
C                                                                       05960043
      IF (ICZERO) 37040, 7040, 37040                                    05970043
 7040 CONTINUE                                                          05980043
      IVON01 = 35                                                       05990043
      IVON02 =  9                                                       06000043
      IVON03 =  3                                                       06010043
      IVCOMP = IVON01 - IVON02 ** IVON03                                06020043
      GO TO 47040                                                       06030043
37040 IVDELE = IVDELE + 1                                               06040043
      WRITE (I02,80003) IVTNUM                                          06050043
      IF (ICZERO) 47040, 7051, 47040                                    06060043
47040 IF (IVCOMP + 694) 27040,17040,27040                               06070043
17040 IVPASS = IVPASS + 1                                               06080043
      WRITE (I02,80001) IVTNUM                                          06090043
      GO TO 7051                                                        06100043
27040 IVFAIL = IVFAIL + 1                                               06110043
      IVCORR = -694                                                     06120043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06130043
 7051 CONTINUE                                                          06140043
      IVTNUM = 705                                                      06150043
C                                                                       06160043
C      ****  TEST 705  ****                                             06170043
C                                                                       06180043
      IF (ICZERO) 37050, 7050, 37050                                    06190043
 7050 CONTINUE                                                          06200043
      IVON01 = 35                                                       06210043
      IVON02 =  9                                                       06220043
      IVON03 =  3                                                       06230043
      IVCOMP = (IVON01 - IVON02) ** IVON03                              06240043
      GO TO 47050                                                       06250043
37050 IVDELE = IVDELE + 1                                               06260043
      WRITE (I02,80003) IVTNUM                                          06270043
      IF (ICZERO) 47050, 7061, 47050                                    06280043
47050 IF (IVCOMP - 17576) 27050,17050,27050                             06290043
17050 IVPASS = IVPASS + 1                                               06300043
      WRITE (I02,80001) IVTNUM                                          06310043
      GO TO 7061                                                        06320043
27050 IVFAIL = IVFAIL + 1                                               06330043
      IVCORR = 17576                                                    06340043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06350043
 7061 CONTINUE                                                          06360043
      IVTNUM = 706                                                      06370043
C                                                                       06380043
C      ****  TEST 706  ****                                             06390043
C                                                                       06400043
      IF (ICZERO) 37060, 7060, 37060                                    06410043
 7060 CONTINUE                                                          06420043
      IVON01 = 35                                                       06430043
      IVON02 =  9                                                       06440043
      IVON03 =  3                                                       06450043
      IVCOMP = IVON01 - (IVON02 ** IVON03)                              06460043
      GO TO 47060                                                       06470043
37060 IVDELE = IVDELE + 1                                               06480043
      WRITE (I02,80003) IVTNUM                                          06490043
      IF (ICZERO) 47060, 7071, 47060                                    06500043
47060 IF (IVCOMP + 694) 27060,17060,27060                               06510043
17060 IVPASS = IVPASS + 1                                               06520043
      WRITE (I02,80001) IVTNUM                                          06530043
      GO TO 7071                                                        06540043
27060 IVFAIL = IVFAIL + 1                                               06550043
      IVCORR = -694                                                     06560043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06570043
 7071 CONTINUE                                                          06580043
C                                                                       06590043
C     TESTS 707 THROUGH 709 TEST '*' FOLLOWED BY '+'.                   06600043
C                                                                       06610043
      IVTNUM = 707                                                      06620043
C                                                                       06630043
C      ****  TEST 707  ****                                             06640043
C                                                                       06650043
      IF (ICZERO) 37070, 7070, 37070                                    06660043
 7070 CONTINUE                                                          06670043
      IVON01 = 45                                                       06680043
      IVON02 =  9                                                       06690043
      IVON03 =  3                                                       06700043
      IVCOMP =  IVON01 * IVON02 + IVON03                                06710043
      GO TO 47070                                                       06720043
37070 IVDELE = IVDELE + 1                                               06730043
      WRITE (I02,80003) IVTNUM                                          06740043
      IF (ICZERO) 47070, 7081, 47070                                    06750043
47070 IF (IVCOMP - 408) 27070,17070,27070                               06760043
17070 IVPASS = IVPASS + 1                                               06770043
      WRITE (I02,80001) IVTNUM                                          06780043
      GO TO 7081                                                        06790043
27070 IVFAIL = IVFAIL + 1                                               06800043
      IVCORR = 408                                                      06810043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06820043
 7081 CONTINUE                                                          06830043
      IVTNUM = 708                                                      06840043
C                                                                       06850043
C      ****  TEST 708  ****                                             06860043
C                                                                       06870043
      IF (ICZERO) 37080, 7080, 37080                                    06880043
 7080 CONTINUE                                                          06890043
      IVON01 = 45                                                       06900043
      IVON02 =  9                                                       06910043
      IVON03 =  3                                                       06920043
      IVCOMP = (IVON01 * IVON02) + IVON03                               06930043
      GO TO 47080                                                       06940043
37080 IVDELE = IVDELE + 1                                               06950043
      WRITE (I02,80003) IVTNUM                                          06960043
      IF (ICZERO) 47080, 7091, 47080                                    06970043
47080 IF (IVCOMP - 408) 27080,17080,27080                               06980043
17080 IVPASS = IVPASS + 1                                               06990043
      WRITE (I02,80001) IVTNUM                                          07000043
      GO TO 7091                                                        07010043
27080 IVFAIL = IVFAIL + 1                                               07020043
      IVCORR = 408                                                      07030043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07040043
 7091 CONTINUE                                                          07050043
      IVTNUM = 709                                                      07060043
C                                                                       07070043
C      ****  TEST 709  ****                                             07080043
C                                                                       07090043
      IF (ICZERO) 37090, 7090, 37090                                    07100043
 7090 CONTINUE                                                          07110043
      IVON01 = 45                                                       07120043
      IVON02 =  9                                                       07130043
      IVON03 =  3                                                       07140043
      IVCOMP = IVON01 * (IVON02 + IVON03)                               07150043
      GO TO 47090                                                       07160043
37090 IVDELE = IVDELE + 1                                               07170043
      WRITE (I02,80003) IVTNUM                                          07180043
      IF (ICZERO) 47090, 7101, 47090                                    07190043
47090 IF (IVCOMP - 540) 27090,17090,27090                               07200043
17090 IVPASS = IVPASS + 1                                               07210043
      WRITE (I02,80001) IVTNUM                                          07220043
      GO TO 7101                                                        07230043
27090 IVFAIL = IVFAIL + 1                                               07240043
      IVCORR = 540                                                      07250043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07260043
 7101 CONTINUE                                                          07270043
C                                                                       07280043
C     TESTS 710 THROUGH 712 TEST '*' FOLLOWED BY '-'.                   07290043
C                                                                       07300043
      IVTNUM = 710                                                      07310043
C                                                                       07320043
C      ****  TEST 710  ****                                             07330043
C                                                                       07340043
      IF (ICZERO) 37100, 7100, 37100                                    07350043
 7100 CONTINUE                                                          07360043
      IVON01 = 45                                                       07370043
      IVON02 =  9                                                       07380043
      IVON03 =  3                                                       07390043
      IVCOMP = IVON01 * IVON02 - IVON03                                 07400043
      GO TO 47100                                                       07410043
37100 IVDELE = IVDELE + 1                                               07420043
      WRITE (I02,80003) IVTNUM                                          07430043
      IF (ICZERO) 47100, 7111, 47100                                    07440043
47100 IF (IVCOMP - 402) 27100,17100,27100                               07450043
17100 IVPASS = IVPASS + 1                                               07460043
      WRITE (I02,80001) IVTNUM                                          07470043
      GO TO 7111                                                        07480043
27100 IVFAIL = IVFAIL + 1                                               07490043
      IVCORR = 402                                                      07500043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07510043
 7111 CONTINUE                                                          07520043
      IVTNUM = 711                                                      07530043
C                                                                       07540043
C      ****  TEST 711  ****                                             07550043
C                                                                       07560043
      IF (ICZERO) 37110, 7110, 37110                                    07570043
 7110 CONTINUE                                                          07580043
      IVON01 = 45                                                       07590043
      IVON02 =  9                                                       07600043
      IVON03 =  3                                                       07610043
      IVCOMP = (IVON01 * IVON02) - IVON03                               07620043
      GO TO 47110                                                       07630043
37110 IVDELE = IVDELE + 1                                               07640043
      WRITE (I02,80003) IVTNUM                                          07650043
      IF (ICZERO) 47110, 7121, 47110                                    07660043
47110 IF (IVCOMP - 402) 27110,17110,27110                               07670043
17110 IVPASS = IVPASS + 1                                               07680043
      WRITE (I02,80001) IVTNUM                                          07690043
      GO TO 7121                                                        07700043
27110 IVFAIL = IVFAIL + 1                                               07710043
      IVCORR = 402                                                      07720043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07730043
 7121 CONTINUE                                                          07740043
      IVTNUM = 712                                                      07750043
C                                                                       07760043
C      ****  TEST 712  ****                                             07770043
C                                                                       07780043
      IF (ICZERO) 37120, 7120, 37120                                    07790043
 7120 CONTINUE                                                          07800043
      IVON01 = 45                                                       07810043
      IVON02 =  9                                                       07820043
      IVON03 =  3                                                       07830043
      IVCOMP = IVON01 * (IVON02 - IVON03)                               07840043
      GO TO 47120                                                       07850043
37120 IVDELE = IVDELE + 1                                               07860043
      WRITE (I02,80003) IVTNUM                                          07870043
      IF (ICZERO) 47120, 7131, 47120                                    07880043
47120 IF (IVCOMP - 270) 27120,17120,27120                               07890043
17120 IVPASS = IVPASS + 1                                               07900043
      WRITE (I02,80001) IVTNUM                                          07910043
      GO TO 7131                                                        07920043
27120 IVFAIL = IVFAIL + 1                                               07930043
      IVCORR = 270                                                      07940043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07950043
 7131 CONTINUE                                                          07960043
C                                                                       07970043
C     TESTS 713 THROUGH 715 TEST '*' FOLLOWED BY '/'.                   07980043
C                                                                       07990043
      IVTNUM = 713                                                      08000043
C                                                                       08010043
C      ****  TEST 713  ****                                             08020043
C                                                                       08030043
      IF (ICZERO) 37130, 7130, 37130                                    08040043
 7130 CONTINUE                                                          08050043
      IVON01 = 45                                                       08060043
      IVON02 =  9                                                       08070043
      IVON03 =  3                                                       08080043
      IVCOMP = IVON01 * IVON02 / IVON03                                 08090043
      GO TO 47130                                                       08100043
37130 IVDELE = IVDELE + 1                                               08110043
      WRITE (I02,80003) IVTNUM                                          08120043
      IF (ICZERO) 47130, 7141, 47130                                    08130043
47130 IF (IVCOMP - 135) 27130,17130,27130                               08140043
17130 IVPASS = IVPASS + 1                                               08150043
      WRITE (I02,80001) IVTNUM                                          08160043
      GO TO 7141                                                        08170043
27130 IVFAIL = IVFAIL + 1                                               08180043
      IVCORR = 135                                                      08190043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          08200043
 7141 CONTINUE                                                          08210043
      IVTNUM = 714                                                      08220043
C                                                                       08230043
C      ****  TEST 714  ****                                             08240043
C                                                                       08250043
      IF (ICZERO) 37140, 7140, 37140                                    08260043
 7140 CONTINUE                                                          08270043
      IVON01 = 45                                                       08280043
      IVON02 =  9                                                       08290043
      IVON03 =  3                                                       08300043
      IVCOMP = (IVON01 * IVON02) / IVON03                               08310043
      GO TO 47140                                                       08320043
37140 IVDELE = IVDELE + 1                                               08330043
      WRITE (I02,80003) IVTNUM                                          08340043
      IF (ICZERO) 47140, 7151, 47140                                    08350043
47140 IF (IVCOMP - 135) 27140,17140,27140                               08360043
17140 IVPASS = IVPASS + 1                                               08370043
      WRITE (I02,80001) IVTNUM                                          08380043
      GO TO 7151                                                        08390043
27140 IVFAIL = IVFAIL + 1                                               08400043
      IVCORR = 135                                                      08410043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          08420043
 7151 CONTINUE                                                          08430043
      IVTNUM = 715                                                      08440043
C                                                                       08450043
C      ****  TEST 715  ****                                             08460043
C                                                                       08470043
      IF (ICZERO) 37150, 7150, 37150                                    08480043
 7150 CONTINUE                                                          08490043
      IVON01 = 45                                                       08500043
      IVON02 =  9                                                       08510043
      IVON03 =  3                                                       08520043
      IVCOMP = IVON01 * (IVON02 / IVON03)                               08530043
      GO TO 47150                                                       08540043
37150 IVDELE = IVDELE + 1                                               08550043
      WRITE (I02,80003) IVTNUM                                          08560043
      IF (ICZERO) 47150, 7161, 47150                                    08570043
47150 IF (IVCOMP - 135) 27150,17150,27150                               08580043
17150 IVPASS = IVPASS + 1                                               08590043
      WRITE (I02,80001) IVTNUM                                          08600043
      GO TO 7161                                                        08610043
27150 IVFAIL = IVFAIL + 1                                               08620043
      IVCORR = 135                                                      08630043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          08640043
 7161 CONTINUE                                                          08650043
C                                                                       08660043
C     TESTS 716 THROUGH 718 TEST '*' FOLLOWED BY '**'.                  08670043
C                                                                       08680043
      IVTNUM = 716                                                      08690043
C                                                                       08700043
C      ****  TEST 716  ****                                             08710043
C                                                                       08720043
      IF (ICZERO) 37160, 7160, 37160                                    08730043
 7160 CONTINUE                                                          08740043
      IVON01 = 7                                                        08750043
      IVON02 = 3                                                        08760043
      IVON03 = 3                                                        08770043
      IVCOMP = IVON01 * IVON02  ** IVON03                               08780043
      GO TO 47160                                                       08790043
37160 IVDELE = IVDELE + 1                                               08800043
      WRITE (I02,80003) IVTNUM                                          08810043
      IF (ICZERO) 47160, 7171, 47160                                    08820043
47160 IF (IVCOMP - 189) 27160,17160,27160                               08830043
17160 IVPASS = IVPASS + 1                                               08840043
      WRITE (I02,80001) IVTNUM                                          08850043
      GO TO 7171                                                        08860043
27160 IVFAIL = IVFAIL + 1                                               08870043
      IVCORR = 189                                                      08880043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          08890043
 7171 CONTINUE                                                          08900043
      IVTNUM = 717                                                      08910043
C                                                                       08920043
C      ****  TEST 717  ****                                             08930043
C                                                                       08940043
      IF (ICZERO) 37170, 7170, 37170                                    08950043
 7170 CONTINUE                                                          08960043
      IVON01 = 7                                                        08970043
      IVON02 = 3                                                        08980043
      IVON03 = 3                                                        08990043
      IVCOMP = (IVON01 * IVON02) ** IVON03                              09000043
      GO TO 47170                                                       09010043
37170 IVDELE = IVDELE + 1                                               09020043
      WRITE (I02,80003) IVTNUM                                          09030043
      IF (ICZERO) 47170, 7181, 47170                                    09040043
47170 IF (IVCOMP - 9261) 27170,17170,27170                              09050043
17170 IVPASS = IVPASS + 1                                               09060043
      WRITE (I02,80001) IVTNUM                                          09070043
      GO TO 7181                                                        09080043
27170 IVFAIL = IVFAIL + 1                                               09090043
      IVCORR = 9261                                                     09100043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          09110043
 7181 CONTINUE                                                          09120043
      IVTNUM = 718                                                      09130043
C                                                                       09140043
C      ****  TEST 718  ****                                             09150043
C                                                                       09160043
      IF (ICZERO) 37180, 7180, 37180                                    09170043
 7180 CONTINUE                                                          09180043
      IVON01 = 7                                                        09190043
      IVON02 = 3                                                        09200043
      IVON03 = 3                                                        09210043
      IVCOMP = IVON01 * (IVON02 ** IVON03)                              09220043
      GO TO 47180                                                       09230043
37180 IVDELE = IVDELE + 1                                               09240043
      WRITE (I02,80003) IVTNUM                                          09250043
      IF (ICZERO) 47180, 7191, 47180                                    09260043
47180 IF (IVCOMP - 189) 27180,17180,27180                               09270043
17180 IVPASS = IVPASS + 1                                               09280043
      WRITE (I02,80001) IVTNUM                                          09290043
      GO TO 7191                                                        09300043
27180 IVFAIL = IVFAIL + 1                                               09310043
      IVCORR = 189                                                      09320043
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          09330043
 7191 CONTINUE                                                          09340043
C                                                                       09350043
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             09360043
99999 CONTINUE                                                          09370043
      WRITE (I02,90002)                                                 09380043
      WRITE (I02,90006)                                                 09390043
      WRITE (I02,90002)                                                 09400043
      WRITE (I02,90002)                                                 09410043
      WRITE (I02,90007)                                                 09420043
      WRITE (I02,90002)                                                 09430043
      WRITE (I02,90008)  IVFAIL                                         09440043
      WRITE (I02,90009) IVPASS                                          09450043
      WRITE (I02,90010) IVDELE                                          09460043
C                                                                       09470043
C                                                                       09480043
C     TERMINATE ROUTINE EXECUTION                                       09490043
      STOP                                                              09500043
C                                                                       09510043
C     FORMAT STATEMENTS FOR PAGE HEADERS                                09520043
90000 FORMAT (1H1)                                                      09530043
90002 FORMAT (1H )                                                      09540043
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09550043
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   09560043
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        09570043
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 09580043
90006 FORMAT (1H ,5X,46H----------------------------------------------) 09590043
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             09600043
C                                                                       09610043
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               09620043
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        09630043
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              09640043
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             09650043
C                                                                       09660043
C     FORMAT STATEMENTS FOR TEST RESULTS                                09670043
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      09680043
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      09690043
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   09700043
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         09710043
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    09720043
C                                                                       09730043
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM043)                          09740043
      END                                                               09750043
