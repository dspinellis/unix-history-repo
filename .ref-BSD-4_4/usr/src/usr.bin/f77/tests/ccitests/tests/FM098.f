C     COMMENT SECTION                                                   00010098
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020098
C     FM098                                                             00030098
C                                                                       00040098
C     THIS ROUTINE TESTS INTRINSIC FUNCTIONS WHERE THE FUNCTION TYPE IS 00050098
C     INTEGER AND THE ARGUMENTS ARE EITHER INTEGER OR REAL.  THE REAL   00060098
C     AND INTEGER VARIABLES AND THE REAL AND INTEGER CONSTANTS CONTAIN  00070098
C     BOTH POSITIVE AND NEGATIVE VALUES.  THE INTRINSIC FUNCTIONS TESTED00080098
C     BY FM098 INCLUDE                                                  00090098
C                                                     TYPE OF           00100098
C       INTRINSIC FUNCTION          NAME       ARGUMENT     FUNCTION    00110098
C       ------------------          ----       --------     --------    00120098
C         ABSOLUTE VALUE            IABS       INTEGER      INTEGER     00130098
C         TRUNCATION                INT        REAL         INTEGER     00140098
C         REMAINDERING              MOD        INTEGER      INTEGER     00150098
C         CHOOSING LARGEST VALUE    MAX0       INTEGER      INTEGER     00160098
C                                   MAX1       REAL         INTEGER     00170098
C         CHOOSING SMALLEST VALUE   MIN0       INTEGER      INTEGER     00180098
C                                   MIN1       REAL         INTEGER     00190098
C         FIX                       IFIX      REAL          INTEGER     00200098
C         TRANSFER OF SIGN          ISIGN     INTEGER       INTEGER     00210098
C         POSITIVE DIFFERENCE       IDIM      INTEGER       INTEGER     00220098
C                                                                       00230098
C      REFERENCES                                                       00240098
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00250098
C              X3.9-1978                                                00260098
C                                                                       00270098
C        SECTION 4.1.2, TYPE RULES FOR DATA AND PROCEDURE IDENTIFIERS   00280098
C        SECTION 15.3, INTRINSIC FUNCTION                               00290098
C        SECTION 15.3.2, INTRINSIC FUNCTIONS AND THEIR REFERENCE        00300098
C                                                                       00310098
C                                                                       00320098
C      **********************************************************       00330098
C                                                                       00340098
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00350098
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00360098
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00370098
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00380098
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00390098
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00400098
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00410098
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00420098
C     OF EXECUTING THESE TESTS.                                         00430098
C                                                                       00440098
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00450098
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00460098
C                                                                       00470098
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00480098
C                                                                       00490098
C                  DEPARTMENT OF THE NAVY                               00500098
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00510098
C                  WASHINGTON, D.C.  20376                              00520098
C                                                                       00530098
C      **********************************************************       00540098
C                                                                       00550098
C                                                                       00560098
C                                                                       00570098
C     INITIALIZATION SECTION                                            00580098
C                                                                       00590098
C     INITIALIZE CONSTANTS                                              00600098
C      **************                                                   00610098
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620098
      I01 = 5                                                           00630098
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640098
      I02 = 6                                                           00650098
C     SYSTEM ENVIRONMENT SECTION                                        00660098
C                                                                       00670098
      I01 = 5                                                           00680098
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690098
C     (UNIT NUMBER FOR CARD READER).                                    00700098
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00710098
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00720098
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00730098
C                                                                       00740098
      I02 = 6                                                           00750098
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00760098
C     (UNIT NUMBER FOR PRINTER).                                        00770098
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00780098
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00790098
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00800098
C                                                                       00810098
      IVPASS=0                                                          00820098
      IVFAIL=0                                                          00830098
      IVDELE=0                                                          00840098
      ICZERO=0                                                          00850098
C                                                                       00860098
C     WRITE PAGE HEADERS                                                00870098
      WRITE (I02,90000)                                                 00880098
      WRITE (I02,90001)                                                 00890098
      WRITE (I02,90002)                                                 00900098
      WRITE (I02, 90002)                                                00910098
      WRITE (I02,90003)                                                 00920098
      WRITE (I02,90002)                                                 00930098
      WRITE (I02,90004)                                                 00940098
      WRITE (I02,90002)                                                 00950098
      WRITE (I02,90011)                                                 00960098
      WRITE (I02,90002)                                                 00970098
      WRITE (I02,90002)                                                 00980098
      WRITE (I02,90005)                                                 00990098
      WRITE (I02,90006)                                                 01000098
      WRITE (I02,90002)                                                 01010098
C                                                                       01020098
C     TEST SECTION                                                      01030098
C                                                                       01040098
C     TEST 907 THROUGH TEST 909 CONTAIN INTRINSIC FUNCTION TESTS FOR    01050098
C     ABSOLUTE VALUE WHERE ARGUMENT AND FUNCTION ARE INTEGER            01060098
C                                                                       01070098
 9071 CONTINUE                                                          01080098
      IVTNUM = 907                                                      01090098
C                                                                       01100098
C      ****  TEST 907  ****                                             01110098
C                                                                       01120098
      IF (ICZERO) 39070, 9070, 39070                                    01130098
 9070 CONTINUE                                                          01140098
      IVCOMP = IABS (-382)                                              01150098
      GO TO 49070                                                       01160098
39070 IVDELE = IVDELE + 1                                               01170098
      WRITE (I02,80003) IVTNUM                                          01180098
      IF (ICZERO) 49070, 9081, 49070                                    01190098
49070 IF (IVCOMP - 382) 29070,19070,29070                               01200098
19070 IVPASS = IVPASS + 1                                               01210098
      WRITE (I02,80001) IVTNUM                                          01220098
      GO TO 9081                                                        01230098
29070 IVFAIL = IVFAIL + 1                                               01240098
      IVCORR = 382                                                      01250098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01260098
 9081 CONTINUE                                                          01270098
      IVTNUM = 908                                                      01280098
C                                                                       01290098
C      ****  TEST 908  ****                                             01300098
C                                                                       01310098
      IF (ICZERO) 39080, 9080, 39080                                    01320098
 9080 CONTINUE                                                          01330098
      IVON01 = 445                                                      01340098
      IVCOMP = IABS (IVON01)                                            01350098
      GO TO 49080                                                       01360098
39080 IVDELE = IVDELE + 1                                               01370098
      WRITE (I02,80003) IVTNUM                                          01380098
      IF (ICZERO) 49080, 9091, 49080                                    01390098
49080 IF (IVCOMP - 445) 29080,19080,29080                               01400098
19080 IVPASS = IVPASS + 1                                               01410098
      WRITE (I02,80001) IVTNUM                                          01420098
      GO TO 9091                                                        01430098
29080 IVFAIL = IVFAIL + 1                                               01440098
      IVCORR = 445                                                      01450098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01460098
 9091 CONTINUE                                                          01470098
      IVTNUM = 909                                                      01480098
C                                                                       01490098
C      ****  TEST 909  ****                                             01500098
C                                                                       01510098
      IF (ICZERO) 39090, 9090, 39090                                    01520098
 9090 CONTINUE                                                          01530098
      IVON01 = -32176                                                   01540098
      IVCOMP = IABS (IVON01)                                            01550098
      GO TO 49090                                                       01560098
39090 IVDELE = IVDELE + 1                                               01570098
      WRITE (I02,80003) IVTNUM                                          01580098
      IF (ICZERO) 49090, 9101, 49090                                    01590098
49090 IF (IVCOMP - 32176) 29090,19090,29090                             01600098
19090 IVPASS = IVPASS + 1                                               01610098
      WRITE (I02,80001) IVTNUM                                          01620098
      GO TO 9101                                                        01630098
29090 IVFAIL = IVFAIL + 1                                               01640098
      IVCORR = 32176                                                    01650098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01660098
C                                                                       01670098
C     TEST 910 THROUGH TEST 913 CONTAIN INTRINSIC FUNCTION TESTS FOR    01680098
C     TRUNCATION WHERE ARGUMENT IS REAL AND FUNCTION IS INTEGER         01690098
C                                                                       01700098
 9101 CONTINUE                                                          01710098
      IVTNUM = 910                                                      01720098
C                                                                       01730098
C      ****  TEST 910  ****                                             01740098
C                                                                       01750098
      IF (ICZERO) 39100, 9100, 39100                                    01760098
 9100 CONTINUE                                                          01770098
      IVCOMP = INT (38.2)                                               01780098
      GO TO 49100                                                       01790098
39100 IVDELE = IVDELE + 1                                               01800098
      WRITE (I02,80003) IVTNUM                                          01810098
      IF (ICZERO) 49100, 9111, 49100                                    01820098
49100 IF (IVCOMP - 38) 29100,19100,29100                                01830098
19100 IVPASS = IVPASS + 1                                               01840098
      WRITE (I02,80001) IVTNUM                                          01850098
      GO TO 9111                                                        01860098
29100 IVFAIL = IVFAIL + 1                                               01870098
      IVCORR = 38                                                       01880098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01890098
 9111 CONTINUE                                                          01900098
      IVTNUM = 911                                                      01910098
C                                                                       01920098
C      ****  TEST 911  ****                                             01930098
C                                                                       01940098
      IF (ICZERO) 39110, 9110, 39110                                    01950098
 9110 CONTINUE                                                          01960098
      RVON01 = -445.95                                                  01970098
      IVCOMP = INT (RVON01)                                             01980098
      GO TO 49110                                                       01990098
39110 IVDELE = IVDELE + 1                                               02000098
      WRITE (I02,80003) IVTNUM                                          02010098
      IF (ICZERO) 49110, 9121, 49110                                    02020098
49110 IF (IVCOMP + 445) 29110,19110,29110                               02030098
19110 IVPASS = IVPASS + 1                                               02040098
      WRITE (I02,80001) IVTNUM                                          02050098
      GO TO 9121                                                        02060098
29110 IVFAIL = IVFAIL + 1                                               02070098
      IVCORR = -445                                                     02080098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02090098
 9121 CONTINUE                                                          02100098
      IVTNUM = 912                                                      02110098
C                                                                       02120098
C      ****  TEST 912  ****                                             02130098
C                                                                       02140098
      IF (ICZERO) 39120, 9120, 39120                                    02150098
 9120 CONTINUE                                                          02160098
      RVON01 = 466.01                                                   02170098
      IVCOMP = INT (RVON01)                                             02180098
      GO TO 49120                                                       02190098
39120 IVDELE = IVDELE + 1                                               02200098
      WRITE (I02,80003) IVTNUM                                          02210098
      IF (ICZERO) 49120, 9131, 49120                                    02220098
49120 IF (IVCOMP - 466) 29120,19120,29120                               02230098
19120 IVPASS = IVPASS + 1                                               02240098
      WRITE (I02,80001) IVTNUM                                          02250098
      GO TO 9131                                                        02260098
29120 IVFAIL = IVFAIL + 1                                               02270098
      IVCORR = 466                                                      02280098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02290098
 9131 CONTINUE                                                          02300098
      IVTNUM = 913                                                      02310098
C                                                                       02320098
C      ****  TEST 913  ****                                             02330098
C                                                                       02340098
      IF (ICZERO) 39130, 9130, 39130                                    02350098
 9130 CONTINUE                                                          02360098
      RVON01 = 382E-1                                                   02370098
      IVCOMP = INT (RVON01)                                             02380098
      GO TO 49130                                                       02390098
39130 IVDELE = IVDELE + 1                                               02400098
      WRITE (I02,80003) IVTNUM                                          02410098
      IF (ICZERO) 49130, 9141, 49130                                    02420098
49130 IF (IVCOMP - 38) 29130,19130,29130                                02430098
19130 IVPASS = IVPASS + 1                                               02440098
      WRITE (I02,80001) IVTNUM                                          02450098
      GO TO 9141                                                        02460098
29130 IVFAIL = IVFAIL + 1                                               02470098
      IVCORR = 38                                                       02480098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02490098
C                                                                       02500098
C     TEST 914 THROUGH TEST 917 CONTAIN INTRINSIC FUNCTION TESTS FOR    02510098
C     REMAINDERING WHERE ARGUMENTS AND FUNCTION ARE INTEGERS            02520098
C                                                                       02530098
 9141 CONTINUE                                                          02540098
      IVTNUM = 914                                                      02550098
C                                                                       02560098
C      ****  TEST 914  ****                                             02570098
C                                                                       02580098
      IF (ICZERO) 39140, 9140, 39140                                    02590098
 9140 CONTINUE                                                          02600098
      IVCOMP = MOD (42,19)                                              02610098
      GO TO 49140                                                       02620098
39140 IVDELE = IVDELE + 1                                               02630098
      WRITE (I02,80003) IVTNUM                                          02640098
      IF (ICZERO) 49140, 9151, 49140                                    02650098
49140 IF (IVCOMP - 4) 29140,19140,29140                                 02660098
19140 IVPASS = IVPASS + 1                                               02670098
      WRITE (I02,80001) IVTNUM                                          02680098
      GO TO 9151                                                        02690098
29140 IVFAIL = IVFAIL + 1                                               02700098
      IVCORR = 4                                                        02710098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02720098
 9151 CONTINUE                                                          02730098
      IVTNUM = 915                                                      02740098
C                                                                       02750098
C      ****  TEST 915  ****                                             02760098
C                                                                       02770098
      IF (ICZERO) 39150, 9150, 39150                                    02780098
 9150 CONTINUE                                                          02790098
      IVON01 = 6667                                                     02800098
      IVON02 = 2                                                        02810098
      IVCOMP = MOD (IVON01,IVON02)                                      02820098
      GO TO 49150                                                       02830098
39150 IVDELE = IVDELE + 1                                               02840098
      WRITE (I02,80003) IVTNUM                                          02850098
      IF (ICZERO) 49150, 9161, 49150                                    02860098
49150 IF (IVCOMP - 1) 29150,19150,29150                                 02870098
19150 IVPASS = IVPASS + 1                                               02880098
      WRITE (I02,80001) IVTNUM                                          02890098
      GO TO 9161                                                        02900098
29150 IVFAIL = IVFAIL + 1                                               02910098
      IVCORR = 1                                                        02920098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02930098
 9161 CONTINUE                                                          02940098
      IVTNUM = 916                                                      02950098
C                                                                       02960098
C      ****  TEST 916  ****                                             02970098
C                                                                       02980098
      IF (ICZERO) 39160, 9160, 39160                                    02990098
 9160 CONTINUE                                                          03000098
      IVON01 = 225                                                      03010098
      IVON02 = 50                                                       03020098
      IVCOMP = MOD (IVON01,IVON02)                                      03030098
      GO TO 49160                                                       03040098
39160 IVDELE = IVDELE + 1                                               03050098
      WRITE (I02,80003) IVTNUM                                          03060098
      IF (ICZERO) 49160, 9171, 49160                                    03070098
49160 IF (IVCOMP - 25) 29160,19160,29160                                03080098
19160 IVPASS = IVPASS + 1                                               03090098
      WRITE (I02,80001) IVTNUM                                          03100098
      GO TO 9171                                                        03110098
29160 IVFAIL = IVFAIL + 1                                               03120098
      IVCORR = 25                                                       03130098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03140098
 9171 CONTINUE                                                          03150098
      IVTNUM = 917                                                      03160098
C                                                                       03170098
C      ****  TEST 917  ****                                             03180098
C                                                                       03190098
      IF (ICZERO) 39170, 9170, 39170                                    03200098
 9170 CONTINUE                                                          03210098
      IVON01 = -39                                                      03220098
      IVON02 = 500                                                      03230098
      IVCOMP = MOD (IVON01,IVON02)                                      03240098
      GO TO 49170                                                       03250098
39170 IVDELE = IVDELE + 1                                               03260098
      WRITE (I02,80003) IVTNUM                                          03270098
      IF (ICZERO) 49170, 9181, 49170                                    03280098
49170 IF (IVCOMP + 39) 29170,19170,29170                                03290098
19170 IVPASS = IVPASS + 1                                               03300098
      WRITE (I02,80001) IVTNUM                                          03310098
      GO TO 9181                                                        03320098
29170 IVFAIL = IVFAIL + 1                                               03330098
      IVCORR = -39                                                      03340098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03350098
C                                                                       03360098
C     TEST 918 AND 919 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    03370098
C     LARGEST VALUE WHERE ARGUMENTS AND FUNCTION ARE INTEGER            03380098
C                                                                       03390098
 9181 CONTINUE                                                          03400098
      IVTNUM = 918                                                      03410098
C                                                                       03420098
C      ****  TEST 918  ****                                             03430098
C                                                                       03440098
      IF (ICZERO) 39180, 9180, 39180                                    03450098
 9180 CONTINUE                                                          03460098
      IVON01 = 317                                                      03470098
      IVON02 = -99                                                      03480098
      IVON03 = 1                                                        03490098
      IVCOMP = MAX0 (263,IVON01,IVON02,IVON03)                          03500098
      GO TO 49180                                                       03510098
39180 IVDELE = IVDELE + 1                                               03520098
      WRITE (I02,80003) IVTNUM                                          03530098
      IF (ICZERO) 49180, 9191, 49180                                    03540098
49180 IF (IVCOMP - 317) 29180,19180,29180                               03550098
19180 IVPASS = IVPASS + 1                                               03560098
      WRITE (I02,80001) IVTNUM                                          03570098
      GO TO 9191                                                        03580098
29180 IVFAIL = IVFAIL + 1                                               03590098
      IVCORR = 317                                                      03600098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03610098
 9191 CONTINUE                                                          03620098
      IVTNUM = 919                                                      03630098
C                                                                       03640098
C      ****  TEST 919  ****                                             03650098
C                                                                       03660098
      IF (ICZERO) 39190, 9190, 39190                                    03670098
 9190 CONTINUE                                                          03680098
      IVON01 = 2572                                                     03690098
      IVON02 = 2570                                                     03700098
      IVCOMP = MAX0 (IVON01,IVON02)                                     03710098
      GO TO 49190                                                       03720098
39190 IVDELE = IVDELE + 1                                               03730098
      WRITE (I02,80003) IVTNUM                                          03740098
      IF (ICZERO) 49190, 9201, 49190                                    03750098
49190 IF (IVCOMP - 2572) 29190,19190,29190                              03760098
19190 IVPASS = IVPASS + 1                                               03770098
      WRITE (I02,80001) IVTNUM                                          03780098
      GO TO 9201                                                        03790098
29190 IVFAIL = IVFAIL + 1                                               03800098
      IVCORR = 2572                                                     03810098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03820098
C                                                                       03830098
C     TEST 920 AND 921 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    03840098
C     LARGEST VALUE WHERE ARGUMENTS ARE REAL AND FUNCTION IS INTEGER    03850098
C                                                                       03860098
 9201 CONTINUE                                                          03870098
      IVTNUM = 920                                                      03880098
C                                                                       03890098
C      ****  TEST 920  ****                                             03900098
C                                                                       03910098
      IF (ICZERO) 39200, 9200, 39200                                    03920098
 9200 CONTINUE                                                          03930098
      RVON01 = .326E+2                                                  03940098
      RVON02 = 22.075                                                   03950098
      RVON03 = 76E-1                                                    03960098
      IVCOMP = MAX1 (RVON01,RVON02,RVON03)                              03970098
      GO TO 49200                                                       03980098
39200 IVDELE = IVDELE + 1                                               03990098
      WRITE (I02,80003) IVTNUM                                          04000098
      IF (ICZERO) 49200, 9211, 49200                                    04010098
49200 IF (IVCOMP - 32) 29200,19200,29200                                04020098
19200 IVPASS = IVPASS + 1                                               04030098
      WRITE (I02,80001) IVTNUM                                          04040098
      GO TO 9211                                                        04050098
29200 IVFAIL = IVFAIL + 1                                               04060098
      IVCORR = 32                                                       04070098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04080098
 9211 CONTINUE                                                          04090098
      IVTNUM = 921                                                      04100098
C                                                                       04110098
C      ****  TEST 921  ****                                             04120098
C                                                                       04130098
      IF (ICZERO) 39210, 9210, 39210                                    04140098
 9210 CONTINUE                                                          04150098
      RVON01 = -6.3E2                                                   04160098
      RVON02 = -21.0                                                    04170098
      IVCOMP = MAX1 (-463.3,RVON01,RVON02)                              04180098
      GO TO 49210                                                       04190098
39210 IVDELE = IVDELE + 1                                               04200098
      WRITE (I02,80003) IVTNUM                                          04210098
      IF (ICZERO) 49210, 9221, 49210                                    04220098
49210 IF (IVCOMP + 21) 29210,19210,29210                                04230098
19210 IVPASS = IVPASS + 1                                               04240098
      WRITE (I02,80001) IVTNUM                                          04250098
      GO TO 9221                                                        04260098
29210 IVFAIL = IVFAIL + 1                                               04270098
      IVCORR = -21                                                      04280098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04290098
C                                                                       04300098
C     TEST 922 AND 923 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    04310098
C     SMALLEST VALUE WHERE ARGUMENTS AND FUNCTION ARE INTEGER           04320098
C                                                                       04330098
 9221 CONTINUE                                                          04340098
      IVTNUM = 922                                                      04350098
C                                                                       04360098
C      ****  TEST 922  ****                                             04370098
C                                                                       04380098
      IF (ICZERO) 39220, 9220, 39220                                    04390098
 9220 CONTINUE                                                          04400098
      IVON01 = -75                                                      04410098
      IVON02 = -243                                                     04420098
      IVCOMP = MIN0 (IVON01,IVON02)                                     04430098
      GO TO 49220                                                       04440098
39220 IVDELE = IVDELE + 1                                               04450098
      WRITE (I02,80003) IVTNUM                                          04460098
      IF (ICZERO) 49220, 9231, 49220                                    04470098
49220 IF (IVCOMP + 243) 29220,19220,29220                               04480098
19220 IVPASS = IVPASS + 1                                               04490098
      WRITE (I02,80001) IVTNUM                                          04500098
      GO TO 9231                                                        04510098
29220 IVFAIL = IVFAIL + 1                                               04520098
      IVCORR = -243                                                     04530098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04540098
 9231 CONTINUE                                                          04550098
      IVTNUM = 923                                                      04560098
C                                                                       04570098
C      ****  TEST 923  ****                                             04580098
C                                                                       04590098
      IF (ICZERO) 39230, 9230, 39230                                    04600098
 9230 CONTINUE                                                          04610098
      IVON01 = -11                                                      04620098
      IVON02 = 11                                                       04630098
      IVCOMP = MIN0 (0,IVON01,IVON02)                                   04640098
      GO TO 49230                                                       04650098
39230 IVDELE = IVDELE + 1                                               04660098
      WRITE (I02,80003) IVTNUM                                          04670098
      IF (ICZERO) 49230, 9241, 49230                                    04680098
49230 IF (IVCOMP + 11) 29230,19230,29230                                04690098
19230 IVPASS = IVPASS + 1                                               04700098
      WRITE (I02,80001) IVTNUM                                          04710098
      GO TO 9241                                                        04720098
29230 IVFAIL = IVFAIL + 1                                               04730098
      IVCORR = -11                                                      04740098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04750098
C                                                                       04760098
C     TEST 924 AND 925 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    04770098
C     SMALLEST VALUE WHERE ARGUMENTS ARE REAL AND FUNCTION IS INTEGER   04780098
C                                                                       04790098
 9241 CONTINUE                                                          04800098
      IVTNUM = 924                                                      04810098
C                                                                       04820098
C      ****  TEST 924  ****                                             04830098
C                                                                       04840098
      IF (ICZERO) 39240, 9240, 39240                                    04850098
 9240 CONTINUE                                                          04860098
      RVON01 = 1.1111                                                   04870098
      RVON02 = 22.222                                                   04880098
      RVON03 = 333.33                                                   04890098
      IVCOMP = MIN1 (RVON01,RVON02,RVON03)                              04900098
      GO TO 49240                                                       04910098
39240 IVDELE = IVDELE + 1                                               04920098
      WRITE (I02,80003) IVTNUM                                          04930098
      IF (ICZERO) 49240, 9251, 49240                                    04940098
49240 IF (IVCOMP - 1) 29240,19240,29240                                 04950098
19240 IVPASS = IVPASS + 1                                               04960098
      WRITE (I02,80001) IVTNUM                                          04970098
      GO TO 9251                                                        04980098
29240 IVFAIL = IVFAIL + 1                                               04990098
      IVCORR = 1                                                        05000098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05010098
 9251 CONTINUE                                                          05020098
      IVTNUM = 925                                                      05030098
C                                                                       05040098
C      ****  TEST 925  ****                                             05050098
C                                                                       05060098
      IF (ICZERO) 39250, 9250, 39250                                    05070098
 9250 CONTINUE                                                          05080098
      RVON01 = 28.8                                                     05090098
      RVON02 = 2.88E1                                                   05100098
      RVON03 = 288E-1                                                   05110098
      RVON04 = 35.0                                                     05120098
      IVCOMP = MIN1 (RVON01,RVON02,RVON03,RVON04)                       05130098
      GO TO 49250                                                       05140098
39250 IVDELE = IVDELE + 1                                               05150098
      WRITE (I02,80003) IVTNUM                                          05160098
      IF (ICZERO) 49250, 9261, 49250                                    05170098
49250 IF (IVCOMP - 28) 29250,19250,29250                                05180098
19250 IVPASS = IVPASS + 1                                               05190098
      WRITE (I02,80001) IVTNUM                                          05200098
      GO TO 9261                                                        05210098
29250 IVFAIL = IVFAIL + 1                                               05220098
      IVCORR = 28                                                       05230098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05240098
C                                                                       05250098
C     TEST 926 THROUGH TEST 929 CONTAIN THE INTRINSIC FUNCTION FIX      05260098
C     WHICH CONVERTS REAL ARGUMENTS TO INTEGER FUNCTION RESULTS         05270098
C                                                                       05280098
 9261 CONTINUE                                                          05290098
      IVTNUM = 926                                                      05300098
C                                                                       05310098
C      ****  TEST 926  ****                                             05320098
C                                                                       05330098
      IF (ICZERO) 39260, 9260, 39260                                    05340098
 9260 CONTINUE                                                          05350098
      IVCOMP = IFIX (-6.06)                                             05360098
      GO TO 49260                                                       05370098
39260 IVDELE = IVDELE + 1                                               05380098
      WRITE (I02,80003) IVTNUM                                          05390098
      IF (ICZERO) 49260, 9271, 49260                                    05400098
49260 IF (IVCOMP + 6) 29260,19260,29260                                 05410098
19260 IVPASS = IVPASS + 1                                               05420098
      WRITE (I02,80001) IVTNUM                                          05430098
      GO TO 9271                                                        05440098
29260 IVFAIL = IVFAIL + 1                                               05450098
      IVCORR = -6                                                       05460098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05470098
 9271 CONTINUE                                                          05480098
      IVTNUM = 927                                                      05490098
C                                                                       05500098
C      ****  TEST 927  ****                                             05510098
C                                                                       05520098
      IF (ICZERO) 39270, 9270, 39270                                    05530098
 9270 CONTINUE                                                          05540098
      RVON01 = 71.01                                                    05550098
      IVCOMP = IFIX (RVON01)                                            05560098
      GO TO 49270                                                       05570098
39270 IVDELE = IVDELE + 1                                               05580098
      WRITE (I02,80003) IVTNUM                                          05590098
      IF (ICZERO) 49270, 9281, 49270                                    05600098
49270 IF (IVCOMP - 71) 29270,19270,29270                                05610098
19270 IVPASS = IVPASS + 1                                               05620098
      WRITE (I02,80001) IVTNUM                                          05630098
      GO TO 9281                                                        05640098
29270 IVFAIL = IVFAIL + 1                                               05650098
      IVCORR = 71                                                       05660098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05670098
 9281 CONTINUE                                                          05680098
      IVTNUM = 928                                                      05690098
C                                                                       05700098
C      ****  TEST 928  ****                                             05710098
C                                                                       05720098
      IF (ICZERO) 39280, 9280, 39280                                    05730098
 9280 CONTINUE                                                          05740098
      RVON01 = 3.211E2                                                  05750098
      IVCOMP = IFIX (RVON01)                                            05760098
      GO TO 49280                                                       05770098
39280 IVDELE = IVDELE + 1                                               05780098
      WRITE (I02,80003) IVTNUM                                          05790098
      IF (ICZERO) 49280, 9291, 49280                                    05800098
49280 IF (IVCOMP - 321) 29280,19280,29280                               05810098
19280 IVPASS = IVPASS + 1                                               05820098
      WRITE (I02,80001) IVTNUM                                          05830098
      GO TO 9291                                                        05840098
29280 IVFAIL = IVFAIL + 1                                               05850098
      IVCORR = 321                                                      05860098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05870098
 9291 CONTINUE                                                          05880098
      IVTNUM = 929                                                      05890098
C                                                                       05900098
C      ****  TEST 929  ****                                             05910098
C                                                                       05920098
      IF (ICZERO) 39290, 9290, 39290                                    05930098
 9290 CONTINUE                                                          05940098
      RVON01 = 777E-1                                                   05950098
      IVCOMP = IFIX (RVON01)                                            05960098
      GO TO 49290                                                       05970098
39290 IVDELE = IVDELE + 1                                               05980098
      WRITE (I02,80003) IVTNUM                                          05990098
      IF (ICZERO) 49290, 9301, 49290                                    06000098
49290 IF (IVCOMP - 77) 29290,19290,29290                                06010098
19290 IVPASS = IVPASS + 1                                               06020098
      WRITE (I02,80001) IVTNUM                                          06030098
      GO TO 9301                                                        06040098
29290 IVFAIL = IVFAIL + 1                                               06050098
      IVCORR = 77                                                       06060098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06070098
C                                                                       06080098
C     TEST 930 THROUGH TEST 932 CONTAIN INTRINSIC FUNCTION TESTS FOR    06090098
C     TRANSFER OF SIGN WHERE ARGUMENTS AND FUNCTION ARE INTEGER         06100098
C                                                                       06110098
 9301 CONTINUE                                                          06120098
      IVTNUM = 930                                                      06130098
C                                                                       06140098
C      ****  TEST 930  ****                                             06150098
C                                                                       06160098
      IF (ICZERO) 39300, 9300, 39300                                    06170098
 9300 CONTINUE                                                          06180098
      IVON01 = 643                                                      06190098
      IVCOMP = ISIGN (IVON01,-1)                                        06200098
      GO TO 49300                                                       06210098
39300 IVDELE = IVDELE + 1                                               06220098
      WRITE (I02,80003) IVTNUM                                          06230098
      IF (ICZERO) 49300, 9311, 49300                                    06240098
49300 IF (IVCOMP + 643) 29300,19300,29300                               06250098
19300 IVPASS = IVPASS + 1                                               06260098
      WRITE (I02,80001) IVTNUM                                          06270098
      GO TO 9311                                                        06280098
29300 IVFAIL = IVFAIL + 1                                               06290098
      IVCORR = -643                                                     06300098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06310098
 9311 CONTINUE                                                          06320098
      IVTNUM = 931                                                      06330098
C                                                                       06340098
C      ****  TEST 931  ****                                             06350098
C                                                                       06360098
      IF (ICZERO) 39310, 9310, 39310                                    06370098
 9310 CONTINUE                                                          06380098
      IVON01 = -22                                                      06390098
      IVON02 = 723                                                      06400098
      IVCOMP = ISIGN (IVON01,IVON02)                                    06410098
      GO TO 49310                                                       06420098
39310 IVDELE = IVDELE + 1                                               06430098
      WRITE (I02,80003) IVTNUM                                          06440098
      IF (ICZERO) 49310, 9321, 49310                                    06450098
49310 IF (IVCOMP - 22) 29310,19310,29310                                06460098
19310 IVPASS = IVPASS + 1                                               06470098
      WRITE (I02,80001) IVTNUM                                          06480098
      GO TO 9321                                                        06490098
29310 IVFAIL = IVFAIL + 1                                               06500098
      IVCORR = 22                                                       06510098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06520098
 9321 CONTINUE                                                          06530098
      IVTNUM = 932                                                      06540098
C                                                                       06550098
C      ****  TEST 932  ****                                             06560098
C                                                                       06570098
      IF (ICZERO) 39320, 9320, 39320                                    06580098
 9320 CONTINUE                                                          06590098
      IVON01 = 3532                                                     06600098
      IVON02 = 1                                                        06610098
      IVCOMP = ISIGN (IVON01,IVON02)                                    06620098
      GO TO 49320                                                       06630098
39320 IVDELE = IVDELE + 1                                               06640098
      WRITE (I02,80003) IVTNUM                                          06650098
      IF (ICZERO) 49320, 9331, 49320                                    06660098
49320 IF (IVCOMP - 3532) 29320,19320,29320                              06670098
19320 IVPASS = IVPASS + 1                                               06680098
      WRITE (I02,80001) IVTNUM                                          06690098
      GO TO 9331                                                        06700098
29320 IVFAIL = IVFAIL + 1                                               06710098
      IVCORR = 3532                                                     06720098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06730098
C                                                                       06740098
C     TEST 933 THROUGH TEST 936 CONTAIN INTRINSIC FUNCTION TESTS FOR    06750098
C     POSITIVE DIFFERENCE WHERE ARGUMENTS AND FUNCTION ARE INTEGERS     06760098
C                                                                       06770098
 9331 CONTINUE                                                          06780098
      IVTNUM = 933                                                      06790098
C                                                                       06800098
C      ****  TEST 933  ****                                             06810098
C                                                                       06820098
      IF (ICZERO) 39330, 9330, 39330                                    06830098
 9330 CONTINUE                                                          06840098
      IVON01 = 222                                                      06850098
      IVCOMP = IDIM (IVON01,1)                                          06860098
      GO TO 49330                                                       06870098
39330 IVDELE = IVDELE + 1                                               06880098
      WRITE (I02,80003) IVTNUM                                          06890098
      IF (ICZERO) 49330, 9341, 49330                                    06900098
49330 IF (IVCOMP - 221) 29330,19330,29330                               06910098
19330 IVPASS = IVPASS + 1                                               06920098
      WRITE (I02,80001) IVTNUM                                          06930098
      GO TO 9341                                                        06940098
29330 IVFAIL = IVFAIL + 1                                               06950098
      IVCORR = 221                                                      06960098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06970098
 9341 CONTINUE                                                          06980098
      IVTNUM = 934                                                      06990098
C                                                                       07000098
C      ****  TEST 934  ****                                             07010098
C                                                                       07020098
      IF (ICZERO) 39340, 9340, 39340                                    07030098
 9340 CONTINUE                                                          07040098
      IVON01 = 45                                                       07050098
      IVON02 = 41                                                       07060098
      IVCOMP = IDIM (IVON01,IVON02)                                     07070098
      GO TO 49340                                                       07080098
39340 IVDELE = IVDELE + 1                                               07090098
      WRITE (I02,80003) IVTNUM                                          07100098
      IF (ICZERO) 49340, 9351, 49340                                    07110098
49340 IF (IVCOMP - 4) 29340,19340,29340                                 07120098
19340 IVPASS = IVPASS + 1                                               07130098
      WRITE (I02,80001) IVTNUM                                          07140098
      GO TO 9351                                                        07150098
29340 IVFAIL = IVFAIL + 1                                               07160098
      IVCORR = 4                                                        07170098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07180098
 9351 CONTINUE                                                          07190098
      IVTNUM = 935                                                      07200098
C                                                                       07210098
C      ****  TEST 935  ****                                             07220098
C                                                                       07230098
      IF (ICZERO) 39350, 9350, 39350                                    07240098
 9350 CONTINUE                                                          07250098
      IVON01 = 2                                                        07260098
      IVON02 = 10                                                       07270098
      IVCOMP = IDIM (IVON01,IVON02)                                     07280098
      GO TO 49350                                                       07290098
39350 IVDELE = IVDELE + 1                                               07300098
      WRITE (I02,80003) IVTNUM                                          07310098
      IF (ICZERO) 49350, 9361, 49350                                    07320098
49350 IF (IVCOMP) 29350,19350,29350                                     07330098
19350 IVPASS = IVPASS + 1                                               07340098
      WRITE (I02,80001) IVTNUM                                          07350098
      GO TO 9361                                                        07360098
29350 IVFAIL = IVFAIL + 1                                               07370098
      IVCORR = 0                                                        07380098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07390098
 9361 CONTINUE                                                          07400098
      IVTNUM = 936                                                      07410098
C                                                                       07420098
C      ****  TEST 936  ****                                             07430098
C                                                                       07440098
      IF (ICZERO) 39360, 9360, 39360                                    07450098
 9360 CONTINUE                                                          07460098
      IVON01 = 165                                                      07470098
      IVON02 = -2                                                       07480098
      IVCOMP = IDIM (IVON01,IVON02)                                     07490098
      GO TO 49360                                                       07500098
39360 IVDELE = IVDELE + 1                                               07510098
      WRITE (I02,80003) IVTNUM                                          07520098
      IF (ICZERO) 49360, 9371, 49360                                    07530098
49360 IF (IVCOMP - 167) 29360,19360,29360                               07540098
19360 IVPASS = IVPASS + 1                                               07550098
      WRITE (I02,80001) IVTNUM                                          07560098
      GO TO 9371                                                        07570098
29360 IVFAIL = IVFAIL + 1                                               07580098
      IVCORR = 167                                                      07590098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07600098
C                                                                       07610098
C     TESTS 937 AND 938 CONTAIN EXPRESSIONS CONTAINING MORE THAN ONE    07620098
C     INTRINSIC FUNCTION - THE FUNCTIONS ARE INTEGER AND THE ARGUMENTS  07630098
C     ARE REAL AND INTEGER                                              07640098
C                                                                       07650098
 9371 CONTINUE                                                          07660098
      IVTNUM = 937                                                      07670098
C                                                                       07680098
C      ****  TEST 937  ****                                             07690098
C                                                                       07700098
      IF (ICZERO) 39370, 9370, 39370                                    07710098
 9370 CONTINUE                                                          07720098
      RVON01 = 33.3                                                     07730098
      IVON01 = -12                                                      07740098
      IVCOMP = INT (RVON01) + IABS (IVON01)                             07750098
      GO TO 49370                                                       07760098
39370 IVDELE = IVDELE + 1                                               07770098
      WRITE (I02,80003) IVTNUM                                          07780098
      IF (ICZERO) 49370, 9381, 49370                                    07790098
49370 IF (IVCOMP -  45) 29370,19370,29370                               07800098
19370 IVPASS = IVPASS + 1                                               07810098
      WRITE (I02,80001) IVTNUM                                          07820098
      GO TO 9381                                                        07830098
29370 IVFAIL = IVFAIL + 1                                               07840098
      IVCORR = 45                                                       07850098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07860098
 9381 CONTINUE                                                          07870098
      IVTNUM = 938                                                      07880098
C                                                                       07890098
C      ****  TEST 938  ****                                             07900098
C                                                                       07910098
      IF (ICZERO) 39380, 9380, 39380                                    07920098
 9380 CONTINUE                                                          07930098
      IVON01 = 76                                                       07940098
      IVON02 = 21                                                       07950098
      IVON03 = 30                                                       07960098
      IVCOMP = MAX0 (IVON01,IVON02,IVON03) - MIN0 (IVON01,IVON02,IVON03)07970098
      GO TO 49380                                                       07980098
39380 IVDELE = IVDELE + 1                                               07990098
      WRITE (I02,80003) IVTNUM                                          08000098
      IF (ICZERO) 49380, 9391, 49380                                    08010098
49380 IF (IVCOMP - 55) 29380,19380,29380                                08020098
19380 IVPASS = IVPASS + 1                                               08030098
      WRITE (I02,80001) IVTNUM                                          08040098
      GO TO 9391                                                        08050098
29380 IVFAIL = IVFAIL + 1                                               08060098
      IVCORR = 55                                                       08070098
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          08080098
 9391 CONTINUE                                                          08090098
C                                                                       08100098
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08110098
99999 CONTINUE                                                          08120098
      WRITE (I02,90002)                                                 08130098
      WRITE (I02,90006)                                                 08140098
      WRITE (I02,90002)                                                 08150098
      WRITE (I02,90002)                                                 08160098
      WRITE (I02,90007)                                                 08170098
      WRITE (I02,90002)                                                 08180098
      WRITE (I02,90008)  IVFAIL                                         08190098
      WRITE (I02,90009) IVPASS                                          08200098
      WRITE (I02,90010) IVDELE                                          08210098
C                                                                       08220098
C                                                                       08230098
C     TERMINATE ROUTINE EXECUTION                                       08240098
      STOP                                                              08250098
C                                                                       08260098
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08270098
90000 FORMAT (1H1)                                                      08280098
90002 FORMAT (1H )                                                      08290098
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08300098
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08310098
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08320098
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08330098
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08340098
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08350098
C                                                                       08360098
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08370098
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08380098
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08390098
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08400098
C                                                                       08410098
C     FORMAT STATEMENTS FOR TEST RESULTS                                08420098
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08430098
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08440098
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08450098
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08460098
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08470098
C                                                                       08480098
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM098)                          08490098
      END                                                               08500098
