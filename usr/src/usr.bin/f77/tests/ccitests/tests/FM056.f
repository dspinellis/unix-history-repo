C                                                                       00010056
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020056
C                                                                       00030056
C     FM056                                                             00040056
C                                                                       00050056
C          FM056 IS A MAIN WHICH TESTS THE ARGUMENT PASSING LINKAGE OF  00060056
C     A 2 LEVEL NESTED SUBROUTINE AND AN EXTERNAL FUNCTION REFERENCE.   00070056
C     THE MAIN PROGRAM FM056 CALLS SUBROUTINE FS057 PASSING ONE         00080056
C     ARGUMENT.  SUBROUTINE FS057 CALLS SUBROUTINE FS058 PASSING TWO    00090056
C     ARGUMENTS.  SUBROUTINE FS058 REFERENCES EXTERNAL FUNCTION FF059   00100056
C     PASSING 3 ARGUMENTS.  FUNCTION FF059 ADDS THE VALUES OF THE 3     00110056
C     ARGUMENTS TOGETHER.  SUBROUTINE FS057 AND FS058 THEN MERELY       00120056
C     RETURN THE RESULT TO FM056 IN THE FIRST ARGUMENT.                 00130056
C                                                                       00140056
C          THE VALUES OF THE ARGUMENTS THAT ARE PASSED TO EACH          00150056
C     SUBPROGRAM AND FUNCTION, AND RETURNED TO THE CALLING OR           00160056
C     REFERENCING PROGRAM ARE SAVED IN AN INTEGER ARRAY.  FM056 THEN    00170056
C     USES THESE VALUES TO TEST THE COMPILER'S ARGUMENT PASSING         00180056
C     CAPABILITIES.                                                     00190056
C                                                                       00200056
C      REFERENCES                                                       00210056
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00220056
C              X3.9-1978                                                00230056
C                                                                       00240056
C        SECTION 15.6.2, SUBROUTINE REFERENCE                           00250056
      COMMON IACN11 (12)                                                00260056
C                                                                       00270056
C      **********************************************************       00280056
C                                                                       00290056
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00300056
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00310056
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00320056
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00330056
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00340056
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00350056
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00360056
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00370056
C     OF EXECUTING THESE TESTS.                                         00380056
C                                                                       00390056
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00400056
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00410056
C                                                                       00420056
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00430056
C                                                                       00440056
C                  DEPARTMENT OF THE NAVY                               00450056
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00460056
C                  WASHINGTON, D.C.  20376                              00470056
C                                                                       00480056
C      **********************************************************       00490056
C                                                                       00500056
C                                                                       00510056
C                                                                       00520056
C     INITIALIZATION SECTION                                            00530056
C                                                                       00540056
C     INITIALIZE CONSTANTS                                              00550056
C      **************                                                   00560056
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00570056
      I01 = 5                                                           00580056
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00590056
      I02 = 6                                                           00600056
C     SYSTEM ENVIRONMENT SECTION                                        00610056
C                                                                       00620056
      I01 = 5                                                           00630056
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00640056
C     (UNIT NUMBER FOR CARD READER).                                    00650056
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00660056
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00670056
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00680056
C                                                                       00690056
      I02 = 6                                                           00700056
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00710056
C     (UNIT NUMBER FOR PRINTER).                                        00720056
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00730056
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00740056
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00750056
C                                                                       00760056
      IVPASS=0                                                          00770056
      IVFAIL=0                                                          00780056
      IVDELE=0                                                          00790056
      ICZERO=0                                                          00800056
C                                                                       00810056
C     WRITE PAGE HEADERS                                                00820056
      WRITE (I02,90000)                                                 00830056
      WRITE (I02,90001)                                                 00840056
      WRITE (I02,90002)                                                 00850056
      WRITE (I02, 90002)                                                00860056
      WRITE (I02,90003)                                                 00870056
      WRITE (I02,90002)                                                 00880056
      WRITE (I02,90004)                                                 00890056
      WRITE (I02,90002)                                                 00900056
      WRITE (I02,90011)                                                 00910056
      WRITE (I02,90002)                                                 00920056
      WRITE (I02,90002)                                                 00930056
      WRITE (I02,90005)                                                 00940056
      WRITE (I02,90006)                                                 00950056
      WRITE (I02,90002)                                                 00960056
C                                                                       00970056
C     TEST SECTION                                                      00980056
C                                                                       00990056
C         SUBROUTINE SUBPROGRAM                                         01000056
C                                                                       01010056
      IVON01 = 5                                                        01020056
      CALL FS057 (IVON01)                                               01030056
      IACN11 (12) = IVON01                                              01040056
      IVTNUM = 430                                                      01050056
C                                                                       01060056
C      ****  TEST 430  ****                                             01070056
C                                                                       01080056
C     TEST 430 TESTS THE VALUE OF THE ARGUMENT RECEIVED BY FS057 FROM   01090056
C     A FM056 CALL TO FS057                                             01100056
C                                                                       01110056
      IF (ICZERO) 34300, 4300, 34300                                    01120056
 4300 CONTINUE                                                          01130056
      IVCOMP = IACN11 (1)                                               01140056
      GO TO 44300                                                       01150056
34300 IVDELE = IVDELE + 1                                               01160056
      WRITE (I02,80003) IVTNUM                                          01170056
      IF (ICZERO) 44300, 4311, 44300                                    01180056
44300 IF (IVCOMP - 5) 24300,14300,24300                                 01190056
14300 IVPASS = IVPASS + 1                                               01200056
      WRITE (I02,80001) IVTNUM                                          01210056
      GO TO 4311                                                        01220056
24300 IVFAIL = IVFAIL + 1                                               01230056
      IVCORR = 5                                                        01240056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01250056
 4311 CONTINUE                                                          01260056
      IVTNUM = 431                                                      01270056
C                                                                       01280056
C      ****  TEST 431  ****                                             01290056
C                                                                       01300056
C     TEST 431 TESTS THE VALUE OF THE SECOND ARGUMENT THAT WAS PASSED   01310056
C     FROM A FS057 CALL TO FS058                                        01320056
C                                                                       01330056
C                                                                       01340056
      IF (ICZERO) 34310, 4310, 34310                                    01350056
 4310 CONTINUE                                                          01360056
      IVCOMP = IACN11 (2)                                               01370056
      GO TO 44310                                                       01380056
34310 IVDELE = IVDELE + 1                                               01390056
      WRITE (I02,80003) IVTNUM                                          01400056
      IF (ICZERO) 44310, 4321, 44310                                    01410056
44310 IF (IVCOMP - 4) 24310,14310,24310                                 01420056
14310 IVPASS = IVPASS + 1                                               01430056
      WRITE (I02,80001) IVTNUM                                          01440056
      GO TO 4321                                                        01450056
24310 IVFAIL = IVFAIL + 1                                               01460056
      IVCORR = 4                                                        01470056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01480056
 4321 CONTINUE                                                          01490056
      IVTNUM = 432                                                      01500056
C                                                                       01510056
C      ****  TEST 432  ****                                             01520056
C                                                                       01530056
C     TEST 432 TESTS THE VALUE OF THE FIRST ARGUMENT RECEIVED BY FS058  01540056
C     FROM A FS057 CALL TO FS058                                        01550056
C                                                                       01560056
C                                                                       01570056
      IF (ICZERO) 34320, 4320, 34320                                    01580056
 4320 CONTINUE                                                          01590056
      IVCOMP = IACN11 (3)                                               01600056
      GO TO 44320                                                       01610056
34320 IVDELE = IVDELE + 1                                               01620056
      WRITE (I02,80003) IVTNUM                                          01630056
      IF (ICZERO) 44320, 4331, 44320                                    01640056
44320 IF (IVCOMP - 5) 24320,14320,24320                                 01650056
14320 IVPASS = IVPASS + 1                                               01660056
      WRITE (I02,80001) IVTNUM                                          01670056
      GO TO 4331                                                        01680056
24320 IVFAIL = IVFAIL + 1                                               01690056
      IVCORR = 5                                                        01700056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01710056
 4331 CONTINUE                                                          01720056
      IVTNUM = 433                                                      01730056
C                                                                       01740056
C      ****  TEST 433  ****                                             01750056
C                                                                       01760056
C     TEST 433 TESTS THE VALUE OF THE SECOND ARGUMENT RECEIVED BY FS058 01770056
C     FROM A FS057 CALL TO FS058                                        01780056
C                                                                       01790056
C                                                                       01800056
      IF (ICZERO) 34330, 4330, 34330                                    01810056
 4330 CONTINUE                                                          01820056
      IVCOMP = IACN11 (4)                                               01830056
      GO TO 44330                                                       01840056
34330 IVDELE = IVDELE + 1                                               01850056
      WRITE (I02,80003) IVTNUM                                          01860056
      IF (ICZERO) 44330, 4341, 44330                                    01870056
44330 IF (IVCOMP - 4) 24330,14330,24330                                 01880056
14330 IVPASS = IVPASS + 1                                               01890056
      WRITE (I02,80001) IVTNUM                                          01900056
      GO TO 4341                                                        01910056
24330 IVFAIL = IVFAIL + 1                                               01920056
      IVCORR = 4                                                        01930056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01940056
 4341 CONTINUE                                                          01950056
      IVTNUM = 434                                                      01960056
C                                                                       01970056
C      ****  TEST 434  ****                                             01980056
C                                                                       01990056
C     TEST 434 TESTS THE VALUE OF THE THIRD ARGUMENT THAT WAS PASSED    02000056
C     FROM A FS058 REFERENCE OF FUNCTION FF059                          02010056
C                                                                       02020056
C                                                                       02030056
      IF (ICZERO) 34340, 4340, 34340                                    02040056
 4340 CONTINUE                                                          02050056
      IVCOMP = IACN11 (5)                                               02060056
      GO TO 44340                                                       02070056
34340 IVDELE = IVDELE + 1                                               02080056
      WRITE (I02,80003) IVTNUM                                          02090056
      IF (ICZERO) 44340, 4351, 44340                                    02100056
44340 IF (IVCOMP - 3) 24340,14340,24340                                 02110056
14340 IVPASS = IVPASS + 1                                               02120056
      WRITE (I02,80001) IVTNUM                                          02130056
      GO TO 4351                                                        02140056
24340 IVFAIL = IVFAIL + 1                                               02150056
      IVCORR = 3                                                        02160056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02170056
 4351 CONTINUE                                                          02180056
      IVTNUM = 435                                                      02190056
C                                                                       02200056
C      ****  TEST 435  ****                                             02210056
C                                                                       02220056
C     TEST 435 TESTS THE VALUE OF THE FIRST ARGUMENT RECEIVED BY FF059  02230056
C     FROM A FS058 REFERENCE OF FUNCTION FF059                          02240056
C                                                                       02250056
C                                                                       02260056
      IF (ICZERO) 34350, 4350, 34350                                    02270056
 4350 CONTINUE                                                          02280056
      IVCOMP = IACN11 (6)                                               02290056
      GO TO 44350                                                       02300056
34350 IVDELE = IVDELE + 1                                               02310056
      WRITE (I02,80003) IVTNUM                                          02320056
      IF (ICZERO) 44350, 4361, 44350                                    02330056
44350 IF (IVCOMP - 5) 24350,14350,24350                                 02340056
14350 IVPASS = IVPASS + 1                                               02350056
      WRITE (I02,80001) IVTNUM                                          02360056
      GO TO 4361                                                        02370056
24350 IVFAIL = IVFAIL + 1                                               02380056
      IVCORR = 5                                                        02390056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02400056
 4361 CONTINUE                                                          02410056
      IVTNUM = 436                                                      02420056
C                                                                       02430056
C      ****  TEST 436  ****                                             02440056
C                                                                       02450056
C     TEST 436 TESTS THE VALUE OF THE SECOND ARGUMENT RECEIVED BY FF059 02460056
C     FROM A FS058 REFERENCE OF FUNCTION FF059                          02470056
C                                                                       02480056
C                                                                       02490056
      IF (ICZERO) 34360, 4360, 34360                                    02500056
 4360 CONTINUE                                                          02510056
      IVCOMP = IACN11 (7)                                               02520056
      GO TO 44360                                                       02530056
34360 IVDELE = IVDELE + 1                                               02540056
      WRITE (I02,80003) IVTNUM                                          02550056
      IF (ICZERO) 44360, 4371, 44360                                    02560056
44360 IF (IVCOMP - 4) 24360,14360,24360                                 02570056
14360 IVPASS = IVPASS + 1                                               02580056
      WRITE (I02,80001) IVTNUM                                          02590056
      GO TO 4371                                                        02600056
24360 IVFAIL = IVFAIL + 1                                               02610056
      IVCORR = 4                                                        02620056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02630056
 4371 CONTINUE                                                          02640056
      IVTNUM = 437                                                      02650056
C                                                                       02660056
C      ****  TEST 437  ****                                             02670056
C                                                                       02680056
C     TEST 437 TESTS THE VALUE OF THE THIRD ARGUMENT RECEIVED BY FF059  02690056
C     FROM A FS058 REFERENCE OF FUNCTION FF059                          02700056
C                                                                       02710056
C                                                                       02720056
      IF (ICZERO) 34370, 4370, 34370                                    02730056
 4370 CONTINUE                                                          02740056
      IVCOMP = IACN11 (8)                                               02750056
      GO TO 44370                                                       02760056
34370 IVDELE = IVDELE + 1                                               02770056
      WRITE (I02,80003) IVTNUM                                          02780056
      IF (ICZERO) 44370, 4381, 44370                                    02790056
44370 IF (IVCOMP - 3) 24370,14370,24370                                 02800056
14370 IVPASS = IVPASS + 1                                               02810056
      WRITE (I02,80001) IVTNUM                                          02820056
      GO TO 4381                                                        02830056
24370 IVFAIL = IVFAIL + 1                                               02840056
      IVCORR = 3                                                        02850056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02860056
 4381 CONTINUE                                                          02870056
      IVTNUM = 438                                                      02880056
C                                                                       02890056
C      ****  TEST 438  ****                                             02900056
C                                                                       02910056
C     TEST 438 TESTS THE VALUE OF THE FUNCTION DETERMINED BY FF059      02920056
C                                                                       02930056
C                                                                       02940056
      IF (ICZERO) 34380, 4380, 34380                                    02950056
 4380 CONTINUE                                                          02960056
      IVCOMP = IACN11 (9)                                               02970056
      GO TO 44380                                                       02980056
34380 IVDELE = IVDELE + 1                                               02990056
      WRITE (I02,80003) IVTNUM                                          03000056
      IF (ICZERO) 44380, 4391, 44380                                    03010056
44380 IF (IVCOMP - 12) 24380,14380,24380                                03020056
14380 IVPASS = IVPASS + 1                                               03030056
      WRITE (I02,80001) IVTNUM                                          03040056
      GO TO 4391                                                        03050056
24380 IVFAIL = IVFAIL + 1                                               03060056
      IVCORR = 12                                                       03070056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03080056
 4391 CONTINUE                                                          03090056
      IVTNUM = 439                                                      03100056
C                                                                       03110056
C      ****  TEST 439  ****                                             03120056
C                                                                       03130056
C     TEST 439 TESTS THE VALUE OF THE FUNCTION RETURNED TO FS058 BY     03140056
C     FF059                                                             03150056
C                                                                       03160056
C                                                                       03170056
      IF (ICZERO) 34390, 4390, 34390                                    03180056
 4390 CONTINUE                                                          03190056
      IVCOMP = IACN11 (10)                                              03200056
      GO TO 44390                                                       03210056
34390 IVDELE = IVDELE + 1                                               03220056
      WRITE (I02,80003) IVTNUM                                          03230056
      IF (ICZERO) 44390, 4401, 44390                                    03240056
44390 IF (IVCOMP - 12) 24390,14390,24390                                03250056
14390 IVPASS = IVPASS + 1                                               03260056
      WRITE (I02,80001) IVTNUM                                          03270056
      GO TO 4401                                                        03280056
24390 IVFAIL = IVFAIL + 1                                               03290056
      IVCORR = 12                                                       03300056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03310056
 4401 CONTINUE                                                          03320056
      IVTNUM = 440                                                      03330056
C                                                                       03340056
C      ****  TEST 440  ****                                             03350056
C                                                                       03360056
C     TEST 440 TESTS THE VALUE OF THE FIRST ARGUMENT RETURNED TO FS057  03370056
C     BY FS058                                                          03380056
C                                                                       03390056
      IF (ICZERO) 34400, 4400, 34400                                    03400056
 4400 CONTINUE                                                          03410056
      IVCOMP = IACN11 (11)                                              03420056
      GO TO 44400                                                       03430056
34400 IVDELE = IVDELE + 1                                               03440056
      WRITE (I02,80003) IVTNUM                                          03450056
      IF (ICZERO) 44400, 4411, 44400                                    03460056
44400 IF (IVCOMP - 12) 24400,14400,24400                                03470056
14400 IVPASS = IVPASS + 1                                               03480056
      WRITE (I02,80001) IVTNUM                                          03490056
      GO TO 4411                                                        03500056
24400 IVFAIL = IVFAIL + 1                                               03510056
      IVCORR = 12                                                       03520056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03530056
 4411 CONTINUE                                                          03540056
      IVTNUM = 441                                                      03550056
C                                                                       03560056
C      ****  TEST 441  ****                                             03570056
C                                                                       03580056
C     TEST 441 TESTS THE VALUE OF THE FIRST ARGUMENT RETURNED TO FM056  03590056
C     BY FS057                                                          03600056
C                                                                       03610056
C                                                                       03620056
      IF (ICZERO) 34410, 4410, 34410                                    03630056
 4410 CONTINUE                                                          03640056
      IVCOMP = IACN11 (12)                                              03650056
      GO TO 44410                                                       03660056
34410 IVDELE = IVDELE + 1                                               03670056
      WRITE (I02,80003) IVTNUM                                          03680056
      IF (ICZERO) 44410, 4421, 44410                                    03690056
44410 IF (IVCOMP - 12) 24410,14410,24410                                03700056
14410 IVPASS = IVPASS + 1                                               03710056
      WRITE (I02,80001) IVTNUM                                          03720056
      GO TO 4421                                                        03730056
24410 IVFAIL = IVFAIL + 1                                               03740056
      IVCORR = 12                                                       03750056
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03760056
 4421 CONTINUE                                                          03770056
C                                                                       03780056
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03790056
99999 CONTINUE                                                          03800056
      WRITE (I02,90002)                                                 03810056
      WRITE (I02,90006)                                                 03820056
      WRITE (I02,90002)                                                 03830056
      WRITE (I02,90002)                                                 03840056
      WRITE (I02,90007)                                                 03850056
      WRITE (I02,90002)                                                 03860056
      WRITE (I02,90008)  IVFAIL                                         03870056
      WRITE (I02,90009) IVPASS                                          03880056
      WRITE (I02,90010) IVDELE                                          03890056
C                                                                       03900056
C                                                                       03910056
C     TERMINATE ROUTINE EXECUTION                                       03920056
      STOP                                                              03930056
C                                                                       03940056
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03950056
90000 FORMAT (1H1)                                                      03960056
90002 FORMAT (1H )                                                      03970056
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03980056
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03990056
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        04000056
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 04010056
90006 FORMAT (1H ,5X,46H----------------------------------------------) 04020056
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             04030056
C                                                                       04040056
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               04050056
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        04060056
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              04070056
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             04080056
C                                                                       04090056
C     FORMAT STATEMENTS FOR TEST RESULTS                                04100056
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04110056
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      04120056
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   04130056
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04140056
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    04150056
C                                                                       04160056
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM056)                          04170056
      END                                                               04180056
C                                                                       00010057
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020057
C                                                                       00030057
C     FS057                                                             00040057
C                                                                       00050057
C          THIS SUBROUTINE IS CALLED BY THE MAIN PROGRAM FM056.  THE    00060057
C     SINGLE ARGUMENT PASSED FROM FM056 ALONG WITH A SECOND PARAMETER   00070057
C     CREATED IN FS057 ARE THEN PASSED VIA A CALL TO SUBROUTINE FS058.  00080057
C     A RESULT FROM AN ARITHMETIC OPERATION IS RETURNED FROM FS058 IN   00090057
C     THE FIRST ARGUMENT.  FS057 ACCEPTS THIS RESULT AND RETURNS CONTROL00100057
C     TO FM056 WITHOUT ANY ADDITIONAL PROCESSING.                       00110057
C                                                                       00120057
C          THE VALUES OF THE ARGUMENTS THAT ARE PASSED FROM FM056 TO    00130057
C     FS057 AND RETURNED ARE SAVED IN AN INTEGER ARRAY FOR LATER        00140057
C     VERIFICATION BY THE MAIN PROGRAM.                                 00150057
C                                                                       00160057
C      REFERENCES                                                       00170057
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00180057
C              X3.9-1978                                                00190057
C                                                                       00200057
C        SECTION 15.6, SUBROUTINES                                      00210057
C        SECTION 15.6.2, SUBROUTINE REFERENCE                           00220057
C        SECTION 15.8, RETURN STATEMENT                                 00230057
C                                                                       00240057
C     TEST SECTION                                                      00250057
C                                                                       00260057
C         SUBROUTINE SUBPROGRAM                                         00270057
C                                                                       00280057
      SUBROUTINE FS057 (IVON01)                                         00290057
      COMMON IACN11 (12)                                                00300057
      IACN11 (1) = IVON01                                               00310057
      IVON02 = 4                                                        00320057
      IACN11 (2) = IVON02                                               00330057
      CALL FS058 (IVON01,IVON02)                                        00340057
      IACN11 (11) = IVON01                                              00350057
      RETURN                                                            00360057
      END                                                               00370057
C                                                                       00010058
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020058
C                                                                       00030058
C     FS058                                                             00040058
C                                                                       00050058
C          THIS SUBROUTINE IS CALLED BY SUBROUTINE FS057.  THE TWO      00060058
C     ARGUMENTS PASSED FROM FS057 ALONG WITH A THIRD PARAMETER CREATED  00070058
C     IN FS058 ARE THEN PASSED TO FUNCTION FF059 WHERE THEY ARE USED IN 00080058
C     AN ARITHMETIC OPERATION.  FS058 THEN SAVES THE RESULT OF THIS     00090058
C     OPERATION IN THE FIRST ARGUMENT AND RETURNS CONTROL TO FS057      00100058
C     WITHOUT ANY ADDITIONAL PROCESSING.                                00110058
C                                                                       00120058
C          THE VALUES OF THE ARGUMENTS THAT ARE PASSED FROM FS057 TO    00130058
C     FS058 AND RETURNED ARE SAVED IN AN INTEGER ARRAY FOR LATER        00140058
C     VERIFICATION BY THE MAIN PROGRAM.                                 00150058
C                                                                       00160058
C      REFERENCES                                                       00170058
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00180058
C              X3.9-1978                                                00190058
C                                                                       00200058
C        SECTION 15.5.2, REFERENCING EXTERNAL FUNCTIONS                 00210058
C        SECTION 15.6, SUBROUTINES                                      00220058
C        SECTION 15.8, RETURN STATEMENT                                 00230058
C                                                                       00240058
C     TEST SECTION                                                      00250058
C                                                                       00260058
C         SUBROUTINE SUBPROGRAM                                         00270058
C                                                                       00280058
      SUBROUTINE FS058 (IVON01,IVON02)                                  00290058
      COMMON IACN11 (12)                                                00300058
      INTEGER FF059                                                     00310058
      IVON03 = 3                                                        00320058
      IACN11 (3) = IVON01                                               00330058
      IACN11 (4) = IVON02                                               00340058
      IACN11 (5) = IVON03                                               00350058
      IVON01 = FF059 (IVON01,IVON02,IVON03)                             00360058
      IACN11 (10) = IVON01                                              00370058
      RETURN                                                            00380058
      END                                                               00390058
C                                                                       00010059
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020059
C                                                                       00030059
C     FF059                                                             00040059
C                                                                       00050059
C          THIS EXTERNAL FUNCTION IS REFERENCED WITHIN SUBROUTINE FS058.00060059
C     THE THREE ARGUMENTS THAT ARE PASSED ARE SIMPLY ADDED TOGETHER AND 00070059
C     THE RESULT SUBSTITUTED FOR THE ORIGINAL REFERENCE.  CONTROL IS    00080059
C     THEN RETURNED TO FS058.                                           00090059
C                                                                       00100059
C          THE VALUES OF THE ARGUMENTS THAT ARE PASSED FROM FS058 TO    00110059
C     FF059 AND THE RESULT THAT IS RETURNED ARE SAVED IN AN INTEGER     00120059
C     ARRAY FOR LATER VERIFICATION BY THE MAIN PROGRAM.                 00130059
C                                                                       00140059
C      REFERENCES                                                       00150059
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00160059
C              X3.9-1978                                                00170059
C                                                                       00180059
C        SECTION 15.5.1, FUNCTION SUBPROGRAM AND FUNCTION STATEMENT     00190059
C        SECTION 15.8, RETURN STATEMENT                                 00200059
C     TEST SECTION                                                      00210059
C                                                                       00220059
C         FUNCTION SUBPROGRAM                                           00230059
C                                                                       00240059
      INTEGER FUNCTION FF059 (IVON01,IVON02,IVON03)                     00250059
      COMMON IACN11 (12)                                                00260059
      IACN11 (6) = IVON01                                               00270059
      IACN11 (7) = IVON02                                               00280059
      IACN11 (8) = IVON03                                               00290059
      FF059 = IVON01 + IVON02 + IVON03                                  00300059
      IACN11 (9) = IVON01 + IVON02 + IVON03                             00310059
      RETURN                                                            00320059
      END                                                               00330059
