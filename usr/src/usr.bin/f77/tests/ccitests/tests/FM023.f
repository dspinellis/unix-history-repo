C     COMMENT SECTION.                                                  00010023
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020023
C     FM023                                                             00030023
C                                                                       00040023
C                  TWO DIMENSIONED ARRAYS ARE USED IN THIS ROUTINE.     00050023
C         THIS ROUTINE TESTS ARRAYS WITH FIXED DIMENSION AND SIZE LIMITS00060023
C     SET EITHER IN A BLANK COMMON OR DIMENSION STATEMENT.  THE VALUES  00070023
C     OF THE ARRAY ELEMENTS ARE SET IN VARIOUS WAYS SUCH AS SIMPLE      00080023
C     ASSIGNMENT STATEMENTS, SET TO THE VALUES OF OTHER ARRAY ELEMENTS  00090023
C     (EITHER POSITIVE OR NEGATIVE), SET BY INTEGER TO REAL OR REAL TO  00100023
C     INTEGER CONVERSION, SET BY ARITHMETIC EXPRESSIONS, OR SET BY      00110023
C     USE OF THE  EQUIVALENCE  STATEMENT.                               00120023
C                                                                       00130023
C                                                                       00140023
C      REFERENCES                                                       00150023
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00160023
C              X3.9-1978                                                00170023
C                                                                       00180023
C        SECTION 8, SPECIFICATION STATEMENTS                            00190023
C        SECTION 8.1, DIMENSION STATEMENT                               00200023
C        SECTION 8.2, EQUIVALENCE STATEMENT                             00210023
C        SECTION 8.3, COMMON STATEMENT                                  00220023
C        SECTION 8.4, TYPE-STATEMENTS                                   00230023
C        SECTION 9, DATA STATEMENT                                      00240023
C                                                                       00250023
      COMMON IADN22(2,2), RADN22(2,2), ICOE01, RCOE01                   00260023
      DIMENSION IADN21(2,2), RADN21(2,2)                                00270023
      DIMENSION IADE23(2,2), IADE24(2,2), RADE23(2,2), RADE24(2,2)      00280023
      EQUIVALENCE (IADE23(2,2),IADN22(2,2),IADE24(2,2))                 00290023
      EQUIVALENCE (RADE23(2,2),RADN22(2,2),RADE24(2,2))                 00300023
      EQUIVALENCE (ICOE01,ICOE02,ICOE03,ICOE04), (RCOE01,RCOE02,RCOE03) 00310023
      INTEGER RADN11(2), RADN25(2,2)                                    00320023
      LOGICAL LADN21(2,2)                                               00330023
      DATA RADN21(2,2)/-512./                                           00340023
      DATA LADN21/4*.TRUE./                                             00350023
C                                                                       00360023
C      **********************************************************       00370023
C                                                                       00380023
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00390023
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00400023
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00410023
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00420023
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00430023
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00440023
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00450023
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00460023
C     OF EXECUTING THESE TESTS.                                         00470023
C                                                                       00480023
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00490023
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00500023
C                                                                       00510023
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00520023
C                                                                       00530023
C                  DEPARTMENT OF THE NAVY                               00540023
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00550023
C                  WASHINGTON, D.C.  20376                              00560023
C                                                                       00570023
C      **********************************************************       00580023
C                                                                       00590023
C                                                                       00600023
C                                                                       00610023
C     INITIALIZATION SECTION                                            00620023
C                                                                       00630023
C     INITIALIZE CONSTANTS                                              00640023
C      **************                                                   00650023
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660023
      I01 = 5                                                           00670023
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680023
      I02 = 6                                                           00690023
C     SYSTEM ENVIRONMENT SECTION                                        00700023
C                                                                       00710023
      I01 = 5                                                           00720023
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00730023
C     (UNIT NUMBER FOR CARD READER).                                    00740023
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00750023
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760023
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00770023
C                                                                       00780023
      I02 = 6                                                           00790023
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00800023
C     (UNIT NUMBER FOR PRINTER).                                        00810023
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00820023
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00830023
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00840023
C                                                                       00850023
      IVPASS=0                                                          00860023
      IVFAIL=0                                                          00870023
      IVDELE=0                                                          00880023
      ICZERO=0                                                          00890023
C                                                                       00900023
C     WRITE PAGE HEADERS                                                00910023
      WRITE (I02,90000)                                                 00920023
      WRITE (I02,90001)                                                 00930023
      WRITE (I02,90002)                                                 00940023
      WRITE (I02, 90002)                                                00950023
      WRITE (I02,90003)                                                 00960023
      WRITE (I02,90002)                                                 00970023
      WRITE (I02,90004)                                                 00980023
      WRITE (I02,90002)                                                 00990023
      WRITE (I02,90011)                                                 01000023
      WRITE (I02,90002)                                                 01010023
      WRITE (I02,90002)                                                 01020023
      WRITE (I02,90005)                                                 01030023
      WRITE (I02,90006)                                                 01040023
      WRITE (I02,90002)                                                 01050023
      IVTNUM = 632                                                      01060023
C                                                                       01070023
C      ****  TEST 632  ****                                             01080023
C     TEST 632  -  TESTS SETTING AN INTEGER ARRAY ELEMENT BY A          01090023
C     SIMPLE ASSIGNMENT STATEMENT TO THE VALUE 9999.                    01100023
C                                                                       01110023
      IF (ICZERO) 36320, 6320, 36320                                    01120023
 6320 CONTINUE                                                          01130023
      IADN21(1,1) = 9999                                                01140023
      IVCOMP = IADN21(1,1)                                              01150023
      GO TO 46320                                                       01160023
36320 IVDELE = IVDELE + 1                                               01170023
      WRITE (I02,80003) IVTNUM                                          01180023
      IF (ICZERO) 46320, 6331, 46320                                    01190023
46320 IF ( IVCOMP - 9999 )  26320, 16320, 26320                         01200023
16320 IVPASS = IVPASS + 1                                               01210023
      WRITE (I02,80001) IVTNUM                                          01220023
      GO TO 6331                                                        01230023
26320 IVFAIL = IVFAIL + 1                                               01240023
      IVCORR = 9999                                                     01250023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01260023
 6331 CONTINUE                                                          01270023
      IVTNUM = 633                                                      01280023
C                                                                       01290023
C      ****  TEST 633  ****                                             01300023
C     TEST 633  -  TESTS SETTING A REAL ARRAY ELEMENT BY A SIMPLE       01310023
C     ASSIGNMENT STATEMENT TO THE VALUE -32766.                         01320023
C                                                                       01330023
      IF (ICZERO) 36330, 6330, 36330                                    01340023
 6330 CONTINUE                                                          01350023
      RADN21(1,2) = -32766.                                             01360023
      IVCOMP = RADN21(1,2)                                              01370023
      GO TO 46330                                                       01380023
36330 IVDELE = IVDELE + 1                                               01390023
      WRITE (I02,80003) IVTNUM                                          01400023
      IF (ICZERO) 46330, 6341, 46330                                    01410023
46330 IF ( IVCOMP + 32766 )  26330, 16330, 26330                        01420023
16330 IVPASS = IVPASS + 1                                               01430023
      WRITE (I02,80001) IVTNUM                                          01440023
      GO TO 6341                                                        01450023
26330 IVFAIL = IVFAIL + 1                                               01460023
      IVCORR = -32766                                                   01470023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01480023
 6341 CONTINUE                                                          01490023
      IVTNUM = 634                                                      01500023
C                                                                       01510023
C      ****  TEST 634  ****                                             01520023
C     TEST 634  -  TEST OF THE DATA INITIALIZATION STATEMENT AND SETTING01530023
C     AN INTEGER ARRAY ELEMENT EQUAL TO THE VALUE OF A REAL ARRAY       01540023
C     ELEMENT.  THE VALUE USED IS -512.                                 01550023
C                                                                       01560023
      IF (ICZERO) 36340, 6340, 36340                                    01570023
 6340 CONTINUE                                                          01580023
      IADN21(2,2) = RADN21(2,2)                                         01590023
      IVCOMP = IADN21(2,2)                                              01600023
      GO TO 46340                                                       01610023
36340 IVDELE = IVDELE + 1                                               01620023
      WRITE (I02,80003) IVTNUM                                          01630023
      IF (ICZERO) 46340, 6351, 46340                                    01640023
46340 IF ( IVCOMP + 512 )  26340, 16340, 26340                          01650023
16340 IVPASS = IVPASS + 1                                               01660023
      WRITE (I02,80001) IVTNUM                                          01670023
      GO TO 6351                                                        01680023
26340 IVFAIL = IVFAIL + 1                                               01690023
      IVCORR = -512                                                     01700023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01710023
 6351 CONTINUE                                                          01720023
      IVTNUM = 635                                                      01730023
C                                                                       01740023
C      ****  TEST 635  ****                                             01750023
C     TEST 635  -  TEST OF SETTING A TWO DIMENSIONED ARRAY ELEMENT      01760023
C     EQUAL TO THE VALUE OF A ONE DIMENSIONED ARRAY ELEMENT.            01770023
C     BOTH ARRAYS ARE SET INTEGER BY THE TYPE STATEMENT AND THE TWO     01780023
C     DIMENSIONED ARRAY ELEMENT IS MINUS THE VALUE OF THE ONE DIMENSION 01790023
C     ELEMENT.  THE VALUE USED IS 3.                                    01800023
C                                                                       01810023
      IF (ICZERO) 36350, 6350, 36350                                    01820023
 6350 CONTINUE                                                          01830023
      RADN11(1) = 3                                                     01840023
      RADN25(2,2) = - RADN11(1)                                         01850023
      IVCOMP = RADN25(2,2)                                              01860023
      GO TO 46350                                                       01870023
36350 IVDELE = IVDELE + 1                                               01880023
      WRITE (I02,80003) IVTNUM                                          01890023
      IF (ICZERO) 46350, 6361, 46350                                    01900023
46350 IF ( IVCOMP + 3 )  26350, 16350, 26350                            01910023
16350 IVPASS = IVPASS + 1                                               01920023
      WRITE (I02,80001) IVTNUM                                          01930023
      GO TO 6361                                                        01940023
26350 IVFAIL = IVFAIL + 1                                               01950023
      IVCORR = -3                                                       01960023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01970023
 6361 CONTINUE                                                          01980023
      IVTNUM = 636                                                      01990023
C                                                                       02000023
C      ****  TEST 636  ****                                             02010023
C     TEST 636  -  TEST OF LOGICAL ARRAY ELEMENTS SET BY DATA STATEMENTS02020023
C                                                                       02030023
      IF (ICZERO) 36360, 6360, 36360                                    02040023
 6360 CONTINUE                                                          02050023
      ICON01 = 0                                                        02060023
      IF ( LADN21(2,1) )  ICON01 = 1                                    02070023
      GO TO 46360                                                       02080023
36360 IVDELE = IVDELE + 1                                               02090023
      WRITE (I02,80003) IVTNUM                                          02100023
      IF (ICZERO) 46360, 6371, 46360                                    02110023
46360 IF ( ICON01 - 1 )  26360, 16360, 26360                            02120023
16360 IVPASS = IVPASS + 1                                               02130023
      WRITE (I02,80001) IVTNUM                                          02140023
      GO TO 6371                                                        02150023
26360 IVFAIL = IVFAIL + 1                                               02160023
      IVCOMP = ICON01                                                   02170023
      IVCORR = 1                                                        02180023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02190023
 6371 CONTINUE                                                          02200023
      IVTNUM = 637                                                      02210023
C                                                                       02220023
C      ****  TEST 637  ****                                             02230023
C     TEST 637  -  TEST OF REAL TO INTEGER CONVERSION AND SETTING       02240023
C     INTEGER ARRAY ELEMENTS TO THE VALUE OBTAINED IN AN ARITHMETIC     02250023
C     EXPRESSION USING REAL ARRAY ELEMENTS.   .5  +  .5  =  1           02260023
C                                                                       02270023
      IF (ICZERO) 36370, 6370, 36370                                    02280023
 6370 CONTINUE                                                          02290023
      RADN21(1,2) = 00000.5                                             02300023
      RADN21(2,1) = .500000                                             02310023
      IADN21(2,1) = RADN21(1,2) + RADN21(2,1)                           02320023
      IVCOMP = IADN21(2,1)                                              02330023
      GO TO 46370                                                       02340023
36370 IVDELE = IVDELE + 1                                               02350023
      WRITE (I02,80003) IVTNUM                                          02360023
      IF (ICZERO) 46370, 6381, 46370                                    02370023
46370 IF ( IVCOMP - 1 )  26370, 16370, 26370                            02380023
16370 IVPASS = IVPASS + 1                                               02390023
      WRITE (I02,80001) IVTNUM                                          02400023
      GO TO 6381                                                        02410023
26370 IVFAIL = IVFAIL + 1                                               02420023
      IVCORR = 1                                                        02430023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02440023
 6381 CONTINUE                                                          02450023
      IVTNUM = 638                                                      02460023
C                                                                       02470023
C      ****  TEST 638  ****                                             02480023
C     TEST 638  -  TEST OF EQUIVALENCE OF THREE INTEGER ARRAYS ONE OF   02490023
C     WHICH IS IN COMMON.                                               02500023
C                                                                       02510023
      IF (ICZERO) 36380, 6380, 36380                                    02520023
 6380 CONTINUE                                                          02530023
      IADN22(2,1) = -9999                                               02540023
      IVCOMP = IADE23(2,1)                                              02550023
      GO TO 46380                                                       02560023
36380 IVDELE = IVDELE + 1                                               02570023
      WRITE (I02,80003) IVTNUM                                          02580023
      IF (ICZERO) 46380, 6391, 46380                                    02590023
46380 IF ( IVCOMP + 9999 )  26380, 16380, 26380                         02600023
16380 IVPASS = IVPASS + 1                                               02610023
      WRITE (I02,80001) IVTNUM                                          02620023
      GO TO 6391                                                        02630023
26380 IVFAIL = IVFAIL + 1                                               02640023
      IVCORR = -9999                                                    02650023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02660023
 6391 CONTINUE                                                          02670023
      IVTNUM = 639                                                      02680023
C                                                                       02690023
C      ****  TEST 639  ****                                             02700023
C     TEST 639  -  LIKE TEST 638 ONLY THE OTHER EQUIVALENCED ARRAY IS   02710023
C     TESTED FOR THE VALUE -9999.                                       02720023
C                                                                       02730023
      IF (ICZERO) 36390, 6390, 36390                                    02740023
 6390 CONTINUE                                                          02750023
      IADE23(2,1) = -9999                                               02760023
      IVCOMP = IADE24(2,1)                                              02770023
      GO TO 46390                                                       02780023
36390 IVDELE = IVDELE + 1                                               02790023
      WRITE (I02,80003) IVTNUM                                          02800023
      IF (ICZERO) 46390, 6401, 46390                                    02810023
46390 IF ( IVCOMP + 9999 )  26390, 16390, 26390                         02820023
16390 IVPASS = IVPASS + 1                                               02830023
      WRITE (I02,80001) IVTNUM                                          02840023
      GO TO 6401                                                        02850023
26390 IVFAIL = IVFAIL + 1                                               02860023
      IVCORR = -9999                                                    02870023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02880023
 6401 CONTINUE                                                          02890023
      IVTNUM = 640                                                      02900023
C                                                                       02910023
C      ****  TEST 640  ****                                             02920023
C     TEST 640  -  TEST OF THREE REAL ARRAYS THAT ARE EQUIVALENCED.     02930023
C     ONE OF THE ARRAYS IS IN COMMON.  THE VALUE 512 IS SET INTO ONE OF 02940023
C     THE DIMENSIONED ARRAY ELEMENTS BY AN INTEGER TO REAL CONVERSION   02950023
C     ASSIGNMENT STATEMENT.                                             02960023
C                                                                       02970023
      IF (ICZERO) 36400, 6400, 36400                                    02980023
 6400 CONTINUE                                                          02990023
      RADE24(2,2) = 512                                                 03000023
      IVCOMP = RADN22(2,2)                                              03010023
      GO TO 46400                                                       03020023
36400 IVDELE = IVDELE + 1                                               03030023
      WRITE (I02,80003) IVTNUM                                          03040023
      IF (ICZERO) 46400, 6411, 46400                                    03050023
46400 IF ( IVCOMP - 512 )  26400, 16400, 26400                          03060023
16400 IVPASS = IVPASS + 1                                               03070023
      WRITE (I02,80001) IVTNUM                                          03080023
      GO TO 6411                                                        03090023
26400 IVFAIL = IVFAIL + 1                                               03100023
      IVCORR = 512                                                      03110023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03120023
 6411 CONTINUE                                                          03130023
      IVTNUM = 641                                                      03140023
C                                                                       03150023
C      ****  TEST 641  ****                                             03160023
C     TEST 641  -  LIKE TEST 640 ONLY THE OTHER EQUIVALENCED ARRAY IS   03170023
C     TESTED FOR THE VALUE 512.                                         03180023
C                                                                       03190023
      IF (ICZERO) 36410, 6410, 36410                                    03200023
 6410 CONTINUE                                                          03210023
      RADN22(2,2) = 512                                                 03220023
      IVCOMP = RADE23(2,2)                                              03230023
      GO TO 46410                                                       03240023
36410 IVDELE = IVDELE + 1                                               03250023
      WRITE (I02,80003) IVTNUM                                          03260023
      IF (ICZERO) 46410, 6421, 46410                                    03270023
46410 IF ( IVCOMP - 512 )  26410, 16410, 26410                          03280023
16410 IVPASS = IVPASS + 1                                               03290023
      WRITE (I02,80001) IVTNUM                                          03300023
      GO TO 6421                                                        03310023
26410 IVFAIL = IVFAIL + 1                                               03320023
      IVCORR = 512                                                      03330023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03340023
 6421 CONTINUE                                                          03350023
      IVTNUM = 642                                                      03360023
C                                                                       03370023
C      ****  TEST 642  ****                                             03380023
C     TEST 642  -  TEST OF FOUR INTEGER VARIABLES THAT ARE EQUIVALENCED.03390023
C     ONE OF THE INTEGER VARIABLES IS IN BLANK COMMON.  THE VALUE USED  03400023
C     IS 3 SET  BY AN ASSIGNMENT STATEMENT.                             03410023
C                                                                       03420023
      IF (ICZERO) 36420, 6420, 36420                                    03430023
 6420 CONTINUE                                                          03440023
      ICOE03 = 3                                                        03450023
      IVCOMP = ICOE01                                                   03460023
      GO TO 46420                                                       03470023
36420 IVDELE = IVDELE + 1                                               03480023
      WRITE (I02,80003) IVTNUM                                          03490023
      IF (ICZERO) 46420, 6431, 46420                                    03500023
46420 IF ( IVCOMP - 3 )  26420, 16420, 26420                            03510023
16420 IVPASS = IVPASS + 1                                               03520023
      WRITE (I02,80001) IVTNUM                                          03530023
      GO TO 6431                                                        03540023
26420 IVFAIL = IVFAIL + 1                                               03550023
      IVCORR = 3                                                        03560023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03570023
 6431 CONTINUE                                                          03580023
      IVTNUM = 643                                                      03590023
C                                                                       03600023
C      ****  TEST 643  ****                                             03610023
C     TEST 643  -  LIKE TEST 642 BUT ANOTHER OF THE ELEMENTS IS TESTED  03620023
C     BY AN ARITHMETIC EXPRESSION USING THE EQUIVALENCED  ELEMENTS.     03630023
C     THE VALUE OF ALL OF THE ELEMENTS SHOULD INITITIALLY BE 3 SINCE    03640023
C     THEY ALL SHOULD SHARE THE SAME STORAGE LOCATION. ICOE04 = 3+3+3+3 03650023
C     ICOE04 = 12  THEN THE ELEMENT ICOE02 IS TESTED FOR THE VALUE 12.  03660023
C                                                                       03670023
      IF (ICZERO) 36430, 6430, 36430                                    03680023
 6430 CONTINUE                                                          03690023
      ICOE01 = 3                                                        03700023
      ICOE04 = ICOE01 + ICOE02 + ICOE03 + ICOE04                        03710023
      IVCOMP = ICOE02                                                   03720023
      GO TO 46430                                                       03730023
36430 IVDELE = IVDELE + 1                                               03740023
      WRITE (I02,80003) IVTNUM                                          03750023
      IF (ICZERO) 46430, 6441, 46430                                    03760023
46430 IF ( IVCOMP - 12 )  26430, 16430, 26430                           03770023
16430 IVPASS = IVPASS + 1                                               03780023
      WRITE (I02,80001) IVTNUM                                          03790023
      GO TO 6441                                                        03800023
26430 IVFAIL = IVFAIL + 1                                               03810023
      IVCORR = 12                                                       03820023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03830023
 6441 CONTINUE                                                          03840023
      IVTNUM = 644                                                      03850023
C                                                                       03860023
C      ****  TEST 644  ****                                             03870023
C     TEST 644  -  TEST OF EQUIVALENCE WITH THREE REAL VARIABLES ONE    03880023
C     OF WHICH IS IN BLANK COMMON.  THE ELEMENTS ARE SET INITIALLY TO .503890023
C     THEN ALL OF THE ELEMENTS ARE USED IN AN ARITHMETIC EXPRESSION     03900023
C     RCOE01 =(.5 + .5 + .5) * 2.   SO RCOE01 = 3.   ELEMENT RCOE02     03910023
C     IS TESTED FOR THE VALUE 3.                                        03920023
C                                                                       03930023
      IF (ICZERO) 36440, 6440, 36440                                    03940023
 6440 CONTINUE                                                          03950023
      RCOE02 = 0.5                                                      03960023
      RCOE01 = ( RCOE01 + RCOE02 + RCOE03 ) * 2.                        03970023
      IVCOMP = RCOE02                                                   03980023
      GO TO 46440                                                       03990023
36440 IVDELE = IVDELE + 1                                               04000023
      WRITE (I02,80003) IVTNUM                                          04010023
      IF (ICZERO) 46440, 6451, 46440                                    04020023
46440 IF ( IVCOMP - 3 )  26440, 16440, 26440                            04030023
16440 IVPASS = IVPASS + 1                                               04040023
      WRITE (I02,80001) IVTNUM                                          04050023
      GO TO 6451                                                        04060023
26440 IVFAIL = IVFAIL + 1                                               04070023
      IVCORR = 3                                                        04080023
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04090023
 6451 CONTINUE                                                          04100023
C                                                                       04110023
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             04120023
99999 CONTINUE                                                          04130023
      WRITE (I02,90002)                                                 04140023
      WRITE (I02,90006)                                                 04150023
      WRITE (I02,90002)                                                 04160023
      WRITE (I02,90002)                                                 04170023
      WRITE (I02,90007)                                                 04180023
      WRITE (I02,90002)                                                 04190023
      WRITE (I02,90008)  IVFAIL                                         04200023
      WRITE (I02,90009) IVPASS                                          04210023
      WRITE (I02,90010) IVDELE                                          04220023
C                                                                       04230023
C                                                                       04240023
C     TERMINATE ROUTINE EXECUTION                                       04250023
      STOP                                                              04260023
C                                                                       04270023
C     FORMAT STATEMENTS FOR PAGE HEADERS                                04280023
90000 FORMAT (1H1)                                                      04290023
90002 FORMAT (1H )                                                      04300023
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04310023
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   04320023
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        04330023
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 04340023
90006 FORMAT (1H ,5X,46H----------------------------------------------) 04350023
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             04360023
C                                                                       04370023
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               04380023
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        04390023
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              04400023
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             04410023
C                                                                       04420023
C     FORMAT STATEMENTS FOR TEST RESULTS                                04430023
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04440023
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      04450023
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   04460023
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04470023
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    04480023
C                                                                       04490023
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM023)                          04500023
      END                                                               04510023
