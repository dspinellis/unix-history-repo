C                                                                       00010050
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020050
C                                                                       00030050
C     FM050                                                             00040050
C                                                                       00050050
C          THIS ROUTINE CONTAINS BASIC SUBROUTINE AND FUNCTION REFERENCE00060050
C     TESTS.  FOUR SUBROUTINES AND ONE FUNCTION ARE CALLED OR           00070050
C     REFERENCED.  FS051 IS CALLED TO TEST THE CALLING AND PASSING OF   00080050
C     ARGUMENTS THROUGH UNLABELED COMMON.  NO ARGUMENTS ARE SPECIFIED   00090050
C     IN THE CALL LINE.  FS052 IS IDENTICAL TO FS051 EXCEPT THAT SEVERAL00100050
C     RETURNS ARE USED.  FS053 UTILIZES MANY ARGUMENTS ON THE CALL      00110050
C     STATEMENT AND MANY RETURN STATEMENTS IN THE SUBROUTINE BODY.      00120050
C     FF054 IS A FUNCTION SUBROUTINE IN WHICH MANY ARGUMENTS AND RETURN 00130050
C     STATEMENTS ARE USED.  AND FINALLY FS055 PASSES A ONE DIMENIONAL   00140050
C     ARRAY BACK TO FM050.                                              00150050
C                                                                       00160050
C      REFERENCES                                                       00170050
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00180050
C              X3.9-1978                                                00190050
C                                                                       00200050
C        SECTION 15.5.2, REFERENCING AN EXTERNAL FUNCTION               00210050
C        SECTION 15.6.2, SUBROUTINE REFERENCE                           00220050
C                                                                       00230050
      COMMON RVCN01,IVCN01,IVCN02,IACN11(20)                            00240050
      INTEGER FF054                                                     00250050
C                                                                       00260050
C      **********************************************************       00270050
C                                                                       00280050
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00290050
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00300050
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00310050
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00320050
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00330050
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00340050
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00350050
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00360050
C     OF EXECUTING THESE TESTS.                                         00370050
C                                                                       00380050
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00390050
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00400050
C                                                                       00410050
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00420050
C                                                                       00430050
C                  DEPARTMENT OF THE NAVY                               00440050
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00450050
C                  WASHINGTON, D.C.  20376                              00460050
C                                                                       00470050
C      **********************************************************       00480050
C                                                                       00490050
C                                                                       00500050
C                                                                       00510050
C     INITIALIZATION SECTION                                            00520050
C                                                                       00530050
C     INITIALIZE CONSTANTS                                              00540050
C      **************                                                   00550050
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00560050
      I01 = 5                                                           00570050
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00580050
      I02 = 6                                                           00590050
C     SYSTEM ENVIRONMENT SECTION                                        00600050
C                                                                       00610050
      I01 = 5                                                           00620050
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00630050
C     (UNIT NUMBER FOR CARD READER).                                    00640050
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00650050
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00660050
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00670050
C                                                                       00680050
      I02 = 6                                                           00690050
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00700050
C     (UNIT NUMBER FOR PRINTER).                                        00710050
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00720050
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00730050
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00740050
C                                                                       00750050
      IVPASS=0                                                          00760050
      IVFAIL=0                                                          00770050
      IVDELE=0                                                          00780050
      ICZERO=0                                                          00790050
C                                                                       00800050
C     WRITE PAGE HEADERS                                                00810050
      WRITE (I02,90000)                                                 00820050
      WRITE (I02,90001)                                                 00830050
      WRITE (I02,90002)                                                 00840050
      WRITE (I02, 90002)                                                00850050
      WRITE (I02,90003)                                                 00860050
      WRITE (I02,90002)                                                 00870050
      WRITE (I02,90004)                                                 00880050
      WRITE (I02,90002)                                                 00890050
      WRITE (I02,90011)                                                 00900050
      WRITE (I02,90002)                                                 00910050
      WRITE (I02,90002)                                                 00920050
      WRITE (I02,90005)                                                 00930050
      WRITE (I02,90006)                                                 00940050
      WRITE (I02,90002)                                                 00950050
C     TEST SECTION                                                      00960050
C                                                                       00970050
C         SUBROUTINE AND FUNCTION SUBPROGRAMS                           00980050
C                                                                       00990050
 4001 CONTINUE                                                          01000050
      IVTNUM = 400                                                      01010050
C                                                                       01020050
C      ****  TEST 400  ****                                             01030050
C     TEST 400 TESTS THE CALL TO A SUBROUTINE CONTAINING NO ARGUMENTS.  01040050
C     ALL PARAMETERS ARE PASSED THROUGH UNLABELED COMMON.               01050050
C                                                                       01060050
      IF (ICZERO) 34000, 4000, 34000                                    01070050
 4000 CONTINUE                                                          01080050
      RVCN01 = 2.1654                                                   01090050
      CALL FS051                                                        01100050
      RVCOMP = RVCN01                                                   01110050
      GO TO 44000                                                       01120050
34000 IVDELE = IVDELE + 1                                               01130050
      WRITE (I02,80003) IVTNUM                                          01140050
      IF (ICZERO) 44000, 4011, 44000                                    01150050
44000 IF (RVCOMP - 3.1649) 24000,14000,44001                            01160050
44001 IF (RVCOMP - 3.1659) 14000,14000,24000                            01170050
14000 IVPASS = IVPASS + 1                                               01180050
      WRITE (I02,80001) IVTNUM                                          01190050
      GO TO 4011                                                        01200050
24000 IVFAIL = IVFAIL + 1                                               01210050
      RVCORR = 3.1654                                                   01220050
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01230050
 4011 CONTINUE                                                          01240050
C                                                                       01250050
C     TEST 401 THROUGH TEST 403 TEST THE CALL TO SUBROUTINE FS052 WHICH 01260050
C     CONTAINS NO ARGUMENTS.  ALL PARAMETERS ARE PASSED THROUGH         01270050
C     UNLABELED COMMON.  SUBROUTINE FS052 CONTAIN SEVERAL RETURN        01280050
C     STATEMENTS.                                                       01290050
C                                                                       01300050
      IVTNUM = 401                                                      01310050
C                                                                       01320050
C      ****  TEST 401  ****                                             01330050
C                                                                       01340050
      IF (ICZERO) 34010, 4010, 34010                                    01350050
 4010 CONTINUE                                                          01360050
      IVCN01 = 5                                                        01370050
      IVCN02 = 1                                                        01380050
      CALL FS052                                                        01390050
      IVCOMP = IVCN01                                                   01400050
      GO TO 44010                                                       01410050
34010 IVDELE = IVDELE + 1                                               01420050
      WRITE (I02,80003) IVTNUM                                          01430050
      IF (ICZERO) 44010, 4021, 44010                                    01440050
44010 IF (IVCOMP - 6) 24010,14010,24010                                 01450050
14010 IVPASS = IVPASS + 1                                               01460050
      WRITE (I02,80001) IVTNUM                                          01470050
      GO TO 4021                                                        01480050
24010 IVFAIL = IVFAIL + 1                                               01490050
      IVCORR = 6                                                        01500050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01510050
 4021 CONTINUE                                                          01520050
      IVTNUM = 402                                                      01530050
C                                                                       01540050
C      ****  TEST 402  ****                                             01550050
C                                                                       01560050
      IF (ICZERO) 34020, 4020, 34020                                    01570050
 4020 CONTINUE                                                          01580050
      IVCN01 = 10                                                       01590050
      IVCN02 =  5                                                       01600050
      CALL FS052                                                        01610050
      IVCOMP = IVCN01                                                   01620050
      GO TO 44020                                                       01630050
34020 IVDELE = IVDELE + 1                                               01640050
      WRITE (I02,80003) IVTNUM                                          01650050
      IF (ICZERO) 44020, 4031, 44020                                    01660050
44020 IF (IVCOMP - 15) 24020,14020,24020                                01670050
14020 IVPASS = IVPASS + 1                                               01680050
      WRITE (I02,80001) IVTNUM                                          01690050
      GO TO 4031                                                        01700050
24020 IVFAIL = IVFAIL + 1                                               01710050
      IVCORR = 15                                                       01720050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01730050
 4031 CONTINUE                                                          01740050
      IVTNUM = 403                                                      01750050
C                                                                       01760050
C      ****  TEST 403  ****                                             01770050
C                                                                       01780050
      IF (ICZERO) 34030, 4030, 34030                                    01790050
 4030 CONTINUE                                                          01800050
      IVCN01 = 30                                                       01810050
      IVCN02 = 3                                                        01820050
      CALL FS052                                                        01830050
      IVCOMP = IVCN01                                                   01840050
      GO TO 44030                                                       01850050
34030 IVDELE = IVDELE + 1                                               01860050
      WRITE (I02,80003) IVTNUM                                          01870050
      IF (ICZERO) 44030, 4041, 44030                                    01880050
44030 IF (IVCOMP - 33) 24030,14030,24030                                01890050
14030 IVPASS = IVPASS + 1                                               01900050
      WRITE (I02,80001) IVTNUM                                          01910050
      GO TO 4041                                                        01920050
24030 IVFAIL = IVFAIL + 1                                               01930050
      IVCORR = 33                                                       01940050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01950050
 4041 CONTINUE                                                          01960050
C                                                                       01970050
C     TEST 404 THROUGH TEST 406 TEST THE CALL TO SUBROUTINE FS053 WHICH 01980050
C     CONTAINS SEVERAL ARGUMENTS AND SEVERAL RETURN STATEMENTS.         01990050
C                                                                       02000050
      IVTNUM = 404                                                      02010050
C                                                                       02020050
C      ****  TEST 404  ****                                             02030050
C                                                                       02040050
      IF (ICZERO) 34040, 4040, 34040                                    02050050
 4040 CONTINUE                                                          02060050
      CALL FS053 (6,10,11,IVON04,1)                                     02070050
      IVCOMP = IVON04                                                   02080050
      GO TO 44040                                                       02090050
34040 IVDELE = IVDELE + 1                                               02100050
      WRITE (I02,80003) IVTNUM                                          02110050
      IF (ICZERO) 44040, 4051, 44040                                    02120050
44040 IF (IVCOMP - 6) 24040,14040,24040                                 02130050
14040 IVPASS = IVPASS + 1                                               02140050
      WRITE (I02,80001) IVTNUM                                          02150050
      GO TO 4051                                                        02160050
24040 IVFAIL = IVFAIL + 1                                               02170050
      IVCORR = 6                                                        02180050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02190050
 4051 CONTINUE                                                          02200050
      IVTNUM = 405                                                      02210050
C                                                                       02220050
C      ****  TEST 405  ****                                             02230050
C                                                                       02240050
      IF (ICZERO) 34050, 4050, 34050                                    02250050
 4050 CONTINUE                                                          02260050
      IVCN01 = 10                                                       02270050
      CALL FS053 (6,IVCN01,11,IVON04,2)                                 02280050
      IVCOMP = IVON04                                                   02290050
      GO TO 44050                                                       02300050
34050 IVDELE = IVDELE + 1                                               02310050
      WRITE (I02,80003) IVTNUM                                          02320050
      IF (ICZERO) 44050, 4061, 44050                                    02330050
44050 IF (IVCOMP - 16) 24050,14050,24050                                02340050
14050 IVPASS = IVPASS + 1                                               02350050
      WRITE (I02,80001) IVTNUM                                          02360050
      GO TO 4061                                                        02370050
24050 IVFAIL = IVFAIL + 1                                               02380050
      IVCORR = 16                                                       02390050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02400050
 4061 CONTINUE                                                          02410050
      IVTNUM = 406                                                      02420050
C                                                                       02430050
C      ****  TEST 406  ****                                             02440050
C                                                                       02450050
      IF (ICZERO) 34060, 4060, 34060                                    02460050
 4060 CONTINUE                                                          02470050
      IVON01 = 6                                                        02480050
      IVON02 = 10                                                       02490050
      IVON03 = 11                                                       02500050
      IVON05 = 3                                                        02510050
      CALL FS053 (IVON01,IVON02,IVON03,IVON04,IVON05)                   02520050
      IVCOMP = IVON04                                                   02530050
      GO TO 44060                                                       02540050
34060 IVDELE = IVDELE + 1                                               02550050
      WRITE (I02,80003) IVTNUM                                          02560050
      IF (ICZERO) 44060, 4071, 44060                                    02570050
44060 IF (IVCOMP - 27) 24060,14060,24060                                02580050
14060 IVPASS = IVPASS + 1                                               02590050
      WRITE (I02,80001) IVTNUM                                          02600050
      GO TO 4071                                                        02610050
24060 IVFAIL = IVFAIL + 1                                               02620050
      IVCORR = 27                                                       02630050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02640050
 4071 CONTINUE                                                          02650050
C                                                                       02660050
C     TEST 407 THROUGH 409 TEST THE REFERENCE TO FUNCTION FF054 WHICH   02670050
C     CONTAINS SEVERAL ARGUMENTS AND SEVERAL RETURN STATEMENTS          02680050
C                                                                       02690050
      IVTNUM = 407                                                      02700050
C                                                                       02710050
C      ****  TEST 407  ****                                             02720050
C                                                                       02730050
      IF (ICZERO) 34070, 4070, 34070                                    02740050
 4070 CONTINUE                                                          02750050
      IVCOMP = FF054 (300,1,21,1)                                       02760050
      GO TO 44070                                                       02770050
34070 IVDELE = IVDELE + 1                                               02780050
      WRITE (I02,80003) IVTNUM                                          02790050
      IF (ICZERO) 44070, 4081, 44070                                    02800050
44070 IF (IVCOMP - 300) 24070,14070,24070                               02810050
14070 IVPASS = IVPASS + 1                                               02820050
      WRITE (I02,80001) IVTNUM                                          02830050
      GO TO 4081                                                        02840050
24070 IVFAIL = IVFAIL + 1                                               02850050
      IVCORR = 300                                                      02860050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02870050
 4081 CONTINUE                                                          02880050
      IVTNUM = 408                                                      02890050
C                                                                       02900050
C      ****  TEST 408  ****                                             02910050
C                                                                       02920050
      IF (ICZERO) 34080, 4080, 34080                                    02930050
 4080 CONTINUE                                                          02940050
      IVON01 = 300                                                      02950050
      IVON04 = 2                                                        02960050
      IVCOMP = FF054 (IVON01,77,5,IVON04)                               02970050
      GO TO 44080                                                       02980050
34080 IVDELE = IVDELE + 1                                               02990050
      WRITE (I02,80003) IVTNUM                                          03000050
      IF (ICZERO) 44080, 4091, 44080                                    03010050
44080 IF (IVCOMP - 377) 24080,14080,24080                               03020050
14080 IVPASS = IVPASS + 1                                               03030050
      WRITE (I02,80001) IVTNUM                                          03040050
      GO TO 4091                                                        03050050
24080 IVFAIL = IVFAIL + 1                                               03060050
      IVCORR = 377                                                      03070050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03080050
 4091 CONTINUE                                                          03090050
      IVTNUM = 409                                                      03100050
C                                                                       03110050
C      ****  TEST 409  ****                                             03120050
C                                                                       03130050
      IF (ICZERO) 34090, 4090, 34090                                    03140050
 4090 CONTINUE                                                          03150050
      IVON01 = 71                                                       03160050
      IVON02 = 21                                                       03170050
      IVON03 = 17                                                       03180050
      IVON04 = 3                                                        03190050
      IVCOMP = FF054 (IVON01,IVON02,IVON03,IVON04)                      03200050
      GO TO 44090                                                       03210050
34090 IVDELE = IVDELE + 1                                               03220050
      WRITE (I02,80003) IVTNUM                                          03230050
      IF (ICZERO) 44090, 4101, 44090                                    03240050
44090 IF (IVCOMP - 109) 24090,14090,24090                               03250050
14090 IVPASS = IVPASS + 1                                               03260050
      WRITE (I02,80001) IVTNUM                                          03270050
      GO TO 4101                                                        03280050
24090 IVFAIL = IVFAIL + 1                                               03290050
      IVCORR = 109                                                      03300050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03310050
 4101 CONTINUE                                                          03320050
C                                                                       03330050
C     TEST 410 THROUGH 429 TEST THE CALL TO SUBROUTINE FS055 WHICH      03340050
C     CONTAINS NO ARGUMENTS.  THE PARAMETERS ARE PASSED THROUGH AN      03350050
C     INTEGER ARRAY VARIABLE IN UNLABELED COMMON.                       03360050
C                                                                       03370050
      CALL FS055                                                        03380050
      DO 20 I = 1,20                                                    03390050
      IF (ICZERO) 34100, 4100, 34100                                    03400050
 4100 CONTINUE                                                          03410050
      IVTNUM = 409 + I                                                  03420050
      IVCOMP = IACN11(I)                                                03430050
      GO TO 44100                                                       03440050
34100 IVDELE = IVDELE + 1                                               03450050
      WRITE (I02,80003) IVTNUM                                          03460050
      IF (ICZERO) 44100, 4111, 44100                                    03470050
44100 IF (IVCOMP - I) 24100,14100,24100                                 03480050
14100 IVPASS = IVPASS + 1                                               03490050
      WRITE (I02,80001) IVTNUM                                          03500050
      GO TO 4111                                                        03510050
24100 IVFAIL = IVFAIL + 1                                               03520050
      IVCORR = I                                                        03530050
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03540050
 4111 CONTINUE                                                          03550050
20    CONTINUE                                                          03560050
C                                                                       03570050
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03580050
99999 CONTINUE                                                          03590050
      WRITE (I02,90002)                                                 03600050
      WRITE (I02,90006)                                                 03610050
      WRITE (I02,90002)                                                 03620050
      WRITE (I02,90002)                                                 03630050
      WRITE (I02,90007)                                                 03640050
      WRITE (I02,90002)                                                 03650050
      WRITE (I02,90008)  IVFAIL                                         03660050
      WRITE (I02,90009) IVPASS                                          03670050
      WRITE (I02,90010) IVDELE                                          03680050
C                                                                       03690050
C                                                                       03700050
C     TERMINATE ROUTINE EXECUTION                                       03710050
      STOP                                                              03720050
C                                                                       03730050
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03740050
90000 FORMAT (1H1)                                                      03750050
90002 FORMAT (1H )                                                      03760050
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03770050
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03780050
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03790050
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03800050
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03810050
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03820050
C                                                                       03830050
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03840050
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03850050
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03860050
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03870050
C                                                                       03880050
C     FORMAT STATEMENTS FOR TEST RESULTS                                03890050
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03900050
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03910050
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03920050
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03930050
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03940050
C                                                                       03950050
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM050)                          03960050
      END                                                               03970050
C                                                                       00010051
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020051
C                                                                       00030051
C     FS051                                                             00040051
C                                                                       00050051
C          FS051 IS A SUBROUTINE SUBPROGRAM WHICH IS CALLED BY THE MAIN 00060051
C     PROGRAM FM050.  NO ARGUMENTS ARE SPECIFIED THEREFORE ALL          00070051
C     PARAMETERS ARE PASSED VIA UNLABELED COMMON.  THE SUBROUTINE FS051 00080051
C     INCREMENTS THE VALUE OF A REAL VARIABLE BY 1 AND RETURNS CONTROL  00090051
C     TO THE CALLING PROGRAM FM050.                                     00100051
C                                                                       00110051
C      REFERENCES                                                       00120051
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00130051
C              X3.9-1978                                                00140051
C                                                                       00150051
C        SECTION 15.6, SUBROUTINES                                      00160051
C        SECTION 15.8, RETURN STATEMENT                                 00170051
C                                                                       00180051
C     TEST SECTION                                                      00190051
C                                                                       00200051
C         SUBROUTINE SUBPROGRAM - NO ARGUMENTS                          00210051
C                                                                       00220051
      SUBROUTINE FS051                                                  00230051
      COMMON //RVCN01                                                   00240051
      RVCN01 = RVCN01 + 1.0                                             00250051
      RETURN                                                            00260051
      END                                                               00270051
C                                                                       00010052
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020052
C                                                                       00030052
C     FS052                                                             00040052
C                                                                       00050052
C          FS052 IS A SUBROUTINE SUBPROGRAM WHICH IS CALLED BY THE MAIN 00060052
C     PROGRAM FM050.  NO ARGUMENTS ARE SPECIFIED THEREFORE ALL          00070052
C     PARAMETERS ARE PASSED VIA UNLABELED COMMON.  THE SUBROUTINE FS052 00080052
C     INCREMENTS THE VALUE OF ONE INTEGER VARIABLE BY 1,2,3,4 OR 5      00090052
C     DEPENDING ON THE VALUE OF A SECOND INTEGER VARIABLE AND THEN      00100052
C     RETURNS CONTROL TO THE CALLING PROGRAM FM050.  SEVERAL RETURN     00110052
C     STATEMENTS ARE INCLUDED.                                          00120052
C                                                                       00130052
C      REFERENCES                                                       00140052
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00150052
C              X3.9-1978                                                00160052
C                                                                       00170052
C        SECTION 15.6, SUBROUTINES                                      00180052
C        SECTION 15.8, RETURN STATEMENT                                 00190052
C                                                                       00200052
C     TEST SECTION                                                      00210052
C                                                                       00220052
C         SUBROUTINE SUBPROGRAM - NO ARGUMENTS, MANY RETURNS            00230052
C                                                                       00240052
      SUBROUTINE FS052                                                  00250052
      COMMON RVDN01,IVCN01,IVCN02                                       00260052
      GO TO (10,20,30,40,50),IVCN02                                     00270052
10    IVCN01 = IVCN01 + 1                                               00280052
      RETURN                                                            00290052
20    IVCN01 = IVCN01 + 2                                               00300052
      RETURN                                                            00310052
30    IVCN01 = IVCN01 + 3                                               00320052
      RETURN                                                            00330052
40    IVCN01 = IVCN01 + 4                                               00340052
      RETURN                                                            00350052
50    IVCN01 = IVCN01 + 5                                               00360052
      RETURN                                                            00370052
      END                                                               00380052
C                                                                       00010053
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020053
C                                                                       00030053
C     FS053                                                             00040053
C                                                                       00050053
C          FS053 IS A SUBROUTINE SUBPROGRAM WHICH IS CALLED BY THE MAIN 00060053
C     PROGRAM FM050.  FIVE INTEGER VARIABLE ARGUMENTS ARE PASSED AND    00070053
C     SEVERAL RETURN STATEMENTS ARE SPECIFIED.  THE SUBROUTINE FS053    00080053
C     ADDS TOGETHER THE VALUES OF THE FIRST ONE, TWO OR THREE ARGUMENTS 00090053
C     DEPENDING ON THE VALUE OF THE FIFTH ARGUMENT.  THE RESULTING SUM  00100053
C     IS THEN RETURNED TO THE CALLING PROGRAM FM050 THROUGH THE FOURTH  00110053
C     ARGUMENT.                                                         00120053
C                                                                       00130053
C      REFERENCES                                                       00140053
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00150053
C              X3.9-1978                                                00160053
C                                                                       00170053
C        SECTION 15.6, SUBROUTINES                                      00180053
C        SECTION 15.8, RETURN STATEMENT                                 00190053
C                                                                       00200053
C     TEST SECTION                                                      00210053
C                                                                       00220053
C         SUBROUTINE SUBPROGRAM - SEVERAL ARGUMENTS, SEVERAL RETURNS    00230053
C                                                                       00240053
      SUBROUTINE FS053 (IVON01,IVON02,IVON03,IVON04,IVON05)             00250053
      GO TO (10,20,30),IVON05                                           00260053
10    IVON04 = IVON01                                                   00270053
      RETURN                                                            00280053
20    IVON04 = IVON01 + IVON02                                          00290053
      RETURN                                                            00300053
30    IVON04 = IVON01 + IVON02 + IVON03                                 00310053
      RETURN                                                            00320053
      END                                                               00330053
C                                                                       00010054
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020054
C                                                                       00030054
C     FF054                                                             00040054
C                                                                       00050054
C          FF054 IS A FUNCTION SUBPROGRAM WHICH IS REFERENCED BY THE    00060054
C     MAIN PROGRAM.  FIVE INTEGER VARIABLE ARGUMENTS ARE PASSED AND     00070054
C     SEVERAL RETURN STATEMENTS ARE SPECIFIED.  THE FUNCTION FF054      00080054
C     ADDS TOGETHER THE VALUES OF THE FIRST ONE, TWO OR THREE ARGUMENTS 00090054
C     DEPENDING ON THE VALUE OF THE FOURTH ARGUMENT.  THE RESULTING SUM 00100054
C     IS THEN RETURNED TO THE REFERENCING PROGRAM FM050 THROUGH THE     00110054
C     FUNCTION REFERENCE.                                               00120054
C                                                                       00130054
C      REFERENCES                                                       00140054
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00150054
C              X3.9-1978                                                00160054
C                                                                       00170054
C        SECTION 15.5.1, FUNCTION SUBPROGRAM AND FUNCTION STATEMENT     00180054
C        SECTION 15.8, RETURN STATEMENT                                 00190054
C                                                                       00200054
C     TEST SECTION                                                      00210054
C                                                                       00220054
C         FUNCTION SUBPROGRAM - SEVERAL ARGUMENTS, SEVERAL RETURNS      00230054
C                                                                       00240054
      INTEGER FUNCTION FF054 (IVON01,IVON02,IVON03,IVON04)              00250054
      GO TO (10,20,30),IVON04                                           00260054
10    FF054 = IVON01                                                    00270054
      RETURN                                                            00280054
20    FF054 = IVON01 + IVON02                                           00290054
      RETURN                                                            00300054
30    FF054 = IVON01 + IVON02 + IVON03                                  00310054
      RETURN                                                            00320054
      END                                                               00330054
C                                                                       00010055
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020055
C                                                                       00030055
C     FS055                                                             00040055
C                                                                       00050055
C          FS055 IS A SUBROUTINE SUBPROGRAM WHICH IS CALLED BY THE MAIN 00060055
C     PROGRAM FM050.  NO ARGUMENTS ARE SPECIFIED THEREFORE ALL          00070055
C     PARAMETERS ARE PASSED VIA UNLABELED COMMON.  THE SUBROUTINE FS055 00080055
C     INITIALIZES A ONE DIMENSIONAL INTEGER ARRAY OF 20 ELEMENTS WITH   00090055
C     THE VALUES 1 THROUGH 20 RESPECTIVELY.  CONTROL IS THEN RETURNED   00100055
C     TO THE CALLING PROGRAM FM050.                                     00110055
C                                                                       00120055
C      REFERENCES                                                       00130055
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140055
C              X3.9-1978                                                00150055
C                                                                       00160055
C        SECTION 15.6, SUBROUTINES                                      00170055
C        SECTION 15.8, RETURN STATEMENT                                 00180055
C                                                                       00190055
C     TEST SECTION                                                      00200055
C                                                                       00210055
C         SUBROUTINE SUBPROGRAM - ARRAY ARGUMENTS                       00220055
C                                                                       00230055
      SUBROUTINE FS055                                                  00240055
      COMMON RVCN01,IVCN01,IVCN02,IACN11                                00250055
      DIMENSION IACN11(20)                                              00260055
      DO 20 I = 1,20                                                    00270055
      IACN11(I) = I                                                     00280055
20    CONTINUE                                                          00290055
      RETURN                                                            00300055
      END                                                               00310055
