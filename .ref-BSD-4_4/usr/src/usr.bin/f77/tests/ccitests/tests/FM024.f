C     COMMENT SECTION.                                                  00010024
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020024
C     FM024                                                             00030024
C                                                                       00040024
C                  THREE DIMENSIONED ARRAYS ARE USED IN THIS ROUTINE.   00050024
C         THIS ROUTINE TESTS ARRAYS WITH FIXED DIMENSION AND SIZE LIMITS00060024
C     SET EITHER IN A BLANK COMMON OR DIMENSION STATEMENT.  THE VALUES  00070024
C     OF THE ARRAY ELEMENTS ARE SET IN VARIOUS WAYS SUCH AS SIMPLE      00080024
C     ASSIGNMENT STATEMENTS, SET TO THE VALUES OF OTHER ARRAY ELEMENTS  00090024
C     (EITHER POSITIVE OR NEGATIVE), SET BY INTEGER TO REAL OR REAL TO  00100024
C     INTEGER CONVERSION, SET BY ARITHMETIC EXPRESSIONS, OR SET BY      00110024
C     USE OF THE  EQUIVALENCE  STATEMENT.                               00120024
C                                                                       00130024
C                                                                       00140024
C      REFERENCES                                                       00150024
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00160024
C              X3.9-1978                                                00170024
C                                                                       00180024
C        SECTION 8, SPECIFICATION STATEMENTS                            00190024
C        SECTION 8.1, DIMENSION STATEMENT                               00200024
C        SECTION 8.2, EQUIVALENCE STATEMENT                             00210024
C        SECTION 8.3, COMMON STATEMENT                                  00220024
C        SECTION 8.4, TYPE-STATEMENTS                                   00230024
C        SECTION 9, DATA STATEMENT                                      00240024
C                                                                       00250024
      COMMON ICOE01, RCOE01, LCOE01                                     00260024
      COMMON IADE31(3,3,3), RADE31(3,3,3), LADE31(3,3,3)                00270024
      COMMON IADN31(2,2,2), RADN31(2,2,2), LADN31(2,2,2)                00280024
C                                                                       00290024
      DIMENSION IADE32(3,3,3), RADE32(3,3,3), LADE32(3,3,3)             00300024
      DIMENSION IADN32(2,2,2), IADN21(2,2), IADN11(2)                   00310024
      DIMENSION IADE21(2,2), IADE11(4)                                  00320024
C                                                                       00330024
      EQUIVALENCE (IADE31(1,1,1), IADE32(1,1,1) )                       00340024
      EQUIVALENCE ( RADE31(1,1,1), RADE32(1,1,1) )                      00350024
      EQUIVALENCE ( LADE31(1,1,1), LADE32(1,1,1) )                      00360024
      EQUIVALENCE ( IADE31(1,1,1), IADE21(1,1), IADE11(1) )             00370024
      EQUIVALENCE ( ICOE01, ICOE02, ICOE03 )                            00380024
C                                                                       00390024
      LOGICAL LADE31, LADN31, LADE32, LCOE01                            00400024
      INTEGER RADN33(2,2,2), RADN21(2,4), RADN11(8)                     00410024
      REAL IADN33(2,2,2), IADN22(2,4), IADN12(8)                        00420024
C                                                                       00430024
C                                                                       00440024
C      **********************************************************       00450024
C                                                                       00460024
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00470024
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00480024
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00490024
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00500024
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00510024
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00520024
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00530024
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00540024
C     OF EXECUTING THESE TESTS.                                         00550024
C                                                                       00560024
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00570024
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00580024
C                                                                       00590024
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00600024
C                                                                       00610024
C                  DEPARTMENT OF THE NAVY                               00620024
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00630024
C                  WASHINGTON, D.C.  20376                              00640024
C                                                                       00650024
C      **********************************************************       00660024
C                                                                       00670024
C                                                                       00680024
C                                                                       00690024
C     INITIALIZATION SECTION                                            00700024
C                                                                       00710024
C     INITIALIZE CONSTANTS                                              00720024
C      **************                                                   00730024
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00740024
      I01 = 5                                                           00750024
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00760024
      I02 = 6                                                           00770024
C     SYSTEM ENVIRONMENT SECTION                                        00780024
C                                                                       00790024
      I01 = 5                                                           00800024
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00810024
C     (UNIT NUMBER FOR CARD READER).                                    00820024
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00830024
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00840024
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00850024
C                                                                       00860024
      I02 = 6                                                           00870024
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00880024
C     (UNIT NUMBER FOR PRINTER).                                        00890024
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00900024
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00910024
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00920024
C                                                                       00930024
      IVPASS=0                                                          00940024
      IVFAIL=0                                                          00950024
      IVDELE=0                                                          00960024
      ICZERO=0                                                          00970024
C                                                                       00980024
C     WRITE PAGE HEADERS                                                00990024
      WRITE (I02,90000)                                                 01000024
      WRITE (I02,90001)                                                 01010024
      WRITE (I02,90002)                                                 01020024
      WRITE (I02, 90002)                                                01030024
      WRITE (I02,90003)                                                 01040024
      WRITE (I02,90002)                                                 01050024
      WRITE (I02,90004)                                                 01060024
      WRITE (I02,90002)                                                 01070024
      WRITE (I02,90011)                                                 01080024
      WRITE (I02,90002)                                                 01090024
      WRITE (I02,90002)                                                 01100024
      WRITE (I02,90005)                                                 01110024
      WRITE (I02,90006)                                                 01120024
      WRITE (I02,90002)                                                 01130024
      IVTNUM = 645                                                      01140024
C                                                                       01150024
C      ****  TEST 645  ****                                             01160024
C     TEST 645  -  TESTS SETTING A THREE DIMENSION INTEGER ARRAY ELEMENT01170024
C     BY A SIMPLE INTEGER ASSIGNMENT STATEMENT.                         01180024
C                                                                       01190024
      IF (ICZERO) 36450, 6450, 36450                                    01200024
 6450 CONTINUE                                                          01210024
      IADN31(2,2,2) = -9999                                             01220024
      IVCOMP = IADN31(2,2,2)                                            01230024
      GO TO 46450                                                       01240024
36450 IVDELE = IVDELE + 1                                               01250024
      WRITE (I02,80003) IVTNUM                                          01260024
      IF (ICZERO) 46450, 6461, 46450                                    01270024
46450 IF ( IVCOMP + 9999 )  26450, 16450, 26450                         01280024
16450 IVPASS = IVPASS + 1                                               01290024
      WRITE (I02,80001) IVTNUM                                          01300024
      GO TO 6461                                                        01310024
26450 IVFAIL = IVFAIL + 1                                               01320024
      IVCORR = -9999                                                    01330024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01340024
 6461 CONTINUE                                                          01350024
      IVTNUM = 646                                                      01360024
C                                                                       01370024
C      ****  TEST 646  ****                                             01380024
C     TEST 646  -  TESTS SETTING A THREE DIMENSION REAL ARRAY ELEMENT   01390024
C     BY A SIMPLE REAL ASSIGNMENT STATEMENT.                            01400024
C                                                                       01410024
      IF (ICZERO) 36460, 6460, 36460                                    01420024
 6460 CONTINUE                                                          01430024
      RADN31(1,2,1) = 512.                                              01440024
      IVCOMP = RADN31(1,2,1)                                            01450024
      GO TO 46460                                                       01460024
36460 IVDELE = IVDELE + 1                                               01470024
      WRITE (I02,80003) IVTNUM                                          01480024
      IF (ICZERO) 46460, 6471, 46460                                    01490024
46460 IF ( IVCOMP - 512 )  26460, 16460, 26460                          01500024
16460 IVPASS = IVPASS + 1                                               01510024
      WRITE (I02,80001) IVTNUM                                          01520024
      GO TO 6471                                                        01530024
26460 IVFAIL = IVFAIL + 1                                               01540024
      IVCORR = 512                                                      01550024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01560024
 6471 CONTINUE                                                          01570024
      IVTNUM = 647                                                      01580024
C                                                                       01590024
C      ****  TEST 647  ****                                             01600024
C     TEST 647  -  TESTS SETTING A THREE DIMENSION LOGICAL ARRAY ELEMENT01610024
C     BY A SIMPLE LOGICAL ASSIGNMENT STATEMENT.                         01620024
C                                                                       01630024
      IF (ICZERO) 36470, 6470, 36470                                    01640024
 6470 CONTINUE                                                          01650024
      LADN31(1,2,2) = .TRUE.                                            01660024
      ICON01 = 0                                                        01670024
      IF ( LADN31(1,2,2) )  ICON01 = 1                                  01680024
      GO TO 46470                                                       01690024
36470 IVDELE = IVDELE + 1                                               01700024
      WRITE (I02,80003) IVTNUM                                          01710024
      IF (ICZERO) 46470, 6481, 46470                                    01720024
46470 IF ( ICON01 - 1 )  26470, 16470, 26470                            01730024
16470 IVPASS = IVPASS + 1                                               01740024
      WRITE (I02,80001) IVTNUM                                          01750024
      GO TO 6481                                                        01760024
26470 IVFAIL = IVFAIL + 1                                               01770024
      IVCOMP = ICON01                                                   01780024
      IVCORR = 1                                                        01790024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01800024
 6481 CONTINUE                                                          01810024
      IVTNUM = 648                                                      01820024
C                                                                       01830024
C      ****  TEST 648  ****                                             01840024
C     TEST 648  -  TESTS SETTING A ONE, TWO, AND THREE DIMENSION ARRAY  01850024
C     ELEMENT TO A VALUE IN ARITHMETIC ASSIGNMENT STATEMENTS.  ALL THREE01860024
C     ELEMENTS ARE INTEGERS.  THE INTEGER ARRAY ELEMENTS ARE THEN USED  01870024
C     IN AN ARITHMETIC STATEMENT AND THE RESULT IS STORED BY INTEGER    01880024
C     TO REAL CONVERSION INTO A THREE DIMENSION REAL ARRAY ELEMENT.     01890024
C                                                                       01900024
      IF (ICZERO) 36480, 6480, 36480                                    01910024
 6480 CONTINUE                                                          01920024
      IADN11(2) = 1                                                     01930024
      IADN21(2,2) = 2                                                   01940024
      IADN32(2,2,2) = 3                                                 01950024
      RADN31(2,2,1) = IADN11(2) + IADN21(2,2) + IADN32(2,2,2)           01960024
      IVCOMP = RADN31(2,2,1)                                            01970024
      GO TO 46480                                                       01980024
36480 IVDELE = IVDELE + 1                                               01990024
      WRITE (I02,80003) IVTNUM                                          02000024
      IF (ICZERO) 46480, 6491, 46480                                    02010024
46480 IF ( IVCOMP - 6) 26480, 16480, 26480                              02020024
16480 IVPASS = IVPASS + 1                                               02030024
      WRITE (I02,80001) IVTNUM                                          02040024
      GO TO 6491                                                        02050024
26480 IVFAIL = IVFAIL + 1                                               02060024
      IVCORR = 6                                                        02070024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02080024
 6491 CONTINUE                                                          02090024
      IVTNUM = 649                                                      02100024
C                                                                       02110024
C      ****  TEST 649  ****                                             02120024
C     TEST 649  -  TESTS OF ONE, TWO, AND THREE DIMENSION ARRAY ELEMENTS02130024
C     SET EXPLICITLY INTEGER BY THE INTEGER TYPE STATEMENT.  ALL ELEMENT02140024
C     VALUES SHOULD BE ZERO FROM REAL TO INTEGER TRUNCATION FROM A VALUE02150024
C     OF 0.5.  ALL THREE ELEMENTS ARE USED IN AN ARITHMETIC EXPRESSION. 02160024
C     THE VALUE OF THE SUM OF THE ELEMENTS SHOULD BE ZERO.              02170024
C                                                                       02180024
      IF (ICZERO) 36490, 6490, 36490                                    02190024
 6490 CONTINUE                                                          02200024
      RADN11(8) = 0000.50000                                            02210024
      RADN21(2,4) = .50000                                              02220024
      RADN33(2,2,2) = 00000.5                                           02230024
      RADN11(1) = RADN11(8) + RADN21(2,4) + RADN33(2,2,2)               02240024
      IVCOMP = RADN11(1)                                                02250024
      GO TO 46490                                                       02260024
36490 IVDELE = IVDELE + 1                                               02270024
      WRITE (I02,80003) IVTNUM                                          02280024
      IF (ICZERO) 46490, 6501, 46490                                    02290024
46490 IF ( IVCOMP - 0 )  26490, 16490, 26490                            02300024
16490 IVPASS = IVPASS + 1                                               02310024
      WRITE (I02,80001) IVTNUM                                          02320024
      GO TO 6501                                                        02330024
26490 IVFAIL = IVFAIL + 1                                               02340024
      IVCORR = 0                                                        02350024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02360024
 6501 CONTINUE                                                          02370024
      IVTNUM = 650                                                      02380024
C                                                                       02390024
C      ****  TEST 650  ****                                             02400024
C     TEST 650  -  TEST OF THE EQUIVALENCE STATEMENT.  A REAL ARRAY     02410024
C     ELEMENT IS SET BY AN ASSIGNMENT STATEMENT.  ITS EQUIVALENT ELEMENT02420024
C     IN COMMON IS USED TO SET THE VALUE OF AN INTEGER ARRAY ELEMENT    02430024
C     ALSO IN COMMON.  FINALLY THE DIMENSIONED EQUIVALENT INTEGER       02440024
C     ARRAY ELEMENT IS TESTED FOR THE VALUE USED THROUGHOUT  32767.     02450024
C                                                                       02460024
      IF (ICZERO) 36500, 6500, 36500                                    02470024
 6500 CONTINUE                                                          02480024
      RADE32(2,2,2) = 32767.                                            02490024
      IADE31(2,2,2) = RADE31(2,2,2)                                     02500024
      IVCOMP = IADE32(2,2,2)                                            02510024
      GO TO 46500                                                       02520024
36500 IVDELE = IVDELE + 1                                               02530024
      WRITE (I02,80003) IVTNUM                                          02540024
      IF (ICZERO) 46500, 6511, 46500                                    02550024
46500 IF ( IVCOMP - 32767 )  26500, 16500, 26500                        02560024
16500 IVPASS = IVPASS + 1                                               02570024
      WRITE (I02,80001) IVTNUM                                          02580024
      GO TO 6511                                                        02590024
26500 IVFAIL = IVFAIL + 1                                               02600024
      IVCORR = 32767                                                    02610024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02620024
 6511 CONTINUE                                                          02630024
      IVTNUM = 651                                                      02640024
C                                                                       02650024
C      ****  TEST 651  ****                                             02660024
C     TEST 651  -  THIS IS A TEST OF COMMON AND DIMENSION AS WELL AS A  02670024
C     TEST OF THE EQUIVALENCE STATEMENT USING LOGICAL ARRAY ELEMENTS    02680024
C     BOTH IN COMMON AND DIMENSIONED.  A LOGICAL VARIABLE IN COMMON IS  02690024
C     SET TO A VALUE OF .NOT. THE VALUE USED IN THE EQUIVALENCED ARRAY  02700024
C     ELEMENTS WHICH WERE SET IN A LOGICAL ASSIGNMENT STATEMENT.        02710024
C                                                                       02720024
      IF (ICZERO) 36510, 6510, 36510                                    02730024
 6510 CONTINUE                                                          02740024
      LADE31(1,2,3) = .FALSE.                                           02750024
      LCOE01 = .NOT. LADE32(1,2,3)                                      02760024
      ICON01 = 0                                                        02770024
      IF ( LCOE01 )  ICON01 = 1                                         02780024
      GO TO 46510                                                       02790024
36510 IVDELE = IVDELE + 1                                               02800024
      WRITE (I02,80003) IVTNUM                                          02810024
      IF (ICZERO) 46510, 6521, 46510                                    02820024
46510 IF ( ICON01 - 1 )  26510, 16510, 26510                            02830024
16510 IVPASS = IVPASS + 1                                               02840024
      WRITE (I02,80001) IVTNUM                                          02850024
      GO TO 6521                                                        02860024
26510 IVFAIL = IVFAIL + 1                                               02870024
      IVCOMP = ICON01                                                   02880024
      IVCORR = 1                                                        02890024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02900024
 6521 CONTINUE                                                          02910024
      IVTNUM = 652                                                      02920024
C                                                                       02930024
C      ****  TEST 652  ****                                             02940024
C     TEST 652  -  TESTS OF ONE, TWO, AND THREE DIMENSION ARRAY ELEMENTS02950024
C     SET EXPLICITLY REAL BY THE REAL TYPE STATEMENT.  ALL ELEMENT      02960024
C     VALUES SHOULD BE 0.5 FROM THE REAL ASSIGNMENT STATEMENT.  THE     02970024
C     ARRAY ELEMENTS ARE SUMMED AND THEN THE SUM MULTIPLIED BY 2.       02980024
C     FINALLY 0.2 IS ADDED TO THE RESULT AND THE FINAL RESULT CONVERTED 02990024
C     TO AN INTEGER  ( ( .5 + .5 + .5 ) * 2. ) + 0.2                    03000024
C                                                                       03010024
      IF (ICZERO) 36520, 6520, 36520                                    03020024
 6520 CONTINUE                                                          03030024
      IADN12(5) = 0.5                                                   03040024
      IADN22(1,3) = 0.5                                                 03050024
      IADN33(1,2,2) = 0.5                                               03060024
      IVCOMP = ( ( IADN12(5) + IADN22(1,3) + IADN33(1,2,2) ) * 2. ) + .203070024
      GO TO 46520                                                       03080024
36520 IVDELE = IVDELE + 1                                               03090024
      WRITE (I02,80003) IVTNUM                                          03100024
      IF (ICZERO) 46520, 6531, 46520                                    03110024
46520 IF ( IVCOMP - 3 )  26520, 16520, 26520                            03120024
16520 IVPASS = IVPASS + 1                                               03130024
      WRITE (I02,80001) IVTNUM                                          03140024
      GO TO 6531                                                        03150024
26520 IVFAIL = IVFAIL + 1                                               03160024
      IVCORR = 3                                                        03170024
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03180024
 6531 CONTINUE                                                          03190024
C                                                                       03200024
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03210024
99999 CONTINUE                                                          03220024
      WRITE (I02,90002)                                                 03230024
      WRITE (I02,90006)                                                 03240024
      WRITE (I02,90002)                                                 03250024
      WRITE (I02,90002)                                                 03260024
      WRITE (I02,90007)                                                 03270024
      WRITE (I02,90002)                                                 03280024
      WRITE (I02,90008)  IVFAIL                                         03290024
      WRITE (I02,90009) IVPASS                                          03300024
      WRITE (I02,90010) IVDELE                                          03310024
C                                                                       03320024
C                                                                       03330024
C     TERMINATE ROUTINE EXECUTION                                       03340024
      STOP                                                              03350024
C                                                                       03360024
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03370024
90000 FORMAT (1H1)                                                      03380024
90002 FORMAT (1H )                                                      03390024
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03400024
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03410024
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03420024
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03430024
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03440024
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03450024
C                                                                       03460024
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03470024
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03480024
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03490024
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03500024
C                                                                       03510024
C     FORMAT STATEMENTS FOR TEST RESULTS                                03520024
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03530024
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03540024
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03550024
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03560024
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03570024
C                                                                       03580024
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM024)                          03590024
      END                                                               03600024
