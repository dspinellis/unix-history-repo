C     COMMENT SECTION.                                                  00010025
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020025
C     FM025                                                             00030025
C                                                                       00040025
C         THIS ROUTINE TESTS ARRAYS WITH IF STATEMENTS, DO LOOPS,       00050025
C     ASSIGNED AND COMPUTED GO TO STATEMENTS IN CONJUNCTION WITH ARRAY  00060025
C     ELEMENTS   IN COMMON OR DIMENSIONED.  ONE, TWO, AND THREE         00070025
C     DIMENSIONED ARRAYS ARE USED.  THE SUBSCRIPTS ARE INTEGER CONSTANTS00080025
C     OR SOMETIMES INTEGER VARIABLES WHEN THE ELEMENTS ARE IN LOOPS     00090025
C     AND ALL ARRAYS HAVE FIXED SIZE LIMITS.  INTEGER, REAL, AND LOGICAL00100025
C     ARRAYS ARE USED WITH THE TYPE SOMETIMES SPECIFIED WITH THE        00110025
C     EXPLICIT TYPE STATEMENT.                                          00120025
C                                                                       00130025
C      REFERENCES                                                       00140025
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00150025
C              X3.9-1978                                                00160025
C                                                                       00170025
C        SECTION 8, SPECIFICATION STATEMENTS                            00180025
C        SECTION 8.1, DIMENSION STATEMENT                               00190025
C        SECTION 8.3, COMMON STATEMENT                                  00200025
C        SECTION 8.4, TYPE-STATEMENTS                                   00210025
C        SECTION 9, DATA STATEMENT                                      00220025
C        SECTION 11.2, COMPUTED GO TO STATEMENT                         00230025
C        SECTION 11.3, ASSIGNED GO TO STATEMENT                         00240025
C        SECTION 11.10, DO STATEMENT                                    00250025
C                                                                       00260025
      COMMON IADN31(2,2,2), RADN31(2,2,2), LADN31(2,2,2)                00270025
C                                                                       00280025
      DIMENSION IADN32(2,2,2), IADN21(2,2), IADN11(2)                   00290025
C                                                                       00300025
      LOGICAL LADN31                                                    00310025
      INTEGER RADN33(2,2,2), RADN21(2,4), RADN11(8)                     00320025
      REAL IADN33(2,2,2), IADN22(2,4), IADN12(8)                        00330025
C                                                                       00340025
C                                                                       00350025
C      **********************************************************       00360025
C                                                                       00370025
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00380025
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00390025
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00400025
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00410025
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00420025
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00430025
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00440025
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00450025
C     OF EXECUTING THESE TESTS.                                         00460025
C                                                                       00470025
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00480025
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00490025
C                                                                       00500025
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00510025
C                                                                       00520025
C                  DEPARTMENT OF THE NAVY                               00530025
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00540025
C                  WASHINGTON, D.C.  20376                              00550025
C                                                                       00560025
C      **********************************************************       00570025
C                                                                       00580025
C                                                                       00590025
C                                                                       00600025
C     INITIALIZATION SECTION                                            00610025
C                                                                       00620025
C     INITIALIZE CONSTANTS                                              00630025
C      **************                                                   00640025
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650025
      I01 = 5                                                           00660025
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670025
      I02 = 6                                                           00680025
C     SYSTEM ENVIRONMENT SECTION                                        00690025
C                                                                       00700025
      I01 = 5                                                           00710025
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720025
C     (UNIT NUMBER FOR CARD READER).                                    00730025
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00740025
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00750025
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00760025
C                                                                       00770025
      I02 = 6                                                           00780025
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00790025
C     (UNIT NUMBER FOR PRINTER).                                        00800025
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00810025
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00820025
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00830025
C                                                                       00840025
      IVPASS=0                                                          00850025
      IVFAIL=0                                                          00860025
      IVDELE=0                                                          00870025
      ICZERO=0                                                          00880025
C                                                                       00890025
C     WRITE PAGE HEADERS                                                00900025
      WRITE (I02,90000)                                                 00910025
      WRITE (I02,90001)                                                 00920025
      WRITE (I02,90002)                                                 00930025
      WRITE (I02, 90002)                                                00940025
      WRITE (I02,90003)                                                 00950025
      WRITE (I02,90002)                                                 00960025
      WRITE (I02,90004)                                                 00970025
      WRITE (I02,90002)                                                 00980025
      WRITE (I02,90011)                                                 00990025
      WRITE (I02,90002)                                                 01000025
      WRITE (I02,90002)                                                 01010025
      WRITE (I02,90005)                                                 01020025
      WRITE (I02,90006)                                                 01030025
      WRITE (I02,90002)                                                 01040025
      IVTNUM = 653                                                      01050025
C                                                                       01060025
C      ****  TEST 653  ****                                             01070025
C     TEST 653  -  TEST OF SETTING ALL VALUES OF AN INTEGER ARRAY       01080025
C     BY THE INTEGER INDEX OF A DO  LOOP.  THE ARRAY HAS ONE DIMENSION. 01090025
C                                                                       01100025
      IF (ICZERO) 36530, 6530, 36530                                    01110025
 6530 CONTINUE                                                          01120025
      DO 6532 I = 1,2,1                                                 01130025
      IADN11(I) = I                                                     01140025
 6532 CONTINUE                                                          01150025
      IVCOMP = IADN11(1)                                                01160025
      GO TO 46530                                                       01170025
36530 IVDELE = IVDELE + 1                                               01180025
      WRITE (I02,80003) IVTNUM                                          01190025
      IF (ICZERO) 46530, 6541, 46530                                    01200025
46530 IF ( IVCOMP - 1 )  26530, 16530, 26530                            01210025
16530 IVPASS = IVPASS + 1                                               01220025
      WRITE (I02,80001) IVTNUM                                          01230025
      GO TO 6541                                                        01240025
26530 IVFAIL = IVFAIL + 1                                               01250025
      IVCORR = 1                                                        01260025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01270025
 6541 CONTINUE                                                          01280025
      IVTNUM = 654                                                      01290025
C                                                                       01300025
C      ****  TEST 654  ****                                             01310025
C     TEST 654  -  SEE TEST 653.  THIS TEST CHECKS THE SECOND ELEMENT OF01320025
C     THE INTEGER ARRAY IADN11(2).                                      01330025
C                                                                       01340025
      IF (ICZERO) 36540, 6540, 36540                                    01350025
 6540 CONTINUE                                                          01360025
      IVCOMP = IADN11(2)                                                01370025
      GO TO 46540                                                       01380025
36540 IVDELE = IVDELE + 1                                               01390025
      WRITE (I02,80003) IVTNUM                                          01400025
      IF (ICZERO) 46540, 6551, 46540                                    01410025
46540 IF ( IVCOMP - 2 )  26540, 16540, 26540                            01420025
16540 IVPASS = IVPASS + 1                                               01430025
      WRITE (I02,80001) IVTNUM                                          01440025
      GO TO 6551                                                        01450025
26540 IVFAIL = IVFAIL + 1                                               01460025
      IVCORR = 2                                                        01470025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01480025
 6551 CONTINUE                                                          01490025
      IVTNUM = 655                                                      01500025
C                                                                       01510025
C      ****  TEST 655  ****                                             01520025
C     TEST 655  -  TEST OF SETTING THE VALUES OF THE COLUMN OF A TWO    01530025
C     DIMENSION INTEGER ARRAY BY A DO LOOP.  THE VALUES FOR THE ELEMENTS01540025
C     IN A COLUMN IS THE NUMBER OF THE COLUMN AS SET BY THE DO LOOP     01550025
C     INDEX.  ROW NUMBERS ARE INTEGER CONSTANTS.                        01560025
C     THE VALUES FOR THE ELEMENTS ARE AS FOLLOWS                        01570025
C     1    2                                                            01580025
C     1    2                                                            01590025
C                                                                       01600025
      IF (ICZERO) 36550, 6550, 36550                                    01610025
 6550 CONTINUE                                                          01620025
      DO 6552 J = 1, 2                                                  01630025
      IADN21(1,J) = J                                                   01640025
      IADN21(2,J) = J                                                   01650025
 6552 CONTINUE                                                          01660025
      IVCOMP = IADN21(1,1)                                              01670025
      GO TO 46550                                                       01680025
36550 IVDELE = IVDELE + 1                                               01690025
      WRITE (I02,80003) IVTNUM                                          01700025
      IF (ICZERO) 46550, 6561, 46550                                    01710025
46550 IF ( IVCOMP - 1 )  26550, 16550, 26550                            01720025
16550 IVPASS = IVPASS + 1                                               01730025
      WRITE (I02,80001) IVTNUM                                          01740025
      GO TO 6561                                                        01750025
26550 IVFAIL = IVFAIL + 1                                               01760025
      IVCORR = 1                                                        01770025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01780025
 6561 CONTINUE                                                          01790025
      IVTNUM = 656                                                      01800025
C                                                                       01810025
C      ****  TEST 656  ****                                             01820025
C     TEST 656  -  SEE TEST 655.  THIS TEST CHECKS THE VALUE OF THE     01830025
C     INTEGER ARRAY  IADN21(2,2)                                        01840025
C                                                                       01850025
      IF (ICZERO) 36560, 6560, 36560                                    01860025
 6560 CONTINUE                                                          01870025
      IVCOMP = IADN21(2,2)                                              01880025
      GO TO 46560                                                       01890025
36560 IVDELE = IVDELE + 1                                               01900025
      WRITE (I02,80003) IVTNUM                                          01910025
      IF (ICZERO) 46560, 6571, 46560                                    01920025
46560 IF ( IVCOMP - 2 )  26560, 16560, 26560                            01930025
16560 IVPASS = IVPASS + 1                                               01940025
      WRITE (I02,80001) IVTNUM                                          01950025
      GO TO 6571                                                        01960025
26560 IVFAIL = IVFAIL + 1                                               01970025
      IVCORR = 2                                                        01980025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01990025
 6571 CONTINUE                                                          02000025
      IVTNUM = 657                                                      02010025
C                                                                       02020025
C      ****  TEST 657  ****                                             02030025
C     TEST 657  -  THIS TESTS SETTING BOTH THE ROW AND COLUMN SUBSCRIPTS02040025
C     IN A TWO DIMENSION INTEGER ARRAY WITH A DOUBLE NESTED DO LOOP.    02050025
C     THE ELEMENT VALUES ARE SET BY AN INTEGER COUNTER.  ELEMENT VALUES 02060025
C     ARE AS FOLLOWS         1   2                                      02070025
C                            3   4                                      02080025
C                                                                       02090025
      IF (ICZERO) 36570, 6570, 36570                                    02100025
 6570 CONTINUE                                                          02110025
      ICON01 = 0                                                        02120025
      DO 6573 I = 1, 2                                                  02130025
      DO 6572 J = 1, 2                                                  02140025
      ICON01 = ICON01 + 1                                               02150025
      IADN21(I,J) = ICON01                                              02160025
 6572 CONTINUE                                                          02170025
 6573 CONTINUE                                                          02180025
      IVCOMP = IADN21(1,2)                                              02190025
      GO TO 46570                                                       02200025
36570 IVDELE = IVDELE + 1                                               02210025
      WRITE (I02,80003) IVTNUM                                          02220025
      IF (ICZERO) 46570, 6581, 46570                                    02230025
46570 IF ( IVCOMP - 2 )  26570, 16570, 26570                            02240025
16570 IVPASS = IVPASS + 1                                               02250025
      WRITE (I02,80001) IVTNUM                                          02260025
      GO TO 6581                                                        02270025
26570 IVFAIL = IVFAIL + 1                                               02280025
      IVCORR = 2                                                        02290025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02300025
 6581 CONTINUE                                                          02310025
      IVTNUM = 658                                                      02320025
C                                                                       02330025
C      ****  TEST 658  ****                                             02340025
C     TEST 658  -  SEE TEST 657.  THIS TEST CHECKS THE VALUE OF ARRAY   02350025
C     ELEMENT IADN21(2,1) = 3                                           02360025
C                                                                       02370025
      IF (ICZERO) 36580, 6580, 36580                                    02380025
 6580 CONTINUE                                                          02390025
      IVCOMP = IADN21(2,1)                                              02400025
      GO TO 46580                                                       02410025
36580 IVDELE = IVDELE + 1                                               02420025
      WRITE (I02,80003) IVTNUM                                          02430025
      IF (ICZERO) 46580, 6591, 46580                                    02440025
46580 IF ( IVCOMP - 3 )  26580, 16580, 26580                            02450025
16580 IVPASS = IVPASS + 1                                               02460025
      WRITE (I02,80001) IVTNUM                                          02470025
      GO TO 6591                                                        02480025
26580 IVFAIL = IVFAIL + 1                                               02490025
      IVCORR = 3                                                        02500025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02510025
 6591 CONTINUE                                                          02520025
      IVTNUM = 659                                                      02530025
C                                                                       02540025
C      ****  TEST 659  ****                                             02550025
C     TEST 659  -  THIS TEST USES A TRIPLE NESTED DO LOOP TO SET THE    02560025
C     ELEMENTS IN ALL THREE DIMENSIONS OF AN INTEGER ARRAY THAT IS      02570025
C     DIMENSIONED.  THE VALUES FOR THE ELEMENTS ARE AS FOLLOWS          02580025
C     FOR ELEMENT (I,J,K) = I + J + K                                   02590025
C     SO FOR ELEMENT (1,1,2) = 1 + 1 + 2 = 4                            02600025
C                                                                       02610025
      IF (ICZERO) 36590, 6590, 36590                                    02620025
 6590 CONTINUE                                                          02630025
      DO 6594 I = 1, 2                                                  02640025
      DO 6593 J = 1, 2                                                  02650025
      DO 6592 K = 1, 2                                                  02660025
      IADN32( I, J, K ) = I + J + K                                     02670025
 6592 CONTINUE                                                          02680025
 6593 CONTINUE                                                          02690025
 6594 CONTINUE                                                          02700025
      IVCOMP = IADN32(1,1,2)                                            02710025
      GO TO 46590                                                       02720025
36590 IVDELE = IVDELE + 1                                               02730025
      WRITE (I02,80003) IVTNUM                                          02740025
      IF (ICZERO) 46590, 6601, 46590                                    02750025
46590 IF ( IVCOMP - 4 )  26590, 16590, 26590                            02760025
16590 IVPASS = IVPASS + 1                                               02770025
      WRITE (I02,80001) IVTNUM                                          02780025
      GO TO 6601                                                        02790025
26590 IVFAIL = IVFAIL + 1                                               02800025
      IVCORR = 4                                                        02810025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02820025
 6601 CONTINUE                                                          02830025
      IVTNUM = 660                                                      02840025
C                                                                       02850025
C      ****  TEST 660  ****                                             02860025
C     TEST 660  -  SEE TEST 659.  THIS CHECKS FOR IADN32(2,2,2) = 6     02870025
C                                                                       02880025
      IF (ICZERO) 36600, 6600, 36600                                    02890025
 6600 CONTINUE                                                          02900025
      IVCOMP = IADN32(2,2,2)                                            02910025
      GO TO 46600                                                       02920025
36600 IVDELE = IVDELE + 1                                               02930025
      WRITE (I02,80003) IVTNUM                                          02940025
      IF (ICZERO) 46600, 6611, 46600                                    02950025
46600 IF ( IVCOMP - 6 )  26600, 16600, 26600                            02960025
16600 IVPASS = IVPASS + 1                                               02970025
      WRITE (I02,80001) IVTNUM                                          02980025
      GO TO 6611                                                        02990025
26600 IVFAIL = IVFAIL + 1                                               03000025
      IVCORR = 6                                                        03010025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03020025
 6611 CONTINUE                                                          03030025
      IVTNUM = 661                                                      03040025
C                                                                       03050025
C      ****  TEST 661  ****                                             03060025
C     TEST 661  -  THIS TEST SETS THE ELEMENTS OF AN INTEGER ARRAY IN   03070025
C     COMMON TO MINUS THE VALUE OF THE INTEGER ARRAY SET IN TEST 659.   03080025
C     ELEMENT IADN32(1,1,2) = 4  SO ELEMENT IADN31(1,1,2) = -4          03090025
C     THE SAME INTEGER ASSIGNMENT STATEMENT IS USED AS THE TERMINATING  03100025
C     STATEMENT FOR ALL THREE DO LOOPS USED TO SET THE ARRAY VALUES     03110025
C     OF INTEGER ARRAY IADN31.                                          03120025
C     IF TEST 659 FAILS, THEN THIS TEST SHOULD ALSO FAIL.  HOWEVER, THE 03130025
C     COMPUTED VALUES SHOULD RELATE IN THAT THE COMPUTED VALUE FOR      03140025
C     TEST 661 SHOULD BE MINUS THE COMPUTED VALUE FOR TEST 659.         03150025
C                                                                       03160025
      IF (ICZERO) 36610, 6610, 36610                                    03170025
 6610 CONTINUE                                                          03180025
      DO 6612 I = 1, 2                                                  03190025
      DO 6612 J = 1, 2                                                  03200025
      DO 6612 K = 1, 2                                                  03210025
 6612 IADN31(I,J,K) = - IADN32 ( I, J, K )                              03220025
      IVCOMP = IADN31(1,1,2)                                            03230025
      GO TO 46610                                                       03240025
36610 IVDELE = IVDELE + 1                                               03250025
      WRITE (I02,80003) IVTNUM                                          03260025
      IF (ICZERO) 46610, 6621, 46610                                    03270025
46610 IF ( IVCOMP + 4 )  26610, 16610, 26610                            03280025
16610 IVPASS = IVPASS + 1                                               03290025
      WRITE (I02,80001) IVTNUM                                          03300025
      GO TO 6621                                                        03310025
26610 IVFAIL = IVFAIL + 1                                               03320025
      IVCORR = -4                                                       03330025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03340025
 6621 CONTINUE                                                          03350025
      IVTNUM = 662                                                      03360025
C                                                                       03370025
C      ****  TEST 662  ****                                             03380025
C     TEST 662  -  THIS IS A TEST OF A TRIPLE NESTED DO LOOP USED TO    03390025
C     SET THE VALUES OF A LOGICAL ARRAY LADN31.  UNLIKE THE OTHER TESTS 03400025
C     THE THIRD DIMENSION IS SET LAST, THE FIRST DIMENSION IS SET SECOND03410025
C     AND THE SECOND DIMENSION IS SET FIRST.  ALL ARRAY ELEMENTS ARE SET03420025
C     TO THE LOGICAL CONSTANT .FALSE.                                   03430025
C                                                                       03440025
      IF (ICZERO) 36620, 6620, 36620                                    03450025
 6620 CONTINUE                                                          03460025
      DO 6622 K = 1, 2                                                  03470025
      DO 6622 I = 1, 2                                                  03480025
      DO 6622 J = 1, 2                                                  03490025
      LADN31( I, J, K ) = .FALSE.                                       03500025
 6622 CONTINUE                                                          03510025
      ICON01 = 1                                                        03520025
      IF ( LADN31(2,1,2) )  ICON01 = 0                                  03530025
      GO TO 46620                                                       03540025
36620 IVDELE = IVDELE + 1                                               03550025
      WRITE (I02,80003) IVTNUM                                          03560025
      IF (ICZERO) 46620, 6631, 46620                                    03570025
46620 IF ( ICON01 - 1 )  26620, 16620, 26620                            03580025
16620 IVPASS = IVPASS + 1                                               03590025
      WRITE (I02,80001) IVTNUM                                          03600025
      GO TO 6631                                                        03610025
26620 IVFAIL = IVFAIL + 1                                               03620025
      IVCOMP = ICON01                                                   03630025
      IVCORR = 1                                                        03640025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03650025
 6631 CONTINUE                                                          03660025
      IVTNUM = 663                                                      03670025
C                                                                       03680025
C     NOTE ****  TEST 663 WAS DELETED BY FCCTS.                         03690025
C                                                                       03700025
      IF (ICZERO) 36630, 6630, 36630                                    03710025
 6630 CONTINUE                                                          03720025
36630 IVDELE = IVDELE + 1                                               03730025
      WRITE (I02,80003) IVTNUM                                          03740025
      IF (ICZERO) 46630, 6641, 46630                                    03750025
46630 IF ( ICON01 - 6633 )  26630, 16630, 26630                         03760025
16630 IVPASS = IVPASS + 1                                               03770025
      WRITE (I02,80001) IVTNUM                                          03780025
      GO TO 6641                                                        03790025
26630 IVFAIL = IVFAIL + 1                                               03800025
      IVCOMP = ICON01                                                   03810025
      IVCORR = 6633                                                     03820025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03830025
 6641 CONTINUE                                                          03840025
      IVTNUM = 664                                                      03850025
C                                                                       03860025
C     NOTE ****  TEST 664 WAS DELETED BY FCCTS.                         03870025
C                                                                       03880025
      IF (ICZERO) 36640, 6640, 36640                                    03890025
 6640 CONTINUE                                                          03900025
36640 IVDELE = IVDELE + 1                                               03910025
      WRITE (I02,80003) IVTNUM                                          03920025
      IF (ICZERO) 46640, 6651, 46640                                    03930025
46640 IF ( ICON01 - 6643 )  26640, 16640, 26640                         03940025
16640 IVPASS = IVPASS + 1                                               03950025
      WRITE (I02,80001) IVTNUM                                          03960025
      GO TO 6651                                                        03970025
26640 IVFAIL = IVFAIL + 1                                               03980025
      IVCOMP = ICON01                                                   03990025
      IVCORR = 6443                                                     04000025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04010025
 6651 CONTINUE                                                          04020025
      IVTNUM = 665                                                      04030025
C                                                                       04040025
C      ****  TEST 665  ****                                             04050025
C     TEST 665  -  ARRAY ELEMENTS SET TO TYPE REAL BY THE EXPLICIT      04060025
C     REAL STATEMENT ARE SET TO THE VALUE 0.5 AND USED TO SET THE VALUE 04070025
C     OF AN ARRAY ELEMENT SET TO TYPE INTEGER BY THE INTEGER STATEMENT. 04080025
C     THIS LAST INTEGER ELEMENT IS USED IN A LOGICAL IF STATEMENT       04090025
C     THAT SHOULD COMPARE TRUE.  ( .5 + .5 + .5 ) * 2. .EQ. 3           04100025
C                                                                       04110025
      IF (ICZERO) 36650, 6650, 36650                                    04120025
 6650 CONTINUE                                                          04130025
      IADN33(2,2,2) = 0.5                                               04140025
      IADN22(2,4) = 0.5                                                 04150025
      IADN12(8) = 0.5                                                   04160025
      RADN11(8) = ( IADN33(2,2,2) + IADN22(2,4) + IADN12(8) ) * 2.      04170025
      ICON01 = 0                                                        04180025
      IF ( RADN11(8) .EQ. 3 )  ICON01 = 1                               04190025
      GO TO 46650                                                       04200025
36650 IVDELE = IVDELE + 1                                               04210025
      WRITE (I02,80003) IVTNUM                                          04220025
      IF (ICZERO) 46650, 6661, 46650                                    04230025
46650 IF ( ICON01 - 1 )  26650, 16650, 26650                            04240025
16650 IVPASS = IVPASS + 1                                               04250025
      WRITE (I02,80001) IVTNUM                                          04260025
      GO TO 6661                                                        04270025
26650 IVFAIL = IVFAIL + 1                                               04280025
      IVCOMP = ICON01                                                   04290025
      IVCORR = 1                                                        04300025
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04310025
 6661 CONTINUE                                                          04320025
C                                                                       04330025
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             04340025
99999 CONTINUE                                                          04350025
      WRITE (I02,90002)                                                 04360025
      WRITE (I02,90006)                                                 04370025
      WRITE (I02,90002)                                                 04380025
      WRITE (I02,90002)                                                 04390025
      WRITE (I02,90007)                                                 04400025
      WRITE (I02,90002)                                                 04410025
      WRITE (I02,90008)  IVFAIL                                         04420025
      WRITE (I02,90009) IVPASS                                          04430025
      WRITE (I02,90010) IVDELE                                          04440025
C                                                                       04450025
C                                                                       04460025
C     TERMINATE ROUTINE EXECUTION                                       04470025
      STOP                                                              04480025
C                                                                       04490025
C     FORMAT STATEMENTS FOR PAGE HEADERS                                04500025
90000 FORMAT (1H1)                                                      04510025
90002 FORMAT (1H )                                                      04520025
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04530025
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   04540025
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        04550025
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 04560025
90006 FORMAT (1H ,5X,46H----------------------------------------------) 04570025
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             04580025
C                                                                       04590025
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               04600025
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        04610025
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              04620025
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             04630025
C                                                                       04640025
C     FORMAT STATEMENTS FOR TEST RESULTS                                04650025
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04660025
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      04670025
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   04680025
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04690025
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    04700025
C                                                                       04710025
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM025)                          04720025
      END                                                               04730025
