C     COMMENT SECTION.                                                  00010022
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020022
C     FM022                                                             00030022
C                                                                       00040022
C         THIS ROUTINE TESTS ARRAYS WITH FIXED DIMENSION AND SIZE LIMITS00050022
C     SET EITHER IN A BLANK COMMON OR DIMENSION STATEMENT.  THE VALUES  00060022
C     OF THE ARRAY ELEMENTS ARE SET IN VARIOUS WAYS SUCH AS SIMPLE      00070022
C     ASSIGNMENT STATEMENTS, SET TO THE VALUES OF OTHER ARRAY ELEMENTS  00080022
C     (EITHER POSITIVE OR NEGATIVE), SET BY INTEGER TO REAL OR REAL TO  00090022
C     INTEGER CONVERSION, SET BY ARITHMETIC EXPRESSIONS, OR SET BY      00100022
C     USE OF THE  EQUIVALENCE  STATEMENT.                               00110022
C                                                                       00120022
C      REFERENCES                                                       00130022
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140022
C              X3.9-1978                                                00150022
C                                                                       00160022
C        SECTION 8, SPECIFICATION STATEMENTS                            00170022
C        SECTION 8.1, DIMENSION STATEMENT                               00180022
C        SECTION 8.2, EQUIVALENCE STATEMENT                             00190022
C        SECTION 8.3, COMMON STATEMENT                                  00200022
C        SECTION 8.4, TYPE-STATEMENTS                                   00210022
C        SECTION 9, DATA STATEMENT                                      00220022
C                                                                       00230022
C                                                                       00240022
C                                                                       00250022
      COMMON IADN14(5), RADN14(5), LADN13(2)                            00260022
C                                                                       00270022
      DIMENSION IADN11(5), RADN11(5), LADN11(2)                         00280022
      DIMENSION IADN12(5), RADN12(5), LADN12(2)                         00290022
      DIMENSION IADN15(2), RADN15(2)                                    00300022
      DIMENSION IADN16(4), IADN17(4)                                    00310022
C                                                                       00320022
      INTEGER RADN13(5)                                                 00330022
      REAL IADN13(5)                                                    00340022
      LOGICAL LADN11, LADN12, LADN13, LCTN01                            00350022
C                                                                       00360022
      EQUIVALENCE (IADN14(1), IADN15(1)), (RADN14(2),RADN15(2))         00370022
      EQUIVALENCE (LADN13(1),LCTN01),  (IADN14(5), ICON02)              00380022
      EQUIVALENCE (RADN14(5), RCON01)                                   00390022
      EQUIVALENCE ( IADN16(3), IADN17(2) )                              00400022
C                                                                       00410022
      DATA IADN12(1)/3/, RADN12(1)/-512./, IADN13(1)/0.5/, RADN13(1)/-3/00420022
C                                                                       00430022
C                                                                       00440022
C                                                                       00450022
C      **********************************************************       00460022
C                                                                       00470022
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00480022
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00490022
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00500022
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00510022
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00520022
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00530022
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00540022
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00550022
C     OF EXECUTING THESE TESTS.                                         00560022
C                                                                       00570022
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00580022
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00590022
C                                                                       00600022
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00610022
C                                                                       00620022
C                  DEPARTMENT OF THE NAVY                               00630022
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00640022
C                  WASHINGTON, D.C.  20376                              00650022
C                                                                       00660022
C      **********************************************************       00670022
C                                                                       00680022
C                                                                       00690022
C                                                                       00700022
C     INITIALIZATION SECTION                                            00710022
C                                                                       00720022
C     INITIALIZE CONSTANTS                                              00730022
C      **************                                                   00740022
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00750022
      I01 = 5                                                           00760022
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00770022
      I02 = 6                                                           00780022
C     SYSTEM ENVIRONMENT SECTION                                        00790022
C                                                                       00800022
      I01 = 5                                                           00810022
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00820022
C     (UNIT NUMBER FOR CARD READER).                                    00830022
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00840022
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00850022
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00860022
C                                                                       00870022
      I02 = 6                                                           00880022
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00890022
C     (UNIT NUMBER FOR PRINTER).                                        00900022
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00910022
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00920022
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00930022
C                                                                       00940022
      IVPASS=0                                                          00950022
      IVFAIL=0                                                          00960022
      IVDELE=0                                                          00970022
      ICZERO=0                                                          00980022
C                                                                       00990022
C     WRITE PAGE HEADERS                                                01000022
      WRITE (I02,90000)                                                 01010022
      WRITE (I02,90001)                                                 01020022
      WRITE (I02,90002)                                                 01030022
      WRITE (I02, 90002)                                                01040022
      WRITE (I02,90003)                                                 01050022
      WRITE (I02,90002)                                                 01060022
      WRITE (I02,90004)                                                 01070022
      WRITE (I02,90002)                                                 01080022
      WRITE (I02,90011)                                                 01090022
      WRITE (I02,90002)                                                 01100022
      WRITE (I02,90002)                                                 01110022
      WRITE (I02,90005)                                                 01120022
      WRITE (I02,90006)                                                 01130022
      WRITE (I02,90002)                                                 01140022
      IVTNUM = 604                                                      01150022
C                                                                       01160022
C      ****  TEST 604  ****                                             01170022
C     TEST 604  -  THIS TESTS A  SIMPLE ASSIGNMENT STATEMENT IN SETTING 01180022
C     AN INTEGER ARRAY ELEMENT TO A POSITIVE VALUE OF 32767.            01190022
C                                                                       01200022
      IF (ICZERO) 36040, 6040, 36040                                    01210022
 6040 CONTINUE                                                          01220022
      IADN11(5) = 32767                                                 01230022
      IVCOMP = IADN11(5)                                                01240022
      GO TO 46040                                                       01250022
36040 IVDELE = IVDELE + 1                                               01260022
      WRITE (I02,80003) IVTNUM                                          01270022
      IF (ICZERO) 46040, 6051, 46040                                    01280022
46040 IF ( IVCOMP - 32767 )  26040, 16040, 26040                        01290022
16040 IVPASS = IVPASS + 1                                               01300022
      WRITE (I02,80001) IVTNUM                                          01310022
      GO TO 6051                                                        01320022
26040 IVFAIL = IVFAIL + 1                                               01330022
      IVCORR = 32767                                                    01340022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01350022
 6051 CONTINUE                                                          01360022
      IVTNUM = 605                                                      01370022
C                                                                       01380022
C      ****  TEST 605  ****                                             01390022
C     TEST 605  -  TEST OF A SIMPLE ASSIGN WITH A NEGATIVE VALUE -32766 01400022
C                                                                       01410022
      IF (ICZERO) 36050, 6050, 36050                                    01420022
 6050 CONTINUE                                                          01430022
      IADN11(1) = -32766                                                01440022
      IVCOMP = IADN11(1)                                                01450022
      GO TO 46050                                                       01460022
36050 IVDELE = IVDELE + 1                                               01470022
      WRITE (I02,80003) IVTNUM                                          01480022
      IF (ICZERO) 46050, 6061, 46050                                    01490022
46050 IF ( IVCOMP + 32766 )  26050, 16050, 26050                        01500022
16050 IVPASS = IVPASS + 1                                               01510022
      WRITE (I02,80001) IVTNUM                                          01520022
      GO TO 6061                                                        01530022
26050 IVFAIL = IVFAIL + 1                                               01540022
      IVCORR = -32766                                                   01550022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01560022
 6061 CONTINUE                                                          01570022
      IVTNUM = 606                                                      01580022
C                                                                       01590022
C      ****  TEST 606  ****                                             01600022
C     TEST 606  -  TEST OF UNSIGNED ZERO SET TO AN ARRAY ELEMENT        01610022
C     BY A SIMPLE ASSIGNMENT STATEMENT.                                 01620022
C                                                                       01630022
      IF (ICZERO) 36060, 6060, 36060                                    01640022
 6060 CONTINUE                                                          01650022
      IADN11(3) = 0                                                     01660022
      IVCOMP = IADN11(3)                                                01670022
      GO TO 46060                                                       01680022
36060 IVDELE = IVDELE + 1                                               01690022
      WRITE (I02,80003) IVTNUM                                          01700022
      IF (ICZERO) 46060, 6071, 46060                                    01710022
46060 IF ( IVCOMP - 0 )  26060, 16060, 26060                            01720022
16060 IVPASS = IVPASS + 1                                               01730022
      WRITE (I02,80001) IVTNUM                                          01740022
      GO TO 6071                                                        01750022
26060 IVFAIL = IVFAIL + 1                                               01760022
      IVCORR = 0                                                        01770022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01780022
 6071 CONTINUE                                                          01790022
      IVTNUM = 607                                                      01800022
C                                                                       01810022
C      ****  TEST 607  ****                                             01820022
C     TEST 607  -  TEST OF A NEGATIVELY SIGNED ZERO COMPARED TO A       01830022
C     ZERO UNSIGNED BOTH VALUES SET AS INTEGER ARRAY ELEMENTS.          01840022
C                                                                       01850022
      IF (ICZERO) 36070, 6070, 36070                                    01860022
 6070 CONTINUE                                                          01870022
      IADN11(2) = -0                                                    01880022
      IADN11(3) = 0                                                     01890022
      ICON01 = 0                                                        01900022
      IF ( IADN11(2) .EQ. IADN11(3) )  ICON01 = 1                       01910022
      GO TO 46070                                                       01920022
36070 IVDELE = IVDELE + 1                                               01930022
      WRITE (I02,80003) IVTNUM                                          01940022
      IF (ICZERO) 46070, 6081, 46070                                    01950022
46070 IF ( ICON01 - 1 )  26070, 16070, 26070                            01960022
16070 IVPASS = IVPASS + 1                                               01970022
      WRITE (I02,80001) IVTNUM                                          01980022
      GO TO 6081                                                        01990022
26070 IVFAIL = IVFAIL + 1                                               02000022
      IVCOMP = ICON01                                                   02010022
      IVCORR = 1                                                        02020022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02030022
 6081 CONTINUE                                                          02040022
      IVTNUM = 608                                                      02050022
C                                                                       02060022
C      ****  TEST 608  ****                                             02070022
C     TEST 608  -  TEST OF SETTING ONE INTEGER ARRAY ELEMENT EQUAL TO   02080022
C     THE VALUE OF ANOTHER INTEGER ARRAY ELEMENT.  THE VALUE IS 32767.  02090022
C                                                                       02100022
      IF (ICZERO) 36080, 6080, 36080                                    02110022
 6080 CONTINUE                                                          02120022
      IADN11(1) = 32767                                                 02130022
      IADN12(5) = IADN11(1)                                             02140022
      IVCOMP = IADN12(5)                                                02150022
      GO TO 46080                                                       02160022
36080 IVDELE = IVDELE + 1                                               02170022
      WRITE (I02,80003) IVTNUM                                          02180022
      IF (ICZERO) 46080, 6091, 46080                                    02190022
46080 IF ( IVCOMP - 32767 )  26080, 16080, 26080                        02200022
16080 IVPASS = IVPASS + 1                                               02210022
      WRITE (I02,80001) IVTNUM                                          02220022
      GO TO 6091                                                        02230022
26080 IVFAIL = IVFAIL + 1                                               02240022
      IVCORR = 32767                                                    02250022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02260022
 6091 CONTINUE                                                          02270022
      IVTNUM = 609                                                      02280022
C                                                                       02290022
C      ****  TEST 609  ****                                             02300022
C     TEST 609  -  TEST OF AN ARRAY ELEMENT SET TO ANOTHER ARRAY ELEMENT02310022
C     WHICH HAD BEEN SET AT COMPILE TIME BY A DATA INITIALIZATION       02320022
C     STATEMENT.  AN INTEGER ARRAY IS USED WITH THE VALUE 3.            02330022
C                                                                       02340022
      IF (ICZERO) 36090, 6090, 36090                                    02350022
 6090 CONTINUE                                                          02360022
      IADN11(4) = IADN12(1)                                             02370022
      IVCOMP = IADN11(4)                                                02380022
      GO TO 46090                                                       02390022
36090 IVDELE = IVDELE + 1                                               02400022
      WRITE (I02,80003) IVTNUM                                          02410022
      IF (ICZERO) 46090, 6101, 46090                                    02420022
46090 IF ( IVCOMP - 3 )  26090, 16090, 26090                            02430022
16090 IVPASS = IVPASS + 1                                               02440022
      WRITE (I02,80001) IVTNUM                                          02450022
      GO TO 6101                                                        02460022
26090 IVFAIL = IVFAIL + 1                                               02470022
      IVCORR = 3                                                        02480022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02490022
 6101 CONTINUE                                                          02500022
      IVTNUM = 610                                                      02510022
C                                                                       02520022
C      ****  TEST 610  ****                                             02530022
C     TEST 610  -   TEST OF SETTING A REAL ARRAY ELEMENT TO A POSITIVE  02540022
C     VALUE IN A SIMPLE ASSIGNMENT STATEMENT.  VALUE IS 32767.          02550022
C                                                                       02560022
      IF (ICZERO) 36100, 6100, 36100                                    02570022
 6100 CONTINUE                                                          02580022
      RADN11(5) = 32767.                                                02590022
      IVCOMP = RADN11(5)                                                02600022
      GO TO 46100                                                       02610022
36100 IVDELE = IVDELE + 1                                               02620022
      WRITE (I02,80003) IVTNUM                                          02630022
      IF (ICZERO) 46100, 6111, 46100                                    02640022
46100 IF ( IVCOMP - 32767 )  26100, 16100, 26100                        02650022
16100 IVPASS = IVPASS + 1                                               02660022
      WRITE (I02,80001) IVTNUM                                          02670022
      GO TO 6111                                                        02680022
26100 IVFAIL = IVFAIL + 1                                               02690022
      IVCORR = 32767                                                    02700022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02710022
 6111 CONTINUE                                                          02720022
      IVTNUM = 611                                                      02730022
C                                                                       02740022
C      ****  TEST 611  ****                                             02750022
C     TEST 611  -  TEST OF SETTING A REAL ARRAY ELEMENT TO A NEGATIVE   02760022
C     VALUE IN A SIMPLE ASSIGNMENT STATEMENT.  VALUE IS -32766.         02770022
C                                                                       02780022
      IF (ICZERO) 36110, 6110, 36110                                    02790022
 6110 CONTINUE                                                          02800022
      RADN11(1) = -32766.                                               02810022
      IVCOMP = RADN11(1)                                                02820022
      GO TO 46110                                                       02830022
36110 IVDELE = IVDELE + 1                                               02840022
      WRITE (I02,80003) IVTNUM                                          02850022
      IF (ICZERO) 46110, 6121, 46110                                    02860022
46110 IF ( IVCOMP + 32766 )  26110, 16110, 26110                        02870022
16110 IVPASS = IVPASS + 1                                               02880022
      WRITE (I02,80001) IVTNUM                                          02890022
      GO TO 6121                                                        02900022
26110 IVFAIL = IVFAIL + 1                                               02910022
      IVCORR = -32766                                                   02920022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02930022
 6121 CONTINUE                                                          02940022
      IVTNUM = 612                                                      02950022
C                                                                       02960022
C      ****  TEST 612  ****                                             02970022
C     TEST 612  -  TEST OF SETTING A REAL ARRAY ELEMENT TO UNSIGNED ZERO02980022
C     IN A SIMPLE ASSIGNMENT STATEMENT.                                 02990022
C                                                                       03000022
      IF (ICZERO) 36120, 6120, 36120                                    03010022
 6120 CONTINUE                                                          03020022
      RADN11(3) = 0.                                                    03030022
      IVCOMP = RADN11(3)                                                03040022
      GO TO 46120                                                       03050022
36120 IVDELE = IVDELE + 1                                               03060022
      WRITE (I02,80003) IVTNUM                                          03070022
      IF (ICZERO) 46120, 6131, 46120                                    03080022
46120 IF ( IVCOMP - 0 )  26120, 16120, 26120                            03090022
16120 IVPASS = IVPASS + 1                                               03100022
      WRITE (I02,80001) IVTNUM                                          03110022
      GO TO 6131                                                        03120022
26120 IVFAIL = IVFAIL + 1                                               03130022
      IVCORR = 0                                                        03140022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03150022
 6131 CONTINUE                                                          03160022
      IVTNUM = 613                                                      03170022
C                                                                       03180022
C      ****  TEST 613  ****                                             03190022
C     TEST 613  -  TEST OF A NEGATIVELY SIGNED ZERO IN A REAL ARRAY     03200022
C     ELEMENT COMPARED TO A REAL ELEMENT SET TO AN UNSIGNED ZERO.       03210022
C                                                                       03220022
      IF (ICZERO) 36130, 6130, 36130                                    03230022
 6130 CONTINUE                                                          03240022
      RADN11(2) = -0.0                                                  03250022
      RADN11(3) = 0.0                                                   03260022
      ICON01 = 0                                                        03270022
      IF ( RADN11(2) .EQ. RADN11(3) )  ICON01 = 1                       03280022
      GO TO 46130                                                       03290022
36130 IVDELE = IVDELE + 1                                               03300022
      WRITE (I02,80003) IVTNUM                                          03310022
      IF (ICZERO) 46130, 6141, 46130                                    03320022
46130 IF ( ICON01 - 1 )  26130, 16130, 26130                            03330022
16130 IVPASS = IVPASS + 1                                               03340022
      WRITE (I02,80001) IVTNUM                                          03350022
      GO TO 6141                                                        03360022
26130 IVFAIL = IVFAIL + 1                                               03370022
      IVCOMP = ICON01                                                   03380022
      IVCORR = 1                                                        03390022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03400022
 6141 CONTINUE                                                          03410022
      IVTNUM = 614                                                      03420022
C                                                                       03430022
C      ****  TEST 614  ****                                             03440022
C     TEST 614  -  TEST OF SETTING ONE REAL ARRAY ELEMENT EQUAL TO THE  03450022
C     VALUE OF ANOTHER REAL ARRAY ELEMENT.  THE VALUE IS 32767.         03460022
C                                                                       03470022
      IF (ICZERO) 36140, 6140, 36140                                    03480022
 6140 CONTINUE                                                          03490022
      RADN11(1) = 32767.                                                03500022
      RADN12(5) = RADN11(1)                                             03510022
      IVCOMP = RADN12(5)                                                03520022
      GO TO 46140                                                       03530022
36140 IVDELE = IVDELE + 1                                               03540022
      WRITE (I02,80003) IVTNUM                                          03550022
      IF (ICZERO) 46140, 6151, 46140                                    03560022
46140 IF ( IVCOMP - 32767 )  26140, 16140, 26140                        03570022
16140 IVPASS = IVPASS + 1                                               03580022
      WRITE (I02,80001) IVTNUM                                          03590022
      GO TO 6151                                                        03600022
26140 IVFAIL = IVFAIL + 1                                               03610022
      IVCORR = 32767                                                    03620022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03630022
 6151 CONTINUE                                                          03640022
      IVTNUM = 615                                                      03650022
C                                                                       03660022
C      ****  TEST 615  ****                                             03670022
C     TEST 615  -  TEST OF A REAL ARRAY ELEMENT SET TO ANOTHER REAL     03680022
C     ARRAY ELEMENT WHICH HAD BEEN SET AT COMPILE TIME BY A DATA        03690022
C     INITIALIZATION STATEMENT. THE VALUE IS -512.                      03700022
C                                                                       03710022
      IF (ICZERO) 36150, 6150, 36150                                    03720022
 6150 CONTINUE                                                          03730022
      RADN11(4) = RADN12(1)                                             03740022
      IVCOMP = RADN11(4)                                                03750022
      GO TO 46150                                                       03760022
36150 IVDELE = IVDELE + 1                                               03770022
      WRITE (I02,80003) IVTNUM                                          03780022
      IF (ICZERO) 46150, 6161, 46150                                    03790022
46150 IF ( IVCOMP + 512 )  26150, 16150, 26150                          03800022
16150 IVPASS = IVPASS + 1                                               03810022
      WRITE (I02,80001) IVTNUM                                          03820022
      GO TO 6161                                                        03830022
26150 IVFAIL = IVFAIL + 1                                               03840022
      IVCORR = - 512                                                    03850022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03860022
 6161 CONTINUE                                                          03870022
      IVTNUM = 616                                                      03880022
C                                                                       03890022
C      ****  TEST 616  ****                                             03900022
C     TEST 616  -  TEST OF SETTING THE VALUE OF AN INTEGER ARRAY ELEMENT03910022
C     BY AN ARITHMETIC EXPRESSION.                                      03920022
C                                                                       03930022
      IF (ICZERO) 36160, 6160, 36160                                    03940022
 6160 CONTINUE                                                          03950022
      ICON01 = 1                                                        03960022
      IADN11(3) = ICON01 + 1                                            03970022
      IVCOMP = IADN11(3)                                                03980022
      GO TO 46160                                                       03990022
36160 IVDELE = IVDELE + 1                                               04000022
      WRITE (I02,80003) IVTNUM                                          04010022
      IF (ICZERO) 46160, 6171, 46160                                    04020022
46160 IF ( IVCOMP - 2 )  26160, 16160, 26160                            04030022
16160 IVPASS = IVPASS + 1                                               04040022
      WRITE (I02,80001) IVTNUM                                          04050022
      GO TO 6171                                                        04060022
26160 IVFAIL = IVFAIL + 1                                               04070022
      IVCORR = 2                                                        04080022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04090022
 6171 CONTINUE                                                          04100022
      IVTNUM = 617                                                      04110022
C                                                                       04120022
C      ****  TEST 617  ****                                             04130022
C     TEST 617  -  TEST OF SETTING THE VALUE OF A REAL ARRAY ELEMENT    04140022
C     BY AN ARITHMETIC EXPRESSION.                                      04150022
C                                                                       04160022
      IF (ICZERO) 36170, 6170, 36170                                    04170022
 6170 CONTINUE                                                          04180022
      RCON01 = 1.                                                       04190022
      RADN11(3) = RCON01 + 1.                                           04200022
      IVCOMP = RADN11(3)                                                04210022
      GO TO 46170                                                       04220022
36170 IVDELE = IVDELE + 1                                               04230022
      WRITE (I02,80003) IVTNUM                                          04240022
      IF (ICZERO) 46170, 6181, 46170                                    04250022
46170 IF ( IVCOMP - 2 )  26170, 16170, 26170                            04260022
16170 IVPASS = IVPASS + 1                                               04270022
      WRITE (I02,80001) IVTNUM                                          04280022
      GO TO 6181                                                        04290022
26170 IVFAIL = IVFAIL + 1                                               04300022
      IVCORR = 2                                                        04310022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04320022
 6181 CONTINUE                                                          04330022
      IVTNUM = 618                                                      04340022
C                                                                       04350022
C      ****  TEST 618  ****                                             04360022
C     TEST 618  -  TEST OF SETTING THE VALUE OF AN INTEGER ARRAY ELEMENT04370022
C     TO ANOTHER INTEGER ARRAY ELEMENT AND CHANGING THE SIGN.           04380022
C                                                                       04390022
      IF (ICZERO) 36180, 6180, 36180                                    04400022
 6180 CONTINUE                                                          04410022
      IADN11(2) = 32766                                                 04420022
      IADN11(4) = - IADN11(2)                                           04430022
      IVCOMP = IADN11(4)                                                04440022
      GO TO 46180                                                       04450022
36180 IVDELE = IVDELE + 1                                               04460022
      WRITE (I02,80003) IVTNUM                                          04470022
      IF (ICZERO) 46180, 6191, 46180                                    04480022
46180 IF ( IVCOMP + 32766 )  26180, 16180, 26180                        04490022
16180 IVPASS = IVPASS + 1                                               04500022
      WRITE (I02,80001) IVTNUM                                          04510022
      GO TO 6191                                                        04520022
26180 IVFAIL = IVFAIL + 1                                               04530022
      IVCORR = -32766                                                   04540022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04550022
 6191 CONTINUE                                                          04560022
      IVTNUM = 619                                                      04570022
C                                                                       04580022
C      ****  TEST 619  ****                                             04590022
C     TEST 619  -  TEST OF SETTING THE VALUE OF A REAL ARRAY ELEMENT    04600022
C     TO THE VALUE OF ANOTHER REAL ARRAY ELEMENT AND CHANGING THE SIGN. 04610022
C                                                                       04620022
      IF (ICZERO) 36190, 6190, 36190                                    04630022
 6190 CONTINUE                                                          04640022
      RADN11(2) = 32766.                                                04650022
      RADN11(4) = - RADN11(2)                                           04660022
      IVCOMP = RADN11(4)                                                04670022
      GO TO 46190                                                       04680022
36190 IVDELE = IVDELE + 1                                               04690022
      WRITE (I02,80003) IVTNUM                                          04700022
      IF (ICZERO) 46190, 6201, 46190                                    04710022
46190 IF ( IVCOMP + 32766 )  26190, 16190, 26190                        04720022
16190 IVPASS = IVPASS + 1                                               04730022
      WRITE (I02,80001) IVTNUM                                          04740022
      GO TO 6201                                                        04750022
26190 IVFAIL = IVFAIL + 1                                               04760022
      IVCORR = -32766                                                   04770022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04780022
 6201 CONTINUE                                                          04790022
      IVTNUM = 620                                                      04800022
C                                                                       04810022
C      ****  TEST 620  ****                                             04820022
C     TEST 620  -  TEST OF SETTING THE VALUE OF A LOGICAL ARRAY ELEMENT 04830022
C     TO THE VALUE OF ANOTHER LOGICAL ARRAY ELEMENT.                    04840022
C                                                                       04850022
      IF (ICZERO) 36200, 6200, 36200                                    04860022
 6200 CONTINUE                                                          04870022
      LADN11(1) = .TRUE.                                                04880022
      LADN12(1) = LADN11(1)                                             04890022
      ICON01 = 0                                                        04900022
      IF ( LADN12(1) )  ICON01 = 1                                      04910022
      GO TO 46200                                                       04920022
36200 IVDELE = IVDELE + 1                                               04930022
      WRITE (I02,80003) IVTNUM                                          04940022
      IF (ICZERO) 46200, 6211, 46200                                    04950022
46200 IF ( ICON01 - 1 )  26200, 16200, 26200                            04960022
16200 IVPASS = IVPASS + 1                                               04970022
      WRITE (I02,80001) IVTNUM                                          04980022
      GO TO 6211                                                        04990022
26200 IVFAIL = IVFAIL + 1                                               05000022
      IVCOMP = ICON01                                                   05010022
      IVCORR = 1                                                        05020022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05030022
 6211 CONTINUE                                                          05040022
      IVTNUM = 621                                                      05050022
C                                                                       05060022
C      ****  TEST 621  ****                                             05070022
C     TEST 621  -  TEST OF SETTING THE VALUE OF A LOGICAL ARRAY ELEMENT 05080022
C     TO THE VALUE OF ANOTHER LOGICAL ARRAY ELEMENT AND CHANGING        05090022
C     THE VALUE FROM  .TRUE.  TO  .FALSE. BY USING THE .NOT. STATEMENT. 05100022
C                                                                       05110022
      IF (ICZERO) 36210, 6210, 36210                                    05120022
 6210 CONTINUE                                                          05130022
      LADN11(2) = .TRUE.                                                05140022
      LADN12(2) = .NOT. LADN11(2)                                       05150022
      ICON01 = 1                                                        05160022
      IF ( LADN12(2) )  ICON01 = 0                                      05170022
      GO TO 46210                                                       05180022
36210 IVDELE = IVDELE + 1                                               05190022
      WRITE (I02,80003) IVTNUM                                          05200022
      IF (ICZERO) 46210, 6221, 46210                                    05210022
46210 IF ( ICON01 - 1 )  26210, 16210, 26210                            05220022
16210 IVPASS = IVPASS + 1                                               05230022
      WRITE (I02,80001) IVTNUM                                          05240022
      GO TO 6221                                                        05250022
26210 IVFAIL = IVFAIL + 1                                               05260022
      IVCOMP = ICON01                                                   05270022
      IVCORR = 1                                                        05280022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05290022
 6221 CONTINUE                                                          05300022
      IVTNUM = 622                                                      05310022
C                                                                       05320022
C      ****  TEST 622  ****                                             05330022
C     TEST 622  -  TEST OF THE TYPE STATEMENT AND THE DATA              05340022
C     INITIALIZATION STATEMENT.  THE EXPLICITLY REAL ARRAY ELEMENT      05350022
C     SHOULD HAVE THE VALUE OF .5                                       05360022
C                                                                       05370022
      IF (ICZERO) 36220, 6220, 36220                                    05380022
 6220 CONTINUE                                                          05390022
      IVCOMP = 2. * IADN13(1)                                           05400022
      GO TO 46220                                                       05410022
36220 IVDELE = IVDELE + 1                                               05420022
      WRITE (I02,80003) IVTNUM                                          05430022
      IF (ICZERO) 46220, 6231, 46220                                    05440022
46220 IF ( IVCOMP - 1 )  26220, 16220, 26220                            05450022
16220 IVPASS = IVPASS + 1                                               05460022
      WRITE (I02,80001) IVTNUM                                          05470022
      GO TO 6231                                                        05480022
26220 IVFAIL = IVFAIL + 1                                               05490022
      IVCORR = 1                                                        05500022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05510022
 6231 CONTINUE                                                          05520022
      IVTNUM = 623                                                      05530022
C                                                                       05540022
C      ****  TEST 623  ****                                             05550022
C     TEST 623  -  TEST OF REAL TO INTEGER CONVERSION USING ARRAYS.     05560022
C     THE INITIALIZED VALUE OF 0.5 SHOULD BE TRUNCATED TO ZERO.         05570022
C                                                                       05580022
      IF (ICZERO) 36230, 6230, 36230                                    05590022
 6230 CONTINUE                                                          05600022
      IADN11(1) = IADN13(1)                                             05610022
      IVCOMP = IADN11(1)                                                05620022
      GO TO 46230                                                       05630022
36230 IVDELE = IVDELE + 1                                               05640022
      WRITE (I02,80003) IVTNUM                                          05650022
      IF (ICZERO) 46230, 6241, 46230                                    05660022
46230 IF ( IVCOMP - 0 )  26230, 16230, 26230                            05670022
16230 IVPASS = IVPASS + 1                                               05680022
      WRITE (I02,80001) IVTNUM                                          05690022
      GO TO 6241                                                        05700022
26230 IVFAIL = IVFAIL + 1                                               05710022
      IVCORR = 0                                                        05720022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05730022
 6241 CONTINUE                                                          05740022
      IVTNUM = 624                                                      05750022
C                                                                       05760022
C      ****  TEST 624  ****                                             05770022
C     TEST 624  -  TEST OF THE COMMON STATEMENT BY SETTING THE VALUE OF 05780022
C     AN INTEGER ARRAY ELEMENT IN A DIMENSIONED ARRAY TO THE VALUE      05790022
C     OF A REAL ARRAY ELEMENT IN COMMON.  THE ELEMENT IN COMMON HAD ITS 05800022
C     VALUE SET IN A SIMPLE ASSIGNMENT STATEMENT TO 9999.               05810022
C                                                                       05820022
      IF (ICZERO) 36240, 6240, 36240                                    05830022
 6240 CONTINUE                                                          05840022
      RADN14(1) = 9999.                                                 05850022
      IADN11(1) = RADN14(1)                                             05860022
      IVCOMP = IADN11(1)                                                05870022
      GO TO 46240                                                       05880022
36240 IVDELE = IVDELE + 1                                               05890022
      WRITE (I02,80003) IVTNUM                                          05900022
      IF (ICZERO) 46240, 6251, 46240                                    05910022
46240 IF ( IVCOMP - 9999 )  26240, 16240, 26240                         05920022
16240 IVPASS = IVPASS + 1                                               05930022
      WRITE (I02,80001) IVTNUM                                          05940022
      GO TO 6251                                                        05950022
26240 IVFAIL = IVFAIL + 1                                               05960022
      IVCORR = 9999                                                     05970022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05980022
 6251 CONTINUE                                                          05990022
      IVTNUM = 625                                                      06000022
C                                                                       06010022
C      ****  TEST 625  ****                                             06020022
C     TEST 625  -  TEST OF SETTING THE VALUE OF AN INTEGER ARRAY ELEMENT06030022
C     IN COMMON TO THE VALUE OF A REAL ARRAY ELEMENT ALSO IN BLANK      06040022
C     COMMON AND CHANGING THE SIGN.  THE VALUE USED IS 9999.            06050022
C                                                                       06060022
      IF (ICZERO) 36250, 6250, 36250                                    06070022
 6250 CONTINUE                                                          06080022
      RADN14(1) = 9999.                                                 06090022
      IADN14(1) = - RADN14(1)                                           06100022
      IVCOMP = IADN14(1)                                                06110022
      GO TO 46250                                                       06120022
36250 IVDELE = IVDELE + 1                                               06130022
      WRITE (I02,80003) IVTNUM                                          06140022
      IF (ICZERO) 46250, 6261, 46250                                    06150022
46250 IF ( IVCOMP + 9999 ) 26250, 16250, 26250                          06160022
16250 IVPASS = IVPASS + 1                                               06170022
      WRITE (I02,80001) IVTNUM                                          06180022
      GO TO 6261                                                        06190022
26250 IVFAIL = IVFAIL + 1                                               06200022
      IVCORR = - 9999                                                   06210022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06220022
 6261 CONTINUE                                                          06230022
      IVTNUM = 626                                                      06240022
C                                                                       06250022
C      ****  TEST 626  ****                                             06260022
C     TEST 626  -  TEST OF SETTING THE VALUE OF A LOGICAL ARRAY ELEMENT 06270022
C     IN BLANK COMMON TO  .NOT.  .TRUE.                                 06280022
C     THE VALUE OF ANOTHER LOGICAL ARRAY ELEMENT ALSO IN COMMON IS THEN 06290022
C     SET TO .NOT. OF THE VALUE OF THE FIRST.                           06300022
C     VALUE OF THE FIRST ELEMENT SHOULD BE .FALSE.                      06310022
C     VALUE OF THE SECOND ELEMENT SHOULD BE .TRUE.                      06320022
C                                                                       06330022
      IF (ICZERO) 36260, 6260, 36260                                    06340022
 6260 CONTINUE                                                          06350022
      LADN13(1) = .NOT. .TRUE.                                          06360022
      LADN13(2) = .NOT. LADN13(1)                                       06370022
      ICON01 = 0                                                        06380022
      IF ( LADN13(2) )  ICON01 = 1                                      06390022
      GO TO 46260                                                       06400022
36260 IVDELE = IVDELE + 1                                               06410022
      WRITE (I02,80003) IVTNUM                                          06420022
      IF (ICZERO) 46260, 6271, 46260                                    06430022
46260 IF ( ICON01 - 1 )  26260, 16260, 26260                            06440022
16260 IVPASS = IVPASS + 1                                               06450022
      WRITE (I02,80001) IVTNUM                                          06460022
      GO TO 6271                                                        06470022
26260 IVFAIL = IVFAIL + 1                                               06480022
      IVCOMP = ICON01                                                   06490022
      IVCORR = 1                                                        06500022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06510022
 6271 CONTINUE                                                          06520022
      IVTNUM = 627                                                      06530022
C                                                                       06540022
C      ****  TEST 627  ****                                             06550022
C     TEST 627  -  TEST OF EQUIVALENCE ON THE FIRST ELEMENTS OF INTEGER 06560022
C     ARRAYS ONE OF WHICH IS IN COMMON AND THE OTHER ONE IS DIMENSIONED.06570022
C                                                                       06580022
      IF (ICZERO) 36270, 6270, 36270                                    06590022
 6270 CONTINUE                                                          06600022
      IADN14(2) = 32767                                                 06610022
      IVCOMP = IADN15(2)                                                06620022
      GO TO 46270                                                       06630022
36270 IVDELE = IVDELE + 1                                               06640022
      WRITE (I02,80003) IVTNUM                                          06650022
      IF (ICZERO) 46270, 6281, 46270                                    06660022
46270 IF ( IVCOMP - 32767 )  26270, 16270, 26270                        06670022
16270 IVPASS = IVPASS + 1                                               06680022
      WRITE (I02,80001) IVTNUM                                          06690022
      GO TO 6281                                                        06700022
26270 IVFAIL = IVFAIL + 1                                               06710022
      IVCORR = 32767                                                    06720022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06730022
 6281 CONTINUE                                                          06740022
      IVTNUM = 628                                                      06750022
C                                                                       06760022
C      ****  TEST 628  ****                                             06770022
C     TEST 628  -  TEST OF EQUIVALENCE ON REAL ARRAYS ONE OF WHICH IS   06780022
C     IN COMMON AND THE OTHER ONE IS DIMENSIONED.  THE ARRAYS WERE      06790022
C     ALIGNED ON THEIR SECOND ELEMENTS.                                 06800022
C                                                                       06810022
      IF (ICZERO) 36280, 6280, 36280                                    06820022
 6280 CONTINUE                                                          06830022
      RADN15(1) = -32766.                                               06840022
      IVCOMP = RADN14(1)                                                06850022
      GO TO 46280                                                       06860022
36280 IVDELE = IVDELE + 1                                               06870022
      WRITE (I02,80003) IVTNUM                                          06880022
      IF (ICZERO) 46280, 6291, 46280                                    06890022
46280 IF ( IVCOMP + 32766 )  26280, 16280, 26280                        06900022
16280 IVPASS = IVPASS + 1                                               06910022
      WRITE (I02,80001) IVTNUM                                          06920022
      GO TO 6291                                                        06930022
26280 IVFAIL = IVFAIL + 1                                               06940022
      IVCORR = -32766                                                   06950022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06960022
 6291 CONTINUE                                                          06970022
      IVTNUM = 629                                                      06980022
C                                                                       06990022
C      ****  TEST 629  ****                                             07000022
C     TEST 629  -  TEST OF EQUIVALENCE WITH LOGICAL ELEMENTS.  AN ARRAY 07010022
C     ELEMENT IN COMMON IS EQUIVALENCED TO A LOGICAL VARIABLE.          07020022
C                                                                       07030022
      IF (ICZERO) 36290, 6290, 36290                                    07040022
 6290 CONTINUE                                                          07050022
      LADN13(2) = .TRUE.                                                07060022
      LCTN01 = .NOT. LADN13(2)                                          07070022
      ICON01 = 1                                                        07080022
      IF ( LADN13(1) )  ICON01 = 0                                      07090022
      GO TO 46290                                                       07100022
36290 IVDELE = IVDELE + 1                                               07110022
      WRITE (I02,80003) IVTNUM                                          07120022
      IF (ICZERO) 46290, 6301, 46290                                    07130022
46290 IF ( ICON01 - 1 )  26290, 16290, 26290                            07140022
16290 IVPASS = IVPASS + 1                                               07150022
      WRITE (I02,80001) IVTNUM                                          07160022
      GO TO 6301                                                        07170022
26290 IVFAIL = IVFAIL + 1                                               07180022
      IVCOMP = ICON01                                                   07190022
      IVCORR = 1                                                        07200022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07210022
 6301 CONTINUE                                                          07220022
      IVTNUM = 630                                                      07230022
C                                                                       07240022
C      ****  TEST 630  ****                                             07250022
C     TEST 630  -  TEST OF EQUIVALENCE WITH REAL AND INTEGER ELEMENTS   07260022
C     WHICH ARE EQUIVALENCED TO ARRAY ELEMENTS IN COMMON.               07270022
C                                                                       07280022
      IF (ICZERO) 36300, 6300, 36300                                    07290022
 6300 CONTINUE                                                          07300022
      RCON01 = 1.                                                       07310022
      ICON02 = - RADN14(5)                                              07320022
      IVCOMP = IADN14(5)                                                07330022
      GO TO 46300                                                       07340022
36300 IVDELE = IVDELE + 1                                               07350022
      WRITE (I02,80003) IVTNUM                                          07360022
      IF (ICZERO) 46300, 6311, 46300                                    07370022
46300 IF ( IVCOMP + 1 )  26300, 16300, 26300                            07380022
16300 IVPASS = IVPASS + 1                                               07390022
      WRITE (I02,80001) IVTNUM                                          07400022
      GO TO 6311                                                        07410022
26300 IVFAIL = IVFAIL + 1                                               07420022
      IVCORR = -1                                                       07430022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07440022
 6311 CONTINUE                                                          07450022
      IVTNUM = 631                                                      07460022
C                                                                       07470022
C      ****  TEST 631  ****                                             07480022
C     TEST 631  -  TEST OF EQUIVALENCE ON INTEGER ARRAY ELEMENTS.       07490022
C     BOTH ARRAYS ARE DIMENSIONED.  THE FOURTH ELEMENT                  07500022
C     OF THE FIRST OF THE ARRAYS SHOULD BE EQUAL TO THE THIRD ELEMENT OF07510022
C     THE SECOND ARRAY.                                                 07520022
C                                                                       07530022
      IF (ICZERO) 36310, 6310, 36310                                    07540022
 6310 CONTINUE                                                          07550022
      IADN16(4) = 9999                                                  07560022
      IVCOMP = IADN17(3)                                                07570022
      GO TO 46310                                                       07580022
36310 IVDELE = IVDELE + 1                                               07590022
      WRITE (I02,80003) IVTNUM                                          07600022
      IF (ICZERO) 46310, 6321, 46310                                    07610022
46310 IF ( IVCOMP - 9999 )  26310, 16310, 26310                         07620022
16310 IVPASS = IVPASS + 1                                               07630022
      WRITE (I02,80001) IVTNUM                                          07640022
      GO TO 6321                                                        07650022
26310 IVFAIL = IVFAIL + 1                                               07660022
      IVCORR = 9999                                                     07670022
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07680022
 6321 CONTINUE                                                          07690022
C                                                                       07700022
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07710022
99999 CONTINUE                                                          07720022
      WRITE (I02,90002)                                                 07730022
      WRITE (I02,90006)                                                 07740022
      WRITE (I02,90002)                                                 07750022
      WRITE (I02,90002)                                                 07760022
      WRITE (I02,90007)                                                 07770022
      WRITE (I02,90002)                                                 07780022
      WRITE (I02,90008)  IVFAIL                                         07790022
      WRITE (I02,90009) IVPASS                                          07800022
      WRITE (I02,90010) IVDELE                                          07810022
C                                                                       07820022
C                                                                       07830022
C     TERMINATE ROUTINE EXECUTION                                       07840022
      STOP                                                              07850022
C                                                                       07860022
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07870022
90000 FORMAT (1H1)                                                      07880022
90002 FORMAT (1H )                                                      07890022
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07900022
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07910022
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07920022
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07930022
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07940022
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07950022
C                                                                       07960022
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07970022
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07980022
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07990022
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08000022
C                                                                       08010022
C     FORMAT STATEMENTS FOR TEST RESULTS                                08020022
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08030022
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08040022
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08050022
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08060022
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08070022
C                                                                       08080022
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM022)                          08090022
      END                                                               08100022
