C                                                                       00010020
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020020
C                                                                       00030020
C     FM020                                                             00040020
C                                                                       00050020
C             THIS ROUTINE TESTS THE FORTRAN IN-LINE STATEMENT FUNCTION 00060020
C     OF TYPE LOGICAL AND INTEGER.  INTEGER CONSTANTS, LOGICAL CONSTANTS00070020
C     INTEGER VARIABLES, LOGICAL VARIABLES, INTEGER ARITHMETIC EXPRESS- 00080020
C     IONS ARE ALL USED TO TEST THE STATEMENT FUNCTION DEFINITION AND   00090020
C     THE VALUE RETURNED FOR THE STATEMENT FUNCTION WHEN IT IS USED     00100020
C     IN THE MAIN BODY OF THE PROGRAM.                                  00110020
C                                                                       00120020
C      REFERENCES                                                       00130020
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140020
C              X3.9-1978                                                00150020
C                                                                       00160020
C        SECTION 8.4.1, INTEGER, REAL, DOUBLE PRECISION, COMPLEX, AND   00170020
C                       LOGICAL TYPE-STATEMENTS                         00180020
C        SECTION 15.3.2, INTRINSIC FUNCTION REFERENCES                  00190020
C        SECTION 15.4, STATEMENT FUNCTIONS                              00200020
C        SECTION 15.4.1, FORMS OF A FUNCTION STATEMENT                  00210020
C        SECTION 15.4.2, REFERENCING A STATEMENT FUNCTION               00220020
C        SECTION 15.5.2, EXTERNAL FUNCTION REFERENCES                   00230020
C                                                                       00240020
      LOGICAL LFTN01, LDTN01                                            00250020
      LOGICAL LFTN02, LDTN02                                            00260020
      LOGICAL LFTN03, LDTN03, LCTN03                                    00270020
      LOGICAL LFTN04, LDTN04, LCTN04                                    00280020
      DIMENSION IADN11(2)                                               00290020
C                                                                       00300020
C..... TEST 553                                                         00310020
      IFON01(IDON01) = 32767                                            00320020
C                                                                       00330020
C..... TEST 554                                                         00340020
      LFTN01(LDTN01) = .TRUE.                                           00350020
C                                                                       00360020
C..... TEST 555                                                         00370020
      IFON02 ( IDON02 ) = IDON02                                        00380020
C                                                                       00390020
C..... TEST 556                                                         00400020
      LFTN02( LDTN02 ) = LDTN02                                         00410020
C                                                                       00420020
C..... TEST 557                                                         00430020
      IFON03 (IDON03 )= IDON03                                          00440020
C                                                                       00450020
C..... TEST 558                                                         00460020
      LFTN03(LDTN03) = LDTN03                                           00470020
C                                                                       00480020
C..... TEST 559                                                         00490020
      LFTN04(LDTN04) = .NOT. LDTN04                                     00500020
C                                                                       00510020
C..... TEST 560                                                         00520020
      IFON04(IDON04) = IDON04 ** 2                                      00530020
C                                                                       00540020
C..... TEST 561                                                         00550020
      IFON05(IDON05, IDON06) = IDON05 + IDON06                          00560020
C                                                                       00570020
C..... TEST 562                                                         00580020
      IFON06(IDON07, IDON08) = SQRT(FLOAT(IDON07**2)+FLOAT(IDON08**2))  00590020
C                                                                       00600020
C..... TEST 563                                                         00610020
      IFON07(IDON09) = IDON09 ** 2                                      00620020
      IFON08(I,J)=SQRT(FLOAT(IFON07(I))+FLOAT(IFON07(J)))               00630020
C                                                                       00640020
C..... TEST 564                                                         00650020
      IFON09(K,L) = K / L + K ** L - K * L                              00660020
C                                                                       00670020
C                                                                       00680020
C                                                                       00690020
C      **********************************************************       00700020
C                                                                       00710020
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00720020
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00730020
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00740020
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00750020
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00760020
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00770020
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00780020
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00790020
C     OF EXECUTING THESE TESTS.                                         00800020
C                                                                       00810020
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00820020
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00830020
C                                                                       00840020
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00850020
C                                                                       00860020
C                  DEPARTMENT OF THE NAVY                               00870020
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00880020
C                  WASHINGTON, D.C.  20376                              00890020
C                                                                       00900020
C      **********************************************************       00910020
C                                                                       00920020
C                                                                       00930020
C                                                                       00940020
C     INITIALIZATION SECTION                                            00950020
C                                                                       00960020
C     INITIALIZE CONSTANTS                                              00970020
C      **************                                                   00980020
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00990020
      I01 = 5                                                           01000020
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01010020
      I02 = 6                                                           01020020
C     SYSTEM ENVIRONMENT SECTION                                        01030020
C                                                                       01040020
      I01 = 5                                                           01050020
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01060020
C     (UNIT NUMBER FOR CARD READER).                                    01070020
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 01080020
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01090020
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01100020
C                                                                       01110020
      I02 = 6                                                           01120020
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01130020
C     (UNIT NUMBER FOR PRINTER).                                        01140020
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01150020
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01160020
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01170020
C                                                                       01180020
      IVPASS=0                                                          01190020
      IVFAIL=0                                                          01200020
      IVDELE=0                                                          01210020
      ICZERO=0                                                          01220020
C                                                                       01230020
C     WRITE PAGE HEADERS                                                01240020
      WRITE (I02,90000)                                                 01250020
      WRITE (I02,90001)                                                 01260020
      WRITE (I02,90002)                                                 01270020
      WRITE (I02, 90002)                                                01280020
      WRITE (I02,90003)                                                 01290020
      WRITE (I02,90002)                                                 01300020
      WRITE (I02,90004)                                                 01310020
      WRITE (I02,90002)                                                 01320020
      WRITE (I02,90011)                                                 01330020
      WRITE (I02,90002)                                                 01340020
      WRITE (I02,90002)                                                 01350020
      WRITE (I02,90005)                                                 01360020
      WRITE (I02,90006)                                                 01370020
      WRITE (I02,90002)                                                 01380020
      IVTNUM = 553                                                      01390020
C                                                                       01400020
C      ****  TEST 553  ****                                             01410020
C     TEST 553  -  THE VALUE OF THE INTEGER FUNCTION IS SET TO A        01420020
C         CONSTANT OF 32767 REGARDLESS OF THE VALUE OF THE ARGUEMENT    01430020
C     SUPPLIED TO THE DUMMY ARGUEMENT.  TEST OF POSITIVE INTEGER        01440020
C     CONSTANTS FOR A STATEMENT FUNCTION.                               01450020
C                                                                       01460020
C                                                                       01470020
      IF (ICZERO) 35530, 5530, 35530                                    01480020
 5530 CONTINUE                                                          01490020
      IVCOMP = IFON01(3)                                                01500020
      GO TO 45530                                                       01510020
35530 IVDELE = IVDELE + 1                                               01520020
      WRITE (I02,80003) IVTNUM                                          01530020
      IF (ICZERO) 45530, 5541, 45530                                    01540020
45530 IF ( IVCOMP - 32767 )  25530, 15530, 25530                        01550020
15530 IVPASS = IVPASS + 1                                               01560020
      WRITE (I02,80001) IVTNUM                                          01570020
      GO TO 5541                                                        01580020
25530 IVFAIL = IVFAIL + 1                                               01590020
      IVCORR = 32767                                                    01600020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01610020
 5541 CONTINUE                                                          01620020
      IVTNUM = 554                                                      01630020
C                                                                       01640020
C      ****  TEST 554  ****                                             01650020
C     TEST 554  -  TEST OF THE STATEMENT FUNCTION OF TYPE LOGICAL       01660020
C         SET TO THE LOGICAL CONSTANT .TRUE. REGARDLESS OF THE          01670020
C     ARGUEMENT SUPPLIED TO THE DUMMY ARGUEMENT.                        01680020
C     A LOGICAL    IF STATEMENT IS USED IN CONJUNCTION WITH THE LOGICAL 01690020
C     STATEMENT FUNCTION.  THE TRUE PATH IS TESTED.                     01700020
C                                                                       01710020
C                                                                       01720020
      IF (ICZERO) 35540, 5540, 35540                                    01730020
 5540 CONTINUE                                                          01740020
      IVON01 = 0                                                        01750020
      IF ( LFTN01(.FALSE.) )  IVON01 = 1                                01760020
      GO TO 45540                                                       01770020
35540 IVDELE = IVDELE + 1                                               01780020
      WRITE (I02,80003) IVTNUM                                          01790020
      IF (ICZERO) 45540, 5551, 45540                                    01800020
45540 IF ( IVON01 - 1 )  25540, 15540, 25540                            01810020
15540 IVPASS = IVPASS + 1                                               01820020
      WRITE (I02,80001) IVTNUM                                          01830020
      GO TO 5551                                                        01840020
25540 IVFAIL = IVFAIL + 1                                               01850020
      IVCOMP = IVON01                                                   01860020
      IVCORR = 1                                                        01870020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01880020
 5551 CONTINUE                                                          01890020
      IVTNUM = 555                                                      01900020
C                                                                       01910020
C      ****  TEST 555  ****                                             01920020
C     TEST 555  -  THE INTEGER STATEMENT FUNCTION IS SET TO THE VALUE   01930020
C         OF THE ARGEUMENT SUPPLIED.                                    01940020
C                                                                       01950020
C                                                                       01960020
      IF (ICZERO) 35550, 5550, 35550                                    01970020
 5550 CONTINUE                                                          01980020
      IVCOMP = IFON02 ( 32767 )                                         01990020
      GO TO 45550                                                       02000020
35550 IVDELE = IVDELE + 1                                               02010020
      WRITE (I02,80003) IVTNUM                                          02020020
      IF (ICZERO) 45550, 5561, 45550                                    02030020
45550 IF ( IVCOMP - 32767 )  25550, 15550, 25550                        02040020
15550 IVPASS = IVPASS + 1                                               02050020
      WRITE (I02,80001) IVTNUM                                          02060020
      GO TO 5561                                                        02070020
25550 IVFAIL = IVFAIL + 1                                               02080020
      IVCORR = 32767                                                    02090020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02100020
 5561 CONTINUE                                                          02110020
      IVTNUM = 556                                                      02120020
C                                                                       02130020
C      ****  TEST 556  ****                                             02140020
C     TEST 556  -  TEST OF A LOGICAL STATEMENT FUNCTION SET TO THE      02150020
C         VALUE OF THE ARGUEMENT SUPPLIED.  THE FALSE PATH OF A LOGICAL 02160020
C            IF STATEMENT IS USED IN CONJUNCTION WITH THE LOGICAL       02170020
C         STATEMENT FUNCTION.                                           02180020
C                                                                       02190020
C                                                                       02200020
      IF (ICZERO) 35560, 5560, 35560                                    02210020
 5560 CONTINUE                                                          02220020
      IVON01 = 1                                                        02230020
      IF ( LFTN02(.FALSE.) )  IVON01 = 0                                02240020
      GO TO 45560                                                       02250020
35560 IVDELE = IVDELE + 1                                               02260020
      WRITE (I02,80003) IVTNUM                                          02270020
      IF (ICZERO) 45560, 5571, 45560                                    02280020
45560 IF ( IVON01 - 1 )  25560, 15560, 25560                            02290020
15560 IVPASS = IVPASS + 1                                               02300020
      WRITE (I02,80001) IVTNUM                                          02310020
      GO TO 5571                                                        02320020
25560 IVFAIL = IVFAIL + 1                                               02330020
      IVCOMP = IVON01                                                   02340020
      IVCORR = 1                                                        02350020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02360020
 5571 CONTINUE                                                          02370020
      IVTNUM = 557                                                      02380020
C                                                                       02390020
C      ****  TEST 557  ****                                             02400020
C     TEST 557  -  THE VALUE OF AN INTEGER FUNCTION IS SET EQUAL TO     02410020
C         VALUE OF THE ARGUEMENT SUPPLIED.  THIS VALUE IS AN INTEGER    02420020
C         VARIABLE SET TO 32767.                                        02430020
C                                                                       02440020
C                                                                       02450020
      IF (ICZERO) 35570, 5570, 35570                                    02460020
 5570 CONTINUE                                                          02470020
      ICON01 = 32767                                                    02480020
      IVCOMP = IFON03 ( ICON01 )                                        02490020
      GO TO 45570                                                       02500020
35570 IVDELE = IVDELE + 1                                               02510020
      WRITE (I02,80003) IVTNUM                                          02520020
      IF (ICZERO) 45570, 5581, 45570                                    02530020
45570 IF ( IVCOMP - 32767 )  25570, 15570, 25570                        02540020
15570 IVPASS = IVPASS + 1                                               02550020
      WRITE (I02,80001) IVTNUM                                          02560020
      GO TO 5581                                                        02570020
25570 IVFAIL = IVFAIL + 1                                               02580020
      IVCORR = 32767                                                    02590020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02600020
 5581 CONTINUE                                                          02610020
      IVTNUM = 558                                                      02620020
C                                                                       02630020
C      ****  TEST 558  ****                                             02640020
C     TEST 558 -  A LOGICAL STATEMENT FUNCTION IS SET EQUAL TO THE      02650020
C         VALUE OF THE ARGUEMENT SUPPLIED.  THIS VALUE IS A LOGICAL     02660020
C     VARIABLE SET TO .TRUE.  THE TRUE PATH OF A LOGICAL IF             02670020
C         STATEMENT IS USED IN CONJUNCTION WITH THE LOGICAL STATEMENT   02680020
C         FUNCTION.                                                     02690020
C                                                                       02700020
C                                                                       02710020
      IF (ICZERO) 35580, 5580, 35580                                    02720020
 5580 CONTINUE                                                          02730020
      IVON01 = 0                                                        02740020
      LCTN03 = .TRUE.                                                   02750020
      IF ( LFTN03(LCTN03) )  IVON01 = 1                                 02760020
      GO TO 45580                                                       02770020
35580 IVDELE = IVDELE + 1                                               02780020
      WRITE (I02,80003) IVTNUM                                          02790020
      IF (ICZERO) 45580, 5591, 45580                                    02800020
45580 IF ( IVON01 - 1 )  25580, 15580, 25580                            02810020
15580 IVPASS = IVPASS + 1                                               02820020
      WRITE (I02,80001) IVTNUM                                          02830020
      GO TO 5591                                                        02840020
25580 IVFAIL = IVFAIL + 1                                               02850020
      IVCOMP = IVON01                                                   02860020
      IVCORR = 1                                                        02870020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02880020
 5591 CONTINUE                                                          02890020
      IVTNUM = 559                                                      02900020
C                                                                       02910020
C      ****  TEST 559  ****                                             02920020
C     TEST 559  -  LIKE TEST 558 ONLY THE LOGICAL  .NOT.  IS USED       02930020
C         IN THE LOGICAL STATEMENT FUNCTION DEFINITION  THE FALSE PATH  02940020
C         OF A LOGICAL IF STATEMENT IS USED IN CONJUNCTION WITH THE     02950020
C         LOGICAL STATEMENT FUNCTION.                                   02960020
C                                                                       02970020
C                                                                       02980020
      IF (ICZERO) 35590, 5590, 35590                                    02990020
 5590 CONTINUE                                                          03000020
      IVON01 = 1                                                        03010020
      LCTN04 = .TRUE.                                                   03020020
      IF ( LFTN04(LCTN04) )  IVON01 = 0                                 03030020
      GO TO 45590                                                       03040020
35590 IVDELE = IVDELE + 1                                               03050020
      WRITE (I02,80003) IVTNUM                                          03060020
      IF (ICZERO) 45590, 5601, 45590                                    03070020
45590 IF ( IVON01 - 1 )  25590, 15590, 25590                            03080020
15590 IVPASS = IVPASS + 1                                               03090020
      WRITE (I02,80001) IVTNUM                                          03100020
      GO TO 5601                                                        03110020
25590 IVFAIL = IVFAIL + 1                                               03120020
      IVCOMP = IVON01                                                   03130020
      IVCORR = 1                                                        03140020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03150020
 5601 CONTINUE                                                          03160020
      IVTNUM = 560                                                      03170020
C                                                                       03180020
C      ****  TEST 560  ****                                             03190020
C     TEST 560  -  INTEGER EXPONIENTIATION USED IN AN INTEGER           03200020
C         STATEMENT FUNCTION.                                           03210020
C                                                                       03220020
C                                                                       03230020
      IF (ICZERO) 35600, 5600, 35600                                    03240020
 5600 CONTINUE                                                          03250020
      ICON04 = 3                                                        03260020
      IVCOMP = IFON04(ICON04)                                           03270020
      GO TO 45600                                                       03280020
35600 IVDELE = IVDELE + 1                                               03290020
      WRITE (I02,80003) IVTNUM                                          03300020
      IF (ICZERO) 45600, 5611, 45600                                    03310020
45600 IF ( IVCOMP - 9 )  25600, 15600, 25600                            03320020
15600 IVPASS = IVPASS + 1                                               03330020
      WRITE (I02,80001) IVTNUM                                          03340020
      GO TO 5611                                                        03350020
25600 IVFAIL = IVFAIL + 1                                               03360020
      IVCORR = 9                                                        03370020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03380020
 5611 CONTINUE                                                          03390020
      IVTNUM = 561                                                      03400020
C                                                                       03410020
C      ****  TEST 561  ****                                             03420020
C     TEST 561  -  TEST OF INTEGER ADDITION USING TWO (2) DUMMY         03430020
C         ARGUEMENTS.                                                   03440020
C                                                                       03450020
C                                                                       03460020
      IF (ICZERO) 35610, 5610, 35610                                    03470020
 5610 CONTINUE                                                          03480020
      ICON05 = 9                                                        03490020
      ICON06 = 16                                                       03500020
      IVCOMP = IFON05(ICON05, ICON06)                                   03510020
      GO TO 45610                                                       03520020
35610 IVDELE = IVDELE + 1                                               03530020
      WRITE (I02,80003) IVTNUM                                          03540020
      IF (ICZERO) 45610, 5621, 45610                                    03550020
45610 IF ( IVCOMP - 25 )  25610, 15610, 25610                           03560020
15610 IVPASS = IVPASS + 1                                               03570020
      WRITE (I02,80001) IVTNUM                                          03580020
      GO TO 5621                                                        03590020
25610 IVFAIL = IVFAIL + 1                                               03600020
      IVCORR = 25                                                       03610020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03620020
 5621 CONTINUE                                                          03630020
      IVTNUM = 562                                                      03640020
C                                                                       03650020
C      ****  TEST 562  ****                                             03660020
C     TEST 562  -  THIS TEST IS THE SOLUTION OF A RIGHT TRIANGLE        03670020
C         USING INTEGER STATEMENT FUNCTIONS WHICH REFERENCE THE         03680020
C         INTRINSIC FUNCTIONS  SQRT  AND  FLOAT.  THIS IS A 3-4-5       03690020
C         RIGHT TRIANGLE.                                               03700020
C                                                                       03710020
C                                                                       03720020
      IF (ICZERO) 35620, 5620, 35620                                    03730020
 5620 CONTINUE                                                          03740020
      ICON07 = 3                                                        03750020
      ICON08 = 4                                                        03760020
      IVCOMP = IFON06(ICON07, ICON08)                                   03770020
      GO TO 45620                                                       03780020
35620 IVDELE = IVDELE + 1                                               03790020
      WRITE (I02,80003) IVTNUM                                          03800020
      IF (ICZERO) 45620, 5631, 45620                                    03810020
45620 IF ( IVCOMP - 5 )  5622, 15620, 5622                              03820020
 5622 IF ( IVCOMP - 4 ) 25620, 15620, 25620                             03830020
15620 IVPASS = IVPASS + 1                                               03840020
      WRITE (I02,80001) IVTNUM                                          03850020
      GO TO 5631                                                        03860020
25620 IVFAIL = IVFAIL + 1                                               03870020
      IVCORR = 5                                                        03880020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03890020
 5631 CONTINUE                                                          03900020
      IVTNUM = 563                                                      03910020
C                                                                       03920020
C      ****  TEST 563  ****                                             03930020
C     TEST 563  -  SOLUTION OF A 3-4-5 RIGHT TRIANGLE LIKE TEST 562     03940020
C         EXCEPT THAT BOTH INTRINSIC AND PREVIOUSLY DEFINED STATEMENT   03950020
C         FUNCTIONS ARE USED.                                           03960020
C                                                                       03970020
C                                                                       03980020
      IF (ICZERO) 35630, 5630, 35630                                    03990020
 5630 CONTINUE                                                          04000020
      ICON09 = 3                                                        04010020
      ICON10 = 4                                                        04020020
      IVCOMP = IFON08(ICON09, ICON10)                                   04030020
      GO TO 45630                                                       04040020
35630 IVDELE = IVDELE + 1                                               04050020
      WRITE (I02,80003) IVTNUM                                          04060020
      IF (ICZERO) 45630, 5641, 45630                                    04070020
45630 IF ( IVCOMP - 5 )   5632, 15630, 5632                             04080020
 5632 IF ( IVCOMP - 4 )  25630, 15630, 25630                            04090020
15630 IVPASS = IVPASS + 1                                               04100020
      WRITE (I02,80001) IVTNUM                                          04110020
      GO TO 5641                                                        04120020
25630 IVFAIL = IVFAIL + 1                                               04130020
      IVCORR = 5                                                        04140020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04150020
 5641 CONTINUE                                                          04160020
      IVTNUM = 564                                                      04170020
C                                                                       04180020
C      ****  TEST 564  ****                                             04190020
C     TEST 564  -  USE  OF ARRAY ELEMENTS IN AN INTEGER STATEMENT       04200020
C         FUNCTION WHICH USES THE OPERATIONS OF + - * /  .              04210020
C                                                                       04220020
C                                                                       04230020
      IF (ICZERO) 35640, 5640, 35640                                    04240020
 5640 CONTINUE                                                          04250020
      IADN11(1) = 2                                                     04260020
      IADN11(2) = 2                                                     04270020
      IVCOMP = IFON09( IADN11(1), IADN11(2) )                           04280020
      GO TO 45640                                                       04290020
35640 IVDELE = IVDELE + 1                                               04300020
      WRITE (I02,80003) IVTNUM                                          04310020
      IF (ICZERO) 45640, 5651, 45640                                    04320020
45640 IF ( IVCOMP - 1 )  25640, 15640, 25640                            04330020
15640 IVPASS = IVPASS + 1                                               04340020
      WRITE (I02,80001) IVTNUM                                          04350020
      GO TO 5651                                                        04360020
25640 IVFAIL = IVFAIL + 1                                               04370020
      IVCORR = 1                                                        04380020
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04390020
 5651 CONTINUE                                                          04400020
C                                                                       04410020
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             04420020
99999 CONTINUE                                                          04430020
      WRITE (I02,90002)                                                 04440020
      WRITE (I02,90006)                                                 04450020
      WRITE (I02,90002)                                                 04460020
      WRITE (I02,90002)                                                 04470020
      WRITE (I02,90007)                                                 04480020
      WRITE (I02,90002)                                                 04490020
      WRITE (I02,90008)  IVFAIL                                         04500020
      WRITE (I02,90009) IVPASS                                          04510020
      WRITE (I02,90010) IVDELE                                          04520020
C                                                                       04530020
C                                                                       04540020
C     TERMINATE ROUTINE EXECUTION                                       04550020
      STOP                                                              04560020
C                                                                       04570020
C     FORMAT STATEMENTS FOR PAGE HEADERS                                04580020
90000 FORMAT (1H1)                                                      04590020
90002 FORMAT (1H )                                                      04600020
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04610020
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   04620020
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        04630020
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 04640020
90006 FORMAT (1H ,5X,46H----------------------------------------------) 04650020
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             04660020
C                                                                       04670020
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               04680020
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        04690020
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              04700020
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             04710020
C                                                                       04720020
C     FORMAT STATEMENTS FOR TEST RESULTS                                04730020
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04740020
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      04750020
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   04760020
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04770020
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    04780020
C                                                                       04790020
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM020)                          04800020
      END                                                               04810020
