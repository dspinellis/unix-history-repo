C                                                                       00010017
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020017
C                                                                       00030017
C     FM017                                                             00040017
C                                                                       00050017
C             THIS ROUTINE CONTINUES TESTS OF THE FORTRAN               00060017
C     LOGICAL    IF STATEMENT IN ALL OF THE VARIOUS FORMS.    THE       00070017
C     FOLLOWING LOGICAL OPERANDS ARE USED FOR THIS ROUTINE - LOGICAL    00080017
C     CONSTANTS, LOGICAL VARIABLES, LOGICAL ARRAY ELEMENTS, AND         00090017
C     ARITHMETIC EXPRESSIONS WITH VARIOUS RELATIONAL OPERATORS.  BOTH   00100017
C     THE TRUE AND FALSE BRANCHES ARE TESTED IN THE SERIES OF TESTS.    00110017
C                                                                       00120017
C      REFERENCES                                                       00130017
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140017
C              X3.9-1978                                                00150017
C                                                                       00160017
C        SECTION 4.7.1, LOGICAL CONSTANT                                00170017
C        SECTION 6, EXPRESSIONS                                         00180017
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00190017
C        SECTION 6.3, RELATIONAL EXPRESSIONS                            00200017
C        SECTION 6.4, LOGICAL EXPRESSIONS                               00210017
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00220017
C        SECTION 10, ASSIGNMENT STATEMENTS                              00230017
C        SECTION 10.2, LOGICAL ASSIGNMENT STATEMENT                     00240017
C        SECTION 11.5, LOGICAL IF STATEMENT                             00250017
C                                                                       00260017
      DIMENSION IADN11(3)                                               00270017
      LOGICAL LATN1A(2), LCTNT1, LCTNT2                                 00280017
C                                                                       00290017
C      **********************************************************       00300017
C                                                                       00310017
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00320017
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00330017
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00340017
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00350017
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00360017
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00370017
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00380017
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00390017
C     OF EXECUTING THESE TESTS.                                         00400017
C                                                                       00410017
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00420017
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00430017
C                                                                       00440017
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00450017
C                                                                       00460017
C                  DEPARTMENT OF THE NAVY                               00470017
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00480017
C                  WASHINGTON, D.C.  20376                              00490017
C                                                                       00500017
C      **********************************************************       00510017
C                                                                       00520017
C                                                                       00530017
C                                                                       00540017
C     INITIALIZATION SECTION                                            00550017
C                                                                       00560017
C     INITIALIZE CONSTANTS                                              00570017
C      **************                                                   00580017
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00590017
      I01 = 5                                                           00600017
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00610017
      I02 = 6                                                           00620017
C     SYSTEM ENVIRONMENT SECTION                                        00630017
C                                                                       00640017
      I01 = 5                                                           00650017
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00660017
C     (UNIT NUMBER FOR CARD READER).                                    00670017
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00680017
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00690017
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00700017
C                                                                       00710017
      I02 = 6                                                           00720017
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00730017
C     (UNIT NUMBER FOR PRINTER).                                        00740017
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00750017
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760017
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00770017
C                                                                       00780017
      IVPASS=0                                                          00790017
      IVFAIL=0                                                          00800017
      IVDELE=0                                                          00810017
      ICZERO=0                                                          00820017
C                                                                       00830017
C     WRITE PAGE HEADERS                                                00840017
      WRITE (I02,90000)                                                 00850017
      WRITE (I02,90001)                                                 00860017
      WRITE (I02,90002)                                                 00870017
      WRITE (I02, 90002)                                                00880017
      WRITE (I02,90003)                                                 00890017
      WRITE (I02,90002)                                                 00900017
      WRITE (I02,90004)                                                 00910017
      WRITE (I02,90002)                                                 00920017
      WRITE (I02,90011)                                                 00930017
      WRITE (I02,90002)                                                 00940017
      WRITE (I02,90002)                                                 00950017
      WRITE (I02,90005)                                                 00960017
      WRITE (I02,90006)                                                 00970017
      WRITE (I02,90002)                                                 00980017
      IVTNUM = 170                                                      00990017
C                                                                       01000017
C      ****  TEST 170  ****                                             01010017
C     TEST 170  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  01020017
C           FALSE PATH.  .LT.                                           01030017
C                                                                       01040017
C                                                                       01050017
      IF (ICZERO) 31700, 1700, 31700                                    01060017
 1700 CONTINUE                                                          01070017
      IVON01 = 3                                                        01080017
      IVON02 = 1                                                        01090017
      IF ( 76 .LT. IVON01 )  IVON02 = 0                                 01100017
      GO TO 41700                                                       01110017
31700 IVDELE = IVDELE + 1                                               01120017
      WRITE (I02,80003) IVTNUM                                          01130017
      IF (ICZERO) 41700, 1711, 41700                                    01140017
41700 IF ( IVON02 - 1 )  21700, 11700, 21700                            01150017
11700 IVPASS = IVPASS + 1                                               01160017
      WRITE (I02,80001) IVTNUM                                          01170017
      GO TO 1711                                                        01180017
21700 IVFAIL = IVFAIL + 1                                               01190017
      IVCOMP = IVON02                                                   01200017
      IVCORR = 1                                                        01210017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01220017
 1711 CONTINUE                                                          01230017
      IVTNUM = 171                                                      01240017
C                                                                       01250017
C      ****  TEST 171  ****                                             01260017
C     TEST 171  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  01270017
C           FALSE PATH.  .LE.                                           01280017
C                                                                       01290017
C                                                                       01300017
      IF (ICZERO) 31710, 1710, 31710                                    01310017
 1710 CONTINUE                                                          01320017
      IVON01 = 3                                                        01330017
      IVON02 = 1                                                        01340017
      IF ( 76 .LE. IVON01 )  IVON02 = 0                                 01350017
      GO TO 41710                                                       01360017
31710 IVDELE = IVDELE + 1                                               01370017
      WRITE (I02,80003) IVTNUM                                          01380017
      IF (ICZERO) 41710, 1721, 41710                                    01390017
41710 IF ( IVON02 - 1 )  21710, 11710, 21710                            01400017
11710 IVPASS = IVPASS + 1                                               01410017
      WRITE (I02,80001) IVTNUM                                          01420017
      GO TO 1721                                                        01430017
21710 IVFAIL = IVFAIL + 1                                               01440017
      IVCOMP = IVON02                                                   01450017
      IVCORR = 1                                                        01460017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01470017
 1721 CONTINUE                                                          01480017
      IVTNUM = 172                                                      01490017
C                                                                       01500017
C      ****  TEST 172  ****                                             01510017
C     TEST 172  -  RELATIONAL EXPRESSIONAL.  INTEGER VARIABLE REFERENCE.01520017
C           FALSE PATH.  .EQ.                                           01530017
C                                                                       01540017
C                                                                       01550017
      IF (ICZERO) 31720, 1720, 31720                                    01560017
 1720 CONTINUE                                                          01570017
      IVON01 = 587                                                      01580017
      IVON02 = 1                                                        01590017
      IF ( 9999 .EQ. IVON01 )  IVON02 = 0                               01600017
      GO TO 41720                                                       01610017
31720 IVDELE = IVDELE + 1                                               01620017
      WRITE (I02,80003) IVTNUM                                          01630017
      IF (ICZERO) 41720, 1731, 41720                                    01640017
41720 IF ( IVON02 - 1 )  21720, 11720, 21720                            01650017
11720 IVPASS = IVPASS + 1                                               01660017
      WRITE (I02,80001) IVTNUM                                          01670017
      GO TO 1731                                                        01680017
21720 IVFAIL = IVFAIL + 1                                               01690017
      IVCOMP = IVON02                                                   01700017
      IVCORR = 1                                                        01710017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01720017
 1731 CONTINUE                                                          01730017
      IVTNUM = 173                                                      01740017
C                                                                       01750017
C      ****  TEST 173  ****                                             01760017
C     TEST 173  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  01770017
C           FALSE PATH.  .NE.                                           01780017
C                                                                       01790017
C                                                                       01800017
      IF (ICZERO) 31730, 1730, 31730                                    01810017
 1730 CONTINUE                                                          01820017
      IVON01 = 3                                                        01830017
      IVON02 = 1                                                        01840017
      IF ( 3 .NE. IVON01 )  IVON02 = 0                                  01850017
      GO TO 41730                                                       01860017
31730 IVDELE = IVDELE + 1                                               01870017
      WRITE (I02,80003) IVTNUM                                          01880017
      IF (ICZERO) 41730, 1741, 41730                                    01890017
41730 IF ( IVON02 - 1 )  21730, 11730, 21730                            01900017
11730 IVPASS = IVPASS + 1                                               01910017
      WRITE (I02,80001) IVTNUM                                          01920017
      GO TO 1741                                                        01930017
21730 IVFAIL = IVFAIL + 1                                               01940017
      IVCOMP = IVON02                                                   01950017
      IVCORR = 1                                                        01960017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01970017
 1741 CONTINUE                                                          01980017
      IVTNUM = 174                                                      01990017
C                                                                       02000017
C      ****  TEST 174  ****                                             02010017
C     TEST 174  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  02020017
C           FALSE PATH.  .GT.                                           02030017
C                                                                       02040017
C                                                                       02050017
      IF (ICZERO) 31740, 1740, 31740                                    02060017
 1740 CONTINUE                                                          02070017
      IVON01 = 32767                                                    02080017
      IVON02 = 1                                                        02090017
      IF ( 76 .GT. IVON01 )  IVON02 = 0                                 02100017
      GO TO 41740                                                       02110017
31740 IVDELE = IVDELE + 1                                               02120017
      WRITE (I02,80003) IVTNUM                                          02130017
      IF (ICZERO) 41740, 1751, 41740                                    02140017
41740 IF ( IVON02 - 1 )  21740, 11740, 21740                            02150017
11740 IVPASS = IVPASS + 1                                               02160017
      WRITE (I02,80001) IVTNUM                                          02170017
      GO TO 1751                                                        02180017
21740 IVFAIL = IVFAIL + 1                                               02190017
      IVCOMP = IVON02                                                   02200017
      IVCORR = 1                                                        02210017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02220017
 1751 CONTINUE                                                          02230017
      IVTNUM = 175                                                      02240017
C                                                                       02250017
C      ****  TEST 175  ****                                             02260017
C     TEST 175  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  02270017
C           FALSE PATH.  .GE.                                           02280017
C                                                                       02290017
C                                                                       02300017
      IF (ICZERO) 31750, 1750, 31750                                    02310017
 1750 CONTINUE                                                          02320017
      IVON01 = 32767                                                    02330017
      IVON02 = 1                                                        02340017
      IF ( 76 .GE. IVON01 )  IVON02 = 0                                 02350017
      GO TO 41750                                                       02360017
31750 IVDELE = IVDELE + 1                                               02370017
      WRITE (I02,80003) IVTNUM                                          02380017
      IF (ICZERO) 41750, 1761, 41750                                    02390017
41750 IF ( IVON02 - 1 )  21750, 11750, 21750                            02400017
11750 IVPASS = IVPASS + 1                                               02410017
      WRITE (I02,80001) IVTNUM                                          02420017
      GO TO 1761                                                        02430017
21750 IVFAIL = IVFAIL + 1                                               02440017
      IVCOMP = IVON02                                                   02450017
      IVCORR = 1                                                        02460017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02470017
 1761 CONTINUE                                                          02480017
      IVTNUM = 176                                                      02490017
C                                                                       02500017
C      ****  TEST 176  ****                                             02510017
C     TEST 176  -  RELATIONAL EXPRESSION.  (IVR)  (RO)  (IC)            02520017
C           INTEGER VARIABLE REFERENCE WITH INTEGER CONSTANT            02530017
C           TRUE PATH.  .LT.                                            02540017
C                                                                       02550017
C                                                                       02560017
      IF (ICZERO) 31760, 1760, 31760                                    02570017
 1760 CONTINUE                                                          02580017
      IVON01 = 3                                                        02590017
      IVON02 = 0                                                        02600017
      IF ( IVON01 .LT. 76 )  IVON02 = 1                                 02610017
      GO TO 41760                                                       02620017
31760 IVDELE = IVDELE + 1                                               02630017
      WRITE (I02,80003) IVTNUM                                          02640017
      IF (ICZERO) 41760, 1771, 41760                                    02650017
41760 IF ( IVON02 - 1 )  21760, 11760, 21760                            02660017
11760 IVPASS = IVPASS + 1                                               02670017
      WRITE (I02,80001) IVTNUM                                          02680017
      GO TO 1771                                                        02690017
21760 IVFAIL = IVFAIL + 1                                               02700017
      IVCOMP = IVON02                                                   02710017
      IVCORR = 1                                                        02720017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02730017
 1771 CONTINUE                                                          02740017
      IVTNUM = 177                                                      02750017
C                                                                       02760017
C      ****  TEST 177  ****                                             02770017
C     TEST 177  - LIKE TEST 176.  FALSE PATH.  .EQ.                     02780017
C                                                                       02790017
C                                                                       02800017
      IF (ICZERO) 31770, 1770, 31770                                    02810017
 1770 CONTINUE                                                          02820017
      IVON01 = 587                                                      02830017
      IVON02 = 1                                                        02840017
      IF ( IVON01 .EQ. 9999 )  IVON02=0                                 02850017
      GO TO 41770                                                       02860017
31770 IVDELE = IVDELE + 1                                               02870017
      WRITE (I02,80003) IVTNUM                                          02880017
      IF (ICZERO) 41770, 1781, 41770                                    02890017
41770 IF ( IVON02 - 1 )  21770, 11770, 21770                            02900017
11770 IVPASS = IVPASS + 1                                               02910017
      WRITE (I02,80001) IVTNUM                                          02920017
      GO TO 1781                                                        02930017
21770 IVFAIL = IVFAIL + 1                                               02940017
      IVCOMP = IVON02                                                   02950017
      IVCORR = 1                                                        02960017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02970017
 1781 CONTINUE                                                          02980017
      IVTNUM = 178                                                      02990017
C                                                                       03000017
C      ****  TEST 178  ****                                             03010017
C     TEST 178  -  LIKE TEST 176.  TRUE PATH.  .GE.                     03020017
C                                                                       03030017
C                                                                       03040017
      IF (ICZERO) 31780, 1780, 31780                                    03050017
 1780 CONTINUE                                                          03060017
      IVON01 = 32767                                                    03070017
      IVON02 = 0                                                        03080017
      IF ( IVON01 .GE. 32767 )  IVON02 = 1                              03090017
      GO TO 41780                                                       03100017
31780 IVDELE = IVDELE + 1                                               03110017
      WRITE (I02,80003) IVTNUM                                          03120017
      IF (ICZERO) 41780, 1791, 41780                                    03130017
41780 IF ( IVON02 - 1 )  21780, 11780, 21780                            03140017
11780 IVPASS = IVPASS + 1                                               03150017
      WRITE (I02,80001) IVTNUM                                          03160017
      GO TO 1791                                                        03170017
21780 IVFAIL = IVFAIL + 1                                               03180017
      IVCOMP = IVON02                                                   03190017
      IVCORR = 1                                                        03200017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03210017
 1791 CONTINUE                                                          03220017
      IVTNUM = 179                                                      03230017
C                                                                       03240017
C      ****  TEST 179  ****                                             03250017
C     TEST 179  -  RELATIONAL EXPRESSION.  INTEGER ARRAY ELEMENT        03260017
C           REFERENCE.  (IC)  (RO)  (IAER)   FALSE PATH.  .LT.          03270017
C                                                                       03280017
C                                                                       03290017
      IF (ICZERO) 31790, 1790, 31790                                    03300017
 1790 CONTINUE                                                          03310017
      IVON01 = 1                                                        03320017
      IADN11(1) = 3                                                     03330017
      IF ( 76 .LT. IADN11(1) )  IVON01 = 0                              03340017
      GO TO 41790                                                       03350017
31790 IVDELE = IVDELE + 1                                               03360017
      WRITE (I02,80003) IVTNUM                                          03370017
      IF (ICZERO) 41790, 1801, 41790                                    03380017
41790 IF ( IVON01 - 1 )  21790, 11790, 21790                            03390017
11790 IVPASS = IVPASS + 1                                               03400017
      WRITE (I02,80001) IVTNUM                                          03410017
      GO TO 1801                                                        03420017
21790 IVFAIL = IVFAIL + 1                                               03430017
      IVCOMP = IVON01                                                   03440017
      IVCORR = 1                                                        03450017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03460017
 1801 CONTINUE                                                          03470017
      IVTNUM = 180                                                      03480017
C                                                                       03490017
C      ****  TEST 180  ****                                             03500017
C     TEST 180  -  LIKE TEST 179.  TRUE PATH.  .LE.                     03510017
C                                                                       03520017
C                                                                       03530017
      IF (ICZERO) 31800, 1800, 31800                                    03540017
 1800 CONTINUE                                                          03550017
      IVON01 = 0                                                        03560017
      IADN11(2) = 587                                                   03570017
      IF ( 587 .LE. IADN11(2) )  IVON01 = 1                             03580017
      GO TO 41800                                                       03590017
31800 IVDELE = IVDELE + 1                                               03600017
      WRITE (I02,80003) IVTNUM                                          03610017
      IF (ICZERO) 41800, 1811, 41800                                    03620017
41800 IF ( IVON01 - 1 )  21800, 11800, 21800                            03630017
11800 IVPASS = IVPASS + 1                                               03640017
      WRITE (I02,80001) IVTNUM                                          03650017
      GO TO 1811                                                        03660017
21800 IVFAIL = IVFAIL + 1                                               03670017
      IVCOMP = IVON01                                                   03680017
      IVCORR = 1                                                        03690017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03700017
 1811 CONTINUE                                                          03710017
      IVTNUM = 181                                                      03720017
C                                                                       03730017
C      ****  TEST 181  ****                                             03740017
C     TEST 181  -  LIKE TEST 179.    FALSE PATH.  .GE.                  03750017
C                                                                       03760017
C                                                                       03770017
      IF (ICZERO) 31810, 1810, 31810                                    03780017
 1810 CONTINUE                                                          03790017
      IVON01 = 1                                                        03800017
      IADN11(3) = 32767                                                 03810017
      IF ( 76 .GE. IADN11(3) )  IVON01 = 0                              03820017
      GO TO 41810                                                       03830017
31810 IVDELE = IVDELE + 1                                               03840017
      WRITE (I02,80003) IVTNUM                                          03850017
      IF (ICZERO) 41810, 1821, 41810                                    03860017
41810 IF ( IVON01 - 1 )  21810, 11810, 21810                            03870017
11810 IVPASS = IVPASS + 1                                               03880017
      WRITE (I02,80001) IVTNUM                                          03890017
      GO TO 1821                                                        03900017
21810 IVFAIL = IVFAIL + 1                                               03910017
      IVCOMP = IVON01                                                   03920017
      IVCORR = 1                                                        03930017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03940017
 1821 CONTINUE                                                          03950017
      IVTNUM = 182                                                      03960017
C                                                                       03970017
C      ****  TEST 182  ****                                             03980017
C     TEST 182  -  RELATIONAL EXPRESSION  (IAER)  (RO)  (IC).  TRUE     03990017
C           PATH.  .EQ.                                                 04000017
C                                                                       04010017
C                                                                       04020017
      IF (ICZERO) 31820, 1820, 31820                                    04030017
 1820 CONTINUE                                                          04040017
      IVON01 = 0                                                        04050017
      IADN11(2) = 32767                                                 04060017
      IF ( IADN11(2) .EQ. 32767 )  IVON01 = 1                           04070017
      GO TO 41820                                                       04080017
31820 IVDELE = IVDELE + 1                                               04090017
      WRITE (I02,80003) IVTNUM                                          04100017
      IF (ICZERO) 41820, 1831, 41820                                    04110017
41820 IF ( IVON01 - 1 )  21820, 11820, 21820                            04120017
11820 IVPASS = IVPASS + 1                                               04130017
      WRITE (I02,80001) IVTNUM                                          04140017
      GO TO 1831                                                        04150017
21820 IVFAIL = IVFAIL + 1                                               04160017
      IVCOMP = IVON01                                                   04170017
      IVCORR = 1                                                        04180017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04190017
 1831 CONTINUE                                                          04200017
      IVTNUM = 183                                                      04210017
C                                                                       04220017
C      ****  TEST 183  ****                                             04230017
C     TEST 183  -  RELATIONAL EXPRESSION  (IVR)  (RO)  (IAER)           04240017
C           FALSE PATH.  .NE.                                           04250017
C                                                                       04260017
C                                                                       04270017
      IF (ICZERO) 31830, 1830, 31830                                    04280017
 1830 CONTINUE                                                          04290017
      IVON01 = 1                                                        04300017
      IVON02 = 587                                                      04310017
      IADN11(1) = 587                                                   04320017
      IF ( IVON02 .NE. IADN11(1) )  IVON01 = 0                          04330017
      GO TO 41830                                                       04340017
31830 IVDELE = IVDELE + 1                                               04350017
      WRITE (I02,80003) IVTNUM                                          04360017
      IF (ICZERO) 41830, 1841, 41830                                    04370017
41830 IF ( IVON01 - 1 )  21830, 11830, 21830                            04380017
11830 IVPASS = IVPASS + 1                                               04390017
      WRITE (I02,80001) IVTNUM                                          04400017
      GO TO 1841                                                        04410017
21830 IVFAIL = IVFAIL + 1                                               04420017
      IVCOMP = IVON01                                                   04430017
      IVCORR = 1                                                        04440017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04450017
 1841 CONTINUE                                                          04460017
      IVTNUM = 184                                                      04470017
C                                                                       04480017
C      ****  TEST 184  ****                                             04490017
C     TEST 184  -  RELATIONAL EXPRESSION  (IAER)  (RO)  (IVR)           04500017
C           TRUE PATH  .NE.                                             04510017
C                                                                       04520017
C                                                                       04530017
      IF (ICZERO) 31840, 1840, 31840                                    04540017
 1840 CONTINUE                                                          04550017
      IVON01 = 0                                                        04560017
      IADN11(3) = 3                                                     04570017
      IVON02 = 32767                                                    04580017
      IF ( IADN11(3) .NE. IVON02 )  IVON01 = 1                          04590017
      GO TO 41840                                                       04600017
31840 IVDELE = IVDELE + 1                                               04610017
      WRITE (I02,80003) IVTNUM                                          04620017
      IF (ICZERO) 41840, 1851, 41840                                    04630017
41840 IF ( IVON01 - 1 )  21840, 11840, 21840                            04640017
11840 IVPASS = IVPASS + 1                                               04650017
      WRITE (I02,80001) IVTNUM                                          04660017
      GO TO 1851                                                        04670017
21840 IVFAIL = IVFAIL + 1                                               04680017
      IVCOMP = IVON01                                                   04690017
      IVCORR = 1                                                        04700017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04710017
 1851 CONTINUE                                                          04720017
      IVTNUM = 185                                                      04730017
C                                                                       04740017
C      ****  TEST 185  ****                                             04750017
C     TEST 185  -  TEST OF PARENTHESES  ( (LE) )                        04760017
C           TRUE PATH  LOGICAL CONSTANT  .TRUE.                         04770017
C                                                                       04780017
C                                                                       04790017
      IF (ICZERO) 31850, 1850, 31850                                    04800017
 1850 CONTINUE                                                          04810017
      IVON01 = 0                                                        04820017
      IF ( ( .TRUE. ) )  IVON01 = 1                                     04830017
      GO TO 41850                                                       04840017
31850 IVDELE = IVDELE + 1                                               04850017
      WRITE (I02,80003) IVTNUM                                          04860017
      IF (ICZERO) 41850, 1861, 41850                                    04870017
41850 IF ( IVON01 - 1 )  21850, 11850, 21850                            04880017
11850 IVPASS = IVPASS + 1                                               04890017
      WRITE (I02,80001) IVTNUM                                          04900017
      GO TO 1861                                                        04910017
21850 IVFAIL = IVFAIL + 1                                               04920017
      IVCOMP = IVON01                                                   04930017
      IVCORR = 1                                                        04940017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04950017
 1861 CONTINUE                                                          04960017
      IVTNUM = 186                                                      04970017
C                                                                       04980017
C      ****  TEST 186  ****                                             04990017
C     TEST 186  -  LIKE TEST 185                                        05000017
C           FALSE PATH  LOGICAL CONSTANT  .FALSE.                       05010017
C                                                                       05020017
C                                                                       05030017
      IF (ICZERO) 31860, 1860, 31860                                    05040017
 1860 CONTINUE                                                          05050017
      IVON01 = 1                                                        05060017
      IF ((( .FALSE. )))  IVON01 = 0                                    05070017
      GO TO 41860                                                       05080017
31860 IVDELE = IVDELE + 1                                               05090017
      WRITE (I02,80003) IVTNUM                                          05100017
      IF (ICZERO) 41860, 1871, 41860                                    05110017
41860 IF ( IVON01 - 1 )  21860, 11860, 21860                            05120017
11860 IVPASS = IVPASS + 1                                               05130017
      WRITE (I02,80001) IVTNUM                                          05140017
      GO TO 1871                                                        05150017
21860 IVFAIL = IVFAIL + 1                                               05160017
      IVCOMP = IVON01                                                   05170017
      IVCORR = 1                                                        05180017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05190017
 1871 CONTINUE                                                          05200017
      IVTNUM = 187                                                      05210017
C                                                                       05220017
C      ****  TEST 187  ****                                             05230017
C     TEST 187  -  PARENS AROUND LOGICAL VARIABLE REFERENCE  ( (LVR) )  05240017
C           TRUE PATH                                                   05250017
C                                                                       05260017
C                                                                       05270017
      IF (ICZERO) 31870, 1870, 31870                                    05280017
 1870 CONTINUE                                                          05290017
      IVON01 = 0                                                        05300017
      LCTNT1 = .TRUE.                                                   05310017
      IF ( ( LCTNT1 ) )  IVON01 = 1                                     05320017
      GO TO 41870                                                       05330017
31870 IVDELE = IVDELE + 1                                               05340017
      WRITE (I02,80003) IVTNUM                                          05350017
      IF (ICZERO) 41870, 1881, 41870                                    05360017
41870 IF ( IVON01 - 1 )  21870, 11870, 21870                            05370017
11870 IVPASS = IVPASS + 1                                               05380017
      WRITE (I02,80001) IVTNUM                                          05390017
      GO TO 1881                                                        05400017
21870 IVFAIL = IVFAIL + 1                                               05410017
      IVCOMP = IVON01                                                   05420017
      IVCORR = 1                                                        05430017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05440017
 1881 CONTINUE                                                          05450017
      IVTNUM = 188                                                      05460017
C                                                                       05470017
C      ****  TEST  188  ****                                            05480017
C     TEST 188  -  PARENS AROUND LOGICAL ARRAY REFERENCE  ( ( LAER ) )  05490017
C           FALSE PATH                                                  05500017
C                                                                       05510017
      IF (ICZERO) 31880, 1880, 31880                                    05520017
 1880 CONTINUE                                                          05530017
      IVON01 = 1                                                        05540017
      LATN1A(1) = .FALSE.                                               05550017
      IF ( ( LATN1A(1) ) )  IVON01 = 0                                  05560017
      GO TO 41880                                                       05570017
31880 IVDELE = IVDELE + 1                                               05580017
      WRITE (I02,80003) IVTNUM                                          05590017
      IF (ICZERO) 41880, 1891, 41880                                    05600017
41880 IF ( IVON01 - 1 )  21880, 11880, 21880                            05610017
11880 IVPASS = IVPASS + 1                                               05620017
      WRITE (I02,80001) IVTNUM                                          05630017
      GO TO 1891                                                        05640017
21880 IVFAIL = IVFAIL + 1                                               05650017
      IVCOMP = IVON01                                                   05660017
      IVCORR = 1                                                        05670017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05680017
 1891 CONTINUE                                                          05690017
      IVTNUM = 189                                                      05700017
C                                                                       05710017
C      ****  TEST 189  ****                                             05720017
C     TEST 189  -  USE OF .NOT. WITH A LOGICAL PRIMARY  .NOT. (LP)      05730017
C           FALSE PATH  .NOT. .TRUE.                                    05740017
C                                                                       05750017
C                                                                       05760017
      IF (ICZERO) 31890, 1890, 31890                                    05770017
 1890 CONTINUE                                                          05780017
      IVON01 = 1                                                        05790017
      IF ( .NOT. .TRUE. )  IVON01 = 0                                   05800017
      GO TO 41890                                                       05810017
31890 IVDELE = IVDELE + 1                                               05820017
      WRITE (I02,80003) IVTNUM                                          05830017
      IF (ICZERO) 41890, 1901, 41890                                    05840017
41890 IF ( IVON01 - 1 )  21890, 11890, 21890                            05850017
11890 IVPASS = IVPASS + 1                                               05860017
      WRITE (I02,80001) IVTNUM                                          05870017
      GO TO 1901                                                        05880017
21890 IVFAIL = IVFAIL + 1                                               05890017
      IVCOMP = IVON01                                                   05900017
      IVCORR = 1                                                        05910017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05920017
 1901 CONTINUE                                                          05930017
      IVTNUM = 190                                                      05940017
C                                                                       05950017
C      ****  TEST 190  ****                                             05960017
C     TEST 190  -  LIKE TEST 189  TRUE PATH  .NOT. .FALSE.              05970017
C                                                                       05980017
C                                                                       05990017
      IF (ICZERO) 31900, 1900, 31900                                    06000017
 1900 CONTINUE                                                          06010017
      IVON01 = 0                                                        06020017
      IF ( .NOT. .FALSE. )  IVON01 = 1                                  06030017
      GO TO 41900                                                       06040017
31900 IVDELE = IVDELE + 1                                               06050017
      WRITE (I02,80003) IVTNUM                                          06060017
      IF (ICZERO) 41900, 1911, 41900                                    06070017
41900 IF ( IVON01 - 1 )  21900, 11900, 21900                            06080017
11900 IVPASS = IVPASS + 1                                               06090017
      WRITE (I02,80001) IVTNUM                                          06100017
      GO TO 1911                                                        06110017
21900 IVFAIL = IVFAIL + 1                                               06120017
      IVCOMP = IVON01                                                   06130017
      IVCORR = 1                                                        06140017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06150017
 1911 CONTINUE                                                          06160017
      IVTNUM = 191                                                      06170017
C                                                                       06180017
C      ****  TEST 191  ****                                             06190017
C     TEST 191  -  TESTS .NOT. WITH A LOGICAL VARIABLE SET TO .FALSE.   06200017
C           IN A LOGICAL ASSIGNMENT STATEMENT     TRUE PATH             06210017
C                                                                       06220017
C                                                                       06230017
      IF (ICZERO) 31910, 1910, 31910                                    06240017
 1910 CONTINUE                                                          06250017
      IVON01 = 0                                                        06260017
      LCTNT1 = .FALSE.                                                  06270017
      IF ( .NOT. LCTNT1 )  IVON01 = 1                                   06280017
      GO TO 41910                                                       06290017
31910 IVDELE = IVDELE + 1                                               06300017
      WRITE (I02,80003) IVTNUM                                          06310017
      IF (ICZERO) 41910, 1921, 41910                                    06320017
41910 IF ( IVON01 - 1 )  21910, 11910, 21910                            06330017
11910 IVPASS = IVPASS + 1                                               06340017
      WRITE (I02,80001) IVTNUM                                          06350017
      GO TO 1921                                                        06360017
21910 IVFAIL = IVFAIL + 1                                               06370017
      IVCOMP = IVON01                                                   06380017
      IVCORR = 1                                                        06390017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06400017
 1921 CONTINUE                                                          06410017
      IVTNUM = 192                                                      06420017
C                                                                       06430017
C      ****  TEST 192  ****                                             06440017
C     TEST 192  -  LIKE TEST 191 ONLY USES A LOGICAL ARRAY ELEMENT      06450017
C           SET TO .FALSE. IN A LOGICAL ASSIGNMENT STATEMENT    TRUE    06460017
C                                                                       06470017
C                                                                       06480017
      IF (ICZERO) 31920, 1920, 31920                                    06490017
 1920 CONTINUE                                                          06500017
      IVON01 = 0                                                        06510017
      LATN1A(2) = .FALSE.                                               06520017
      IF ( .NOT. LATN1A(2) )  IVON01 = 1                                06530017
      GO TO 41920                                                       06540017
31920 IVDELE = IVDELE + 1                                               06550017
      WRITE (I02,80003) IVTNUM                                          06560017
      IF (ICZERO) 41920, 1931, 41920                                    06570017
41920 IF ( IVON01 - 1 )  21920, 11920, 21920                            06580017
11920 IVPASS = IVPASS + 1                                               06590017
      WRITE (I02,80001) IVTNUM                                          06600017
      GO TO 1931                                                        06610017
21920 IVFAIL = IVFAIL + 1                                               06620017
      IVCOMP = IVON01                                                   06630017
      IVCORR = 1                                                        06640017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06650017
 1931 CONTINUE                                                          06660017
      IVTNUM = 193                                                      06670017
C                                                                       06680017
C      ****  TEST 193  ****                                             06690017
C     TEST 193  -  USE OF LOGICAL .AND.    (LT) .AND. (LF)              06700017
C           USES TWO LOGICAL VARIABLES EACH SET TO .FALSE.              06710017
C           FALSE  .AND.  FALSE    FALSE PATH                           06720017
C                                                                       06730017
C                                                                       06740017
      IF (ICZERO) 31930, 1930, 31930                                    06750017
 1930 CONTINUE                                                          06760017
      IVON01 = 1                                                        06770017
      LCTNT1 = .FALSE.                                                  06780017
      LCTNT2 = .FALSE.                                                  06790017
      IF ( LCTNT1 .AND. LCTNT2 )  IVON01 = 0                            06800017
      GO TO 41930                                                       06810017
31930 IVDELE = IVDELE + 1                                               06820017
      WRITE (I02,80003) IVTNUM                                          06830017
      IF (ICZERO) 41930, 1941, 41930                                    06840017
41930 IF ( IVON01 - 1 )  21930, 11930, 21930                            06850017
11930 IVPASS = IVPASS + 1                                               06860017
      WRITE (I02,80001) IVTNUM                                          06870017
      GO TO 1941                                                        06880017
21930 IVFAIL = IVFAIL + 1                                               06890017
      IVCOMP = IVON01                                                   06900017
      IVCORR = 1                                                        06910017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06920017
 1941 CONTINUE                                                          06930017
      IVTNUM = 194                                                      06940017
C                                                                       06950017
C      ****  TEST 194  ****                                             06960017
C     TEST 194  -  LIKE TEST 193    FALSE  .AND.  TRUE   FALSE PATH     06970017
C                                                                       06980017
C                                                                       06990017
      IF (ICZERO) 31940, 1940, 31940                                    07000017
 1940 CONTINUE                                                          07010017
      IVON01 = 1                                                        07020017
      LCTNT1 = .FALSE.                                                  07030017
      LCTNT2 = .TRUE.                                                   07040017
      IF ( LCTNT1 .AND. LCTNT2 )  IVON01 = 0                            07050017
      GO TO 41940                                                       07060017
31940 IVDELE = IVDELE + 1                                               07070017
      WRITE (I02,80003) IVTNUM                                          07080017
      IF (ICZERO) 41940, 1951, 41940                                    07090017
41940 IF ( IVON01 - 1 )  21940, 11940, 21940                            07100017
11940 IVPASS = IVPASS + 1                                               07110017
      WRITE (I02,80001) IVTNUM                                          07120017
      GO TO 1951                                                        07130017
21940 IVFAIL = IVFAIL + 1                                               07140017
      IVCOMP = IVON01                                                   07150017
      IVCORR = 1                                                        07160017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07170017
 1951 CONTINUE                                                          07180017
      IVTNUM = 195                                                      07190017
C                                                                       07200017
C      ****  TEST 195  ****                                             07210017
C     TEST 195  -  LIKE TEST 193   TRUE  .AND.  FALSE     FALSE PATH    07220017
C                                                                       07230017
C                                                                       07240017
      IF (ICZERO) 31950, 1950, 31950                                    07250017
 1950 CONTINUE                                                          07260017
      IVON01 = 1                                                        07270017
      LCTNT1 = .TRUE.                                                   07280017
      LCTNT2 = .FALSE.                                                  07290017
      IF ( LCTNT1 .AND. LCTNT2 )  IVON01 = 0                            07300017
      GO TO 41950                                                       07310017
31950 IVDELE = IVDELE + 1                                               07320017
      WRITE (I02,80003) IVTNUM                                          07330017
      IF (ICZERO) 41950, 1961, 41950                                    07340017
41950 IF ( IVON01 - 1 )  21950, 11950, 21950                            07350017
11950 IVPASS = IVPASS + 1                                               07360017
      WRITE (I02,80001) IVTNUM                                          07370017
      GO TO 1961                                                        07380017
21950 IVFAIL = IVFAIL + 1                                               07390017
      IVCOMP = IVON01                                                   07400017
      IVCORR = 1                                                        07410017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07420017
 1961 CONTINUE                                                          07430017
      IVTNUM = 196                                                      07440017
C                                                                       07450017
C      ****  TEST 196  ****                                             07460017
C     TEST 196  -  LIKE TEST 193   TRUE  .AND.  TRUE    TRUE PATH       07470017
C                                                                       07480017
C                                                                       07490017
      IF (ICZERO) 31960, 1960, 31960                                    07500017
 1960 CONTINUE                                                          07510017
      IVON01 = 0                                                        07520017
      LCTNT1 = .TRUE.                                                   07530017
      LCTNT2 = .TRUE.                                                   07540017
      IF ( LCTNT1 .AND. LCTNT2 )  IVON01 = 1                            07550017
      GO TO 41960                                                       07560017
31960 IVDELE = IVDELE + 1                                               07570017
      WRITE (I02,80003) IVTNUM                                          07580017
      IF (ICZERO) 41960, 1971, 41960                                    07590017
41960 IF ( IVON01 - 1 )  21960, 11960, 21960                            07600017
11960 IVPASS = IVPASS + 1                                               07610017
      WRITE (I02,80001) IVTNUM                                          07620017
      GO TO 1971                                                        07630017
21960 IVFAIL = IVFAIL + 1                                               07640017
      IVCOMP = IVON01                                                   07650017
      IVCORR = 1                                                        07660017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07670017
 1971 CONTINUE                                                          07680017
      IVTNUM = 197                                                      07690017
C                                                                       07700017
C      ****  TEST 197  ****                                             07710017
C     TEST 197  -  TEST OF THE INCLUSIVE  .OR.  .    (LE)  .OR.  (LT)   07720017
C           USES LOGICAL VARIABLES SET IN LOGICAL ASSIGNMENT STATEMENTS 07730017
C           FALSE  .OR.  FALSE    FALSE PATH                            07740017
C                                                                       07750017
C                                                                       07760017
      IF (ICZERO) 31970, 1970, 31970                                    07770017
 1970 CONTINUE                                                          07780017
      IVON01 = 1                                                        07790017
      LCTNT1 = .FALSE.                                                  07800017
      LCTNT2 = .FALSE.                                                  07810017
      IF ( LCTNT1 .OR. LCTNT2 )  IVON01 = 0                             07820017
      GO TO 41970                                                       07830017
31970 IVDELE = IVDELE + 1                                               07840017
      WRITE (I02,80003) IVTNUM                                          07850017
      IF (ICZERO) 41970, 1981, 41970                                    07860017
41970 IF ( IVON01 - 1 )  21970, 11970, 21970                            07870017
11970 IVPASS = IVPASS + 1                                               07880017
      WRITE (I02,80001) IVTNUM                                          07890017
      GO TO 1981                                                        07900017
21970 IVFAIL = IVFAIL + 1                                               07910017
      IVCOMP = IVON01                                                   07920017
      IVCORR = 1                                                        07930017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07940017
 1981 CONTINUE                                                          07950017
      IVTNUM = 198                                                      07960017
C                                                                       07970017
C      ****  TEST 198  ****                                             07980017
C     TEST 198  -  LIKE TEST 197  FALSE  .OR.  TRUE    TRUE PATH        07990017
C                                                                       08000017
C                                                                       08010017
      IF (ICZERO) 31980, 1980, 31980                                    08020017
 1980 CONTINUE                                                          08030017
      IVON01 = 0                                                        08040017
      LCTNT1 = .FALSE.                                                  08050017
      LCTNT2 = .TRUE.                                                   08060017
      IF ( LCTNT1 .OR. LCTNT2 )  IVON01 = 1                             08070017
      GO TO 41980                                                       08080017
31980 IVDELE = IVDELE + 1                                               08090017
      WRITE (I02,80003) IVTNUM                                          08100017
      IF (ICZERO) 41980, 1991, 41980                                    08110017
41980 IF ( IVON01 - 1 )  21980, 11980, 21980                            08120017
11980 IVPASS = IVPASS + 1                                               08130017
      WRITE (I02,80001) IVTNUM                                          08140017
      GO TO 1991                                                        08150017
21980 IVFAIL = IVFAIL + 1                                               08160017
      IVCOMP = IVON01                                                   08170017
      IVCORR = 1                                                        08180017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08190017
 1991 CONTINUE                                                          08200017
      IVTNUM = 199                                                      08210017
C                                                                       08220017
C      ****  TEST 199  ****                                             08230017
C     TEST 199  -  LIKE TEST 197.  TRUE  .OR.  FALSE    TRUE PATH.      08240017
C                                                                       08250017
C                                                                       08260017
      IF (ICZERO) 31990, 1990, 31990                                    08270017
 1990 CONTINUE                                                          08280017
      IVON01 = 0                                                        08290017
      LCTNT1 = .TRUE.                                                   08300017
      LCTNT2 = .FALSE.                                                  08310017
      IF ( LCTNT1 .OR. LCTNT2 )  IVON01 = 1                             08320017
      GO TO 41990                                                       08330017
31990 IVDELE = IVDELE + 1                                               08340017
      WRITE (I02,80003) IVTNUM                                          08350017
      IF (ICZERO) 41990, 5001, 41990                                    08360017
41990 IF ( IVON01 - 1 )  21990, 11990, 21990                            08370017
11990 IVPASS = IVPASS + 1                                               08380017
      WRITE (I02,80001) IVTNUM                                          08390017
      GO TO 5001                                                        08400017
21990 IVFAIL = IVFAIL + 1                                               08410017
      IVCOMP = IVON01                                                   08420017
      IVCORR = 1                                                        08430017
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08440017
 5001 CONTINUE                                                          08450017
C                                                                       08460017
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08470017
99999 CONTINUE                                                          08480017
      WRITE (I02,90002)                                                 08490017
      WRITE (I02,90006)                                                 08500017
      WRITE (I02,90002)                                                 08510017
      WRITE (I02,90002)                                                 08520017
      WRITE (I02,90007)                                                 08530017
      WRITE (I02,90002)                                                 08540017
      WRITE (I02,90008)  IVFAIL                                         08550017
      WRITE (I02,90009) IVPASS                                          08560017
      WRITE (I02,90010) IVDELE                                          08570017
C                                                                       08580017
C                                                                       08590017
C     TERMINATE ROUTINE EXECUTION                                       08600017
      STOP                                                              08610017
C                                                                       08620017
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08630017
90000 FORMAT (1H1)                                                      08640017
90002 FORMAT (1H )                                                      08650017
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08660017
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08670017
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08680017
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08690017
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08700017
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08710017
C                                                                       08720017
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08730017
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08740017
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08750017
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08760017
C                                                                       08770017
C     FORMAT STATEMENTS FOR TEST RESULTS                                08780017
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08790017
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08800017
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08810017
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08820017
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08830017
C                                                                       08840017
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM017)                          08850017
      END                                                               08860017
