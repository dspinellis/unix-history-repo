C     COMMENT SECTION                                                   00010109
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020109
C     FM109                                                             00030109
C                                                                       00040109
C         THIS ROUTINE TESTS THE BASIC OPTIONS     REGARDING THE SIMPLE 00050109
C     FORMATTED WRITE STATEMENT OF FORM                                 00060109
C            WRITE (U,F)     OR                                         00070109
C            WRITE (U,F) L                                              00080109
C     WHERE      U IS A LOGICAL UNIT NUMBER                             00090109
C                F IS A FORMAT STATEMENT LABEL, AND                     00100109
C                L IS A LIST OF INTEGER VARIABLES.                      00110109
C     THE FORMAT STATEMENT F CONTAINS NH HOLLERITH FIELD DESCRIPTORS,   00120109
C     NX BLANK FIELD DESCRIPTORS AND IW NUMERIC FIELD DESCRIPTORS.      00130109
C                                                                       00140109
C         THIS ROUTINE TESTS WHETHER THE FIRST CHARACTER OF A FORMAT    00150109
C     RECORD FOR PRINTER OUTPUT DETERMINES VERTICAL SPACING AS FOLLOWS  00160109
C                 1    -  ADVANCE TO FIRST LINE OF NEXT PAGE            00170109
C               BLANK  -  ONE LINE                                      00180109
C                 0    -  ADVANCE TWO LINES BEFORE PRINTING             00190109
C                 +    -  DO NOT ADVANCE BEFORE PRINTING  -  ADVANCE 0  00200109
C                                                                       00210109
C      REFERENCES                                                       00220109
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00230109
C              X3.9-1978                                                00240109
C                                                                       00250109
C        SECTION 8, SPECIFICATION STATEMENTS                            00260109
C        SECTION 9, DATA STATEMENT                                      00270109
C        SECTION 11.10, DO STATEMENT                                    00280109
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00290109
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00300109
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00310109
C        SECTION 13, FORMAT STATEMENT                                   00320109
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00330109
C                                                                       00340109
C         ALL OF THE RESULTS OF THIS ROUTINE MUST BE VISUALLY CHECKED   00350109
C     ON THE OUTPUT REPORT.  THE USUAL TEST CODE FOR PASS, FAIL, OR     00360109
C     DELETE DOES NOT APPLY TO THIS ROUTINE.  IF ANY TEST IS TO BE      00370109
C     DELETED, CHANGE THE OFFENDING WRITE OR FORMAT STATEMENT TO A      00380109
C     COMMENT.  THE PERSON RESPONSIBLE FOR CHECKING THE OUTPUT MUST ALSO00390109
C     CHECK THE COMPILER LISTING TO SEE IF ANY STATEMENTS HAVE BEEN     00400109
C     CHANGED TO COMMENTS.                                              00410109
C                                                                       00420109
C                                                                       00430109
C      **********************************************************       00440109
C                                                                       00450109
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00460109
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00470109
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00480109
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00490109
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00500109
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00510109
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00520109
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00530109
C     OF EXECUTING THESE TESTS.                                         00540109
C                                                                       00550109
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00560109
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00570109
C                                                                       00580109
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00590109
C                                                                       00600109
C                  DEPARTMENT OF THE NAVY                               00610109
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00620109
C                  WASHINGTON, D.C.  20376                              00630109
C                                                                       00640109
C      **********************************************************       00650109
C                                                                       00660109
C                                                                       00670109
C                                                                       00680109
C     INITIALIZATION SECTION                                            00690109
C                                                                       00700109
C     INITIALIZE CONSTANTS                                              00710109
C      **************                                                   00720109
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00730109
      I01 = 5                                                           00740109
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00750109
      I02 = 6                                                           00760109
C     SYSTEM ENVIRONMENT SECTION                                        00770109
C                                                                       00780109
      I01 = 5                                                           00790109
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00800109
C     (UNIT NUMBER FOR CARD READER).                                    00810109
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00820109
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00830109
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00840109
C                                                                       00850109
      I02 = 6                                                           00860109
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00870109
C     (UNIT NUMBER FOR PRINTER).                                        00880109
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00890109
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00900109
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00910109
C                                                                       00920109
      IVPASS=0                                                          00930109
      IVFAIL=0                                                          00940109
      IVDELE=0                                                          00950109
      ICZERO=0                                                          00960109
C                                                                       00970109
C     WRITE PAGE HEADERS                                                00980109
      WRITE (I02,90000)                                                 00990109
      WRITE (I02,90001)                                                 01000109
      WRITE (I02,90002)                                                 01010109
      WRITE (I02, 90002)                                                01020109
      WRITE (I02,90003)                                                 01030109
      WRITE (I02,90002)                                                 01040109
      WRITE (I02,90004)                                                 01050109
      WRITE (I02,90002)                                                 01060109
      WRITE (I02,90011)                                                 01070109
      WRITE (I02,90002)                                                 01080109
      WRITE (I02,90002)                                                 01090109
      WRITE (I02,90006)                                                 01100109
      WRITE (I02,90002)                                                 01110109
C                                                                       01120109
      IVTNUM = 156                                                      01130109
C      ****  TEST 156  ****                                             01140109
C     TEST 156    - VERTICAL SPACING TEST                               01150109
C             1 IN FIRST CHARACTER OF FORMATTED PRINT RECORD MEANS      01160109
C             RECORD IS FIRST LINE AT TOP OF NEXT PAGE.                 01170109
C                                                                       01180109
      IF (ICZERO) 31560, 1560, 31560                                    01190109
 1560 CONTINUE                                                          01200109
      WRITE (I02,80001) IVTNUM                                          01210109
      WRITE (I02,80331)                                                 01220109
80331 FORMAT (5X,22HLAST LINE ON THIS PAGE)                             01230109
      WRITE (I02,80330)                                                 01240109
80330 FORMAT (1H1,31H     THIS IS FIRST LINE ON PAGE)                   01250109
      GO TO 1571                                                        01260109
31560 IVDELE = IVDELE + 1                                               01270109
      WRITE (I02,80003) IVTNUM                                          01280109
 1571 CONTINUE                                                          01290109
      IVTNUM = 157                                                      01300109
C                                                                       01310109
C      ****  TEST 157  ****                                             01320109
C     TEST  157  -  VERTICAL SPACING TEST                               01330109
C         PRINT BLANK LINES                                             01340109
C                                                                       01350109
C                                                                       01360109
      IF (ICZERO) 31570, 1570, 31570                                    01370109
 1570 CONTINUE                                                          01380109
      WRITE (I02,90002)                                                 01390109
      WRITE (I02,80001) IVTNUM                                          01400109
      WRITE (I02,80340)                                                 01410109
80340 FORMAT (1H , 10X)                                                 01420109
      WRITE (I02,80341)                                                 01430109
80341 FORMAT (41H THERE IS ONE BLANK LINE BEFORE THIS LINE)             01440109
      WRITE (I02,80342)                                                 01450109
      WRITE (I02,80342)                                                 01460109
80342 FORMAT (11H           )                                           01470109
      WRITE (I02,80343)                                                 01480109
80343 FORMAT (43H THERE ARE TWO BLANK LINES BEFORE THIS LINE)           01490109
      WRITE (I02,80344)                                                 01500109
      WRITE (I02,80344)                                                 01510109
      WRITE (I02,80344)                                                 01520109
80344 FORMAT (11X)                                                      01530109
      WRITE (I02,80345)                                                 01540109
80345 FORMAT (45H THERE ARE THREE BLANK LINES BEFORE THIS LINE)         01550109
      GO TO 1581                                                        01560109
31570 IVDELE = IVDELE + 1                                               01570109
      WRITE (I02,80003) IVTNUM                                          01580109
 1581 CONTINUE                                                          01590109
      IVTNUM = 158                                                      01600109
C                                                                       01610109
C      ****  TEST 158  ****                                             01620109
C     TEST  158  -  PRINT 54 CHARACTERS                                 01630109
C                                                                       01640109
C                                                                       01650109
      IF (ICZERO) 31580, 1580, 31580                                    01660109
 1580 CONTINUE                                                          01670109
      WRITE (I02,90002)                                                 01680109
      WRITE (I02,80001)IVTNUM                                           01690109
      WRITE (I02,80351)                                                 01700109
80351 FORMAT (33H NEXT LINE CONTAINS 54 CHARACTERS)                     01710109
      WRITE (I02,80350)                                                 01720109
80350 FORMAT(55H 123456789012345678901234567890123456789012345678901234)01730109
      GO TO 1591                                                        01740109
31580 IVDELE = IVDELE + 1                                               01750109
      WRITE (I02,80003) IVTNUM                                          01760109
 1591 CONTINUE                                                          01770109
      IVTNUM = 159                                                      01780109
C                                                                       01790109
C      ****  TEST 159  ****                                             01800109
C     TEST  159  -  NUMERIC FIELD DESCRIPTOR I1                         01810109
C                                                                       01820109
      IF (ICZERO) 31590, 1590, 31590                                    01830109
 1590 CONTINUE                                                          01840109
      WRITE (I02,90002)                                                 01850109
      WRITE (I02,80001) IVTNUM                                          01860109
      WRITE (I02,80361)                                                 01870109
80361 FORMAT (1H ,10X,38HTHIS TEST PRINTS 3 UNDER I1 DESCRIPTOR)        01880109
      IVON01 = 3                                                        01890109
      WRITE (I02,80360) IVON01                                          01900109
80360 FORMAT (1H ,10X,I1)                                               01910109
      GO TO 1601                                                        01920109
31590 IVDELE = IVDELE + 1                                               01930109
      WRITE (I02,80003) IVTNUM                                          01940109
 1601 CONTINUE                                                          01950109
      IVTNUM = 160                                                      01960109
C                                                                       01970109
C      ****  TEST 160  ****                                             01980109
C     TEST  160  -  NUMERIC FIELD DESCRIPTOR I2                         01990109
C                                                                       02000109
      IF (ICZERO) 31600, 1600, 31600                                    02010109
 1600 CONTINUE                                                          02020109
      WRITE (I02,90002)                                                 02030109
      WRITE (I02,80001) IVTNUM                                          02040109
      WRITE (I02,80371)                                                 02050109
80371 FORMAT (11X,39HTHIS TEST PRINTS 15 UNDER I2 DESCRIPTOR)           02060109
      IVON01 = 15                                                       02070109
      WRITE (I02,80370) IVON01                                          02080109
80370 FORMAT (1H ,10X,I2)                                               02090109
      GO TO 1611                                                        02100109
31600 IVDELE = IVDELE + 1                                               02110109
      WRITE (I02,80003) IVTNUM                                          02120109
 1611 CONTINUE                                                          02130109
      IVTNUM = 161                                                      02140109
C                                                                       02150109
C      ****  TEST 161  ****                                             02160109
C     TEST  161  -  NUMERIC FIELD DESCRIPTOR I3                         02170109
C                                                                       02180109
      IF (ICZERO) 31610, 1610, 31610                                    02190109
 1610 CONTINUE                                                          02200109
      WRITE (I02,90002)                                                 02210109
      WRITE (I02,80001) IVTNUM                                          02220109
      WRITE (I02,80381)                                                 02230109
80381 FORMAT (11X,40HTHIS TEST PRINTS 291 UNDER I3 DESCRIPTOR)          02240109
      IVON01 = 291                                                      02250109
      WRITE (I02,80380) IVON01                                          02260109
80380 FORMAT (11X,I3)                                                   02270109
      GO TO 1621                                                        02280109
31610 IVDELE = IVDELE + 1                                               02290109
      WRITE (I02,80003) IVTNUM                                          02300109
 1621 CONTINUE                                                          02310109
      IVTNUM = 162                                                      02320109
C                                                                       02330109
C      ****  TEST 162  ****                                             02340109
C     TEST  162  -  NUMERIC FIELD DESCRIPTOR I4                         02350109
C                                                                       02360109
      IF (ICZERO) 31620, 1620, 31620                                    02370109
 1620 CONTINUE                                                          02380109
      WRITE (I02,90002)                                                 02390109
      WRITE (I02,80001) IVTNUM                                          02400109
      WRITE (I02,80391)                                                 02410109
80391 FORMAT (11X,41HTHIS TEST PRINTS 4321 UNDER I4 DESCRIPTOR)         02420109
      IVON01 = 4321                                                     02430109
      WRITE (I02,80390) IVON01                                          02440109
80390 FORMAT (11X,I4)                                                   02450109
      GO TO 1631                                                        02460109
31620 IVDELE = IVDELE + 1                                               02470109
      WRITE (I02,80003) IVTNUM                                          02480109
 1631 CONTINUE                                                          02490109
      IVTNUM = 163                                                      02500109
C                                                                       02510109
C      ****  TEST 163  ****                                             02520109
C     TEST  163  -  NUMERIC FIELD DESCRIPTOR I5                         02530109
C                                                                       02540109
      IF (ICZERO) 31630, 1630, 31630                                    02550109
 1630 CONTINUE                                                          02560109
      WRITE (I02,90002)                                                 02570109
      WRITE (I02,80001) IVTNUM                                          02580109
      WRITE (I02,80401)                                                 02590109
80401 FORMAT (1H ,10X,42HTHIS TEST PRINTS 12345 UNDER I5 DESCRIPTOR)    02600109
      IVON01 = 12345                                                    02610109
      WRITE (I02,80400) IVON01                                          02620109
80400 FORMAT (1H ,10X,I5)                                               02630109
      GO TO 1641                                                        02640109
31630 IVDELE = IVDELE + 1                                               02650109
      WRITE (I02,80003) IVTNUM                                          02660109
 1641 CONTINUE                                                          02670109
      IVTNUM = 164                                                      02680109
C                                                                       02690109
C      ****  TEST 164  ****                                             02700109
C     TEST  164  -  NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       02710109
C                                                                       02720109
      IF (ICZERO) 31640, 1640, 31640                                    02730109
 1640 CONTINUE                                                          02740109
      IVON01 = 1                                                        02750109
      IVON02 = 22                                                       02760109
      IVON03 = 333                                                      02770109
      IVON04 = 4444                                                     02780109
      IVON05 = 25555                                                    02790109
      WRITE (I02,90002)                                                 02800109
      WRITE (I02,80001) IVTNUM                                          02810109
      WRITE (I02,80411)                                                 02820109
80411 FORMAT (3X,50HTHIS TEST PRINTS 1, 22, 333, 4444, AND 25555 UNDER) 02830109
      WRITE (I02,80412)                                                 02840109
80412 FORMAT (10X,32H(10X,I1,3X,I2,3X,I3,3X,I4,3X,I5))                  02850109
      WRITE (I02,80410) IVON01, IVON02, IVON03, IVON04, IVON05          02860109
80410 FORMAT (10X,I1,3X,I2,3X,I3,3X,I4,3X,I5)                           02870109
      GO TO 1651                                                        02880109
31640 IVDELE = IVDELE + 1                                               02890109
      WRITE (I02,80003) IVTNUM                                          02900109
 1651 CONTINUE                                                          02910109
      IVTNUM = 165                                                      02920109
C                                                                       02930109
C      ****  TEST 165  ****                                             02940109
C     TEST  165   - HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS          02950109
C            COMBINE HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS IN      02960109
C            ONE FORMAT STATEMENT                                       02970109
C                                                                       02980109
      IF (ICZERO) 31650, 1650, 31650                                    02990109
 1650 CONTINUE                                                          03000109
      IVON01=113                                                        03010109
      IVON02=8                                                          03020109
      WRITE (I02,90002)                                                 03030109
      WRITE (I02,80001) IVTNUM                                          03040109
      WRITE (I02,80421)                                                 03050109
80421 FORMAT (10X,28HNEXT TWO LINES ARE IDENTICAL)                      03060109
      WRITE (I02,80422)                                                 03070109
80422 FORMAT (35H      IVON01 =  113   IVON02 =    8)                   03080109
      WRITE (I02,80420) IVON01, IVON02                                  03090109
80420 FORMAT (6X,8HIVON01 =,I5,3X,8HIVON02 =,I5)                        03100109
      GO TO 1661                                                        03110109
31650 IVDELE = IVDELE + 1                                               03120109
      WRITE (I02,80003) IVTNUM                                          03130109
 1661 CONTINUE                                                          03140109
      IVTNUM = 166                                                      03150109
C                                                                       03160109
C      ****  TEST 166  ****                                             03170109
C     TEST  166   - NUMERIC FIELD DESCRIPTOR I2                         03180109
C           PRINT NEGATIVE INTEGER                                      03190109
C                                                                       03200109
      IF (ICZERO) 31660, 1660, 31660                                    03210109
 1660 CONTINUE                                                          03220109
      IVON01 = -1                                                       03230109
      WRITE (I02,90002)                                                 03240109
      WRITE (I02,80001)  IVTNUM                                         03250109
      WRITE (I02,80431)                                                 03260109
80431 FORMAT (11X,39HTHIS TEST PRINTS -1 UNDER I2 DESCRIPTOR)           03270109
      WRITE (I02,80430) IVON01                                          03280109
80430 FORMAT (11X,I2)                                                   03290109
      GO TO 1671                                                        03300109
31660 IVDELE = IVDELE + 1                                               03310109
      WRITE (I02,80003) IVTNUM                                          03320109
 1671 CONTINUE                                                          03330109
      IVTNUM = 167                                                      03340109
C                                                                       03350109
C      ****  TEST 167  ****                                             03360109
C     TEST  167 -   NUMERIC FIELD DESCRIPTOR I3                         03370109
C           PRINT NEGATIVE INTEGER                                      03380109
C                                                                       03390109
      IF (ICZERO) 31670, 1670, 31670                                    03400109
 1670 CONTINUE                                                          03410109
      IVON01 = -22                                                      03420109
      WRITE (I02,90002)                                                 03430109
      WRITE (I02,80001) IVTNUM                                          03440109
      WRITE (I02,80441)                                                 03450109
80441 FORMAT (11X,40HTHIS TEST PRINTS -22 UNDER I3 DESCRIPTOR)          03460109
      WRITE (I02,80440) IVON01                                          03470109
80440 FORMAT (11X,I3)                                                   03480109
      GO TO 1681                                                        03490109
31670 IVDELE = IVDELE + 1                                               03500109
      WRITE (I02,80003) IVTNUM                                          03510109
 1681 CONTINUE                                                          03520109
      IVTNUM = 168                                                      03530109
C                                                                       03540109
C      ****  TEST 168  ****                                             03550109
C     TEST  168 -   NUMERIC FIELD DESCRIPTOR I4                         03560109
C           PRINT NEGATIVE INTEGER                                      03570109
C                                                                       03580109
      IF (ICZERO) 31680, 1680, 31680                                    03590109
 1680 CONTINUE                                                          03600109
      IVON01 = -333                                                     03610109
      WRITE (I02,90002)                                                 03620109
      WRITE (I02,80001) IVTNUM                                          03630109
      WRITE (I02,80451)                                                 03640109
80451 FORMAT (11X,41HTHIS TEST PRINTS -333 UNDER I4 DESCRIPTOR)         03650109
      WRITE (I02,80450) IVON01                                          03660109
80450 FORMAT (11X,I4)                                                   03670109
      GO TO 1691                                                        03680109
31680 IVDELE = IVDELE + 1                                               03690109
      WRITE (I02,80003) IVTNUM                                          03700109
 1691 CONTINUE                                                          03710109
      IVTNUM = 169                                                      03720109
C                                                                       03730109
C      ****  TEST 169  ****                                             03740109
C     TEST  169 -   NUMERIC FIELD DESCRIPTOR I5                         03750109
C           PRINT NEGATIVE INTEGER                                      03760109
C                                                                       03770109
      IF (ICZERO) 31690, 1690, 31690                                    03780109
 1690 CONTINUE                                                          03790109
      IVON01 = -4444                                                    03800109
      WRITE (I02,90002)                                                 03810109
      WRITE (I02,80001) IVTNUM                                          03820109
      WRITE (I02,80461)                                                 03830109
80461 FORMAT (11X,42HTHIS TEST PRINTS -4444 UNDER I5 DESCRIPTOR)        03840109
      WRITE (I02,80460) IVON01                                          03850109
80460 FORMAT (11X,I5)                                                   03860109
      GO TO 1701                                                        03870109
31690 IVDELE = IVDELE + 1                                               03880109
      WRITE (I02,80003) IVTNUM                                          03890109
 1701 CONTINUE                                                          03900109
      IVTNUM = 170                                                      03910109
C                                                                       03920109
C      ****  TEST 170  ****                                             03930109
C     TEST  170 -   NUMERIC FIELD DESCRIPTOR I6                         03940109
C           PRINT NEGATIVE INTEGER                                      03950109
C                                                                       03960109
      IF (ICZERO) 31700, 1700, 31700                                    03970109
 1700 CONTINUE                                                          03980109
      IVON01 = -15555                                                   03990109
      WRITE (I02,90002)                                                 04000109
      WRITE (I02,80001) IVTNUM                                          04010109
      WRITE (I02,80471)                                                 04020109
80471 FORMAT (11X,43HTHIS TEST PRINTS -15555 UNDER DESCRIPTOR I6)       04030109
      WRITE (I02,80470) IVON01                                          04040109
80470 FORMAT (11X,I6)                                                   04050109
      GO TO 1711                                                        04060109
31700 IVDELE = IVDELE + 1                                               04070109
      WRITE (I02,80003) IVTNUM                                          04080109
 1711 CONTINUE                                                          04090109
      IVTNUM = 171                                                      04100109
C                                                                       04110109
C      ****  TEST 171  ****                                             04120109
C     TEST  171 -   NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       04130109
C           PRINT NEGATIVE INTEGERS                                     04140109
C                                                                       04150109
      IF (ICZERO) 31710, 1710, 31710                                    04160109
 1710 CONTINUE                                                          04170109
      IVON01 = -9                                                       04180109
      IVON02 = -88                                                      04190109
      IVON03 = -777                                                     04200109
      IVON04 = -6666                                                    04210109
      IVON05 = -25555                                                   04220109
      WRITE (I02,90002)                                                 04230109
      WRITE (I02,80001) IVTNUM                                          04240109
      WRITE (I02,80481)                                                 04250109
80481 FORMAT (8X,49HTHIS TEST PRINTS -9, -88, -777, -6666, AND -25555)  04260109
      WRITE (I02,80482)                                                 04270109
80482 FORMAT (11X,43HUNDER FORMAT 10X,I2,3X,I3,3X,I4,3X,I5,3X,I6)       04280109
      WRITE (I02,80480) IVON01,IVON02,IVON03,IVON04,IVON05              04290109
80480 FORMAT (10X,I2,3X,I3,3X,I4,3X,I5,3X,I6)                           04300109
      GO TO 1721                                                        04310109
31710 IVDELE = IVDELE + 1                                               04320109
      WRITE (I02,80003) IVTNUM                                          04330109
 1721 CONTINUE                                                          04340109
      IVTNUM = 172                                                      04350109
C                                                                       04360109
C      ****  TEST 172  ****                                             04370109
C     TEST  172 -   NUMERIC FIELD DESCRIPTOR I5                         04380109
C            MIX POSITIVE AND NEGATIVE INTEGER OUTPUT IN ONE FORMAT     04390109
C         STATEMENT ALL UNDER I5 DESCRIPTOR                             04400109
C                                                                       04410109
      IF (ICZERO) 31720, 1720, 31720                                    04420109
 1720 CONTINUE                                                          04430109
      IVON01 =5                                                         04440109
      IVON02 = -54                                                      04450109
      IVON03 = 543                                                      04460109
      IVON04 = -5432                                                    04470109
      IVON05=32000                                                      04480109
      WRITE (I02,90002)                                                 04490109
      WRITE (I02,80001) IVTNUM                                          04500109
      WRITE (I02,80491)                                                 04510109
80491 FORMAT (18X,46HTHIS TEST PRINTS 5, -54, 543, -5432, AND 32000)    04520109
      WRITE (I02,80492)                                                 04530109
80492 FORMAT (11X,33HUNDER I5 NUMERIC FIELD DESCRIPTOR)                 04540109
      WRITE (I02,80490) IVON01,IVON02,IVON03,IVON04,IVON05              04550109
80490 FORMAT (11X,I5,3X,I5,3X,I5,3X,I5,3X,I5)                           04560109
      GO TO 1731                                                        04570109
31720 IVDELE = IVDELE + 1                                               04580109
      WRITE (I02,80003) IVTNUM                                          04590109
 1731 CONTINUE                                                          04600109
      IVTNUM = 173                                                      04610109
C                                                                       04620109
C      ****  TEST 173  ****                                             04630109
C     TEST 173  -  VERTICAL SPACING TEST USING THE 1H0 AS A DOUBLE      04640109
C     SPACE BEFORE PRINT ( ADVANCE TWO LINES BEFORE WRITING ).  THE 0   04650109
C     AS A CARRIAGE CONTROL CHARACTER IS USED WITH THE BLANK CHARACTER  04660109
C     TO GET AN ODD NUMBER OF LINES TO ADVANCE BEFORE WRITING.          04670109
C                                                                       04680109
      IF (ICZERO) 31730, 1730, 31730                                    04690109
 1730 CONTINUE                                                          04700109
      WRITE (I02,90002)                                                 04710109
      WRITE (I02,80001) IVTNUM                                          04720109
      WRITE (I02,81730)                                                 04730109
81730 FORMAT (1H , 10X)                                                 04740109
      WRITE (I02,81731)                                                 04750109
81731 FORMAT (41H THERE IS ONE BLANK LINE BEFORE THIS LINE)             04760109
      WRITE ( I02, 81732 )                                              04770109
81732 FORMAT ( 1H0,10X)                                                 04780109
      WRITE ( I02, 81733 )                                              04790109
81733 FORMAT (43H THERE ARE TWO BLANK LINES BEFORE THIS LINE)           04800109
      WRITE ( I02, 81730 )                                              04810109
      WRITE ( I02, 81732 )                                              04820109
      WRITE ( I02, 81735 )                                              04830109
81735 FORMAT (45H THERE ARE THREE BLANK LINES BEFORE THIS LINE)         04840109
      WRITE ( I02, 81732 )                                              04850109
      WRITE ( I02, 81732 )                                              04860109
      WRITE ( I02, 81736 )                                              04870109
81736 FORMAT (45H THERE ARE FOUR  BLANK LINES BEFORE THIS LINE)         04880109
      GO TO 1741                                                        04890109
31730 IVDELE = IVDELE + 1                                               04900109
      WRITE (I02,80003) IVTNUM                                          04910109
 1741 CONTINUE                                                          04920109
      IVTNUM = 174                                                      04930109
C                                                                       04940109
C      ****  TEST 174  ****                                             04950109
C     TEST 174  -  VERTICAL SPACING TEST USING THE + CHARACTER TO       04960109
C     SUPPRESS ADVANCING BEFORE THE PRINT AND THIS SHOULD CAUSE TWO AND 04970109
C     THEN THREE SUCCESSIVE LINES TO OVERPRINT                          04980109
C                                                                       04990109
      IF (ICZERO) 31740, 1740, 31740                                    05000109
 1740 CONTINUE                                                          05010109
      WRITE ( I02, 90002 )                                              05020109
      WRITE ( I02, 80001 ) IVTNUM                                       05030109
      WRITE ( I02, 81740 )                                              05040109
81740 FORMAT ( 1H  )                                                    05050109
      WRITE ( I02, 81741 )                                              05060109
81741 FORMAT ( 1H ,10X, 19H1ST LINE - AABBCCDD)                         05070109
      WRITE ( I02, 81742 )                                              05080109
81742 FORMAT ( 1H+, 25X, 30HWWXXYYZZ OVERPRINTS - 2ND LINE)             05090109
      WRITE ( I02, 81743 )                                              05100109
81743 FORMAT ( /////1H )                                                05110109
C     SKIP DOWN A FEW LINES TO GET SET  -  OK AWAY WE GO..              05120109
      WRITE ( I02, 81740 )                                              05130109
      WRITE ( I02, 81744 )                                              05140109
81744 FORMAT ( 1H , 10X, 29H11    44     1ST         LINE)              05150109
      WRITE ( I02, 81745 )                                              05160109
81745 FORMAT ( 1H+, 10X, 20H  22    55       2ND)                       05170109
      WRITE ( I02, 81746 )                                              05180109
81746 FORMAT ( 1H+, 10X, 24H    33    66         3RD)                   05190109
      GO TO 1751                                                        05200109
31740 IVDELE = IVDELE + 1                                               05210109
      WRITE (I02,80003) IVTNUM                                          05220109
 1751 CONTINUE                                                          05230109
      IVTNUM = 175                                                      05240109
C                                                                       05250109
C      ****  TEST 175  ****                                             05260109
C     TEST 175  -  NUMERIC FIELD DESCRIPTOR F3.0                        05270109
C                                                                       05280109
      IF (ICZERO) 31750, 1750, 31750                                    05290109
 1750 CONTINUE                                                          05300109
      WRITE ( I02, 90002 )                                              05310109
      WRITE ( I02, 80001 ) IVTNUM                                       05320109
      WRITE ( I02, 81751 )                                              05330109
81751 FORMAT (1H ,10X,42HTHIS TESTS PRINTS 3. UNDER F3.0 DESCRIPTOR)    05340109
      RVON01 = 3.                                                       05350109
      WRITE ( I02, 81752 )  RVON01                                      05360109
81752 FORMAT ( 1H ,10X, F3.0 )                                          05370109
      GO TO 1761                                                        05380109
31750 IVDELE = IVDELE + 1                                               05390109
      WRITE (I02,80003) IVTNUM                                          05400109
 1761 CONTINUE                                                          05410109
      IVTNUM = 176                                                      05420109
C                                                                       05430109
C      ****  TEST 176  ****                                             05440109
C     TEST 176  -  SIGNED NUMERIC FIELD DESCRIPTOR F4.0                 05450109
C                                                                       05460109
      IF (ICZERO) 31760, 1760, 31760                                    05470109
 1760 CONTINUE                                                          05480109
      WRITE ( I02, 90002 )                                              05490109
      WRITE ( I02, 80001 ) IVTNUM                                       05500109
      WRITE ( I02, 81761 )                                              05510109
81761 FORMAT ( 1H ,10X,43HTHIS TEST  PRINTS -15. WITH F4.0 DESCRIPTOR)  05520109
      RVON01 = -15.                                                     05530109
      WRITE ( I02, 81762 )  RVON01                                      05540109
81762 FORMAT ( 1H ,10X, F4.0)                                           05550109
      GO TO 1771                                                        05560109
31760 IVDELE = IVDELE + 1                                               05570109
      WRITE (I02,80003) IVTNUM                                          05580109
 1771 CONTINUE                                                          05590109
      IVTNUM = 177                                                      05600109
C                                                                       05610109
C      ****  TEST 177  ****                                             05620109
C     TEST 177  -  SIGNED NUMERIC FIELD DESCRIPTOR E12.5                05630109
C                                                                       05640109
      IF (ICZERO) 31770, 1770, 31770                                    05650109
 1770 CONTINUE                                                          05660109
      WRITE ( I02, 90002 )                                              05670109
      WRITE ( I02, 80001 )  IVTNUM                                      05680109
      WRITE ( I02, 81771 )                                              05690109
81771 FORMAT ( 1H , 10X,41HTHIS TEST PRINTS -0.12345E+03 USING E12.5)   05700109
      RVON01 = -123.45                                                  05710109
      WRITE ( I02, 81772 )  RVON01                                      05720109
81772 FORMAT ( 1H , 10X, E12.5 )                                        05730109
      GO TO 1781                                                        05740109
31770 IVDELE = IVDELE + 1                                               05750109
      WRITE (I02,80003) IVTNUM                                          05760109
 1781 CONTINUE                                                          05770109
C                                                                       05780109
C     WRITE PAGE FOOTINGS                                               05790109
99999 CONTINUE                                                          05800109
      WRITE (I02,90002)                                                 05810109
      WRITE (I02,90006)                                                 05820109
      WRITE (I02,90002)                                                 05830109
      WRITE (I02,90007)                                                 05840109
C                                                                       05850109
C     TERMINATE ROUTINE EXECUTION                                       05860109
      STOP                                                              05870109
C                                                                       05880109
C     FORMAT STATEMENTS FOR PAGE HEADERS                                05890109
90000 FORMAT (1H1)                                                      05900109
90002 FORMAT (1H )                                                      05910109
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05920109
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   05930109
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        05940109
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 05950109
90006 FORMAT (1H ,5X,46H----------------------------------------------) 05960109
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             05970109
C     FORMAT STATEMENTS FOR THIS ROUTINE                                05980109
80001 FORMAT (10X,5HTEST ,I5)                                           05990109
80003 FORMAT ( 1H ,4X,I5,7X,7HDELETED)                                  06000109
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM109)                          06010109
      END                                                               06020109
