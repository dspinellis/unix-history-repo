C     COMMENT SECTION                                                   00010039
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020039
C        FM039                                                          00030039
C                                                                       00040039
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050039
C     FORM          INTEGER VARIABLE = ARITHMETIC EXPRESSION            00060039
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00070039
C     OPERATOR /, INTEGER CONSTANTS AND AN INTEGER VARIABLE.  BOTH      00080039
C     POSITIVE AND NEGATIVE VALUES ARE USED FOR THE INTEGER CONSTANTS   00090039
C     AND THE INTEGER VARIABLE.                                         00100039
C                                                                       00110039
C         THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     00120039
C     AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  00130039
C     IN THE RESULTANT INTEGER VARIABLE.  SOME OF THE TESTS USE PARENS  00140039
C     TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSION.                   00150039
C                                                                       00160039
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00170039
C             (1) INTEGER VARIABLE/INTEGER CONSTANT/INTEGER CONSTANT    00180039
C                 INTEGER CONSTANT/INTEGER VARIABLE/INTEGER CONSTANT    00190039
C                 INTEGER CONSTANT/INTEGER CONSTANT/INTEGER VARIABLE    00200039
C             (2) SAME AS (1) BUT WITH PARENTHESES TO GROUP ELEMENTS    00210039
C                   IN THE ARITHMETIC EXPRESSION.                       00220039
C                                                                       00230039
C      REFERENCES                                                       00240039
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00250039
C              X3.9-1978                                                00260039
C                                                                       00270039
C        SECTION 4.3, INTEGER TYPE                                      00280039
C        SECTION 4.3.1, INTEGER CONSTANT                                00290039
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00300039
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00310039
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00320039
C                                                                       00330039
C      **********************************************************       00340039
C                                                                       00350039
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00360039
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00370039
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00380039
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00390039
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00400039
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00410039
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00420039
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00430039
C     OF EXECUTING THESE TESTS.                                         00440039
C                                                                       00450039
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00460039
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00470039
C                                                                       00480039
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00490039
C                                                                       00500039
C                  DEPARTMENT OF THE NAVY                               00510039
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00520039
C                  WASHINGTON, D.C.  20376                              00530039
C                                                                       00540039
C      **********************************************************       00550039
C                                                                       00560039
C                                                                       00570039
C                                                                       00580039
C     INITIALIZATION SECTION                                            00590039
C                                                                       00600039
C     INITIALIZE CONSTANTS                                              00610039
C      **************                                                   00620039
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00630039
      I01 = 5                                                           00640039
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00650039
      I02 = 6                                                           00660039
C     SYSTEM ENVIRONMENT SECTION                                        00670039
C                                                                       00680039
      I01 = 5                                                           00690039
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700039
C     (UNIT NUMBER FOR CARD READER).                                    00710039
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00720039
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00730039
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00740039
C                                                                       00750039
      I02 = 6                                                           00760039
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00770039
C     (UNIT NUMBER FOR PRINTER).                                        00780039
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00790039
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00800039
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00810039
C                                                                       00820039
      IVPASS=0                                                          00830039
      IVFAIL=0                                                          00840039
      IVDELE=0                                                          00850039
      ICZERO=0                                                          00860039
C                                                                       00870039
C     WRITE PAGE HEADERS                                                00880039
      WRITE (I02,90000)                                                 00890039
      WRITE (I02,90001)                                                 00900039
      WRITE (I02,90002)                                                 00910039
      WRITE (I02, 90002)                                                00920039
      WRITE (I02,90003)                                                 00930039
      WRITE (I02,90002)                                                 00940039
      WRITE (I02,90004)                                                 00950039
      WRITE (I02,90002)                                                 00960039
      WRITE (I02,90011)                                                 00970039
      WRITE (I02,90002)                                                 00980039
      WRITE (I02,90002)                                                 00990039
      WRITE (I02,90005)                                                 01000039
      WRITE (I02,90006)                                                 01010039
      WRITE (I02,90002)                                                 01020039
C                                                                       01030039
C     TEST SECTION                                                      01040039
C                                                                       01050039
C         ARITHMETIC ASSIGNMENT STATEMENT                               01060039
C                                                                       01070039
C     TEST 552 THROUGH TEST 557 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS01080039
C     OF THE FORM             IV = IV/IC/IC.                            01090039
C                                                                       01100039
 5521 CONTINUE                                                          01110039
      IVTNUM = 552                                                      01120039
C                                                                       01130039
C      ****  TEST 552  ****                                             01140039
C                                                                       01150039
      IF (ICZERO) 35520, 5520, 35520                                    01160039
 5520 CONTINUE                                                          01170039
      IVON01 = 24                                                       01180039
      IVCOMP = IVON01/3/4                                               01190039
      GO TO 45520                                                       01200039
35520 IVDELE = IVDELE + 1                                               01210039
      WRITE (I02,80003) IVTNUM                                          01220039
      IF (ICZERO) 45520, 5531, 45520                                    01230039
45520 IF (IVCOMP - 2) 25520,15520,25520                                 01240039
15520 IVPASS = IVPASS + 1                                               01250039
      WRITE (I02,80001) IVTNUM                                          01260039
      GO TO 5531                                                        01270039
25520 IVFAIL = IVFAIL + 1                                               01280039
      IVCORR = 2                                                        01290039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01300039
 5531 CONTINUE                                                          01310039
      IVTNUM = 553                                                      01320039
C                                                                       01330039
C      ****  TEST 553  ****                                             01340039
C                                                                       01350039
      IF (ICZERO) 35530, 5530, 35530                                    01360039
 5530 CONTINUE                                                          01370039
      IVON01 = 7151                                                     01380039
      IVCOMP = IVON01/3/10                                              01390039
      GO TO 45530                                                       01400039
35530 IVDELE = IVDELE + 1                                               01410039
      WRITE (I02,80003) IVTNUM                                          01420039
      IF (ICZERO) 45530, 5541, 45530                                    01430039
45530 IF (IVCOMP - 238) 25530,15530,25530                               01440039
15530 IVPASS = IVPASS + 1                                               01450039
      WRITE (I02,80001) IVTNUM                                          01460039
      GO TO 5541                                                        01470039
25530 IVFAIL = IVFAIL + 1                                               01480039
      IVCORR = 238                                                      01490039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01500039
 5541 CONTINUE                                                          01510039
      IVTNUM = 554                                                      01520039
C                                                                       01530039
C      ****  TEST 554  ****                                             01540039
C                                                                       01550039
      IF (ICZERO) 35540, 5540, 35540                                    01560039
 5540 CONTINUE                                                          01570039
      IVON01 = -330                                                     01580039
      IVCOMP = IVON01/3/2                                               01590039
      GO TO 45540                                                       01600039
35540 IVDELE = IVDELE + 1                                               01610039
      WRITE (I02,80003) IVTNUM                                          01620039
      IF (ICZERO) 45540, 5551, 45540                                    01630039
45540 IF (IVCOMP + 55) 25540,15540,25540                                01640039
15540 IVPASS = IVPASS + 1                                               01650039
      WRITE (I02,80001) IVTNUM                                          01660039
      GO TO 5551                                                        01670039
25540 IVFAIL = IVFAIL + 1                                               01680039
      IVCORR = -55                                                      01690039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01700039
 5551 CONTINUE                                                          01710039
      IVTNUM = 555                                                      01720039
C                                                                       01730039
C      ****  TEST 555  ****                                             01740039
C                                                                       01750039
      IF (ICZERO) 35550, 5550, 35550                                    01760039
 5550 CONTINUE                                                          01770039
      IVON01 = 15249                                                    01780039
      IVCOMP = IVON01/(-13)/51                                          01790039
      GO TO 45550                                                       01800039
35550 IVDELE = IVDELE + 1                                               01810039
      WRITE (I02,80003) IVTNUM                                          01820039
      IF (ICZERO) 45550, 5561, 45550                                    01830039
45550 IF (IVCOMP + 23) 25550,15550,25550                                01840039
15550 IVPASS = IVPASS + 1                                               01850039
      WRITE (I02,80001) IVTNUM                                          01860039
      GO TO 5561                                                        01870039
25550 IVFAIL = IVFAIL + 1                                               01880039
      IVCORR = -23                                                      01890039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01900039
 5561 CONTINUE                                                          01910039
      IVTNUM = 556                                                      01920039
C                                                                       01930039
C      ****  TEST 556  ****                                             01940039
C                                                                       01950039
      IF (ICZERO) 35560, 5560, 35560                                    01960039
 5560 CONTINUE                                                          01970039
      IVON01 = -27342                                                   01980039
      IVCOMP = IVON01/(-4)/(-3)                                         01990039
      GO TO 45560                                                       02000039
35560 IVDELE = IVDELE + 1                                               02010039
      WRITE (I02,80003) IVTNUM                                          02020039
      IF (ICZERO) 45560, 5571, 45560                                    02030039
45560 IF (IVCOMP + 2278) 25560,15560,25560                              02040039
15560 IVPASS = IVPASS + 1                                               02050039
      WRITE (I02,80001) IVTNUM                                          02060039
      GO TO 5571                                                        02070039
25560 IVFAIL = IVFAIL + 1                                               02080039
      IVCORR = -2278                                                    02090039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02100039
 5571 CONTINUE                                                          02110039
      IVTNUM = 557                                                      02120039
C                                                                       02130039
C      ****  TEST 557  ****                                             02140039
C                                                                       02150039
      IF (ICZERO) 35570, 5570, 35570                                    02160039
 5570 CONTINUE                                                          02170039
      IVON01 = -27342                                                   02180039
      IVCOMP = -IVON01/4/(-3)                                           02190039
      GO TO 45570                                                       02200039
35570 IVDELE = IVDELE + 1                                               02210039
      WRITE (I02,80003) IVTNUM                                          02220039
      IF (ICZERO) 45570, 5581, 45570                                    02230039
45570 IF (IVCOMP + 2278) 25570,15570,25570                              02240039
15570 IVPASS = IVPASS + 1                                               02250039
      WRITE (I02,80001) IVTNUM                                          02260039
      GO TO 5581                                                        02270039
25570 IVFAIL = IVFAIL + 1                                               02280039
      IVCORR = -2278                                                    02290039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02300039
C                                                                       02310039
C     TEST 558 THROUGH TEST 563 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS02320039
C     OF THE FORM             IV=IC/IV/IC.                              02330039
C                                                                       02340039
 5581 CONTINUE                                                          02350039
      IVTNUM = 558                                                      02360039
C                                                                       02370039
C      ****  TEST 558  ****                                             02380039
C                                                                       02390039
      IF (ICZERO) 35580, 5580, 35580                                    02400039
 5580 CONTINUE                                                          02410039
      IVON02 = 3                                                        02420039
      IVCOMP = 24/IVON02/4                                              02430039
      GO TO 45580                                                       02440039
35580 IVDELE = IVDELE + 1                                               02450039
      WRITE (I02,80003) IVTNUM                                          02460039
      IF (ICZERO) 45580, 5591, 45580                                    02470039
45580 IF (IVCOMP - 2) 25580,15580,25580                                 02480039
15580 IVPASS = IVPASS + 1                                               02490039
      WRITE (I02,80001) IVTNUM                                          02500039
      GO TO 5591                                                        02510039
25580 IVFAIL = IVFAIL + 1                                               02520039
      IVCORR = 2                                                        02530039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02540039
 5591 CONTINUE                                                          02550039
      IVTNUM = 559                                                      02560039
C                                                                       02570039
C      ****  TEST 559  ****                                             02580039
C                                                                       02590039
      IF (ICZERO) 35590, 5590, 35590                                    02600039
 5590 CONTINUE                                                          02610039
      IVON02 = 3                                                        02620039
      IVCOMP = 7151/IVON02/10                                           02630039
      GO TO 45590                                                       02640039
35590 IVDELE = IVDELE + 1                                               02650039
      WRITE (I02,80003) IVTNUM                                          02660039
      IF (ICZERO) 45590, 5601, 45590                                    02670039
45590 IF (IVCOMP - 238) 25590,15590,25590                               02680039
15590 IVPASS = IVPASS + 1                                               02690039
      WRITE (I02,80001) IVTNUM                                          02700039
      GO TO 5601                                                        02710039
25590 IVFAIL = IVFAIL + 1                                               02720039
      IVCORR = 238                                                      02730039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02740039
 5601 CONTINUE                                                          02750039
      IVTNUM = 560                                                      02760039
C                                                                       02770039
C      ****  TEST 560  ****                                             02780039
C                                                                       02790039
      IF (ICZERO) 35600, 5600, 35600                                    02800039
 5600 CONTINUE                                                          02810039
      IVON02 = -3                                                       02820039
      IVCOMP = 330/IVON02/2                                             02830039
      GO TO 45600                                                       02840039
35600 IVDELE = IVDELE + 1                                               02850039
      WRITE (I02,80003) IVTNUM                                          02860039
      IF (ICZERO) 45600, 5611, 45600                                    02870039
45600 IF (IVCOMP +55) 25600,15600,25600                                 02880039
15600 IVPASS = IVPASS + 1                                               02890039
      WRITE (I02,80001) IVTNUM                                          02900039
      GO TO 5611                                                        02910039
25600 IVFAIL = IVFAIL + 1                                               02920039
      IVCORR = -55                                                      02930039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02940039
 5611 CONTINUE                                                          02950039
      IVTNUM = 561                                                      02960039
C                                                                       02970039
C      ****  TEST 561  ****                                             02980039
C                                                                       02990039
      IF (ICZERO) 35610, 5610, 35610                                    03000039
 5610 CONTINUE                                                          03010039
      IVON02 = +13                                                      03020039
      IVCOMP = 15249/IVON02/(-51)                                       03030039
      GO TO 45610                                                       03040039
35610 IVDELE = IVDELE + 1                                               03050039
      WRITE (I02,80003) IVTNUM                                          03060039
      IF (ICZERO) 45610, 5621, 45610                                    03070039
45610 IF (IVCOMP + 23) 25610,15610,25610                                03080039
15610 IVPASS = IVPASS + 1                                               03090039
      WRITE (I02,80001) IVTNUM                                          03100039
      GO TO 5621                                                        03110039
25610 IVFAIL = IVFAIL + 1                                               03120039
      IVCORR = -23                                                      03130039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03140039
 5621 CONTINUE                                                          03150039
      IVTNUM = 562                                                      03160039
C                                                                       03170039
C      ****  TEST 562  ****                                             03180039
C                                                                       03190039
      IF (ICZERO) 35620, 5620, 35620                                    03200039
 5620 CONTINUE                                                          03210039
      IVON02 = -4                                                       03220039
      IVCOMP = (-27342)/IVON02/(-3)                                     03230039
      GO TO 45620                                                       03240039
35620 IVDELE = IVDELE + 1                                               03250039
      WRITE (I02,80003) IVTNUM                                          03260039
      IF (ICZERO) 45620, 5631, 45620                                    03270039
45620 IF (IVCOMP + 2278) 25620,15620,25620                              03280039
15620 IVPASS = IVPASS + 1                                               03290039
      WRITE (I02,80001) IVTNUM                                          03300039
      GO TO 5631                                                        03310039
25620 IVFAIL = IVFAIL + 1                                               03320039
      IVCORR = -2278                                                    03330039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03340039
 5631 CONTINUE                                                          03350039
      IVTNUM = 563                                                      03360039
C                                                                       03370039
C      ****  TEST 563  ****                                             03380039
C                                                                       03390039
      IF (ICZERO) 35630, 5630, 35630                                    03400039
 5630 CONTINUE                                                          03410039
      IVON02 = -4                                                       03420039
      IVCOMP = -27342/(-IVON02)/(-3)                                    03430039
      GO TO 45630                                                       03440039
35630 IVDELE = IVDELE + 1                                               03450039
      WRITE (I02,80003) IVTNUM                                          03460039
      IF (ICZERO) 45630, 5641, 45630                                    03470039
45630 IF (IVCOMP - 2278) 25630,15630,25630                              03480039
15630 IVPASS = IVPASS + 1                                               03490039
      WRITE (I02,80001) IVTNUM                                          03500039
      GO TO 5641                                                        03510039
25630 IVFAIL = IVFAIL + 1                                               03520039
      IVCORR = 2278                                                     03530039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03540039
C                                                                       03550039
C     TEST 564 THROUGH TEST 569 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS03560039
C     OF THE FORM             IV = IC/IC/IV.                            03570039
C                                                                       03580039
 5641 CONTINUE                                                          03590039
      IVTNUM = 564                                                      03600039
C                                                                       03610039
C      ****  TEST 564  ****                                             03620039
C                                                                       03630039
      IF (ICZERO) 35640, 5640, 35640                                    03640039
 5640 CONTINUE                                                          03650039
      IVON03 = 4                                                        03660039
      IVCOMP = 24/3/IVON03                                              03670039
      GO TO 45640                                                       03680039
35640 IVDELE = IVDELE + 1                                               03690039
      WRITE (I02,80003) IVTNUM                                          03700039
      IF (ICZERO) 45640, 5651, 45640                                    03710039
45640 IF (IVCOMP -2) 25640,15640,25640                                  03720039
15640 IVPASS = IVPASS + 1                                               03730039
      WRITE (I02,80001) IVTNUM                                          03740039
      GO TO 5651                                                        03750039
25640 IVFAIL = IVFAIL + 1                                               03760039
      IVCORR = 2                                                        03770039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03780039
 5651 CONTINUE                                                          03790039
      IVTNUM = 565                                                      03800039
C                                                                       03810039
C      ****  TEST 565  ****                                             03820039
C                                                                       03830039
      IF (ICZERO) 35650, 5650, 35650                                    03840039
 5650 CONTINUE                                                          03850039
      IVON03 = 10                                                       03860039
      IVCOMP = 7151/3/IVON03                                            03870039
      GO TO 45650                                                       03880039
35650 IVDELE = IVDELE + 1                                               03890039
      WRITE (I02,80003) IVTNUM                                          03900039
      IF (ICZERO) 45650, 5661, 45650                                    03910039
45650 IF (IVCOMP - 238) 25650,15650,25650                               03920039
15650 IVPASS = IVPASS + 1                                               03930039
      WRITE (I02,80001) IVTNUM                                          03940039
      GO TO 5661                                                        03950039
25650 IVFAIL = IVFAIL + 1                                               03960039
      IVCORR = 238                                                      03970039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03980039
 5661 CONTINUE                                                          03990039
      IVTNUM = 566                                                      04000039
C                                                                       04010039
C      ****  TEST 566  ****                                             04020039
C                                                                       04030039
      IF (ICZERO) 35660, 5660, 35660                                    04040039
 5660 CONTINUE                                                          04050039
      IVON03 = -2                                                       04060039
      IVCOMP = 330/3/IVON03                                             04070039
      GO TO 45660                                                       04080039
35660 IVDELE = IVDELE + 1                                               04090039
      WRITE (I02,80003) IVTNUM                                          04100039
      IF (ICZERO) 45660, 5671, 45660                                    04110039
45660 IF (IVCOMP + 55) 25660,15660,25660                                04120039
15660 IVPASS = IVPASS + 1                                               04130039
      WRITE (I02,80001) IVTNUM                                          04140039
      GO TO 5671                                                        04150039
25660 IVFAIL = IVFAIL + 1                                               04160039
      IVCORR = -55                                                      04170039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04180039
 5671 CONTINUE                                                          04190039
      IVTNUM = 567                                                      04200039
C                                                                       04210039
C      ****  TEST 567  ****                                             04220039
C                                                                       04230039
      IF (ICZERO) 35670, 5670, 35670                                    04240039
 5670 CONTINUE                                                          04250039
      IVON03 = +51                                                      04260039
      IVCOMP = 15249/(-13)/IVON03                                       04270039
      GO TO 45670                                                       04280039
35670 IVDELE = IVDELE + 1                                               04290039
      WRITE (I02,80003) IVTNUM                                          04300039
      IF (ICZERO) 45670, 5681, 45670                                    04310039
45670 IF (IVCOMP + 23) 25670,15670,25670                                04320039
15670 IVPASS = IVPASS + 1                                               04330039
      WRITE (I02,80001) IVTNUM                                          04340039
      GO TO 5681                                                        04350039
25670 IVFAIL = IVFAIL + 1                                               04360039
      IVCORR = -23                                                      04370039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04380039
 5681 CONTINUE                                                          04390039
      IVTNUM = 568                                                      04400039
C                                                                       04410039
C      ****  TEST 568  ****                                             04420039
C                                                                       04430039
      IF (ICZERO) 35680, 5680, 35680                                    04440039
 5680 CONTINUE                                                          04450039
      IVON03 = -3                                                       04460039
      IVCOMP = (-27342)/(-4)/IVON03                                     04470039
      GO TO 45680                                                       04480039
35680 IVDELE = IVDELE + 1                                               04490039
      WRITE (I02,80003) IVTNUM                                          04500039
      IF (ICZERO) 45680, 5691, 45680                                    04510039
45680 IF (IVCOMP + 2278) 25680,15680,25680                              04520039
15680 IVPASS = IVPASS + 1                                               04530039
      WRITE (I02,80001) IVTNUM                                          04540039
      GO TO 5691                                                        04550039
25680 IVFAIL = IVFAIL + 1                                               04560039
      IVCORR = -2278                                                    04570039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04580039
 5691 CONTINUE                                                          04590039
      IVTNUM = 569                                                      04600039
C                                                                       04610039
C      ****  TEST 569  ****                                             04620039
C                                                                       04630039
      IF (ICZERO) 35690, 5690, 35690                                    04640039
 5690 CONTINUE                                                          04650039
      IVON03 = -3                                                       04660039
      IVCOMP = -27342/(-4)/(-IVON03)                                    04670039
      GO TO 45690                                                       04680039
35690 IVDELE = IVDELE + 1                                               04690039
      WRITE (I02,80003) IVTNUM                                          04700039
      IF (ICZERO) 45690, 5701, 45690                                    04710039
45690 IF (IVCOMP - 2278) 25690,15690,25690                              04720039
15690 IVPASS = IVPASS + 1                                               04730039
      WRITE (I02,80001) IVTNUM                                          04740039
      GO TO 5701                                                        04750039
25690 IVFAIL = IVFAIL + 1                                               04760039
      IVCORR = 2278                                                     04770039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04780039
C                                                                       04790039
C     TEST 570 AND TEST 571  -   IV =(IV/IC)/IC                         04800039
C                                                                       04810039
 5701 CONTINUE                                                          04820039
      IVTNUM = 570                                                      04830039
C                                                                       04840039
C      ****  TEST 570  ****                                             04850039
C                                                                       04860039
      IF (ICZERO) 35700, 5700, 35700                                    04870039
 5700 CONTINUE                                                          04880039
      IVON01 = 24                                                       04890039
      IVCOMP = (IVON01/3)/4                                             04900039
      GO TO 45700                                                       04910039
35700 IVDELE = IVDELE + 1                                               04920039
      WRITE (I02,80003) IVTNUM                                          04930039
      IF (ICZERO) 45700, 5711, 45700                                    04940039
45700 IF (IVCOMP -2) 25700,15700,25700                                  04950039
15700 IVPASS = IVPASS + 1                                               04960039
      WRITE (I02,80001) IVTNUM                                          04970039
      GO TO 5711                                                        04980039
25700 IVFAIL = IVFAIL + 1                                               04990039
      IVCORR = 2                                                        05000039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05010039
 5711 CONTINUE                                                          05020039
      IVTNUM = 571                                                      05030039
C                                                                       05040039
C      ****  TEST 571  ****                                             05050039
C                                                                       05060039
      IF (ICZERO) 35710, 5710, 35710                                    05070039
 5710 CONTINUE                                                          05080039
      IVON01 = -330                                                     05090039
      IVCOMP = (IVON01/(-3))/4                                          05100039
      GO TO 45710                                                       05110039
35710 IVDELE = IVDELE + 1                                               05120039
      WRITE (I02,80003) IVTNUM                                          05130039
      IF (ICZERO) 45710, 5721, 45710                                    05140039
45710 IF (IVCOMP - 27) 25710,15710,25710                                05150039
15710 IVPASS = IVPASS + 1                                               05160039
      WRITE (I02,80001) IVTNUM                                          05170039
      GO TO 5721                                                        05180039
25710 IVFAIL = IVFAIL + 1                                               05190039
      IVCORR = 27                                                       05200039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05210039
C                                                                       05220039
C     TEST 572 AND TEST 573  -  IV= IV/(IC/IC)                          05230039
C                                                                       05240039
 5721 CONTINUE                                                          05250039
      IVTNUM = 572                                                      05260039
C                                                                       05270039
C      ****  TEST 572  ****                                             05280039
C                                                                       05290039
      IF (ICZERO) 35720, 5720, 35720                                    05300039
 5720 CONTINUE                                                          05310039
      IVON01 = 24                                                       05320039
      IVCOMP = IVON01/(8/4)                                             05330039
      GO TO 45720                                                       05340039
35720 IVDELE = IVDELE + 1                                               05350039
      WRITE (I02,80003) IVTNUM                                          05360039
      IF (ICZERO) 45720, 5731, 45720                                    05370039
45720 IF (IVCOMP - 12) 25720,15720,25720                                05380039
15720 IVPASS = IVPASS + 1                                               05390039
      WRITE (I02,80001) IVTNUM                                          05400039
      GO TO 5731                                                        05410039
25720 IVFAIL = IVFAIL + 1                                               05420039
      IVCORR = 12                                                       05430039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05440039
 5731 CONTINUE                                                          05450039
      IVTNUM = 573                                                      05460039
C                                                                       05470039
C      ****  TEST 573  ****                                             05480039
C                                                                       05490039
      IF (ICZERO) 35730, 5730, 35730                                    05500039
 5730 CONTINUE                                                          05510039
      IVON01 = -7154                                                    05520039
      IVCOMP = -IVON01/((-26)/5)                                        05530039
      GO TO 45730                                                       05540039
35730 IVDELE = IVDELE + 1                                               05550039
      WRITE (I02,80003) IVTNUM                                          05560039
      IF (ICZERO) 45730, 5741, 45730                                    05570039
45730 IF (IVCOMP + 1430) 25730,15730,25730                              05580039
15730 IVPASS = IVPASS + 1                                               05590039
      WRITE (I02,80001) IVTNUM                                          05600039
      GO TO 5741                                                        05610039
25730 IVFAIL = IVFAIL + 1                                               05620039
      IVCORR = -1430                                                    05630039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05640039
C                                                                       05650039
C     TEST 574 AND TEST 575  -  IV=(IC/IV)/IC                           05660039
C                                                                       05670039
 5741 CONTINUE                                                          05680039
      IVTNUM = 574                                                      05690039
C                                                                       05700039
C      ****  TEST 574  ****                                             05710039
C                                                                       05720039
      IF (ICZERO) 35740, 5740, 35740                                    05730039
 5740 CONTINUE                                                          05740039
      IVON02 = 3                                                        05750039
      IVCOMP = (24/IVON02)/4                                            05760039
      GO TO 45740                                                       05770039
35740 IVDELE = IVDELE + 1                                               05780039
      WRITE (I02,80003) IVTNUM                                          05790039
      IF (ICZERO) 45740, 5751, 45740                                    05800039
45740 IF (IVCOMP -2) 25740,15740,25740                                  05810039
15740 IVPASS = IVPASS + 1                                               05820039
      WRITE (I02,80001) IVTNUM                                          05830039
      GO TO 5751                                                        05840039
25740 IVFAIL = IVFAIL + 1                                               05850039
      IVCORR = 2                                                        05860039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05870039
 5751 CONTINUE                                                          05880039
      IVTNUM = 575                                                      05890039
C                                                                       05900039
C      ****  TEST 575  ****                                             05910039
C                                                                       05920039
      IF (ICZERO) 35750, 5750, 35750                                    05930039
 5750 CONTINUE                                                          05940039
      IVON02 = -3                                                       05950039
      IVCOMP = (-330/IVON02)/(-4)                                       05960039
      GO TO 45750                                                       05970039
35750 IVDELE = IVDELE + 1                                               05980039
      WRITE (I02,80003) IVTNUM                                          05990039
      IF (ICZERO) 45750, 5761, 45750                                    06000039
45750 IF (IVCOMP + 27) 25750,15750,25750                                06010039
15750 IVPASS = IVPASS + 1                                               06020039
      WRITE (I02,80001) IVTNUM                                          06030039
      GO TO 5761                                                        06040039
25750 IVFAIL = IVFAIL + 1                                               06050039
      IVCORR = -27                                                      06060039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06070039
C                                                                       06080039
C     TEST 576 AND TEST 577  -  IV=IC/(IV/IC)                           06090039
C                                                                       06100039
 5761 CONTINUE                                                          06110039
      IVTNUM = 576                                                      06120039
C                                                                       06130039
C      ****  TEST 576  ****                                             06140039
C                                                                       06150039
      IF (ICZERO) 35760, 5760, 35760                                    06160039
 5760 CONTINUE                                                          06170039
      IVON02 = 8                                                        06180039
      IVCOMP = 24/(IVON02/4)                                            06190039
      GO TO 45760                                                       06200039
35760 IVDELE = IVDELE + 1                                               06210039
      WRITE (I02,80003) IVTNUM                                          06220039
      IF (ICZERO) 45760, 5771, 45760                                    06230039
45760 IF (IVCOMP - 12) 25760,15760,25760                                06240039
15760 IVPASS = IVPASS + 1                                               06250039
      WRITE (I02,80001) IVTNUM                                          06260039
      GO TO 5771                                                        06270039
25760 IVFAIL = IVFAIL + 1                                               06280039
      IVCORR = 12                                                       06290039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06300039
 5771 CONTINUE                                                          06310039
      IVTNUM = 577                                                      06320039
C                                                                       06330039
C      ****  TEST 577  ****                                             06340039
C                                                                       06350039
      IF (ICZERO) 35770, 5770, 35770                                    06360039
 5770 CONTINUE                                                          06370039
      IVON02 = -26                                                      06380039
      IVCOMP = 7154/((-IVON02)/(-5))                                    06390039
      GO TO 45770                                                       06400039
35770 IVDELE = IVDELE + 1                                               06410039
      WRITE (I02,80003) IVTNUM                                          06420039
      IF (ICZERO) 45770, 5781, 45770                                    06430039
45770 IF (IVCOMP + 1430) 25770,15770,25770                              06440039
15770 IVPASS = IVPASS + 1                                               06450039
      WRITE (I02,80001) IVTNUM                                          06460039
      GO TO 5781                                                        06470039
25770 IVFAIL = IVFAIL + 1                                               06480039
      IVCORR = -1430                                                    06490039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06500039
C                                                                       06510039
C     TEST 578 AND TEST 579  -  IV=(IC/IC)/IV                           06520039
C                                                                       06530039
 5781 CONTINUE                                                          06540039
      IVTNUM = 578                                                      06550039
C                                                                       06560039
C      ****  TEST 578  ****                                             06570039
C                                                                       06580039
      IF (ICZERO) 35780, 5780, 35780                                    06590039
 5780 CONTINUE                                                          06600039
      IVON03 = 4                                                        06610039
      IVCOMP = (24/3)/IVON03                                            06620039
      GO TO 45780                                                       06630039
35780 IVDELE = IVDELE + 1                                               06640039
      WRITE (I02,80003) IVTNUM                                          06650039
      IF (ICZERO) 45780, 5791, 45780                                    06660039
45780 IF (IVCOMP - 2) 25780,15780,25780                                 06670039
15780 IVPASS = IVPASS + 1                                               06680039
      WRITE (I02,80001) IVTNUM                                          06690039
      GO TO 5791                                                        06700039
25780 IVFAIL = IVFAIL + 1                                               06710039
      IVCORR = 2                                                        06720039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06730039
 5791 CONTINUE                                                          06740039
      IVTNUM = 579                                                      06750039
C                                                                       06760039
C      ****  TEST 579  ****                                             06770039
C                                                                       06780039
      IF (ICZERO) 35790, 5790, 35790                                    06790039
 5790 CONTINUE                                                          06800039
      IVON03 = -4                                                       06810039
      IVCOMP = (330/(-3))/IVON03                                        06820039
      GO TO 45790                                                       06830039
35790 IVDELE = IVDELE + 1                                               06840039
      WRITE (I02,80003) IVTNUM                                          06850039
      IF (ICZERO) 45790, 5801, 45790                                    06860039
45790 IF (IVCOMP - 27) 25790,15790,25790                                06870039
15790 IVPASS = IVPASS + 1                                               06880039
      WRITE (I02,80001) IVTNUM                                          06890039
      GO TO 5801                                                        06900039
25790 IVFAIL = IVFAIL + 1                                               06910039
      IVCORR = 27                                                       06920039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06930039
C                                                                       06940039
C     TEST 580 AND TEST 581  -  IV= IC/(IC/IV)                          06950039
C                                                                       06960039
 5801 CONTINUE                                                          06970039
      IVTNUM = 580                                                      06980039
C                                                                       06990039
C      ****  TEST 580  ****                                             07000039
C                                                                       07010039
      IF (ICZERO) 35800, 5800, 35800                                    07020039
 5800 CONTINUE                                                          07030039
      IVON03 = 4                                                        07040039
      IVCOMP = 24/(8/IVON03)                                            07050039
      GO TO 45800                                                       07060039
35800 IVDELE = IVDELE + 1                                               07070039
      WRITE (I02,80003) IVTNUM                                          07080039
      IF (ICZERO) 45800, 5811, 45800                                    07090039
45800 IF (IVCOMP - 12) 25800,15800,25800                                07100039
15800 IVPASS = IVPASS + 1                                               07110039
      WRITE (I02,80001) IVTNUM                                          07120039
      GO TO 5811                                                        07130039
25800 IVFAIL = IVFAIL + 1                                               07140039
      IVCORR = 12                                                       07150039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07160039
 5811 CONTINUE                                                          07170039
      IVTNUM = 581                                                      07180039
C                                                                       07190039
C      ****  TEST 581  ****                                             07200039
C                                                                       07210039
      IF (ICZERO) 35810, 5810, 35810                                    07220039
 5810 CONTINUE                                                          07230039
      IVON03 = -5                                                       07240039
      IVCOMP = -7154/((-26)/IVON03)                                     07250039
      GO TO 45810                                                       07260039
35810 IVDELE = IVDELE + 1                                               07270039
      WRITE (I02,80003) IVTNUM                                          07280039
      IF (ICZERO) 45810, 5821, 45810                                    07290039
45810 IF (IVCOMP + 1430) 25810,15810,25810                              07300039
15810 IVPASS = IVPASS + 1                                               07310039
      WRITE (I02,80001) IVTNUM                                          07320039
      GO TO 5821                                                        07330039
25810 IVFAIL = IVFAIL + 1                                               07340039
      IVCORR = -1430                                                    07350039
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07360039
C                                                                       07370039
C      ****    END OF TESTS    ****                                     07380039
 5821 CONTINUE                                                          07390039
C                                                                       07400039
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07410039
99999 CONTINUE                                                          07420039
      WRITE (I02,90002)                                                 07430039
      WRITE (I02,90006)                                                 07440039
      WRITE (I02,90002)                                                 07450039
      WRITE (I02,90002)                                                 07460039
      WRITE (I02,90007)                                                 07470039
      WRITE (I02,90002)                                                 07480039
      WRITE (I02,90008)  IVFAIL                                         07490039
      WRITE (I02,90009) IVPASS                                          07500039
      WRITE (I02,90010) IVDELE                                          07510039
C                                                                       07520039
C                                                                       07530039
C     TERMINATE ROUTINE EXECUTION                                       07540039
      STOP                                                              07550039
C                                                                       07560039
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07570039
90000 FORMAT (1H1)                                                      07580039
90002 FORMAT (1H )                                                      07590039
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07600039
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07610039
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07620039
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07630039
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07640039
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07650039
C                                                                       07660039
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07670039
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07680039
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07690039
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07700039
C                                                                       07710039
C     FORMAT STATEMENTS FOR TEST RESULTS                                07720039
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07730039
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07740039
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07750039
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07760039
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07770039
C                                                                       07780039
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM039)                          07790039
      END                                                               07800039
