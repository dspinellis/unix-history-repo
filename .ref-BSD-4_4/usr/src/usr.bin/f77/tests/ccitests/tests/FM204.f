      PROGRAM FM204                                                     00010204
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020204
C                                                                       00030204
C        THIS ROUTINE CONTINUES THE TESTING OF CHARACTER VARIABLES AND  00040204
C     CHARACTER ARRAYS OF LENGTH ONE.  THE CHARACTER FEATURES TESTED IN 00050204
C     FM202 AND FM203 ARE USED IN THE TESTS IN THIS ROUTINE.  THE       00060204
C     FOLLOWING CHARACTER FEATURES ARE TESTED                           00070204
C                                                                       00080204
C        (1)  INITIAL DEFINITION OF CHARACTER ENTITIES OF LENGTH ONE BY 00090204
C      SPECIFYING THEM IN A DATA STATEMENT.                             00100204
C                                                                       00110204
C        (2)  THE SUBSET FORTRAN LANGUAGE SPECIFIES THE FOLLOWING       00120204
C     COLLATING SEQUENCE RULES.                                         00130204
C                                                                       00140204
C              A LESS THAN B ... LESS THAN Z,                           00150204
C              0 LESS THAN 1 ... LESS THAN 9,                           00160204
C              ALL OF THE DIGITS PRECEDE A OR ALL OF THE DIGITS FOLLOW  00170204
C                  Z,                                                   00180204
C              BLANK IS LESS THAN THE LETTER A AND BLANK IS LESS THAN   00190204
C                  THE DIGIT ZERO.                                      00200204
C                                                                       00210204
C        (3)  THE VALUE OF THE INTRINSIC FUNCTION ICHAR IS AN INTEGER   00220204
C      IN THE RANGE (0, N-1), WHERE N IS THE NUMBER OF CHARACTERS IN    00230204
C      THE COLLATING SEQUENCE FOR THE PROCESSOR.  FOR ANY CHARACTERS    00240204
C      C1 AND C2 CAPABLE OF REPRESENTATION IN THE PROCESSOR, C1 .LE. C2 00250204
C      IS TRUE IF AND ONLY IF ICHAR(C1) .LE. ICHAR(C2) IS TRUE; AND     00260204
C      C1 .EQ. C2 IF AND ONLY IF ICHAR(C1) .EQ. ICHAR(C2).              00270204
C                                                                       00280204
C      REFERENCES                                                       00290204
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00300204
C              X3.9-1978                                                00310204
C                                                                       00320204
C        SECTION 3.1.5, COLLATING SEQUENCE AND GRAPHICS                 00330204
C        SECTION 4.8, CHARACTER TYPE                                    00340204
C        SECTION 6.2, CHARACTER EXPRESSIONS                             00350204
C        SECTION 6.3.4, CHARACTER RELATIONAL EXPRESSIONS                00360204
C        SECTION 6.3.5, INTERPRETATION OF CHARACTER RELATIONAL          00370204
C                          EXPRESSIONS                                  00380204
C        SECTION 8.4.2, CHARACTER TYPE-STATEMENT                        00390204
C        SECTION 9.4, CHARACTER CONSTANT IN A DATA STATEMENT            00400204
C        SECTION 10.4, CHARACTER ASSIGNMENT STATEMENT                   00410204
C        SECTION 15.3, INTRINSIC FUNCTIONS                              00420204
C        SECTION 15.10, TABLE 5 INTRINSIC FUNCTIONS                     00430204
C                                                                       00440204
C                                                                       00450204
C     ******************************************************************00460204
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00470204
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00480204
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00490204
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00500204
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00510204
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00520204
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00530204
C     THE RESULT OF EXECUTING THESE TESTS.                              00540204
C                                                                       00550204
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00560204
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00570204
C                                                                       00580204
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00590204
C                    DEPARTMENT OF THE NAVY                             00600204
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00610204
C                    WASHINGTON, D.C.   20376                           00620204
C                                                                       00630204
C     ******************************************************************00640204
C                                                                       00650204
C                                                                       00660204
      IMPLICIT LOGICAL (L)                                              00670204
      IMPLICIT CHARACTER*14 (C)                                         00680204
C                                                                       00690204
      CHARACTER*1  CATN11(47), CATN12(26), CATN13(10)                   00700204
      CHARACTER  CVTN10*1, CATN14(6)*1, CVTN01                          00710204
      DIMENSION IAON11(47)                                              00720204
      DATA CATN11/'A','B','C','D','E','F','G','H','I','J','K','L','M',  00730204
     1     'N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1', 00740204
     2     '2','3','4','5','6','7','8','9',' ','=','+','-','*','/','(', 00750204
     3     ')',',','.',''''/                                            00760204
      DATA CATN12/'A','B','C','D','E','F','G','H','I','J','K','L','M',  00770204
     1     'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/         00780204
      DATA CATN14(1),CATN14(2),CATN14(3),CATN14(4),CATN14(5),CATN14(6)  00790204
     1     /6*'V'/,IAON11/47*7/, CATN13/'0','1','2','3','4','5','6',    00800204
     2     '7','8','9'/,CVTN10/' '/                                     00810204
C                                                                       00820204
C                                                                       00830204
C                                                                       00840204
C     INITIALIZATION SECTION.                                           00850204
C                                                                       00860204
C     INITIALIZE CONSTANTS                                              00870204
C     ********************                                              00880204
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00890204
      I01 = 5                                                           00900204
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00910204
      I02 = 6                                                           00920204
C     SYSTEM ENVIRONMENT SECTION                                        00930204
C                                                                       00940204
      I01 = 5                                                           00950204
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00960204
C     (UNIT NUMBER FOR CARD READER).                                    00970204
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00980204
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00990204
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01000204
C                                                                       01010204
      I02 = 6                                                           01020204
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01030204
C     (UNIT NUMBER FOR PRINTER).                                        01040204
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01050204
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01060204
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01070204
C                                                                       01080204
      IVPASS = 0                                                        01090204
      IVFAIL = 0                                                        01100204
      IVDELE = 0                                                        01110204
      ICZERO = 0                                                        01120204
C                                                                       01130204
C     WRITE OUT PAGE HEADERS                                            01140204
C                                                                       01150204
      WRITE (I02,90002)                                                 01160204
      WRITE (I02,90006)                                                 01170204
      WRITE (I02,90008)                                                 01180204
      WRITE (I02,90004)                                                 01190204
      WRITE (I02,90010)                                                 01200204
      WRITE (I02,90004)                                                 01210204
      WRITE (I02,90016)                                                 01220204
      WRITE (I02,90001)                                                 01230204
      WRITE (I02,90004)                                                 01240204
      WRITE (I02,90012)                                                 01250204
      WRITE (I02,90014)                                                 01260204
      WRITE (I02,90004)                                                 01270204
C                                                                       01280204
C                                                                       01290204
C        TEST 61 THROUGH TEST 73 VERIFY THE CONTENTS OF CHARACTER ARRAY 01300204
C     ELEMENTS AND CHARACTER VARIABLES WHICH WERE INITIALLY DEFINED IN  01310204
C     A DATA STATEMENT.                                                 01320204
C                                                                       01330204
C        TEST 61 THROUGH TEST 65 VERIFY THE CONTENTS OF SELECTED        01340204
C     ELEMENTS OF THE ARRAY CATN11 WHICH WAS INITIALLY SET EQUAL TO THE 01350204
C     47 CHARACTERS OF THE FORTRAN SUBSET LANGUAGE CHARACTER SET.       01360204
C                                                                       01370204
C                                                                       01380204
C     ****  FCVS PROGRAM 204  -  TEST 061  ****                         01390204
C                                                                       01400204
C                                                                       01410204
      IVTNUM =  61                                                      01420204
      IF (ICZERO) 30610, 0610, 30610                                    01430204
 0610 CONTINUE                                                          01440204
      IVCOMP = 0                                                        01450204
      IVCORR = 1                                                        01460204
      IF (CATN11(1) .EQ. 'A') IVCOMP = 1                                01470204
40610 IF (IVCOMP - 1) 20610, 10610, 20610                               01480204
30610 IVDELE = IVDELE + 1                                               01490204
      WRITE (I02,80000) IVTNUM                                          01500204
      IF (ICZERO) 10610, 0621, 20610                                    01510204
10610 IVPASS = IVPASS + 1                                               01520204
      WRITE (I02,80002) IVTNUM                                          01530204
      GO TO 0621                                                        01540204
20610 IVFAIL = IVFAIL + 1                                               01550204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01560204
 0621 CONTINUE                                                          01570204
C                                                                       01580204
C     ****  FCVS PROGRAM 204  -  TEST 062  ****                         01590204
C                                                                       01600204
C                                                                       01610204
      IVTNUM =  62                                                      01620204
      IF (ICZERO) 30620, 0620, 30620                                    01630204
 0620 CONTINUE                                                          01640204
      IVCOMP = 0                                                        01650204
      IVCORR = 1                                                        01660204
      IF (CATN11(47) .EQ. '''') IVCOMP = 1                              01670204
40620 IF (IVCOMP - 1) 20620, 10620, 20620                               01680204
30620 IVDELE = IVDELE + 1                                               01690204
      WRITE (I02,80000) IVTNUM                                          01700204
      IF (ICZERO) 10620, 0631, 20620                                    01710204
10620 IVPASS = IVPASS + 1                                               01720204
      WRITE (I02,80002) IVTNUM                                          01730204
      GO TO 0631                                                        01740204
20620 IVFAIL = IVFAIL + 1                                               01750204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01760204
 0631 CONTINUE                                                          01770204
C                                                                       01780204
C     ****  FCVS PROGRAM 204  -  TEST 063  ****                         01790204
C                                                                       01800204
C                                                                       01810204
      IVTNUM =  63                                                      01820204
      IF (ICZERO) 30630, 0630, 30630                                    01830204
 0630 CONTINUE                                                          01840204
      IVCOMP = 0                                                        01850204
      IVCORR = 1                                                        01860204
      IF (CATN11(46) .EQ. '.') IVCOMP = 1                               01870204
40630 IF (IVCOMP - 1) 20630, 10630, 20630                               01880204
30630 IVDELE = IVDELE + 1                                               01890204
      WRITE (I02,80000) IVTNUM                                          01900204
      IF (ICZERO) 10630, 0641, 20630                                    01910204
10630 IVPASS = IVPASS + 1                                               01920204
      WRITE (I02,80002) IVTNUM                                          01930204
      GO TO 0641                                                        01940204
20630 IVFAIL = IVFAIL + 1                                               01950204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01960204
 0641 CONTINUE                                                          01970204
C                                                                       01980204
C     ****  FCVS PROGRAM 204  -  TEST 064  ****                         01990204
C                                                                       02000204
C                                                                       02010204
      IVTNUM =  64                                                      02020204
      IF (ICZERO) 30640, 0640, 30640                                    02030204
 0640 CONTINUE                                                          02040204
      IVCOMP = 0                                                        02050204
      IVCORR = 1                                                        02060204
      IF (CATN11(27) .EQ. '0') IVCOMP = 1                               02070204
40640 IF (IVCOMP - 1) 20640, 10640, 20640                               02080204
30640 IVDELE = IVDELE + 1                                               02090204
      WRITE (I02,80000) IVTNUM                                          02100204
      IF (ICZERO) 10640, 0651, 20640                                    02110204
10640 IVPASS = IVPASS + 1                                               02120204
      WRITE (I02,80002) IVTNUM                                          02130204
      GO TO 0651                                                        02140204
20640 IVFAIL = IVFAIL + 1                                               02150204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02160204
 0651 CONTINUE                                                          02170204
C                                                                       02180204
C     ****  FCVS PROGRAM 204  -  TEST 065  ****                         02190204
C                                                                       02200204
C                                                                       02210204
      IVTNUM =  65                                                      02220204
      IF (ICZERO) 30650, 0650, 30650                                    02230204
 0650 CONTINUE                                                          02240204
      IVCOMP = 0                                                        02250204
      IVCORR = 1                                                        02260204
      IF (CATN11(36) .EQ. '9') IVCOMP = 1                               02270204
40650 IF (IVCOMP - 1) 20650, 10650, 20650                               02280204
30650 IVDELE = IVDELE + 1                                               02290204
      WRITE (I02,80000) IVTNUM                                          02300204
      IF (ICZERO) 10650, 0661, 20650                                    02310204
10650 IVPASS = IVPASS + 1                                               02320204
      WRITE (I02,80002) IVTNUM                                          02330204
      GO TO 0661                                                        02340204
20650 IVFAIL = IVFAIL + 1                                               02350204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02360204
 0661 CONTINUE                                                          02370204
C                                                                       02380204
C        TEST 66 THROUGH TEST 68 VERIFY THE CONTENTS OF SELECTED        02390204
C     ELEMENTS OF THE ARRAY CATN12 WHICH WAS INITIALLY SET EQUAL TO THE 02400204
C     26 LETTERS OF THE ALPHABET.                                       02410204
C                                                                       02420204
C                                                                       02430204
C     ****  FCVS PROGRAM 204  -  TEST 066  ****                         02440204
C                                                                       02450204
C                                                                       02460204
      IVTNUM =  66                                                      02470204
      IF (ICZERO) 30660, 0660, 30660                                    02480204
 0660 CONTINUE                                                          02490204
      IVCOMP = 0                                                        02500204
      IVCORR = 1                                                        02510204
      IF (CATN12(1) .EQ. 'A') IVCOMP = 1                                02520204
40660 IF (IVCOMP - 1) 20660, 10660, 20660                               02530204
30660 IVDELE = IVDELE + 1                                               02540204
      WRITE (I02,80000) IVTNUM                                          02550204
      IF (ICZERO) 10660, 0671, 20660                                    02560204
10660 IVPASS = IVPASS + 1                                               02570204
      WRITE (I02,80002) IVTNUM                                          02580204
      GO TO 0671                                                        02590204
20660 IVFAIL = IVFAIL + 1                                               02600204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02610204
 0671 CONTINUE                                                          02620204
C                                                                       02630204
C     ****  FCVS PROGRAM 204  -  TEST 067  ****                         02640204
C                                                                       02650204
C                                                                       02660204
      IVTNUM =  67                                                      02670204
      IF (ICZERO) 30670, 0670, 30670                                    02680204
 0670 CONTINUE                                                          02690204
      IVCOMP = 0                                                        02700204
      IVCORR = 1                                                        02710204
      IF (CATN12(26) .EQ. 'Z') IVCOMP = 1                               02720204
40670 IF (IVCOMP - 1) 20670, 10670, 20670                               02730204
30670 IVDELE = IVDELE + 1                                               02740204
      WRITE (I02,80000) IVTNUM                                          02750204
      IF (ICZERO) 10670, 0681, 20670                                    02760204
10670 IVPASS = IVPASS + 1                                               02770204
      WRITE (I02,80002) IVTNUM                                          02780204
      GO TO 0681                                                        02790204
20670 IVFAIL = IVFAIL + 1                                               02800204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02810204
 0681 CONTINUE                                                          02820204
C                                                                       02830204
C     ****  FCVS PROGRAM 204  -  TEST 068  ****                         02840204
C                                                                       02850204
C                                                                       02860204
      IVTNUM =  68                                                      02870204
      IF (ICZERO) 30680, 0680, 30680                                    02880204
 0680 CONTINUE                                                          02890204
      IVCOMP = 0                                                        02900204
      IVCORR = 1                                                        02910204
      IF (CATN12(20) .EQ. 'T') IVCOMP = 1                               02920204
40680 IF (IVCOMP - 1) 20680, 10680, 20680                               02930204
30680 IVDELE = IVDELE + 1                                               02940204
      WRITE (I02,80000) IVTNUM                                          02950204
      IF (ICZERO) 10680, 0691, 20680                                    02960204
10680 IVPASS = IVPASS + 1                                               02970204
      WRITE (I02,80002) IVTNUM                                          02980204
      GO TO 0691                                                        02990204
20680 IVFAIL = IVFAIL + 1                                               03000204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03010204
 0691 CONTINUE                                                          03020204
C                                                                       03030204
C        TEST 69 AND TEST 70 VERIFY THE CONTENTS OF SELECTED ELEMENTS   03040204
C     OF THE ARRAY CATN13 WHICH WAS INITIALLY SET EQUAL TO THE TEN      03050204
C     NUMERIC DIGITS.                                                   03060204
C                                                                       03070204
C                                                                       03080204
C     ****  FCVS PROGRAM 204  -  TEST 069  ****                         03090204
C                                                                       03100204
C                                                                       03110204
      IVTNUM =  69                                                      03120204
      IF (ICZERO) 30690, 0690, 30690                                    03130204
 0690 CONTINUE                                                          03140204
      IVCOMP = 0                                                        03150204
      IVCORR = 1                                                        03160204
      IF (CATN13(1) .EQ. '0') IVCOMP = 1                                03170204
40690 IF (IVCOMP - 1) 20690, 10690, 20690                               03180204
30690 IVDELE = IVDELE + 1                                               03190204
      WRITE (I02,80000) IVTNUM                                          03200204
      IF (ICZERO) 10690, 0701, 20690                                    03210204
10690 IVPASS = IVPASS + 1                                               03220204
      WRITE (I02,80002) IVTNUM                                          03230204
      GO TO 0701                                                        03240204
20690 IVFAIL = IVFAIL + 1                                               03250204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03260204
 0701 CONTINUE                                                          03270204
C                                                                       03280204
C     ****  FCVS PROGRAM 204  -  TEST 070  ****                         03290204
C                                                                       03300204
C                                                                       03310204
      IVTNUM =  70                                                      03320204
      IF (ICZERO) 30700, 0700, 30700                                    03330204
 0700 CONTINUE                                                          03340204
      IVCOMP = 0                                                        03350204
      IVCORR = 1                                                        03360204
      IF (CATN13(10) .EQ. '9') IVCOMP = 1                               03370204
40700 IF (IVCOMP - 1) 20700, 10700, 20700                               03380204
30700 IVDELE = IVDELE + 1                                               03390204
      WRITE (I02,80000) IVTNUM                                          03400204
      IF (ICZERO) 10700, 0711, 20700                                    03410204
10700 IVPASS = IVPASS + 1                                               03420204
      WRITE (I02,80002) IVTNUM                                          03430204
      GO TO 0711                                                        03440204
20700 IVFAIL = IVFAIL + 1                                               03450204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03460204
 0711 CONTINUE                                                          03470204
C                                                                       03480204
C     ****  FCVS PROGRAM 204  -  TEST 071  ****                         03490204
C                                                                       03500204
C        TEST 71 VERIFIES THE CONTENTS OF THE VARIABLE CVTN10 WHICH     03510204
C     WAS INITIALLY SET EQUAL TO BLANK.                                 03520204
C                                                                       03530204
      IVTNUM =  71                                                      03540204
      IF (ICZERO) 30710, 0710, 30710                                    03550204
 0710 CONTINUE                                                          03560204
      IVCOMP = 0                                                        03570204
      IVCORR = 1                                                        03580204
      IF (CVTN10 .EQ. ' ') IVCOMP = 1                                   03590204
40710 IF (IVCOMP - 1) 20710, 10710, 20710                               03600204
30710 IVDELE = IVDELE + 1                                               03610204
      WRITE (I02,80000) IVTNUM                                          03620204
      IF (ICZERO) 10710, 0721, 20710                                    03630204
10710 IVPASS = IVPASS + 1                                               03640204
      WRITE (I02,80002) IVTNUM                                          03650204
      GO TO 0721                                                        03660204
20710 IVFAIL = IVFAIL + 1                                               03670204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03680204
 0721 CONTINUE                                                          03690204
C                                                                       03700204
C     ****  FCVS PROGRAM 204  -  TEST 072  ****                         03710204
C                                                                       03720204
C        TEST 72 VERIFIES THE CONTENTS OF THE ARRAY CATN14 WHICH WAS    03730204
C     INITIALLY SET EQUAL TO ALL V'S.                                   03740204
C                                                                       03750204
      IVTNUM =  72                                                      03760204
      IF (ICZERO) 30720, 0720, 30720                                    03770204
 0720 CONTINUE                                                          03780204
      IVCOMP = 0                                                        03790204
      IVCORR = 6                                                        03800204
      DO 722, I= 1,6                                                    03810204
      IF (CATN14(I) .EQ. 'V') IVCOMP = IVCOMP + 1                       03820204
  722 CONTINUE                                                          03830204
40720 IF (IVCOMP - 6) 20720, 10720, 20720                               03840204
30720 IVDELE = IVDELE + 1                                               03850204
      WRITE (I02,80000) IVTNUM                                          03860204
      IF (ICZERO) 10720, 0731, 20720                                    03870204
10720 IVPASS = IVPASS + 1                                               03880204
      WRITE (I02,80002) IVTNUM                                          03890204
      GO TO 0731                                                        03900204
20720 IVFAIL = IVFAIL + 1                                               03910204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03920204
 0731 CONTINUE                                                          03930204
C                                                                       03940204
C     ****  FCVS PROGRAM 204  -  TEST 073  ****                         03950204
C                                                                       03960204
C        TEST 73 VERIFIES THE CONTENTS OF THE ARRAY IAON11 WHICH WAS    03970204
C     INITIALLY SET EQUAL TO ALL 7'S.                                   03980204
C                                                                       03990204
      IVTNUM =  73                                                      04000204
      IF (ICZERO) 30730, 0730, 30730                                    04010204
 0730 CONTINUE                                                          04020204
      IVCOMP = 0                                                        04030204
      IVCORR = 47                                                       04040204
      DO 732, I= 1,47                                                   04050204
      IF (IAON11(I) - 7) 732, 733, 732                                  04060204
  733 IVCOMP = IVCOMP + 1                                               04070204
  732 CONTINUE                                                          04080204
40730 IF (IVCOMP - 47) 20730, 10730, 20730                              04090204
30730 IVDELE = IVDELE + 1                                               04100204
      WRITE (I02,80000) IVTNUM                                          04110204
      IF (ICZERO) 10730, 0741, 20730                                    04120204
10730 IVPASS = IVPASS + 1                                               04130204
      WRITE (I02,80002) IVTNUM                                          04140204
      GO TO 0741                                                        04150204
20730 IVFAIL = IVFAIL + 1                                               04160204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04170204
 0741 CONTINUE                                                          04180204
C                                                                       04190204
C        TEST 74 THROUGH TEST 79 VERIFY THE COLLATING SEQUENCE          04200204
C     SPECIFICATIONS FOR THE FORTRAN SUBSET LANGUAGE.                   04210204
C                                                                       04220204
C        TEST 74 AND TEST 75 VERIFY THE COLLATING SEQUENCE FOR LETTERS. 04230204
C                                                                       04240204
C                                                                       04250204
C     ****  FCVS PROGRAM 204  -  TEST 074  ****                         04260204
C                                                                       04270204
C                                                                       04280204
      IVTNUM =  74                                                      04290204
      IF (ICZERO) 30740, 0740, 30740                                    04300204
 0740 CONTINUE                                                          04310204
      IVCOMP = 1                                                        04320204
      IVCORR = 210                                                      04330204
      IF ('A' .LT. 'B') IVCOMP = IVCOMP * 2                             04340204
      IF ('B' .LT. 'M') IVCOMP = IVCOMP * 3                             04350204
      IF ('M' .LT. 'V') IVCOMP = IVCOMP * 5                             04360204
      IF ('V' .LT. 'Z') IVCOMP = IVCOMP * 7                             04370204
40740 IF (IVCOMP - 210) 20740, 10740, 20740                             04380204
30740 IVDELE = IVDELE + 1                                               04390204
      WRITE (I02,80000) IVTNUM                                          04400204
      IF (ICZERO) 10740, 0751, 20740                                    04410204
10740 IVPASS = IVPASS + 1                                               04420204
      WRITE (I02,80002) IVTNUM                                          04430204
      GO TO 0751                                                        04440204
20740 IVFAIL = IVFAIL + 1                                               04450204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04460204
 0751 CONTINUE                                                          04470204
C                                                                       04480204
C     ****  FCVS PROGRAM 204  -  TEST 075  ****                         04490204
C                                                                       04500204
C                                                                       04510204
      IVTNUM =  75                                                      04520204
      IF (ICZERO) 30750, 0750, 30750                                    04530204
 0750 CONTINUE                                                          04540204
      IVCOMP = 0                                                        04550204
      IVCORR = 25                                                       04560204
      DO 752, I=1,25                                                    04570204
      J= I + 1                                                          04580204
      IF (CATN12(J) .GT. CATN12(I)) IVCOMP = IVCOMP + 1                 04590204
  752 CONTINUE                                                          04600204
40750 IF (IVCOMP - 25) 20750, 10750, 20750                              04610204
30750 IVDELE = IVDELE + 1                                               04620204
      WRITE (I02,80000) IVTNUM                                          04630204
      IF (ICZERO) 10750, 0761, 20750                                    04640204
10750 IVPASS = IVPASS + 1                                               04650204
      WRITE (I02,80002) IVTNUM                                          04660204
      GO TO 0761                                                        04670204
20750 IVFAIL = IVFAIL + 1                                               04680204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04690204
 0761 CONTINUE                                                          04700204
C                                                                       04710204
C        TEST 76 AND TEST 77 VERIFY THE COLLATING SEQUENCE FOR DIGITS.  04720204
C                                                                       04730204
C                                                                       04740204
C     ****  FCVS PROGRAM 204  -  TEST 076  ****                         04750204
C                                                                       04760204
C                                                                       04770204
      IVTNUM =  76                                                      04780204
      IF (ICZERO) 30760, 0760, 30760                                    04790204
 0760 CONTINUE                                                          04800204
      IVCOMP = 1                                                        04810204
      IVCORR = 30                                                       04820204
      IF ('0' .LT. '1') IVCOMP = IVCOMP * 2                             04830204
      IF ('1' .LT. '5') IVCOMP = IVCOMP * 3                             04840204
      IF ('5' .LT. '9') IVCOMP = IVCOMP * 5                             04850204
40760 IF (IVCOMP - 30) 20760, 10760, 20760                              04860204
30760 IVDELE = IVDELE + 1                                               04870204
      WRITE (I02,80000) IVTNUM                                          04880204
      IF (ICZERO) 10760, 0771, 20760                                    04890204
10760 IVPASS = IVPASS + 1                                               04900204
      WRITE (I02,80002) IVTNUM                                          04910204
      GO TO 0771                                                        04920204
20760 IVFAIL = IVFAIL + 1                                               04930204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04940204
 0771 CONTINUE                                                          04950204
C                                                                       04960204
C     ****  FCVS PROGRAM 204  -  TEST 077  ****                         04970204
C                                                                       04980204
C                                                                       04990204
      IVTNUM =  77                                                      05000204
      IF (ICZERO) 30770, 0770, 30770                                    05010204
 0770 CONTINUE                                                          05020204
      IVCOMP = 0                                                        05030204
      IVCORR = 9                                                        05040204
      DO 772, I=1,9                                                     05050204
      J = I + 1                                                         05060204
      IF (CATN13(I) .LT. CATN13(J)) IVCOMP = IVCOMP + 1                 05070204
  772 CONTINUE                                                          05080204
40770 IF (IVCOMP - 9) 20770, 10770, 20770                               05090204
30770 IVDELE = IVDELE + 1                                               05100204
      WRITE (I02,80000) IVTNUM                                          05110204
      IF (ICZERO) 10770, 0781, 20770                                    05120204
10770 IVPASS = IVPASS + 1                                               05130204
      WRITE (I02,80002) IVTNUM                                          05140204
      GO TO 0781                                                        05150204
20770 IVFAIL = IVFAIL + 1                                               05160204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05170204
 0781 CONTINUE                                                          05180204
C                                                                       05190204
C     ****  FCVS PROGRAM 204  -  TEST 078  ****                         05200204
C                                                                       05210204
C        TEST 78 VERIFIES THAT BLANK IS LESS THAN THE LETTER A AND BLANK05220204
C     IS LESS THAN THE DIGIT ZERO.                                      05230204
C                                                                       05240204
      IVTNUM =  78                                                      05250204
      IF (ICZERO) 30780, 0780, 30780                                    05260204
 0780 CONTINUE                                                          05270204
      IVCOMP = 1                                                        05280204
      IVCORR = 6                                                        05290204
      IF (' ' .LT. 'A') IVCOMP = IVCOMP * 2                             05300204
      IF (' ' .LT. '0') IVCOMP = IVCOMP * 3                             05310204
40780 IF (IVCOMP - 6) 20780, 10780, 20780                               05320204
30780 IVDELE = IVDELE + 1                                               05330204
      WRITE (I02,80000) IVTNUM                                          05340204
      IF (ICZERO) 10780, 0791, 20780                                    05350204
10780 IVPASS = IVPASS + 1                                               05360204
      WRITE (I02,80002) IVTNUM                                          05370204
      GO TO 0791                                                        05380204
20780 IVFAIL = IVFAIL + 1                                               05390204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05400204
 0791 CONTINUE                                                          05410204
C                                                                       05420204
C     ****  FCVS PROGRAM 204  -  TEST 079  ****                         05430204
C                                                                       05440204
C        TEST 79 VERIFIES THAT THE DIGITS AND LETTERS ARE NOT INTERMIXED05450204
C     IN THE COLLATING SEQUENCE.  EITHER ALL OF THE DIGITS MUST PRECEDE 05460204
C     A OR ALL OF THE DIGITS MUST FOLLOW Z.                             05470204
C                                                                       05480204
      IVTNUM =  79                                                      05490204
      IF (ICZERO) 30790, 0790, 30790                                    05500204
 0790 CONTINUE                                                          05510204
      IVCOMP = 0                                                        05520204
      IVCORR = 10                                                       05530204
      IF ('0' .NE. 'A') GO TO 792                                       05540204
      IVCOMP = 111                                                      05550204
      GO TO 40790                                                       05560204
  792 IF ('0' .GT.  'A') GO TO 793                                      05570204
C                                                                       05580204
C          ZERO IS LESS THAN LETTER A, SO ALL DIGITS MUST BE LESS THAN A05590204
C                                                                       05600204
      DO 794, I= 1,10                                                   05610204
      IF (CATN13(I) .LT. 'A') IVCOMP = IVCOMP + 1                       05620204
  794 CONTINUE                                                          05630204
      GO TO 40790                                                       05640204
C                                                                       05650204
C          ZERO IS GREATER THAN LETTER A, SO ALL DIGITS MUST BE GREATER 05660204
C     THAN LETTER Z.                                                    05670204
C                                                                       05680204
  793 DO 795 I=1,10                                                     05690204
      IF (CATN13(I) .GT. 'Z') IVCOMP = IVCOMP + 1                       05700204
  795 CONTINUE                                                          05710204
40790 IF (IVCOMP - 10) 20790,10790, 20790                               05720204
30790 IVDELE = IVDELE + 1                                               05730204
      WRITE (I02,80000) IVTNUM                                          05740204
      IF (ICZERO) 10790, 0801, 20790                                    05750204
10790 IVPASS = IVPASS + 1                                               05760204
      WRITE (I02,80002) IVTNUM                                          05770204
      GO TO 0801                                                        05780204
20790 IVFAIL = IVFAIL + 1                                               05790204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05800204
 0801 CONTINUE                                                          05810204
C                                                                       05820204
C        TEST 80 THROUGH TEST 85 PERFORM THE SAME COMPARISONS AS TEST 7405830204
C     THROUGH TEST 79 EXCEPT THAT THE ICHAR INTRINSIC FUNCTION IS USED  05840204
C     IN PLACE OF THE INDIVIDUAL CHARACTERS.                            05850204
C                                                                       05860204
C        TEST 80 AND TEST 81 VERIFY THE COLLATING SEQUENCE FOR LETTERS  05870204
C     USING THE ICHAR INTRINSIC FUNCTION.                               05880204
C                                                                       05890204
C                                                                       05900204
C     ****  FCVS PROGRAM 204  -  TEST 080  ****                         05910204
C                                                                       05920204
C                                                                       05930204
      IVTNUM =  80                                                      05940204
      IF (ICZERO) 30800, 0800, 30800                                    05950204
 0800 CONTINUE                                                          05960204
      IVCOMP = 1                                                        05970204
      IVCORR = 210                                                      05980204
      IVON01 = ICHAR('A')                                               05990204
      IVON02 = ICHAR('B')                                               06000204
      IVON03 = ICHAR('M')                                               06010204
      IVON04 = ICHAR('V')                                               06020204
      IVON05 = ICHAR('Z')                                               06030204
      IF (IVON01 .LT. IVON02) IVCOMP = IVCOMP * 2                       06040204
      IF (IVON02 .LT. IVON03) IVCOMP = IVCOMP * 3                       06050204
      IF (IVON03 .LT. IVON04) IVCOMP = IVCOMP * 5                       06060204
      IF (IVON04 .LT. IVON05) IVCOMP = IVCOMP * 7                       06070204
40800 IF (IVCOMP - 210) 20800, 10800, 20800                             06080204
30800 IVDELE = IVDELE + 1                                               06090204
      WRITE (I02,80000) IVTNUM                                          06100204
      IF (ICZERO) 10800, 0811, 20800                                    06110204
10800 IVPASS = IVPASS + 1                                               06120204
      WRITE (I02,80002) IVTNUM                                          06130204
      GO TO 0811                                                        06140204
20800 IVFAIL = IVFAIL + 1                                               06150204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06160204
 0811 CONTINUE                                                          06170204
C                                                                       06180204
C     ****  FCVS PROGRAM 204  -  TEST 081  ****                         06190204
C                                                                       06200204
C                                                                       06210204
      IVTNUM =  81                                                      06220204
      IF (ICZERO) 30810, 0810, 30810                                    06230204
 0810 CONTINUE                                                          06240204
      IVON01 = 0                                                        06250204
      IVON02 = 0                                                        06260204
      IVCOMP = 0                                                        06270204
      IVCORR = 25                                                       06280204
      DO 812, I=1,25                                                    06290204
      J= I + 1                                                          06300204
      IVON01 = ICHAR(CATN12(J))                                         06310204
      IVON02 = ICHAR(CATN12(I))                                         06320204
      IF (IVON01 .GT. IVON02) IVCOMP = IVCOMP + 1                       06330204
  812 CONTINUE                                                          06340204
40810 IF (IVCOMP - 25) 20810, 10810, 20810                              06350204
30810 IVDELE = IVDELE + 1                                               06360204
      WRITE (I02,80000) IVTNUM                                          06370204
      IF (ICZERO) 10810, 0821, 20810                                    06380204
10810 IVPASS = IVPASS + 1                                               06390204
      WRITE (I02,80002) IVTNUM                                          06400204
      GO TO 0821                                                        06410204
20810 IVFAIL = IVFAIL + 1                                               06420204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06430204
 0821 CONTINUE                                                          06440204
C                                                                       06450204
C        TEST 82 AND TEST 83 VERIFY THE COLLATING SEQUENCE FOR DIGITS   06460204
C     USING THE ICHAR INTRINSIC FUNCTION.                               06470204
C                                                                       06480204
C                                                                       06490204
C     ****  FCVS PROGRAM 204  -  TEST 082  ****                         06500204
C                                                                       06510204
C                                                                       06520204
      IVTNUM =  82                                                      06530204
      IF (ICZERO) 30820, 0820, 30820                                    06540204
 0820 CONTINUE                                                          06550204
      IVCOMP = 1                                                        06560204
      IVCORR = 30                                                       06570204
      IF (ICHAR('0') .LT. ICHAR('1')) IVCOMP = IVCOMP *2                06580204
      IF (ICHAR('1') .LT. ICHAR('5')) IVCOMP = IVCOMP * 3               06590204
      IF (ICHAR('5') .LT. ICHAR('9')) IVCOMP = IVCOMP * 5               06600204
40820 IF (IVCOMP - 30) 20820, 10820, 20820                              06610204
30820 IVDELE = IVDELE + 1                                               06620204
      WRITE (I02,80000) IVTNUM                                          06630204
      IF (ICZERO) 10820, 0831, 20820                                    06640204
10820 IVPASS = IVPASS + 1                                               06650204
      WRITE (I02,80002) IVTNUM                                          06660204
      GO TO 0831                                                        06670204
20820 IVFAIL = IVFAIL + 1                                               06680204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06690204
 0831 CONTINUE                                                          06700204
C                                                                       06710204
C     ****  FCVS PROGRAM 204  -  TEST 083  ****                         06720204
C                                                                       06730204
C                                                                       06740204
      IVTNUM =  83                                                      06750204
      IF (ICZERO) 30830, 0830, 30830                                    06760204
 0830 CONTINUE                                                          06770204
      IVON01 = 0                                                        06780204
      IVON02 = 0                                                        06790204
      IVCOMP = 0                                                        06800204
      IVCORR = 9                                                        06810204
      DO 832, I=1,9                                                     06820204
      J = I + 1                                                         06830204
      IVON01 = ICHAR(CATN13(J))                                         06840204
      IVON02 = ICHAR(CATN13(I))                                         06850204
      IF (IVON02 .LT. IVON01) IVCOMP = IVCOMP + 1                       06860204
  832 CONTINUE                                                          06870204
40830 IF (IVCOMP -9) 20830, 10830, 20830                                06880204
30830 IVDELE = IVDELE + 1                                               06890204
      WRITE (I02,80000) IVTNUM                                          06900204
      IF (ICZERO) 10830, 0841, 20830                                    06910204
10830 IVPASS = IVPASS + 1                                               06920204
      WRITE (I02,80002) IVTNUM                                          06930204
      GO TO 0841                                                        06940204
20830 IVFAIL = IVFAIL + 1                                               06950204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06960204
 0841 CONTINUE                                                          06970204
C                                                                       06980204
C     ****  FCVS PROGRAM 204  -  TEST 084  ****                         06990204
C                                                                       07000204
C        TEST 84 VERIFIES THAT BLANK IS LESS THAN THE LETTER A AND BLANK07010204
C     IS LESS THAN THE DIGIT ZERO.  THE INTRINSIC FUNCTION ICHAR IS     07020204
C     USED IN THIS TEST.                                                07030204
C                                                                       07040204
      IVTNUM =  84                                                      07050204
      IF (ICZERO) 30840, 0840, 30840                                    07060204
 0840 CONTINUE                                                          07070204
      IVCOMP = 1                                                        07080204
      IVCORR = 6                                                        07090204
      IF (ICHAR(' ') .LT. ICHAR('A')) IVCOMP = IVCOMP * 2               07100204
      IF (ICHAR(' ') .LT. ICHAR('0')) IVCOMP = IVCOMP * 3               07110204
40840 IF (IVCOMP - 6) 20840, 10840, 20840                               07120204
30840 IVDELE = IVDELE + 1                                               07130204
      WRITE (I02,80000) IVTNUM                                          07140204
      IF (ICZERO) 10840, 0851, 20840                                    07150204
10840 IVPASS = IVPASS + 1                                               07160204
      WRITE (I02,80002) IVTNUM                                          07170204
      GO TO 0851                                                        07180204
20840 IVFAIL = IVFAIL + 1                                               07190204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07200204
 0851 CONTINUE                                                          07210204
C                                                                       07220204
C     ****  FCVS PROGRAM 204  -  TEST 085  ****                         07230204
C                                                                       07240204
C        TEST 85 VERIFIES THAT THE DIGITS AND LETTERS ARE NOT INTERMIXED07250204
C     IN THE COLLATING SEQUENCE.  THE ICHAR INTRINSIC FUNCTION IS USED  07260204
C     TO VERIFY THAT EITHER ALL OF THE DIGITS PRECEDE A OR ALL OF THE   07270204
C     DIGITS FOLLOW Z.                                                  07280204
C                                                                       07290204
      IVTNUM =  85                                                      07300204
      IF (ICZERO) 30850, 0850, 30850                                    07310204
 0850 CONTINUE                                                          07320204
      IVCOMP = 0                                                        07330204
      IVCORR = 10                                                       07340204
      IF (ICHAR('0') .NE. ICHAR('A')) GO TO 852                         07350204
      IVCOMP = 111                                                      07360204
      GO TO 40850                                                       07370204
  852 IF (ICHAR('0') .GT. ICHAR('A')) GO TO 853                         07380204
C                                                                       07390204
C          ZERO IS LESS THAN LETTER A ACCORDING TO ICHAR INTRINSIC      07400204
C     FUNCTION VALUE.  THUS, THE ICHAR VALUE FOR ALL DIGITS MUST BE     07410204
C     LESS THAN ICHAR VALUE FOR LETTER A.                               07420204
C                                                                       07430204
      DO 854, I=1,10                                                    07440204
      IF (ICHAR(CATN13(I)) .LT. ICHAR('A')) IVCOMP = IVCOMP + 1         07450204
  854 CONTINUE                                                          07460204
      GO TO 40850                                                       07470204
C                                                                       07480204
C          ZERO IS GREATER THAN LETTER A ACCORDING TO ICHAR INTRINSIC   07490204
C     FUNCTION VALUE.  THUS, THE ICHAR VALUE FOR ALL DIGITS MUST BE     07500204
C     GREATER THAN ICHAR VALUE FOR LETTER Z.                            07510204
C                                                                       07520204
  853 DO 855, I=1,10                                                    07530204
      IF (ICHAR(CATN13(I)).GT. ICHAR('Z')) IVCOMP = IVCOMP + 1          07540204
  855 CONTINUE                                                          07550204
40850 IF (IVCOMP - 10) 20850, 10850, 20850                              07560204
30850 IVDELE = IVDELE + 1                                               07570204
      WRITE (I02,80000) IVTNUM                                          07580204
      IF (ICZERO) 10850, 0861, 20850                                    07590204
10850 IVPASS = IVPASS + 1                                               07600204
      WRITE (I02,80002) IVTNUM                                          07610204
      GO TO 0861                                                        07620204
20850 IVFAIL = IVFAIL + 1                                               07630204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07640204
 0861 CONTINUE                                                          07650204
C                                                                       07660204
C     ****  FCVS PROGRAM 204  -  TEST 086  ****                         07670204
C                                                                       07680204
C          THE ARRAY IAON11 IS SET EQUAL TO THE ICHAR INTRINSIC FUNCTION07690204
C     VALUE OF THE CORRESPONDING ELEMENT IN THE CATN11 ARRAY.  THE      07700204
C     IAON11 ARRAY IS THEN SORTED IN ASCENDING ORDER, AND ENTRIES IN    07710204
C     THE CATN11 ARRAY ARE ARRANGED ACCORDING TO THE ASCENDING SORT     07720204
C     ORDER IN IAON11.  THE RESULTING ORDER OF THE CATN11 ARRAY GIVES   07730204
C     THE PROCESSOR'S COLLATING SEQUENCE FOR THE FORTRAN SUBSET LANGUAGE07740204
C     CHARACTER SET.  THE CATN11 ARRAY IS PRINTED AND MUST BE VISUALLY  07750204
C     CHECKED TO DETERMINE IF THE COLLATING SEQUENCE RULES ARE FOLLOWED 07760204
C     BY THE COMPILER.                                                  07770204
C                                                                       07780204
      IVTNUM =  86                                                      07790204
      IF (ICZERO) 30860, 0860, 30860                                    07800204
 0860 CONTINUE                                                          07810204
      IVCOMP = 0                                                        07820204
C                                                                       07830204
C          INITIALIZE IAON11 TO ZERO.                                   07840204
      DO 862 I=1,47                                                     07850204
      IAON11(I) = 0                                                     07860204
  862 CONTINUE                                                          07870204
C                                                                       07880204
C          PLACE ICHAR INTRINSIC VALUE IN IAON11.                       07890204
C                                                                       07900204
      DO 863, I= 1,47                                                   07910204
      IAON11(I) = ICHAR(CATN11(I))                                      07920204
  863 CONTINUE                                                          07930204
C                                                                       07940204
C          SORT FORTRAN CHARACTERS ACCORDING TO THEIR POSITION IN THE   07950204
C     COLLATING SEQUENCE.                                               07960204
C                                                                       07970204
      DO 864, I=1,46                                                    07980204
      J=I                                                               07990204
      N = I + 1                                                         08000204
      DO 865 K = N,47                                                   08010204
      IF (IAON11(J) .LT. IAON11(K)) GO TO 865                           08020204
      J=K                                                               08030204
  865 CONTINUE                                                          08040204
      IVON01 = IAON11(J)                                                08050204
      IAON11(J)= IAON11(I)                                              08060204
      IAON11(I)= IVON01                                                 08070204
      CVTN01 = CATN11(J)                                                08080204
      CATN11(J) = CATN11(I)                                             08090204
      CATN11(I) = CVTN01                                                08100204
  864 CONTINUE                                                          08110204
      WRITE (I02, 866) CATN11                                           08120204
      WRITE (I02, 867) IAON11                                           08130204
  866 FORMAT (3X,'FORTRAN CHARACTER SET IN ASCENDING ORDER',3X,/        08140204
     1  3X, 'VISUAL VERIFICATION REQUIRED'     //,3X, 12(A1,3X)/        08150204
     2  3X, 12(A1,3X)/ 3X, 12(A1,3X)/ 3X, 11(A1,3X))                    08160204
  867 FORMAT ( 3X/3X, 'ICHAR INTRINSIC FUNCTION VALUES FOR FORTRAN ',   08170204
     1   'CHARACTER SET'// 3X, 12I4/ 3X, 12I4/ 3X, 12I4/                08180204
     2     3X,11I4//)                                                   08190204
      IVCOMP = 1                                                        08200204
      IVCORR = 1                                                        08210204
40860 IF (IVCOMP - 1) 20860, 10860, 20860                               08220204
30860 IVDELE = IVDELE + 1                                               08230204
      WRITE (I02,80000) IVTNUM                                          08240204
      IF (ICZERO) 10860, 0871, 20860                                    08250204
10860 IVPASS = IVPASS + 1                                               08260204
      WRITE (I02,80002) IVTNUM                                          08270204
      GO TO 0871                                                        08280204
20860 IVFAIL = IVFAIL + 1                                               08290204
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08300204
 0871 CONTINUE                                                          08310204
C                                                                       08320204
C                                                                       08330204
C     WRITE OUT TEST SUMMARY                                            08340204
C                                                                       08350204
      WRITE (I02,90004)                                                 08360204
      WRITE (I02,90014)                                                 08370204
      WRITE (I02,90004)                                                 08380204
      WRITE (I02,90000)                                                 08390204
      WRITE (I02,90004)                                                 08400204
      WRITE (I02,90020) IVFAIL                                          08410204
      WRITE (I02,90022) IVPASS                                          08420204
      WRITE (I02,90024) IVDELE                                          08430204
      STOP                                                              08440204
90001 FORMAT (1H ,24X,5HFM204)                                          08450204
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM204)                          08460204
C                                                                       08470204
C     FORMATS FOR TEST DETAIL LINES                                     08480204
C                                                                       08490204
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   08500204
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08510204
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08520204
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08530204
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        08540204
C                                                                       08550204
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08560204
C                                                                       08570204
90002 FORMAT (1H1)                                                      08580204
90004 FORMAT (1H )                                                      08590204
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08600204
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   08610204
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         08620204
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  08630204
90014 FORMAT (1H ,5X,46H----------------------------------------------) 08640204
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08650204
C                                                                       08660204
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 08670204
C                                                                       08680204
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              08690204
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              08700204
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             08710204
      END                                                               08720204
