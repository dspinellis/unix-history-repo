C     COMMENT SECTION                                                   00010097
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020097
C     FM097                                                             00030097
C                                                                       00040097
C     THIS ROUTINE TESTS INTRINSIC FUNCTIONS WHERE THE FUNCTION TYPE IS 00050097
C     REAL AND THE ARGUMENTS ARE EITHER INTEGER OR REAL.  THE REAL AND  00060097
C     INTEGER VARIABLES AND THE REAL AND INTEGER CONSTANTS CONTAIN BOTH 00070097
C     POSITIVE AND NEGATIVE VALUES.  THE INTRINSIC FUNCTIONS TESTED BY  00080097
C     FM097 INCLUDE                                                     00090097
C                                                   TYPE OF             00100097
C       INTRINSIC FUNCTION          NAME       ARGUMENT     FUNCTION    00110097
C       ------------------          ----       --------     --------    00120097
C         ABSOLUTE VALUE            ABS        REAL         REAL        00130097
C         TRUNCATION                AINT       REAL         REAL        00140097
C         REMAINDERING              AMOD       REAL         REAL        00150097
C         CHOOSING LARGEST VALUE    AMAX0      INTEGER      REAL        00160097
C                                   AMAX1      REAL         REAL        00170097
C         CHOOSING SMALLEST VALUE   AMIN0     INTEGER       REAL        00180097
C                                   AMIN1      REAL         REAL        00190097
C         FLOAT                     FLOAT      INTEGER      REAL        00200097
C         TRANSFER OF SIGN          SIGN       REAL         REAL        00210097
C         POSITIVE DIFFERENCE       DIM        REAL         REAL        00220097
C                                                                       00230097
C      REFERENCES                                                       00240097
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00250097
C              X3.9-1978                                                00260097
C                                                                       00270097
C        SECTION 4.1.2, TYPE RULES FOR DATA AND PROCEDURE IDENTIFIERS   00280097
C        SECTION 15.3, INTRINSIC FUNCTION                               00290097
C        SECTION 15.3.2, INTRINSIC FUNCTIONS AND THEIR REFERENCE        00300097
C                                                                       00310097
C                                                                       00320097
C      **********************************************************       00330097
C                                                                       00340097
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00350097
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00360097
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00370097
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00380097
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00390097
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00400097
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00410097
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00420097
C     OF EXECUTING THESE TESTS.                                         00430097
C                                                                       00440097
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00450097
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00460097
C                                                                       00470097
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00480097
C                                                                       00490097
C                  DEPARTMENT OF THE NAVY                               00500097
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00510097
C                  WASHINGTON, D.C.  20376                              00520097
C                                                                       00530097
C      **********************************************************       00540097
C                                                                       00550097
C                                                                       00560097
C                                                                       00570097
C     INITIALIZATION SECTION                                            00580097
C                                                                       00590097
C     INITIALIZE CONSTANTS                                              00600097
C      **************                                                   00610097
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620097
      I01 = 5                                                           00630097
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640097
      I02 = 6                                                           00650097
C     SYSTEM ENVIRONMENT SECTION                                        00660097
C                                                                       00670097
      I01 = 5                                                           00680097
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690097
C     (UNIT NUMBER FOR CARD READER).                                    00700097
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00710097
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00720097
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00730097
C                                                                       00740097
      I02 = 6                                                           00750097
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00760097
C     (UNIT NUMBER FOR PRINTER).                                        00770097
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00780097
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00790097
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00800097
C                                                                       00810097
      IVPASS=0                                                          00820097
      IVFAIL=0                                                          00830097
      IVDELE=0                                                          00840097
      ICZERO=0                                                          00850097
C                                                                       00860097
C     WRITE PAGE HEADERS                                                00870097
      WRITE (I02,90000)                                                 00880097
      WRITE (I02,90001)                                                 00890097
      WRITE (I02,90002)                                                 00900097
      WRITE (I02, 90002)                                                00910097
      WRITE (I02,90003)                                                 00920097
      WRITE (I02,90002)                                                 00930097
      WRITE (I02,90004)                                                 00940097
      WRITE (I02,90002)                                                 00950097
      WRITE (I02,90011)                                                 00960097
      WRITE (I02,90002)                                                 00970097
      WRITE (I02,90002)                                                 00980097
      WRITE (I02,90005)                                                 00990097
      WRITE (I02,90006)                                                 01000097
      WRITE (I02,90002)                                                 01010097
C                                                                       01020097
C     TEST SECTION                                                      01030097
C                                                                       01040097
C     TEST 875 THROUGH TEST 878 CONTAIN INTRINSIC FUNCTION TESTS FOR    01050097
C     ABSOLUTE VALUE WHERE ARGUMENT AND FUNCTION ARE REAL               01060097
C                                                                       01070097
      IVTNUM = 875                                                      01080097
C                                                                       01090097
C      ****  TEST 875  ****                                             01100097
C                                                                       01110097
      IF (ICZERO) 38750, 8750, 38750                                    01120097
 8750 CONTINUE                                                          01130097
      RVCOMP = ABS (-38.2)                                              01140097
      GO TO 48750                                                       01150097
38750 IVDELE = IVDELE + 1                                               01160097
      WRITE (I02,80003) IVTNUM                                          01170097
      IF (ICZERO) 48750, 8761, 48750                                    01180097
48750 IF (RVCOMP - 38.195) 28750,18750,48751                            01190097
48751 IF (RVCOMP - 38.205) 18750,18750,28750                            01200097
18750 IVPASS = IVPASS + 1                                               01210097
      WRITE (I02,80001) IVTNUM                                          01220097
      GO TO 8761                                                        01230097
28750 IVFAIL = IVFAIL + 1                                               01240097
      RVCORR = 38.200                                                   01250097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01260097
 8761 CONTINUE                                                          01270097
      IVTNUM = 876                                                      01280097
C                                                                       01290097
C      ****  TEST 876  ****                                             01300097
C                                                                       01310097
      IF (ICZERO) 38760, 8760, 38760                                    01320097
 8760 CONTINUE                                                          01330097
      RVON01 = 445.06                                                   01340097
      RVCOMP = ABS (RVON01)                                             01350097
      GO TO 48760                                                       01360097
38760 IVDELE = IVDELE + 1                                               01370097
      WRITE (I02,80003) IVTNUM                                          01380097
      IF (ICZERO) 48760, 8771, 48760                                    01390097
48760 IF (RVCOMP - 445.01) 28760,18760,48761                            01400097
48761 IF (RVCOMP - 445.11) 18760,18760,28760                            01410097
18760 IVPASS = IVPASS + 1                                               01420097
      WRITE (I02,80001) IVTNUM                                          01430097
      GO TO 8771                                                        01440097
28760 IVFAIL = IVFAIL + 1                                               01450097
      RVCORR = 445.06                                                   01460097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01470097
 8771 CONTINUE                                                          01480097
      IVTNUM = 877                                                      01490097
C                                                                       01500097
C      ****  TEST 877  ****                                             01510097
C                                                                       01520097
      IF (ICZERO) 38770, 8770, 38770                                    01530097
 8770 CONTINUE                                                          01540097
      RVON01 = -32.176                                                  01550097
      RVCOMP = ABS (RVON01)                                             01560097
      GO TO 48770                                                       01570097
38770 IVDELE = IVDELE + 1                                               01580097
      WRITE (I02,80003) IVTNUM                                          01590097
      IF (ICZERO) 48770, 8781, 48770                                    01600097
48770 IF (RVCOMP - 32.171) 28770,18770,48771                            01610097
48771 IF (RVCOMP - 32.181) 18770,18770,28770                            01620097
18770 IVPASS = IVPASS + 1                                               01630097
      WRITE (I02,80001) IVTNUM                                          01640097
      GO TO 8781                                                        01650097
28770 IVFAIL = IVFAIL + 1                                               01660097
      RVCORR = 32.176                                                   01670097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01680097
 8781 CONTINUE                                                          01690097
      IVTNUM = 878                                                      01700097
C                                                                       01710097
C      ****  TEST 878  ****                                             01720097
C                                                                       01730097
      IF (ICZERO) 38780, 8780, 38780                                    01740097
 8780 CONTINUE                                                          01750097
      RVON01 = -2.2E+2                                                  01760097
      RVCOMP = ABS (RVON01)                                             01770097
      GO TO 48780                                                       01780097
38780 IVDELE = IVDELE + 1                                               01790097
      WRITE (I02,80003) IVTNUM                                          01800097
      IF (ICZERO) 48780, 8791, 48780                                    01810097
48780 IF (RVCOMP - 219.95) 28780,18780,48781                            01820097
48781 IF (RVCOMP - 220.05) 18780,18780,28780                            01830097
18780 IVPASS = IVPASS + 1                                               01840097
      WRITE (I02,80001) IVTNUM                                          01850097
      GO TO 8791                                                        01860097
28780 IVFAIL = IVFAIL + 1                                               01870097
      RVCORR = 220.00                                                   01880097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          01890097
 8791 CONTINUE                                                          01900097
      IVTNUM = 879                                                      01910097
C                                                                       01920097
C      ****  TEST 879  ****                                             01930097
C                                                                       01940097
C     TEST 879 THROUGH TEST 882 CONTAIN INTRINSIC FUNCTION TESTS FOR    01950097
C     TRUNCATION WHERE ARGUMENT AND FUNCTION ARE REAL                   01960097
C                                                                       01970097
C                                                                       01980097
      IF (ICZERO) 38790, 8790, 38790                                    01990097
 8790 CONTINUE                                                          02000097
      RVCOMP = AINT (38.2)                                              02010097
      GO TO 48790                                                       02020097
38790 IVDELE = IVDELE + 1                                               02030097
      WRITE (I02,80003) IVTNUM                                          02040097
      IF (ICZERO) 48790, 8801, 48790                                    02050097
48790 IF (RVCOMP - 37.995) 28790,18790,48791                            02060097
48791 IF (RVCOMP - 38.005) 18790,18790,28790                            02070097
18790 IVPASS = IVPASS + 1                                               02080097
      WRITE (I02,80001) IVTNUM                                          02090097
      GO TO 8801                                                        02100097
28790 IVFAIL = IVFAIL + 1                                               02110097
      RVCORR = 38.000                                                   02120097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02130097
 8801 CONTINUE                                                          02140097
      IVTNUM = 880                                                      02150097
C                                                                       02160097
C      ****  TEST 880  ****                                             02170097
C                                                                       02180097
      IF (ICZERO) 38800, 8800, 38800                                    02190097
 8800 CONTINUE                                                          02200097
      RVON01 = -445.95                                                  02210097
      RVCOMP = AINT (RVON01)                                            02220097
      GO TO 48800                                                       02230097
38800 IVDELE = IVDELE + 1                                               02240097
      WRITE (I02,80003) IVTNUM                                          02250097
      IF (ICZERO) 48800, 8811, 48800                                    02260097
48800 IF (RVCOMP + 445.05) 28800,18800,48801                            02270097
48801 IF (RVCOMP + 444.95) 18800,18800,28800                            02280097
18800 IVPASS = IVPASS + 1                                               02290097
      WRITE (I02,80001) IVTNUM                                          02300097
      GO TO 8811                                                        02310097
28800 IVFAIL = IVFAIL + 1                                               02320097
      RVCORR = -445.00                                                  02330097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02340097
 8811 CONTINUE                                                          02350097
      IVTNUM = 881                                                      02360097
C                                                                       02370097
C      ****  TEST 881  ****                                             02380097
C                                                                       02390097
      IF (ICZERO) 38810, 8810, 38810                                    02400097
 8810 CONTINUE                                                          02410097
      RVON01 = 466.01                                                   02420097
      RVCOMP = AINT (RVON01)                                            02430097
      GO TO 48810                                                       02440097
38810 IVDELE = IVDELE + 1                                               02450097
      WRITE (I02,80003) IVTNUM                                          02460097
      IF (ICZERO) 48810, 8821, 48810                                    02470097
48810 IF (RVCOMP - 465.95) 28810,18810,48811                            02480097
48811 IF (RVCOMP - 466.05) 18810,18810,28810                            02490097
18810 IVPASS = IVPASS + 1                                               02500097
      WRITE (I02,80001) IVTNUM                                          02510097
      GO TO 8821                                                        02520097
28810 IVFAIL = IVFAIL + 1                                               02530097
      RVCOMP = 466.00                                                   02540097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02550097
 8821 CONTINUE                                                          02560097
      IVTNUM = 882                                                      02570097
C                                                                       02580097
C      ****  TEST 882  ****                                             02590097
C                                                                       02600097
      IF (ICZERO) 38820, 8820, 38820                                    02610097
 8820 CONTINUE                                                          02620097
      RVON01 = 382E-1                                                   02630097
      RVCOMP = AINT (RVON01)                                            02640097
      GO TO 48820                                                       02650097
38820 IVDELE = IVDELE + 1                                               02660097
      WRITE (I02,80003) IVTNUM                                          02670097
      IF (ICZERO) 48820, 8831, 48820                                    02680097
48820 IF (RVCOMP - 37.995) 28820,18820,48821                            02690097
48821 IF (RVCOMP - 38.005) 18820,18820,28820                            02700097
18820 IVPASS = IVPASS + 1                                               02710097
      WRITE (I02,80001) IVTNUM                                          02720097
      GO TO 8831                                                        02730097
28820 IVFAIL = IVFAIL + 1                                               02740097
      RVCORR = 38.000                                                   02750097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02760097
 8831 CONTINUE                                                          02770097
C                                                                       02780097
C     TEST 883 THROUGH 886 CONTAIN INTRINSIC FUNCTION TESTS FOR         02790097
C     REMAINDERING WHERE ARGUMENT AND FUNCTION ARE REAL                 02800097
C                                                                       02810097
      IVTNUM = 883                                                      02820097
C                                                                       02830097
C      ****  TEST 883  ****                                             02840097
C                                                                       02850097
      IF (ICZERO) 38830, 8830, 38830                                    02860097
 8830 CONTINUE                                                          02870097
      RVCOMP = AMOD (42.0,19.0)                                         02880097
      GO TO 48830                                                       02890097
38830 IVDELE = IVDELE + 1                                               02900097
      WRITE (I02,80003) IVTNUM                                          02910097
      IF (ICZERO) 48830, 8841, 48830                                    02920097
48830 IF (RVCOMP - 3.9995) 28830,18830,48831                            02930097
48831 IF (RVCOMP - 4.0005) 18830,18830,28830                            02940097
18830 IVPASS = IVPASS + 1                                               02950097
      WRITE (I02,80001) IVTNUM                                          02960097
      GO TO 8841                                                        02970097
28830 IVFAIL = IVFAIL + 1                                               02980097
      RVCORR = 4.0000                                                   02990097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03000097
 8841 CONTINUE                                                          03010097
      IVTNUM = 884                                                      03020097
C                                                                       03030097
C      ****  TEST 884  ****                                             03040097
C                                                                       03050097
      IF (ICZERO) 38840, 8840, 38840                                    03060097
 8840 CONTINUE                                                          03070097
      RVON01 = 16.27                                                    03080097
      RVON02 = 2.0                                                      03090097
      RVCOMP = AMOD (RVON01,RVON02)                                     03100097
      GO TO 48840                                                       03110097
38840 IVDELE = IVDELE + 1                                               03120097
      WRITE (I02,80003) IVTNUM                                          03130097
      IF (ICZERO) 48840, 8851, 48840                                    03140097
48840 IF (RVCOMP - .26995) 28840,18840,48841                            03150097
48841 IF (RVCOMP - .27005) 18840,18840,28840                            03160097
18840 IVPASS = IVPASS + 1                                               03170097
      WRITE (I02,80001) IVTNUM                                          03180097
      GO TO 8851                                                        03190097
28840 IVFAIL = IVFAIL + 1                                               03200097
      RVCORR = .27000                                                   03210097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03220097
 8851 CONTINUE                                                          03230097
      IVTNUM = 885                                                      03240097
C                                                                       03250097
C      ****  TEST 885  ****                                             03260097
C                                                                       03270097
      IF (ICZERO) 38850, 8850, 38850                                    03280097
 8850 CONTINUE                                                          03290097
      RVON01 = 225.0                                                    03300097
      RVON02 = 5.0E1                                                    03310097
      RVCOMP = AMOD (RVON01,RVON02)                                     03320097
      GO TO 48850                                                       03330097
38850 IVDELE = IVDELE + 1                                               03340097
      WRITE (I02,80003) IVTNUM                                          03350097
      IF (ICZERO) 48850, 8861, 48850                                    03360097
48850 IF (RVCOMP - 24.995) 28850,18850,48851                            03370097
48851 IF (RVCOMP - 25.005) 18850,18850,28850                            03380097
18850 IVPASS = IVPASS + 1                                               03390097
      WRITE (I02,80001) IVTNUM                                          03400097
      GO TO 8861                                                        03410097
28850 IVFAIL = IVFAIL + 1                                               03420097
      RVCORR = 25.000                                                   03430097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03440097
 8861 CONTINUE                                                          03450097
      IVTNUM = 886                                                      03460097
C                                                                       03470097
C      ****  TEST 886  ****                                             03480097
C                                                                       03490097
      IF (ICZERO) 38860, 8860, 38860                                    03500097
 8860 CONTINUE                                                          03510097
      RVON01 = -0.390E+2                                                03520097
      RVON02 = 5E2                                                      03530097
      RVCOMP = AMOD (RVON01,RVON02)                                     03540097
      GO TO 48860                                                       03550097
38860 IVDELE = IVDELE + 1                                               03560097
      WRITE (I02,80003) IVTNUM                                          03570097
      IF (ICZERO) 48860, 8871, 48860                                    03580097
48860 IF (RVCOMP + 39.005) 28860,18860,48861                            03590097
48861 IF (RVCOMP + 38.995) 18860,18860,28860                            03600097
18860 IVPASS = IVPASS + 1                                               03610097
      WRITE (I02,80001) IVTNUM                                          03620097
      GO TO 8871                                                        03630097
28860 IVFAIL = IVFAIL + 1                                               03640097
      RVCORR = -39.000                                                  03650097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03660097
 8871 CONTINUE                                                          03670097
C                                                                       03680097
C     TEST 887 AND 888 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    03690097
C     LARGEST VALUE WHERE ARGUMENTS ARE INTEGER AND FUNCTION IS REAL    03700097
C                                                                       03710097
      IVTNUM = 887                                                      03720097
C                                                                       03730097
C      ****  TEST 887  ****                                             03740097
C                                                                       03750097
      IF (ICZERO) 38870, 8870, 38870                                    03760097
 8870 CONTINUE                                                          03770097
      IVON01 = 317                                                      03780097
      IVON02 = -99                                                      03790097
      IVON03 = 1                                                        03800097
      RVCOMP = AMAX0 (263,IVON01,IVON02,IVON03)                         03810097
      GO TO 48870                                                       03820097
38870 IVDELE = IVDELE + 1                                               03830097
      WRITE (I02,80003) IVTNUM                                          03840097
      IF (ICZERO) 48870, 8881, 48870                                    03850097
48870 IF (RVCOMP - 316.95) 28870,18870,48871                            03860097
48871 IF (RVCOMP - 317.05) 18870,18870,28870                            03870097
18870 IVPASS = IVPASS + 1                                               03880097
      WRITE (I02,80001) IVTNUM                                          03890097
      GO TO 8881                                                        03900097
28870 IVFAIL = IVFAIL + 1                                               03910097
      RVCORR = 317.00                                                   03920097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03930097
 8881 CONTINUE                                                          03940097
      IVTNUM = 888                                                      03950097
C                                                                       03960097
C      ****  TEST 888  ****                                             03970097
C                                                                       03980097
      IF (ICZERO) 38880, 8880, 38880                                    03990097
 8880 CONTINUE                                                          04000097
      IVON01 = 2572                                                     04010097
      IVON02 = 2570                                                     04020097
      RVCOMP = AMAX0 (IVON01,IVON02)                                    04030097
      GO TO 48880                                                       04040097
38880 IVDELE = IVDELE + 1                                               04050097
      WRITE (I02,80003) IVTNUM                                          04060097
      IF (ICZERO) 48880, 8891, 48880                                    04070097
48880 IF (RVCOMP - 2571.5) 28880,18880,48881                            04080097
48881 IF (RVCOMP - 2572.5) 18880,18880,28880                            04090097
18880 IVPASS = IVPASS + 1                                               04100097
      WRITE (I02,80001) IVTNUM                                          04110097
      GO TO 8891                                                        04120097
28880 IVFAIL = IVFAIL + 1                                               04130097
      RVCORR = 2572.0                                                   04140097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04150097
 8891 CONTINUE                                                          04160097
C                                                                       04170097
C     TEST 889 AND 890 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    04180097
C     LARGEST VALUE WHERE THE ARGUMENTS AND FUNCTION ARE REAL           04190097
C                                                                       04200097
      IVTNUM = 889                                                      04210097
C                                                                       04220097
C      ****  TEST 889  ****                                             04230097
C                                                                       04240097
      IF (ICZERO) 38890, 8890, 38890                                    04250097
 8890 CONTINUE                                                          04260097
      RVON01 = .326E+2                                                  04270097
      RVON02 = 22.075                                                   04280097
      RVON03 = 76E-1                                                    04290097
      RVCOMP = AMAX1 (RVON01,RVON02,RVON03)                             04300097
      GO TO 48890                                                       04310097
38890 IVDELE = IVDELE + 1                                               04320097
      WRITE (I02,80003) IVTNUM                                          04330097
      IF (ICZERO) 48890, 8901, 48890                                    04340097
48890 IF (RVCOMP - 32.595) 28890,18890,48891                            04350097
48891 IF (RVCOMP - 32.605) 18890,18890,28890                            04360097
18890 IVPASS = IVPASS + 1                                               04370097
      WRITE (I02,80001) IVTNUM                                          04380097
      GO TO 8901                                                        04390097
28890 IVFAIL = IVFAIL + 1                                               04400097
      RVCORR = 32.600                                                   04410097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04420097
 8901 CONTINUE                                                          04430097
      IVTNUM = 890                                                      04440097
C                                                                       04450097
C      ****  TEST 890  ****                                             04460097
C                                                                       04470097
      IF (ICZERO) 38900, 8900, 38900                                    04480097
 8900 CONTINUE                                                          04490097
      RVON01 = -6.3E2                                                   04500097
      RVON02 = -21.0                                                    04510097
      RVCOMP = AMAX1 (-463.3,RVON01,RVON02)                             04520097
      GO TO 48900                                                       04530097
38900 IVDELE = IVDELE + 1                                               04540097
      WRITE (I02,80003) IVTNUM                                          04550097
      IF (ICZERO) 48900, 8911, 48900                                    04560097
48900 IF (RVCOMP + 21.005) 28900,18900,48901                            04570097
48901 IF (RVCOMP + 20.995) 18900,18900,28900                            04580097
18900 IVPASS = IVPASS + 1                                               04590097
      WRITE (I02,80001) IVTNUM                                          04600097
      GO TO 8911                                                        04610097
28900 IVFAIL = IVFAIL + 1                                               04620097
      RVCORR = -21.000                                                  04630097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04640097
 8911 CONTINUE                                                          04650097
C                                                                       04660097
C     TESTS 891 AND 892 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING   04670097
C     SMALLEST VALUE WHERE ARGUMENTS ARE INTEGER AND FUNCTION IS REAL   04680097
C                                                                       04690097
      IVTNUM = 891                                                      04700097
C                                                                       04710097
C      ****  TEST 891  ****                                             04720097
C                                                                       04730097
      IF (ICZERO) 38910, 8910, 38910                                    04740097
 8910 CONTINUE                                                          04750097
      IVON01 = -75                                                      04760097
      IVON02 = -243                                                     04770097
      RVCOMP = AMIN0 (IVON01,IVON02)                                    04780097
      GO TO 48910                                                       04790097
38910 IVDELE = IVDELE + 1                                               04800097
      WRITE (I02,80003) IVTNUM                                          04810097
      IF (ICZERO) 48910, 8921, 48910                                    04820097
48910 IF (RVCOMP + 243.05) 28910,18910,48911                            04830097
48911 IF (RVCOMP + 242.95) 18910,18910,28910                            04840097
18910 IVPASS = IVPASS + 1                                               04850097
      WRITE (I02,80001) IVTNUM                                          04860097
      GO TO 8921                                                        04870097
28910 IVFAIL = IVFAIL + 1                                               04880097
      RVCORR = -243.00                                                  04890097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04900097
 8921 CONTINUE                                                          04910097
      IVTNUM = 892                                                      04920097
C                                                                       04930097
C      ****  TEST 892  ****                                             04940097
C                                                                       04950097
      IF (ICZERO) 38920, 8920, 38920                                    04960097
 8920 CONTINUE                                                          04970097
      IVON01 = -11                                                      04980097
      IVON02 = 11                                                       04990097
      RVCOMP = AMIN0 (0,IVON01,IVON02)                                  05000097
      GO TO 48920                                                       05010097
38920 IVDELE = IVDELE + 1                                               05020097
      WRITE (I02,80003) IVTNUM                                          05030097
      IF (ICZERO) 48920, 8931, 48920                                    05040097
48920 IF (RVCOMP + 11.005) 28920,18920,48921                            05050097
48921 IF (RVCOMP + 10.995) 18920,18920,28920                            05060097
18920 IVPASS = IVPASS + 1                                               05070097
      WRITE (I02,80001) IVTNUM                                          05080097
      GO TO 8931                                                        05090097
28920 IVFAIL = IVFAIL + 1                                               05100097
      RVCORR = -11.000                                                  05110097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05120097
 8931 CONTINUE                                                          05130097
C                                                                       05140097
C     TESTS 893 AND 894 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING   05150097
C     SMALLEST VALUE WHERE ARGUMENTS AND FUNCTION ARE REAL              05160097
C                                                                       05170097
      IVTNUM = 893                                                      05180097
C                                                                       05190097
C      ****  TEST 893  ****                                             05200097
C                                                                       05210097
      IF (ICZERO) 38930, 8930, 38930                                    05220097
 8930 CONTINUE                                                          05230097
      RVON01 = 1.1111                                                   05240097
      RVON02 = 22.222                                                   05250097
      RVON03 = 333.33                                                   05260097
      RVCOMP = AMIN1 (RVON01,RVON02,RVON03)                             05270097
      GO TO 48930                                                       05280097
38930 IVDELE = IVDELE + 1                                               05290097
      WRITE (I02,80003) IVTNUM                                          05300097
      IF (ICZERO) 48930, 8941, 48930                                    05310097
48930 IF (RVCOMP - 1.1106) 28930,18930,48931                            05320097
48931 IF (RVCOMP - 1.1116) 18930,18930,28930                            05330097
18930 IVPASS = IVPASS + 1                                               05340097
      WRITE (I02,80001) IVTNUM                                          05350097
      GO TO 8941                                                        05360097
28930 IVFAIL = IVFAIL + 1                                               05370097
      RVCORR = 1.1111                                                   05380097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05390097
 8941 CONTINUE                                                          05400097
      IVTNUM = 894                                                      05410097
C                                                                       05420097
C      ****  TEST 894  ****                                             05430097
C                                                                       05440097
      IF (ICZERO) 38940, 8940, 38940                                    05450097
 8940 CONTINUE                                                          05460097
      RVON01 = 28.8                                                     05470097
      RVON02 = 2.88E1                                                   05480097
      RVON03 = 288E-1                                                   05490097
      RVON04 = 35.0                                                     05500097
      RVCOMP = AMIN1 (RVON01,RVON02,RVON03,RVON04)                      05510097
      GO TO 48940                                                       05520097
38940 IVDELE = IVDELE + 1                                               05530097
      WRITE (I02,80003) IVTNUM                                          05540097
      IF (ICZERO) 48940, 8951, 48940                                    05550097
48940 IF (RVCOMP - 28.795) 28940,18940,48941                            05560097
48941 IF (RVCOMP - 28.805) 18940,18940,28940                            05570097
18940 IVPASS = IVPASS + 1                                               05580097
      WRITE (I02,80001) IVTNUM                                          05590097
      GO TO 8951                                                        05600097
28940 IVFAIL = IVFAIL + 1                                               05610097
      RVCORR = 28.800                                                   05620097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05630097
 8951 CONTINUE                                                          05640097
C                                                                       05650097
C     TEST 895 THROUGH TEST 897 CONTAIN INTRINSIC FUNCTION TESTS FOR    05660097
C     FLOAT - CONVERSION OF AN INTEGER ARGUMENT TO REAL FUNCTION        05670097
C                                                                       05680097
      IVTNUM = 895                                                      05690097
C                                                                       05700097
C      ****  TEST 895  ****                                             05710097
C                                                                       05720097
      IF (ICZERO) 38950, 8950, 38950                                    05730097
 8950 CONTINUE                                                          05740097
      RVCOMP = FLOAT (-606)                                             05750097
      GO TO 48950                                                       05760097
38950 IVDELE = IVDELE + 1                                               05770097
      WRITE (I02,80003) IVTNUM                                          05780097
      IF (ICZERO) 48950, 8961, 48950                                    05790097
48950 IF (RVCOMP + 606.05) 28950,18950,48951                            05800097
48951 IF (RVCOMP + 605.95) 18950,18950,28950                            05810097
18950 IVPASS = IVPASS + 1                                               05820097
      WRITE (I02,80001) IVTNUM                                          05830097
      GO TO 8961                                                        05840097
28950 IVFAIL = IVFAIL + 1                                               05850097
      RVCORR = -606.00                                                  05860097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          05870097
 8961 CONTINUE                                                          05880097
      IVTNUM = 896                                                      05890097
C                                                                       05900097
C      ****  TEST 896  ****                                             05910097
C                                                                       05920097
      IF (ICZERO) 38960, 8960, 38960                                    05930097
 8960 CONTINUE                                                          05940097
      IVON01 = 71                                                       05950097
      RVCOMP = FLOAT (IVON01)                                           05960097
      GO TO 48960                                                       05970097
38960 IVDELE = IVDELE + 1                                               05980097
      WRITE (I02,80003) IVTNUM                                          05990097
      IF (ICZERO) 48960, 8971, 48960                                    06000097
48960 IF (RVCOMP - 70.995) 28960,18960,48961                            06010097
48961 IF (RVCOMP - 71.005) 18960,18960,28960                            06020097
18960 IVPASS = IVPASS + 1                                               06030097
      WRITE (I02,80001) IVTNUM                                          06040097
      GO TO 8971                                                        06050097
28960 IVFAIL = IVFAIL + 1                                               06060097
      RVCORR = 71.000                                                   06070097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06080097
 8971 CONTINUE                                                          06090097
      IVTNUM = 897                                                      06100097
C                                                                       06110097
C      ****  TEST 897  ****                                             06120097
C                                                                       06130097
      IF (ICZERO) 38970, 8970, 38970                                    06140097
 8970 CONTINUE                                                          06150097
      IVON01 = 321                                                      06160097
      RVCOMP = FLOAT (-IVON01)                                          06170097
      GO TO 48970                                                       06180097
38970 IVDELE = IVDELE + 1                                               06190097
      WRITE (I02,80003) IVTNUM                                          06200097
      IF (ICZERO) 48970, 8981, 48970                                    06210097
48970 IF (RVCOMP + 321.05) 28970,18970,48971                            06220097
48971 IF (RVCOMP + 320.95) 18970,18970,28970                            06230097
18970 IVPASS = IVPASS + 1                                               06240097
      WRITE (I02,80001) IVTNUM                                          06250097
      GO TO 8981                                                        06260097
28970 IVFAIL = IVFAIL + 1                                               06270097
      RVCORR = -321.00                                                  06280097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06290097
 8981 CONTINUE                                                          06300097
C                                                                       06310097
C     TEST 898 THROUGH TEST 900 CONTAIN INTRINSIC FUNCTION TESTS FOR    06320097
C     TRANSFER OF SIGN - BOTH ARGUMENTS AND FUNCTION ARE REAL           06330097
C                                                                       06340097
      IVTNUM = 898                                                      06350097
C                                                                       06360097
C      ****  TEST 898  ****                                             06370097
C                                                                       06380097
      IF (ICZERO) 38980, 8980, 38980                                    06390097
 8980 CONTINUE                                                          06400097
      RVON01 = 64.3                                                     06410097
      RVCOMP = SIGN (RVON01,-1.0)                                       06420097
      GO TO 48980                                                       06430097
38980 IVDELE = IVDELE + 1                                               06440097
      WRITE (I02,80003) IVTNUM                                          06450097
      IF (ICZERO) 48980, 8991, 48980                                    06460097
48980 IF (RVCOMP + 64.305) 28980,18980,48981                            06470097
48981 IF (RVCOMP + 64.295) 18980,18980,28980                            06480097
18980 IVPASS = IVPASS + 1                                               06490097
      WRITE (I02,80001) IVTNUM                                          06500097
      GO TO 8991                                                        06510097
28980 IVFAIL = IVFAIL + 1                                               06520097
      RVCORR = -64.300                                                  06530097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06540097
 8991 CONTINUE                                                          06550097
      IVTNUM = 899                                                      06560097
C                                                                       06570097
C      ****  TEST 899  ****                                             06580097
C                                                                       06590097
      IF (ICZERO) 38990, 8990, 38990                                    06600097
 8990 CONTINUE                                                          06610097
      RVON01 = -2.2                                                     06620097
      RVON02 = 7.23E1                                                   06630097
      RVCOMP = SIGN (RVON01,RVON02)                                     06640097
      GO TO 48990                                                       06650097
38990 IVDELE = IVDELE + 1                                               06660097
      WRITE (I02,80003) IVTNUM                                          06670097
      IF (ICZERO) 48990, 9001, 48990                                    06680097
48990 IF (RVCOMP - 2.1995) 28990,18990,48991                            06690097
48991 IF (RVCOMP - 2.2005) 18990,18990,28990                            06700097
18990 IVPASS = IVPASS + 1                                               06710097
      WRITE (I02,80001) IVTNUM                                          06720097
      GO TO 9001                                                        06730097
28990 IVFAIL = IVFAIL + 1                                               06740097
      RVCORR = 2.2000                                                   06750097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06760097
 9001 CONTINUE                                                          06770097
      IVTNUM = 900                                                      06780097
C                                                                       06790097
C      ****  TEST 900  ****                                             06800097
C                                                                       06810097
      IF (ICZERO) 39000, 9000, 39000                                    06820097
 9000 CONTINUE                                                          06830097
      RVON01 = 35.32E+1                                                 06840097
      RVON02 = 1.0                                                      06850097
      RVCOMP = SIGN (RVON01,RVON02)                                     06860097
      GO TO 49000                                                       06870097
39000 IVDELE = IVDELE + 1                                               06880097
      WRITE (I02,80003) IVTNUM                                          06890097
      IF (ICZERO) 49000, 9011, 49000                                    06900097
49000 IF (RVCOMP - 353.15) 29000,19000,49001                            06910097
49001 IF (RVCOMP - 353.25) 19000,19000,29000                            06920097
19000 IVPASS = IVPASS + 1                                               06930097
      WRITE (I02,80001) IVTNUM                                          06940097
      GO TO 9011                                                        06950097
29000 IVFAIL = IVFAIL + 1                                               06960097
      RVCORR = 353.20                                                   06970097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06980097
 9011 CONTINUE                                                          06990097
C                                                                       07000097
C     TEST 901 THROUGH TEST 904 CONTAIN INTRINSIC FUNCTION TESTS FOR    07010097
C     POSITIVE DIFFERENCE WHERE ARGUMENTS AND FUNCTION ARE REAL         07020097
C                                                                       07030097
      IVTNUM = 901                                                      07040097
C                                                                       07050097
C      ****  TEST 901  ****                                             07060097
C                                                                       07070097
      IF (ICZERO) 39010, 9010, 39010                                    07080097
 9010 CONTINUE                                                          07090097
      RVON01 = 22.2                                                     07100097
      RVCOMP = DIM (RVON01,1.0)                                         07110097
      GO TO 49010                                                       07120097
39010 IVDELE = IVDELE + 1                                               07130097
      WRITE (I02,80003) IVTNUM                                          07140097
      IF (ICZERO) 49010, 9021, 49010                                    07150097
49010 IF (RVCOMP - 21.195) 29010,19010,49011                            07160097
49011 IF (RVCOMP - 21.205) 19010,19010,29010                            07170097
19010 IVPASS = IVPASS + 1                                               07180097
      WRITE (I02,80001) IVTNUM                                          07190097
      GO TO 9021                                                        07200097
29010 IVFAIL = IVFAIL + 1                                               07210097
      RVCORR = 21.200                                                   07220097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07230097
 9021 CONTINUE                                                          07240097
      IVTNUM = 902                                                      07250097
C                                                                       07260097
C      ****  TEST 902  ****                                             07270097
C                                                                       07280097
      IF (ICZERO) 39020, 9020, 39020                                    07290097
 9020 CONTINUE                                                          07300097
      RVON01 = 4.5E1                                                    07310097
      RVON02 = 41.0                                                     07320097
      RVCOMP = DIM (RVON01,RVON02)                                      07330097
      GO TO 49020                                                       07340097
39020 IVDELE = IVDELE + 1                                               07350097
      WRITE (I02,80003) IVTNUM                                          07360097
      IF (ICZERO) 49020, 9031, 49020                                    07370097
49020 IF (RVCOMP - 3.9995) 29020,19020,49021                            07380097
49021 IF (RVCOMP - 4.0005) 19020,19020,29020                            07390097
19020 IVPASS = IVPASS + 1                                               07400097
      WRITE (I02,80001) IVTNUM                                          07410097
      GO TO 9031                                                        07420097
29020 IVFAIL = IVFAIL + 1                                               07430097
      RVCORR = 4.0000                                                   07440097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07450097
 9031 CONTINUE                                                          07460097
      IVTNUM = 903                                                      07470097
C                                                                       07480097
C      ****  TEST 903  ****                                             07490097
C                                                                       07500097
      IF (ICZERO) 39030, 9030, 39030                                    07510097
 9030 CONTINUE                                                          07520097
      RVON01 = 2.0                                                      07530097
      RVON02 = 10.0                                                     07540097
      RVCOMP = DIM (RVON01,RVON02)                                      07550097
      GO TO 49030                                                       07560097
39030 IVDELE = IVDELE + 1                                               07570097
      WRITE (I02,80003) IVTNUM                                          07580097
      IF (ICZERO) 49030, 9041, 49030                                    07590097
49030 IF (RVCOMP) 29030,19030,29030                                     07600097
19030 IVPASS = IVPASS + 1                                               07610097
      WRITE (I02,80001) IVTNUM                                          07620097
      GO TO 9041                                                        07630097
29030 IVFAIL = IVFAIL + 1                                               07640097
      RVCORR = 0.0000                                                   07650097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07660097
 9041 CONTINUE                                                          07670097
      IVTNUM = 904                                                      07680097
C                                                                       07690097
C      ****  TEST 904  ****                                             07700097
C                                                                       07710097
      IF (ICZERO) 39040, 9040, 39040                                    07720097
 9040 CONTINUE                                                          07730097
      RVON01 = 1.65E+1                                                  07740097
      RVON02 = -2.0                                                     07750097
      RVCOMP = DIM (RVON01,RVON02)                                      07760097
      GO TO 49040                                                       07770097
39040 IVDELE = IVDELE + 1                                               07780097
      WRITE (I02,80003) IVTNUM                                          07790097
      IF (ICZERO) 49040, 9051, 49040                                    07800097
49040 IF (RVCOMP - 18.495) 29040,19040,49041                            07810097
49041 IF (RVCOMP - 18.505) 19040,19040,29040                            07820097
19040 IVPASS = IVPASS + 1                                               07830097
      WRITE (I02,80001) IVTNUM                                          07840097
      GO TO 9051                                                        07850097
29040 IVFAIL = IVFAIL + 1                                               07860097
      RVCORR = 18.500                                                   07870097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07880097
 9051 CONTINUE                                                          07890097
C                                                                       07900097
C     TESTS 905 AND 906 CONTAIN EXPRESSIONS CONTAINING MORE THAN ONE    07910097
C     INTRINSIC FUNCTION - ALL ARGUMENTS AND FUNCTIONS ARE REAL         07920097
C                                                                       07930097
      IVTNUM = 905                                                      07940097
C                                                                       07950097
C      ****  TEST 905  ****                                             07960097
C                                                                       07970097
      IF (ICZERO) 39050, 9050, 39050                                    07980097
 9050 CONTINUE                                                          07990097
      RVON01 = 33.3                                                     08000097
      RVON02 = -12.1                                                    08010097
      RVCOMP = AINT (RVON01) + ABS (RVON02)                             08020097
      GO TO 49050                                                       08030097
39050 IVDELE = IVDELE + 1                                               08040097
      WRITE (I02,80003) IVTNUM                                          08050097
      IF (ICZERO) 49050, 9061, 49050                                    08060097
49050 IF (RVCOMP - 45.095) 29050,19050,49051                            08070097
49051 IF (RVCOMP - 45.105) 19050,19050,29050                            08080097
19050 IVPASS = IVPASS + 1                                               08090097
      WRITE (I02,80001) IVTNUM                                          08100097
      GO TO 9061                                                        08110097
29050 IVFAIL = IVFAIL + 1                                               08120097
      RVCORR = 45.100                                                   08130097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          08140097
 9061 CONTINUE                                                          08150097
      IVTNUM = 906                                                      08160097
C                                                                       08170097
C      ****  TEST 906  ****                                             08180097
C                                                                       08190097
      IF (ICZERO) 39060, 9060, 39060                                    08200097
 9060 CONTINUE                                                          08210097
      RVON01 = 76.3                                                     08220097
      RVON02 = 2.1E1                                                    08230097
      RVON03 = 3E1                                                      08240097
      RVCOMP = AMAX1(RVON01,RVON02,RVON03)-AMIN1(RVON01,RVON02,RVON03)  08250097
      GO TO 49060                                                       08260097
39060 IVDELE = IVDELE + 1                                               08270097
      WRITE (I02,80003) IVTNUM                                          08280097
      IF (ICZERO) 49060, 9071, 49060                                    08290097
49060 IF (RVCOMP - 55.295) 29060,19060,49061                            08300097
49061 IF (RVCOMP - 55.305) 19060,19060,29060                            08310097
19060 IVPASS = IVPASS + 1                                               08320097
      WRITE (I02,80001) IVTNUM                                          08330097
      GO TO 9071                                                        08340097
29060 IVFAIL = IVFAIL + 1                                               08350097
      RVCORR = 55.300                                                   08360097
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          08370097
 9071 CONTINUE                                                          08380097
C                                                                       08390097
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08400097
99999 CONTINUE                                                          08410097
      WRITE (I02,90002)                                                 08420097
      WRITE (I02,90006)                                                 08430097
      WRITE (I02,90002)                                                 08440097
      WRITE (I02,90002)                                                 08450097
      WRITE (I02,90007)                                                 08460097
      WRITE (I02,90002)                                                 08470097
      WRITE (I02,90008)  IVFAIL                                         08480097
      WRITE (I02,90009) IVPASS                                          08490097
      WRITE (I02,90010) IVDELE                                          08500097
C                                                                       08510097
C                                                                       08520097
C     TERMINATE ROUTINE EXECUTION                                       08530097
      STOP                                                              08540097
C                                                                       08550097
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08560097
90000 FORMAT (1H1)                                                      08570097
90002 FORMAT (1H )                                                      08580097
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08590097
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08600097
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08610097
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08620097
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08630097
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08640097
C                                                                       08650097
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08660097
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08670097
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08680097
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08690097
C                                                                       08700097
C     FORMAT STATEMENTS FOR TEST RESULTS                                08710097
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08720097
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08730097
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08740097
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08750097
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08760097
C                                                                       08770097
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM097)                          08780097
      END                                                               08790097
