C        COMMENT SECTION                                                00010007
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020007
C     FM007                                                             00030007
C                                                                       00040007
C         THIS ROUTINE TESTS THE USE OF DATA INITIALIZATION STATEMENTS. 00050007
C     DATA INITIALIZATION STATEMENTS ARE USED TO DEFINE INITIAL VALUES  00060007
C     OF INTEGER VARIABLES.  THE DATA STATEMENTS CONTAIN UNSIGNED,      00070007
C     POSITIVE SIGNED AND NEGATIVE SIGNED INTEGER CONSTANTS.  THE LAST  00080007
C     DATA STATEMENT CONTAINS THE FORM                                  00090007
C                   J*INTEGER CONSTANT                                  00100007
C     WHICH INDICATES THE CONSTANT IS TO BE SPECIFIED J TIMES.          00110007
C                                                                       00120007
C      THE TESTS IN THIS ROUTINE CHECK THE INTEGER VARIABLES IN THE     00130007
C     DATA STATEMENT FOR THE ASSIGNED INITIAL VALUES.                   00140007
C                                                                       00150007
C      REFERENCES                                                       00160007
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00170007
C              X3.9-1978                                                00180007
C                                                                       00190007
C        SECTION 4.3, INTEGER TYPE                                      00200007
C        SECTION 4.3.1, INTEGER CONSTANT                                00210007
C        SECTION 9, DATA STATEMENT                                      00220007
C                                                                       00230007
C                                                                       00240007
C         DATA INITIALIZATION STATEMENTS                                00250007
C                                                                       00260007
      DATA IVON01,IVON02,IVON03,IVON04,IVON05/3,76,587,9999,21111/      00270007
      DATA IVON06,IVON07,IVON08,IVON09,IVON10/+3,+76,+587,+9999,+21111/ 00280007
      DATA IVON11,IVON12,IVON13,IVON14,IVON15/-3,-76,-587,-9999,-21111/ 00290007
      DATA IVON16,IVON17,IVON18,IVON19,IVON20/ 2*119, 2*7, -427/        00300007
C                                                                       00310007
C                                                                       00320007
C      **********************************************************       00330007
C                                                                       00340007
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00350007
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00360007
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00370007
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00380007
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00390007
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00400007
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00410007
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00420007
C     OF EXECUTING THESE TESTS.                                         00430007
C                                                                       00440007
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00450007
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00460007
C                                                                       00470007
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00480007
C                                                                       00490007
C                  DEPARTMENT OF THE NAVY                               00500007
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00510007
C                  WASHINGTON, D.C.  20376                              00520007
C                                                                       00530007
C      **********************************************************       00540007
C                                                                       00550007
C                                                                       00560007
C                                                                       00570007
C     INITIALIZATION SECTION                                            00580007
C                                                                       00590007
C     INITIALIZE CONSTANTS                                              00600007
C      **************                                                   00610007
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620007
      I01 = 5                                                           00630007
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640007
      I02 = 6                                                           00650007
C     SYSTEM ENVIRONMENT SECTION                                        00660007
C                                                                       00670007
      I01 = 5                                                           00680007
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690007
C     (UNIT NUMBER FOR CARD READER).                                    00700007
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00710007
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00720007
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00730007
C                                                                       00740007
      I02 = 6                                                           00750007
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00760007
C     (UNIT NUMBER FOR PRINTER).                                        00770007
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00780007
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00790007
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00800007
C                                                                       00810007
      IVPASS=0                                                          00820007
      IVFAIL=0                                                          00830007
      IVDELE=0                                                          00840007
      ICZERO=0                                                          00850007
C                                                                       00860007
C     WRITE PAGE HEADERS                                                00870007
      WRITE (I02,90000)                                                 00880007
      WRITE (I02,90001)                                                 00890007
      WRITE (I02,90002)                                                 00900007
      WRITE (I02, 90002)                                                00910007
      WRITE (I02,90003)                                                 00920007
      WRITE (I02,90002)                                                 00930007
      WRITE (I02,90004)                                                 00940007
      WRITE (I02,90002)                                                 00950007
      WRITE (I02,90011)                                                 00960007
      WRITE (I02,90002)                                                 00970007
      WRITE (I02,90002)                                                 00980007
      WRITE (I02,90005)                                                 00990007
      WRITE (I02,90006)                                                 01000007
      WRITE (I02,90002)                                                 01010007
C     TEST SECTION                                                      01020007
C                                                                       01030007
C     TESTS 80 THROUGH 84 CHECK THE VALUES INITIALIZED BY THE DATA      01040007
C     STATEMENT CONTAINING IVON01,..., IVON05.                          01050007
C                                                                       01060007
  801 CONTINUE                                                          01070007
      IVTNUM =  80                                                      01080007
C                                                                       01090007
C      ****  TEST 80  ****                                              01100007
C                                                                       01110007
      IF (ICZERO) 30800,  800, 30800                                    01120007
  800 CONTINUE                                                          01130007
      IVCOMP = IVON01                                                   01140007
      GO TO 40800                                                       01150007
30800 IVDELE = IVDELE + 1                                               01160007
      WRITE (I02,80003) IVTNUM                                          01170007
      IF (ICZERO) 40800,  811, 40800                                    01180007
40800 IF (IVCOMP - 3) 20800, 10800,20800                                01190007
10800 IVPASS = IVPASS + 1                                               01200007
      WRITE (I02,80001) IVTNUM                                          01210007
      GO TO  811                                                        01220007
20800 IVFAIL = IVFAIL + 1                                               01230007
      IVCORR = 3                                                        01240007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01250007
  811 CONTINUE                                                          01260007
      IVTNUM =  81                                                      01270007
C                                                                       01280007
C      ****  TEST 81  ****                                              01290007
C                                                                       01300007
      IF (ICZERO) 30810,  810, 30810                                    01310007
  810 CONTINUE                                                          01320007
      IVCOMP = IVON02                                                   01330007
      GO TO 40810                                                       01340007
30810 IVDELE = IVDELE + 1                                               01350007
      WRITE (I02,80003) IVTNUM                                          01360007
      IF (ICZERO) 40810,  821, 40810                                    01370007
40810 IF (IVCOMP - 76) 20810, 10810, 20810                              01380007
10810 IVPASS = IVPASS + 1                                               01390007
      WRITE (I02,80001) IVTNUM                                          01400007
      GO TO  821                                                        01410007
20810 IVFAIL = IVFAIL + 1                                               01420007
      IVCORR = 76                                                       01430007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01440007
  821 CONTINUE                                                          01450007
      IVTNUM =  82                                                      01460007
C                                                                       01470007
C      ****  TEST 82  ****                                              01480007
C                                                                       01490007
      IF (ICZERO) 30820,  820, 30820                                    01500007
  820 CONTINUE                                                          01510007
      IVCOMP = IVON03                                                   01520007
      GO TO 40820                                                       01530007
30820 IVDELE = IVDELE + 1                                               01540007
      WRITE (I02,80003) IVTNUM                                          01550007
      IF (ICZERO) 40820,  831, 40820                                    01560007
40820 IF (IVCOMP - 587) 20820, 10820, 20820                             01570007
10820 IVPASS = IVPASS + 1                                               01580007
      WRITE (I02,80001) IVTNUM                                          01590007
      GO TO  831                                                        01600007
20820 IVFAIL = IVFAIL + 1                                               01610007
      IVCORR = 587                                                      01620007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01630007
  831 CONTINUE                                                          01640007
      IVTNUM =  83                                                      01650007
C                                                                       01660007
C      ****  TEST 83  ****                                              01670007
C                                                                       01680007
      IF (ICZERO) 30830,  830, 30830                                    01690007
  830 CONTINUE                                                          01700007
      IVCOMP =IVON04                                                    01710007
      GO TO 40830                                                       01720007
30830 IVDELE = IVDELE + 1                                               01730007
      WRITE (I02,80003) IVTNUM                                          01740007
      IF (ICZERO) 40830,  841, 40830                                    01750007
40830 IF (IVCOMP - 9999)  20830, 10830, 20830                           01760007
10830 IVPASS = IVPASS + 1                                               01770007
      WRITE (I02,80001) IVTNUM                                          01780007
      GO TO  841                                                        01790007
20830 IVFAIL = IVFAIL + 1                                               01800007
      IVCORR = 9999                                                     01810007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01820007
  841 CONTINUE                                                          01830007
      IVTNUM =  84                                                      01840007
C                                                                       01850007
C      ****  TEST 84  ****                                              01860007
C                                                                       01870007
      IF (ICZERO) 30840,  840, 30840                                    01880007
  840 CONTINUE                                                          01890007
      IVCOMP = IVON05                                                   01900007
      GO TO 40840                                                       01910007
30840 IVDELE = IVDELE + 1                                               01920007
      WRITE (I02,80003) IVTNUM                                          01930007
      IF (ICZERO) 40840,  851, 40840                                    01940007
40840 IF (IVCOMP - 21111) 20840, 10840, 20840                           01950007
10840 IVPASS = IVPASS + 1                                               01960007
      WRITE (I02,80001) IVTNUM                                          01970007
      GO TO  851                                                        01980007
20840 IVFAIL = IVFAIL + 1                                               01990007
      IVCORR = 21111                                                    02000007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02010007
C                                                                       02020007
C        TESTS 85 THROUGH 89 CHECK THE VALUES INITIALIZED BY THE DATA   02030007
C     STATEMENT CONTAINING IVON06,...,IVON10.                           02040007
C                                                                       02050007
  851 CONTINUE                                                          02060007
      IVTNUM =  85                                                      02070007
C                                                                       02080007
C      ****  TEST 85  ****                                              02090007
C                                                                       02100007
      IF (ICZERO) 30850,  850, 30850                                    02110007
  850 CONTINUE                                                          02120007
      IVCOMP=IVON06                                                     02130007
      GO TO 40850                                                       02140007
30850 IVDELE = IVDELE + 1                                               02150007
      WRITE (I02,80003) IVTNUM                                          02160007
      IF (ICZERO) 40850,  861, 40850                                    02170007
40850 IF (IVCOMP - 3) 20850, 10850, 20850                               02180007
10850 IVPASS = IVPASS + 1                                               02190007
      WRITE (I02,80001) IVTNUM                                          02200007
      GO TO  861                                                        02210007
20850 IVFAIL = IVFAIL + 1                                               02220007
      IVCORR = 3                                                        02230007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02240007
  861 CONTINUE                                                          02250007
      IVTNUM =  86                                                      02260007
C                                                                       02270007
C      ****  TEST 86  ****                                              02280007
C                                                                       02290007
      IF (ICZERO) 30860,  860, 30860                                    02300007
  860 CONTINUE                                                          02310007
      IVCOMP = IVON07                                                   02320007
      GO TO 40860                                                       02330007
30860 IVDELE = IVDELE + 1                                               02340007
      WRITE (I02,80003) IVTNUM                                          02350007
      IF (ICZERO) 40860,  871, 40860                                    02360007
40860 IF (IVCOMP - 76) 20860, 10860, 20860                              02370007
10860 IVPASS = IVPASS + 1                                               02380007
      WRITE (I02,80001) IVTNUM                                          02390007
      GO TO  871                                                        02400007
20860 IVFAIL = IVFAIL + 1                                               02410007
      IVCORR = 76                                                       02420007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02430007
  871 CONTINUE                                                          02440007
      IVTNUM =  87                                                      02450007
C                                                                       02460007
C      ****  TEST 87  ****                                              02470007
C                                                                       02480007
      IF (ICZERO) 30870,  870, 30870                                    02490007
  870 CONTINUE                                                          02500007
      IVCOMP = IVON08                                                   02510007
      GO TO 40870                                                       02520007
30870 IVDELE = IVDELE + 1                                               02530007
      WRITE (I02,80003) IVTNUM                                          02540007
      IF (ICZERO) 40870,  881, 40870                                    02550007
40870 IF (IVCOMP - 587) 20870, 10870, 20870                             02560007
10870 IVPASS = IVPASS + 1                                               02570007
      WRITE (I02,80001) IVTNUM                                          02580007
      GO TO  881                                                        02590007
20870 IVFAIL = IVFAIL + 1                                               02600007
      IVCORR = 587                                                      02610007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02620007
  881 CONTINUE                                                          02630007
      IVTNUM =  88                                                      02640007
C                                                                       02650007
C      ****  TEST 88  ****                                              02660007
C                                                                       02670007
      IF (ICZERO) 30880,  880, 30880                                    02680007
  880 CONTINUE                                                          02690007
      IVCOMP = IVON09                                                   02700007
      GO TO 40880                                                       02710007
30880 IVDELE = IVDELE + 1                                               02720007
      WRITE (I02,80003) IVTNUM                                          02730007
      IF (ICZERO) 40880,  891, 40880                                    02740007
40880 IF (IVCOMP - 9999) 20880, 10880, 20880                            02750007
10880 IVPASS = IVPASS + 1                                               02760007
      WRITE (I02,80001) IVTNUM                                          02770007
      GO TO  891                                                        02780007
20880 IVFAIL = IVFAIL + 1                                               02790007
      IVCORR = 9999                                                     02800007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02810007
  891 CONTINUE                                                          02820007
      IVTNUM =  89                                                      02830007
C                                                                       02840007
C      ****  TEST 89  ****                                              02850007
C                                                                       02860007
      IF (ICZERO) 30890,  890, 30890                                    02870007
  890 CONTINUE                                                          02880007
      IVCOMP = IVON10                                                   02890007
      GO TO 40890                                                       02900007
30890 IVDELE = IVDELE + 1                                               02910007
      WRITE (I02,80003) IVTNUM                                          02920007
      IF (ICZERO) 40890,  901, 40890                                    02930007
40890 IF (IVCOMP - 21111)  20890, 10890, 20890                          02940007
10890 IVPASS = IVPASS + 1                                               02950007
      WRITE (I02,80001) IVTNUM                                          02960007
      GO TO  901                                                        02970007
20890 IVFAIL = IVFAIL + 1                                               02980007
      IVCORR= 21111                                                     02990007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03000007
C                                                                       03010007
C         TESTS 90 THROUGH 94 CHECK THE VALUES INITIALIZED BY THE DATA  03020007
C     STATEMENT CONTAINING IVON11,...,IVON15.                           03030007
C                                                                       03040007
  901 CONTINUE                                                          03050007
      IVTNUM =  90                                                      03060007
C                                                                       03070007
C      ****  TEST 90  ****                                              03080007
C                                                                       03090007
      IF (ICZERO) 30900,  900, 30900                                    03100007
  900 CONTINUE                                                          03110007
      IVCOMP = IVON11                                                   03120007
      GO TO 40900                                                       03130007
30900 IVDELE = IVDELE + 1                                               03140007
      WRITE (I02,80003) IVTNUM                                          03150007
      IF (ICZERO) 40900,  911, 40900                                    03160007
40900 IF (IVCOMP + 3) 20900, 10900, 20900                               03170007
10900 IVPASS = IVPASS + 1                                               03180007
      WRITE (I02,80001) IVTNUM                                          03190007
      GO TO  911                                                        03200007
20900 IVFAIL = IVFAIL + 1                                               03210007
      IVCORR = -3                                                       03220007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03230007
  911 CONTINUE                                                          03240007
      IVTNUM =  91                                                      03250007
C                                                                       03260007
C      ****  TEST 91  ****                                              03270007
C                                                                       03280007
      IF (ICZERO) 30910,  910, 30910                                    03290007
  910 CONTINUE                                                          03300007
      IVCOMP = IVON12                                                   03310007
      GO TO 40910                                                       03320007
30910 IVDELE = IVDELE + 1                                               03330007
      WRITE (I02,80003) IVTNUM                                          03340007
      IF (ICZERO) 40910,  921, 40910                                    03350007
40910 IF (IVCOMP + 76) 20910, 10910, 20910                              03360007
10910 IVPASS = IVPASS + 1                                               03370007
      WRITE (I02,80001) IVTNUM                                          03380007
      GO TO  921                                                        03390007
20910 IVFAIL = IVFAIL + 1                                               03400007
      IVCORR = -76                                                      03410007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03420007
  921 CONTINUE                                                          03430007
      IVTNUM =  92                                                      03440007
C                                                                       03450007
C      ****  TEST 92  ****                                              03460007
C                                                                       03470007
      IF (ICZERO) 30920,  920, 30920                                    03480007
  920 CONTINUE                                                          03490007
      IVCOMP= IVON13                                                    03500007
      GO TO 40920                                                       03510007
30920 IVDELE = IVDELE + 1                                               03520007
      WRITE (I02,80003) IVTNUM                                          03530007
      IF (ICZERO) 40920,  931, 40920                                    03540007
40920 IF (IVCOMP + 587) 20920, 10920, 20920                             03550007
10920 IVPASS = IVPASS + 1                                               03560007
      WRITE (I02,80001) IVTNUM                                          03570007
      GO TO  931                                                        03580007
20920 IVFAIL = IVFAIL + 1                                               03590007
      IVCORR = -587                                                     03600007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03610007
  931 CONTINUE                                                          03620007
      IVTNUM =  93                                                      03630007
C                                                                       03640007
C      ****  TEST 93  ****                                              03650007
C                                                                       03660007
      IF (ICZERO) 30930,  930, 30930                                    03670007
  930 CONTINUE                                                          03680007
      IVCOMP = IVON14                                                   03690007
      GO TO 40930                                                       03700007
30930 IVDELE = IVDELE + 1                                               03710007
      WRITE (I02,80003) IVTNUM                                          03720007
      IF (ICZERO) 40930,  941, 40930                                    03730007
40930 IF (IVCOMP + 9999) 20930, 10930, 20930                            03740007
10930 IVPASS = IVPASS + 1                                               03750007
      WRITE (I02,80001) IVTNUM                                          03760007
      GO TO  941                                                        03770007
20930 IVFAIL = IVFAIL + 1                                               03780007
      IVCORR = -9999                                                    03790007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03800007
  941 CONTINUE                                                          03810007
      IVTNUM =  94                                                      03820007
C                                                                       03830007
C      ****  TEST 94  ****                                              03840007
C                                                                       03850007
      IF (ICZERO) 30940,  940, 30940                                    03860007
  940 CONTINUE                                                          03870007
      IVCOMP = IVON15                                                   03880007
      GO TO 40940                                                       03890007
30940 IVDELE = IVDELE + 1                                               03900007
      WRITE (I02,80003) IVTNUM                                          03910007
      IF (ICZERO) 40940,  951, 40940                                    03920007
40940 IF (IVCOMP + 21111) 20940, 10940, 20940                           03930007
10940 IVPASS = IVPASS + 1                                               03940007
      WRITE (I02,80001) IVTNUM                                          03950007
      GO TO  951                                                        03960007
20940 IVFAIL = IVFAIL + 1                                               03970007
      IVCORR = -21111                                                   03980007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03990007
C                                                                       04000007
C         TESTS 95 THROUGH 99 CHECK THE VALUES INITIALIZED BY THE DATA  04010007
C     STATEMENT CONTAINING IVON16,...,IVON20.                           04020007
C                                                                       04030007
  951 CONTINUE                                                          04040007
      IVTNUM =  95                                                      04050007
C                                                                       04060007
C      ****  TEST 95  ****                                              04070007
C                                                                       04080007
      IF (ICZERO) 30950,  950, 30950                                    04090007
  950 CONTINUE                                                          04100007
      IVCOMP =IVON16                                                    04110007
      GO TO 40950                                                       04120007
30950 IVDELE = IVDELE + 1                                               04130007
      WRITE (I02,80003) IVTNUM                                          04140007
      IF (ICZERO) 40950,  961, 40950                                    04150007
40950 IF (IVCOMP - 119) 20950, 10950, 20950                             04160007
10950 IVPASS = IVPASS + 1                                               04170007
      WRITE (I02,80001) IVTNUM                                          04180007
      GO TO  961                                                        04190007
20950 IVFAIL = IVFAIL + 1                                               04200007
      IVCORR = 119                                                      04210007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04220007
  961 CONTINUE                                                          04230007
      IVTNUM =  96                                                      04240007
C                                                                       04250007
C      ****  TEST 96  ****                                              04260007
C                                                                       04270007
      IF (ICZERO) 30960,  960, 30960                                    04280007
  960 CONTINUE                                                          04290007
      IVCOMP=IVON17                                                     04300007
      GO TO 40960                                                       04310007
30960 IVDELE = IVDELE + 1                                               04320007
      WRITE (I02,80003) IVTNUM                                          04330007
      IF (ICZERO) 40960,  971, 40960                                    04340007
40960 IF (IVCOMP - 119) 20960, 10960, 20960                             04350007
10960 IVPASS = IVPASS + 1                                               04360007
      WRITE (I02,80001) IVTNUM                                          04370007
      GO TO  971                                                        04380007
20960 IVFAIL = IVFAIL + 1                                               04390007
      IVCORR = 119                                                      04400007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04410007
  971 CONTINUE                                                          04420007
      IVTNUM =  97                                                      04430007
C                                                                       04440007
C      ****  TEST 97  ****                                              04450007
C                                                                       04460007
      IF (ICZERO) 30970,  970, 30970                                    04470007
  970 CONTINUE                                                          04480007
      IVCOMP = IVON18                                                   04490007
      GO TO 40970                                                       04500007
30970 IVDELE = IVDELE + 1                                               04510007
      WRITE (I02,80003) IVTNUM                                          04520007
      IF (ICZERO) 40970,  981, 40970                                    04530007
40970 IF (IVCOMP - 7) 20970, 10970, 20970                               04540007
10970 IVPASS = IVPASS + 1                                               04550007
      WRITE (I02,80001) IVTNUM                                          04560007
      GO TO  981                                                        04570007
20970 IVFAIL = IVFAIL + 1                                               04580007
      IVCORR = 7                                                        04590007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04600007
  981 CONTINUE                                                          04610007
      IVTNUM =  98                                                      04620007
C                                                                       04630007
C      ****  TEST 98  ****                                              04640007
C                                                                       04650007
      IF (ICZERO) 30980,  980, 30980                                    04660007
  980 CONTINUE                                                          04670007
      IVCOMP = IVON19                                                   04680007
      GO TO 40980                                                       04690007
30980 IVDELE = IVDELE + 1                                               04700007
      WRITE (I02,80003) IVTNUM                                          04710007
      IF (ICZERO) 40980,  991, 40980                                    04720007
40980 IF (IVCOMP - 7) 20980, 10980, 20980                               04730007
10980 IVPASS = IVPASS + 1                                               04740007
      WRITE (I02,80001) IVTNUM                                          04750007
      GO TO  991                                                        04760007
20980 IVFAIL = IVFAIL + 1                                               04770007
      IVCORR = 7                                                        04780007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04790007
  991 CONTINUE                                                          04800007
      IVTNUM =  99                                                      04810007
C                                                                       04820007
C      ****  TEST 99  ****                                              04830007
C                                                                       04840007
      IF (ICZERO) 30990,  990, 30990                                    04850007
  990 CONTINUE                                                          04860007
      IVCOMP = IVON20                                                   04870007
      GO TO 40990                                                       04880007
30990 IVDELE = IVDELE + 1                                               04890007
      WRITE (I02,80003) IVTNUM                                          04900007
      IF (ICZERO) 40990, 1001, 40990                                    04910007
40990 IF (IVCOMP + 427)  20990,10990,20990                              04920007
10990 IVPASS = IVPASS + 1                                               04930007
      WRITE (I02,80001) IVTNUM                                          04940007
      GO TO 1001                                                        04950007
20990 IVFAIL = IVFAIL + 1                                               04960007
      IVCORR = -427                                                     04970007
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04980007
 1001 CONTINUE                                                          04990007
C                                                                       05000007
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             05010007
99999 CONTINUE                                                          05020007
      WRITE (I02,90002)                                                 05030007
      WRITE (I02,90006)                                                 05040007
      WRITE (I02,90002)                                                 05050007
      WRITE (I02,90002)                                                 05060007
      WRITE (I02,90007)                                                 05070007
      WRITE (I02,90002)                                                 05080007
      WRITE (I02,90008)  IVFAIL                                         05090007
      WRITE (I02,90009) IVPASS                                          05100007
      WRITE (I02,90010) IVDELE                                          05110007
C                                                                       05120007
C                                                                       05130007
C     TERMINATE ROUTINE EXECUTION                                       05140007
      STOP                                                              05150007
C                                                                       05160007
C     FORMAT STATEMENTS FOR PAGE HEADERS                                05170007
90000 FORMAT (1H1)                                                      05180007
90002 FORMAT (1H )                                                      05190007
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05200007
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   05210007
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        05220007
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 05230007
90006 FORMAT (1H ,5X,46H----------------------------------------------) 05240007
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             05250007
C                                                                       05260007
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               05270007
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        05280007
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              05290007
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             05300007
C                                                                       05310007
C     FORMAT STATEMENTS FOR TEST RESULTS                                05320007
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      05330007
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      05340007
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   05350007
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         05360007
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    05370007
C                                                                       05380007
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM007)                          05390007
      END                                                               05400007
