C     COMMENT SECTION                                                   00010031
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020031
C     FM031                                                             00030031
C                                                                       00040031
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050031
C     FORM                                                              00060031
C               INTEGER VARIABLE = ARITHMETIC EXPRESSION                00070031
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080031
C     OPERATOR -, INTEGER CONSTANTS AND INTEGER VARIABLES.  SOME OF THE 00090031
C     TESTS USE PARENTHESES TO GROUP ELEMENTS IN AN ARITHMETIC          00100031
C     EXPRESSION.                                                       00110031
C                                                                       00120031
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00130031
C           (1)  INTEGER CONSTANT-INTEGER CONSTANT-INTEGER VARIABLE     00140031
C                INTEGER CONSTANT-INTEGER VARIABLE-INTEGER CONSTANT     00150031
C                INTEGER VARIABLE-INTEGER CONSTANT-INTEGER CONSTANT     00160031
C           (2)  SAME AS (1) BUT WITH PARENTHESES TO GROUP ELEMENTS     00170031
C                IN ARITHMETIC EXPRESSION.                              00180031
C           (3)  INTEGER VARIABLE - INTEGER VARIABLE                    00190031
C                                                                       00200031
C      REFERENCES                                                       00210031
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00220031
C              X3.9-1978                                                00230031
C                                                                       00240031
C        SECTION 4.3, INTEGER TYPE                                      00250031
C        SECTION 4.3.1, INTEGER CONSTANT                                00260031
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00270031
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00280031
C                                                                       00290031
C      **********************************************************       00300031
C                                                                       00310031
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00320031
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00330031
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00340031
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00350031
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00360031
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00370031
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00380031
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00390031
C     OF EXECUTING THESE TESTS.                                         00400031
C                                                                       00410031
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00420031
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00430031
C                                                                       00440031
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00450031
C                                                                       00460031
C                  DEPARTMENT OF THE NAVY                               00470031
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00480031
C                  WASHINGTON, D.C.  20376                              00490031
C                                                                       00500031
C      **********************************************************       00510031
C                                                                       00520031
C                                                                       00530031
C                                                                       00540031
C     INITIALIZATION SECTION                                            00550031
C                                                                       00560031
C     INITIALIZE CONSTANTS                                              00570031
C      **************                                                   00580031
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00590031
      I01 = 5                                                           00600031
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00610031
      I02 = 6                                                           00620031
C     SYSTEM ENVIRONMENT SECTION                                        00630031
C                                                                       00640031
      I01 = 5                                                           00650031
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00660031
C     (UNIT NUMBER FOR CARD READER).                                    00670031
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00680031
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00690031
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00700031
C                                                                       00710031
      I02 = 6                                                           00720031
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00730031
C     (UNIT NUMBER FOR PRINTER).                                        00740031
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00750031
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760031
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00770031
C                                                                       00780031
      IVPASS=0                                                          00790031
      IVFAIL=0                                                          00800031
      IVDELE=0                                                          00810031
      ICZERO=0                                                          00820031
C                                                                       00830031
C     WRITE PAGE HEADERS                                                00840031
      WRITE (I02,90000)                                                 00850031
      WRITE (I02,90001)                                                 00860031
      WRITE (I02,90002)                                                 00870031
      WRITE (I02, 90002)                                                00880031
      WRITE (I02,90003)                                                 00890031
      WRITE (I02,90002)                                                 00900031
      WRITE (I02,90004)                                                 00910031
      WRITE (I02,90002)                                                 00920031
      WRITE (I02,90011)                                                 00930031
      WRITE (I02,90002)                                                 00940031
      WRITE (I02,90002)                                                 00950031
      WRITE (I02,90005)                                                 00960031
      WRITE (I02,90006)                                                 00970031
      WRITE (I02,90002)                                                 00980031
C                                                                       00990031
C     TEST SECTION                                                      01000031
C                                                                       01010031
C     TEST 300 THROUGH TEST 309 CONTAIN 2 INTEGER CONSTANTS, AN INTEGER 01020031
C     VARIABLE AND OPERATOR - IN AN ARITHMETIC EXPRESSION.              01030031
C                                                                       01040031
 3001 CONTINUE                                                          01050031
      IVTNUM = 300                                                      01060031
C                                                                       01070031
C      ****  TEST 300  ****                                             01080031
C                                                                       01090031
      IF (ICZERO) 33000, 3000, 33000                                    01100031
 3000 CONTINUE                                                          01110031
      IVON01 = 9                                                        01120031
      IVCOMP =IVON01 -3 -4                                              01130031
      GO TO 43000                                                       01140031
33000 IVDELE = IVDELE + 1                                               01150031
      WRITE (I02,80003) IVTNUM                                          01160031
      IF (ICZERO) 43000, 3011, 43000                                    01170031
43000 IF (IVCOMP-2) 23000,13000,23000                                   01180031
13000 IVPASS = IVPASS + 1                                               01190031
      WRITE (I02,80001) IVTNUM                                          01200031
      GO TO 3011                                                        01210031
23000 IVFAIL = IVFAIL + 1                                               01220031
      IVCORR =2                                                         01230031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01240031
 3011 CONTINUE                                                          01250031
      IVTNUM = 301                                                      01260031
C                                                                       01270031
C      ****  TEST 301  ****                                             01280031
C                                                                       01290031
      IF (ICZERO) 33010, 3010, 33010                                    01300031
 3010 CONTINUE                                                          01310031
      IVON02 =3                                                         01320031
      IVCOMP =9-IVON02-4                                                01330031
      GO TO 43010                                                       01340031
33010 IVDELE = IVDELE + 1                                               01350031
      WRITE (I02,80003) IVTNUM                                          01360031
      IF (ICZERO) 43010, 3021, 43010                                    01370031
43010 IF (IVCOMP-2) 23010,13010,23010                                   01380031
13010 IVPASS = IVPASS + 1                                               01390031
      WRITE (I02,80001) IVTNUM                                          01400031
      GO TO 3021                                                        01410031
23010 IVFAIL = IVFAIL + 1                                               01420031
      IVCORR =2                                                         01430031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01440031
 3021 CONTINUE                                                          01450031
      IVTNUM = 302                                                      01460031
C                                                                       01470031
C      ****  TEST 302  ****                                             01480031
C                                                                       01490031
      IF (ICZERO) 33020, 3020, 33020                                    01500031
 3020 CONTINUE                                                          01510031
      IVON03 = 4                                                        01520031
      IVCOMP = 9-3-IVON03                                               01530031
      GO TO 43020                                                       01540031
33020 IVDELE = IVDELE + 1                                               01550031
      WRITE (I02,80003) IVTNUM                                          01560031
      IF (ICZERO) 43020, 3031, 43020                                    01570031
43020 IF (IVCOMP-2) 23020,13020,23020                                   01580031
13020 IVPASS = IVPASS + 1                                               01590031
      WRITE (I02,80001) IVTNUM                                          01600031
      GO TO 3031                                                        01610031
23020 IVFAIL = IVFAIL + 1                                               01620031
      IVCORR =2                                                         01630031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01640031
 3031 CONTINUE                                                          01650031
      IVTNUM = 303                                                      01660031
C                                                                       01670031
C      ****  TEST 303  ****                                             01680031
C                                                                       01690031
      IF (ICZERO) 33030, 3030, 33030                                    01700031
 3030 CONTINUE                                                          01710031
      IVON01 = 57                                                       01720031
      IVCOMP = IVON01 -25-22                                            01730031
      GO TO 43030                                                       01740031
33030 IVDELE = IVDELE + 1                                               01750031
      WRITE (I02,80003) IVTNUM                                          01760031
      IF (ICZERO) 43030, 3041, 43030                                    01770031
43030 IF (IVCOMP-10) 23030,13030,23030                                  01780031
13030 IVPASS = IVPASS + 1                                               01790031
      WRITE (I02,80001) IVTNUM                                          01800031
      GO TO 3041                                                        01810031
23030 IVFAIL = IVFAIL + 1                                               01820031
      IVCORR = 10                                                       01830031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01840031
 3041 CONTINUE                                                          01850031
      IVTNUM = 304                                                      01860031
C                                                                       01870031
C      ****  TEST 304  ****                                             01880031
C                                                                       01890031
      IF (ICZERO) 33040, 3040, 33040                                    01900031
 3040 CONTINUE                                                          01910031
      IVON02 =683                                                       01920031
      IVCOMP = 101-IVON02-156                                           01930031
      GO TO 43040                                                       01940031
33040 IVDELE = IVDELE + 1                                               01950031
      WRITE (I02,80003) IVTNUM                                          01960031
      IF (ICZERO) 43040, 3051, 43040                                    01970031
43040 IF (IVCOMP+738) 23040,13040,23040                                 01980031
13040 IVPASS = IVPASS + 1                                               01990031
      WRITE (I02,80001) IVTNUM                                          02000031
      GO TO 3051                                                        02010031
23040 IVFAIL = IVFAIL + 1                                               02020031
      IVCORR = -738                                                     02030031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02040031
 3051 CONTINUE                                                          02050031
      IVTNUM = 305                                                      02060031
C                                                                       02070031
C      ****  TEST 305  ****                                             02080031
C                                                                       02090031
      IF (ICZERO) 33050, 3050, 33050                                    02100031
 3050 CONTINUE                                                          02110031
      IVON03 = 1289                                                     02120031
      IVCOMP = 8542-1122-IVON03                                         02130031
      GO TO 43050                                                       02140031
33050 IVDELE = IVDELE + 1                                               02150031
      WRITE (I02,80003) IVTNUM                                          02160031
      IF (ICZERO) 43050, 3061, 43050                                    02170031
43050 IF (IVCOMP-6131) 23050,13050,23050                                02180031
13050 IVPASS = IVPASS + 1                                               02190031
      WRITE (I02,80001) IVTNUM                                          02200031
      GO TO 3061                                                        02210031
23050 IVFAIL = IVFAIL + 1                                               02220031
      IVCORR = 6131                                                     02230031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02240031
 3061 CONTINUE                                                          02250031
      IVTNUM = 306                                                      02260031
C                                                                       02270031
C      ****  TEST 306  ****                                             02280031
C                                                                       02290031
      IF (ICZERO) 33060, 3060, 33060                                    02300031
 3060 CONTINUE                                                          02310031
      IVON03 = 11111                                                    02320031
      IVCOMP = 32333-11111-IVON03                                       02330031
      GO TO 43060                                                       02340031
33060 IVDELE = IVDELE + 1                                               02350031
      WRITE (I02,80003) IVTNUM                                          02360031
      IF (ICZERO) 43060, 3071, 43060                                    02370031
43060 IF (IVCOMP-10111) 23060,13060,23060                               02380031
13060 IVPASS = IVPASS + 1                                               02390031
      WRITE (I02,80001) IVTNUM                                          02400031
      GO TO 3071                                                        02410031
23060 IVFAIL = IVFAIL + 1                                               02420031
      IVCORR =10111                                                     02430031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02440031
 3071 CONTINUE                                                          02450031
      IVTNUM = 307                                                      02460031
C                                                                       02470031
C      ****  TEST 307  ****                                             02480031
C                                                                       02490031
      IF (ICZERO) 33070, 3070, 33070                                    02500031
 3070 CONTINUE                                                          02510031
      IVON01 = -3                                                       02520031
      IVCOMP = IVON01-2-4                                               02530031
      GO TO 43070                                                       02540031
33070 IVDELE = IVDELE + 1                                               02550031
      WRITE (I02,80003) IVTNUM                                          02560031
      IF (ICZERO) 43070, 3081, 43070                                    02570031
43070 IF (IVCOMP +9) 23070,13070,23070                                  02580031
13070 IVPASS = IVPASS + 1                                               02590031
      WRITE (I02,80001) IVTNUM                                          02600031
      GO TO 3081                                                        02610031
23070 IVFAIL = IVFAIL + 1                                               02620031
      IVCORR =-9                                                        02630031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02640031
 3081 CONTINUE                                                          02650031
      IVTNUM = 308                                                      02660031
C                                                                       02670031
C      ****  TEST 308  ****                                             02680031
C                                                                       02690031
      IF (ICZERO) 33080, 3080, 33080                                    02700031
 3080 CONTINUE                                                          02710031
      IVON02 =-9                                                        02720031
      IVCOMP =1-IVON02-4                                                02730031
      GO TO 43080                                                       02740031
33080 IVDELE = IVDELE + 1                                               02750031
      WRITE (I02,80003) IVTNUM                                          02760031
      IF (ICZERO) 43080, 3091, 43080                                    02770031
43080 IF (IVCOMP-6) 23080,13080,23080                                   02780031
13080 IVPASS = IVPASS + 1                                               02790031
      WRITE (I02,80001) IVTNUM                                          02800031
      GO TO 3091                                                        02810031
23080 IVFAIL = IVFAIL + 1                                               02820031
      IVCORR = 6                                                        02830031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02840031
 3091 CONTINUE                                                          02850031
      IVTNUM = 309                                                      02860031
C                                                                       02870031
C      ****  TEST 309  ****                                             02880031
C                                                                       02890031
      IF (ICZERO) 33090, 3090, 33090                                    02900031
 3090 CONTINUE                                                          02910031
      IVON03 = -8542                                                    02920031
      IVCOMP = 100-3-IVON03                                             02930031
      GO TO 43090                                                       02940031
33090 IVDELE = IVDELE + 1                                               02950031
      WRITE (I02,80003) IVTNUM                                          02960031
      IF (ICZERO) 43090, 3101, 43090                                    02970031
43090 IF (IVCOMP-8639) 23090,13090,23090                                02980031
13090 IVPASS = IVPASS + 1                                               02990031
      WRITE (I02,80001) IVTNUM                                          03000031
      GO TO 3101                                                        03010031
23090 IVFAIL = IVFAIL + 1                                               03020031
      IVCORR = 8639                                                     03030031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03040031
C                                                                       03050031
C     TEST 310 THROUGH TEST 319 CONTAIN 2 INTEGER CONSTANTS, AN INTEGER 03060031
C     VARIABLE AND OPERATOR - IN AN ARITHMETIC EXPRESSION.  PARENTHESES 03070031
C     ARE USED TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSION.          03080031
C                                                                       03090031
 3101 CONTINUE                                                          03100031
      IVTNUM = 310                                                      03110031
C                                                                       03120031
C      ****  TEST 310  ****                                             03130031
C                                                                       03140031
      IF (ICZERO) 33100, 3100, 33100                                    03150031
 3100 CONTINUE                                                          03160031
      IVON01 =9                                                         03170031
      IVCOMP = IVON01-(3-4)                                             03180031
      GO TO 43100                                                       03190031
33100 IVDELE = IVDELE + 1                                               03200031
      WRITE (I02,80003) IVTNUM                                          03210031
      IF (ICZERO) 43100, 3111, 43100                                    03220031
43100 IF (IVCOMP-10) 23100,13100,23100                                  03230031
13100 IVPASS = IVPASS + 1                                               03240031
      WRITE (I02,80001) IVTNUM                                          03250031
      GO TO 3111                                                        03260031
23100 IVFAIL = IVFAIL + 1                                               03270031
      IVCORR=10                                                         03280031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03290031
 3111 CONTINUE                                                          03300031
      IVTNUM = 311                                                      03310031
C                                                                       03320031
C      ****  TEST 311  ****                                             03330031
C                                                                       03340031
      IF (ICZERO) 33110, 3110, 33110                                    03350031
 3110 CONTINUE                                                          03360031
      IVON01=9                                                          03370031
      IVCOMP=(IVON01-3)-4                                               03380031
      GO TO 43110                                                       03390031
33110 IVDELE = IVDELE + 1                                               03400031
      WRITE (I02,80003) IVTNUM                                          03410031
      IF (ICZERO) 43110, 3121, 43110                                    03420031
43110 IF (IVCOMP-2) 23110,13110,23110                                   03430031
13110 IVPASS = IVPASS + 1                                               03440031
      WRITE (I02,80001) IVTNUM                                          03450031
      GO TO 3121                                                        03460031
23110 IVFAIL = IVFAIL + 1                                               03470031
      IVCORR =2                                                         03480031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03490031
 3121 CONTINUE                                                          03500031
      IVTNUM = 312                                                      03510031
C                                                                       03520031
C      ****  TEST 312  ****                                             03530031
C                                                                       03540031
      IF (ICZERO) 33120, 3120, 33120                                    03550031
 3120 CONTINUE                                                          03560031
      IVON02 = 3                                                        03570031
      IVCOMP = 9-(IVON02-4)                                             03580031
      GO TO 43120                                                       03590031
33120 IVDELE = IVDELE + 1                                               03600031
      WRITE (I02,80003) IVTNUM                                          03610031
      IF (ICZERO) 43120, 3131, 43120                                    03620031
43120 IF (IVCOMP-10) 23120,13120,23120                                  03630031
13120 IVPASS = IVPASS + 1                                               03640031
      WRITE (I02,80001) IVTNUM                                          03650031
      GO TO 3131                                                        03660031
23120 IVFAIL = IVFAIL + 1                                               03670031
      IVCORR = 10                                                       03680031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03690031
 3131 CONTINUE                                                          03700031
      IVTNUM = 313                                                      03710031
C                                                                       03720031
C      ****  TEST 313  ****                                             03730031
C                                                                       03740031
      IF (ICZERO) 33130, 3130, 33130                                    03750031
 3130 CONTINUE                                                          03760031
      IVON02 = 3                                                        03770031
      IVCOMP = (9-IVON02) -4                                            03780031
      GO TO 43130                                                       03790031
33130 IVDELE = IVDELE + 1                                               03800031
      WRITE (I02,80003) IVTNUM                                          03810031
      IF (ICZERO) 43130, 3141, 43130                                    03820031
43130 IF (IVCOMP-2) 23130,13130,23130                                   03830031
13130 IVPASS = IVPASS + 1                                               03840031
      WRITE (I02,80001) IVTNUM                                          03850031
      GO TO 3141                                                        03860031
23130 IVFAIL = IVFAIL + 1                                               03870031
      IVCORR = 2                                                        03880031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03890031
 3141 CONTINUE                                                          03900031
      IVTNUM = 314                                                      03910031
C                                                                       03920031
C      ****  TEST 314  ****                                             03930031
C                                                                       03940031
      IF (ICZERO) 33140, 3140, 33140                                    03950031
 3140 CONTINUE                                                          03960031
      IVON03 = 4                                                        03970031
      IVCOMP = 9 -(3-IVON03)                                            03980031
      GO TO 43140                                                       03990031
33140 IVDELE = IVDELE + 1                                               04000031
      WRITE (I02,80003) IVTNUM                                          04010031
      IF (ICZERO) 43140, 3151, 43140                                    04020031
43140 IF (IVCOMP-10) 23140,13140,23140                                  04030031
13140 IVPASS = IVPASS + 1                                               04040031
      WRITE (I02,80001) IVTNUM                                          04050031
      GO TO 3151                                                        04060031
23140 IVFAIL = IVFAIL + 1                                               04070031
      IVCORR = 10                                                       04080031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04090031
 3151 CONTINUE                                                          04100031
      IVTNUM = 315                                                      04110031
C                                                                       04120031
C      ****  TEST 315  ****                                             04130031
C                                                                       04140031
      IF (ICZERO) 33150, 3150, 33150                                    04150031
 3150 CONTINUE                                                          04160031
      IVON03 = 4                                                        04170031
      IVCOMP = (9-3)-IVON03                                             04180031
      GO TO 43150                                                       04190031
33150 IVDELE = IVDELE + 1                                               04200031
      WRITE (I02,80003) IVTNUM                                          04210031
      IF (ICZERO) 43150, 3161, 43150                                    04220031
43150 IF (IVCOMP-2) 23150,13150,23150                                   04230031
13150 IVPASS = IVPASS + 1                                               04240031
      WRITE (I02,80001) IVTNUM                                          04250031
      GO TO 3161                                                        04260031
23150 IVFAIL = IVFAIL + 1                                               04270031
      IVCORR = 2                                                        04280031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04290031
 3161 CONTINUE                                                          04300031
      IVTNUM = 316                                                      04310031
C                                                                       04320031
C      ****  TEST 316  ****                                             04330031
C                                                                       04340031
      IF (ICZERO) 33160, 3160, 33160                                    04350031
 3160 CONTINUE                                                          04360031
      IVON01 = -9                                                       04370031
      IVCOMP = (IVON01-3)-4                                             04380031
      GO TO 43160                                                       04390031
33160 IVDELE = IVDELE + 1                                               04400031
      WRITE (I02,80003) IVTNUM                                          04410031
      IF (ICZERO) 43160, 3171, 43160                                    04420031
43160 IF (IVCOMP +16) 23160,13160,23160                                 04430031
13160 IVPASS = IVPASS + 1                                               04440031
      WRITE (I02,80001) IVTNUM                                          04450031
      GO TO 3171                                                        04460031
23160 IVFAIL = IVFAIL + 1                                               04470031
      IVCORR = -16                                                      04480031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04490031
 3171 CONTINUE                                                          04500031
      IVTNUM = 317                                                      04510031
C                                                                       04520031
C      ****  TEST 317  ****                                             04530031
C                                                                       04540031
      IF (ICZERO) 33170, 3170, 33170                                    04550031
 3170 CONTINUE                                                          04560031
      IVON02 = -3                                                       04570031
      IVCOMP = 9-(IVON02-4)                                             04580031
      GO TO 43170                                                       04590031
33170 IVDELE = IVDELE + 1                                               04600031
      WRITE (I02,80003) IVTNUM                                          04610031
      IF (ICZERO) 43170, 3181, 43170                                    04620031
43170 IF (IVCOMP-16) 23170,13170,23170                                  04630031
13170 IVPASS = IVPASS + 1                                               04640031
      WRITE (I02,80001) IVTNUM                                          04650031
      GO TO 3181                                                        04660031
23170 IVFAIL = IVFAIL + 1                                               04670031
      IVCORR = 16                                                       04680031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04690031
 3181 CONTINUE                                                          04700031
      IVTNUM = 318                                                      04710031
C                                                                       04720031
C      ****  TEST 318  ****                                             04730031
C                                                                       04740031
      IF (ICZERO) 33180, 3180, 33180                                    04750031
 3180 CONTINUE                                                          04760031
      IVON03 = +4                                                       04770031
      IVCOMP = 9 - (3 - IVON03)                                         04780031
      GO TO 43180                                                       04790031
33180 IVDELE = IVDELE + 1                                               04800031
      WRITE (I02,80003) IVTNUM                                          04810031
      IF (ICZERO) 43180, 3191, 43180                                    04820031
43180 IF (IVCOMP - 10) 23180,13180,23180                                04830031
13180 IVPASS = IVPASS + 1                                               04840031
      WRITE (I02,80001) IVTNUM                                          04850031
      GO TO 3191                                                        04860031
23180 IVFAIL = IVFAIL + 1                                               04870031
      IVCORR= 10                                                        04880031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04890031
 3191 CONTINUE                                                          04900031
      IVTNUM = 319                                                      04910031
C                                                                       04920031
C      ****  TEST 319  ****                                             04930031
C                                                                       04940031
      IF (ICZERO) 33190, 3190, 33190                                    04950031
 3190 CONTINUE                                                          04960031
      IVON02 = 11111                                                    04970031
      IVCOMP = (32333-IVON02) -11111                                    04980031
      GO TO 43190                                                       04990031
33190 IVDELE = IVDELE + 1                                               05000031
      WRITE (I02,80003) IVTNUM                                          05010031
      IF (ICZERO) 43190, 3201, 43190                                    05020031
43190 IF (IVCOMP - 10111) 23190,13190,23190                             05030031
13190 IVPASS = IVPASS + 1                                               05040031
      WRITE (I02,80001) IVTNUM                                          05050031
      GO TO 3201                                                        05060031
23190 IVFAIL = IVFAIL + 1                                               05070031
      IVCORR = 10111                                                    05080031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05090031
C                                                                       05100031
C     TEST 320 THROUGH TEST 329 CONTAIN 2 INTEGER VARIABLES AND         05110031
C     OPERATOR - IN AN ARITHMETIC EXPRESSION.  THE INTEGER VARIABLES    05120031
C     CONTAIN POSITIVE AND NEGATIVE VALUES.                             05130031
C                                                                       05140031
 3201 CONTINUE                                                          05150031
      IVTNUM = 320                                                      05160031
C                                                                       05170031
C      ****  TEST 320  ****                                             05180031
C                                                                       05190031
      IF (ICZERO) 33200, 3200, 33200                                    05200031
 3200 CONTINUE                                                          05210031
      IVON01 = 3                                                        05220031
      IVON02 = 2                                                        05230031
      IVCOMP = IVON01 - IVON02                                          05240031
      GO TO 43200                                                       05250031
33200 IVDELE = IVDELE + 1                                               05260031
      WRITE (I02,80003) IVTNUM                                          05270031
      IF (ICZERO) 43200, 3211, 43200                                    05280031
43200 IF (IVCOMP - 1) 23200,13200,23200                                 05290031
13200 IVPASS = IVPASS + 1                                               05300031
      WRITE (I02,80001) IVTNUM                                          05310031
      GO TO 3211                                                        05320031
23200 IVFAIL = IVFAIL + 1                                               05330031
      IVCORR = 1                                                        05340031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05350031
 3211 CONTINUE                                                          05360031
      IVTNUM = 321                                                      05370031
C                                                                       05380031
C      ****  TEST 321  ****                                             05390031
C                                                                       05400031
      IF (ICZERO) 33210, 3210, 33210                                    05410031
 3210 CONTINUE                                                          05420031
      IVON01 =2                                                         05430031
      IVON02 =3                                                         05440031
      IVCOMP = IVON01 - IVON02                                          05450031
      GO TO 43210                                                       05460031
33210 IVDELE = IVDELE + 1                                               05470031
      WRITE (I02,80003) IVTNUM                                          05480031
      IF (ICZERO) 43210, 3221, 43210                                    05490031
43210 IF (IVCOMP +1) 23210,13210,23210                                  05500031
13210 IVPASS = IVPASS + 1                                               05510031
      WRITE (I02,80001) IVTNUM                                          05520031
      GO TO 3221                                                        05530031
23210 IVFAIL = IVFAIL + 1                                               05540031
      IVCORR = -1                                                       05550031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05560031
 3221 CONTINUE                                                          05570031
      IVTNUM = 322                                                      05580031
C                                                                       05590031
C      ****  TEST 322  ****                                             05600031
C                                                                       05610031
      IF (ICZERO) 33220, 3220, 33220                                    05620031
 3220 CONTINUE                                                          05630031
      IVON01 = -2                                                       05640031
      IVON02 =  3                                                       05650031
      IVCOMP = IVON01 - IVON02                                          05660031
      GO TO 43220                                                       05670031
33220 IVDELE = IVDELE + 1                                               05680031
      WRITE (I02,80003) IVTNUM                                          05690031
      IF (ICZERO) 43220, 3231, 43220                                    05700031
43220 IF (IVCOMP +5) 23220,13220,23220                                  05710031
13220 IVPASS = IVPASS + 1                                               05720031
      WRITE (I02,80001) IVTNUM                                          05730031
      GO TO 3231                                                        05740031
23220 IVFAIL = IVFAIL + 1                                               05750031
      IVCORR =-5                                                        05760031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05770031
 3231 CONTINUE                                                          05780031
      IVTNUM = 323                                                      05790031
C                                                                       05800031
C      ****  TEST 323  ****                                             05810031
C                                                                       05820031
      IF (ICZERO) 33230, 3230, 33230                                    05830031
 3230 CONTINUE                                                          05840031
      IVON01 = -2                                                       05850031
      IVON02 = -3                                                       05860031
      IVCOMP = IVON01 - IVON02                                          05870031
      GO TO 43230                                                       05880031
33230 IVDELE = IVDELE + 1                                               05890031
      WRITE (I02,80003) IVTNUM                                          05900031
      IF (ICZERO) 43230, 3241, 43230                                    05910031
43230 IF (IVCOMP -1) 23230,13230,23230                                  05920031
13230 IVPASS = IVPASS + 1                                               05930031
      WRITE (I02,80001) IVTNUM                                          05940031
      GO TO 3241                                                        05950031
23230 IVFAIL = IVFAIL + 1                                               05960031
      IVCORR = 1                                                        05970031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05980031
 3241 CONTINUE                                                          05990031
      IVTNUM = 324                                                      06000031
C                                                                       06010031
C      ****  TEST 324  ****                                             06020031
C                                                                       06030031
      IF (ICZERO) 33240, 3240, 33240                                    06040031
 3240 CONTINUE                                                          06050031
      IVON01 = 51                                                       06060031
      IVON02 = 52                                                       06070031
      IVCOMP = IVON01 - IVON02                                          06080031
      GO TO 43240                                                       06090031
33240 IVDELE = IVDELE + 1                                               06100031
      WRITE (I02,80003) IVTNUM                                          06110031
      IF (ICZERO) 43240, 3251, 43240                                    06120031
43240 IF (IVCOMP + 1) 23240,13240,23240                                 06130031
13240 IVPASS = IVPASS + 1                                               06140031
      WRITE (I02,80001) IVTNUM                                          06150031
      GO TO 3251                                                        06160031
23240 IVFAIL = IVFAIL + 1                                               06170031
      IVCORR = -1                                                       06180031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06190031
 3251 CONTINUE                                                          06200031
      IVTNUM = 325                                                      06210031
C                                                                       06220031
C      ****  TEST 325  ****                                             06230031
C                                                                       06240031
      IF (ICZERO) 33250, 3250, 33250                                    06250031
 3250 CONTINUE                                                          06260031
      IVON01 = 676                                                      06270031
      IVON02 =-189                                                      06280031
      IVCOMP = IVON01 - IVON02                                          06290031
      GO TO 43250                                                       06300031
33250 IVDELE = IVDELE + 1                                               06310031
      WRITE (I02,80003) IVTNUM                                          06320031
      IF (ICZERO) 43250, 3261, 43250                                    06330031
43250 IF (IVCOMP - 865) 23250,13250,23250                               06340031
13250 IVPASS = IVPASS + 1                                               06350031
      WRITE (I02,80001) IVTNUM                                          06360031
      GO TO 3261                                                        06370031
23250 IVFAIL = IVFAIL + 1                                               06380031
      IVCORR = 865                                                      06390031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06400031
 3261 CONTINUE                                                          06410031
      IVTNUM = 326                                                      06420031
C                                                                       06430031
C      ****  TEST 326  ****                                             06440031
C                                                                       06450031
      IF (ICZERO) 33260, 3260, 33260                                    06460031
 3260 CONTINUE                                                          06470031
      IVON01 = 1358                                                     06480031
      IVON02 = -8001                                                    06490031
      IVCOMP = IVON01 - IVON02                                          06500031
      GO TO 43260                                                       06510031
33260 IVDELE = IVDELE + 1                                               06520031
      WRITE (I02,80003) IVTNUM                                          06530031
      IF (ICZERO) 43260, 3271, 43260                                    06540031
43260 IF (IVCOMP - 9359) 23260,13260,23260                              06550031
13260 IVPASS = IVPASS + 1                                               06560031
      WRITE (I02,80001) IVTNUM                                          06570031
      GO TO 3271                                                        06580031
23260 IVFAIL = IVFAIL + 1                                               06590031
      IVCORR = 9359                                                     06600031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06610031
 3271 CONTINUE                                                          06620031
      IVTNUM = 327                                                      06630031
C                                                                       06640031
C      ****  TEST 327  ****                                             06650031
C                                                                       06660031
      IF (ICZERO) 33270, 3270, 33270                                    06670031
 3270 CONTINUE                                                          06680031
      IVON01 =-16383                                                    06690031
      IVON02 = 16383                                                    06700031
      IVCOMP = IVON01 - IVON02                                          06710031
      GO TO 43270                                                       06720031
33270 IVDELE = IVDELE + 1                                               06730031
      WRITE (I02,80003) IVTNUM                                          06740031
      IF (ICZERO) 43270, 3281, 43270                                    06750031
43270 IF (IVCOMP + 32766) 23270,13270,23270                             06760031
13270 IVPASS = IVPASS + 1                                               06770031
      WRITE (I02,80001) IVTNUM                                          06780031
      GO TO 3281                                                        06790031
23270 IVFAIL = IVFAIL + 1                                               06800031
      IVCORR = -32766                                                   06810031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06820031
 3281 CONTINUE                                                          06830031
      IVTNUM = 328                                                      06840031
C                                                                       06850031
C      ****  TEST 328  ****                                             06860031
C                                                                       06870031
      IF (ICZERO) 33280, 3280, 33280                                    06880031
 3280 CONTINUE                                                          06890031
      IVON01 = 9876                                                     06900031
      IVON02 = 189                                                      06910031
      IVCOMP = IVON01 - IVON02                                          06920031
      GO TO 43280                                                       06930031
33280 IVDELE = IVDELE + 1                                               06940031
      WRITE (I02,80003) IVTNUM                                          06950031
      IF (ICZERO) 43280, 3291, 43280                                    06960031
43280 IF (IVCOMP - 9687) 23280,13280,23280                              06970031
13280 IVPASS = IVPASS + 1                                               06980031
      WRITE (I02,80001) IVTNUM                                          06990031
      GO TO 3291                                                        07000031
23280 IVFAIL = IVFAIL + 1                                               07010031
      IVCORR = 9687                                                     07020031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07030031
 3291 CONTINUE                                                          07040031
      IVTNUM = 329                                                      07050031
C                                                                       07060031
C      ****  TEST 329  ****                                             07070031
C                                                                       07080031
      IF (ICZERO) 33290, 3290, 33290                                    07090031
 3290 CONTINUE                                                          07100031
      IVON01 = 11112                                                    07110031
      IVON02 = 11112                                                    07120031
      IVCOMP = IVON01 - IVON02                                          07130031
      GO TO 43290                                                       07140031
33290 IVDELE = IVDELE + 1                                               07150031
      WRITE (I02,80003) IVTNUM                                          07160031
      IF (ICZERO) 43290, 3301, 43290                                    07170031
43290 IF (IVCOMP) 23290,13290,23290                                     07180031
13290 IVPASS = IVPASS + 1                                               07190031
      WRITE (I02,80001) IVTNUM                                          07200031
      GO TO 3301                                                        07210031
23290 IVFAIL = IVFAIL + 1                                               07220031
      IVCORR = 0                                                        07230031
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07240031
C                                                                       07250031
C      ****  END OF TESTS  ****                                         07260031
 3301 CONTINUE                                                          07270031
C                                                                       07280031
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07290031
99999 CONTINUE                                                          07300031
      WRITE (I02,90002)                                                 07310031
      WRITE (I02,90006)                                                 07320031
      WRITE (I02,90002)                                                 07330031
      WRITE (I02,90002)                                                 07340031
      WRITE (I02,90007)                                                 07350031
      WRITE (I02,90002)                                                 07360031
      WRITE (I02,90008)  IVFAIL                                         07370031
      WRITE (I02,90009) IVPASS                                          07380031
      WRITE (I02,90010) IVDELE                                          07390031
C                                                                       07400031
C                                                                       07410031
C     TERMINATE ROUTINE EXECUTION                                       07420031
      STOP                                                              07430031
C                                                                       07440031
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07450031
90000 FORMAT (1H1)                                                      07460031
90002 FORMAT (1H )                                                      07470031
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07480031
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07490031
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07500031
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07510031
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07520031
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07530031
C                                                                       07540031
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07550031
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07560031
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07570031
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07580031
C                                                                       07590031
C     FORMAT STATEMENTS FOR TEST RESULTS                                07600031
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07610031
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07620031
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07630031
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07640031
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07650031
C                                                                       07660031
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM031)                          07670031
      END                                                               07680031
