C     COMMENT SECTION                                                   00010032
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020032
C     FM032                                                             00030032
C                                                                       00040032
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050032
C     FORM                                                              00060032
C                INTEGER VARIABLE = ARITHMETIC EXPRESSION               00070032
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00080032
C     OPERATOR -, INTEGER CONSTANTS AND INTEGER VARIABLES.  SOME OF THE 00090032
C     TESTS USE PARENTHESES TO GROUP ELEMENTS IN AN ARITHMETIC          00100032
C     EXPRESSION.                                                       00110032
C                                                                       00120032
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00130032
C         (1)  INTEGER VAR.= INT. VAR. - INT.VAR.-INT.CON               00140032
C                          = INT. VAR. - INT.CON.-INT.VAR               00150032
C                          = INT. CON. - INT.VAR -INT.VAR.              00160032
C         (2)  SAME FORMS AS (1) BUT WITH PARENTHESES TO GROUP ELEMENTS 00170032
C              IN ARITHMETIC EXPRESSION.                                00180032
C                                                                       00190032
C      REFERENCES                                                       00200032
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00210032
C              X3.9-1978                                                00220032
C                                                                       00230032
C        SECTION 4.3, INTEGER TYPE                                      00240032
C        SECTION 4.3.1, INTEGER CONSTANT                                00250032
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00260032
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00270032
C                                                                       00280032
C                                                                       00290032
C      **********************************************************       00300032
C                                                                       00310032
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00320032
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00330032
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00340032
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00350032
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00360032
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00370032
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00380032
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00390032
C     OF EXECUTING THESE TESTS.                                         00400032
C                                                                       00410032
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00420032
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00430032
C                                                                       00440032
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00450032
C                                                                       00460032
C                  DEPARTMENT OF THE NAVY                               00470032
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00480032
C                  WASHINGTON, D.C.  20376                              00490032
C                                                                       00500032
C      **********************************************************       00510032
C                                                                       00520032
C                                                                       00530032
C                                                                       00540032
C     INITIALIZATION SECTION                                            00550032
C                                                                       00560032
C     INITIALIZE CONSTANTS                                              00570032
C      **************                                                   00580032
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00590032
      I01 = 5                                                           00600032
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00610032
      I02 = 6                                                           00620032
C     SYSTEM ENVIRONMENT SECTION                                        00630032
C                                                                       00640032
      I01 = 5                                                           00650032
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00660032
C     (UNIT NUMBER FOR CARD READER).                                    00670032
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00680032
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00690032
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00700032
C                                                                       00710032
      I02 = 6                                                           00720032
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00730032
C     (UNIT NUMBER FOR PRINTER).                                        00740032
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00750032
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760032
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00770032
C                                                                       00780032
      IVPASS=0                                                          00790032
      IVFAIL=0                                                          00800032
      IVDELE=0                                                          00810032
      ICZERO=0                                                          00820032
C                                                                       00830032
C     WRITE PAGE HEADERS                                                00840032
      WRITE (I02,90000)                                                 00850032
      WRITE (I02,90001)                                                 00860032
      WRITE (I02,90002)                                                 00870032
      WRITE (I02, 90002)                                                00880032
      WRITE (I02,90003)                                                 00890032
      WRITE (I02,90002)                                                 00900032
      WRITE (I02,90004)                                                 00910032
      WRITE (I02,90002)                                                 00920032
      WRITE (I02,90011)                                                 00930032
      WRITE (I02,90002)                                                 00940032
      WRITE (I02,90002)                                                 00950032
      WRITE (I02,90005)                                                 00960032
      WRITE (I02,90006)                                                 00970032
      WRITE (I02,90002)                                                 00980032
C     TEST SECTION                                                      00990032
C                                                                       01000032
C         ARITHMETIC ASSIGNMENT STATEMENT                               01010032
C                                                                       01020032
C     TEST 330 THROUGH TEST 347 CONTAIN TWO INTEGER VARIABLES, AN       01030032
C     INTEGER CONSTANT AND OPERATOR - IN AN ARITHMETIC EXPRESSION.  THE 01040032
C     INTEGER VARIABLES CONTAIN POSITIVE AND NEGATIVE VALUES.           01050032
C                                                                       01060032
C     TEST 330 THROUGH TEST 337     IV = IV -IV -IC                     01070032
C                                                                       01080032
 3301 CONTINUE                                                          01090032
      IVTNUM = 330                                                      01100032
C                                                                       01110032
C      ****  TEST 330  ****                                             01120032
C                                                                       01130032
      IF (ICZERO) 33300, 3300, 33300                                    01140032
 3300 CONTINUE                                                          01150032
      IVON01 =9                                                         01160032
      IVON02 =4                                                         01170032
      IVCOMP = IVON01-IVON02-2                                          01180032
      GO TO 43300                                                       01190032
33300 IVDELE = IVDELE + 1                                               01200032
      WRITE (I02,80003) IVTNUM                                          01210032
      IF (ICZERO) 43300, 3311, 43300                                    01220032
43300 IF (IVCOMP-3) 23300,13300,23300                                   01230032
13300 IVPASS = IVPASS + 1                                               01240032
      WRITE (I02,80001) IVTNUM                                          01250032
      GO TO 3311                                                        01260032
23300 IVFAIL = IVFAIL + 1                                               01270032
      IVCORR= 3                                                         01280032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01290032
 3311 CONTINUE                                                          01300032
      IVTNUM = 331                                                      01310032
C                                                                       01320032
C      ****  TEST 331  ****                                             01330032
C                                                                       01340032
      IF (ICZERO) 33310, 3310, 33310                                    01350032
 3310 CONTINUE                                                          01360032
      IVON01 =-9                                                        01370032
      IVON02 = 4                                                        01380032
      IVCOMP = IVON01-IVON02-2                                          01390032
      GO TO 43310                                                       01400032
33310 IVDELE = IVDELE + 1                                               01410032
      WRITE (I02,80003) IVTNUM                                          01420032
      IF (ICZERO) 43310, 3321, 43310                                    01430032
43310 IF (IVCOMP +15) 23310,13310,23310                                 01440032
13310 IVPASS = IVPASS + 1                                               01450032
      WRITE (I02,80001) IVTNUM                                          01460032
      GO TO 3321                                                        01470032
23310 IVFAIL = IVFAIL + 1                                               01480032
      IVCORR = -15                                                      01490032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01500032
 3321 CONTINUE                                                          01510032
      IVTNUM = 332                                                      01520032
C                                                                       01530032
C      ****  TEST 332  ****                                             01540032
C                                                                       01550032
      IF (ICZERO) 33320, 3320, 33320                                    01560032
 3320 CONTINUE                                                          01570032
      IVON01 =9                                                         01580032
      IVON02 =-4                                                        01590032
      IVCOMP =IVON01-IVON02-2                                           01600032
      GO TO 43320                                                       01610032
33320 IVDELE = IVDELE + 1                                               01620032
      WRITE (I02,80003) IVTNUM                                          01630032
      IF (ICZERO) 43320, 3331, 43320                                    01640032
43320 IF (IVCOMP-11) 23320,13320,23320                                  01650032
13320 IVPASS = IVPASS + 1                                               01660032
      WRITE (I02,80001) IVTNUM                                          01670032
      GO TO 3331                                                        01680032
23320 IVFAIL = IVFAIL + 1                                               01690032
      IVCORR = 11                                                       01700032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01710032
 3331 CONTINUE                                                          01720032
      IVTNUM = 333                                                      01730032
C                                                                       01740032
C      ****  TEST 333  ****                                             01750032
C                                                                       01760032
      IF (ICZERO) 33330, 3330, 33330                                    01770032
 3330 CONTINUE                                                          01780032
      IVON01 =57                                                        01790032
      IVON02 =25                                                        01800032
      IVCOMP=IVON01-IVON02-22                                           01810032
      GO TO 43330                                                       01820032
33330 IVDELE = IVDELE + 1                                               01830032
      WRITE (I02,80003) IVTNUM                                          01840032
      IF (ICZERO) 43330, 3341, 43330                                    01850032
43330 IF (IVCOMP -10) 23330,13330,23330                                 01860032
13330 IVPASS = IVPASS + 1                                               01870032
      WRITE (I02,80001) IVTNUM                                          01880032
      GO TO 3341                                                        01890032
23330 IVFAIL = IVFAIL + 1                                               01900032
      IVCORR = 10                                                       01910032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01920032
 3341 CONTINUE                                                          01930032
      IVTNUM = 334                                                      01940032
C                                                                       01950032
C      ****  TEST 334  ****                                             01960032
C                                                                       01970032
      IF (ICZERO) 33340, 3340, 33340                                    01980032
 3340 CONTINUE                                                          01990032
      IVON01 = 101                                                      02000032
      IVON02 = 683                                                      02010032
      IVCOMP = IVON01 - IVON02 - 156                                    02020032
      GO TO 43340                                                       02030032
33340 IVDELE = IVDELE + 1                                               02040032
      WRITE (I02,80003) IVTNUM                                          02050032
      IF (ICZERO) 43340, 3351, 43340                                    02060032
43340 IF (IVCOMP +738) 23340,13340,23340                                02070032
13340 IVPASS = IVPASS + 1                                               02080032
      WRITE (I02,80001) IVTNUM                                          02090032
      GO TO 3351                                                        02100032
23340 IVFAIL = IVFAIL + 1                                               02110032
      IVCORR = -738                                                     02120032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02130032
 3351 CONTINUE                                                          02140032
      IVTNUM = 335                                                      02150032
C                                                                       02160032
C      ****  TEST 335  ****                                             02170032
C                                                                       02180032
      IF (ICZERO) 33350, 3350, 33350                                    02190032
 3350 CONTINUE                                                          02200032
      IVON01=8542                                                       02210032
      IVON02=1122                                                       02220032
      IVCOMP=IVON01-IVON02-1289                                         02230032
      GO TO 43350                                                       02240032
33350 IVDELE = IVDELE + 1                                               02250032
      WRITE (I02,80003) IVTNUM                                          02260032
      IF (ICZERO) 43350, 3361, 43350                                    02270032
43350 IF (IVCOMP -6131) 23350,13350,23350                               02280032
13350 IVPASS = IVPASS + 1                                               02290032
      WRITE (I02,80001) IVTNUM                                          02300032
      GO TO 3361                                                        02310032
23350 IVFAIL = IVFAIL + 1                                               02320032
      IVCORR = 6131                                                     02330032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02340032
 3361 CONTINUE                                                          02350032
      IVTNUM = 336                                                      02360032
C                                                                       02370032
C      ****  TEST 336  ****                                             02380032
C                                                                       02390032
      IF (ICZERO) 33360, 3360, 33360                                    02400032
 3360 CONTINUE                                                          02410032
      IVON01 = 31333                                                    02420032
      IVON02 = 11111                                                    02430032
      IVCOMP = IVON01-IVON02-10111                                      02440032
      GO TO 43360                                                       02450032
33360 IVDELE = IVDELE + 1                                               02460032
      WRITE (I02,80003) IVTNUM                                          02470032
      IF (ICZERO) 43360, 3371, 43360                                    02480032
43360 IF (IVCOMP -10111) 23360,13360,23360                              02490032
13360 IVPASS = IVPASS + 1                                               02500032
      WRITE (I02,80001) IVTNUM                                          02510032
      GO TO 3371                                                        02520032
23360 IVFAIL = IVFAIL + 1                                               02530032
      IVCORR = 10111                                                    02540032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02550032
 3371 CONTINUE                                                          02560032
      IVTNUM = 337                                                      02570032
C                                                                       02580032
C      ****  TEST 337  ****                                             02590032
C                                                                       02600032
      IF (ICZERO) 33370, 3370, 33370                                    02610032
 3370 CONTINUE                                                          02620032
      IVON01 = -31444                                                   02630032
      IVON02 = +1001                                                    02640032
      IVCOMP = IVON01-IVON02-300                                        02650032
      GO TO 43370                                                       02660032
33370 IVDELE = IVDELE + 1                                               02670032
      WRITE (I02,80003) IVTNUM                                          02680032
      IF (ICZERO) 43370, 3381, 43370                                    02690032
43370 IF (IVCOMP +32745) 23370,13370,23370                              02700032
13370 IVPASS = IVPASS + 1                                               02710032
      WRITE (I02,80001) IVTNUM                                          02720032
      GO TO 3381                                                        02730032
23370 IVFAIL = IVFAIL + 1                                               02740032
      IVCORR = -32745                                                   02750032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02760032
C                                                                       02770032
C     TEST 338 THROUGH TEST 343           IV=IV-IC-IV                   02780032
C                                                                       02790032
 3381 CONTINUE                                                          02800032
      IVTNUM = 338                                                      02810032
C                                                                       02820032
C      ****  TEST 338  ****                                             02830032
C                                                                       02840032
      IF (ICZERO) 33380, 3380, 33380                                    02850032
 3380 CONTINUE                                                          02860032
      IVON01 =9                                                         02870032
      IVON03 =2                                                         02880032
      IVCOMP = IVON01-4-IVON03                                          02890032
      GO TO 43380                                                       02900032
33380 IVDELE = IVDELE + 1                                               02910032
      WRITE (I02,80003) IVTNUM                                          02920032
      IF (ICZERO) 43380, 3391, 43380                                    02930032
43380 IF (IVCOMP -3) 23380,13380,23380                                  02940032
13380 IVPASS = IVPASS + 1                                               02950032
      WRITE (I02,80001) IVTNUM                                          02960032
      GO TO 3391                                                        02970032
23380 IVFAIL = IVFAIL + 1                                               02980032
      IVCORR = 3                                                        02990032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03000032
 3391 CONTINUE                                                          03010032
      IVTNUM = 339                                                      03020032
C                                                                       03030032
C      ****  TEST 339  ****                                             03040032
C                                                                       03050032
      IF (ICZERO) 33390, 3390, 33390                                    03060032
 3390 CONTINUE                                                          03070032
      IVON01 = -9                                                       03080032
      IVON03 =  2                                                       03090032
      IVCOMP = IVON01-4-IVON03                                          03100032
      GO TO 43390                                                       03110032
33390 IVDELE = IVDELE + 1                                               03120032
      WRITE (I02,80003) IVTNUM                                          03130032
      IF (ICZERO) 43390, 3401, 43390                                    03140032
43390 IF (IVCOMP+15) 23390,13390,23390                                  03150032
13390 IVPASS = IVPASS + 1                                               03160032
      WRITE (I02,80001) IVTNUM                                          03170032
      GO TO 3401                                                        03180032
23390 IVFAIL = IVFAIL + 1                                               03190032
      IVCORR = -15                                                      03200032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03210032
 3401 CONTINUE                                                          03220032
      IVTNUM = 340                                                      03230032
C                                                                       03240032
C      ****  TEST 340  ****                                             03250032
C                                                                       03260032
      IF (ICZERO) 33400, 3400, 33400                                    03270032
 3400 CONTINUE                                                          03280032
      IVON01 = 9                                                        03290032
      IVON03 =-2                                                        03300032
      IVCOMP =IVON01-4-IVON03                                           03310032
      GO TO 43400                                                       03320032
33400 IVDELE = IVDELE + 1                                               03330032
      WRITE (I02,80003) IVTNUM                                          03340032
      IF (ICZERO) 43400, 3411, 43400                                    03350032
43400 IF (IVCOMP-7) 23400,13400,23400                                   03360032
13400 IVPASS = IVPASS + 1                                               03370032
      WRITE (I02,80001) IVTNUM                                          03380032
      GO TO 3411                                                        03390032
23400 IVFAIL = IVFAIL + 1                                               03400032
      IVCORR=7                                                          03410032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03420032
 3411 CONTINUE                                                          03430032
      IVTNUM = 341                                                      03440032
C                                                                       03450032
C      ****  TEST 341  ****                                             03460032
C                                                                       03470032
      IF (ICZERO) 33410, 3410, 33410                                    03480032
 3410 CONTINUE                                                          03490032
      IVON01=-57                                                        03500032
      IVON03=22                                                         03510032
      IVCOMP=IVON01-25-IVON03                                           03520032
      GO TO 43410                                                       03530032
33410 IVDELE = IVDELE + 1                                               03540032
      WRITE (I02,80003) IVTNUM                                          03550032
      IF (ICZERO) 43410, 3421, 43410                                    03560032
43410 IF (IVCOMP+104) 23410,13410,23410                                 03570032
13410 IVPASS = IVPASS + 1                                               03580032
      WRITE (I02,80001) IVTNUM                                          03590032
      GO TO 3421                                                        03600032
23410 IVFAIL = IVFAIL + 1                                               03610032
      IVCORR = -104                                                     03620032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03630032
 3421 CONTINUE                                                          03640032
      IVTNUM = 342                                                      03650032
C                                                                       03660032
C      ****  TEST 342  ****                                             03670032
C                                                                       03680032
      IF (ICZERO) 33420, 3420, 33420                                    03690032
 3420 CONTINUE                                                          03700032
      IVON01=8542                                                       03710032
      IVON03=3                                                          03720032
      IVCOMP=IVON01-125-IVON03                                          03730032
      GO TO 43420                                                       03740032
33420 IVDELE = IVDELE + 1                                               03750032
      WRITE (I02,80003) IVTNUM                                          03760032
      IF (ICZERO) 43420, 3431, 43420                                    03770032
43420 IF (IVCOMP-8414) 23420,13420,23420                                03780032
13420 IVPASS = IVPASS + 1                                               03790032
      WRITE (I02,80001) IVTNUM                                          03800032
      GO TO 3431                                                        03810032
23420 IVFAIL = IVFAIL + 1                                               03820032
      IVCORR = 8414                                                     03830032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03840032
 3431 CONTINUE                                                          03850032
      IVTNUM = 343                                                      03860032
C                                                                       03870032
C      ****  TEST 343  ****                                             03880032
C                                                                       03890032
      IF (ICZERO) 33430, 3430, 33430                                    03900032
 3430 CONTINUE                                                          03910032
      IVON01 = -32111                                                   03920032
      IVON03 = -111                                                     03930032
      IVCOMP = IVON01-111-IVON03                                        03940032
      GO TO 43430                                                       03950032
33430 IVDELE = IVDELE + 1                                               03960032
      WRITE (I02,80003) IVTNUM                                          03970032
      IF (ICZERO) 43430, 3441, 43430                                    03980032
43430 IF (IVCOMP + 32111) 23430,13430,23430                             03990032
13430 IVPASS = IVPASS + 1                                               04000032
      WRITE (I02,80001) IVTNUM                                          04010032
      GO TO 3441                                                        04020032
23430 IVFAIL = IVFAIL + 1                                               04030032
      IVCORR = -32111                                                   04040032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04050032
C                                                                       04060032
C     TEST 344 THROUGH TEST 347      IV=IC-IV-IV                        04070032
C                                                                       04080032
 3441 CONTINUE                                                          04090032
      IVTNUM = 344                                                      04100032
C                                                                       04110032
C      ****  TEST 344  ****                                             04120032
C                                                                       04130032
      IF (ICZERO) 33440, 3440, 33440                                    04140032
 3440 CONTINUE                                                          04150032
      IVON02=4                                                          04160032
      IVON03=2                                                          04170032
      IVCOMP=9-IVON02-IVON03                                            04180032
      GO TO 43440                                                       04190032
33440 IVDELE = IVDELE + 1                                               04200032
      WRITE (I02,80003) IVTNUM                                          04210032
      IF (ICZERO) 43440, 3451, 43440                                    04220032
43440 IF (IVCOMP -3) 23440,13440,23440                                  04230032
13440 IVPASS = IVPASS + 1                                               04240032
      WRITE (I02,80001) IVTNUM                                          04250032
      GO TO 3451                                                        04260032
23440 IVFAIL = IVFAIL + 1                                               04270032
      IVCORR = 3                                                        04280032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04290032
 3451 CONTINUE                                                          04300032
      IVTNUM = 345                                                      04310032
C                                                                       04320032
C      ****  TEST 345  ****                                             04330032
C                                                                       04340032
      IF (ICZERO) 33450, 3450, 33450                                    04350032
 3450 CONTINUE                                                          04360032
      IVON02=-4                                                         04370032
      IVON03= 2                                                         04380032
      IVCOMP= 9-IVON02-IVON03                                           04390032
      GO TO 43450                                                       04400032
33450 IVDELE = IVDELE + 1                                               04410032
      WRITE (I02,80003) IVTNUM                                          04420032
      IF (ICZERO) 43450, 3461, 43450                                    04430032
43450 IF (IVCOMP -11) 23450,13450,23450                                 04440032
13450 IVPASS = IVPASS + 1                                               04450032
      WRITE (I02,80001) IVTNUM                                          04460032
      GO TO 3461                                                        04470032
23450 IVFAIL = IVFAIL + 1                                               04480032
      IVCORR =11                                                        04490032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04500032
 3461 CONTINUE                                                          04510032
      IVTNUM = 346                                                      04520032
C                                                                       04530032
C      ****  TEST 346  ****                                             04540032
C                                                                       04550032
      IF (ICZERO) 33460, 3460, 33460                                    04560032
 3460 CONTINUE                                                          04570032
      IVON02 = 683                                                      04580032
      IVON03 = 156                                                      04590032
      IVCOMP = 101 -IVON02-IVON03                                       04600032
      GO TO 43460                                                       04610032
33460 IVDELE = IVDELE + 1                                               04620032
      WRITE (I02,80003) IVTNUM                                          04630032
      IF (ICZERO) 43460, 3471, 43460                                    04640032
43460 IF (IVCOMP +738) 23460,13460,23460                                04650032
13460 IVPASS = IVPASS + 1                                               04660032
      WRITE (I02,80001) IVTNUM                                          04670032
      GO TO 3471                                                        04680032
23460 IVFAIL = IVFAIL + 1                                               04690032
      IVCORR = -738                                                     04700032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04710032
 3471 CONTINUE                                                          04720032
      IVTNUM = 347                                                      04730032
C                                                                       04740032
C      ****  TEST 347  ****                                             04750032
C                                                                       04760032
      IF (ICZERO) 33470, 3470, 33470                                    04770032
 3470 CONTINUE                                                          04780032
      IVON02 = 15687                                                    04790032
      IVON03 =  387                                                     04800032
      IVCOMP = 8542-IVON02-IVON03                                       04810032
      GO TO 43470                                                       04820032
33470 IVDELE = IVDELE + 1                                               04830032
      WRITE (I02,80003) IVTNUM                                          04840032
      IF (ICZERO) 43470, 3481, 43470                                    04850032
43470 IF (IVCOMP + 7532) 23470,13470,23470                              04860032
13470 IVPASS = IVPASS + 1                                               04870032
      WRITE (I02,80001) IVTNUM                                          04880032
      GO TO 3481                                                        04890032
23470 IVFAIL = IVFAIL + 1                                               04900032
      IVCORR = -7532                                                    04910032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04920032
C                                                                       04930032
C     TEST 348 THROUGH TEST 359 CONTAIN TWO INTEGER VARIABLES, AN       04940032
C     INTEGER CONSTANT AND OPERATOR - IN AN ARITHMETIC EXPRESSION.      04950032
C     PARENTHESES ARE USED TO GROUP THE ELEMENTS IN THE ARITHMETIC      04960032
C     EXPRESSION.  THE INTEGER VARIABLES CONTAIN POSITIVE AND NEGATIVE  04970032
C     VALUES.                                                           04980032
C                                                                       04990032
 3481 CONTINUE                                                          05000032
      IVTNUM = 348                                                      05010032
C                                                                       05020032
C      ****  TEST 348  ****                                             05030032
C                                                                       05040032
      IF (ICZERO) 33480, 3480, 33480                                    05050032
 3480 CONTINUE                                                          05060032
      IVON01= 9                                                         05070032
      IVON02= 4                                                         05080032
      IVCOMP=(IVON01-IVON02)-2                                          05090032
      GO TO 43480                                                       05100032
33480 IVDELE = IVDELE + 1                                               05110032
      WRITE (I02,80003) IVTNUM                                          05120032
      IF (ICZERO) 43480, 3491, 43480                                    05130032
43480 IF (IVCOMP - 3) 23480,13480,23480                                 05140032
13480 IVPASS = IVPASS + 1                                               05150032
      WRITE (I02,80001) IVTNUM                                          05160032
      GO TO 3491                                                        05170032
23480 IVFAIL = IVFAIL + 1                                               05180032
      IVCORR = 3                                                        05190032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05200032
 3491 CONTINUE                                                          05210032
      IVTNUM = 349                                                      05220032
C                                                                       05230032
C      ****  TEST 349  ****                                             05240032
C                                                                       05250032
      IF (ICZERO) 33490, 3490, 33490                                    05260032
 3490 CONTINUE                                                          05270032
      IVON01=9                                                          05280032
      IVON02=4                                                          05290032
      IVCOMP=IVON01-(IVON02-2)                                          05300032
      GO TO 43490                                                       05310032
33490 IVDELE = IVDELE + 1                                               05320032
      WRITE (I02,80003) IVTNUM                                          05330032
      IF (ICZERO) 43490, 3501, 43490                                    05340032
43490 IF (IVCOMP -7) 23490,13490,23490                                  05350032
13490 IVPASS = IVPASS + 1                                               05360032
      WRITE (I02,80001) IVTNUM                                          05370032
      GO TO 3501                                                        05380032
23490 IVFAIL = IVFAIL + 1                                               05390032
      IVCORR=7                                                          05400032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05410032
 3501 CONTINUE                                                          05420032
      IVTNUM = 350                                                      05430032
C                                                                       05440032
C      ****  TEST 350  ****                                             05450032
C                                                                       05460032
      IF (ICZERO) 33500, 3500, 33500                                    05470032
 3500 CONTINUE                                                          05480032
      IVON01 = 9                                                        05490032
      IVON02 = -4                                                       05500032
      IVCOMP = (IVON01-IVON02) -2                                       05510032
      GO TO 43500                                                       05520032
33500 IVDELE = IVDELE + 1                                               05530032
      WRITE (I02,80003) IVTNUM                                          05540032
      IF (ICZERO) 43500, 3511, 43500                                    05550032
43500 IF (IVCOMP -11) 23500,13500,23500                                 05560032
13500 IVPASS = IVPASS + 1                                               05570032
      WRITE (I02,80001) IVTNUM                                          05580032
      GO TO 3511                                                        05590032
23500 IVFAIL = IVFAIL + 1                                               05600032
      IVCORR = 11                                                       05610032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05620032
 3511 CONTINUE                                                          05630032
      IVTNUM = 351                                                      05640032
C                                                                       05650032
C      ****  TEST 351  ****                                             05660032
C                                                                       05670032
      IF (ICZERO) 33510, 3510, 33510                                    05680032
 3510 CONTINUE                                                          05690032
      IVON01 = 9                                                        05700032
      IVON02 = -4                                                       05710032
      IVCOMP = IVON01-(IVON02-2)                                        05720032
      GO TO 43510                                                       05730032
33510 IVDELE = IVDELE + 1                                               05740032
      WRITE (I02,80003) IVTNUM                                          05750032
      IF (ICZERO) 43510, 3521, 43510                                    05760032
43510 IF (IVCOMP - 15) 23510,13510,23510                                05770032
13510 IVPASS = IVPASS + 1                                               05780032
      WRITE (I02,80001) IVTNUM                                          05790032
      GO TO 3521                                                        05800032
23510 IVFAIL = IVFAIL + 1                                               05810032
      IVCORR = 15                                                       05820032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05830032
 3521 CONTINUE                                                          05840032
      IVTNUM = 352                                                      05850032
C                                                                       05860032
C      ****  TEST 352  ****                                             05870032
C                                                                       05880032
      IF (ICZERO) 33520, 3520, 33520                                    05890032
 3520 CONTINUE                                                          05900032
      IVON01 = 683                                                      05910032
      IVON03 = 156                                                      05920032
      IVCOMP = (IVON01-101)-IVON03                                      05930032
      GO TO 43520                                                       05940032
33520 IVDELE = IVDELE + 1                                               05950032
      WRITE (I02,80003) IVTNUM                                          05960032
      IF (ICZERO) 43520, 3531, 43520                                    05970032
43520 IF (IVCOMP - 426) 23520,13520,23520                               05980032
13520 IVPASS = IVPASS + 1                                               05990032
      WRITE (I02,80001) IVTNUM                                          06000032
      GO TO 3531                                                        06010032
23520 IVFAIL = IVFAIL + 1                                               06020032
      IVCORR = 426                                                      06030032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06040032
 3531 CONTINUE                                                          06050032
      IVTNUM = 353                                                      06060032
C                                                                       06070032
C      ****  TEST 353  ****                                             06080032
C                                                                       06090032
      IF (ICZERO) 33530, 3530, 33530                                    06100032
 3530 CONTINUE                                                          06110032
      IVON01 = 683                                                      06120032
      IVON03 = 156                                                      06130032
      IVCOMP = IVON01 -(101-IVON03)                                     06140032
      GO TO 43530                                                       06150032
33530 IVDELE = IVDELE + 1                                               06160032
      WRITE (I02,80003) IVTNUM                                          06170032
      IF (ICZERO) 43530, 3541, 43530                                    06180032
43530 IF (IVCOMP -738) 23530,13530,23530                                06190032
13530 IVPASS = IVPASS + 1                                               06200032
      WRITE (I02,80001) IVTNUM                                          06210032
      GO TO 3541                                                        06220032
23530 IVFAIL = IVFAIL + 1                                               06230032
      IVCORR = 738                                                      06240032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06250032
 3541 CONTINUE                                                          06260032
      IVTNUM = 354                                                      06270032
C                                                                       06280032
C      ****  TEST 354  ****                                             06290032
C                                                                       06300032
      IF (ICZERO) 33540, 3540, 33540                                    06310032
 3540 CONTINUE                                                          06320032
      IVON01 = 683                                                      06330032
      IVON03 =-156                                                      06340032
      IVCOMP = IVON01 -(101-IVON03)                                     06350032
      GO TO 43540                                                       06360032
33540 IVDELE = IVDELE + 1                                               06370032
      WRITE (I02,80003) IVTNUM                                          06380032
      IF (ICZERO) 43540, 3551, 43540                                    06390032
43540 IF (IVCOMP -426) 23540,13540,23540                                06400032
13540 IVPASS = IVPASS + 1                                               06410032
      WRITE (I02,80001) IVTNUM                                          06420032
      GO TO 3551                                                        06430032
23540 IVFAIL = IVFAIL + 1                                               06440032
      IVCORR = 426                                                      06450032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06460032
 3551 CONTINUE                                                          06470032
      IVTNUM = 355                                                      06480032
C                                                                       06490032
C      ****  TEST 355  ****                                             06500032
C                                                                       06510032
      IF (ICZERO) 33550, 3550, 33550                                    06520032
 3550 CONTINUE                                                          06530032
      IVON01 = -683                                                     06540032
      IVON03 = -156                                                     06550032
      IVCOMP = (IVON01-101)-IVON03                                      06560032
      GO TO 43550                                                       06570032
33550 IVDELE = IVDELE + 1                                               06580032
      WRITE (I02,80003) IVTNUM                                          06590032
      IF (ICZERO) 43550, 3561, 43550                                    06600032
43550 IF (IVCOMP +628) 23550,13550,23550                                06610032
13550 IVPASS = IVPASS + 1                                               06620032
      WRITE (I02,80001) IVTNUM                                          06630032
      GO TO 3561                                                        06640032
23550 IVFAIL = IVFAIL + 1                                               06650032
      IVCORR = -628                                                     06660032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06670032
 3561 CONTINUE                                                          06680032
      IVTNUM = 356                                                      06690032
C                                                                       06700032
C      ****  TEST 356  ****                                             06710032
C                                                                       06720032
      IF (ICZERO) 33560, 3560, 33560                                    06730032
 3560 CONTINUE                                                          06740032
      IVON02 = 15687                                                    06750032
      IVON03 =  387                                                     06760032
      IVCOMP = (8542-IVON02)-IVON03                                     06770032
      GO TO 43560                                                       06780032
33560 IVDELE = IVDELE + 1                                               06790032
      WRITE (I02,80003) IVTNUM                                          06800032
      IF (ICZERO) 43560, 3571, 43560                                    06810032
43560 IF (IVCOMP +7532) 23560,13560,23560                               06820032
13560 IVPASS = IVPASS + 1                                               06830032
      WRITE (I02,80001) IVTNUM                                          06840032
      GO TO 3571                                                        06850032
23560 IVFAIL = IVFAIL + 1                                               06860032
      IVCORR = -7532                                                    06870032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06880032
 3571 CONTINUE                                                          06890032
      IVTNUM = 357                                                      06900032
C                                                                       06910032
C      ****  TEST 357  ****                                             06920032
C                                                                       06930032
      IF (ICZERO) 33570, 3570, 33570                                    06940032
 3570 CONTINUE                                                          06950032
      IVON02= 15687                                                     06960032
      IVON03=  387                                                      06970032
      IVCOMP= 8542-(IVON02-IVON03)                                      06980032
      GO TO 43570                                                       06990032
33570 IVDELE = IVDELE + 1                                               07000032
      WRITE (I02,80003) IVTNUM                                          07010032
      IF (ICZERO) 43570, 3581, 43570                                    07020032
43570 IF (IVCOMP + 6758) 23570,13570,23570                              07030032
13570 IVPASS = IVPASS + 1                                               07040032
      WRITE (I02,80001) IVTNUM                                          07050032
      GO TO 3581                                                        07060032
23570 IVFAIL = IVFAIL + 1                                               07070032
      IVCORR = -6758                                                    07080032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07090032
 3581 CONTINUE                                                          07100032
      IVTNUM = 358                                                      07110032
C                                                                       07120032
C      ****  TEST 358  ****                                             07130032
C                                                                       07140032
      IF (ICZERO) 33580, 3580, 33580                                    07150032
 3580 CONTINUE                                                          07160032
      IVON02 = -15687                                                   07170032
      IVON03 = 387                                                      07180032
      IVCOMP =(8542-IVON02)-IVON03                                      07190032
      GO TO 43580                                                       07200032
33580 IVDELE = IVDELE + 1                                               07210032
      WRITE (I02,80003) IVTNUM                                          07220032
      IF (ICZERO) 43580, 3591, 43580                                    07230032
43580 IF (IVCOMP - 23842) 23580,13580,23580                             07240032
13580 IVPASS = IVPASS + 1                                               07250032
      WRITE (I02,80001) IVTNUM                                          07260032
      GO TO 3591                                                        07270032
23580 IVFAIL = IVFAIL + 1                                               07280032
      IVCORR =23842                                                     07290032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07300032
 3591 CONTINUE                                                          07310032
      IVTNUM = 359                                                      07320032
C                                                                       07330032
C      ****  TEST 359  ****                                             07340032
C                                                                       07350032
      IF (ICZERO) 33590, 3590, 33590                                    07360032
 3590 CONTINUE                                                          07370032
      IVON02 = -15687                                                   07380032
      IVON03 =  387                                                     07390032
      IVCOMP = 8542-(IVON02-IVON03)                                     07400032
      GO TO 43590                                                       07410032
33590 IVDELE = IVDELE + 1                                               07420032
      WRITE (I02,80003) IVTNUM                                          07430032
      IF (ICZERO) 43590, 3601, 43590                                    07440032
43590 IF (IVCOMP - 24616) 23590,13590,23590                             07450032
13590 IVPASS = IVPASS + 1                                               07460032
      WRITE (I02,80001) IVTNUM                                          07470032
      GO TO 3601                                                        07480032
23590 IVFAIL = IVFAIL + 1                                               07490032
      IVCORR = 24616                                                    07500032
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07510032
C      ****   END OF TESTS   ****                                       07520032
 3601 CONTINUE                                                          07530032
C                                                                       07540032
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07550032
99999 CONTINUE                                                          07560032
      WRITE (I02,90002)                                                 07570032
      WRITE (I02,90006)                                                 07580032
      WRITE (I02,90002)                                                 07590032
      WRITE (I02,90002)                                                 07600032
      WRITE (I02,90007)                                                 07610032
      WRITE (I02,90002)                                                 07620032
      WRITE (I02,90008)  IVFAIL                                         07630032
      WRITE (I02,90009) IVPASS                                          07640032
      WRITE (I02,90010) IVDELE                                          07650032
C                                                                       07660032
C                                                                       07670032
C     TERMINATE ROUTINE EXECUTION                                       07680032
      STOP                                                              07690032
C                                                                       07700032
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07710032
90000 FORMAT (1H1)                                                      07720032
90002 FORMAT (1H )                                                      07730032
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07740032
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07750032
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07760032
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07770032
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07780032
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07790032
C                                                                       07800032
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07810032
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07820032
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07830032
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07840032
C                                                                       07850032
C     FORMAT STATEMENTS FOR TEST RESULTS                                07860032
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07870032
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07880032
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07890032
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07900032
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07910032
C                                                                       07920032
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM032)                          07930032
      END                                                               07940032
