C     COMMENT SECTION                                                   00010061
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020061
C     FM061                                                             00030061
C                                                                       00040061
C          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE   00050061
C     FORM                                                              00060061
C                   INTEGER VARIABLE = REAL CONSTANT                    00070061
C                   INTEGER VARIABLE = REAL VARIABLE                    00080061
C                   REAL VARIABLE = INTEGER VARIABLE                    00090061
C                   REAL VARIABLE = INTEGER CONSTANT                    00100061
C                                                                       00110061
C     THE CONSTANTS AND VARIABLES CONTAIN BOTH POSITIVE AND NEGATIVE    00120061
C     VALUES.                                                           00130061
C                                                                       00140061
C           A REAL DATUM IS A PROCESSOR APPROXIMATION TO THE VALUE OF A 00150061
C     REAL NUMBER.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES.   00160061
C                                                                       00170061
C          A BASIC REAL CONSTANT IS WRITTEN AS AN INTEGER PART, A       00180061
C     DECIMAL POINT, AND A DECIMAL FRACTION PART IN THAT ORDER.  BOTH   00190061
C     THE INTEGER PART AND THE DECIMAL PART ARE STRINGS OF DIGITS;      00200061
C     EITHER ONE OF THESE STRINGS MAY BE EMPTY BUT NOT BOTH.  THE       00210061
C     CONSTANT IS AN APPROXIMATION TO THE DIGIT STRING INTERPRETED AS A 00220061
C     DECIMAL NUMERAL.                                                  00230061
C                                                                       00240061
C         A DECIMAL EXPONENT IS WRITTEN AS THE LETTER E, FOLLOWED BY AN 00250061
C     OPTIONALLY SIGNED INTEGER CONSTANT.                               00260061
C                                                                       00270061
C         A REAL CONSTANT IS INDICATED BY WRITING A BASIC REAL CONSTANT,00280061
C     A BASIC REAL CONSTANT FOLLOWED BY A DECIMAL EXPONENT, OR AN       00290061
C     INTEGER CONSTANT FOLLOWED BY A DECIMAL EXPONENT.                  00300061
C                                                                       00310061
C      REFERENCES                                                       00320061
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00330061
C              X3.9-1978                                                00340061
C                                                                       00350061
C        SECTION 4.4, REAL TYPE                                         00360061
C        SECTION 4.4.1, REAL CONSTANT                                   00370061
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00380061
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00390061
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00400061
C        SECTION 11.4, ARITHMETIC IF STATEMENT                          00410061
C                                                                       00420061
C      **********************************************************       00430061
C                                                                       00440061
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00450061
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00460061
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00470061
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00480061
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00490061
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00500061
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00510061
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00520061
C     OF EXECUTING THESE TESTS.                                         00530061
C                                                                       00540061
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00550061
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00560061
C                                                                       00570061
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00580061
C                                                                       00590061
C                  DEPARTMENT OF THE NAVY                               00600061
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00610061
C                  WASHINGTON, D.C.  20376                              00620061
C                                                                       00630061
C      **********************************************************       00640061
C                                                                       00650061
C                                                                       00660061
C                                                                       00670061
C     INITIALIZATION SECTION                                            00680061
C                                                                       00690061
C     INITIALIZE CONSTANTS                                              00700061
C      **************                                                   00710061
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00720061
      I01 = 5                                                           00730061
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00740061
      I02 = 6                                                           00750061
C     SYSTEM ENVIRONMENT SECTION                                        00760061
C                                                                       00770061
      I01 = 5                                                           00780061
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00790061
C     (UNIT NUMBER FOR CARD READER).                                    00800061
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00810061
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00820061
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00830061
C                                                                       00840061
      I02 = 6                                                           00850061
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00860061
C     (UNIT NUMBER FOR PRINTER).                                        00870061
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00880061
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00890061
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00900061
C                                                                       00910061
      IVPASS=0                                                          00920061
      IVFAIL=0                                                          00930061
      IVDELE=0                                                          00940061
      ICZERO=0                                                          00950061
C                                                                       00960061
C     WRITE PAGE HEADERS                                                00970061
      WRITE (I02,90000)                                                 00980061
      WRITE (I02,90001)                                                 00990061
      WRITE (I02,90002)                                                 01000061
      WRITE (I02, 90002)                                                01010061
      WRITE (I02,90003)                                                 01020061
      WRITE (I02,90002)                                                 01030061
      WRITE (I02,90004)                                                 01040061
      WRITE (I02,90002)                                                 01050061
      WRITE (I02,90011)                                                 01060061
      WRITE (I02,90002)                                                 01070061
      WRITE (I02,90002)                                                 01080061
      WRITE (I02,90005)                                                 01090061
      WRITE (I02,90006)                                                 01100061
      WRITE (I02,90002)                                                 01110061
C                                                                       01120061
C     TEST SECTION                                                      01130061
C                                                                       01140061
C     TEST 32 THROUGH TEST 42 CONTAIN ARITHMETIC ASSIGNMENT             01150061
C     STATEMENTS OF THE FORM                                            01160061
C                                                                       01170061
C                   INTEGER VARIABLE = REAL VARIABLE                    01180061
C                                                                       01190061
      IVTNUM =  32                                                      01200061
C                                                                       01210061
C      ****  TEST  32  ****                                             01220061
C                                                                       01230061
      IF (ICZERO) 30320,  320, 30320                                    01240061
  320 CONTINUE                                                          01250061
      RVON01 = 44.5                                                     01260061
      IVCOMP = RVON01                                                   01270061
      GO TO 40320                                                       01280061
30320 IVDELE = IVDELE + 1                                               01290061
      WRITE (I02,80003) IVTNUM                                          01300061
      IF (ICZERO) 40320,  331, 40320                                    01310061
40320 IF (IVCOMP - 44) 20320,10320,20320                                01320061
10320 IVPASS = IVPASS + 1                                               01330061
      WRITE (I02,80001) IVTNUM                                          01340061
      GO TO  331                                                        01350061
20320 IVFAIL = IVFAIL + 1                                               01360061
      IVCORR = 44                                                       01370061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01380061
  331 CONTINUE                                                          01390061
      IVTNUM =  33                                                      01400061
C                                                                       01410061
C      ****  TEST  33  ****                                             01420061
C                                                                       01430061
      IF (ICZERO) 30330,  330, 30330                                    01440061
  330 CONTINUE                                                          01450061
      RVON01 = -2.0005                                                  01460061
      IVCOMP = RVON01                                                   01470061
      GO TO 40330                                                       01480061
30330 IVDELE = IVDELE + 1                                               01490061
      WRITE (I02,80003) IVTNUM                                          01500061
      IF (ICZERO) 40330,  341, 40330                                    01510061
40330 IF (IVCOMP + 2) 20330,10330,20330                                 01520061
10330 IVPASS = IVPASS + 1                                               01530061
      WRITE (I02,80001) IVTNUM                                          01540061
      GO TO  341                                                        01550061
20330 IVFAIL = IVFAIL + 1                                               01560061
      IVCORR = -2                                                       01570061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01580061
  341 CONTINUE                                                          01590061
      IVTNUM =  34                                                      01600061
C                                                                       01610061
C      ****  TEST  34  ****                                             01620061
C                                                                       01630061
      IF (ICZERO) 30340,  340, 30340                                    01640061
  340 CONTINUE                                                          01650061
      RVON01 = .32767                                                   01660061
      IVCOMP = RVON01                                                   01670061
      GO TO 40340                                                       01680061
30340 IVDELE = IVDELE + 1                                               01690061
      WRITE (I02,80003) IVTNUM                                          01700061
      IF (ICZERO) 40340,  351, 40340                                    01710061
40340 IF (IVCOMP) 20340,10340,20340                                     01720061
10340 IVPASS = IVPASS + 1                                               01730061
      WRITE (I02,80001) IVTNUM                                          01740061
      GO TO  351                                                        01750061
20340 IVFAIL = IVFAIL + 1                                               01760061
      IVCORR = 0                                                        01770061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01780061
  351 CONTINUE                                                          01790061
      IVTNUM =  35                                                      01800061
C                                                                       01810061
C      ****  TEST  35  ****                                             01820061
C                                                                       01830061
      IF (ICZERO) 30350,  350, 30350                                    01840061
  350 CONTINUE                                                          01850061
      RVON01 = 1.999                                                    01860061
      IVCOMP = RVON01                                                   01870061
      GO TO 40350                                                       01880061
30350 IVDELE = IVDELE + 1                                               01890061
      WRITE (I02,80003) IVTNUM                                          01900061
      IF (ICZERO) 40350,  361, 40350                                    01910061
40350 IF (IVCOMP - 1) 20350,10350,20350                                 01920061
10350 IVPASS = IVPASS + 1                                               01930061
      WRITE (I02,80001) IVTNUM                                          01940061
      GO TO  361                                                        01950061
20350 IVFAIL = IVFAIL + 1                                               01960061
      IVCORR = 1                                                        01970061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01980061
  361 CONTINUE                                                          01990061
      IVTNUM =  36                                                      02000061
C                                                                       02010061
C      ****  TEST  36  ****                                             02020061
C                                                                       02030061
      IF (ICZERO) 30360,  360, 30360                                    02040061
  360 CONTINUE                                                          02050061
      RVON01 = .25E+1                                                   02060061
      IVCOMP = RVON01                                                   02070061
      GO TO 40360                                                       02080061
30360 IVDELE = IVDELE + 1                                               02090061
      WRITE (I02,80003) IVTNUM                                          02100061
      IF (ICZERO) 40360,  371, 40360                                    02110061
40360 IF (IVCOMP - 2) 20360,10360,20360                                 02120061
10360 IVPASS = IVPASS + 1                                               02130061
      WRITE (I02,80001) IVTNUM                                          02140061
      GO TO  371                                                        02150061
20360 IVFAIL = IVFAIL + 1                                               02160061
      IVCORR = 2                                                        02170061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02180061
  371 CONTINUE                                                          02190061
      IVTNUM =  37                                                      02200061
C                                                                       02210061
C      ****  TEST  37  ****                                             02220061
C                                                                       02230061
      IF (ICZERO) 30370,  370, 30370                                    02240061
  370 CONTINUE                                                          02250061
      RVON01 = 445.0E-01                                                02260061
      IVCOMP = RVON01                                                   02270061
      GO TO 40370                                                       02280061
30370 IVDELE = IVDELE + 1                                               02290061
      WRITE (I02,80003) IVTNUM                                          02300061
      IF (ICZERO) 40370,  381, 40370                                    02310061
40370 IF (IVCOMP - 44) 20370,10370,20370                                02320061
10370 IVPASS = IVPASS + 1                                               02330061
      WRITE (I02,80001) IVTNUM                                          02340061
      GO TO  381                                                        02350061
20370 IVFAIL = IVFAIL + 1                                               02360061
      IVCORR = 44                                                       02370061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02380061
  381 CONTINUE                                                          02390061
      IVTNUM =  38                                                      02400061
C                                                                       02410061
C      ****  TEST  38  ****                                             02420061
C                                                                       02430061
      IF (ICZERO) 30380,  380, 30380                                    02440061
  380 CONTINUE                                                          02450061
      RVON01 = -651.1E-0                                                02460061
      IVCOMP = RVON01                                                   02470061
      GO TO 40380                                                       02480061
30380 IVDELE = IVDELE + 1                                               02490061
      WRITE (I02,80003) IVTNUM                                          02500061
      IF (ICZERO) 40380,  391, 40380                                    02510061
40380 IF (IVCOMP + 651) 20380,10380,20380                               02520061
10380 IVPASS = IVPASS + 1                                               02530061
      WRITE (I02,80001) IVTNUM                                          02540061
      GO TO  391                                                        02550061
20380 IVFAIL = IVFAIL + 1                                               02560061
      IVCORR = -651                                                     02570061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02580061
  391 CONTINUE                                                          02590061
      IVTNUM =  39                                                      02600061
C                                                                       02610061
C      ****  TEST  39  ****                                             02620061
C                                                                       02630061
      IF (ICZERO) 30390,  390, 30390                                    02640061
  390 CONTINUE                                                          02650061
      RVON01 = .3266E4                                                  02660061
      IVCOMP = RVON01                                                   02670061
      GO TO 40390                                                       02680061
30390 IVDELE = IVDELE + 1                                               02690061
      WRITE (I02,80003) IVTNUM                                          02700061
      IF (ICZERO) 40390,  401, 40390                                    02710061
40390 IF (IVCOMP - 3266) 20390,10390,20390                              02720061
10390 IVPASS = IVPASS + 1                                               02730061
      WRITE (I02,80001) IVTNUM                                          02740061
      GO TO  401                                                        02750061
20390 IVFAIL = IVFAIL + 1                                               02760061
      IVCORR = 3266                                                     02770061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02780061
  401 CONTINUE                                                          02790061
      IVTNUM =  40                                                      02800061
C                                                                       02810061
C      ****  TEST  40  ****                                             02820061
C                                                                       02830061
      IF (ICZERO) 30400,  400, 30400                                    02840061
  400 CONTINUE                                                          02850061
      RVON01 = 35.43E-01                                                02860061
      IVCOMP = RVON01                                                   02870061
      GO TO 40400                                                       02880061
30400 IVDELE = IVDELE + 1                                               02890061
      WRITE (I02,80003) IVTNUM                                          02900061
      IF (ICZERO) 40400,  411, 40400                                    02910061
40400 IF (IVCOMP - 3) 20400,10400,20400                                 02920061
10400 IVPASS = IVPASS + 1                                               02930061
      WRITE (I02,80001) IVTNUM                                          02940061
      GO TO  411                                                        02950061
20400 IVFAIL = IVFAIL + 1                                               02960061
      IVCORR = 3                                                        02970061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02980061
  411 CONTINUE                                                          02990061
      IVTNUM =  41                                                      03000061
C                                                                       03010061
C      ****  TEST  41  ****                                             03020061
C                                                                       03030061
      IF (ICZERO) 30410,  410, 30410                                    03040061
  410 CONTINUE                                                          03050061
      RVON01 = -7.001E2                                                 03060061
      IVCOMP = RVON01                                                   03070061
      GO TO 40410                                                       03080061
30410 IVDELE = IVDELE + 1                                               03090061
      WRITE (I02,80003) IVTNUM                                          03100061
      IF (ICZERO) 40410,  421, 40410                                    03110061
40410 IF (IVCOMP + 700) 20410,10410,20410                               03120061
10410 IVPASS = IVPASS + 1                                               03130061
      WRITE (I02,80001) IVTNUM                                          03140061
      GO TO  421                                                        03150061
20410 IVFAIL = IVFAIL + 1                                               03160061
      IVCORR = -700                                                     03170061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03180061
  421 CONTINUE                                                          03190061
      IVTNUM =  42                                                      03200061
C                                                                       03210061
C      ****  TEST  42  ****                                             03220061
C                                                                       03230061
      IF (ICZERO) 30420,  420, 30420                                    03240061
  420 CONTINUE                                                          03250061
      RVON01 = 4.45E-02                                                 03260061
      IVCOMP = RVON01                                                   03270061
      GO TO 40420                                                       03280061
30420 IVDELE = IVDELE + 1                                               03290061
      WRITE (I02,80003) IVTNUM                                          03300061
      IF (ICZERO) 40420,  431, 40420                                    03310061
40420 IF (IVCOMP) 20420,10420,20420                                     03320061
10420 IVPASS = IVPASS + 1                                               03330061
      WRITE (I02,80001) IVTNUM                                          03340061
      GO TO  431                                                        03350061
20420 IVFAIL = IVFAIL + 1                                               03360061
      IVCORR = 0                                                        03370061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03380061
C     TEST 43 THROUGH TEST 48 CONTAIN ARITHMETIC ASSIGNMENT             03390061
C     STATEMENTS OF THE FORM                                            03400061
C                                                                       03410061
C                   REAL VARIABLE = INTEGER VARIABLE                    03420061
C                                                                       03430061
  431 CONTINUE                                                          03440061
      IVTNUM =  43                                                      03450061
C                                                                       03460061
C      ****  TEST  43  ****                                             03470061
C                                                                       03480061
      IF (ICZERO) 30430,  430, 30430                                    03490061
  430 CONTINUE                                                          03500061
      IVON01 = 2                                                        03510061
      RVCOMP = IVON01                                                   03520061
      GO TO 40430                                                       03530061
30430 IVDELE = IVDELE + 1                                               03540061
      WRITE (I02,80003) IVTNUM                                          03550061
      IF (ICZERO) 40430,  441, 40430                                    03560061
40430 IF (RVCOMP - 1.9995) 20430,10430,40431                            03570061
40431 IF (RVCOMP - 2.0005) 10430,10430,20430                            03580061
10430 IVPASS = IVPASS + 1                                               03590061
      WRITE (I02,80001) IVTNUM                                          03600061
      GO TO  441                                                        03610061
20430 IVFAIL = IVFAIL + 1                                               03620061
      RVCORR = 2.0000                                                   03630061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03640061
  441 CONTINUE                                                          03650061
      IVTNUM =  44                                                      03660061
C                                                                       03670061
C      ****  TEST  44  ****                                             03680061
C                                                                       03690061
      IF (ICZERO) 30440,  440, 30440                                    03700061
  440 CONTINUE                                                          03710061
      IVON01 = 25                                                       03720061
      RVCOMP = IVON01                                                   03730061
      GO TO 40440                                                       03740061
30440 IVDELE = IVDELE + 1                                               03750061
      WRITE (I02,80003) IVTNUM                                          03760061
      IF (ICZERO) 40440,  451, 40440                                    03770061
40440 IF (RVCOMP - 24.995) 20440,10440,40441                            03780061
40441 IF (RVCOMP - 25.005) 10440,10440,20440                            03790061
10440 IVPASS = IVPASS + 1                                               03800061
      WRITE (I02,80001) IVTNUM                                          03810061
      GO TO  451                                                        03820061
20440 IVFAIL = IVFAIL + 1                                               03830061
      RVCORR = 25.000                                                   03840061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03850061
  451 CONTINUE                                                          03860061
      IVTNUM =  45                                                      03870061
C                                                                       03880061
C      ****  TEST  45  ****                                             03890061
C                                                                       03900061
      IF (ICZERO) 30450,  450, 30450                                    03910061
  450 CONTINUE                                                          03920061
      IVON01 = 357                                                      03930061
      RVCOMP = IVON01                                                   03940061
      GO TO 40450                                                       03950061
30450 IVDELE = IVDELE + 1                                               03960061
      WRITE (I02,80003) IVTNUM                                          03970061
      IF (ICZERO) 40450,  461, 40450                                    03980061
40450 IF (RVCOMP - 356.95) 20450,10450,40451                            03990061
40451 IF (RVCOMP - 357.05) 10450,10450,20450                            04000061
10450 IVPASS = IVPASS + 1                                               04010061
      WRITE (I02,80001) IVTNUM                                          04020061
      GO TO  461                                                        04030061
20450 IVFAIL = IVFAIL + 1                                               04040061
      RVCORR = 357.00                                                   04050061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04060061
  461 CONTINUE                                                          04070061
      IVTNUM =  46                                                      04080061
C                                                                       04090061
C      ****  TEST  46  ****                                             04100061
C                                                                       04110061
      IF (ICZERO) 30460,  460, 30460                                    04120061
  460 CONTINUE                                                          04130061
      IVON01 = 4968                                                     04140061
      RVCOMP = IVON01                                                   04150061
      GO TO 40460                                                       04160061
30460 IVDELE = IVDELE + 1                                               04170061
      WRITE (I02,80003) IVTNUM                                          04180061
      IF (ICZERO) 40460,  471, 40460                                    04190061
40460 IF (RVCOMP - 4967.5) 20460,10460,40461                            04200061
40461 IF (RVCOMP - 4968.5) 10460,10460,20460                            04210061
10460 IVPASS = IVPASS + 1                                               04220061
      WRITE (I02,80001) IVTNUM                                          04230061
      GO TO  471                                                        04240061
20460 IVFAIL = IVFAIL + 1                                               04250061
      RVCORR = 4968.0                                                   04260061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04270061
  471 CONTINUE                                                          04280061
      IVTNUM =  47                                                      04290061
C                                                                       04300061
C      ****  TEST  47  ****                                             04310061
C                                                                       04320061
      IF (ICZERO) 30470,  470, 30470                                    04330061
  470 CONTINUE                                                          04340061
      IVON01 = 32767                                                    04350061
      RVCOMP = IVON01                                                   04360061
      GO TO 40470                                                       04370061
30470 IVDELE = IVDELE + 1                                               04380061
      WRITE (I02,80003) IVTNUM                                          04390061
      IF (ICZERO) 40470,  481, 40470                                    04400061
40470 IF (RVCOMP - 32762.) 20470,10470,40471                            04410061
40471 IF (RVCOMP - 32772.) 10470,10470,20470                            04420061
10470 IVPASS = IVPASS + 1                                               04430061
      WRITE (I02,80001) IVTNUM                                          04440061
      GO TO  481                                                        04450061
20470 IVFAIL = IVFAIL + 1                                               04460061
      RVCORR = 32767.                                                   04470061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04480061
  481 CONTINUE                                                          04490061
      IVTNUM =  48                                                      04500061
C                                                                       04510061
C      ****  TEST  48  ****                                             04520061
C                                                                       04530061
      IF (ICZERO) 30480,  480, 30480                                    04540061
  480 CONTINUE                                                          04550061
      IVON01 = -2                                                       04560061
      RVCOMP = IVON01                                                   04570061
      GO TO 40480                                                       04580061
30480 IVDELE = IVDELE + 1                                               04590061
      WRITE (I02,80003) IVTNUM                                          04600061
      IF (ICZERO) 40480,  491, 40480                                    04610061
40480 IF (RVCOMP + 2.0005) 20480,10480,40481                            04620061
40481 IF (RVCOMP + 1.9995) 10480,10480,20450                            04630061
10480 IVPASS = IVPASS + 1                                               04640061
      WRITE (I02,80001) IVTNUM                                          04650061
      GO TO  491                                                        04660061
20480 IVFAIL = IVFAIL + 1                                               04670061
      RVCORR = -2.0000                                                  04680061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          04690061
C                                                                       04700061
C     TEST 49 THROUGH TEST 51 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS  04710061
C     OF THE FORM                                                       04720061
C                   INTEGER VARIABLE = REAL CONSTANT                    04730061
C     WHERE CONSTANT IS BASIC REAL CONSTANT                             04740061
C                                                                       04750061
  491 CONTINUE                                                          04760061
      IVTNUM =  49                                                      04770061
C                                                                       04780061
C      ****  TEST  49  ****                                             04790061
C                                                                       04800061
      IF (ICZERO) 30490,  490, 30490                                    04810061
  490 CONTINUE                                                          04820061
      IVCOMP = 44.5                                                     04830061
      GO TO 40490                                                       04840061
30490 IVDELE = IVDELE + 1                                               04850061
      WRITE (I02,80003) IVTNUM                                          04860061
      IF (ICZERO) 40490,  501, 40490                                    04870061
40490 IF (IVCOMP - 44) 20490,10490,20490                                04880061
10490 IVPASS = IVPASS + 1                                               04890061
      WRITE (I02,80001) IVTNUM                                          04900061
      GO TO  501                                                        04910061
20490 IVFAIL = IVFAIL + 1                                               04920061
      IVCORR = 44                                                       04930061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04940061
  501 CONTINUE                                                          04950061
      IVTNUM =  50                                                      04960061
C                                                                       04970061
C      ****  TEST  50  ****                                             04980061
C                                                                       04990061
      IF (ICZERO) 30500,  500, 30500                                    05000061
  500 CONTINUE                                                          05010061
      IVCOMP = 6500.1                                                   05020061
      GO TO 40500                                                       05030061
30500 IVDELE = IVDELE + 1                                               05040061
      WRITE (I02,80003) IVTNUM                                          05050061
      IF (ICZERO) 40500,  511, 40500                                    05060061
40500 IF (IVCOMP - 6500)  20500,10500,20500                             05070061
10500 IVPASS = IVPASS + 1                                               05080061
      WRITE (I02,80001) IVTNUM                                          05090061
      GO TO  511                                                        05100061
20500 IVFAIL = IVFAIL + 1                                               05110061
      IVCORR = 6500                                                     05120061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05130061
  511 CONTINUE                                                          05140061
      IVTNUM =  51                                                      05150061
C                                                                       05160061
C      ****  TEST  51  ****                                             05170061
C                                                                       05180061
      IF (ICZERO) 30510,  510, 30510                                    05190061
  510 CONTINUE                                                          05200061
      IVCOMP = -.33333                                                  05210061
      GO TO 40510                                                       05220061
30510 IVDELE = IVDELE + 1                                               05230061
      WRITE (I02,80003) IVTNUM                                          05240061
      IF (ICZERO) 40510,  521, 40510                                    05250061
40510 IF (IVCOMP) 20510,10510,20510                                     05260061
10510 IVPASS = IVPASS + 1                                               05270061
      WRITE (I02,80001) IVTNUM                                          05280061
      GO TO  521                                                        05290061
20510 IVFAIL = IVFAIL + 1                                               05300061
      IVCORR = 0                                                        05310061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05320061
C                                                                       05330061
C     TEST 52 THROUGH TEST 55 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS  05340061
C     OF THE FORM                                                       05350061
C                   INTEGER VARIABLE = REAL CONSTANT                    05360061
C                                                                       05370061
C     WHERE CONSTANT IS BASIC REAL CONSTANT FOLLOWED BY DECIMAL EXPONENT05380061
C                                                                       05390061
  521 CONTINUE                                                          05400061
      IVTNUM =  52                                                      05410061
C                                                                       05420061
C      ****  TEST  52  ****                                             05430061
C                                                                       05440061
      IF (ICZERO) 30520,  520, 30520                                    05450061
  520 CONTINUE                                                          05460061
      IVCOMP = .21E+1                                                   05470061
      GO TO 40520                                                       05480061
30520 IVDELE = IVDELE + 1                                               05490061
      WRITE (I02,80003) IVTNUM                                          05500061
      IF (ICZERO) 40520,  531, 40520                                    05510061
40520 IF (IVCOMP - 2) 20520,10520,20520                                 05520061
10520 IVPASS = IVPASS + 1                                               05530061
      WRITE (I02,80001) IVTNUM                                          05540061
      GO TO  531                                                        05550061
20520 IVFAIL = IVFAIL + 1                                               05560061
      IVCORR = 2                                                        05570061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05580061
  531 CONTINUE                                                          05590061
      IVTNUM =  53                                                      05600061
C                                                                       05610061
C      ****  TEST  53  ****                                             05620061
C                                                                       05630061
      IF (ICZERO) 30530,  530, 30530                                    05640061
  530 CONTINUE                                                          05650061
      IVCOMP = 445.0E-01                                                05660061
      GO TO 40530                                                       05670061
30530 IVDELE = IVDELE + 1                                               05680061
      WRITE (I02,80003) IVTNUM                                          05690061
      IF (ICZERO) 40530,  541, 40530                                    05700061
40530 IF (IVCOMP - 44) 20530,10530,20530                                05710061
10530 IVPASS = IVPASS + 1                                               05720061
      WRITE (I02,80001) IVTNUM                                          05730061
      GO TO  541                                                        05740061
20530 IVFAIL = IVFAIL + 1                                               05750061
      IVCORR = 44                                                       05760061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05770061
  541 CONTINUE                                                          05780061
      IVTNUM =  54                                                      05790061
C                                                                       05800061
C      ****  TEST  54  ****                                             05810061
C                                                                       05820061
      IF (ICZERO) 30540,  540, 30540                                    05830061
  540 CONTINUE                                                          05840061
      IVCOMP = 4.450E1                                                  05850061
      GO TO 40540                                                       05860061
30540 IVDELE = IVDELE + 1                                               05870061
      WRITE (I02,80003) IVTNUM                                          05880061
      IF (ICZERO) 40540,  551, 40540                                    05890061
40540 IF (IVCOMP - 44) 20540,10540,20540                                05900061
10540 IVPASS = IVPASS + 1                                               05910061
      WRITE (I02,80001) IVTNUM                                          05920061
      GO TO  551                                                        05930061
20540 IVFAIL = IVFAIL + 1                                               05940061
      IVCORR = 44                                                       05950061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05960061
  551 CONTINUE                                                          05970061
      IVTNUM =  55                                                      05980061
C                                                                       05990061
C      ****  TEST  55  ****                                             06000061
C                                                                       06010061
      IF (ICZERO) 30550,  550, 30550                                    06020061
  550 CONTINUE                                                          06030061
      IVCOMP = -4.45E0                                                  06040061
      GO TO 40550                                                       06050061
30550 IVDELE = IVDELE + 1                                               06060061
      WRITE (I02,80003) IVTNUM                                          06070061
      IF (ICZERO) 40550,  561, 40550                                    06080061
40550 IF (IVCOMP + 4) 20550,10550,20550                                 06090061
10550 IVPASS = IVPASS + 1                                               06100061
      WRITE (I02,80001) IVTNUM                                          06110061
      GO TO  561                                                        06120061
20550 IVFAIL = IVFAIL + 1                                               06130061
      IVCORR = -4                                                       06140061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06150061
C                                                                       06160061
C     TEST 56 AND 57 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS OF THE    06170061
C     FORM          INTEGER VARIABLE = REAL CONSTANT                    06180061
C     WHERE CONSTANT IS INTEGER CONSTANT FOLLOWED BY DECIMAL EXPONENT   06190061
C                                                                       06200061
  561 CONTINUE                                                          06210061
      IVTNUM =  56                                                      06220061
C                                                                       06230061
C      ****  TEST  56  ****                                             06240061
C                                                                       06250061
      IF (ICZERO) 30560,  560, 30560                                    06260061
  560 CONTINUE                                                          06270061
      IVCOMP = 445E-02                                                  06280061
      GO TO 40560                                                       06290061
30560 IVDELE = IVDELE + 1                                               06300061
      WRITE (I02,80003) IVTNUM                                          06310061
      IF (ICZERO) 40560,  571, 40560                                    06320061
40560 IF (IVCOMP - 4) 20560,10560,20560                                 06330061
10560 IVPASS = IVPASS + 1                                               06340061
      WRITE (I02,80001) IVTNUM                                          06350061
      GO TO  571                                                        06360061
20560 IVFAIL = IVFAIL + 1                                               06370061
      IVCORR = 4                                                        06380061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06390061
  571 CONTINUE                                                          06400061
      IVTNUM =  57                                                      06410061
C                                                                       06420061
C      ****  TEST  57  ****                                             06430061
C                                                                       06440061
      IF (ICZERO) 30570,  570, 30570                                    06450061
  570 CONTINUE                                                          06460061
      IVCOMP = -701E-1                                                  06470061
      GO TO 40570                                                       06480061
30570 IVDELE = IVDELE + 1                                               06490061
      WRITE (I02,80003) IVTNUM                                          06500061
      IF (ICZERO) 40570,  581, 40570                                    06510061
40570 IF (IVCOMP + 70) 20570,10570,20570                                06520061
10570 IVPASS = IVPASS + 1                                               06530061
      WRITE (I02,80001) IVTNUM                                          06540061
      GO TO  581                                                        06550061
20570 IVFAIL = IVFAIL + 1                                               06560061
      IVCORR = -70                                                      06570061
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06580061
C                                                                       06590061
C     TEST 58 THROUGH TEST 62 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS  06600061
C     OF THE FORM   REAL VARIABLE = INTEGER CONSTANT                    06610061
C                                                                       06620061
  581 CONTINUE                                                          06630061
      IVTNUM =  58                                                      06640061
C                                                                       06650061
C      ****  TEST  58  ****                                             06660061
C                                                                       06670061
      IF (ICZERO) 30580,  580, 30580                                    06680061
  580 CONTINUE                                                          06690061
      RVCOMP = 23                                                       06700061
      GO TO 40580                                                       06710061
30580 IVDELE = IVDELE + 1                                               06720061
      WRITE (I02,80003) IVTNUM                                          06730061
      IF (ICZERO) 40580,  591, 40580                                    06740061
40580 IF (RVCOMP - 22.995) 20580,10580,40581                            06750061
40581 IF (RVCOMP - 23.005) 10580,10580,20580                            06760061
10580 IVPASS = IVPASS + 1                                               06770061
      WRITE (I02,80001) IVTNUM                                          06780061
      GO TO  591                                                        06790061
20580 IVFAIL = IVFAIL + 1                                               06800061
      RVCORR = 23.000                                                   06810061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          06820061
  591 CONTINUE                                                          06830061
      IVTNUM =  59                                                      06840061
C                                                                       06850061
C      ****  TEST  59  ****                                             06860061
C                                                                       06870061
      IF (ICZERO) 30590,  590, 30590                                    06880061
  590 CONTINUE                                                          06890061
      RVCOMP = 32645                                                    06900061
      GO TO 40590                                                       06910061
30590 IVDELE = IVDELE + 1                                               06920061
      WRITE (I02,80003) IVTNUM                                          06930061
      IF (ICZERO) 40590,  601, 40590                                    06940061
40590 IF (RVCOMP - 32640.) 20590,10590,40591                            06950061
40591 IF (RVCOMP - 32650.) 10590,10590,20590                            06960061
10590 IVPASS = IVPASS + 1                                               06970061
      WRITE (I02,80001) IVTNUM                                          06980061
      GO TO  601                                                        06990061
20590 IVFAIL = IVFAIL + 1                                               07000061
      RVCORR = 32645.                                                   07010061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07020061
  601 CONTINUE                                                          07030061
      IVTNUM =  60                                                      07040061
C                                                                       07050061
C      ****  TEST  60  ****                                             07060061
C                                                                       07070061
      IF (ICZERO) 30600,  600, 30600                                    07080061
  600 CONTINUE                                                          07090061
      RVCOMP = 0                                                        07100061
      GO TO 40600                                                       07110061
30600 IVDELE = IVDELE + 1                                               07120061
      WRITE (I02,80003) IVTNUM                                          07130061
      IF (ICZERO) 40600,  611, 40600                                    07140061
40600 IF (RVCOMP) 20600,10600,20600                                     07150061
10600 IVPASS = IVPASS + 1                                               07160061
      WRITE (I02,80001) IVTNUM                                          07170061
      GO TO  611                                                        07180061
20600 IVFAIL = IVFAIL + 1                                               07190061
      RVCORR = 00000.                                                   07200061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07210061
  611 CONTINUE                                                          07220061
      IVTNUM =  61                                                      07230061
C                                                                       07240061
C      ****  TEST  61  ****                                             07250061
C                                                                       07260061
      IF (ICZERO) 30610,  610, 30610                                    07270061
  610 CONTINUE                                                          07280061
      RVCOMP = -15                                                      07290061
      GO TO 40610                                                       07300061
30610 IVDELE = IVDELE + 1                                               07310061
      WRITE (I02,80003) IVTNUM                                          07320061
      IF (ICZERO) 40610,  621, 40610                                    07330061
40610 IF (RVCOMP -14.995) 40611,10610,20610                             07340061
40611 IF (RVCOMP + 15.005) 20610,10610,10610                            07350061
10610 IVPASS = IVPASS + 1                                               07360061
      WRITE (I02,80001) IVTNUM                                          07370061
      GO TO  621                                                        07380061
20610 IVFAIL = IVFAIL + 1                                               07390061
      RVCORR = -15.000                                                  07400061
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          07410061
  621 CONTINUE                                                          07420061
C                                                                       07430061
C      ****    END OF TESTS    ****                                     07440061
C                                                                       07450061
C                                                                       07460061
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07470061
99999 CONTINUE                                                          07480061
      WRITE (I02,90002)                                                 07490061
      WRITE (I02,90006)                                                 07500061
      WRITE (I02,90002)                                                 07510061
      WRITE (I02,90002)                                                 07520061
      WRITE (I02,90007)                                                 07530061
      WRITE (I02,90002)                                                 07540061
      WRITE (I02,90008)  IVFAIL                                         07550061
      WRITE (I02,90009) IVPASS                                          07560061
      WRITE (I02,90010) IVDELE                                          07570061
C                                                                       07580061
C                                                                       07590061
C     TERMINATE ROUTINE EXECUTION                                       07600061
      STOP                                                              07610061
C                                                                       07620061
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07630061
90000 FORMAT (1H1)                                                      07640061
90002 FORMAT (1H )                                                      07650061
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07660061
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07670061
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07680061
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07690061
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07700061
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07710061
C                                                                       07720061
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07730061
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07740061
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07750061
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07760061
C                                                                       07770061
C     FORMAT STATEMENTS FOR TEST RESULTS                                07780061
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07790061
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07800061
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07810061
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07820061
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07830061
C                                                                       07840061
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM061)                          07850061
      END                                                               07860061
