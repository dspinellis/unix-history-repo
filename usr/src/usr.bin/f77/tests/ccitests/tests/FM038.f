C     COMMENT SECTION                                                   00010038
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020038
C     FM038                                                             00030038
C                                                                       00040038
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    00050038
C     FORM          INTEGER VARIABLE = ARITHMETIC EXPRESSION            00060038
C     WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     00070038
C     OPERATOR /, INTEGER CONSTANTS AND AN INTEGER VARIABLE.  BOTH      00080038
C     POSITIVE AND NEGATIVE VALUES ARE USED FOR THE INTEGER CONSTANTS   00090038
C     AND THE INTEGER VARIABLE.                                         00100038
C                                                                       00110038
C         THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     00120038
C     AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  00130038
C     IN THE RESULTANT INTEGER VARIABLE.  SOME OF THE TESTS USE PARENS  00140038
C     TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSION.                   00150038
C                                                                       00160038
C         THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      00170038
C             (1) (INTEGER CONSTANT/INTEGER CONSTANT)/INTEGER CONSTANT  00180038
C             (2) INTEGER CONSTANT/(INTEGER CONSTANT/INTEGER CONSTANT)  00190038
C             (3) INTEGER VARIABLE/INTEGER CONSTANT                     00200038
C             (4) INTEGER CONSTANT/INTEGER VARIABLE                     00210038
C                                                                       00220038
C      REFERENCES                                                       00230038
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00240038
C              X3.9-1978                                                00250038
C                                                                       00260038
C        SECTION 4.3, INTEGER TYPE                                      00270038
C        SECTION 4.3.1, INTEGER CONSTANT                                00280038
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00290038
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00300038
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00310038
C                                                                       00320038
C      **********************************************************       00330038
C                                                                       00340038
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00350038
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00360038
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00370038
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00380038
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00390038
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00400038
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00410038
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00420038
C     OF EXECUTING THESE TESTS.                                         00430038
C                                                                       00440038
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00450038
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00460038
C                                                                       00470038
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00480038
C                                                                       00490038
C                  DEPARTMENT OF THE NAVY                               00500038
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00510038
C                  WASHINGTON, D.C.  20376                              00520038
C                                                                       00530038
C      **********************************************************       00540038
C                                                                       00550038
C                                                                       00560038
C                                                                       00570038
C     INITIALIZATION SECTION                                            00580038
C                                                                       00590038
C     INITIALIZE CONSTANTS                                              00600038
C      **************                                                   00610038
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620038
      I01 = 5                                                           00630038
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640038
      I02 = 6                                                           00650038
C     SYSTEM ENVIRONMENT SECTION                                        00660038
C                                                                       00670038
      I01 = 5                                                           00680038
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690038
C     (UNIT NUMBER FOR CARD READER).                                    00700038
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00710038
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00720038
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00730038
C                                                                       00740038
      I02 = 6                                                           00750038
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00760038
C     (UNIT NUMBER FOR PRINTER).                                        00770038
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00780038
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00790038
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00800038
C                                                                       00810038
      IVPASS=0                                                          00820038
      IVFAIL=0                                                          00830038
      IVDELE=0                                                          00840038
      ICZERO=0                                                          00850038
C                                                                       00860038
C     WRITE PAGE HEADERS                                                00870038
      WRITE (I02,90000)                                                 00880038
      WRITE (I02,90001)                                                 00890038
      WRITE (I02,90002)                                                 00900038
      WRITE (I02, 90002)                                                00910038
      WRITE (I02,90003)                                                 00920038
      WRITE (I02,90002)                                                 00930038
      WRITE (I02,90004)                                                 00940038
      WRITE (I02,90002)                                                 00950038
      WRITE (I02,90011)                                                 00960038
      WRITE (I02,90002)                                                 00970038
      WRITE (I02,90002)                                                 00980038
      WRITE (I02,90005)                                                 00990038
      WRITE (I02,90006)                                                 01000038
      WRITE (I02,90002)                                                 01010038
C                                                                       01020038
C     TEST SECTION                                                      01030038
C                                                                       01040038
C         ARITHMETIC ASSIGNMENT STATEMENT                               01050038
C                                                                       01060038
C     TEST 520 THROUGH TEST 525 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS01070038
C     OF THE FORM       INTEGER VARIABLE = (INT.CON./INT.CON.)/INT.CON. 01080038
C     NO TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND       01090038
C     NEGATIVE CONSTANTS ARE INCLUDED.                                  01100038
C                                                                       01110038
 5201 CONTINUE                                                          01120038
      IVTNUM = 520                                                      01130038
C                                                                       01140038
C      ****  TEST 520  ****                                             01150038
C                                                                       01160038
      IF (ICZERO) 35200, 5200, 35200                                    01170038
 5200 CONTINUE                                                          01180038
      IVCOMP = (24/3)/4                                                 01190038
      GO TO 45200                                                       01200038
35200 IVDELE = IVDELE + 1                                               01210038
      WRITE (I02,80003) IVTNUM                                          01220038
      IF (ICZERO) 45200, 5211, 45200                                    01230038
45200 IF (IVCOMP - 2) 25200,15200,25200                                 01240038
15200 IVPASS = IVPASS + 1                                               01250038
      WRITE (I02,80001) IVTNUM                                          01260038
      GO TO 5211                                                        01270038
25200 IVFAIL = IVFAIL + 1                                               01280038
      IVCORR = 2                                                        01290038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01300038
 5211 CONTINUE                                                          01310038
      IVTNUM = 521                                                      01320038
C                                                                       01330038
C      ****  TEST 521  ****                                             01340038
C                                                                       01350038
      IF (ICZERO) 35210, 5210, 35210                                    01360038
 5210 CONTINUE                                                          01370038
      IVCOMP = (7150/2)/25                                              01380038
      GO TO 45210                                                       01390038
35210 IVDELE = IVDELE + 1                                               01400038
      WRITE (I02,80003) IVTNUM                                          01410038
      IF (ICZERO) 45210, 5221, 45210                                    01420038
45210 IF (IVCOMP - 143) 25210,15210,25210                               01430038
15210 IVPASS = IVPASS + 1                                               01440038
      WRITE (I02,80001) IVTNUM                                          01450038
      GO TO 5221                                                        01460038
25210 IVFAIL = IVFAIL + 1                                               01470038
      IVCORR = 143                                                      01480038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01490038
 5221 CONTINUE                                                          01500038
      IVTNUM = 522                                                      01510038
C                                                                       01520038
C      ****  TEST 522  ****                                             01530038
C                                                                       01540038
      IF (ICZERO) 35220, 5220, 35220                                    01550038
                                                                        01560038
 5220 CONTINUE                                                          01570038
      IVCOMP = (-24/3)/4                                                01580038
      GO TO 45220                                                       01590038
35220 IVDELE = IVDELE + 1                                               01600038
      WRITE (I02,80003) IVTNUM                                          01610038
      IF (ICZERO) 45220, 5231, 45220                                    01620038
45220 IF (IVCOMP + 2) 25220,15220,25220                                 01630038
15220 IVPASS = IVPASS + 1                                               01640038
      WRITE (I02,80001) IVTNUM                                          01650038
      GO TO 5231                                                        01660038
25220 IVFAIL = IVFAIL + 1                                               01670038
      IVCORR = -2                                                       01680038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01690038
 5231 CONTINUE                                                          01700038
      IVTNUM = 523                                                      01710038
C                                                                       01720038
C      ****  TEST 523  ****                                             01730038
C                                                                       01740038
      IF (ICZERO) 35230, 5230, 35230                                    01750038
 5230 CONTINUE                                                          01760038
      IVCOMP = (330/(-3))/2                                             01770038
      GO TO 45230                                                       01780038
35230 IVDELE = IVDELE + 1                                               01790038
      WRITE (I02,80003) IVTNUM                                          01800038
      IF (ICZERO) 45230, 5241, 45230                                    01810038
45230 IF (IVCOMP + 55) 25230,15230,25230                                01820038
15230 IVPASS = IVPASS + 1                                               01830038
      WRITE (I02,80001) IVTNUM                                          01840038
      GO TO 5241                                                        01850038
25230 IVFAIL = IVFAIL + 1                                               01860038
      IVCORR = -55                                                      01870038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01880038
 5241 CONTINUE                                                          01890038
      IVTNUM = 524                                                      01900038
C                                                                       01910038
C      ****  TEST 524  ****                                             01920038
C                                                                       01930038
      IF (ICZERO) 35240, 5240, 35240                                    01940038
 5240 CONTINUE                                                          01950038
      IVCOMP = ((-7150)/(-2))/(-25)                                     01960038
      GO TO 45240                                                       01970038
35240 IVDELE = IVDELE + 1                                               01980038
      WRITE (I02,80003) IVTNUM                                          01990038
      IF (ICZERO) 45240, 5251, 45240                                    02000038
45240 IF (IVCOMP + 143) 25240,15240,25240                               02010038
15240 IVPASS = IVPASS + 1                                               02020038
      WRITE (I02,80001) IVTNUM                                          02030038
      GO TO 5251                                                        02040038
25240 IVFAIL = IVFAIL + 1                                               02050038
      IVCORR = -143                                                     02060038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02070038
 5251 CONTINUE                                                          02080038
      IVTNUM = 525                                                      02090038
C                                                                       02100038
C      ****  TEST 525  ****                                             02110038
C                                                                       02120038
      IF (ICZERO) 35250, 5250, 35250                                    02130038
 5250 CONTINUE                                                          02140038
      IVCOMP = (15249/(-13))/(-51)                                      02150038
      GO TO 45250                                                       02160038
35250 IVDELE = IVDELE + 1                                               02170038
      WRITE (I02,80003) IVTNUM                                          02180038
      IF (ICZERO) 45250, 5261, 45250                                    02190038
45250 IF (IVCOMP - 23) 25250,15250,25250                                02200038
15250 IVPASS = IVPASS + 1                                               02210038
      WRITE (I02,80001) IVTNUM                                          02220038
      GO TO 5261                                                        02230038
25250 IVFAIL = IVFAIL + 1                                               02240038
      IVCORR = 23                                                       02250038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02260038
C                                                                       02270038
C     TEST 526 THROUGH TEST 531 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS02280038
C     OF THE FORM   IV = (IC/IC)/IC.                                    02290038
C     TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND          02300038
C     NEGATIVE CONSTANTS ARE INCLUDED.                                  02310038
C                                                                       02320038
 5261 CONTINUE                                                          02330038
      IVTNUM = 526                                                      02340038
C                                                                       02350038
C      ****  TEST 526  ****                                             02360038
C                                                                       02370038
      IF (ICZERO) 35260, 5260, 35260                                    02380038
 5260 CONTINUE                                                          02390038
      IVCOMP = (24/3)/3                                                 02400038
      GO TO 45260                                                       02410038
35260 IVDELE = IVDELE + 1                                               02420038
      WRITE (I02,80003) IVTNUM                                          02430038
      IF (ICZERO) 45260, 5271, 45260                                    02440038
45260 IF (IVCOMP - 2) 25260,15260,25260                                 02450038
15260 IVPASS = IVPASS + 1                                               02460038
      WRITE (I02,80001) IVTNUM                                          02470038
      GO TO 5271                                                        02480038
25260 IVFAIL = IVFAIL + 1                                               02490038
      IVCORR = 2                                                        02500038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02510038
 5271 CONTINUE                                                          02520038
      IVTNUM = 527                                                      02530038
C                                                                       02540038
C      ****  TEST 527  ****                                             02550038
C                                                                       02560038
      IF (ICZERO) 35270, 5270, 35270                                    02570038
 5270 CONTINUE                                                          02580038
      IVCOMP = (7151/3)/10                                              02590038
      GO TO 45270                                                       02600038
35270 IVDELE = IVDELE + 1                                               02610038
      WRITE (I02,80003) IVTNUM                                          02620038
      IF (ICZERO) 45270, 5281, 45270                                    02630038
45270 IF (IVCOMP - 238) 25270,15270,25270                               02640038
15270 IVPASS = IVPASS + 1                                               02650038
      WRITE (I02,80001) IVTNUM                                          02660038
      GO TO 5281                                                        02670038
25270 IVFAIL = IVFAIL + 1                                               02680038
      IVCORR = 238                                                      02690038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02700038
 5281 CONTINUE                                                          02710038
      IVTNUM = 528                                                      02720038
C                                                                       02730038
C      ****  TEST 528  ****                                             02740038
C                                                                       02750038
      IF (ICZERO) 35280, 5280, 35280                                    02760038
 5280 CONTINUE                                                          02770038
      IVCOMP = (-24/3)/3                                                02780038
      GO TO 45280                                                       02790038
35280 IVDELE = IVDELE + 1                                               02800038
      WRITE (I02,80003) IVTNUM                                          02810038
      IF (ICZERO) 45280, 5291, 45280                                    02820038
45280 IF (IVCOMP + 2) 25280,15280,25280                                 02830038
15280 IVPASS = IVPASS + 1                                               02840038
      WRITE (I02,80001) IVTNUM                                          02850038
      GO TO 5291                                                        02860038
25280 IVFAIL = IVFAIL + 1                                               02870038
      IVCORR = -2                                                       02880038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02890038
 5291 CONTINUE                                                          02900038
      IVTNUM = 529                                                      02910038
C                                                                       02920038
C      ****  TEST 529  ****                                             02930038
C                                                                       02940038
      IF (ICZERO) 35290, 5290, 35290                                    02950038
 5290 CONTINUE                                                          02960038
      IVCOMP = (7151/(-3))/10                                           02970038
      GO TO 45290                                                       02980038
35290 IVDELE = IVDELE + 1                                               02990038
      WRITE (I02,80003) IVTNUM                                          03000038
      IF (ICZERO) 45290, 5301, 45290                                    03010038
45290 IF (IVCOMP + 238) 25290,15290,25290                               03020038
15290 IVPASS = IVPASS + 1                                               03030038
      WRITE (I02,80001) IVTNUM                                          03040038
      GO TO 5301                                                        03050038
25290 IVFAIL = IVFAIL + 1                                               03060038
      IVCORR = -238                                                     03070038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03080038
 5301 CONTINUE                                                          03090038
      IVTNUM = 530                                                      03100038
C                                                                       03110038
C      ****  TEST 530  ****                                             03120038
C                                                                       03130038
      IF (ICZERO) 35300, 5300, 35300                                    03140038
 5300 CONTINUE                                                          03150038
      IVCOMP = (15248/(-51))/(-23)                                      03160038
      GO TO 45300                                                       03170038
35300 IVDELE = IVDELE + 1                                               03180038
      WRITE (I02,80003) IVTNUM                                          03190038
      IF (ICZERO) 45300, 5311, 45300                                    03200038
45300 IF (IVCOMP - 12) 25300,15300,25300                                03210038
15300 IVPASS = IVPASS + 1                                               03220038
      WRITE (I02,80001) IVTNUM                                          03230038
      GO TO 5311                                                        03240038
25300 IVFAIL = IVFAIL + 1                                               03250038
      IVCORR = 12                                                       03260038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03270038
 5311 CONTINUE                                                          03280038
      IVTNUM = 531                                                      03290038
C                                                                       03300038
C      ****  TEST 531  ****                                             03310038
C                                                                       03320038
      IF (ICZERO) 35310, 5310, 35310                                    03330038
 5310 CONTINUE                                                          03340038
      IVCOMP = ((-27342)/(-4))/(-3)                                     03350038
      GO TO 45310                                                       03360038
35310 IVDELE = IVDELE + 1                                               03370038
      WRITE (I02,80003) IVTNUM                                          03380038
      IF (ICZERO) 45310, 5321, 45310                                    03390038
45310 IF (IVCOMP + 2278) 25310,15310,25310                              03400038
15310 IVPASS = IVPASS + 1                                               03410038
      WRITE (I02,80001) IVTNUM                                          03420038
      GO TO 5321                                                        03430038
25310 IVFAIL = IVFAIL + 1                                               03440038
      IVCORR = -2278                                                    03450038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03460038
C                                                                       03470038
C     TEST 532 THROUGH TEST 537 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS03480038
C     OF THE FORM   IV = IC/(IC/IC).                                    03490038
C     NO TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND       03500038
C     NEGATIVE CONSTANTS ARE INCLUDED.                                  03510038
C                                                                       03520038
 5321 CONTINUE                                                          03530038
      IVTNUM = 532                                                      03540038
C                                                                       03550038
C      ****  TEST 532  ****                                             03560038
C                                                                       03570038
      IF (ICZERO) 35320, 5320, 35320                                    03580038
 5320 CONTINUE                                                          03590038
      IVCOMP = 24/(8/4)                                                 03600038
      GO TO 45320                                                       03610038
35320 IVDELE = IVDELE + 1                                               03620038
      WRITE (I02,80003) IVTNUM                                          03630038
      IF (ICZERO) 45320, 5331, 45320                                    03640038
45320 IF (IVCOMP - 12) 25320,15320,25320                                03650038
15320 IVPASS = IVPASS + 1                                               03660038
      WRITE (I02,80001) IVTNUM                                          03670038
      GO TO 5331                                                        03680038
25320 IVFAIL = IVFAIL + 1                                               03690038
      IVCORR = 12                                                       03700038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03710038
 5331 CONTINUE                                                          03720038
      IVTNUM = 533                                                      03730038
C                                                                       03740038
C      ****  TEST 533  ****                                             03750038
C                                                                       03760038
      IF (ICZERO) 35330, 5330, 35330                                    03770038
 5330 CONTINUE                                                          03780038
      IVCOMP = 7150/(25/5)                                              03790038
      GO TO 45330                                                       03800038
35330 IVDELE = IVDELE + 1                                               03810038
      WRITE (I02,80003) IVTNUM                                          03820038
      IF (ICZERO) 45330, 5341, 45330                                    03830038
45330 IF (IVCOMP - 1430) 25330,15330,25330                              03840038
15330 IVPASS = IVPASS + 1                                               03850038
      WRITE (I02,80001) IVTNUM                                          03860038
      GO TO 5341                                                        03870038
25330 IVFAIL = IVFAIL + 1                                               03880038
      IVCORR = 1430                                                     03890038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03900038
 5341 CONTINUE                                                          03910038
      IVTNUM = 534                                                      03920038
C                                                                       03930038
C      ****  TEST 534  ****                                             03940038
C                                                                       03950038
      IF (ICZERO) 35340, 5340, 35340                                    03960038
 5340 CONTINUE                                                          03970038
      IVCOMP = -24/(8/4)                                                03980038
      GO TO 45340                                                       03990038
35340 IVDELE = IVDELE + 1                                               04000038
      WRITE (I02,80003) IVTNUM                                          04010038
      IF (ICZERO) 45340, 5351, 45340                                    04020038
45340 IF (IVCOMP + 12) 25340,15340,25340                                04030038
15340 IVPASS = IVPASS + 1                                               04040038
      WRITE (I02,80001) IVTNUM                                          04050038
      GO TO 5351                                                        04060038
25340 IVFAIL = IVFAIL + 1                                               04070038
      IVCORR = -12                                                      04080038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04090038
 5351 CONTINUE                                                          04100038
      IVTNUM = 535                                                      04110038
C                                                                       04120038
C      ****  TEST 535  ****                                             04130038
C                                                                       04140038
      IF (ICZERO) 35350, 5350, 35350                                    04150038
 5350 CONTINUE                                                          04160038
      IVCOMP = 24/((-8)/4)                                              04170038
      GO TO 45350                                                       04180038
35350 IVDELE = IVDELE + 1                                               04190038
      WRITE (I02,80003) IVTNUM                                          04200038
      IF (ICZERO) 45350, 5361, 45350                                    04210038
45350 IF (IVCOMP + 12) 25350,15350,25350                                04220038
15350 IVPASS = IVPASS + 1                                               04230038
      WRITE (I02,80001) IVTNUM                                          04240038
      GO TO 5361                                                        04250038
25350 IVFAIL = IVFAIL + 1                                               04260038
      IVCORR = -12                                                      04270038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04280038
 5361 CONTINUE                                                          04290038
      IVTNUM = 536                                                      04300038
C                                                                       04310038
C      ****  TEST 536  ****                                             04320038
C                                                                       04330038
      IF (ICZERO) 35360, 5360, 35360                                    04340038
 5360 CONTINUE                                                          04350038
      IVCOMP = (-7150)/((-25)/(-5))                                     04360038
      GO TO 45360                                                       04370038
35360 IVDELE = IVDELE + 1                                               04380038
      WRITE (I02,80003) IVTNUM                                          04390038
      IF (ICZERO) 45360, 5371, 45360                                    04400038
45360 IF (IVCOMP + 1430) 25360,15360,25360                              04410038
15360 IVPASS = IVPASS + 1                                               04420038
      WRITE (I02,80001) IVTNUM                                          04430038
      GO TO 5371                                                        04440038
25360 IVFAIL = IVFAIL + 1                                               04450038
      IVCORR = -1430                                                    04460038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04470038
 5371 CONTINUE                                                          04480038
      IVTNUM = 537                                                      04490038
C                                                                       04500038
C      ****  TEST 537  ****                                             04510038
C                                                                       04520038
      IF (ICZERO) 35370, 5370, 35370                                    04530038
 5370 CONTINUE                                                          04540038
      IVCOMP = -7150/(25/(-5))                                          04550038
      GO TO 45370                                                       04560038
35370 IVDELE = IVDELE + 1                                               04570038
      WRITE (I02,80003) IVTNUM                                          04580038
      IF (ICZERO) 45370, 5381, 45370                                    04590038
45370 IF (IVCOMP - 1430) 25370,15370,25370                              04600038
15370 IVPASS = IVPASS + 1                                               04610038
      WRITE (I02,80001) IVTNUM                                          04620038
      GO TO 5381                                                        04630038
25370 IVFAIL = IVFAIL + 1                                               04640038
      IVCORR = 1430                                                     04650038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04660038
C                                                                       04670038
C     TEST 538 THROUGH TEST 543 CONTAIN ARITHMETIC ASSIGMMENT STATEMENTS04680038
C     OF THE FORM   IV = IC/(IC/IC).                                    04690038
C     TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND          04700038
C     NEGATIVE CONSTANTS ARE INCLUDED.                                  04710038
C                                                                       04720038
 5381 CONTINUE                                                          04730038
      IVTNUM = 538                                                      04740038
C                                                                       04750038
C      ****  TEST 538  ****                                             04760038
C                                                                       04770038
      IF (ICZERO) 35380, 5380, 35380                                    04780038
 5380 CONTINUE                                                          04790038
      IVCOMP = 29/(5/2)                                                 04800038
      GO TO 45380                                                       04810038
35380 IVDELE = IVDELE + 1                                               04820038
      WRITE (I02,80003) IVTNUM                                          04830038
      IF (ICZERO) 45380, 5391, 45380                                    04840038
45380 IF (IVCOMP - 14) 25380,15380,25380                                04850038
15380 IVPASS = IVPASS + 1                                               04860038
      WRITE (I02,80001) IVTNUM                                          04870038
      GO TO 5391                                                        04880038
25380 IVFAIL = IVFAIL + 1                                               04890038
      IVCORR = 14                                                       04900038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04910038
 5391 CONTINUE                                                          04920038
      IVTNUM = 539                                                      04930038
C                                                                       04940038
C      ****  TEST 539  ****                                             04950038
C                                                                       04960038
      IF (ICZERO) 35390, 5390, 35390                                    04970038
 5390 CONTINUE                                                          04980038
      IVCOMP = 7154/(26/5)                                              04990038
      GO TO 45390                                                       05000038
35390 IVDELE = IVDELE + 1                                               05010038
      WRITE (I02,80003) IVTNUM                                          05020038
      IF (ICZERO) 45390, 5401, 45390                                    05030038
45390 IF (IVCOMP - 1430) 25390,15390,25390                              05040038
15390 IVPASS = IVPASS + 1                                               05050038
      WRITE (I02,80001) IVTNUM                                          05060038
      GO TO 5401                                                        05070038
25390 IVFAIL = IVFAIL + 1                                               05080038
      IVCORR = 1430                                                     05090038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05100038
 5401 CONTINUE                                                          05110038
      IVTNUM = 540                                                      05120038
C                                                                       05130038
C      ****  TEST 540  ****                                             05140038
C                                                                       05150038
      IF (ICZERO) 35400, 5400, 35400                                    05160038
 5400 CONTINUE                                                          05170038
      IVCOMP = -7154/(26/5)                                             05180038
      GO TO 45400                                                       05190038
35400 IVDELE = IVDELE + 1                                               05200038
      WRITE (I02,80003) IVTNUM                                          05210038
      IF (ICZERO) 45400, 5411, 45400                                    05220038
45400 IF (IVCOMP + 1430) 25400,15400,25400                              05230038
15400 IVPASS = IVPASS + 1                                               05240038
      WRITE (I02,80001) IVTNUM                                          05250038
      GO TO 5411                                                        05260038
25400 IVFAIL = IVFAIL + 1                                               05270038
      IVCORR = -1430                                                    05280038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05290038
 5411 CONTINUE                                                          05300038
      IVTNUM = 541                                                      05310038
C                                                                       05320038
C      ****  TEST 541  ****                                             05330038
C                                                                       05340038
      IF (ICZERO) 35410, 5410, 35410                                    05350038
 5410 CONTINUE                                                          05360038
      IVCOMP = (-7154)/((-26)/5)                                        05370038
      GO TO 45410                                                       05380038
35410 IVDELE = IVDELE + 1                                               05390038
      WRITE (I02,80003) IVTNUM                                          05400038
      IF (ICZERO) 45410, 5421, 45410                                    05410038
45410 IF (IVCOMP - 1430) 25410,15410,25410                              05420038
15410 IVPASS = IVPASS + 1                                               05430038
      WRITE (I02,80001) IVTNUM                                          05440038
      GO TO 5421                                                        05450038
25410 IVFAIL = IVFAIL + 1                                               05460038
      IVCORR = 1430                                                     05470038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05480038
 5421 CONTINUE                                                          05490038
      IVTNUM = 542                                                      05500038
C                                                                       05510038
C      ****  TEST 542  ****                                             05520038
C                                                                       05530038
      IF (ICZERO) 35420, 5420, 35420                                    05540038
 5420 CONTINUE                                                          05550038
      IVCOMP = 7154/((-26)/(-5))                                        05560038
      GO TO 45420                                                       05570038
35420 IVDELE = IVDELE + 1                                               05580038
      WRITE (I02,80003) IVTNUM                                          05590038
      IF (ICZERO) 45420, 5431, 45420                                    05600038
45420 IF (IVCOMP - 1430) 25420,15420,25420                              05610038
15420 IVPASS = IVPASS + 1                                               05620038
      WRITE (I02,80001) IVTNUM                                          05630038
      GO TO 5431                                                        05640038
25420 IVFAIL = IVFAIL + 1                                               05650038
      IVCORR = 1430                                                     05660038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05670038
 5431 CONTINUE                                                          05680038
      IVTNUM = 543                                                      05690038
C                                                                       05700038
C      ****  TEST 543  ****                                             05710038
C                                                                       05720038
      IF (ICZERO) 35430, 5430, 35430                                    05730038
 5430 CONTINUE                                                          05740038
      IVCOMP = (-7154)/((-26)/(-5))                                     05750038
      GO TO 45430                                                       05760038
35430 IVDELE = IVDELE + 1                                               05770038
      WRITE (I02,80003) IVTNUM                                          05780038
      IF (ICZERO) 45430, 5441, 45430                                    05790038
45430 IF (IVCOMP + 1430) 25430,15430,25430                              05800038
15430 IVPASS = IVPASS + 1                                               05810038
      WRITE (I02,80001) IVTNUM                                          05820038
      GO TO 5441                                                        05830038
25430 IVFAIL = IVFAIL + 1                                               05840038
      IVCORR = -1430                                                    05850038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05860038
C                                                                       05870038
C     TEST 544 THROUGH TEST 547 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS05880038
C     OF THE FORM   INTEGER VARIABLE = INTEGER VARIABLE/INTEGER CONSTANT05890038
C                                                                       05900038
 5441 CONTINUE                                                          05910038
      IVTNUM = 544                                                      05920038
C                                                                       05930038
C      ****  TEST 544  ****                                             05940038
C                                                                       05950038
      IF (ICZERO) 35440, 5440, 35440                                    05960038
 5440 CONTINUE                                                          05970038
      IVON01 = 75                                                       05980038
      IVCOMP = IVON01/25                                                05990038
      GO TO 45440                                                       06000038
35440 IVDELE = IVDELE + 1                                               06010038
      WRITE (I02,80003) IVTNUM                                          06020038
      IF (ICZERO) 45440, 5451, 45440                                    06030038
45440 IF (IVCOMP - 3) 25440,15440,25440                                 06040038
15440 IVPASS = IVPASS + 1                                               06050038
      WRITE (I02,80001) IVTNUM                                          06060038
      GO TO 5451                                                        06070038
25440 IVFAIL = IVFAIL + 1                                               06080038
      IVCORR = 3                                                        06090038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06100038
 5451 CONTINUE                                                          06110038
      IVTNUM = 545                                                      06120038
C                                                                       06130038
C      ****  TEST 545  ****                                             06140038
C                                                                       06150038
      IF (ICZERO) 35450, 5450, 35450                                    06160038
 5450 CONTINUE                                                          06170038
      IVON01 = -3575                                                    06180038
      IVCOMP = IVON01/25                                                06190038
      GO TO 45450                                                       06200038
35450 IVDELE = IVDELE + 1                                               06210038
      WRITE (I02,80003) IVTNUM                                          06220038
      IF (ICZERO) 45450, 5461, 45450                                    06230038
45450 IF (IVCOMP + 143) 25450,15450,25450                               06240038
15450 IVPASS = IVPASS + 1                                               06250038
      WRITE (I02,80001) IVTNUM                                          06260038
      GO TO 5461                                                        06270038
25450 IVFAIL = IVFAIL + 1                                               06280038
      IVCORR = -143                                                     06290038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06300038
 5461 CONTINUE                                                          06310038
      IVTNUM = 546                                                      06320038
C                                                                       06330038
C      ****  TEST 546  ****                                             06340038
C                                                                       06350038
      IF (ICZERO) 35460, 5460, 35460                                    06360038
 5460 CONTINUE                                                          06370038
      IVON01 = 3575                                                     06380038
      IVCOMP = IVON01/(-143)                                            06390038
      GO TO 45460                                                       06400038
35460 IVDELE = IVDELE + 1                                               06410038
      WRITE (I02,80003) IVTNUM                                          06420038
      IF (ICZERO) 45460, 5471, 45460                                    06430038
45460 IF (IVCOMP + 25) 25460,15460,25460                                06440038
15460 IVPASS = IVPASS + 1                                               06450038
      WRITE (I02,80001) IVTNUM                                          06460038
      GO TO 5471                                                        06470038
25460 IVFAIL = IVFAIL + 1                                               06480038
      IVCORR = -25                                                      06490038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06500038
 5471 CONTINUE                                                          06510038
      IVTNUM = 547                                                      06520038
C                                                                       06530038
C      ****  TEST 547  ****                                             06540038
C                                                                       06550038
      IF (ICZERO) 35470, 5470, 35470                                    06560038
 5470 CONTINUE                                                          06570038
      IVON01 = 959                                                      06580038
      IVCOMP = IVON01/120                                               06590038
      GO TO 45470                                                       06600038
35470 IVDELE = IVDELE + 1                                               06610038
      WRITE (I02,80003) IVTNUM                                          06620038
      IF (ICZERO) 45470, 5481, 45470                                    06630038
45470 IF (IVCOMP -7)  25470,15470,25470                                 06640038
15470 IVPASS = IVPASS + 1                                               06650038
      WRITE (I02,80001) IVTNUM                                          06660038
      GO TO 5481                                                        06670038
25470 IVFAIL = IVFAIL + 1                                               06680038
      IVCORR = 7                                                        06690038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06700038
C                                                                       06710038
C     TEST 548 THROUGH TEST 551 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS06720038
C     OF THE FORM   INTEGER VARIABLE =INTEGER CONSTANT/INTEGER VARIABLE.06730038
C                                                                       06740038
 5481 CONTINUE                                                          06750038
      IVTNUM = 548                                                      06760038
C                                                                       06770038
C      ****  TEST 548  ****                                             06780038
C                                                                       06790038
      IF (ICZERO) 35480, 5480, 35480                                    06800038
 5480 CONTINUE                                                          06810038
      IVON02 = 25                                                       06820038
      IVCOMP = 75/IVON02                                                06830038
      GO TO 45480                                                       06840038
35480 IVDELE = IVDELE + 1                                               06850038
      WRITE (I02,80003) IVTNUM                                          06860038
      IF (ICZERO) 45480, 5491, 45480                                    06870038
45480 IF (IVCOMP - 3) 25480,15480,25480                                 06880038
15480 IVPASS = IVPASS + 1                                               06890038
      WRITE (I02,80001) IVTNUM                                          06900038
      GO TO 5491                                                        06910038
25480 IVFAIL = IVFAIL + 1                                               06920038
      IVCORR = 3                                                        06930038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06940038
 5491 CONTINUE                                                          06950038
      IVTNUM = 549                                                      06960038
C                                                                       06970038
C      ****  TEST 549  ****                                             06980038
C                                                                       06990038
      IF (ICZERO) 35490, 5490, 35490                                    07000038
 5490 CONTINUE                                                          07010038
      IVON02 = -25                                                      07020038
      IVCOMP = 3579/IVON02                                              07030038
      GO TO 45490                                                       07040038
35490 IVDELE = IVDELE + 1                                               07050038
      WRITE (I02,80003) IVTNUM                                          07060038
      IF (ICZERO) 45490, 5501, 45490                                    07070038
45490 IF (IVCOMP + 143) 25490,15490,25490                               07080038
15490 IVPASS = IVPASS + 1                                               07090038
      WRITE (I02,80001) IVTNUM                                          07100038
      GO TO 5501                                                        07110038
25490 IVFAIL = IVFAIL + 1                                               07120038
      IVCORR = -143                                                     07130038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07140038
 5501 CONTINUE                                                          07150038
      IVTNUM = 550                                                      07160038
C                                                                       07170038
C      ****  TEST 550  ****                                             07180038
C                                                                       07190038
      IF (ICZERO) 35500, 5500, 35500                                    07200038
 5500 CONTINUE                                                          07210038
      IVON02 = -143                                                     07220038
      IVCOMP = (-3575)/IVON02                                           07230038
      GO TO 45500                                                       07240038
35500 IVDELE = IVDELE + 1                                               07250038
      WRITE (I02,80003) IVTNUM                                          07260038
      IF (ICZERO) 45500, 5511, 45500                                    07270038
45500 IF (IVCOMP - 25) 25500,15500,25500                                07280038
15500 IVPASS = IVPASS + 1                                               07290038
      WRITE (I02,80001) IVTNUM                                          07300038
      GO TO 5511                                                        07310038
25500 IVFAIL = IVFAIL + 1                                               07320038
      IVCORR = 25                                                       07330038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07340038
 5511 CONTINUE                                                          07350038
      IVTNUM = 551                                                      07360038
C                                                                       07370038
C      ****  TEST 551  ****                                             07380038
C                                                                       07390038
      IF (ICZERO) 35510, 5510, 35510                                    07400038
 5510 CONTINUE                                                          07410038
      IVON02 = 120                                                      07420038
      IVCOMP = -959/IVON02                                              07430038
      GO TO 45510                                                       07440038
35510 IVDELE = IVDELE + 1                                               07450038
      WRITE (I02,80003) IVTNUM                                          07460038
      IF (ICZERO) 45510, 5521, 45510                                    07470038
45510 IF (IVCOMP + 7) 25510,15510,25510                                 07480038
15510 IVPASS = IVPASS + 1                                               07490038
      WRITE (I02,80001) IVTNUM                                          07500038
      GO TO 5521                                                        07510038
25510 IVFAIL = IVFAIL + 1                                               07520038
      IVCORR = -7                                                       07530038
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07540038
C      ****    END OF TESTS    ****                                     07550038
 5521 CONTINUE                                                          07560038
C                                                                       07570038
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07580038
99999 CONTINUE                                                          07590038
      WRITE (I02,90002)                                                 07600038
      WRITE (I02,90006)                                                 07610038
      WRITE (I02,90002)                                                 07620038
      WRITE (I02,90002)                                                 07630038
      WRITE (I02,90007)                                                 07640038
      WRITE (I02,90002)                                                 07650038
      WRITE (I02,90008)  IVFAIL                                         07660038
      WRITE (I02,90009) IVPASS                                          07670038
      WRITE (I02,90010) IVDELE                                          07680038
C                                                                       07690038
C                                                                       07700038
C     TERMINATE ROUTINE EXECUTION                                       07710038
      STOP                                                              07720038
C                                                                       07730038
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07740038
90000 FORMAT (1H1)                                                      07750038
90002 FORMAT (1H )                                                      07760038
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07770038
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07780038
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07790038
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07800038
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07810038
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07820038
C                                                                       07830038
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07840038
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07850038
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07860038
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07870038
C                                                                       07880038
C     FORMAT STATEMENTS FOR TEST RESULTS                                07890038
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07900038
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07910038
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07920038
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07930038
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07940038
C                                                                       07950038
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM038)                          07960038
      END                                                               07970038
