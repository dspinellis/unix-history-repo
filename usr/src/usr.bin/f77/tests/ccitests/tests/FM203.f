      PROGRAM FM203                                                     00010203
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020203
C                                                                       00030203
C         THIS ROUTINE CONTINUES THE TESTING OF CHARACTER DATA TYPES    00040203
C     WHICH WAS STARTED IN FM202.  THE CHARACTER TYPE-STATEMENTS SPECIFY00050203
C     CHARACTER VARIABLES AND ONE-DIMENSIONAL CHARACTER ARRAYS OF       00060203
C     LENGTH ONE AND LENGTH TWO.  THE TESTS IN THIS ROUTINE DETERMINE   00070203
C     THAT THE FOLLOWING LANGUAGE FEATURES FUNCTION CORRECTLY.          00080203
C                                                                       00090203
C         (1)  CHARACTER ASSIGNMENT STATEMENTS OF THE FORM              00100203
C                                                                       00110203
C         CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                  00120203
C         CHARACTER ARRAY ELEMENT = CHARACTER VARIABLE                  00130203
C         CHARACTER ARRAY ELEMENT = CHARACTER ARRAY ELEMENT             00140203
C         CHARACTER VARIABLE = CHARACTER ARRAY ELEMENT                  00150203
C                                                                       00160203
C     WHERE THE ARRAY ELEMENTS, VARIABLES AND CONSTANTS ARE OF LENGTH   00170203
C     ONE OR TWO.                                                       00180203
C                                                                       00190203
C         (2)  CHARACTER RELATIONAL EXPRESSIONS OF THE FORM             00200203
C                                                                       00210203
C         CHARACTER ARRAY ELEMENT RELOP CHARACTER CONSTANT              00220203
C         CHARACTER ARRAY ELEMENT RELOP CHARACTER VARIABLE              00230203
C         CHARACTER ARRAY ELEMENT RELOP CHARACTER ARRAY ELEMENT         00240203
C                                                                       00250203
C     WHERE THE ARRAY ELEMENTS, VARIABLES AND CONSTANTS ARE OF LENGTH   00260203
C     ONE OR TWO.                                                       00270203
C                                                                       00280203
C         (3)  CHARACTER EXPRESSIONS ENCLOSED IN PARENTHESES.  THE FORMS00290203
C     TESTED ARE                                                        00300203
C                                                                       00310203
C         (CHARACTER CONSTANT)                                          00320203
C         (CHARACTER VARIABLE)                                          00330203
C         (CHARACTER ARRAY ELEMENT)                                     00340203
C         ((CHARACTER ARRAY ELEMENT))                                   00350203
C                                                                       00360203
C         (4)  CHARACTER RELATIONAL EXPRESSIONS OF THE FORM             00370203
C                                                                       00380203
C         CHARACTER ARRAY ELEMENT .EQ. CHARACTER CONSTANT               00390203
C                                                                       00400203
C     ARE USED IN THIS ROUTINE TO VERIFY THE CHARACTER ASSIGNMENT       00410203
C     STATEMENTS.                                                       00420203
C                                                                       00430203
C     REFERENCES                                                        00440203
C         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,      00450203
C              X3.9-1978                                                00460203
C                                                                       00470203
C         SECTION 4.8,    CHARACTER TYPE                                00480203
C         SECTION 4.8.1,  CHARACTER CONSTANT                            00490203
C         SECTION 6.2,    CHARACTER EXPRESSIONS                         00500203
C         SECTION 6.3.4,  CHARACTER RELATIONAL EXPRESSION               00510203
C         SECTION 6.3.5,  INTERPRETATION OF CHARACTER RELATIONAL        00520203
C                           EXPRESSIONS                                 00530203
C         SECTION 8.4.2,  CHARACTER TYPE-STATEMENT                      00540203
C         SECTION 10.4,   CHARACTER ASSIGNMENT STATEMENT                00550203
C                                                                       00560203
C                                                                       00570203
C                                                                       00580203
C     ******************************************************************00590203
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00600203
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00610203
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00620203
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00630203
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00640203
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00650203
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00660203
C     THE RESULT OF EXECUTING THESE TESTS.                              00670203
C                                                                       00680203
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00690203
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00700203
C                                                                       00710203
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00720203
C                    DEPARTMENT OF THE NAVY                             00730203
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00740203
C                    WASHINGTON, D.C.   20376                           00750203
C                                                                       00760203
C     ******************************************************************00770203
C                                                                       00780203
C                                                                       00790203
      IMPLICIT LOGICAL (L)                                              00800203
      IMPLICIT CHARACTER*14 (C)                                         00810203
C                                                                       00820203
      CHARACTER CATN11(5), CVTN01, CATN12(5), CVTN02                    00830203
      CHARACTER*2  CATN13, CVTN03, CATN14(5), CVTN04                    00840203
      DIMENSION CATN13(5)                                               00850203
C                                                                       00860203
C                                                                       00870203
C                                                                       00880203
C     INITIALIZATION SECTION.                                           00890203
C                                                                       00900203
C     INITIALIZE CONSTANTS                                              00910203
C     ********************                                              00920203
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00930203
      I01 = 5                                                           00940203
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00950203
      I02 = 6                                                           00960203
C     SYSTEM ENVIRONMENT SECTION                                        00970203
C                                                                       00980203
      I01 = 5                                                           00990203
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01000203
C     (UNIT NUMBER FOR CARD READER).                                    01010203
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01020203
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01030203
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01040203
C                                                                       01050203
      I02 = 6                                                           01060203
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01070203
C     (UNIT NUMBER FOR PRINTER).                                        01080203
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01090203
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01100203
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01110203
C                                                                       01120203
      IVPASS = 0                                                        01130203
      IVFAIL = 0                                                        01140203
      IVDELE = 0                                                        01150203
      ICZERO = 0                                                        01160203
C                                                                       01170203
C     WRITE OUT PAGE HEADERS                                            01180203
C                                                                       01190203
      WRITE (I02,90002)                                                 01200203
      WRITE (I02,90006)                                                 01210203
      WRITE (I02,90008)                                                 01220203
      WRITE (I02,90004)                                                 01230203
      WRITE (I02,90010)                                                 01240203
      WRITE (I02,90004)                                                 01250203
      WRITE (I02,90016)                                                 01260203
      WRITE (I02,90001)                                                 01270203
      WRITE (I02,90004)                                                 01280203
      WRITE (I02,90012)                                                 01290203
      WRITE (I02,90014)                                                 01300203
      WRITE (I02,90004)                                                 01310203
C                                                                       01320203
C                                                                       01330203
C         TEST 31 THROUGH TEST 33 VERIFY THAT THE CHARACTER ASSIGNMENT  01340203
C     STATEMENT                                                         01350203
C                                                                       01360203
C         CHARACTER ARRAY ELEMENT (LEN 1) = CHARACTER CONSTANT (LEN 1)  01370203
C                                                                       01380203
C     IS CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                  01390203
C                                                                       01400203
C       CHARACTER ARRAY ELEMENT (LEN 1) .EQ. CHARACTER CONSTANT (LEN 1) 01410203
C                                                                       01420203
C     IS USED TO VERIFY THE ASSIGNMENT STATEMENT.  BOTH OF THE ABOVE    01430203
C     STATEMENT FORMS MUST MEET THE LANGUAGE SPECIFICATIONS FOR THESE   01440203
C     TESTS TO PASS.                                                    01450203
C                                                                       01460203
C         THE TWO ARRAYS USED IN THESE TESTS ARE CATN11(5) AND CATN12(5)01470203
C     THE ARRAYS ARE INITIALIZED TO A BLANK CHARACTER BY THE DO-LOOP    01480203
C                                                                       01490203
      DO 312 I= 1,5                                                     01500203
      CATN11(I) = ' '                                                   01510203
      CATN12(I) = ' '                                                   01520203
  312 CONTINUE                                                          01530203
C                                                                       01540203
C     ****  FCVS PROGRAM 203  -  TEST 031  ****                         01550203
C                                                                       01560203
C                                                                       01570203
      IVTNUM =  31                                                      01580203
      IF (ICZERO) 30310, 0310, 30310                                    01590203
 0310 CONTINUE                                                          01600203
      IVCOMP = 0                                                        01610203
      IVCORR = 1                                                        01620203
      CATN11(2) = 'V'                                                   01630203
      IF (CATN11(2) .EQ. 'V') IVCOMP = 1                                01640203
40310 IF (IVCOMP - 1) 20310,10310,20310                                 01650203
30310 IVDELE = IVDELE + 1                                               01660203
      WRITE (I02,80000) IVTNUM                                          01670203
      IF (ICZERO) 10310, 0321, 20310                                    01680203
10310 IVPASS = IVPASS + 1                                               01690203
      WRITE (I02,80002) IVTNUM                                          01700203
      GO TO 0321                                                        01710203
20310 IVFAIL = IVFAIL + 1                                               01720203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01730203
 0321 CONTINUE                                                          01740203
C                                                                       01750203
C     ****  FCVS PROGRAM 203  -  TEST 032  ****                         01760203
C                                                                       01770203
C                                                                       01780203
      IVTNUM =  32                                                      01790203
      IF (ICZERO) 30320, 0320, 30320                                    01800203
 0320 CONTINUE                                                          01810203
      IVCOMP=0                                                          01820203
      IVCORR=1                                                          01830203
      CATN11(3) = '+'                                                   01840203
      IF (CATN11(3) .EQ. '+') IVCOMP = 1                                01850203
40320 IF (IVCOMP - 1) 20320,10320,20320                                 01860203
30320 IVDELE = IVDELE + 1                                               01870203
      WRITE (I02,80000) IVTNUM                                          01880203
      IF (ICZERO) 10320, 0331, 20320                                    01890203
10320 IVPASS = IVPASS + 1                                               01900203
      WRITE (I02,80002) IVTNUM                                          01910203
      GO TO 0331                                                        01920203
20320 IVFAIL = IVFAIL + 1                                               01930203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01940203
 0331 CONTINUE                                                          01950203
C                                                                       01960203
C     ****  FCVS PROGRAM 203  -  TEST 033  ****                         01970203
C                                                                       01980203
C                                                                       01990203
      IVTNUM =  33                                                      02000203
      IF (ICZERO) 30330, 0330, 30330                                    02010203
 0330 CONTINUE                                                          02020203
      IVCOMP = 0                                                        02030203
      IVCORR = 1                                                        02040203
      CATN11 (4) = '7'                                                  02050203
      IF (CATN11 (4) .EQ. '7') IVCOMP = 1                               02060203
40330 IF (IVCOMP -1) 20330,10330,20330                                  02070203
30330 IVDELE = IVDELE + 1                                               02080203
      WRITE (I02,80000) IVTNUM                                          02090203
      IF (ICZERO) 10330, 0341, 20330                                    02100203
10330 IVPASS = IVPASS + 1                                               02110203
      WRITE (I02,80002) IVTNUM                                          02120203
      GO TO 0341                                                        02130203
20330 IVFAIL = IVFAIL + 1                                               02140203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02150203
 0341 CONTINUE                                                          02160203
C                                                                       02170203
C         TEST 34 THROUGH TEST 36 VERIFY THAT THE CHARACTER ASSIGNMENT  02180203
C     STATEMENTS                                                        02190203
C                                                                       02200203
C         CHARACTER VARIABLE (LEN 1) = CHARACTER CONSTANT (LEN 1)       02210203
C         CHARACTER ARRAY ELEMENT (LEN1) = CHARACTER VARIABLE (LEN1)    02220203
C                                                                       02230203
C     ARE CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                 02240203
C                                                                       02250203
C         CHARACTER ARRAY ELEMENT (LEN1) .EQ. CHAR. CONSTANT (LEN1)     02260203
C                                                                       02270203
C     IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENTS.        02280203
C                                                                       02290203
C                                                                       02300203
C     ****  FCVS PROGRAM 203  -  TEST 034  ****                         02310203
C                                                                       02320203
C                                                                       02330203
      IVTNUM =  34                                                      02340203
      IF (ICZERO) 30340, 0340, 30340                                    02350203
 0340 CONTINUE                                                          02360203
      IVCOMP = 0                                                        02370203
      IVCORR = 1                                                        02380203
      CVTN01 = 'V'                                                      02390203
      CATN12(2) = CVTN01                                                02400203
      IF (CATN12(2) .EQ. 'V') IVCOMP = 1                                02410203
40340 IF (IVCOMP - 1) 20340,10340,20340                                 02420203
30340 IVDELE = IVDELE + 1                                               02430203
      WRITE (I02,80000) IVTNUM                                          02440203
      IF (ICZERO) 10340, 0351, 20340                                    02450203
10340 IVPASS = IVPASS + 1                                               02460203
      WRITE (I02,80002) IVTNUM                                          02470203
      GO TO 0351                                                        02480203
20340 IVFAIL = IVFAIL + 1                                               02490203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02500203
 0351 CONTINUE                                                          02510203
C                                                                       02520203
C     ****  FCVS PROGRAM 203  -  TEST 035  ****                         02530203
C                                                                       02540203
C                                                                       02550203
      IVTNUM =  35                                                      02560203
      IF (ICZERO) 30350, 0350, 30350                                    02570203
 0350 CONTINUE                                                          02580203
      IVCOMP = 0                                                        02590203
      IVCORR = 1                                                        02600203
      CVTN01 = '+'                                                      02610203
      CATN12(3) = CVTN01                                                02620203
      IF (CATN12(3) .EQ. '+') IVCOMP = 1                                02630203
40350 IF (IVCOMP - 1) 20350,10350,20350                                 02640203
30350 IVDELE = IVDELE + 1                                               02650203
      WRITE (I02,80000) IVTNUM                                          02660203
      IF (ICZERO) 10350, 0361, 20350                                    02670203
10350 IVPASS = IVPASS + 1                                               02680203
      WRITE (I02,80002) IVTNUM                                          02690203
      GO TO 0361                                                        02700203
20350 IVFAIL = IVFAIL + 1                                               02710203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02720203
 0361 CONTINUE                                                          02730203
C                                                                       02740203
C     ****  FCVS PROGRAM 203  -  TEST 036  ****                         02750203
C                                                                       02760203
C                                                                       02770203
      IVTNUM =  36                                                      02780203
      IF (ICZERO) 30360, 0360, 30360                                    02790203
 0360 CONTINUE                                                          02800203
      IVCOMP = 0                                                        02810203
      IVCORR = 1                                                        02820203
      CVTN01 = '7'                                                      02830203
      CATN12(4) = CVTN01                                                02840203
      IF (CATN12(4) .EQ. '7') IVCOMP = 1                                02850203
40360 IF (IVCOMP - 1) 20360,10360,20360                                 02860203
30360 IVDELE = IVDELE + 1                                               02870203
      WRITE (I02,80000) IVTNUM                                          02880203
      IF (ICZERO) 10360, 0371, 20360                                    02890203
10360 IVPASS = IVPASS + 1                                               02900203
      WRITE (I02,80002) IVTNUM                                          02910203
      GO TO 0371                                                        02920203
20360 IVFAIL = IVFAIL + 1                                               02930203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02940203
 0371 CONTINUE                                                          02950203
C                                                                       02960203
C         TEST 37 THROUGH TEST 39 VERIFY THAT THE CHARACTER ASSIGNMENT  02970203
C     STATEMENTS                                                        02980203
C                                                                       02990203
C         CHAR. ARRAY ELEMENT (LEN 1) = CHAR. CONSTANT (LEN 1)          03000203
C         CHAR. ARRAY ELEMENT (LEN 1) = CHAR. ARRAY ELEMENT (LEN 1)     03010203
C                                                                       03020203
C     ARE CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                 03030203
C                                                                       03040203
C         CHAR. ARRAY ELEMENT (LEN 1) .EQ. CHAR. CONSTANT (LEN 1)       03050203
C                                                                       03060203
C     IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENTS.        03070203
C                                                                       03080203
C                                                                       03090203
C     ****  FCVS PROGRAM 203  -  TEST 037  ****                         03100203
C                                                                       03110203
C                                                                       03120203
      IVTNUM =  37                                                      03130203
      IF (ICZERO) 30370, 0370, 30370                                    03140203
 0370 CONTINUE                                                          03150203
      IVCOMP = 1                                                        03160203
      IVCORR = 6                                                        03170203
      CATN11 (1) = 'V'                                                  03180203
      CATN12 (1) = CATN11 (1)                                           03190203
      IF (CATN12(1) .EQ. 'V') IVCOMP=IVCOMP*2                           03200203
      IF (CATN11(1) .EQ. 'V') IVCOMP=IVCOMP*3                           03210203
40370 IF (IVCOMP-6) 20370,10370,20370                                   03220203
30370 IVDELE = IVDELE + 1                                               03230203
      WRITE (I02,80000) IVTNUM                                          03240203
      IF (ICZERO) 10370, 0381, 20370                                    03250203
10370 IVPASS = IVPASS + 1                                               03260203
      WRITE (I02,80002) IVTNUM                                          03270203
      GO TO 0381                                                        03280203
20370 IVFAIL = IVFAIL + 1                                               03290203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03300203
 0381 CONTINUE                                                          03310203
C                                                                       03320203
C     ****  FCVS PROGRAM 203  -  TEST 038  ****                         03330203
C                                                                       03340203
C                                                                       03350203
      IVTNUM =  38                                                      03360203
      IF (ICZERO) 30380, 0380, 30380                                    03370203
 0380 CONTINUE                                                          03380203
      IVCOMP=1                                                          03390203
      IVCORR=6                                                          03400203
      CATN11(2) = '+'                                                   03410203
      CATN12(2) = CATN11(2)                                             03420203
      IF (CATN12(2) .EQ. '+') IVCOMP=IVCOMP*2                           03430203
      IF (CATN11(2) .EQ. '+') IVCOMP=IVCOMP*3                           03440203
40380 IF (IVCOMP - 6) 20380,10380,20380                                 03450203
30380 IVDELE = IVDELE + 1                                               03460203
      WRITE (I02,80000) IVTNUM                                          03470203
      IF (ICZERO) 10380, 0391, 20380                                    03480203
10380 IVPASS = IVPASS + 1                                               03490203
      WRITE (I02,80002) IVTNUM                                          03500203
      GO TO 0391                                                        03510203
20380 IVFAIL = IVFAIL + 1                                               03520203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03530203
 0391 CONTINUE                                                          03540203
C                                                                       03550203
C     ****  FCVS PROGRAM 203  -  TEST 039  ****                         03560203
C                                                                       03570203
C                                                                       03580203
      IVTNUM =  39                                                      03590203
      IF (ICZERO) 30390, 0390, 30390                                    03600203
 0390 CONTINUE                                                          03610203
      IVCOMP = 1                                                        03620203
      IVCORR = 6                                                        03630203
      CATN11 (3) = '7'                                                  03640203
      CATN12 (3) = CATN11 (3)                                           03650203
      IF (CATN12(3) .EQ. '7') IVCOMP = IVCOMP * 2                       03660203
      IF (CATN11(3) .EQ. '7') IVCOMP = IVCOMP * 3                       03670203
40390 IF (IVCOMP - 6) 20390,10390,20390                                 03680203
30390 IVDELE = IVDELE + 1                                               03690203
      WRITE (I02,80000) IVTNUM                                          03700203
      IF (ICZERO) 10390, 0401, 20390                                    03710203
10390 IVPASS = IVPASS + 1                                               03720203
      WRITE (I02,80002) IVTNUM                                          03730203
      GO TO 0401                                                        03740203
20390 IVFAIL = IVFAIL + 1                                               03750203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03760203
 0401 CONTINUE                                                          03770203
C                                                                       03780203
C         TEST 40 AND TEST 41 VERIFY THAT THE CHARACTER ASSIGNMENT      03790203
C     STATEMENTS                                                        03800203
C                                                                       03810203
C         CHAR. ARRAY ELEMENT (LEN 1) = CHAR. CONSTANT (LEN 1)          03820203
C         CHAR. VARIABLE (LEN 1) = CHAR. ARRAY ELEMENT (LEN 1)          03830203
C                                                                       03840203
C     ARE CORRECT.                                                      03850203
C                                                                       03860203
C                                                                       03870203
C     ****  FCVS PROGRAM 203  -  TEST 040  ****                         03880203
C                                                                       03890203
C                                                                       03900203
      IVTNUM =  40                                                      03910203
      IF (ICZERO) 30400, 0400, 30400                                    03920203
 0400 CONTINUE                                                          03930203
      IVCOMP = 0                                                        03940203
      IVCORR = 1                                                        03950203
      CATN11(4) = 'X'                                                   03960203
      CVTN02 = CATN11 (4)                                               03970203
      IF (CVTN02 .EQ. 'X') IVCOMP = 1                                   03980203
40400 IF (IVCOMP - 1) 20400,10400,20400                                 03990203
30400 IVDELE = IVDELE + 1                                               04000203
      WRITE (I02,80000) IVTNUM                                          04010203
      IF (ICZERO) 10400, 0411, 20400                                    04020203
10400 IVPASS = IVPASS + 1                                               04030203
      WRITE (I02,80002) IVTNUM                                          04040203
      GO TO 0411                                                        04050203
20400 IVFAIL = IVFAIL + 1                                               04060203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04070203
 0411 CONTINUE                                                          04080203
C                                                                       04090203
C     ****  FCVS PROGRAM 203  -  TEST 041  ****                         04100203
C                                                                       04110203
C                                                                       04120203
      IVTNUM =  41                                                      04130203
      IF (ICZERO) 30410, 0410, 30410                                    04140203
 0410 CONTINUE                                                          04150203
      IVCOMP = 0                                                        04160203
      IVCORR = 1                                                        04170203
      CATN11(3) = '-'                                                   04180203
      CVTN02 = CATN11(3)                                                04190203
      IF (CVTN02 .EQ. '-') IVCOMP=1                                     04200203
40410 IF (IVCOMP - 1) 20410,10410,20410                                 04210203
30410 IVDELE = IVDELE + 1                                               04220203
      WRITE (I02,80000) IVTNUM                                          04230203
      IF (ICZERO) 10410, 0421, 20410                                    04240203
10410 IVPASS = IVPASS + 1                                               04250203
      WRITE (I02,80002) IVTNUM                                          04260203
      GO TO 0421                                                        04270203
20410 IVFAIL = IVFAIL + 1                                               04280203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04290203
 0421 CONTINUE                                                          04300203
C                                                                       04310203
C         TEST 42 THROUGH TEST 44 VERIFY THE RESULTS OF CHARACTER       04320203
C     RELATIONAL EXPRESSIONS USING EACH OF THE SIX RELATIONAL OPERATORS.04330203
C     THE CHARACTER DATA 'A' AND '1' ARE COMPARED IN THE EXPRESSION     04340203
C     AND ARE INITIALIZED BY THE CHARACTER ASSIGNMENT STATEMENTS        04350203
C                                                                       04360203
      CATN11 (4) = 'A'                                                  04370203
      CATN12 (3) = '1'                                                  04380203
      CVTN01 = 'A'                                                      04390203
      CVTN02 = '1'                                                      04400203
C                                                                       04410203
C     ****  FCVS PROGRAM 203  -  TEST 042  ****                         04420203
C                                                                       04430203
C         RELATIONAL OPERATORS .NE. AND .EQ.                            04440203
C         CHAR. ARRAY ELEMENT (LEN 1) RELOP CHAR. CONSTANT (LEN 1)      04450203
C                                                                       04460203
      IVTNUM =  42                                                      04470203
      IF (ICZERO) 30420, 0420, 30420                                    04480203
 0420 CONTINUE                                                          04490203
      IVCOMP = 1                                                        04500203
      IVCORR = 3                                                        04510203
      IF (CATN11(4) .EQ. '1') IVCOMP=IVCOMP*2                           04520203
      IF ('A' .NE. CATN12(3)) IVCOMP=IVCOMP*3                           04530203
40420 IF (IVCOMP - 3) 20420,10420,20420                                 04540203
30420 IVDELE = IVDELE + 1                                               04550203
      WRITE (I02,80000) IVTNUM                                          04560203
      IF (ICZERO) 10420, 0431, 20420                                    04570203
10420 IVPASS = IVPASS + 1                                               04580203
      WRITE (I02,80002) IVTNUM                                          04590203
      GO TO 0431                                                        04600203
20420 IVFAIL = IVFAIL + 1                                               04610203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04620203
 0431 CONTINUE                                                          04630203
C                                                                       04640203
C     ****  FCVS PROGRAM 203  -  TEST 043  ****                         04650203
C                                                                       04660203
C         RELATIONAL OPERATORS .LE. AND .GE.                            04670203
C         CHAR. ARRAY ELEMENT (LEN 1) RELOP CHAR. VARIABLE (LEN 1)      04680203
C                                                                       04690203
      IVTNUM =  43                                                      04700203
      IF (ICZERO) 30430, 0430, 30430                                    04710203
 0430 CONTINUE                                                          04720203
      IVCOMP = 0                                                        04730203
      IVCORR = 1                                                        04740203
      IF (CATN11(4) .LE. CVTN02) IVCOMP=IVCOMP+1                        04750203
      IF (CVTN01 .GE. CATN12(3)) IVCOMP=IVCOMP+1                        04760203
40430 IF (IVCOMP - 1) 20430,10430,20430                                 04770203
30430 IVDELE = IVDELE + 1                                               04780203
      WRITE (I02,80000) IVTNUM                                          04790203
      IF (ICZERO) 10430, 0441, 20430                                    04800203
10430 IVPASS = IVPASS + 1                                               04810203
      WRITE (I02,80002) IVTNUM                                          04820203
      GO TO 0441                                                        04830203
20430 IVFAIL = IVFAIL + 1                                               04840203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04850203
 0441 CONTINUE                                                          04860203
C                                                                       04870203
C     ****  FCVS PROGRAM 203  -  TEST 044  ****                         04880203
C                                                                       04890203
C         RELATIONAL OPERATORS .LT. AND .GT.                            04900203
C         CHAR. ARRAY ELEMENT (LEN 1) RELOP CHAR. ARRAY ELEMENT (LEN 1) 04910203
C                                                                       04920203
      IVTNUM =  44                                                      04930203
      IF (ICZERO) 30440, 0440, 30440                                    04940203
 0440 CONTINUE                                                          04950203
      IVCOMP = 0                                                        04960203
      IVCORR = 1                                                        04970203
      IF (CATN11(4) .LT. CATN12(3)) IVCOMP=IVCOMP+1                     04980203
      IF (CATN11(4) .GT. CATN12(3)) IVCOMP=IVCOMP+1                     04990203
40440 IF (IVCOMP - 1) 20440,10440,20440                                 05000203
30440 IVDELE = IVDELE + 1                                               05010203
      WRITE (I02,80000) IVTNUM                                          05020203
      IF (ICZERO) 10440, 0451, 20440                                    05030203
10440 IVPASS = IVPASS + 1                                               05040203
      WRITE (I02,80002) IVTNUM                                          05050203
      GO TO 0451                                                        05060203
20440 IVFAIL = IVFAIL + 1                                               05070203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05080203
 0451 CONTINUE                                                          05090203
C                                                                       05100203
C     ****  FCVS PROGRAM 203  -  TEST 045  ****                         05110203
C                                                                       05120203
C         TEST 45 VERIFIES THAT THE LAST ELEMENTS OF THE ARRAYS USED    05130203
C     IN TEST 31 THROUGH TEST 44 WERE NOT AFFECTED BY THE SETTING       05140203
C     OF OTHER CHARACTER ARRAY ELEMENTS.                                05150203
C                                                                       05160203
      IVTNUM =  45                                                      05170203
      IF (ICZERO) 30450, 0450, 30450                                    05180203
 0450 CONTINUE                                                          05190203
      IVCOMP = 1                                                        05200203
      IVCORR = 30                                                       05210203
      IF (CATN11(5) .EQ. ' ') IVCOMP=IVCOMP*2                           05220203
      IF (CATN12(5) .EQ. ' ') IVCOMP=IVCOMP*3                           05230203
      IF (CATN11(5) .EQ. CATN12(5)) IVCOMP=IVCOMP*5                     05240203
40450 IF (IVCOMP - 30) 20450,10450,20450                                05250203
30450 IVDELE = IVDELE + 1                                               05260203
      WRITE (I02,80000) IVTNUM                                          05270203
      IF (ICZERO) 10450, 0461, 20450                                    05280203
10450 IVPASS = IVPASS + 1                                               05290203
      WRITE (I02,80002) IVTNUM                                          05300203
      GO TO 0461                                                        05310203
20450 IVFAIL = IVFAIL + 1                                               05320203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05330203
 0461 CONTINUE                                                          05340203
C                                                                       05350203
C         TEST 46 THROUGH TEST 49 CONTAIN CHARACTER ARRAY ELEMENTS OF   05360203
C     LENGTH TWO IN CHARACTER ASSIGNMENT STATEMENTS.  THE CHARACTER     05370203
C     RELATIONAL EXPRESSION                                             05380203
C                                                                       05390203
C         CHAR. ARRAY ELEMENT (LEN 2) .EQ. CHAR. CONSTANT (LEN 2)       05400203
C                                                                       05410203
C     IS USED TO VERIFY THE TEST RESULTS.                               05420203
C                                                                       05430203
C         THE TWO ARRAYS USED IN THESE TESTS ARE CATN13(5) AND CATN14(5)05440203
C     THE ARRAYS ARE INITIALIZED TO TWO BLANK CHARACTERS BY THE DO-LOOP 05450203
C                                                                       05460203
      DO 462 I=1,5                                                      05470203
      CATN13(I) = '  '                                                  05480203
      CATN14(I) = '  '                                                  05490203
  462 CONTINUE                                                          05500203
C                                                                       05510203
C     ****  FCVS PROGRAM 203  -  TEST 046  ****                         05520203
C                                                                       05530203
C         CHAR. ARRAY ELEMENT (LEN 2) = CHAR. CONSTANT (LEN 2)          05540203
C                                                                       05550203
      IVTNUM =  46                                                      05560203
      IF (ICZERO) 30460, 0460, 30460                                    05570203
 0460 CONTINUE                                                          05580203
      IVCOMP = 0                                                        05590203
      IVCORR = 1                                                        05600203
      CATN13(1) = 'AB'                                                  05610203
      IF (CATN13(1) .EQ. 'AB') IVCOMP = 1                               05620203
40460 IF (IVCOMP - 1) 20460,10460,20460                                 05630203
30460 IVDELE = IVDELE + 1                                               05640203
      WRITE (I02,80000) IVTNUM                                          05650203
      IF (ICZERO) 10460, 0471, 20460                                    05660203
10460 IVPASS = IVPASS + 1                                               05670203
      WRITE (I02,80002) IVTNUM                                          05680203
      GO TO 0471                                                        05690203
20460 IVFAIL = IVFAIL + 1                                               05700203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05710203
 0471 CONTINUE                                                          05720203
C                                                                       05730203
C     ****  FCVS PROGRAM 203  -  TEST 047  ****                         05740203
C                                                                       05750203
C         CHAR. VARIABLE (LEN 2) = CHAR. CONSTANT (LEN 2)               05760203
C         CHAR. ARRAY ELEMENT (LEN 2) = CHAR. VARIABLE (LEN 2)          05770203
C                                                                       05780203
      IVTNUM =  47                                                      05790203
      IF (ICZERO) 30470, 0470, 30470                                    05800203
 0470 CONTINUE                                                          05810203
      IVCOMP = 0                                                        05820203
      IVCORR = 1                                                        05830203
      CVTN03 = '+-'                                                     05840203
      CATN13(2) = CVTN03                                                05850203
      IF (CATN13(2) .EQ. '+-') IVCOMP=1                                 05860203
40470 IF (IVCOMP - 1) 20470,10470,20470                                 05870203
30470 IVDELE = IVDELE + 1                                               05880203
      WRITE (I02,80000) IVTNUM                                          05890203
      IF (ICZERO) 10470, 0481, 20470                                    05900203
10470 IVPASS = IVPASS + 1                                               05910203
      WRITE (I02,80002) IVTNUM                                          05920203
      GO TO 0481                                                        05930203
20470 IVFAIL = IVFAIL + 1                                               05940203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05950203
 0481 CONTINUE                                                          05960203
C                                                                       05970203
C     ****  FCVS PROGRAM 203  -  TEST 048  ****                         05980203
C                                                                       05990203
C         CHAR. ARRAY ELEMENT (LEN 2) = CHAR. CONSTANT (LEN 2)          06000203
C         CHAR. ARRAY ELEMENT (LEN 2) = CHAR. ARRAY ELEMENT (LEN 2)     06010203
C                                                                       06020203
      IVTNUM =  48                                                      06030203
      IF (ICZERO) 30480, 0480, 30480                                    06040203
 0480 CONTINUE                                                          06050203
      IVCOMP = 0                                                        06060203
      IVCORR = 1                                                        06070203
      CATN13(4) = '24'                                                  06080203
      CATN13(3) = CATN13(4)                                             06090203
      IF (CATN13(3) .EQ. '24') IVCOMP = 1                               06100203
40480 IF (IVCOMP - 1) 20480,10480,20480                                 06110203
30480 IVDELE = IVDELE + 1                                               06120203
      WRITE (I02,80000) IVTNUM                                          06130203
      IF (ICZERO) 10480, 0491, 20480                                    06140203
10480 IVPASS = IVPASS + 1                                               06150203
      WRITE (I02,80002) IVTNUM                                          06160203
      GO TO 0491                                                        06170203
20480 IVFAIL = IVFAIL + 1                                               06180203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06190203
 0491 CONTINUE                                                          06200203
C                                                                       06210203
C     ****  FCVS PROGRAM 203  -  TEST 049  ****                         06220203
C                                                                       06230203
C         CHAR. ARRAY ELEMENT (LEN 2) = CHAR. CONSTANT (LEN 2)          06240203
C         CHAR. VARIABLE (LEN 2) = CHAR. ARRAY ELEMENT (LEN 2)          06250203
C                                                                       06260203
      IVTNUM =  49                                                      06270203
      IF (ICZERO) 30490, 0490, 30490                                    06280203
 0490 CONTINUE                                                          06290203
      IVCOMP = 0                                                        06300203
      IVCORR = 1                                                        06310203
      CATN14(1) = 'AB'                                                  06320203
      CVTN04 = CATN14(1)                                                06330203
      IF (CVTN04 .EQ. 'AB') IVCOMP = 1                                  06340203
40490 IF (IVCOMP - 1) 20490,10490,20490                                 06350203
30490 IVDELE = IVDELE + 1                                               06360203
      WRITE (I02,80000) IVTNUM                                          06370203
      IF (ICZERO) 10490, 0501, 20490                                    06380203
10490 IVPASS = IVPASS + 1                                               06390203
      WRITE (I02,80002) IVTNUM                                          06400203
      GO TO 0501                                                        06410203
20490 IVFAIL = IVFAIL + 1                                               06420203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06430203
 0501 CONTINUE                                                          06440203
C                                                                       06450203
C         TEST 50 THROUGH TEST 52 VERIFY THE RESULTS OF CHARACTER       06460203
C     RELATIONAL EXPRESSIONS USING EACH OF THE SIX RELATIONAL OPERATORS.06470203
C     THE CHARACTER DATA 'ZA' AND 'Z1' ARE COMPARED IN THE EXPRESSION   06480203
C     AND ARE INITIALIZED BY THE CHARACTER ASSIGNMENT STATEMENTS        06490203
C                                                                       06500203
      CATN14(2) = 'ZA'                                                  06510203
      CATN14(3) = 'Z1'                                                  06520203
      CVTN03 = 'ZA'                                                     06530203
      CVTN04 = 'Z1'                                                     06540203
C                                                                       06550203
C     ****  FCVS PROGRAM 203  -  TEST 050  ****                         06560203
C                                                                       06570203
C         RELATIONAL OPERATORS .NE. AND .EQ.                            06580203
C         CHAR. ARRAY ELEMENT (LEN 2) RELOP CHAR. VARIABLE (LEN 2)      06590203
C                                                                       06600203
      IVTNUM =  50                                                      06610203
      IF (ICZERO) 30500, 0500, 30500                                    06620203
 0500 CONTINUE                                                          06630203
      IVCOMP = 1                                                        06640203
      IVCORR = 3                                                        06650203
      IF (CATN14(2) .EQ. 'Z1') IVCOMP=IVCOMP*2                          06660203
      IF ('ZA' .NE. CATN14(3)) IVCOMP=IVCOMP*3                          06670203
40500 IF (IVCOMP - 3) 20500,10500,20500                                 06680203
30500 IVDELE = IVDELE + 1                                               06690203
      WRITE (I02,80000) IVTNUM                                          06700203
      IF (ICZERO) 10500, 0511, 20500                                    06710203
10500 IVPASS = IVPASS + 1                                               06720203
      WRITE (I02,80002) IVTNUM                                          06730203
      GO TO 0511                                                        06740203
20500 IVFAIL = IVFAIL + 1                                               06750203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06760203
 0511 CONTINUE                                                          06770203
C                                                                       06780203
C     ****  FCVS PROGRAM 203  -  TEST 051  ****                         06790203
C                                                                       06800203
C         RELATIONAL OPERATORS .LE. AND .GE.                            06810203
C         CHAR. ARRAY ELEMENT (LEN 2) RELOP CHAR. VARIABLE (LEN 2)      06820203
C                                                                       06830203
      IVTNUM =  51                                                      06840203
      IF (ICZERO) 30510, 0510, 30510                                    06850203
 0510 CONTINUE                                                          06860203
      IVCOMP = 0                                                        06870203
      IVCORR = 1                                                        06880203
      IF (CATN14(2) .LE. CVTN04) IVCOMP=IVCOMP+1                        06890203
      IF (CVTN03 .GE. CATN14(3)) IVCOMP=IVCOMP+1                        06900203
40510 IF (IVCOMP - 1) 20510,10510,20510                                 06910203
30510 IVDELE = IVDELE + 1                                               06920203
      WRITE (I02,80000) IVTNUM                                          06930203
      IF (ICZERO) 10510, 0521, 20510                                    06940203
10510 IVPASS = IVPASS + 1                                               06950203
      WRITE (I02,80002) IVTNUM                                          06960203
      GO TO 0521                                                        06970203
20510 IVFAIL = IVFAIL + 1                                               06980203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06990203
 0521 CONTINUE                                                          07000203
C                                                                       07010203
C     ****  FCVS PROGRAM 203  -  TEST 052  ****                         07020203
C                                                                       07030203
C         RELATIONAL OPERATORS .LT. AND .GT.                            07040203
C         CHAR. ARRAY ELEMENT (LEN 2) RELOP CHAR. ARRAY ELEMENT (LEN 2) 07050203
C                                                                       07060203
      IVTNUM =  52                                                      07070203
      IF (ICZERO) 30520, 0520, 30520                                    07080203
 0520 CONTINUE                                                          07090203
      IVCOMP =0                                                         07100203
      IVCORR =1                                                         07110203
      IF (CATN14(2) .LT. CATN14(3)) IVCOMP=IVCOMP+1                     07120203
      IF (CATN14(2) .GT. CATN14(3)) IVCOMP=IVCOMP+1                     07130203
40520 IF (IVCOMP - 1) 20520,10520,20520                                 07140203
30520 IVDELE = IVDELE + 1                                               07150203
      WRITE (I02,80000) IVTNUM                                          07160203
      IF (ICZERO) 10520, 0531, 20520                                    07170203
10520 IVPASS = IVPASS + 1                                               07180203
      WRITE (I02,80002) IVTNUM                                          07190203
      GO TO 0531                                                        07200203
20520 IVFAIL = IVFAIL + 1                                               07210203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07220203
 0531 CONTINUE                                                          07230203
C                                                                       07240203
C     ****  FCVS PROGRAM 203  -  TEST 053  ****                         07250203
C                                                                       07260203
C         TEST 53 VERIFIES THAT THE LAST ELEMENTS OF THE ARRAYS USED IN 07270203
C     TEST 46 THROUGH TEST 52 WERE NOT AFFECTED BY THE SETTING OF OTHER 07280203
C     CHARACTER ARRAY ELEMENTS.                                         07290203
C                                                                       07300203
      IVTNUM =  53                                                      07310203
      IF (ICZERO) 30530, 0530, 30530                                    07320203
 0530 CONTINUE                                                          07330203
      IVCOMP = 1                                                        07340203
      IVCORR = 30                                                       07350203
      IF (CATN13(5) .EQ. '  ')IVCOMP=IVCOMP*2                           07360203
      IF (CATN14(5) .EQ. '  ') IVCOMP= IVCOMP * 3                       07370203
      IF (CATN14(5) .EQ. CATN13(5)) IVCOMP=IVCOMP*5                     07380203
40530 IF (IVCOMP - 30) 20530,10530,20530                                07390203
30530 IVDELE = IVDELE + 1                                               07400203
      WRITE (I02,80000) IVTNUM                                          07410203
      IF (ICZERO) 10530, 0541, 20530                                    07420203
10530 IVPASS = IVPASS + 1                                               07430203
      WRITE (I02,80002) IVTNUM                                          07440203
      GO TO 0541                                                        07450203
20530 IVFAIL = IVFAIL + 1                                               07460203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07470203
 0541 CONTINUE                                                          07480203
C                                                                       07490203
C         TEST 54 THROUGH TEST 60 VERIFY THAT A CHARACTER PRIMARY CAN   07500203
C     BE ENCLOSED IN PARENTHESES.  THE CHARACTER PRIMARIES FOR THE      07510203
C     SUBSET ARE CHARACTER CONSTANT, CHARACTER VARIABLE, CHARACTER ARRAY07520203
C     ELEMENT, AND CHARACTER EXPRESSION ENCLOSED IN PARENTHESES.  THE   07530203
C     FORM OF A CHARACTER EXPRESSION IS CHARACTER PRIMARY.              07540203
C                                                                       07550203
C                                                                       07560203
C     ****  FCVS PROGRAM 203  -  TEST 054  ****                         07570203
C                                                                       07580203
C         CHARACTER ASSIGNMENT STATEMENT                                07590203
C         CHAR. VARIABLE = (CHARACTER CONSTANT)   LENGTH 1              07600203
C                                                                       07610203
      IVTNUM =  54                                                      07620203
      IF (ICZERO) 30540, 0540, 30540                                    07630203
 0540 CONTINUE                                                          07640203
      CVTN01 = ' '                                                      07650203
      IVCOMP = 0                                                        07660203
      IVCORR = 1                                                        07670203
      CVTN01 = ('N')                                                    07680203
      IF (CVTN01 .EQ. 'N') IVCOMP = 1                                   07690203
40540 IF (IVCOMP - 1) 20540,10540,20540                                 07700203
30540 IVDELE = IVDELE + 1                                               07710203
      WRITE (I02,80000) IVTNUM                                          07720203
      IF (ICZERO) 10540, 0551, 20540                                    07730203
10540 IVPASS = IVPASS + 1                                               07740203
      WRITE (I02,80002) IVTNUM                                          07750203
      GO TO 0551                                                        07760203
20540 IVFAIL = IVFAIL + 1                                               07770203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07780203
 0551 CONTINUE                                                          07790203
C                                                                       07800203
C     ****  FCVS PROGRAM 203  -  TEST 055  ****                         07810203
C                                                                       07820203
C         CHARACTER ASSIGNMENT STATEMENT                                07830203
C         CHAR. VARIABLE = (CHAR. VARIABLE)   LENGTH 2                  07840203
C                                                                       07850203
      IVTNUM =  55                                                      07860203
      IF (ICZERO) 30550, 0550, 30550                                    07870203
 0550 CONTINUE                                                          07880203
      CVTN04 = '  '                                                     07890203
      IVCOMP = 0                                                        07900203
      IVCORR = 1                                                        07910203
      CVTN03 = '/+'                                                     07920203
      CVTN04 = (CVTN03)                                                 07930203
      IF (CVTN04 .EQ. '/+') IVCOMP=1                                    07940203
40550 IF (IVCOMP - 1) 20550,10550,20550                                 07950203
30550 IVDELE = IVDELE + 1                                               07960203
      WRITE (I02,80000) IVTNUM                                          07970203
      IF (ICZERO) 10550, 0561, 20550                                    07980203
10550 IVPASS = IVPASS + 1                                               07990203
      WRITE (I02,80002) IVTNUM                                          08000203
      GO TO 0561                                                        08010203
20550 IVFAIL = IVFAIL + 1                                               08020203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08030203
 0561 CONTINUE                                                          08040203
C                                                                       08050203
C     ****  FCVS PROGRAM 203  -  TEST 056  ****                         08060203
C                                                                       08070203
C         CHARACTER ASSIGNMENT STATEMENT                                08080203
C         CHAR. VARIABLE = (CHAR. ARRAY ELEMENT)   LENGTH 2             08090203
C                                                                       08100203
      IVTNUM =  56                                                      08110203
      IF (ICZERO) 30560, 0560, 30560                                    08120203
 0560 CONTINUE                                                          08130203
      IVCOMP = 0                                                        08140203
      IVCORR = 1                                                        08150203
      CVTN04 = '  '                                                     08160203
      CATN13(1) = 'BC'                                                  08170203
      CVTN04 = (CATN13(1))                                              08180203
      IF (CVTN04 .EQ. 'BC') IVCOMP = 1                                  08190203
40560 IF (IVCOMP - 1) 20560,10560,20560                                 08200203
30560 IVDELE = IVDELE + 1                                               08210203
      WRITE (I02,80000) IVTNUM                                          08220203
      IF (ICZERO) 10560, 0571, 20560                                    08230203
10560 IVPASS = IVPASS + 1                                               08240203
      WRITE (I02,80002) IVTNUM                                          08250203
      GO TO 0571                                                        08260203
20560 IVFAIL = IVFAIL + 1                                               08270203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08280203
 0571 CONTINUE                                                          08290203
C                                                                       08300203
C     ****  FCVS PROGRAM 203  -  TEST 057  ****                         08310203
C                                                                       08320203
C         CHARACTER ASSIGNMENT STATEMENT                                08330203
C         CHAR. VARIABLE = ((CHAR. ARRAY ELEMENT))  LENGTH 2            08340203
C                                                                       08350203
      IVTNUM =  57                                                      08360203
      IF (ICZERO) 30570, 0570, 30570                                    08370203
 0570 CONTINUE                                                          08380203
      IVCOMP = 0                                                        08390203
      IVCORR = 1                                                        08400203
      CVTN04 = '  '                                                     08410203
      CATN13(3) = 'BC'                                                  08420203
      CVTN04 = ((CATN13(3)))                                            08430203
      IF (CVTN04 .EQ. 'BC') IVCOMP=1                                    08440203
40570 IF (IVCOMP - 1) 20570,10570,20570                                 08450203
30570 IVDELE = IVDELE + 1                                               08460203
      WRITE (I02,80000) IVTNUM                                          08470203
      IF (ICZERO) 10570, 0581, 20570                                    08480203
10570 IVPASS = IVPASS + 1                                               08490203
      WRITE (I02,80002) IVTNUM                                          08500203
      GO TO 0581                                                        08510203
20570 IVFAIL = IVFAIL + 1                                               08520203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08530203
 0581 CONTINUE                                                          08540203
C                                                                       08550203
C     ****  FCVS PROGRAM 203  -  TEST 058  ****                         08560203
C                                                                       08570203
C         RELATIONAL EXPRESSION, .NE.                                   08580203
C         (CHAR. CONSTANT) .NE. (CHAR. VARIABLE)   LENGTH 1             08590203
C                                                                       08600203
      IVTNUM =  58                                                      08610203
      IF (ICZERO) 30580, 0580, 30580                                    08620203
 0580 CONTINUE                                                          08630203
      IVCOMP = 0                                                        08640203
      IVCORR = 1                                                        08650203
      CVTN01 = '6'                                                      08660203
      IF (('9') .NE. (CVTN01)) IVCOMP=1                                 08670203
40580 IF (IVCOMP - 1) 20580,10580,20580                                 08680203
30580 IVDELE = IVDELE + 1                                               08690203
      WRITE (I02,80000) IVTNUM                                          08700203
      IF (ICZERO) 10580, 0591, 20580                                    08710203
10580 IVPASS = IVPASS + 1                                               08720203
      WRITE (I02,80002) IVTNUM                                          08730203
      GO TO 0591                                                        08740203
20580 IVFAIL = IVFAIL + 1                                               08750203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08760203
 0591 CONTINUE                                                          08770203
C                                                                       08780203
C     ****  FCVS PROGRAM 203  -  TEST 059  ****                         08790203
C                                                                       08800203
C         RELATIONAL EXPRESSION, .GE.                                   08810203
C         (CHAR. VARIABLE) .GE. (CHAR. ARRAY ELEMENT)  LENGTH 2         08820203
C                                                                       08830203
      IVTNUM =  59                                                      08840203
      IF (ICZERO) 30590, 0590, 30590                                    08850203
 0590 CONTINUE                                                          08860203
      IVCOMP = 0                                                        08870203
      IVCORR = 1                                                        08880203
      CVTN03 = 'DE'                                                     08890203
      CATN13(5) = 'DE'                                                  08900203
      IF ((CVTN03) .GE. (CATN13(5))) IVCOMP=1                           08910203
40590 IF (IVCOMP - 1) 20590,10590,20590                                 08920203
30590 IVDELE = IVDELE + 1                                               08930203
      WRITE (I02,80000) IVTNUM                                          08940203
      IF (ICZERO) 10590, 0601, 20590                                    08950203
10590 IVPASS = IVPASS + 1                                               08960203
      WRITE (I02,80002) IVTNUM                                          08970203
      GO TO 0601                                                        08980203
20590 IVFAIL = IVFAIL + 1                                               08990203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09000203
 0601 CONTINUE                                                          09010203
C                                                                       09020203
C     ****  FCVS PROGRAM 203  -  TEST 060  ****                         09030203
C                                                                       09040203
C         RELATIONAL EXPRESSION, .LE.                                   09050203
C         ((CHAR. ARRAY ELEMENT)) .LE. ((CHAR. ARRAY ELEMENT))  LEN 2   09060203
C                                                                       09070203
      IVTNUM =  60                                                      09080203
      IF (ICZERO) 30600, 0600, 30600                                    09090203
 0600 CONTINUE                                                          09100203
      IVCOMP = 0                                                        09110203
      IVCORR = 1                                                        09120203
      CATN13(4) = 'MC'                                                  09130203
      CATN13(5) = 'MC'                                                  09140203
      IF (((CATN13(4))) .LE. ((CATN13(5)))) IVCOMP = 1                  09150203
40600 IF (IVCOMP - 1) 20600,10600,20600                                 09160203
30600 IVDELE = IVDELE + 1                                               09170203
      WRITE (I02,80000) IVTNUM                                          09180203
      IF (ICZERO) 10600, 0611, 20600                                    09190203
10600 IVPASS = IVPASS + 1                                               09200203
      WRITE (I02,80002) IVTNUM                                          09210203
      GO TO 0611                                                        09220203
20600 IVFAIL = IVFAIL + 1                                               09230203
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09240203
 0611 CONTINUE                                                          09250203
C                                                                       09260203
C                                                                       09270203
C     WRITE OUT TEST SUMMARY                                            09280203
C                                                                       09290203
      WRITE (I02,90004)                                                 09300203
      WRITE (I02,90014)                                                 09310203
      WRITE (I02,90004)                                                 09320203
      WRITE (I02,90000)                                                 09330203
      WRITE (I02,90004)                                                 09340203
      WRITE (I02,90020) IVFAIL                                          09350203
      WRITE (I02,90022) IVPASS                                          09360203
      WRITE (I02,90024) IVDELE                                          09370203
      STOP                                                              09380203
90001 FORMAT (1H ,24X,5HFM203)                                          09390203
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM203)                          09400203
C                                                                       09410203
C     FORMATS FOR TEST DETAIL LINES                                     09420203
C                                                                       09430203
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   09440203
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      09450203
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         09460203
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    09470203
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        09480203
C                                                                       09490203
C     FORMAT STATEMENTS FOR PAGE HEADERS                                09500203
C                                                                       09510203
90002 FORMAT (1H1)                                                      09520203
90004 FORMAT (1H )                                                      09530203
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09540203
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   09550203
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         09560203
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  09570203
90014 FORMAT (1H ,5X,46H----------------------------------------------) 09580203
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             09590203
C                                                                       09600203
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 09610203
C                                                                       09620203
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              09630203
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              09640203
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             09650203
      END                                                               09660203
