      PROGRAM FM308                                                     00010308
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020308
C                                                                       00030308
C          THIS ROUTINE TESTS INTRINSIC FUNCTIONS WHERE THE ACTUAL      00040308
C     ARGUMENTS CONSIST OF INTRINSIC FUNCTION REFERENCES, EXTERNAL      00050308
C     FUNCTION REFERENCES, STATEMENT FUNCTION REFERENCES, AND           00060308
C     EXPRESSIONS INVOLVING OPERATORS.  THE ARGUMENT AND FUNCTION       00070308
C     TYPES OF ALL INTRINSIC FUNCTIONS TESTED ARE EITHER INTEGER OR     00080308
C     REAL.  THE INTRINSIC AND EXTERNAL SPECIFICATION STATEMENTS ARE    00090308
C     SPECIFIED IN ORDER TO ALLOW INTRINSIC AND EXTERNAL FUNCTIONS TO   00100308
C     BE USED AS ACTUAL ARGUMENTS.  THE IMPLICIT STATEMENT AND THE      00110308
C     TYPE-STATEMENT ARE TESTED TO ENSURE THAT THEY DO NOT CHANGE THE   00120308
C     TYPE OF AN INTRINSIC FUNCTION.  THE COMMON STATEMENT IS USED TO   00130308
C     PASS DATA ENTITIES TO AN EXTERNAL FUNCTION.  THE DATA STATEMENT   00140308
C     IS USED TO ENSURE THAT INITIALLY DEFINED ENTITIES CAN BE USED AS  00150308
C     ACTUAL ARGUMENTS.  THE EQUIVALENCE STATEMENT IS USED TO EQUATE A  00160308
C     VARIABLE USED AS AN ACTUAL ARGUMENT.  THE INTRINSIC FUNCTIONS     00170308
C     TESTED IN THIS ROUTINE INCLUDE.                                   00180308
C                                                                       00190308
C                                        SPECIFIC        TYPE OF        00200308
C          INTRINSIC FUNCTION            NAME        ARGUMENT   FUNCTION00210308
C          ------------------            --------    --------   --------00220308
C          TYPE CONVERSION               INT         REAL       INTEGER 00230308
C          TYPE CONVERSION               IFIX        REAL       INTEGER 00240308
C          TYPE CONVERSION               FLOAT       INTEGER    REAL    00250308
C          TYPE CONVERSION               REAL        INTEGER    REAL    00260308
C          TRUNCATION                    AINT        REAL       REAL    00270308
C          NEAREST WHOLE NUMBER          ANINT       REAL       REAL    00280308
C          NEAREST INTEGER               NINT        REAL       INTEGER 00290308
C          ABSOLUTE VALUE                IABS        INTEGER    INTEGER 00300308
C          ABSOLUTE VALUE                ABS         REAL       REAL    00310308
C          REMAINDERING                  MOD         INTEGER    INTEGER 00320308
C          REMAINDERING                  AMOD        REAL       REAL    00330308
C          TRANSFER OF SIGN              ISIGN       INTEGER    INTEGER 00340308
C          TRANSFER OF SIGN              SIGN        REAL       REAL    00350308
C          POSITIVE DIFFERENCE           IDIM        INTEGER    INTEGER 00360308
C          POSITIVE DIFFERENCE           DIM         REAL       REAL    00370308
C          CHOOSING LARGEST VALUE        MAX0        INTEGER    INTEGER 00380308
C          CHOOSING LARGEST VALUE        AMAX0       INTEGER    REAL    00390308
C          CHOOSING LARGEST VALUE        MAX1        REAL       INTEGER 00400308
C          CHOOSING SMALLEST VALUE       AMIN1       REAL       REAL    00410308
C          CHOOSING SMALLEST VALUE       MIN1        REAL       INTEGER 00420308
C          SQUARE ROOT                   SQRT        REAL       REAL    00430308
C          EXPONENTIAL                   EXP         REAL       REAL    00440308
C          NATURAL LOGARITHM             ALOG        REAL       REAL    00450308
C          SINE                          SIN         REAL       REAL    00460308
C          COSINE                        COS         REAL       REAL    00470308
C          TANGENT                       TAN         REAL       REAL    00480308
C          ARCSINE                       ASIN        REAL       REAL    00490308
C          ARCCOSINE                     ACOS        REAL      REAL     00500308
C          ARCTANGENT                    ATAN        REAL      REAL     00510308
C          HYPERBOLIC SINE               SINH        REAL      REAL     00520308
C          HYPERBOLIC COSINE             COSH        REAL      REAL     00530308
C          HYPERBOLIC TANGENT            TANH        REAL      REAL     00540308
C                                                                       00550308
C          SUBSET LEVEL ROUTINES FM097, FM098, FM099 AND FM307 TEST THE 00560308
C     USE OF INTEGER AND REAL INTRINSIC FUNCTIONS USING INTEGER AND REAL00570308
C     CONSTANTS, VARIABLES AND ARRAY ELEMENT ENTITIES AS ACTUAL         00580308
C     ARGUMENTS.                                                        00590308
C                                                                       00600308
C     REFERENCES.                                                       00610308
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00620308
C           X3.9-1978                                                   00630308
C                                                                       00640308
C        SECTION 8.2,     EQUIVALENCE STATEMENT                         00650308
C        SECTION 8.3,     COMMON STATEMENT                              00660308
C        SECTION 8.4,     TYPE-STATEMENTS                               00670308
C        SECTION 8.5,     IMPLICIT STATEMENT                            00680308
C        SECTION 8.7,     EXTERNAL STATEMENT                            00690308
C        SECTION 8.8,     INTRINSIC STATEMENT                           00700308
C        SECTION 9,       DATA STATEMENT                                00710308
C        SECTION 15.3,    INTRINSIC FUNCTION                            00720308
C        SECTION 15.4,    STATEMENT FUNCTION                            00730308
C        SECTION 15.5,    EXTERNAL FUNCTION                             00740308
C        SECTION 15.5.2, .REFERENCING AN EXTERNAL FUNCTION              00750308
C        SECTION 15.9.2,  ACTUAL ARGUMENTS                              00760308
C        SECTION 15.9.3,  ASSOCIATION OF DUMMY AND ACTUAL ARGUMENTS     00770308
C        TABLE 5,         INTRINSIC FUNCTIONS (INCLUDING NOTES)         00780308
C        SECTION 15.10.1, RESTRICTIONS ON RANGE OF ARGUMENTS AND RESULTS00790308
C                                                                       00800308
C                                                                       00810308
C     ******************************************************************00820308
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00830308
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00840308
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00850308
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00860308
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00870308
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00880308
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00890308
C     THE RESULT OF EXECUTING THESE TESTS.                              00900308
C                                                                       00910308
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00920308
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00930308
C                                                                       00940308
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00950308
C                    DEPARTMENT OF THE NAVY                             00960308
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00970308
C                    WASHINGTON, D.C.   20376                           00980308
C                                                                       00990308
C     ******************************************************************01000308
C                                                                       01010308
C                                                                       01020308
      IMPLICIT LOGICAL (L)                                              01030308
      IMPLICIT CHARACTER*14 (C)                                         01040308
C                                                                       01050308
      IMPLICIT INTEGER (E)                                              01060308
      IMPLICIT REAL (N)                                                 01070308
      INTEGER MAX1                                                      01080308
      REAL SINH                                                         01090308
      DIMENSION RADN11(5)                                               01100308
      DIMENSION IADN11(5)                                               01110308
      COMMON RVCN01                                                     01120308
      EQUIVALENCE (IVOE01,IVOE02)                                       01130308
      EXTERNAL FF309,FF310                                              01140308
      INTRINSIC ABS, AINT, IABS, ISIGN, SQRT                            01150308
      DATA RVON04/2.23/                                                 01160308
      RFOS01(RDON01) = RDON01 + 1.0                                     01170308
C                                                                       01180308
C                                                                       01190308
C                                                                       01200308
C     INITIALIZATION SECTION.                                           01210308
C                                                                       01220308
C     INITIALIZE CONSTANTS                                              01230308
C     ********************                                              01240308
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01250308
      I01 = 5                                                           01260308
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01270308
      I02 = 6                                                           01280308
C     SYSTEM ENVIRONMENT SECTION                                        01290308
C                                                                       01300308
      I01 = 5                                                           01310308
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01320308
C     (UNIT NUMBER FOR CARD READER).                                    01330308
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01340308
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01350308
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01360308
C                                                                       01370308
      I02 = 6                                                           01380308
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01390308
C     (UNIT NUMBER FOR PRINTER).                                        01400308
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01410308
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01420308
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01430308
C                                                                       01440308
      IVPASS = 0                                                        01450308
      IVFAIL = 0                                                        01460308
      IVDELE = 0                                                        01470308
      ICZERO = 0                                                        01480308
C                                                                       01490308
C     WRITE OUT PAGE HEADERS                                            01500308
C                                                                       01510308
      WRITE (I02,90002)                                                 01520308
      WRITE (I02,90006)                                                 01530308
      WRITE (I02,90008)                                                 01540308
      WRITE (I02,90004)                                                 01550308
      WRITE (I02,90010)                                                 01560308
      WRITE (I02,90004)                                                 01570308
      WRITE (I02,90016)                                                 01580308
      WRITE (I02,90001)                                                 01590308
      WRITE (I02,90004)                                                 01600308
      WRITE (I02,90012)                                                 01610308
      WRITE (I02,90014)                                                 01620308
      WRITE (I02,90004)                                                 01630308
C                                                                       01640308
C                                                                       01650308
C     TEST 032 THROUGH TEST 040 TEST INTRINSIC FUNCTIONS USING          01660308
C     INTRINSIC FUNCTION REFERENCES AS ACTUAL ARGUMENTS.                01670308
C                                                                       01680308
C                                                                       01690308
C     ****  FCVS PROGRAM 308  -  TEST 032  ****                         01700308
C                                                                       01710308
C                                                                       01720308
      IVTNUM =  32                                                      01730308
      IF (ICZERO) 30320, 0320, 30320                                    01740308
 0320 CONTINUE                                                          01750308
      RVCOMP = 10.0                                                     01760308
      RVCOMP = ANINT (ABS (-2.78) )                                     01770308
      RVCORR = 3.0                                                      01780308
40320 IF (RVCOMP - 2.9995) 20320, 10320, 40321                          01790308
40321 IF (RVCOMP - 3.0005) 10320, 10320, 20320                          01800308
30320 IVDELE = IVDELE + 1                                               01810308
      WRITE (I02,80000) IVTNUM                                          01820308
      IF (ICZERO) 10320, 0331, 20320                                    01830308
10320 IVPASS = IVPASS + 1                                               01840308
      WRITE (I02,80002) IVTNUM                                          01850308
      GO TO 0331                                                        01860308
20320 IVFAIL = IVFAIL + 1                                               01870308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01880308
 0331 CONTINUE                                                          01890308
C                                                                       01900308
C     ****  FCVS PROGRAM 308  -  TEST 033  ****                         01910308
C                                                                       01920308
C                                                                       01930308
      IVTNUM =  33                                                      01940308
      IF (ICZERO) 30330, 0330, 30330                                    01950308
 0330 CONTINUE                                                          01960308
      RVCOMP = 10.0                                                     01970308
      RVCOMP = ATAN (AINT (1.2) )                                       01980308
      RVCORR = .78540                                                   01990308
40330 IF (RVCOMP - .78535) 20330, 10330, 40331                          02000308
40331 IF (RVCOMP - .78545) 10330, 10330, 20330                          02010308
30330 IVDELE = IVDELE + 1                                               02020308
      WRITE (I02,80000) IVTNUM                                          02030308
      IF (ICZERO) 10330, 0341, 20330                                    02040308
10330 IVPASS = IVPASS + 1                                               02050308
      WRITE (I02,80002) IVTNUM                                          02060308
      GO TO 0341                                                        02070308
20330 IVFAIL = IVFAIL + 1                                               02080308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02090308
 0341 CONTINUE                                                          02100308
C                                                                       02110308
C     ****  FCVS PROGRAM 308  -  TEST 034  ****                         02120308
C                                                                       02130308
C                                                                       02140308
      IVTNUM =  34                                                      02150308
      IF (ICZERO) 30340, 0340, 30340                                    02160308
 0340 CONTINUE                                                          02170308
      RVCOMP = 10.0                                                     02180308
      RVCOMP = COS (ABS (-.78540) )                                     02190308
      RVCORR = .70711                                                   02200308
40340 IF (RVCOMP - .70706) 20340, 10340, 40341                          02210308
40341 IF (RVCOMP - .70716) 10340, 10340, 20340                          02220308
30340 IVDELE = IVDELE + 1                                               02230308
      WRITE (I02,80000) IVTNUM                                          02240308
      IF (ICZERO) 10340, 0351, 20340                                    02250308
10340 IVPASS = IVPASS + 1                                               02260308
      WRITE (I02,80002) IVTNUM                                          02270308
      GO TO 0351                                                        02280308
20340 IVFAIL = IVFAIL + 1                                               02290308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02300308
 0351 CONTINUE                                                          02310308
C                                                                       02320308
C     ****  FCVS PROGRAM 308  -  TEST 035  ****                         02330308
C                                                                       02340308
C                                                                       02350308
      IVTNUM =  35                                                      02360308
      IF (ICZERO) 30350, 0350, 30350                                    02370308
 0350 CONTINUE                                                          02380308
      RVCOMP = 10.0                                                     02390308
      IVON01 = 6                                                        02400308
      RVCOMP = AMAX0 (1, IVON01, IABS(-7) )                             02410308
      RVCORR = 7.0                                                      02420308
40350 IF (RVCOMP - 6.9995) 20350, 10350, 40351                          02430308
40351 IF (RVCOMP - 7.0005) 10350, 10350, 20350                          02440308
30350 IVDELE = IVDELE + 1                                               02450308
      WRITE (I02,80000) IVTNUM                                          02460308
      IF (ICZERO) 10350, 0361, 20350                                    02470308
10350 IVPASS = IVPASS + 1                                               02480308
      WRITE (I02,80002) IVTNUM                                          02490308
      GO TO 0361                                                        02500308
20350 IVFAIL = IVFAIL + 1                                               02510308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02520308
 0361 CONTINUE                                                          02530308
C                                                                       02540308
C     ****  FCVS PROGRAM 308  -  TEST 036  ****                         02550308
C                                                                       02560308
C                                                                       02570308
      IVTNUM =  36                                                      02580308
      IF (ICZERO) 30360, 0360, 30360                                    02590308
 0360 CONTINUE                                                          02600308
      IVCOMP = 10                                                       02610308
      IVCOMP = IABS (ISIGN (7, -2))                                     02620308
      IVCORR = 7                                                        02630308
40360 IF (IVCOMP - 7) 20360, 10360, 20360                               02640308
30360 IVDELE = IVDELE + 1                                               02650308
      WRITE (I02,80000) IVTNUM                                          02660308
      IF (ICZERO) 10360, 0371, 20360                                    02670308
10360 IVPASS = IVPASS + 1                                               02680308
      WRITE (I02,80002) IVTNUM                                          02690308
      GO TO 0371                                                        02700308
20360 IVFAIL = IVFAIL + 1                                               02710308
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02720308
 0371 CONTINUE                                                          02730308
C                                                                       02740308
C     ****  FCVS PROGRAM 308  -  TEST 037  ****                         02750308
C                                                                       02760308
C                                                                       02770308
      IVTNUM =  37                                                      02780308
      IF (ICZERO) 30370, 0370, 30370                                    02790308
 0370 CONTINUE                                                          02800308
      IVCOMP = 10                                                       02810308
      IVCOMP = MOD (5, IABS (-3) )                                      02820308
      IVCORR = 2                                                        02830308
40370 IF (IVCOMP - 2) 20370, 10370, 20370                               02840308
30370 IVDELE = IVDELE + 1                                               02850308
      WRITE (I02,80000) IVTNUM                                          02860308
      IF (ICZERO) 10370, 0381, 20370                                    02870308
10370 IVPASS = IVPASS + 1                                               02880308
      WRITE (I02,80002) IVTNUM                                          02890308
      GO TO 0381                                                        02900308
20370 IVFAIL = IVFAIL + 1                                               02910308
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02920308
 0381 CONTINUE                                                          02930308
C                                                                       02940308
C     ****  FCVS PROGRAM 308  -  TEST 038  ****                         02950308
C                                                                       02960308
C                                                                       02970308
      IVTNUM =  38                                                      02980308
      IF (ICZERO) 30380, 0380, 30380                                    02990308
 0380 CONTINUE                                                          03000308
      IVCOMP = 10                                                       03010308
      IVCOMP = ISIGN (-3, IABS (-5) )                                   03020308
      IVCORR = 3                                                        03030308
40380 IF (IVCOMP - 3) 20380, 10380, 20380                               03040308
30380 IVDELE = IVDELE + 1                                               03050308
      WRITE (I02,80000) IVTNUM                                          03060308
      IF (ICZERO) 10380, 0391, 20380                                    03070308
10380 IVPASS = IVPASS + 1                                               03080308
      WRITE (I02,80002) IVTNUM                                          03090308
      GO TO 0391                                                        03100308
20380 IVFAIL = IVFAIL + 1                                               03110308
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03120308
 0391 CONTINUE                                                          03130308
C                                                                       03140308
C     ****  FCVS PROGRAM 308  -  TEST 039  ****                         03150308
C                                                                       03160308
C     REPEAT FUNCTION REFERENCE TWICE IN ONE INTRINSIC FUNCTION         03170308
C     REFERENCE.                                                        03180308
C                                                                       03190308
      IVTNUM =  39                                                      03200308
      IF (ICZERO) 30390, 0390, 30390                                    03210308
 0390 CONTINUE                                                          03220308
      IVCOMP = 10                                                       03230308
      IVCOMP = MAX0 (IABS (-5), IABS (-6) )                             03240308
      IVCORR = 6                                                        03250308
40390 IF (IVCOMP -6) 20390, 10390, 20390                                03260308
30390 IVDELE = IVDELE + 1                                               03270308
      WRITE (I02,80000) IVTNUM                                          03280308
      IF (ICZERO) 10390, 0401, 20390                                    03290308
10390 IVPASS = IVPASS + 1                                               03300308
      WRITE (I02,80002) IVTNUM                                          03310308
      GO TO 0401                                                        03320308
20390 IVFAIL = IVFAIL + 1                                               03330308
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03340308
 0401 CONTINUE                                                          03350308
C                                                                       03360308
C     ****  FCVS PROGRAM 308  -  TEST 040  ****                         03370308
C                                                                       03380308
C     USE INTRINSIC FUNCTION REFERENCE AS ACTUAL ARGUMENT TO ITSELF.    03390308
C                                                                       03400308
      IVTNUM =  40                                                      03410308
      IF (ICZERO) 30400, 0400, 30400                                    03420308
 0400 CONTINUE                                                          03430308
      RVCOMP = 10.0                                                     03440308
      RVCOMP = SQRT (SQRT (25.) )                                       03450308
      RVCORR = 2.2361                                                   03460308
40400 IF (RVCOMP - 2.2356) 20400, 10400, 40401                          03470308
40401 IF (RVCOMP - 2.2366) 10400, 10400, 20400                          03480308
30400 IVDELE = IVDELE + 1                                               03490308
      WRITE (I02,80000) IVTNUM                                          03500308
      IF (ICZERO) 10400, 0411, 20400                                    03510308
10400 IVPASS = IVPASS + 1                                               03520308
      WRITE (I02,80002) IVTNUM                                          03530308
      GO TO 0411                                                        03540308
20400 IVFAIL = IVFAIL + 1                                               03550308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03560308
 0411 CONTINUE                                                          03570308
C                                                                       03580308
C     TEST 041 THROUGH TEST 045 TEST INTRINSIC FUNCTIONS USING EXTERNAL 03590308
C     FUNCTION REFERENCES AS ACTUAL ARGUMENTS.                          03600308
C                                                                       03610308
C                                                                       03620308
C     ****  FCVS PROGRAM 308  -  TEST 041  ****                         03630308
C                                                                       03640308
C                                                                       03650308
      IVTNUM =  41                                                      03660308
      IF (ICZERO) 30410, 0410, 30410                                    03670308
 0410 CONTINUE                                                          03680308
      RVCOMP = 10.0                                                     03690308
      RVCOMP = ALOG (FF309 (29.0) )                                     03700308
      RVCORR = 3.4012                                                   03710308
40410 IF (RVCOMP - 3.4007) 20410, 10410, 40411                          03720308
40411 IF (RVCOMP - 3.4017) 10410, 10410, 20410                          03730308
30410 IVDELE = IVDELE + 1                                               03740308
      WRITE (I02,80000) IVTNUM                                          03750308
      IF (ICZERO) 10410, 0421, 20410                                    03760308
10410 IVPASS = IVPASS + 1                                               03770308
      WRITE (I02,80002) IVTNUM                                          03780308
      GO TO 0421                                                        03790308
20410 IVFAIL = IVFAIL + 1                                               03800308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03810308
 0421 CONTINUE                                                          03820308
C                                                                       03830308
C     ****  FCVS PROGRAM 308  -  TEST 042  ****                         03840308
C                                                                       03850308
C                                                                       03860308
      IVTNUM =  42                                                      03870308
      IF (ICZERO) 30420, 0420, 30420                                    03880308
 0420 CONTINUE                                                          03890308
      RVCOMP = 10.0                                                     03900308
      RVCOMP = ASIN (FF309 (0.) )                                       03910308
      RVCORR = 1.5708                                                   03920308
40420 IF (RVCOMP - 1.5703) 20420, 10420, 40421                          03930308
40421 IF (RVCOMP - 1.5713) 10420, 10420, 20420                          03940308
30420 IVDELE = IVDELE + 1                                               03950308
      WRITE (I02,80000) IVTNUM                                          03960308
      IF (ICZERO) 10420, 0431, 20420                                    03970308
10420 IVPASS = IVPASS + 1                                               03980308
      WRITE (I02,80002) IVTNUM                                          03990308
      GO TO 0431                                                        04000308
20420 IVFAIL = IVFAIL + 1                                               04010308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04020308
 0431 CONTINUE                                                          04030308
C                                                                       04040308
C     ****  FCVS PROGRAM 308  -  TEST 043  ****                         04050308
C                                                                       04060308
C                                                                       04070308
      IVTNUM =  43                                                      04080308
      IF (ICZERO) 30430, 0430, 30430                                    04090308
 0430 CONTINUE                                                          04100308
      RVCOMP = 10.0                                                     04110308
      RVON01 = 1.5                                                      04120308
      RVCOMP = COSH (FF309 (RVON01) )                                   04130308
      RVCORR = 6.1323                                                   04140308
40430 IF (RVCOMP - 6.1318) 20430, 10430, 40431                          04150308
40431 IF (RVCOMP - 6.1328) 10430, 10430, 20430                          04160308
30430 IVDELE = IVDELE + 1                                               04170308
      WRITE (I02,80000) IVTNUM                                          04180308
      IF (ICZERO) 10430, 0441, 20430                                    04190308
10430 IVPASS = IVPASS + 1                                               04200308
      WRITE (I02,80002) IVTNUM                                          04210308
      GO TO 0441                                                        04220308
20430 IVFAIL = IVFAIL + 1                                               04230308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04240308
 0441 CONTINUE                                                          04250308
C                                                                       04260308
C     ****  FCVS PROGRAM 308  -  TEST 044  ****                         04270308
C                                                                       04280308
C                                                                       04290308
      IVTNUM =  44                                                      04300308
      IF (ICZERO) 30440, 0440, 30440                                    04310308
 0440 CONTINUE                                                          04320308
      IVCOMP = 10                                                       04330308
      IVCOMP = IFIX (FF309 (33.3) )                                     04340308
      IVCORR = 34                                                       04350308
40440 IF (IVCOMP - 34) 20440, 10440, 20440                              04360308
30440 IVDELE = IVDELE + 1                                               04370308
      WRITE (I02,80000) IVTNUM                                          04380308
      IF (ICZERO) 10440, 0451, 20440                                    04390308
10440 IVPASS = IVPASS + 1                                               04400308
      WRITE (I02,80002) IVTNUM                                          04410308
      GO TO 0451                                                        04420308
20440 IVFAIL = IVFAIL + 1                                               04430308
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04440308
 0451 CONTINUE                                                          04450308
C                                                                       04460308
C     ****  FCVS PROGRAM 308  -  TEST 045  ****                         04470308
C                                                                       04480308
C                                                                       04490308
      IVTNUM =  45                                                      04500308
      IF (ICZERO) 30450, 0450, 30450                                    04510308
 0450 CONTINUE                                                          04520308
      RVCOMP = 10.0                                                     04530308
      RADN11(2) = 2.1416                                                04540308
      RVCOMP = TAN (FF309 (RADN11(2)))                                  04550308
      RVCORR = 0.0                                                      04560308
40450 IF (RVCOMP + .00005) 20450, 10450, 40451                          04570308
40451 IF (RVCOMP - .00005) 10450, 10450, 20450                          04580308
30450 IVDELE = IVDELE + 1                                               04590308
      WRITE (I02,80000) IVTNUM                                          04600308
      IF (ICZERO) 10450, 0461, 20450                                    04610308
10450 IVPASS = IVPASS + 1                                               04620308
      WRITE (I02,80002) IVTNUM                                          04630308
      GO TO 0461                                                        04640308
20450 IVFAIL = IVFAIL + 1                                               04650308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04660308
 0461 CONTINUE                                                          04670308
C                                                                       04680308
C     TEST 046 THROUGH TEST 052 TEST INTRINSIC FUNCTIONS USING          04690308
C     EXPRESSIONS INVOLVING OPERATORS AS ACTUAL ARGUMENTS.              04700308
C                                                                       04710308
C                                                                       04720308
C     ****  FCVS PROGRAM 308  -  TEST 046  ****                         04730308
C                                                                       04740308
C                                                                       04750308
      IVTNUM =  46                                                      04760308
      IF (ICZERO) 30460, 0460, 30460                                    04770308
 0460 CONTINUE                                                          04780308
      RVCOMP = 10.0                                                     04790308
      RVCOMP = ABS (3.4 - 8.2)                                          04800308
      RVCORR = 4.8                                                      04810308
40460 IF (RVCOMP - 4.7995) 20460, 10460, 40461                          04820308
40461 IF (RVCOMP - 4.8005) 10460, 10460, 20460                          04830308
30460 IVDELE = IVDELE + 1                                               04840308
      WRITE (I02,80000) IVTNUM                                          04850308
      IF (ICZERO) 10460, 0471, 20460                                    04860308
10460 IVPASS = IVPASS + 1                                               04870308
      WRITE (I02,80002) IVTNUM                                          04880308
      GO TO 0471                                                        04890308
20460 IVFAIL = IVFAIL + 1                                               04900308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04910308
 0471 CONTINUE                                                          04920308
C                                                                       04930308
C     ****  FCVS PROGRAM 308  -  TEST 047  ****                         04940308
C                                                                       04950308
C                                                                       04960308
      IVTNUM =  47                                                      04970308
      IF (ICZERO) 30470, 0470, 30470                                    04980308
 0470 CONTINUE                                                          04990308
      RVCOMP = 10.0                                                     05000308
      IVON01 = 2                                                        05010308
      RVON01 = 3.0                                                      05020308
      RVCOMP = ACOS (IVON01 - RVON01 * .5)                              05030308
      RVCORR = 1.0472                                                   05040308
40470 IF (RVCOMP - 1.0467) 20470, 10470, 40471                          05050308
40471 IF (RVCOMP - 1.0477) 10470, 10470, 20470                          05060308
30470 IVDELE = IVDELE + 1                                               05070308
      WRITE (I02,80000) IVTNUM                                          05080308
      IF (ICZERO) 10470, 0481, 20470                                    05090308
10470 IVPASS = IVPASS + 1                                               05100308
      WRITE (I02,80002) IVTNUM                                          05110308
      GO TO 0481                                                        05120308
20470 IVFAIL = IVFAIL + 1                                               05130308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05140308
 0481 CONTINUE                                                          05150308
C                                                                       05160308
C     ****  FCVS PROGRAM 308  -  TEST 048  ****                         05170308
C                                                                       05180308
C                                                                       05190308
      IVTNUM =  48                                                      05200308
      IF (ICZERO) 30480, 0480, 30480                                    05210308
 0480 CONTINUE                                                          05220308
      RVCOMP = 10.0                                                     05230308
      IVON01 = 2                                                        05240308
      RVON01 = -4.8                                                     05250308
      RVON02 = 4.5                                                      05260308
      RVCOMP = AMIN1 (RVON01, (IVON01 - 3.2) * RVON02)                  05270308
      RVCORR = -5.4                                                     05280308
40480 IF (RVCOMP + 5.4005 ) 20480, 10480, 40481                         05290308
40481 IF (RVCOMP + 5.3995 ) 10480, 10480, 20480                         05300308
30480 IVDELE = IVDELE + 1                                               05310308
      WRITE (I02,80000) IVTNUM                                          05320308
      IF (ICZERO) 10480, 0491, 20480                                    05330308
10480 IVPASS = IVPASS + 1                                               05340308
      WRITE (I02,80002) IVTNUM                                          05350308
      GO TO 0491                                                        05360308
20480 IVFAIL = IVFAIL + 1                                               05370308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05380308
 0491 CONTINUE                                                          05390308
C                                                                       05400308
C     ****  FCVS PROGRAM 308  -  TEST 049  ****                         05410308
C                                                                       05420308
C                                                                       05430308
      IVTNUM =  49                                                      05440308
      IF (ICZERO) 30490, 0490, 30490                                    05450308
 0490 CONTINUE                                                          05460308
      RVCOMP = 10.0                                                     05470308
      RVON01 = 12.0                                                     05480308
      IADN11(1) = 3                                                     05490308
      RADN11(2) = 2.5                                                   05500308
      RVCOMP = AMOD (RVON01 / IADN11(1), 12 / RADN11(2))                05510308
      RVCORR = 4.0                                                      05520308
40490 IF (RVCOMP - 3.9995) 20490, 10490, 40491                          05530308
40491 IF (RVCOMP - 4.0005) 10490, 10490, 20490                          05540308
30490 IVDELE = IVDELE + 1                                               05550308
      WRITE (I02,80000) IVTNUM                                          05560308
      IF (ICZERO) 10490, 0501, 20490                                    05570308
10490 IVPASS = IVPASS + 1                                               05580308
      WRITE (I02,80002) IVTNUM                                          05590308
      GO TO 0501                                                        05600308
20490 IVFAIL = IVFAIL + 1                                               05610308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05620308
 0501 CONTINUE                                                          05630308
C                                                                       05640308
C     ****  FCVS PROGRAM 308  -  TEST 050  ****                         05650308
C                                                                       05660308
C                                                                       05670308
      IVTNUM =  50                                                      05680308
      IF (ICZERO) 30500, 0500, 30500                                    05690308
 0500 CONTINUE                                                          05700308
      IVCOMP = 10                                                       05710308
      IVON01 = 2                                                        05720308
      IVON02 = 9                                                        05730308
      IVCOMP = IDIM (IVON01 ** 3, IVON02)                               05740308
      IVCORR = 0                                                        05750308
40500 IF (IVCOMP) 20500, 10500, 20500                                   05760308
30500 IVDELE = IVDELE + 1                                               05770308
      WRITE (I02,80000) IVTNUM                                          05780308
      IF (ICZERO) 10500, 0511, 20500                                    05790308
10500 IVPASS = IVPASS + 1                                               05800308
      WRITE (I02,80002) IVTNUM                                          05810308
      GO TO 0511                                                        05820308
20500 IVFAIL = IVFAIL + 1                                               05830308
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05840308
 0511 CONTINUE                                                          05850308
C                                                                       05860308
C     ****  FCVS PROGRAM 308  -  TEST 051  ****                         05870308
C                                                                       05880308
C                                                                       05890308
      IVTNUM =  51                                                      05900308
      IF (ICZERO) 30510, 0510, 30510                                    05910308
 0510 CONTINUE                                                          05920308
      RVCOMP = 10.0                                                     05930308
      IVON01 = 6                                                        05940308
      RVCOMP = REAL (IABS (-3) + IVON01)                                05950308
      RVCORR = 9.0                                                      05960308
40510 IF (RVCOMP - 8.9995) 20510, 10510, 40511                          05970308
40511 IF (RVCOMP - 9.0005) 10510, 10510, 20510                          05980308
30510 IVDELE = IVDELE + 1                                               05990308
      WRITE (I02,80000) IVTNUM                                          06000308
      IF (ICZERO) 10510, 0521, 20510                                    06010308
10510 IVPASS = IVPASS + 1                                               06020308
      WRITE (I02,80002) IVTNUM                                          06030308
      GO TO 0521                                                        06040308
20510 IVFAIL = IVFAIL + 1                                               06050308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06060308
 0521 CONTINUE                                                          06070308
C                                                                       06080308
C     ****  FCVS PROGRAM 308  -  TEST 052  ****                         06090308
C                                                                       06100308
C                                                                       06110308
      IVTNUM =  52                                                      06120308
      IF (ICZERO) 30520, 0520, 30520                                    06130308
 0520 CONTINUE                                                          06140308
      RVCOMP = 10.0                                                     06150308
      RVON01 = 2.3                                                      06160308
      IVON01 = 150                                                      06170308
      IADN11(1) = 3                                                     06180308
      RVCOMP = SIGN(13+RVON01*IABS(-4)-IVON01/FF309(1.)**IADN11(1),-1.) 06190308
      RVCORR = -3.45                                                    06200308
40520 IF (RVCOMP + 3.4505) 20520, 10520, 40521                          06210308
40521 IF (RVCOMP + 3.4495) 10520, 10520, 20520                          06220308
30520 IVDELE = IVDELE + 1                                               06230308
      WRITE (I02,80000) IVTNUM                                          06240308
      IF (ICZERO) 10520, 0531, 20520                                    06250308
10520 IVPASS = IVPASS + 1                                               06260308
      WRITE (I02,80002) IVTNUM                                          06270308
      GO TO 0531                                                        06280308
20520 IVFAIL = IVFAIL + 1                                               06290308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06300308
 0531 CONTINUE                                                          06310308
C                                                                       06320308
C     TEST 053 THROUGH TEST 056 TEST INTRINSIC FUNCTIONS USING          06330308
C     STATEMENT FUNCTION REFERENCES AS ACTUAL ARGUMENTS.                06340308
C                                                                       06350308
C                                                                       06360308
C     ****  FCVS PROGRAM 308  -  TEST 053  ****                         06370308
C                                                                       06380308
C                                                                       06390308
      IVTNUM =  53                                                      06400308
      IF (ICZERO) 30530, 0530, 30530                                    06410308
 0530 CONTINUE                                                          06420308
      RVCOMP = 10.0                                                     06430308
      RVCOMP = DIM (RFOS01(5.4), 6.0)                                   06440308
      RVCORR = .4                                                       06450308
40530 IF (RVCOMP - .39995) 20530, 10530, 40531                          06460308
40531 IF (RVCOMP - .40005) 10530, 10530, 20530                          06470308
30530 IVDELE = IVDELE + 1                                               06480308
      WRITE (I02,80000) IVTNUM                                          06490308
      IF (ICZERO) 10530, 0541, 20530                                    06500308
10530 IVPASS = IVPASS + 1                                               06510308
      WRITE (I02,80002) IVTNUM                                          06520308
      GO TO 0541                                                        06530308
20530 IVFAIL = IVFAIL + 1                                               06540308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06550308
 0541 CONTINUE                                                          06560308
C                                                                       06570308
C     ****  FCVS PROGRAM 308  -  TEST 054  ****                         06580308
C                                                                       06590308
C                                                                       06600308
      IVTNUM =  54                                                      06610308
      IF (ICZERO) 30540, 0540, 30540                                    06620308
 0540 CONTINUE                                                          06630308
      IVCOMP = 10                                                       06640308
      IVCOMP = INT(RFOS01(2.01))                                        06650308
      IVCORR = 3                                                        06660308
40540 IF (IVCOMP - 3) 20540, 10540, 20540                               06670308
30540 IVDELE = IVDELE + 1                                               06680308
      WRITE (I02,80000) IVTNUM                                          06690308
      IF (ICZERO) 10540, 0551, 20540                                    06700308
10540 IVPASS = IVPASS + 1                                               06710308
      WRITE (I02,80002) IVTNUM                                          06720308
      GO TO 0551                                                        06730308
20540 IVFAIL = IVFAIL + 1                                               06740308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06750308
 0551 CONTINUE                                                          06760308
C                                                                       06770308
C     ****  FCVS PROGRAM 308  -  TEST 055  ****                         06780308
C                                                                       06790308
C                                                                       06800308
      IVTNUM =  55                                                      06810308
      IF (ICZERO) 30550, 0550, 30550                                    06820308
 0550 CONTINUE                                                          06830308
      RVCOMP = 10.0                                                     06840308
      RVON01 = 0.5708                                                   06850308
      RVCOMP = SIN (RFOS01 (RVON01) / 2)                                06860308
      RVCORR = .70711                                                   06870308
40550 IF (RVCOMP - .70706) 20550, 10550, 40551                          06880308
40551 IF (RVCOMP - .70716) 10550, 10550, 20550                          06890308
30550 IVDELE = IVDELE + 1                                               06900308
      WRITE (I02,80000) IVTNUM                                          06910308
      IF (ICZERO) 10550, 0561, 20550                                    06920308
10550 IVPASS = IVPASS + 1                                               06930308
      WRITE (I02,80002) IVTNUM                                          06940308
      GO TO 0561                                                        06950308
20550 IVFAIL = IVFAIL + 1                                               06960308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06970308
 0561 CONTINUE                                                          06980308
C                                                                       06990308
C     ****  FCVS PROGRAM 308  -  TEST 056  ****                         07000308
C                                                                       07010308
C                                                                       07020308
      IVTNUM =  56                                                      07030308
      IF (ICZERO) 30560, 0560, 30560                                    07040308
 0560 CONTINUE                                                          07050308
      RVCOMP = 10.0                                                     07060308
      RADN11(2) = 1.5                                                   07070308
      RVCOMP = TANH(RFOS01(RADN11(2)))                                  07080308
      RVCORR = .98661                                                   07090308
40560 IF (RVCOMP - .98656) 20560, 10560, 40561                          07100308
40561 IF (RVCOMP - .98666) 10560, 10560, 20560                          07110308
30560 IVDELE = IVDELE + 1                                               07120308
      WRITE (I02,80000) IVTNUM                                          07130308
      IF (ICZERO) 10560, 0571, 20560                                    07140308
10560 IVPASS = IVPASS + 1                                               07150308
      WRITE (I02,80002) IVTNUM                                          07160308
      GO TO 0571                                                        07170308
20560 IVFAIL = IVFAIL + 1                                               07180308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07190308
 0571 CONTINUE                                                          07200308
C                                                                       07210308
C     ****  FCVS PROGRAM 308  -  TEST 057  ****                         07220308
C                                                                       07230308
C     TEST 057 TESTS THE INTRINSIC FUNCTION AINT USING AN EXTERNAL      07240308
C     FUNCTION REFERENCE AS AN ACTUAL ARGUMENT AND THE COMMON           07250308
C     STATEMENT AS A MEANS OF PASSING DATA TO THE EXTERNAL FUNCTION.    07260308
C                                                                       07270308
      IVTNUM =  57                                                      07280308
      IF (ICZERO) 30570, 0570, 30570                                    07290308
 0570 CONTINUE                                                          07300308
      RVCOMP = 10.0                                                     07310308
      RVCN01 = 25.3                                                     07320308
      RVCOMP = AINT(FF310( ))                                           07330308
      RVCORR = 26.0                                                     07340308
40570 IF (RVCOMP - 25.995) 20570, 10570, 40571                          07350308
40571 IF (RVCOMP - 26.005) 10570, 10570, 20570                          07360308
30570 IVDELE = IVDELE + 1                                               07370308
      WRITE (I02,80000) IVTNUM                                          07380308
      IF (ICZERO) 10570, 0581, 20570                                    07390308
10570 IVPASS = IVPASS + 1                                               07400308
      WRITE (I02,80002) IVTNUM                                          07410308
      GO TO 0581                                                        07420308
20570 IVFAIL = IVFAIL + 1                                               07430308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07440308
 0581 CONTINUE                                                          07450308
C                                                                       07460308
C     ****  FCVS PROGRAM 308  -  TEST 058  ****                         07470308
C                                                                       07480308
C     TEST 058 TESTS THE INTRINSIC FUNCTION FLOAT BY USING A VARIABLE   07490308
C     EQUATED BY EQUIVALENCE ASSOCIATION AS AN ACTUAL ARGUMENT.         07500308
C                                                                       07510308
      IVTNUM =  58                                                      07520308
      IF (ICZERO) 30580, 0580, 30580                                    07530308
 0580 CONTINUE                                                          07540308
      RVCOMP = 10.0                                                     07550308
      IVOE01 = 5                                                        07560308
      RVCOMP = FLOAT(IVOE01)                                            07570308
      RVCORR = 5.0                                                      07580308
40580 IF (RVCOMP - 4.9995) 20580, 10580, 40581                          07590308
40581 IF (RVCOMP - 5.0005) 10580, 10580, 20580                          07600308
30580 IVDELE = IVDELE + 1                                               07610308
      WRITE (I02,80000) IVTNUM                                          07620308
      IF (ICZERO) 10580, 0591, 20580                                    07630308
10580 IVPASS = IVPASS + 1                                               07640308
      WRITE (I02,80002) IVTNUM                                          07650308
      GO TO 0591                                                        07660308
20580 IVFAIL = IVFAIL + 1                                               07670308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07680308
 0591 CONTINUE                                                          07690308
C                                                                       07700308
C     ****  FCVS PROGRAM 308  -  TEST 059  ****                         07710308
C                                                                       07720308
C     TEST 059 TESTS THE INTRINSIC FUNCTION MIN1 BY USING A VARIABLE    07730308
C     INITIALIZED BY THE DATA STATEMENT AS AN ACTUAL ARGUMENT.          07740308
C                                                                       07750308
      IVTNUM =  59                                                      07760308
      IF (ICZERO) 30590, 0590, 30590                                    07770308
 0590 CONTINUE                                                          07780308
      IVCOMP = 10                                                       07790308
      IVCOMP = MIN1(6., RVON04, 7.3)                                    07800308
      IVCORR = 2                                                        07810308
40590 IF (IVCOMP - 2) 20590, 10590, 20590                               07820308
30590 IVDELE = IVDELE + 1                                               07830308
      WRITE (I02,80000) IVTNUM                                          07840308
      IF (ICZERO) 10590, 0601, 20590                                    07850308
10590 IVPASS = IVPASS + 1                                               07860308
      WRITE (I02,80002) IVTNUM                                          07870308
      GO TO 0601                                                        07880308
20590 IVFAIL = IVFAIL + 1                                               07890308
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07900308
 0601 CONTINUE                                                          07910308
C                                                                       07920308
C     ****  FCVS PROGRAM 308  -  TEST 060  ****                         07930308
C                                                                       07940308
C     TEST 060 ATTEMPTS TO OVERRIDE THE TYPING OF REAL FOR THE          07950308
C     INTRINSIC FUNCTION EXP WITH IMPLICIT INTEGER TYPING.              07960308
C                                                                       07970308
      IVTNUM =  60                                                      07980308
      IF (ICZERO) 30600, 0600, 30600                                    07990308
 0600 CONTINUE                                                          08000308
      RVCOMP = 10.0                                                     08010308
      RVON01 = 2.05                                                     08020308
      RVCOMP = EXP(RVON01)                                              08030308
      RVCORR = 7.7679                                                   08040308
40600 IF (RVCOMP - 7.7674) 20600, 10600, 40601                          08050308
40601 IF (RVCOMP - 7.7684) 10600, 10600, 20600                          08060308
30600 IVDELE = IVDELE + 1                                               08070308
      WRITE (I02,80000) IVTNUM                                          08080308
      IF (ICZERO) 10600, 0611, 20600                                    08090308
10600 IVPASS = IVPASS + 1                                               08100308
      WRITE (I02,80002) IVTNUM                                          08110308
      GO TO 0611                                                        08120308
20600 IVFAIL = IVFAIL + 1                                               08130308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08140308
 0611 CONTINUE                                                          08150308
C                                                                       08160308
C     ****  FCVS PROGRAM 308  -  TEST 061  ****                         08170308
C                                                                       08180308
C     TEST 061 ATTEMPTS TO OVERRIDE THE TYPING OF INTEGER FOR THE       08190308
C     INTRINSIC FUNCTION NINT WITH IMPLICIT REAL TYPING.                08200308
C                                                                       08210308
      IVTNUM =  61                                                      08220308
      IF (ICZERO) 30610, 0610, 30610                                    08230308
 0610 CONTINUE                                                          08240308
      RVCOMP = 10.0                                                     08250308
      RVON01 = 3.78                                                     08260308
      RVCOMP = NINT(RVON01) / 5                                         08270308
      RVCORR = 0.0                                                      08280308
40610 IF (RVCOMP + .00005) 20610, 10610, 40611                          08290308
40611 IF (RVCOMP - .00005) 10610, 10610, 20610                          08300308
30610 IVDELE = IVDELE + 1                                               08310308
      WRITE (I02,80000) IVTNUM                                          08320308
      IF (ICZERO) 10610, 0621, 20610                                    08330308
10610 IVPASS = IVPASS + 1                                               08340308
      WRITE (I02,80002) IVTNUM                                          08350308
      GO TO 0621                                                        08360308
20610 IVFAIL = IVFAIL + 1                                               08370308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08380308
 0621 CONTINUE                                                          08390308
C                                                                       08400308
C     ****  FCVS PROGRAM 308  -  TEST 062  ****                         08410308
C                                                                       08420308
C     TEST 062 ATTEMPTS TO OVERRIDE THE TYPING OF REAL FOR THE          08430308
C     INTRINSIC FUNCTION SINH WITH TYPE-STATEMENT TYPING OF INTEGER.    08440308
C                                                                       08450308
      IVTNUM =  62                                                      08460308
      IF (ICZERO) 30620, 0620, 30620                                    08470308
 0620 CONTINUE                                                          08480308
      RVCOMP = 10.0                                                     08490308
      RVCOMP = SINH(2.0)                                                08500308
      RVCORR = 3.6269                                                   08510308
40620 IF (RVCOMP - 3.6264) 20620, 10620, 40621                          08520308
40621 IF (RVCOMP - 3.6274) 10620, 10620, 20620                          08530308
30620 IVDELE = IVDELE + 1                                               08540308
      WRITE (I02,80000) IVTNUM                                          08550308
      IF (ICZERO) 10620, 0631, 20620                                    08560308
10620 IVPASS = IVPASS + 1                                               08570308
      WRITE (I02,80002) IVTNUM                                          08580308
      GO TO 0631                                                        08590308
20620 IVFAIL = IVFAIL + 1                                               08600308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08610308
 0631 CONTINUE                                                          08620308
C                                                                       08630308
C     ****  FCVS PROGRAM 308  -  TEST 063  ****                         08640308
C                                                                       08650308
C     TEST 063 ATTEMPTS TO OVERRIDE THE TYPING OF INTEGER FOR THE       08660308
C     INTRINSIC FUNCTION MAX1 WITH TYPE-STATEMENT TYPING OF REAL.       08670308
C                                                                       08680308
      IVTNUM =  63                                                      08690308
      IF (ICZERO) 30630, 0630, 30630                                    08700308
 0630 CONTINUE                                                          08710308
      RVCOMP = 10.0                                                     08720308
      RVCOMP = MAX1(2.3, 3.1, 4.4) / 5                                  08730308
      RVCORR = 0.0                                                      08740308
40630 IF (RVCOMP + .00005) 20630, 10630, 40631                          08750308
40631 IF (RVCOMP - .00005) 10630, 10630, 20630                          08760308
30630 IVDELE = IVDELE + 1                                               08770308
      WRITE (I02,80000) IVTNUM                                          08780308
      IF (ICZERO) 10630, 0641, 20630                                    08790308
10630 IVPASS = IVPASS + 1                                               08800308
      WRITE (I02,80002) IVTNUM                                          08810308
      GO TO 0641                                                        08820308
20630 IVFAIL = IVFAIL + 1                                               08830308
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08840308
 0641 CONTINUE                                                          08850308
C                                                                       08860308
C                                                                       08870308
C     WRITE OUT TEST SUMMARY                                            08880308
C                                                                       08890308
      WRITE (I02,90004)                                                 08900308
      WRITE (I02,90014)                                                 08910308
      WRITE (I02,90004)                                                 08920308
      WRITE (I02,90000)                                                 08930308
      WRITE (I02,90004)                                                 08940308
      WRITE (I02,90020) IVFAIL                                          08950308
      WRITE (I02,90022) IVPASS                                          08960308
      WRITE (I02,90024) IVDELE                                          08970308
      STOP                                                              08980308
90001 FORMAT (1H ,24X,5HFM308)                                          08990308
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM308)                          09000308
C                                                                       09010308
C     FORMATS FOR TEST DETAIL LINES                                     09020308
C                                                                       09030308
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   09040308
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      09050308
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         09060308
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    09070308
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        09080308
C                                                                       09090308
C     FORMAT STATEMENTS FOR PAGE HEADERS                                09100308
C                                                                       09110308
90002 FORMAT (1H1)                                                      09120308
90004 FORMAT (1H )                                                      09130308
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09140308
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   09150308
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         09160308
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  09170308
90014 FORMAT (1H ,5X,46H----------------------------------------------) 09180308
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             09190308
C                                                                       09200308
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 09210308
C                                                                       09220308
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              09230308
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              09240308
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             09250308
      END                                                               09260308
      REAL FUNCTION FF309(RDON01)                                       00010309
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED TO INCREMENT THE ARGUMENT VALUE BY     00020309
C     ONE AND RETURN THE RESULT AS THE FUNCTION VALUE.                  00030309
      FF309 = RDON01 + 1.0                                              00040309
      RETURN                                                            00050309
      END                                                               00060309
      REAL FUNCTION FF310 ( )                                           00010310
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED TO INCREMENT BY ONE A VALUE PASSED     00020310
C     TO THE FUNCTION THROUGH COMMON.                                   00030310
      COMMON RVCN01                                                     00040310
      FF310 = RVCN01 + 1.0                                              00050310
      RETURN                                                            00060310
      END                                                               00070310
