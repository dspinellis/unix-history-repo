      PROGRAM FM307                                                     00010307
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020307
C                                                                       00030307
C          THIS ROUTINE TESTS INTRINSIC FUNCTIONS WHERE THE FUNCTION    00040307
C     TYPE IS REAL AND THE ARGUMENTS ARE EITHER INTEGER OR REAL.  THE   00050307
C     FUNCTION NINT IS AN EXCEPTION AND HAS AN INTEGER FUNCTION TYPE.   00060307
C     THE REAL OR INTEGER ARGUMENTS CONSIST OF POSITIVE, NEGATIVE AND   00070307
C     UNSIGNED CONSTANTS, VARIABLES AND ARRAY ELEMENT VALUES.  EACH     00080307
C     INTRINSIC FUNCTION IS TESTED WITH THREE OR FOUR DIFFERENT         00090307
C     COMBINATIONS OF ACTUAL ARGUMENTS DESIGNED TO TEST NOT ONLY THE    00100307
C     VARIOUS COMBINATIONS OF DATA USAGES BUT ALSO TO TEST THE RANGE OF 00110307
C     ARGUMENT AND FUNCTION VALUES, WHERE THAT IS APPROPRIATE.  THE     00120307
C     INTRINSIC FUNCTIONS TESTED IN THIS ROUTINE INCLUDE.               00130307
C                                                                       00140307
C                                        SPECIFIC        TYPE  OF       00150307
C          INTRINSIC FUNCTION            NAME        ARGUMENT   FUNCTION00160307
C          ------------------            ------      --------   --------00170307
C          CONVERSION TO REAL            REAL        INTEGER    REAL    00180307
C          NEAREST WHOLE NUMBER          ANINT       REAL       REAL    00190307
C          NEAREST INTEGER               NINT        REAL       INTEGER 00200307
C          TANGENT                       TAN         REAL       REAL    00210307
C          ARCSINE                       ASIN        REAL       REAL    00220307
C          ARCCOSINE                     ACOS        REAL       REAL    00230307
C          HYPERBOLIC SINE               SINH        REAL       REAL    00240307
C          HYPERBOLIC COSINE             COSH        REAL       REAL    00250307
C                                                                       00260307
C          SUBSET LEVEL ROUTINES FM097 THROUGH FM099 AND FM308 ALSO     00270307
C     TEST THE USE OF INTEGER AND REAL INTRINSIC FUNCTIONS.             00280307
C                                                                       00290307
C     REFERENCES.                                                       00300307
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00310307
C           X3.9-1978                                                   00320307
C                                                                       00330307
C        SECTION 15.3,     INTRINSIC FUNCTIONS                          00340307
C        SECTION 15.9.2,   ACTUAL ARGUMENTS                             00350307
C        SECTION 15.9.3,   ASSOCIATION OF DUMMY AND ACTUAL ARGUMENTS    00360307
C        TABLE 5,          INTRINSIC FUNCTIONS (INCLUDING NOTES)        00370307
C        SECTION 15.10.1,  RESTRICTION ON RANGE OF ARGUMENTS AND RESULTS00380307
C                                                                       00390307
C                                                                       00400307
C     ******************************************************************00410307
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00420307
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00430307
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00440307
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00450307
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00460307
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00470307
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00480307
C     THE RESULT OF EXECUTING THESE TESTS.                              00490307
C                                                                       00500307
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00510307
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00520307
C                                                                       00530307
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00540307
C                    DEPARTMENT OF THE NAVY                             00550307
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00560307
C                    WASHINGTON, D.C.   20376                           00570307
C                                                                       00580307
C     ******************************************************************00590307
C                                                                       00600307
C                                                                       00610307
      IMPLICIT LOGICAL (L)                                              00620307
      IMPLICIT CHARACTER*14 (C)                                         00630307
C                                                                       00640307
      DIMENSION IAON11(4)                                               00650307
      DIMENSION RAON11(4)                                               00660307
      DATA PI/3.141592654/                                              00670307
C                                                                       00680307
C                                                                       00690307
C                                                                       00700307
C     INITIALIZATION SECTION.                                           00710307
C                                                                       00720307
C     INITIALIZE CONSTANTS                                              00730307
C     ********************                                              00740307
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00750307
      I01 = 5                                                           00760307
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00770307
      I02 = 6                                                           00780307
C     SYSTEM ENVIRONMENT SECTION                                        00790307
C                                                                       00800307
      I01 = 5                                                           00810307
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00820307
C     (UNIT NUMBER FOR CARD READER).                                    00830307
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00840307
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00850307
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00860307
C                                                                       00870307
      I02 = 6                                                           00880307
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00890307
C     (UNIT NUMBER FOR PRINTER).                                        00900307
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00910307
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00920307
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00930307
C                                                                       00940307
      IVPASS = 0                                                        00950307
      IVFAIL = 0                                                        00960307
      IVDELE = 0                                                        00970307
      ICZERO = 0                                                        00980307
C                                                                       00990307
C     WRITE OUT PAGE HEADERS                                            01000307
C                                                                       01010307
      WRITE (I02,90002)                                                 01020307
      WRITE (I02,90006)                                                 01030307
      WRITE (I02,90008)                                                 01040307
      WRITE (I02,90004)                                                 01050307
      WRITE (I02,90010)                                                 01060307
      WRITE (I02,90004)                                                 01070307
      WRITE (I02,90016)                                                 01080307
      WRITE (I02,90001)                                                 01090307
      WRITE (I02,90004)                                                 01100307
      WRITE (I02,90012)                                                 01110307
      WRITE (I02,90014)                                                 01120307
      WRITE (I02,90004)                                                 01130307
C                                                                       01140307
C                                                                       01150307
C     TEST 001 THROUGH TEST 004 CONTAIN INTRINSIC FUNCTION TESTS FOR    01160307
C     TYPE CONVERSION TO REAL (REAL) WHERE THE FUNCTION IS REAL AND THE 01170307
C     ARGUMENT IS INTEGER.                                              01180307
C                                                                       01190307
C                                                                       01200307
C     ****  FCVS PROGRAM 307  -  TEST 001  ****                         01210307
C                                                                       01220307
C     CONSTANT ARGUMENT                                                 01230307
C                                                                       01240307
      IVTNUM =   1                                                      01250307
      IF (ICZERO) 30010, 0010, 30010                                    01260307
 0010 CONTINUE                                                          01270307
      RVCOMP = 10.0                                                     01280307
      RVCOMP = REAL (6)                                                 01290307
      RVCORR = 6.0                                                      01300307
40010 IF (RVCOMP - 5.9995) 20010,10010,40011                            01310307
40011 IF (RVCOMP - 6.0005) 10010,10010,20010                            01320307
30010 IVDELE = IVDELE + 1                                               01330307
      WRITE (I02,80000) IVTNUM                                          01340307
      IF (ICZERO) 10010, 0021, 20010                                    01350307
10010 IVPASS = IVPASS + 1                                               01360307
      WRITE (I02,80002) IVTNUM                                          01370307
      GO TO 0021                                                        01380307
20010 IVFAIL = IVFAIL + 1                                               01390307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01400307
 0021 CONTINUE                                                          01410307
C                                                                       01420307
C     ****  FCVS PROGRAM 307  -  TEST 002  ****                         01430307
C                                                                       01440307
C     VARIABLE ARGUMENT                                                 01450307
C                                                                       01460307
      IVTNUM =   2                                                      01470307
      IF (ICZERO) 30020, 0020, 30020                                    01480307
 0020 CONTINUE                                                          01490307
      RVCOMP = 10.0                                                     01500307
      IVON01 = 6                                                        01510307
      RVCOMP = REAL (IVON01)                                            01520307
      RVCORR = 6.0                                                      01530307
40020 IF (RVCOMP - 5.9995) 20020,10020,40021                            01540307
40021 IF (RVCOMP - 6.0005) 10020, 10020, 20020                          01550307
30020 IVDELE = IVDELE + 1                                               01560307
      WRITE (I02,80000) IVTNUM                                          01570307
      IF (ICZERO) 10020, 0031, 20020                                    01580307
10020 IVPASS = IVPASS + 1                                               01590307
      WRITE (I02,80002) IVTNUM                                          01600307
      GO TO 0031                                                        01610307
20020 IVFAIL = IVFAIL + 1                                               01620307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01630307
 0031 CONTINUE                                                          01640307
C                                                                       01650307
C     ****  FCVS PROGRAM 307  -  TEST 003  ****                         01660307
C                                                                       01670307
C     ARRAY ELEMENT NAME ARGUMENT                                       01680307
C                                                                       01690307
      IVTNUM =   3                                                      01700307
      IF (ICZERO) 30030, 0030, 30030                                    01710307
 0030 CONTINUE                                                          01720307
      RVCOMP = 10.0                                                     01730307
      IAON11(3) = 6                                                     01740307
      RVCOMP = REAL (IAON11(3))                                         01750307
      RVCORR = 6.0                                                      01760307
40030 IF (RVCOMP - 5.9995) 20030, 10030, 40031                          01770307
40031 IF (RVCOMP - 6.0005) 10030, 10030, 20030                          01780307
30030 IVDELE = IVDELE + 1                                               01790307
      WRITE (I02,80000) IVTNUM                                          01800307
      IF (ICZERO) 10030, 0041, 20030                                    01810307
10030 IVPASS = IVPASS + 1                                               01820307
      WRITE (I02,80002) IVTNUM                                          01830307
      GO TO 0041                                                        01840307
20030 IVFAIL = IVFAIL + 1                                               01850307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          01860307
 0041 CONTINUE                                                          01870307
C                                                                       01880307
C     ****  FCVS PROGRAM 307  -  TEST 004  ****                         01890307
C                                                                       01900307
C     EXPRESSION AS ARGUMENT                                            01910307
C                                                                       01920307
      IVTNUM =   4                                                      01930307
      IF (ICZERO) 30040, 0040, 30040                                    01940307
 0040 CONTINUE                                                          01950307
      RVCOMP = 10.0                                                     01960307
      IVON01 = 6                                                        01970307
      RVCOMP = REAL (IVON01 - 6)                                        01980307
      RVCORR = 0.0                                                      01990307
40040 IF(RVCOMP + .00005) 20040, 10040, 40041                           02000307
40041 IF(RVCOMP - .00005) 10040, 10040, 20040                           02010307
30040 IVDELE = IVDELE + 1                                               02020307
      WRITE (I02,80000) IVTNUM                                          02030307
      IF (ICZERO) 10040, 0051, 20040                                    02040307
10040 IVPASS = IVPASS + 1                                               02050307
      WRITE (I02,80002) IVTNUM                                          02060307
      GO TO 0051                                                        02070307
20040 IVFAIL = IVFAIL + 1                                               02080307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02090307
 0051 CONTINUE                                                          02100307
C                                                                       02110307
C     TEST 005 THROUGH TEST 008 CONTAIN INTRINSIC FUNCTION TESTS FOR    02120307
C     FINDING THE NEAREST WHOLE NUMBER (ANINT) WHERE THE FUNCTION AND   02130307
C     ARGUMENT TYPES ARE BOTH REAL.                                     02140307
C                                                                       02150307
C                                                                       02160307
C     ****  FCVS PROGRAM 307  -  TEST 005  ****                         02170307
C                                                                       02180307
C     CONSTANT ARGUMENT                                                 02190307
C                                                                       02200307
      IVTNUM =   5                                                      02210307
      IF (ICZERO) 30050, 0050, 30050                                    02220307
 0050 CONTINUE                                                          02230307
      RVCOMP = 10.0                                                     02240307
      RVCOMP = ANINT (3.4994)                                           02250307
      RVCORR = 3.0                                                      02260307
40050 IF (RVCOMP - 2.9995) 20050, 10050, 40051                          02270307
40051 IF (RVCOMP - 3.0005) 10050, 10050, 20050                          02280307
30050 IVDELE = IVDELE + 1                                               02290307
      WRITE (I02,80000) IVTNUM                                          02300307
      IF (ICZERO) 10050, 0061, 20050                                    02310307
10050 IVPASS = IVPASS + 1                                               02320307
      WRITE (I02,80002) IVTNUM                                          02330307
      GO TO 0061                                                        02340307
20050 IVFAIL = IVFAIL + 1                                               02350307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02360307
 0061 CONTINUE                                                          02370307
C                                                                       02380307
C     ****  FCVS PROGRAM 307  -  TEST 006  ****                         02390307
C                                                                       02400307
C     VARIABLE ARGUMENT                                                 02410307
C                                                                       02420307
      IVTNUM =   6                                                      02430307
      IF (ICZERO) 30060, 0060, 30060                                    02440307
 0060 CONTINUE                                                          02450307
      RVCOMP = 10.0                                                     02460307
      RVON01 = -3.4994                                                  02470307
      RVCOMP = ANINT (RVON01)                                           02480307
      RVCORR = -3.0                                                     02490307
40060 IF (RVCOMP + 3.0005) 20060, 10060, 40061                          02500307
40061 IF (RVCOMP + 2.9995) 10060, 10060, 20060                          02510307
30060 IVDELE = IVDELE + 1                                               02520307
      WRITE (I02,80000) IVTNUM                                          02530307
      IF (ICZERO) 10060, 0071, 20060                                    02540307
10060 IVPASS = IVPASS + 1                                               02550307
      WRITE (I02,80002) IVTNUM                                          02560307
      GO TO 0071                                                        02570307
20060 IVFAIL = IVFAIL + 1                                               02580307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02590307
 0071 CONTINUE                                                          02600307
C                                                                       02610307
C     ****  FCVS PROGRAM 307  -  TEST 007  ****                         02620307
C                                                                       02630307
C     ARRAY ELEMENT NAME ARGUMENT                                       02640307
C                                                                       02650307
      IVTNUM =   7                                                      02660307
      IF (ICZERO) 30070, 0070, 30070                                    02670307
 0070 CONTINUE                                                          02680307
      RVCOMP = 10.0                                                     02690307
      RAON11(3) = 3.0000                                                02700307
      RVCOMP = ANINT (RAON11(3))                                        02710307
      RVCORR = 3.0                                                      02720307
40070 IF (RVCOMP - 2.9995) 20070, 10070, 40071                          02730307
40071 IF (RVCOMP - 3.0005) 10070, 10070, 20070                          02740307
30070 IVDELE = IVDELE + 1                                               02750307
      WRITE (I02,80000) IVTNUM                                          02760307
      IF (ICZERO) 10070, 0081, 20070                                    02770307
10070 IVPASS = IVPASS + 1                                               02780307
      WRITE (I02,80002) IVTNUM                                          02790307
      GO TO 0081                                                        02800307
20070 IVFAIL = IVFAIL + 1                                               02810307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02820307
 0081 CONTINUE                                                          02830307
C                                                                       02840307
C     ****  FCVS PROGRAM 307  -  TEST 008  ****                         02850307
C                                                                       02860307
C     ZERO ARGUMENT                                                     02870307
C                                                                       02880307
      IVTNUM =   8                                                      02890307
      IF (ICZERO) 30080, 0080, 30080                                    02900307
 0080 CONTINUE                                                          02910307
      RVCOMP = 10.0                                                     02920307
      RVCOMP = ANINT (0.0)                                              02930307
      RVCORR = 0.0                                                      02940307
40080 IF (RVCOMP) 20080, 10080, 20080                                   02950307
30080 IVDELE = IVDELE + 1                                               02960307
      WRITE (I02,80000) IVTNUM                                          02970307
      IF (ICZERO) 10080, 0091, 20080                                    02980307
10080 IVPASS = IVPASS + 1                                               02990307
      WRITE (I02,80002) IVTNUM                                          03000307
      GO TO 0091                                                        03010307
20080 IVFAIL = IVFAIL + 1                                               03020307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03030307
 0091 CONTINUE                                                          03040307
C                                                                       03050307
C     TEST 009 THROUGH TEST 012 CONTAIN INTRINSIC FUNCTION TESTS FOR    03060307
C     FINDING THE NEAREST INTEGER (NINT) WHERE THE ARGUMENT IS REAL     03070307
C     AND THE FUNCTION TYPE IS INTEGER.                                 03080307
C                                                                       03090307
C                                                                       03100307
C     ****  FCVS PROGRAM 307  -  TEST 009  ****                         03110307
C                                                                       03120307
C     CONSTANT ARGUMENT                                                 03130307
C                                                                       03140307
      IVTNUM =   9                                                      03150307
      IF (ICZERO) 30090, 0090, 30090                                    03160307
 0090 CONTINUE                                                          03170307
      IVCOMP = 10                                                       03180307
      IVCOMP = NINT (3.4994)                                            03190307
      IVCORR = 3                                                        03200307
40090 IF (IVCOMP - 3) 20090, 10090, 20090                               03210307
30090 IVDELE = IVDELE + 1                                               03220307
      WRITE (I02,80000) IVTNUM                                          03230307
      IF (ICZERO) 10090, 0101, 20090                                    03240307
10090 IVPASS = IVPASS + 1                                               03250307
      WRITE (I02,80002) IVTNUM                                          03260307
      GO TO 0101                                                        03270307
20090 IVFAIL = IVFAIL + 1                                               03280307
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03290307
 0101 CONTINUE                                                          03300307
C                                                                       03310307
C     ****  FCVS PROGRAM 307  -  TEST 010  ****                         03320307
C                                                                       03330307
C     VARIABLE ARGUMENT                                                 03340307
C                                                                       03350307
      IVTNUM =  10                                                      03360307
      IF (ICZERO) 30100, 0100, 30100                                    03370307
 0100 CONTINUE                                                          03380307
      IVCOMP = 10                                                       03390307
      RVON01 = -3.4994                                                  03400307
      IVCOMP = NINT (RVON01)                                            03410307
      IVCORR = -3                                                       03420307
40100 IF (IVCOMP +3) 20100, 10100, 20100                                03430307
30100 IVDELE = IVDELE + 1                                               03440307
      WRITE (I02,80000) IVTNUM                                          03450307
      IF (ICZERO) 10100, 0111, 20100                                    03460307
10100 IVPASS = IVPASS + 1                                               03470307
      WRITE (I02,80002) IVTNUM                                          03480307
      GO TO 0111                                                        03490307
20100 IVFAIL = IVFAIL + 1                                               03500307
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03510307
 0111 CONTINUE                                                          03520307
C                                                                       03530307
C     ****  FCVS PROGRAM 307  -  TEST 011  ****                         03540307
C                                                                       03550307
C     ARRAY ELEMENT NAME ARGUMENT                                       03560307
C                                                                       03570307
      IVTNUM =  11                                                      03580307
      IF (ICZERO) 30110, 0110, 30110                                    03590307
 0110 CONTINUE                                                          03600307
      IVCOMP = 10                                                       03610307
      RAON11(1) = 3.0000                                                03620307
      IVCOMP = NINT (RAON11(1))                                         03630307
      IVCORR = 3                                                        03640307
40110 IF (IVCOMP -3) 20110, 10110, 20110                                03650307
30110 IVDELE = IVDELE + 1                                               03660307
      WRITE (I02,80000) IVTNUM                                          03670307
      IF (ICZERO) 10110, 0121, 20110                                    03680307
10110 IVPASS = IVPASS + 1                                               03690307
      WRITE (I02,80002) IVTNUM                                          03700307
      GO TO 0121                                                        03710307
20110 IVFAIL = IVFAIL + 1                                               03720307
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03730307
 0121 CONTINUE                                                          03740307
C                                                                       03750307
C     ****  FCVS PROGRAM 307  -  TEST 012  ****                         03760307
C                                                                       03770307
C     ZERO ARGUMENT                                                     03780307
C                                                                       03790307
      IVTNUM =  12                                                      03800307
      IF (ICZERO) 30120, 0120, 30120                                    03810307
 0120 CONTINUE                                                          03820307
      IVCOMP = 10                                                       03830307
      IVCOMP = NINT (0.0)                                               03840307
      IVCORR = 0                                                        03850307
40120 IF (IVCOMP) 20120, 10120, 20120                                   03860307
30120 IVDELE = IVDELE + 1                                               03870307
      WRITE (I02,80000) IVTNUM                                          03880307
      IF (ICZERO) 10120, 0131, 20120                                    03890307
10120 IVPASS = IVPASS + 1                                               03900307
      WRITE (I02,80002) IVTNUM                                          03910307
      GO TO 0131                                                        03920307
20120 IVFAIL = IVFAIL + 1                                               03930307
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03940307
 0131 CONTINUE                                                          03950307
C                                                                       03960307
C     TEST 013 THROUGH TEST 017 CONTAIN INTRINSIC FUNCTION TESTS FOR    03970307
C     FINDING THE TRIGONOMETRIC TANGENT (TAN) WHERE THE FUNCTION AND    03980307
C     ARGUMENT TYPES ARE BOTH REAL.  ALL ARGUMENTS ARE GIVEN IN RADIANS 03990307
C     WHERE ONE RADIAN EQUALS 57.296 DEGREES.                           04000307
C                                                                       04010307
C                                                                       04020307
C     ****  FCVS PROGRAM 307  -  TEST 013  ****                         04030307
C                                                                       04040307
C     FIND THE TANGENT OF 0 DEGREES (0.0 RADIANS)                       04050307
C                                                                       04060307
      IVTNUM =  13                                                      04070307
      IF (ICZERO) 30130, 0130, 30130                                    04080307
 0130 CONTINUE                                                          04090307
      RVCOMP = 10.0                                                     04100307
      RVCOMP = TAN (0.0)                                                04110307
      RVCORR = 0.0                                                      04120307
40130 IF (RVCOMP) 20130, 10130, 20130                                   04130307
30130 IVDELE = IVDELE + 1                                               04140307
      WRITE (I02,80000) IVTNUM                                          04150307
      IF (ICZERO) 10130, 0141, 20130                                    04160307
10130 IVPASS = IVPASS + 1                                               04170307
      WRITE (I02,80002) IVTNUM                                          04180307
      GO TO 0141                                                        04190307
20130 IVFAIL = IVFAIL + 1                                               04200307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04210307
 0141 CONTINUE                                                          04220307
C                                                                       04230307
C     ****  FCVS PROGRAM 307  -  TEST 014  ****                         04240307
C                                                                       04250307
C     FIND THE TANGENT OF 135 DEGREES (2.3562 RADIANS)                  04260307
C                                                                       04270307
      IVTNUM =  14                                                      04280307
      IF (ICZERO) 30140, 0140, 30140                                    04290307
 0140 CONTINUE                                                          04300307
      RVCOMP = 10.0                                                     04310307
      RVON01 = 3 * PI / 4                                               04320307
      RVCOMP = TAN (RVON01)                                             04330307
      RVCORR = -1.0                                                     04340307
40140 IF (RVCOMP + 1.0005) 20140, 10140, 40141                          04350307
40141 IF (RVCOMP + .9995) 10140, 10140, 20140                           04360307
30140 IVDELE = IVDELE + 1                                               04370307
      WRITE (I02,80000) IVTNUM                                          04380307
      IF (ICZERO) 10140, 0151, 20140                                    04390307
10140 IVPASS = IVPASS + 1                                               04400307
      WRITE (I02,80002) IVTNUM                                          04410307
      GO TO 0151                                                        04420307
20140 IVFAIL = IVFAIL + 1                                               04430307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04440307
 0151 CONTINUE                                                          04450307
C                                                                       04460307
C     ****  FCVS PROGRAM 307  -  TEST 015  ****                         04470307
C                                                                       04480307
C     FIND THE TANGENT OF 540 DEGREES (9.4248 RADIANS)                  04490307
C                                                                       04500307
      IVTNUM =  15                                                      04510307
      IF (ICZERO) 30150, 0150, 30150                                    04520307
 0150 CONTINUE                                                          04530307
      RVCOMP = 10.0                                                     04540307
      RAON11(2) = 3 * PI                                                04550307
      RVCOMP = TAN (RAON11(2))                                          04560307
      RVCORR = 0.0                                                      04570307
40150 IF (RVCOMP + .00005) 20150, 10150, 40151                          04580307
40151 IF (RVCOMP - .00005) 10150, 10150, 20150                          04590307
30150 IVDELE = IVDELE + 1                                               04600307
      WRITE (I02,80000) IVTNUM                                          04610307
      IF (ICZERO) 10150, 0161, 20150                                    04620307
10150 IVPASS = IVPASS + 1                                               04630307
      WRITE (I02,80002) IVTNUM                                          04640307
      GO TO 0161                                                        04650307
20150 IVFAIL = IVFAIL + 1                                               04660307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04670307
 0161 CONTINUE                                                          04680307
C                                                                       04690307
C     ****  FCVS PROGRAM 307  -  TEST 016  ****                         04700307
C                                                                       04710307
C     FIND THE TANGENT OF 30 DEGREES (.52360 RADIANS)                   04720307
C                                                                       04730307
      IVTNUM =  16                                                      04740307
      IF (ICZERO) 30160, 0160, 30160                                    04750307
 0160 CONTINUE                                                          04760307
      RVCOMP = 10.0                                                     04770307
      RVON01 = PI/6                                                     04780307
      RVCOMP = TAN (RVON01)                                             04790307
      RVCORR = .57735                                                   04800307
40160 IF (RVCOMP - .57730) 20160, 10160, 40161                          04810307
40161 IF (RVCOMP - .57740) 10160, 10160, 20160                          04820307
30160 IVDELE = IVDELE + 1                                               04830307
      WRITE (I02,80000) IVTNUM                                          04840307
      IF (ICZERO) 10160, 0171, 20160                                    04850307
10160 IVPASS = IVPASS + 1                                               04860307
      WRITE (I02,80002) IVTNUM                                          04870307
      GO TO 0171                                                        04880307
20160 IVFAIL = IVFAIL + 1                                               04890307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04900307
 0171 CONTINUE                                                          04910307
C                                                                       04920307
C     ****  FCVS PROGRAM 307  -  TEST 017  ****                         04930307
C                                                                       04940307
C     FIND THE TANGENT OF 30 DEGREES BY DIVIDING THE SINE OF 30 DEGREES 04950307
C     BY THE COSINE OF 30 DEGREES.                                      04960307
C                                                                       04970307
      IVTNUM =  17                                                      04980307
      IF (ICZERO) 30170, 0170, 30170                                    04990307
 0170 CONTINUE                                                          05000307
      RVCOMP = 10.0                                                     05010307
      RVON01 = PI/6                                                     05020307
      RVCOMP = SIN(RVON01)/COS(RVON01)                                  05030307
      RVCORR = .57735                                                   05040307
40170 IF (RVCOMP - .57730) 20170, 10170, 40171                          05050307
40171 IF (RVCOMP - .57740) 10170, 10170, 20170                          05060307
30170 IVDELE = IVDELE + 1                                               05070307
      WRITE (I02,80000) IVTNUM                                          05080307
      IF (ICZERO) 10170, 0181, 20170                                    05090307
10170 IVPASS = IVPASS + 1                                               05100307
      WRITE (I02,80002) IVTNUM                                          05110307
      GO TO 0181                                                        05120307
20170 IVFAIL = IVFAIL + 1                                               05130307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05140307
 0181 CONTINUE                                                          05150307
C                                                                       05160307
C     TEST 018 THROUGH TEST 021 CONTAIN INTRINSIC FUNCTION TESTS FOR    05170307
C     FINDING THE TRIGONOMETRIC ARCSINE (ASIN) WHERE THE FUNCTION AND   05180307
C     ARGUMENT TYPES ARE BOTH REAL.  THE ABSOLUTE VALUES OF ALL         05190307
C     ARGUMENTS ARE LESS THAN OR EQUAL TO ONE.  THE FUNCTION VALUES     05200307
C     ARE EXPRESSED IN RADIANS WHERE ONE RADIAN EQUALS 57.296 DEGREES.  05210307
C                                                                       05220307
C                                                                       05230307
C     ****  FCVS PROGRAM 307  -  TEST 018  ****                         05240307
C                                                                       05250307
C     THE ARCSINE OF +1. IS 90 DEGREES (1.5708 RADIANS)                 05260307
C                                                                       05270307
      IVTNUM =  18                                                      05280307
      IF (ICZERO) 30180, 0180, 30180                                    05290307
 0180 CONTINUE                                                          05300307
      RVCOMP = 10.0                                                     05310307
      RVCOMP = ASIN (+1.0)                                              05320307
      RVCORR = 1.5708                                                   05330307
40180 IF (RVCOMP - 1.5703) 20180, 10180, 40181                          05340307
40181 IF (RVCOMP - 1.5713) 10180, 10180, 20180                          05350307
30180 IVDELE = IVDELE + 1                                               05360307
      WRITE (I02,80000) IVTNUM                                          05370307
      IF (ICZERO) 10180, 0191, 20180                                    05380307
10180 IVPASS = IVPASS + 1                                               05390307
      WRITE (I02,80002) IVTNUM                                          05400307
      GO TO 0191                                                        05410307
20180 IVFAIL = IVFAIL + 1                                               05420307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05430307
 0191 CONTINUE                                                          05440307
C                                                                       05450307
C     ****  FCVS PROGRAM 307  -  TEST 019  ****                         05460307
C                                                                       05470307
C     THE ARCSINE OF -1. IS -90 DEGREES (-1.5708 RADIANS)               05480307
C                                                                       05490307
      IVTNUM =  19                                                      05500307
      IF (ICZERO) 30190, 0190, 30190                                    05510307
 0190 CONTINUE                                                          05520307
      RVCOMP = 10.0                                                     05530307
      RVON01 = -1.0                                                     05540307
      RVCOMP = ASIN(RVON01)                                             05550307
      RVCORR = -1.5708                                                  05560307
40190 IF (RVCOMP + 1.5713) 20190, 10190, 40191                          05570307
40191 IF (RVCOMP + 1.5703) 10190, 10190, 20190                          05580307
30190 IVDELE = IVDELE + 1                                               05590307
      WRITE (I02,80000) IVTNUM                                          05600307
      IF (ICZERO) 10190, 0201, 20190                                    05610307
10190 IVPASS = IVPASS + 1                                               05620307
      WRITE (I02,80002) IVTNUM                                          05630307
      GO TO 0201                                                        05640307
20190 IVFAIL = IVFAIL + 1                                               05650307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05660307
 0201 CONTINUE                                                          05670307
C                                                                       05680307
C     ****  FCVS PROGRAM 307  -  TEST 020  ****                         05690307
C                                                                       05700307
C     THE ARCSINE OF -.5 TS -30 DEGREES (-.52360 RADIANS)               05710307
C                                                                       05720307
      IVTNUM =  20                                                      05730307
      IF (ICZERO) 30200, 0200, 30200                                    05740307
 0200 CONTINUE                                                          05750307
      RVCOMP = 10.0                                                     05760307
      RAON11(1) = -.5                                                   05770307
      RVCOMP = ASIN (RAON11(1))                                         05780307
      RVCORR = -.52360                                                  05790307
40200 IF (RVCOMP + .52365) 20200, 10200, 40201                          05800307
40201 IF (RVCOMP + .52355) 10200, 10200, 20200                          05810307
30200 IVDELE = IVDELE + 1                                               05820307
      WRITE (I02,80000) IVTNUM                                          05830307
      IF (ICZERO) 10200, 0211, 20200                                    05840307
10200 IVPASS = IVPASS + 1                                               05850307
      WRITE (I02,80002) IVTNUM                                          05860307
      GO TO 0211                                                        05870307
20200 IVFAIL = IVFAIL + 1                                               05880307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05890307
 0211 CONTINUE                                                          05900307
C                                                                       05910307
C     ****  FCVS PROGRAM 307  -  TEST 021  ****                         05920307
C                                                                       05930307
C     THE ARCSINE OF 0.0 IS 0 DEGREES (0.0 RADIANS)                     05940307
C                                                                       05950307
      IVTNUM =  21                                                      05960307
      IF (ICZERO) 30210, 0210, 30210                                    05970307
 0210 CONTINUE                                                          05980307
      RVCOMP = 10.0                                                     05990307
      RVON01 = 0.0                                                      06000307
      RVCOMP = ASIN (RVON01)                                            06010307
      RVCORR = 0.0                                                      06020307
40210 IF (RVCOMP) 20210, 10210, 20210                                   06030307
30210 IVDELE = IVDELE + 1                                               06040307
      WRITE (I02,80000) IVTNUM                                          06050307
      IF (ICZERO) 10210, 0221, 20210                                    06060307
10210 IVPASS = IVPASS + 1                                               06070307
      WRITE (I02,80002) IVTNUM                                          06080307
      GO TO 0221                                                        06090307
20210 IVFAIL = IVFAIL + 1                                               06100307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06110307
 0221 CONTINUE                                                          06120307
C                                                                       06130307
C     TEST 022 THROUGH TEST 025 CONTAIN INTRINSIC FUNCTION TESTS FOR    06140307
C     FINDING THE TRIGONOMETRIC ARCCOSINE (ACOS) WHERE THE FUNCTION     06150307
C     AND ARGUMENT TYPES ARE BOTH REAL.  THE ABSOLUTE VALUES ALL        06160307
C     ARGUMENTS ARE LESS THAN OR EQUAL TO ONE.  THE FUNCTION VALUES     06170307
C     ARE EXPRESSED IN RADIANS WHERE ONE RADIAN EQUALS 57.296 DEGREES.  06180307
C                                                                       06190307
C                                                                       06200307
C     ****  FCVS PROGRAM 307  -  TEST 022  ****                         06210307
C                                                                       06220307
C     THE ARCCOSINE OF +1. IS 0 DEGREES ( 0.0 RADIANS)                  06230307
C                                                                       06240307
      IVTNUM =  22                                                      06250307
      IF (ICZERO) 30220, 0220, 30220                                    06260307
 0220 CONTINUE                                                          06270307
      RVCOMP = 10.0                                                     06280307
      RVCOMP = ACOS(+1.)                                                06290307
      RVCORR = 0.0                                                      06300307
40220 IF (RVCOMP + .00005) 20220, 10220, 40221                          06310307
40221 IF (RVCOMP - .00005) 10220, 10220, 20220                          06320307
30220 IVDELE = IVDELE + 1                                               06330307
      WRITE (I02,80000) IVTNUM                                          06340307
      IF (ICZERO) 10220, 0231, 20220                                    06350307
10220 IVPASS = IVPASS + 1                                               06360307
      WRITE (I02,80002) IVTNUM                                          06370307
      GO TO 0231                                                        06380307
20220 IVFAIL = IVFAIL + 1                                               06390307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06400307
 0231 CONTINUE                                                          06410307
C                                                                       06420307
C     ****  FCVS PROGRAM 307  -  TEST 023  ****                         06430307
C                                                                       06440307
C     THE ARCCOSINE OF -1. IS 180 DEGREES (3.1416 RADIANS)              06450307
C                                                                       06460307
      IVTNUM =  23                                                      06470307
      IF (ICZERO) 30230, 0230, 30230                                    06480307
 0230 CONTINUE                                                          06490307
      RVCOMP = 10.0                                                     06500307
      RVON01 = -1.0                                                     06510307
      RVCOMP = ACOS (RVON01)                                            06520307
      RVCORR = 3.1416                                                   06530307
40230 IF (RVCOMP - 3.1411) 20230, 10230, 40231                          06540307
40231 IF (RVCOMP - 3.1421) 10230, 10230, 20230                          06550307
30230 IVDELE = IVDELE + 1                                               06560307
      WRITE (I02,80000) IVTNUM                                          06570307
      IF (ICZERO) 10230, 0241, 20230                                    06580307
10230 IVPASS = IVPASS + 1                                               06590307
      WRITE (I02,80002) IVTNUM                                          06600307
      GO TO 0241                                                        06610307
20230 IVFAIL = IVFAIL + 1                                               06620307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06630307
 0241 CONTINUE                                                          06640307
C                                                                       06650307
C     ****  FCVS PROGRAM 307  -  TEST 024  ****                         06660307
C                                                                       06670307
C     THE ARCCOSINE OF -.5 IS 120 DEGREES (2.0944 RADIANS)              06680307
C                                                                       06690307
      IVTNUM =  24                                                      06700307
      IF (ICZERO) 30240, 0240, 30240                                    06710307
 0240 CONTINUE                                                          06720307
      RVCOMP = 10.0                                                     06730307
      RAON11(1) = -.5                                                   06740307
      RVCOMP = ACOS (RAON11(1))                                         06750307
      RVCORR = 2.0944                                                   06760307
40240 IF (RVCOMP - 2.0939) 20240, 10240, 40241                          06770307
40241 IF (RVCOMP - 2.0949) 10240, 10240, 20240                          06780307
30240 IVDELE = IVDELE + 1                                               06790307
      WRITE (I02,80000) IVTNUM                                          06800307
      IF (ICZERO) 10240, 0251, 20240                                    06810307
10240 IVPASS = IVPASS + 1                                               06820307
      WRITE (I02,80002) IVTNUM                                          06830307
      GO TO 0251                                                        06840307
20240 IVFAIL = IVFAIL + 1                                               06850307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06860307
 0251 CONTINUE                                                          06870307
C                                                                       06880307
C     ****  FCVS PROGRAM 307  -  TEST 025  ****                         06890307
C                                                                       06900307
C     THE ARCCOSINE OF 0.0 IS 90 DEGREES (1.5708 RADIANS)               06910307
C                                                                       06920307
      IVTNUM =  25                                                      06930307
      IF (ICZERO) 30250, 0250, 30250                                    06940307
 0250 CONTINUE                                                          06950307
      RVCOMP = 10.0                                                     06960307
      RVCOMP = ACOS (0.)                                                06970307
      RVCORR = 1.5708                                                   06980307
40250 IF (RVCOMP - 1.5703) 20250, 10250, 40251                          06990307
40251 IF (RVCOMP - 1.5713) 10250, 10250, 20250                          07000307
30250 IVDELE = IVDELE + 1                                               07010307
      WRITE (I02,80000) IVTNUM                                          07020307
      IF (ICZERO) 10250, 0261, 20250                                    07030307
10250 IVPASS = IVPASS + 1                                               07040307
      WRITE (I02,80002) IVTNUM                                          07050307
      GO TO 0261                                                        07060307
20250 IVFAIL = IVFAIL + 1                                               07070307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07080307
 0261 CONTINUE                                                          07090307
C                                                                       07100307
C     TEST 026 THROUGH TEST 028 CONTAIN INTRINSIC FUNCTION TESTS FOR    07110307
C     FINDING THE HYPERBOLIC SINE (SINH) WHERE THE FUNCTION AND         07120307
C     ARGUMENT TYPES ARE BOTH REAL.  ONLY POSITIVE ARGUMENTS ARE        07130307
C     TESTED.                                                           07140307
C                                                                       07150307
C                                                                       07160307
C     ****  FCVS PROGRAM 307  -  TEST 026  ****                         07170307
C                                                                       07180307
C     CONSTANT ARGUMENT                                                 07190307
C                                                                       07200307
      IVTNUM =  26                                                      07210307
      IF (ICZERO) 30260, 0260, 30260                                    07220307
 0260 CONTINUE                                                          07230307
      RVCOMP = 10.0                                                     07240307
      RVCOMP = SINH (0.0)                                               07250307
      RVCORR = 0.0                                                      07260307
40260 IF (RVCOMP + .00005) 20260, 10260, 40261                          07270307
40261 IF (RVCOMP - .00005) 10260, 10260, 20260                          07280307
30260 IVDELE = IVDELE + 1                                               07290307
      WRITE (I02,80000) IVTNUM                                          07300307
      IF (ICZERO) 10260, 0271, 20260                                    07310307
10260 IVPASS = IVPASS + 1                                               07320307
      WRITE (I02,80002) IVTNUM                                          07330307
      GO TO 0271                                                        07340307
20260 IVFAIL = IVFAIL + 1                                               07350307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07360307
 0271 CONTINUE                                                          07370307
C                                                                       07380307
C     ****  FCVS PROGRAM 307  -  TEST 027  ****                         07390307
C                                                                       07400307
C     VARIABLE ARGUMENT                                                 07410307
C                                                                       07420307
      IVTNUM =  27                                                      07430307
      IF (ICZERO) 30270, 0270, 30270                                    07440307
 0270 CONTINUE                                                          07450307
      RVCOMP =10.0                                                      07460307
      RVON01 = 2.0                                                      07470307
      RVCOMP = SINH (RVON01)                                            07480307
      RVCORR = 3.6269                                                   07490307
40270 IF (RVCOMP - 3.6264) 20270, 10270, 40271                          07500307
40271 IF (RVCOMP - 3.6274) 10270, 10270, 20270                          07510307
30270 IVDELE = IVDELE + 1                                               07520307
      WRITE (I02,80000) IVTNUM                                          07530307
      IF (ICZERO) 10270, 0281, 20270                                    07540307
10270 IVPASS = IVPASS + 1                                               07550307
      WRITE (I02,80002) IVTNUM                                          07560307
      GO TO 0281                                                        07570307
20270 IVFAIL = IVFAIL + 1                                               07580307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07590307
 0281 CONTINUE                                                          07600307
C                                                                       07610307
C     ****  FCVS PROGRAM 307  -  TEST 028  ****                         07620307
C                                                                       07630307
C     ARRAY ELEMENT NAME ARGUMENT                                       07640307
C                                                                       07650307
      IVTNUM =  28                                                      07660307
      IF (ICZERO) 30280, 0280, 30280                                    07670307
 0280 CONTINUE                                                          07680307
      RVCOMP = 10.0                                                     07690307
      RAON11(1) = 6.0                                                   07700307
      RVCOMP = SINH (RAON11(1))                                         07710307
      RVCORR = 201.71                                                   07720307
40280 IF (RVCOMP - 201.66) 20280, 10280, 40281                          07730307
40281 IF (RVCOMP - 201.76) 10280, 10280, 20280                          07740307
30280 IVDELE = IVDELE + 1                                               07750307
      WRITE (I02,80000) IVTNUM                                          07760307
      IF (ICZERO) 10280, 0291, 20280                                    07770307
10280 IVPASS = IVPASS + 1                                               07780307
      WRITE (I02,80002) IVTNUM                                          07790307
      GO TO 0291                                                        07800307
20280 IVFAIL = IVFAIL + 1                                               07810307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07820307
 0291 CONTINUE                                                          07830307
C                                                                       07840307
C     TEST 029 THROUGH TEST 031 CONTAIN INTRINSIC FUNCTION TESTS FOR    07850307
C     FINDING THE HYPERBOLIC COSINE (COSH) WHERE THE FUNCTION AND       07860307
C     ARGUMENT TYPES ARE BOTH REAL.  ONLY POSITIVE ARGUMENTS ARE TESTED.07870307
C                                                                       07880307
C                                                                       07890307
C     ****  FCVS PROGRAM 307  -  TEST 029  ****                         07900307
C                                                                       07910307
C     CONSTANT ARGUMENT                                                 07920307
C                                                                       07930307
      IVTNUM =  29                                                      07940307
      IF (ICZERO) 30290, 0290, 30290                                    07950307
 0290 CONTINUE                                                          07960307
      RVCOMP = 10.0                                                     07970307
      RVCOMP = COSH (0.0)                                               07980307
      RVCORR = 1.0                                                      07990307
40290 IF (RVCOMP - .9995) 20290, 10290, 40291                           08000307
40291 IF (RVCOMP - 1.0005) 10290, 10290, 20290                          08010307
30290 IVDELE = IVDELE + 1                                               08020307
      WRITE (I02,80000) IVTNUM                                          08030307
      IF (ICZERO) 10290, 0301, 20290                                    08040307
10290 IVPASS = IVPASS + 1                                               08050307
      WRITE (I02,80002) IVTNUM                                          08060307
      GO TO 0301                                                        08070307
20290 IVFAIL = IVFAIL + 1                                               08080307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08090307
 0301 CONTINUE                                                          08100307
C                                                                       08110307
C     ****  FCVS PROGRAM 307  -  TEST 030  ****                         08120307
C                                                                       08130307
C     VARIABLE ARGUMENT                                                 08140307
C                                                                       08150307
      IVTNUM =  30                                                      08160307
      IF (ICZERO) 30300, 0300, 30300                                    08170307
 0300 CONTINUE                                                          08180307
      RVCOMP = 10.0                                                     08190307
      RVON01 = 2.0                                                      08200307
      RVCOMP = COSH (RVON01)                                            08210307
      RVCORR = 3.7622                                                   08220307
40300 IF (RVCOMP - 3.7617) 20300, 10300, 40301                          08230307
40301 IF (RVCOMP - 3.7627) 10300, 10300, 20300                          08240307
30300 IVDELE = IVDELE + 1                                               08250307
      WRITE (I02,80000) IVTNUM                                          08260307
      IF (ICZERO) 10300, 0311, 20300                                    08270307
10300 IVPASS = IVPASS + 1                                               08280307
      WRITE (I02,80002) IVTNUM                                          08290307
      GO TO 0311                                                        08300307
20300 IVFAIL = IVFAIL + 1                                               08310307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08320307
 0311 CONTINUE                                                          08330307
C                                                                       08340307
C     ****  FCVS PROGRAM 307  -  TEST 031  ****                         08350307
C                                                                       08360307
C     ARRAY ELEMENT NAME ARGUMENT                                       08370307
C                                                                       08380307
      IVTNUM =  31                                                      08390307
      IF (ICZERO) 30310, 0310, 30310                                    08400307
 0310 CONTINUE                                                          08410307
      RVCOMP = 10.0                                                     08420307
      RAON11(2) = 6.0                                                   08430307
      RVCOMP = COSH (RAON11(2))                                         08440307
      RVCORR = 201.72                                                   08450307
40310 IF (RVCOMP - 201.67) 20310, 10310, 40311                          08460307
40311 IF (RVCOMP - 201.77) 10310, 10310, 20310                          08470307
30310 IVDELE = IVDELE + 1                                               08480307
      WRITE (I02,80000) IVTNUM                                          08490307
      IF (ICZERO) 10310, 0321, 20310                                    08500307
10310 IVPASS = IVPASS + 1                                               08510307
      WRITE (I02,80002) IVTNUM                                          08520307
      GO TO 0321                                                        08530307
20310 IVFAIL = IVFAIL + 1                                               08540307
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08550307
 0321 CONTINUE                                                          08560307
C                                                                       08570307
C                                                                       08580307
C     WRITE OUT TEST SUMMARY                                            08590307
C                                                                       08600307
      WRITE (I02,90004)                                                 08610307
      WRITE (I02,90014)                                                 08620307
      WRITE (I02,90004)                                                 08630307
      WRITE (I02,90000)                                                 08640307
      WRITE (I02,90004)                                                 08650307
      WRITE (I02,90020) IVFAIL                                          08660307
      WRITE (I02,90022) IVPASS                                          08670307
      WRITE (I02,90024) IVDELE                                          08680307
      STOP                                                              08690307
90001 FORMAT (1H ,24X,5HFM307)                                          08700307
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM307)                          08710307
C                                                                       08720307
C     FORMATS FOR TEST DETAIL LINES                                     08730307
C                                                                       08740307
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   08750307
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08760307
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08770307
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08780307
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        08790307
C                                                                       08800307
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08810307
C                                                                       08820307
90002 FORMAT (1H1)                                                      08830307
90004 FORMAT (1H )                                                      08840307
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08850307
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   08860307
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         08870307
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  08880307
90014 FORMAT (1H ,5X,46H----------------------------------------------) 08890307
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08900307
C                                                                       08910307
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 08920307
C                                                                       08930307
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              08940307
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              08950307
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             08960307
      END                                                               08970307
