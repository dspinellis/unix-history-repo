      PROGRAM FM301                                                     00010301
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020301
C                                                                       00030301
C        FM301 TESTS THE USE OF THE TYPE-STATEMENT TO EXPLICITLY        00040301
C     DEFINE THE DATA TYPE FOR VARIABLES, ARRAYS, AND STATEMENT         00050301
C     FUNCTIONS.  ONLY INTEGER, REAL, LOGICAL AND CHARACTER DATA        00060301
C     TYPES ARE TESTED IN THIS ROUTINE.  INTEGER AND REAL VARIABLES     00070301
C     AND ARRAYS ARE TESTED IN A MANNER WHICH BOTH CONFIRMS AND         00080301
C     OVERRIDES THE IMPLICIT TYPING OF THE DATA ENTITIES.               00090301
C                                                                       00100301
C        FM301 DOES NOT ATTEMPT TO TEST ALL OF THE ELEMENTARY SYNTAX    00110301
C     FORMS OF THE TYPE-STATEMENT.  THESE FORMS ARE TESTED ADEQUATELY   00120301
C     WITHIN THE BOILER PLATE AND OTHER AUDIT PROGRAMS.                 00130301
C                                                                       00140301
C     REFERENCES.                                                       00150301
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00160301
C           X3.9-1978                                                   00170301
C                                                                       00180301
C        SECTION 4.1, DATA TYPES                                        00190301
C        SECTION 8.4, TYPE-STATEMENT                                    00200301
C        SECTION 8.5, IMPLICIT STATEMENT                                00210301
C        SECTION 15.4, STATEMENT FUNCTION                               00220301
C                                                                       00230301
C                                                                       00240301
C     ******************************************************************00250301
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00260301
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00270301
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00280301
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00290301
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00300301
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00310301
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00320301
C     THE RESULT OF EXECUTING THESE TESTS.                              00330301
C                                                                       00340301
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00350301
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00360301
C                                                                       00370301
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00380301
C                    DEPARTMENT OF THE NAVY                             00390301
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00400301
C                    WASHINGTON, D.C.   20376                           00410301
C                                                                       00420301
C     ******************************************************************00430301
C                                                                       00440301
C                                                                       00450301
      IMPLICIT LOGICAL (L)                                              00460301
      IMPLICIT CHARACTER*14 (C)                                         00470301
C                                                                       00480301
                                                                        00490301
C                                                                       00500301
C     *** IMPLICIT STATEMENT FOR TEST 006 ***                           00510301
C                                                                       00520301
      IMPLICIT LOGICAL (M)                                              00530301
C                                                                       00540301
C     *** IMPLICIT STATEMENT FOR TEST 017 ***                           00550301
C                                                                       00560301
      IMPLICIT INTEGER (G)                                              00570301
C                                                                       00580301
C     *** IMPLICIT STATEMENT FOR TEST 018 ***                           00590301
C                                                                       00600301
      IMPLICIT CHARACTER*2 (F)                                          00610301
C                                                                       00620301
C     *** SPECIFICATION STATEMENTS FOR TEST 001 ***                     00630301
C                                                                       00640301
      INTEGER AVTN01                                                    00650301
C                                                                       00660301
C     *** SPECIFICATION STATEMENTS FOR TEST 002 ***                     00670301
C                                                                       00680301
      REAL KVTN01                                                       00690301
C                                                                       00700301
C     *** SPECIFICATION STATEMENTS FOR TEST 003 ***                     00710301
C                                                                       00720301
      INTEGER KVTN02, AVTN02, KVTN03                                    00730301
C                                                                       00740301
C     *** SPECIFICATION STATEMENTS FOR TEST 004 ***                     00750301
C                                                                       00760301
      REAL AVTN03, AVTN04, KVTN04                                       00770301
C                                                                       00780301
C     *** SPECIFICATION STATEMENTS FOR TEST 005 ***                     00790301
C                                                                       00800301
      LOGICAL HVTN01                                                    00810301
C                                                                       00820301
C     *** SPECIFICATION STATEMENTS FOR TEST 006 ***                     00830301
C        (ALSO SEE THE IMPLICIT STATEMENTS FOR TEST 006)                00840301
C                                                                       00850301
      REAL MVTN01                                                       00860301
C                                                                       00870301
C     *** SPECIFICATION STATEMENTS FOR TEST 007 ***                     00880301
C                                                                       00890301
      INTEGER NVTN11(4)                                                 00900301
C                                                                       00910301
C     *** SPECIFICATION STATEMENTS FOR TEST 008 ***                     00920301
C                                                                       00930301
      REAL NVTN22(2,2)                                                  00940301
C                                                                       00950301
C     *** SPECIFICATION STATEMENTS FOR TESTS 009 AND 010 ***            00960301
C                                                                       00970301
      INTEGER NVTN33(3,3,3), AVTN15(5)                                  00980301
C                                                                       00990301
C     *** SPECIFICATION STATEMENTS FOR TEST 011 ***                     01000301
C                                                                       01010301
      DIMENSION NVTN14(5)                                               01020301
      INTEGER NVTN14                                                    01030301
C                                                                       01040301
C     *** SPECIFICATION STATEMENTS FOR TEST 012 ***                     01050301
C                                                                       01060301
      DIMENSION AVTN16(4)                                               01070301
      INTEGER AVTN16                                                    01080301
C                                                                       01090301
C     *** SPECIFICATION STATEMENTS FOR TESTS 013 AND 014 ***            01100301
C                                                                       01110301
      CHARACTER CVTN01*14, CATN12(4)*14                                 01120301
C                                                                       01130301
C     *** SPECIFICATION STATEMENTS FOR TEST 015 ***                     01140301
C                                                                       01150301
      DIMENSION CADN13(6)                                               01160301
      CHARACTER CADN13*14                                               01170301
C                                                                       01180301
C     *** SPECIFICATION STATEMENTS FOR TEST 016 ***                     01190301
C                                                                       01200301
      CHARACTER KVTN05                                                  01210301
C                                                                       01220301
C     *** SPECIFICATION STATEMENTS FOR TEST 017 ***                     01230301
C        (ALSO SEE THE IMPLICIT STATEMENT FOR TEST 017)                 01240301
C                                                                       01250301
      CHARACTER GVTN01*3                                                01260301
C                                                                       01270301
C     *** SPECIFICATION STATEMENTS FOR TEST 018 ***                     01280301
C        (ALSO SEE THE IMPLICIT STATEMENT FOR TEST 018)                 01290301
C                                                                       01300301
      CHARACTER FVTN01*3                                                01310301
C                                                                       01320301
C     *** SPECIFICATION STATEMENTS FOR TEST 019 ***                     01330301
C                                                                       01340301
      INTEGER IFTN01                                                    01350301
      IFTN01(IDON01) = IDON01 + 1                                       01360301
C                                                                       01370301
C                                                                       01380301
C                                                                       01390301
C     INITIALIZATION SECTION.                                           01400301
C                                                                       01410301
C     INITIALIZE CONSTANTS                                              01420301
C     ********************                                              01430301
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01440301
      I01 = 5                                                           01450301
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01460301
      I02 = 6                                                           01470301
C     SYSTEM ENVIRONMENT SECTION                                        01480301
C                                                                       01490301
      I01 = 5                                                           01500301
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01510301
C     (UNIT NUMBER FOR CARD READER).                                    01520301
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01530301
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01540301
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01550301
C                                                                       01560301
      I02 = 6                                                           01570301
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01580301
C     (UNIT NUMBER FOR PRINTER).                                        01590301
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01600301
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01610301
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01620301
C                                                                       01630301
      IVPASS = 0                                                        01640301
      IVFAIL = 0                                                        01650301
      IVDELE = 0                                                        01660301
      ICZERO = 0                                                        01670301
C                                                                       01680301
C     WRITE OUT PAGE HEADERS                                            01690301
C                                                                       01700301
      WRITE (I02,90002)                                                 01710301
      WRITE (I02,90006)                                                 01720301
      WRITE (I02,90008)                                                 01730301
      WRITE (I02,90004)                                                 01740301
      WRITE (I02,90010)                                                 01750301
      WRITE (I02,90004)                                                 01760301
      WRITE (I02,90016)                                                 01770301
      WRITE (I02,90001)                                                 01780301
      WRITE (I02,90004)                                                 01790301
      WRITE (I02,90012)                                                 01800301
      WRITE (I02,90014)                                                 01810301
      WRITE (I02,90004)                                                 01820301
C                                                                       01830301
C                                                                       01840301
C     ****  FCVS PROGRAM 301  -  TEST 001  ****                         01850301
C                                                                       01860301
C         TEST 001 DEFINES AN INTEGER VARIABLE OVERRIDING THE IMPLICIT  01870301
C     COMPILER DEFAULT TYPE SPECIFYING REAL.                            01880301
C                                                                       01890301
C                                                                       01900301
      IVTNUM =   1                                                      01910301
      IF (ICZERO) 30010, 0010, 30010                                    01920301
 0010 CONTINUE                                                          01930301
      IVCOMP = 0                                                        01940301
      AVTN01 = 100                                                      01950301
      IVCORR = 100                                                      01960301
      IVCOMP = AVTN01                                                   01970301
40010 IF (IVCOMP - 100) 20010, 10010, 20010                             01980301
30010 IVDELE = IVDELE + 1                                               01990301
      WRITE (I02,80000) IVTNUM                                          02000301
      IF (ICZERO) 10010, 0021, 20010                                    02010301
10010 IVPASS = IVPASS + 1                                               02020301
      WRITE (I02,80002) IVTNUM                                          02030301
      GO TO 0021                                                        02040301
20010 IVFAIL = IVFAIL + 1                                               02050301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02060301
 0021 CONTINUE                                                          02070301
C                                                                       02080301
C     ****  FCVS PROGRAM 301  -  TEST 002  ****                         02090301
C                                                                       02100301
C         TEST 002 DEFINES A REAL VARIABLE OVERRIDING THE IMPLICIT      02110301
C     COMPILER DEFAULT TYPE SPECIFYING INTEGER.                         02120301
C                                                                       02130301
C                                                                       02140301
      IVTNUM =   2                                                      02150301
      IF (ICZERO) 30020, 0020, 30020                                    02160301
 0020 CONTINUE                                                          02170301
      RVCOMP = 0.0                                                      02180301
      KVTN01 = 1.004                                                    02190301
      RVCORR = 1.004                                                    02200301
      RVCOMP = KVTN01                                                   02210301
40020 IF (RVCOMP - 1.0035) 20020, 10020, 40021                          02220301
40021 IF (RVCOMP - 1.0045) 10020, 10020, 20020                          02230301
30020 IVDELE = IVDELE + 1                                               02240301
      WRITE (I02,80000) IVTNUM                                          02250301
      IF (ICZERO) 10020, 0031, 20020                                    02260301
10020 IVPASS = IVPASS + 1                                               02270301
      WRITE (I02,80002) IVTNUM                                          02280301
      GO TO 0031                                                        02290301
20020 IVFAIL = IVFAIL + 1                                               02300301
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02310301
 0031 CONTINUE                                                          02320301
C                                                                       02330301
C     ****  FCVS PROGRAM 301  -  TEST 003  ****                         02340301
C                                                                       02350301
C         TEST 003 DEFINES A SERIES OF INTEGER VARIABLES IN ONE TYPE-   02360301
C     STATEMENT.  TWO VARIABLES CONFIRM THE IMPLICIT INTEGER TYPING.    02370301
C     THE OTHER VARIABLE OVERRIDES THE IMPLICIT TYPING.                 02380301
C                                                                       02390301
C                                                                       02400301
      IVTNUM =   3                                                      02410301
      IF (ICZERO) 30030, 0030, 30030                                    02420301
 0030 CONTINUE                                                          02430301
      IVCOMP = 0                                                        02440301
      KVTN02 = 20                                                       02450301
      KVTN03 = 30                                                       02460301
      AVTN02 = 200                                                      02470301
      IVCORR = 20                                                       02480301
      IVCOMP = KVTN02                                                   02490301
40030 IF (IVCOMP - 20) 20030, 40031, 20030                              02500301
40031 IVCORR = 30                                                       02510301
      IVCOMP = KVTN03                                                   02520301
40033 IF (IVCOMP - 30) 20030, 40034, 20030                              02530301
40034 IVCORR = 200                                                      02540301
      IVCOMP = AVTN02                                                   02550301
40035 IF (IVCOMP - 200) 20030, 10030, 20030                             02560301
30030 IVDELE = IVDELE + 1                                               02570301
      WRITE (I02,80000) IVTNUM                                          02580301
      IF (ICZERO) 10030, 0041, 20030                                    02590301
10030 IVPASS = IVPASS + 1                                               02600301
      WRITE (I02,80002) IVTNUM                                          02610301
      GO TO 0041                                                        02620301
20030 IVFAIL = IVFAIL + 1                                               02630301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02640301
 0041 CONTINUE                                                          02650301
C                                                                       02660301
C     ****  FCVS PROGRAM 301  -  TEST 004  ****                         02670301
C                                                                       02680301
C         TEST 004 DEFINES A SERIES OF REAL VARIABLES IN ONE TYPE-      02690301
C     STATEMENT.  TWO VARIABLES CONFIRM THE IMPLICIT REAL TYPING.  THE  02700301
C     THIRD VARIABLE OVERRIDES THE IMPLICIT TYPING.                     02710301
C                                                                       02720301
C                                                                       02730301
      IVTNUM =   4                                                      02740301
      IF (ICZERO) 30040, 0040, 30040                                    02750301
 0040 CONTINUE                                                          02760301
      RVCOMP = 0.0                                                      02770301
      AVTN03 = 3.0                                                      02780301
      AVTN04 = 4.                                                       02790301
      KVTN04 = .4                                                       02800301
      RVCORR = 3.0                                                      02810301
      RVCOMP = AVTN03                                                   02820301
40040 IF (RVCOMP - 2.9995) 20040, 40042, 40041                          02830301
40041 IF (RVCOMP - 3.0005) 40042, 40042, 20040                          02840301
40042 RVCORR = 4.                                                       02850301
      RVCOMP = AVTN04                                                   02860301
40043 IF (RVCOMP - 3.9995) 20040, 40045, 40044                          02870301
40044 IF (RVCOMP - 4.0005) 40045, 40045, 20040                          02880301
40045 RVCORR = .4                                                       02890301
      RVCOMP = KVTN04                                                   02900301
40046 IF (RVCOMP - .39995) 20040, 10040, 40047                          02910301
40047 IF (RVCOMP - .40005) 10040, 10040, 20040                          02920301
30040 IVDELE = IVDELE + 1                                               02930301
      WRITE (I02,80000) IVTNUM                                          02940301
      IF (ICZERO) 10040, 0051, 20040                                    02950301
10040 IVPASS = IVPASS + 1                                               02960301
      WRITE (I02,80002) IVTNUM                                          02970301
      GO TO 0051                                                        02980301
20040 IVFAIL = IVFAIL + 1                                               02990301
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03000301
 0051 CONTINUE                                                          03010301
C                                                                       03020301
C     ****  FCVS PROGRAM 301  -  TEST 005  ****                         03030301
C                                                                       03040301
C         TEST 005 DEFINES A LOGICAL VARIABLE.                          03050301
C                                                                       03060301
C                                                                       03070301
      IVTNUM =   5                                                      03080301
      IF (ICZERO) 30050, 0050, 30050                                    03090301
 0050 CONTINUE                                                          03100301
      HVTN01 = .TRUE.                                                   03110301
      IVCORR = 1                                                        03120301
      IVCOMP = 0                                                        03130301
      IF (HVTN01) IVCOMP = 1                                            03140301
40050 IF (IVCOMP - 1) 20050, 10050, 20050                               03150301
30050 IVDELE = IVDELE + 1                                               03160301
      WRITE (I02,80000) IVTNUM                                          03170301
      IF (ICZERO) 10050, 0061, 20050                                    03180301
10050 IVPASS = IVPASS + 1                                               03190301
      WRITE (I02,80002) IVTNUM                                          03200301
      GO TO 0061                                                        03210301
20050 IVFAIL = IVFAIL + 1                                               03220301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03230301
 0061 CONTINUE                                                          03240301
C                                                                       03250301
C     ****  FCVS PROGRAM 301  -  TEST 006  ****                         03260301
C                                                                       03270301
C         TEST 006 DEFINES A REAL VARIABLE WITH A TYPE-STATEMENT THAT   03280301
C     OVERRIDES THE IMPLICIT STATEMENT TYPING OF THE INTEGER LETTER 'M' 03290301
C     AS LOGICAL.                                                       03300301
C                                                                       03310301
C                                                                       03320301
      IVTNUM =   6                                                      03330301
      IF (ICZERO) 30060, 0060, 30060                                    03340301
 0060 CONTINUE                                                          03350301
      RVCOMP = 0.0                                                      03360301
      MVTN01 = 12.345                                                   03370301
      RVCORR = 12.345                                                   03380301
      RVCOMP = MVTN01                                                   03390301
40060 IF (RVCOMP - 12.340) 20060, 10060, 40061                          03400301
40061 IF (RVCOMP - 12.350) 10060, 10060, 20060                          03410301
30060 IVDELE = IVDELE + 1                                               03420301
      WRITE (I02,80000) IVTNUM                                          03430301
      IF (ICZERO) 10060, 0071, 20060                                    03440301
10060 IVPASS = IVPASS + 1                                               03450301
      WRITE (I02,80002) IVTNUM                                          03460301
      GO TO 0071                                                        03470301
20060 IVFAIL = IVFAIL + 1                                               03480301
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03490301
 0071 CONTINUE                                                          03500301
C                                                                       03510301
C     ****  FCVS PROGRAM 301  -  TEST 007  ****                         03520301
C                                                                       03530301
C         TEST 007 DEFINES A ONE DIMENSIONAL INTEGER ARRAY.             03540301
C                                                                       03550301
C                                                                       03560301
      IVTNUM =   7                                                      03570301
      IF (ICZERO) 30070, 0070, 30070                                    03580301
 0070 CONTINUE                                                          03590301
      IVCOMP = 0                                                        03600301
      NVTN11(3) = 3                                                     03610301
      IVCORR = 3                                                        03620301
      IVCOMP = NVTN11(3)                                                03630301
40070 IF (IVCOMP - 3) 20070, 10070, 20070                               03640301
30070 IVDELE = IVDELE + 1                                               03650301
      WRITE (I02,80000) IVTNUM                                          03660301
      IF (ICZERO) 10070, 0081, 20070                                    03670301
10070 IVPASS = IVPASS + 1                                               03680301
      WRITE (I02,80002) IVTNUM                                          03690301
      GO TO 0081                                                        03700301
20070 IVFAIL = IVFAIL + 1                                               03710301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03720301
 0081 CONTINUE                                                          03730301
C                                                                       03740301
C     ****  FCVS PROGRAM 301  -  TEST 008  ****                         03750301
C                                                                       03760301
C         TEST 008 DEFINES A TWO DIMENSIONAL REAL ARRAY THAT OVERRIDES  03770301
C     THE IMPLICIT TYPING OF INTEGER.                                   03780301
C                                                                       03790301
C                                                                       03800301
      IVTNUM =   8                                                      03810301
      IF (ICZERO) 30080, 0080, 30080                                    03820301
 0080 CONTINUE                                                          03830301
      RVCOMP = 0.0                                                      03840301
      NVTN22(1,2) = 2.12                                                03850301
      RVCORR = 2.12                                                     03860301
      RVCOMP = NVTN22(1,2)                                              03870301
40080 IF (RVCOMP - 2.1195) 20080, 10080, 40081                          03880301
40081 IF (RVCOMP - 2.1205) 10080, 10080, 20080                          03890301
30080 IVDELE = IVDELE + 1                                               03900301
      WRITE (I02,80000) IVTNUM                                          03910301
      IF (ICZERO) 10080, 0091, 20080                                    03920301
10080 IVPASS = IVPASS + 1                                               03930301
      WRITE (I02,80002) IVTNUM                                          03940301
      GO TO 0091                                                        03950301
20080 IVFAIL = IVFAIL + 1                                               03960301
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          03970301
 0091 CONTINUE                                                          03980301
C                                                                       03990301
C     ****  FCVS PROGRAM 301  -  TEST 009  ****                         04000301
C                                                                       04010301
C         TEST 009 DEFINES TWO INTEGER ARRAYS WITH ONE TYPE-STATEMENT.  04020301
C     ONE ARRAY IS THREE DIMENSIONAL WHILE THE OTHER ARRAY OVERRIDES    04030301
C     THE IMPLICIT TYPING OF REAL.  ONLY THE THREE DIMENSIONAL ARRAY    04040301
C     IS CHECKED IN THIS TEST.                                          04050301
C                                                                       04060301
C                                                                       04070301
      IVTNUM =   9                                                      04080301
      IF (ICZERO) 30090, 0090, 30090                                    04090301
 0090 CONTINUE                                                          04100301
      IVCOMP = 0                                                        04110301
      NVTN33(1,2,3) = 123                                               04120301
      IVCORR = 123                                                      04130301
      IVCOMP = NVTN33(1,2,3)                                            04140301
40090 IF (IVCOMP - 123) 20090, 10090, 20090                             04150301
30090 IVDELE = IVDELE + 1                                               04160301
      WRITE (I02,80000) IVTNUM                                          04170301
      IF (ICZERO) 10090, 0101, 20090                                    04180301
10090 IVPASS = IVPASS + 1                                               04190301
      WRITE (I02,80002) IVTNUM                                          04200301
      GO TO 0101                                                        04210301
20090 IVFAIL = IVFAIL + 1                                               04220301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04230301
 0101 CONTINUE                                                          04240301
C                                                                       04250301
C     ****  FCVS PROGRAM 301  -  TEST 010  ****                         04260301
C                                                                       04270301
C         TEST 010 CHECKS THE SECOND ARRAY DESCRIBED IN THE PREVIOUS    04280301
C     TEST.                                                             04290301
C                                                                       04300301
C                                                                       04310301
      IVTNUM =  10                                                      04320301
      IF (ICZERO) 30100, 0100, 30100                                    04330301
 0100 CONTINUE                                                          04340301
      IVCOMP = 0                                                        04350301
      AVTN15(2) = 5                                                     04360301
      IVCORR = 5                                                        04370301
      IVCOMP = AVTN15(2)                                                04380301
40100 IF (IVCOMP - 5) 20100, 10100, 20100                               04390301
30100 IVDELE = IVDELE + 1                                               04400301
      WRITE (I02,80000) IVTNUM                                          04410301
      IF (ICZERO) 10100, 0111, 20100                                    04420301
10100 IVPASS = IVPASS + 1                                               04430301
      WRITE (I02,80002) IVTNUM                                          04440301
      GO TO 0111                                                        04450301
20100 IVFAIL = IVFAIL + 1                                               04460301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04470301
 0111 CONTINUE                                                          04480301
C                                                                       04490301
C     ****  FCVS PROGRAM 301  -  TEST 011  ****                         04500301
C                                                                       04510301
C         TEST 011 USES THE TYPE-STATEMENT TO EXPLICITLY TYPE AN ARRAY  04520301
C     THAT WAS DEFINED WITH A DIMENSION STATEMENT.                      04530301
C                                                                       04540301
C                                                                       04550301
      IVTNUM =  11                                                      04560301
      IF (ICZERO) 30110, 0110, 30110                                    04570301
 0110 CONTINUE                                                          04580301
      IVCOMP = 0                                                        04590301
      NVTN14(5) = 5                                                     04600301
      IVCORR = 5                                                        04610301
      IVCOMP = NVTN14(5)                                                04620301
40110 IF (IVCOMP - 5) 20110, 10110, 20110                               04630301
30110 IVDELE = IVDELE + 1                                               04640301
      WRITE (I02,80000) IVTNUM                                          04650301
      IF (ICZERO) 10110, 0121, 20110                                    04660301
10110 IVPASS = IVPASS + 1                                               04670301
      WRITE (I02,80002) IVTNUM                                          04680301
      GO TO 0121                                                        04690301
20110 IVFAIL = IVFAIL + 1                                               04700301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04710301
 0121 CONTINUE                                                          04720301
C                                                                       04730301
C     ****  FCVS PROGRAM 301  -  TEST 012  ****                         04740301
C                                                                       04750301
C         TEST 012 USES THE TYPE-STATEMENT TO OVERRIDE THE TYPING OF    04760301
C     AN ARRAY THAT WAS DEFINED WITH A DIMENSION STATEMENT.             04770301
C                                                                       04780301
      IVTNUM =  12                                                      04790301
      IF (ICZERO) 30120, 0120, 30120                                    04800301
 0120 CONTINUE                                                          04810301
      IVCOMP = 0                                                        04820301
      AVTN16(3) = 163                                                   04830301
      IVCORR = 163                                                      04840301
      IVCOMP = AVTN16(3)                                                04850301
40120 IF (IVCOMP - 163) 20120, 10120, 20120                             04860301
30120 IVDELE = IVDELE + 1                                               04870301
      WRITE (I02,80000) IVTNUM                                          04880301
      IF (ICZERO) 10120, 0131, 20120                                    04890301
10120 IVPASS = IVPASS + 1                                               04900301
      WRITE (I02,80002) IVTNUM                                          04910301
      GO TO 0131                                                        04920301
20120 IVFAIL = IVFAIL + 1                                               04930301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04940301
 0131 CONTINUE                                                          04950301
C                                                                       04960301
C     ****  FCVS PROGRAM 301  -  TEST 013  ****                         04970301
C                                                                       04980301
C         TEST 013 USES ONE CHARACTER TYPE-STATEMENT TO SPECIFY BOTH A  04990301
C     VARIABLE AND AN ARRAY DECLARATOR.  ONLY THE VARIABLE IS CHECKED   05000301
C     IN THIS TEST.                                                     05010301
C                                                                       05020301
      IVTNUM =  13                                                      05030301
      IF (ICZERO) 30130, 0130, 30130                                    05040301
 0130 CONTINUE                                                          05050301
      CVTN01 = '12345678901234'                                         05060301
      CVCOMP = '              '                                         05070301
      CVCORR = '12345678901234'                                         05080301
      CVCOMP = CVTN01                                                   05090301
40130 IF (CVCOMP .EQ. '12345678901234') GO TO 10130                     05100301
40131 GO TO 20130                                                       05110301
30130 IVDELE = IVDELE + 1                                               05120301
      WRITE (I02,80000) IVTNUM                                          05130301
      IF (ICZERO) 10130, 0141, 20130                                    05140301
10130 IVPASS = IVPASS + 1                                               05150301
      WRITE (I02,80002) IVTNUM                                          05160301
      GO TO 0141                                                        05170301
20130 IVFAIL = IVFAIL + 1                                               05180301
      WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                          05190301
 0141 CONTINUE                                                          05200301
C                                                                       05210301
C     ****  FCVS PROGRAM 301  -  TEST 014  ****                         05220301
C                                                                       05230301
C         TEST 014 CHECKS THE ARRAY DECLARATOR FROM THE PREVIOUS TEST.  05240301
C                                                                       05250301
      IVTNUM =  14                                                      05260301
      IF (ICZERO) 30140, 0140, 30140                                    05270301
 0140 CONTINUE                                                          05280301
      CVCOMP = '              '                                         05290301
      CATN12(2) = 'ABCDEFGHIJKLMN'                                      05300301
      CVCORR = 'ABCDEFGHIJKLMN'                                         05310301
      CVCOMP = CATN12(2)                                                05320301
40140 IF (CVCOMP .EQ. 'ABCDEFGHIJKLMN') GO TO 10140                     05330301
40141 GO TO 20140                                                       05340301
30140 IVDELE = IVDELE + 1                                               05350301
      WRITE (I02,80000) IVTNUM                                          05360301
      IF (ICZERO) 10140, 0151, 20140                                    05370301
10140 IVPASS = IVPASS + 1                                               05380301
      WRITE (I02,80002) IVTNUM                                          05390301
      GO TO 0151                                                        05400301
20140 IVFAIL = IVFAIL + 1                                               05410301
      WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                          05420301
 0151 CONTINUE                                                          05430301
C                                                                       05440301
C     ****  FCVS PROGRAM 301  -  TEST 015  ****                         05450301
C                                                                       05460301
C         TEST 015 USES THE CHARACTER TYPE-STATEMENT TO SPECIFY AN      05470301
C     ARRAY-NAME.  THE ARRAY IS DECLARED IN A DIMENSION STATEMENT.      05480301
C                                                                       05490301
      IVTNUM =  15                                                      05500301
      IF (ICZERO) 30150, 0150, 30150                                    05510301
 0150 CONTINUE                                                          05520301
      CVCOMP = '              '                                         05530301
      CADN13(3) = '12345678901234'                                      05540301
      CVCORR = '12345678901234'                                         05550301
      CVCOMP = CADN13(3)                                                05560301
40150 IF (CVCOMP .EQ. '12345678901234') GO TO 10150                     05570301
40151 GO TO 20150                                                       05580301
30150 IVDELE = IVDELE + 1                                               05590301
      WRITE (I02,80000) IVTNUM                                          05600301
      IF (ICZERO) 10150, 0161, 20150                                    05610301
10150 IVPASS = IVPASS + 1                                               05620301
      WRITE (I02,80002) IVTNUM                                          05630301
      GO TO 0161                                                        05640301
20150 IVFAIL = IVFAIL + 1                                               05650301
      WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                          05660301
 0161 CONTINUE                                                          05670301
C                                                                       05680301
C     ****  FCVS PROGRAM 301  -  TEST 016  ****                         05690301
C                                                                       05700301
C         TEST 016 USES THE CHARACTER TYPE-STATEMENT TO OVERRIDE THE    05710301
C     IMPLICIT (DEFAULT) TYPING OF INTEGER.                             05720301
C                                                                       05730301
      IVTNUM =  16                                                      05740301
      IF (ICZERO) 30160, 0160, 30160                                    05750301
 0160 CONTINUE                                                          05760301
      CVCOMP = '   '                                                    05770301
      KVTN05 = 'A'                                                      05780301
      CVCORR = 'A'                                                      05790301
      CVCOMP = KVTN05                                                   05800301
40160 IF (CVCOMP .EQ. 'A') GO TO 10160                                  05810301
40161 GO TO 20160                                                       05820301
30160 IVDELE = IVDELE + 1                                               05830301
      WRITE (I02,80000) IVTNUM                                          05840301
      IF (ICZERO) 10160, 0171, 20160                                    05850301
10160 IVPASS = IVPASS + 1                                               05860301
      WRITE (I02,80002) IVTNUM                                          05870301
      GO TO 0171                                                        05880301
20160 IVFAIL = IVFAIL + 1                                               05890301
      WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                          05900301
 0171 CONTINUE                                                          05910301
C                                                                       05920301
C     ****  FCVS PROGRAM 301  -  TEST 017  ****                         05930301
C                                                                       05940301
C         TEST 017 USES THE CHARACTER TYPE-STATEMENT TO OVERRIDE THE    05950301
C     IMPLICIT TYPING OF THE LETTER 'G' AS INTEGER.                     05960301
C                                                                       05970301
      IVTNUM =  17                                                      05980301
      IF (ICZERO) 30170, 0170, 30170                                    05990301
 0170 CONTINUE                                                          06000301
      CVCOMP = '   '                                                    06010301
      GVTN01 = 'ABC'                                                    06020301
      CVCORR = 'ABC'                                                    06030301
      CVCOMP = GVTN01                                                   06040301
40170 IF (CVCOMP .EQ. 'ABC') GO TO 10170                                06050301
40171 GO TO 20170                                                       06060301
30170 IVDELE = IVDELE + 1                                               06070301
      WRITE (I02,80000) IVTNUM                                          06080301
      IF (ICZERO) 10170, 0181, 20170                                    06090301
10170 IVPASS = IVPASS + 1                                               06100301
      WRITE (I02,80002) IVTNUM                                          06110301
      GO TO 0181                                                        06120301
20170 IVFAIL = IVFAIL + 1                                               06130301
      WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                          06140301
 0181 CONTINUE                                                          06150301
C                                                                       06160301
C     ****  FCVS PROGRAM 301  -  TEST 018  ****                         06170301
C                                                                       06180301
C         TEST 018 USES THE CHARAACTER TYPE-STATEMENT TO OVERRIDE THE   06190301
C     LENGTH OF A CHARACTER FIELD DEFINED BY AN IMPLICIT STATEMENT.     06200301
C                                                                       06210301
      IVTNUM =  18                                                      06220301
      IF (ICZERO) 30180, 0180, 30180                                    06230301
 0180 CONTINUE                                                          06240301
      CVCOMP = '   '                                                    06250301
      FVTN01 = 'ABC'                                                    06260301
      CVCORR = 'ABC'                                                    06270301
      CVCOMP = FVTN01                                                   06280301
40180 IF (CVCOMP .EQ. 'ABC') GO TO 10180                                06290301
40181 GO TO 20180                                                       06300301
30180 IVDELE = IVDELE + 1                                               06310301
      WRITE (I02,80000) IVTNUM                                          06320301
      IF (ICZERO) 10180, 0191, 20180                                    06330301
10180 IVPASS = IVPASS + 1                                               06340301
      WRITE (I02,80002) IVTNUM                                          06350301
      GO TO 0191                                                        06360301
20180 IVFAIL = IVFAIL + 1                                               06370301
      WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                          06380301
 0191 CONTINUE                                                          06390301
C                                                                       06400301
C     ****  FCVS PROGRAM 301  -  TEST 019  ****                         06410301
C                                                                       06420301
C         TEST 019 USES THE TYPE-STATEMENT TO SPECIFY AN INTEGER        06430301
C     STATEMENT FUNCTION.                                               06440301
C                                                                       06450301
      IVTNUM =  19                                                      06460301
      IF (ICZERO) 30190, 0190, 30190                                    06470301
 0190 CONTINUE                                                          06480301
      IVCOMP = 0                                                        06490301
      IVON01 = 5                                                        06500301
      IVON02 = IFTN01(IVON01)                                           06510301
      IVCORR = 6                                                        06520301
      IVCOMP = IVON02                                                   06530301
40190 IF (IVCOMP - 6) 20190, 10190, 20190                               06540301
30190 IVDELE = IVDELE + 1                                               06550301
      WRITE (I02,80000) IVTNUM                                          06560301
      IF (ICZERO) 10190, 0201, 20190                                    06570301
10190 IVPASS = IVPASS + 1                                               06580301
      WRITE (I02,80002) IVTNUM                                          06590301
      GO TO 0201                                                        06600301
20190 IVFAIL = IVFAIL + 1                                               06610301
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06620301
 0201 CONTINUE                                                          06630301
C                                                                       06640301
C                                                                       06650301
C     WRITE OUT TEST SUMMARY                                            06660301
C                                                                       06670301
      WRITE (I02,90004)                                                 06680301
      WRITE (I02,90014)                                                 06690301
      WRITE (I02,90004)                                                 06700301
      WRITE (I02,90000)                                                 06710301
      WRITE (I02,90004)                                                 06720301
      WRITE (I02,90020) IVFAIL                                          06730301
      WRITE (I02,90022) IVPASS                                          06740301
      WRITE (I02,90024) IVDELE                                          06750301
      STOP                                                              06760301
90001 FORMAT (1H ,24X,5HFM301)                                          06770301
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM301)                          06780301
C                                                                       06790301
C     FORMATS FOR TEST DETAIL LINES                                     06800301
C                                                                       06810301
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   06820301
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      06830301
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         06840301
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    06850301
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        06860301
C                                                                       06870301
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06880301
C                                                                       06890301
90002 FORMAT (1H1)                                                      06900301
90004 FORMAT (1H )                                                      06910301
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06920301
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   06930301
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         06940301
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  06950301
90014 FORMAT (1H ,5X,46H----------------------------------------------) 06960301
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             06970301
C                                                                       06980301
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 06990301
C                                                                       07000301
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              07010301
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              07020301
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             07030301
      END                                                               07040301
