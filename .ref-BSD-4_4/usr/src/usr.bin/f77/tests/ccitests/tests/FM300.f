      PROGRAM FM300                                                     00010300
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020300
C                                                                       00030300
C        THIS ROUTINE TESTS THE USE OF THE EQUIVALENCE STATEMENT TO     00040300
C     EQUATE STORAGE UNITS OF VARIABLES, ARRAYS AND ARRAY ELEMENTS.     00050300
C     ONLY INTEGER, REAL, LOGICAL AND CHARACTER DATA TYPES ARE TESTED.  00060300
C     NO ATTEMPT IS MADE TO TEST DATA OF DIFFERENT TYPES THAT ARE       00070300
C     EQUATED WITH THE EQUIVALENCE STATEMENT.                           00080300
C                                                                       00090300
C     REFERENCES.                                                       00100300
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110300
C           X3.9-1978                                                   00120300
C                                                                       00130300
C        SECTION 8.1, DIMENSION STATEMENT                               00140300
C        SECTION 8.2, EQUIVALENCE STATEMENT                             00150300
C        SECTION 9, DATA STATEMENT                                      00160300
C                                                                       00170300
C                                                                       00180300
C     ******************************************************************00190300
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00200300
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00210300
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00220300
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00230300
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00240300
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00250300
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00260300
C     THE RESULT OF EXECUTING THESE TESTS.                              00270300
C                                                                       00280300
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00290300
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00300300
C                                                                       00310300
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00320300
C                    DEPARTMENT OF THE NAVY                             00330300
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00340300
C                    WASHINGTON, D.C.   20376                           00350300
C                                                                       00360300
C     ******************************************************************00370300
C                                                                       00380300
C                                                                       00390300
      IMPLICIT LOGICAL (L)                                              00400300
      IMPLICIT CHARACTER*14 (C)                                         00410300
C                                                                       00420300
                                                                        00430300
C     *** SPECIFICATION STATEMENTS FOR TEST 001 ***                     00440300
C                                                                       00450300
      EQUIVALENCE (IVOE01, IVOE02)                                      00460300
C                                                                       00470300
C     *** SPECIFICATION STATEMENTS FOR TEST 002 ***                     00480300
C                                                                       00490300
      EQUIVALENCE (RVOE01, RVOE02)                                      00500300
C                                                                       00510300
C     *** SPECIFICATION STATEMENTS FOR TEST 003 ***                     00520300
C                                                                       00530300
      EQUIVALENCE (LVOE01, LVOE02)                                      00540300
C                                                                       00550300
C     *** SPECIFICATION STATEMENTS FOR TEST 004 ***                     00560300
C                                                                       00570300
      CHARACTER CVTE01*3, CVTE02*3, CVCOMP*3                            00580300
      EQUIVALENCE (CVTE01, CVTE02)                                      00590300
C                                                                       00600300
C     *** SPECIFICATION STATEMENTS FOR TEST 005 ***                     00610300
C                                                                       00620300
      EQUIVALENCE (IVOE03, IVOE04, IVOE05)                              00630300
C                                                                       00640300
C     *** SPECIFICATION STATEMENTS FOR TEST 006 ***                     00650300
C                                                                       00660300
      EQUIVALENCE (IVOE06, IVOE07, RVOE03)                              00670300
C                                                                       00680300
C     *** SPECIFICATION STATEMENTS FOR TESTS 007 AND 008 ***            00690300
C                                                                       00700300
      EQUIVALENCE (IVOE08, IVOE09), (IVOE10, IVOE11)                    00710300
C                                                                       00720300
C     *** SPECIFICATION STATEMENTS FOR TEST 009 ***                     00730300
C                                                                       00740300
      EQUIVALENCE (IVOE12, IVOE13), (IVOE13, IVOE14)                    00750300
C                                                                       00760300
C     *** SPECIFICATION STATEMENTS FOR TEST 010 ***                     00770300
C                                                                       00780300
      EQUIVALENCE (IVOE15, IVOE16)                                      00790300
      EQUIVALENCE (IVOE16, IVOE17)                                      00800300
C                                                                       00810300
C     *** SPECIFICATION STATEMENTS FOR TESTS 011 AND 012 ***            00820300
C                                                                       00830300
      DIMENSION IADE11(2), IADE12(3)                                    00840300
      EQUIVALENCE (IADE11, IADE12)                                      00850300
C                                                                       00860300
C     *** SPECIFICATION STATEMENTS FOR TESTS 013 AND 014 ***            00870300
C                                                                       00880300
      DIMENSION RADE11(5), RADE12(5)                                    00890300
      EQUIVALENCE (RADE11(4), RADE12(2))                                00900300
C                                                                       00910300
C     *** SPECIFICATION STATEMENTS FOR TEST 015 ***                     00920300
C                                                                       00930300
      DIMENSION IADE13(4), IADE14(4)                                    00940300
      EQUIVALENCE (IADE13, IADE14(3))                                   00950300
C                                                                       00960300
C     *** SPECIFICATION STATEMENTS FOR TEST 016 ***                     00970300
C                                                                       00980300
      DIMENSION IADE15(3)                                               00990300
      EQUIVALENCE (IADE15(2), IVOE18)                                   01000300
C                                                                       01010300
C     *** SPECIFICATION STATEMENTS FOR TESTS 017 AND 018 ***            01020300
C                                                                       01030300
      DIMENSION IADE21(2,2), IADE16(4)                                  01040300
      EQUIVALENCE (IADE21, IADE16)                                      01050300
C                                                                       01060300
C     *** SPECIFICATION STATEMENTS FOR TEST 019 ***                     01070300
C                                                                       01080300
      EQUIVALENCE (IVOE19, IVOE20)                                      01090300
      DATA IVOE19/19/                                                   01100300
C                                                                       01110300
C                                                                       01120300
C                                                                       01130300
C     INITIALIZATION SECTION.                                           01140300
C                                                                       01150300
C     INITIALIZE CONSTANTS                                              01160300
C     ********************                                              01170300
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01180300
      I01 = 5                                                           01190300
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01200300
      I02 = 6                                                           01210300
C     SYSTEM ENVIRONMENT SECTION                                        01220300
C                                                                       01230300
      I01 = 5                                                           01240300
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01250300
C     (UNIT NUMBER FOR CARD READER).                                    01260300
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01270300
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01280300
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01290300
C                                                                       01300300
      I02 = 6                                                           01310300
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01320300
C     (UNIT NUMBER FOR PRINTER).                                        01330300
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01340300
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01350300
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01360300
C                                                                       01370300
      IVPASS = 0                                                        01380300
      IVFAIL = 0                                                        01390300
      IVDELE = 0                                                        01400300
      ICZERO = 0                                                        01410300
C                                                                       01420300
C     WRITE OUT PAGE HEADERS                                            01430300
C                                                                       01440300
      WRITE (I02,90002)                                                 01450300
      WRITE (I02,90006)                                                 01460300
      WRITE (I02,90008)                                                 01470300
      WRITE (I02,90004)                                                 01480300
      WRITE (I02,90010)                                                 01490300
      WRITE (I02,90004)                                                 01500300
      WRITE (I02,90016)                                                 01510300
      WRITE (I02,90001)                                                 01520300
      WRITE (I02,90004)                                                 01530300
      WRITE (I02,90012)                                                 01540300
      WRITE (I02,90014)                                                 01550300
      WRITE (I02,90004)                                                 01560300
C                                                                       01570300
C                                                                       01580300
C     ****  FCVS PROGRAM 300  -  TEST 001  ****                         01590300
C                                                                       01600300
C        THIS IS A TEST FOR EQUATING TWO INTEGER VARIABLES.             01610300
C                                                                       01620300
C                                                                       01630300
      IVTNUM =   1                                                      01640300
      IF (ICZERO) 30010, 0010, 30010                                    01650300
 0010 CONTINUE                                                          01660300
      IVCOMP = 0                                                        01670300
      IVOE01 = 5                                                        01680300
      IVOE02 = 7                                                        01690300
      IVCORR = 7                                                        01700300
      IVCOMP = IVOE01                                                   01710300
40010 IF (IVCOMP - 7) 20010,10010,20010                                 01720300
30010 IVDELE = IVDELE + 1                                               01730300
      WRITE (I02,80000) IVTNUM                                          01740300
      IF (ICZERO) 10010, 0021, 20010                                    01750300
10010 IVPASS = IVPASS + 1                                               01760300
      WRITE (I02,80002) IVTNUM                                          01770300
      GO TO 0021                                                        01780300
20010 IVFAIL = IVFAIL + 1                                               01790300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01800300
 0021 CONTINUE                                                          01810300
C                                                                       01820300
C     ****  FCVS PROGRAM 300  -  TEST 002  ****                         01830300
C                                                                       01840300
C        THIS IS A TEST FOR EQUATING TWO REAL VARIABLES.                01850300
C                                                                       01860300
C                                                                       01870300
      IVTNUM =   2                                                      01880300
      IF (ICZERO) 30020, 0020, 30020                                    01890300
 0020 CONTINUE                                                          01900300
      RVCOMP = 0.0                                                      01910300
      RVOE01 = 4.5                                                      01920300
      RVOE02 = 1.2                                                      01930300
      RVCORR = 1.2                                                      01940300
      RVCOMP = RVOE01                                                   01950300
40020 IF (RVCOMP - 1.1995) 20020,10020,40021                            01960300
40021 IF (RVCOMP - 1.2005) 10020,10020,20020                            01970300
30020 IVDELE = IVDELE + 1                                               01980300
      WRITE (I02,80000) IVTNUM                                          01990300
      IF (ICZERO) 10020, 0031, 20020                                    02000300
10020 IVPASS = IVPASS + 1                                               02010300
      WRITE (I02,80002) IVTNUM                                          02020300
      GO TO 0031                                                        02030300
20020 IVFAIL = IVFAIL + 1                                               02040300
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          02050300
 0031 CONTINUE                                                          02060300
C                                                                       02070300
C     ****  FCVS PROGRAM 300  -  TEST 003  ****                         02080300
C                                                                       02090300
C        THIS IS A TEST FOR EQUATING TWO LOGICAL VARIABLES.             02100300
C                                                                       02110300
C                                                                       02120300
      IVTNUM =   3                                                      02130300
      IF (ICZERO) 30030, 0030, 30030                                    02140300
 0030 CONTINUE                                                          02150300
      LVOE01 = .TRUE.                                                   02160300
      LVOE02 = .FALSE.                                                  02170300
      IVCORR = 0                                                        02180300
      IVCOMP = 0                                                        02190300
      IF (LVOE01) IVCOMP = 1                                            02200300
40030 IF (IVCOMP) 20030,10030,20030                                     02210300
30030 IVDELE = IVDELE + 1                                               02220300
      WRITE (I02,80000) IVTNUM                                          02230300
      IF (ICZERO) 10030, 0041, 20030                                    02240300
10030 IVPASS = IVPASS + 1                                               02250300
      WRITE (I02,80002) IVTNUM                                          02260300
      GO TO 0041                                                        02270300
20030 IVFAIL = IVFAIL + 1                                               02280300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02290300
 0041 CONTINUE                                                          02300300
C                                                                       02310300
C     ****  FCVS PROGRAM 300  -  TEST 004  ****                         02320300
C                                                                       02330300
C        THIS IS A TEST FOR EQUATING TWO CHARACTER VARIABLES.           02340300
C                                                                       02350300
C                                                                       02360300
      IVTNUM =   4                                                      02370300
      IF (ICZERO) 30040, 0040, 30040                                    02380300
 0040 CONTINUE                                                          02390300
      CVCOMP = '   '                                                    02400300
      CVTE01 = 'ABC'                                                    02410300
      CVTE02 = 'DEF'                                                    02420300
      CVCORR = 'DEF'                                                    02430300
      CVCOMP = CVTE01                                                   02440300
40040 IF (CVCOMP .EQ. 'DEF') GO TO 10040                                02450300
40041 GO TO 20040                                                       02460300
30040 IVDELE = IVDELE + 1                                               02470300
      WRITE (I02,80000) IVTNUM                                          02480300
      IF (ICZERO) 10040, 0051, 20040                                    02490300
10040 IVPASS = IVPASS + 1                                               02500300
      WRITE (I02,80002) IVTNUM                                          02510300
      GO TO 0051                                                        02520300
20040 IVFAIL = IVFAIL + 1                                               02530300
      WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                          02540300
 0051 CONTINUE                                                          02550300
C                                                                       02560300
C     ****  FCVS PROGRAM 300  -  TEST 005  ****                         02570300
C                                                                       02580300
C     THIS IS A TEST FOR EQUATING THREE INTEGER VARIABLES.              02590300
C                                                                       02600300
C                                                                       02610300
      IVTNUM =   5                                                      02620300
      IF (ICZERO) 30050, 0050, 30050                                    02630300
 0050 CONTINUE                                                          02640300
      IVCOMP = 0                                                        02650300
      IVOE03 = 3                                                        02660300
      IVOE04 = 4                                                        02670300
      IVOE05 = 5                                                        02680300
      IVCORR = 5                                                        02690300
      IVCOMP = IVOE03                                                   02700300
40050 IF (IVCOMP - 5) 20050,40051,20050                                 02710300
40051 IVCOMP = IVOE04                                                   02720300
40052 IF (IVCOMP - 5) 20050,10050,20050                                 02730300
30050 IVDELE = IVDELE + 1                                               02740300
      WRITE (I02,80000) IVTNUM                                          02750300
      IF (ICZERO) 10050, 0061, 20050                                    02760300
10050 IVPASS = IVPASS + 1                                               02770300
      WRITE (I02,80002) IVTNUM                                          02780300
      GO TO 0061                                                        02790300
20050 IVFAIL = IVFAIL + 1                                               02800300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02810300
 0061 CONTINUE                                                          02820300
C                                                                       02830300
C     ****  FCVS PROGRAM 300  -  TEST 006  ****                         02840300
C                                                                       02850300
C        THIS IS A TEST FOR EQUATING TWO INTEGER VARIABLES AND ONE      02860300
C     REAL VARIABLE WITHIN ONE EQUIVALENCE STATEMENT LIST OF NAMES.  THE02870300
C     VALUE OF THE REAL VARIABLE IS NOT TESTED.                         02880300
C                                                                       02890300
C                                                                       02900300
      IVTNUM =   6                                                      02910300
      IF (ICZERO) 30060, 0060, 30060                                    02920300
 0060 CONTINUE                                                          02930300
      IVCOMP = 0                                                        02940300
      RVOE03 = 3.445                                                    02950300
      IVOE06 = 6                                                        02960300
      IVOE07 = 7                                                        02970300
      IVCORR = 7                                                        02980300
      IVCOMP = IVOE06                                                   02990300
40060 IF (IVCOMP - 7) 20060,10060,20060                                 03000300
30060 IVDELE = IVDELE + 1                                               03010300
      WRITE (I02,80000) IVTNUM                                          03020300
      IF (ICZERO) 10060, 0071, 20060                                    03030300
10060 IVPASS = IVPASS + 1                                               03040300
      WRITE (I02,80002) IVTNUM                                          03050300
      GO TO 0071                                                        03060300
20060 IVFAIL = IVFAIL + 1                                               03070300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03080300
 0071 CONTINUE                                                          03090300
C                                                                       03100300
C     ****  FCVS PROGRAM 300  -  TEST 007  ****                         03110300
C                                                                       03120300
C        THIS IS A TEST FOR EQUATING INTEGER VARIABLES USING TWO LISTS  03130300
C     OF NAMES IN ONE EQUIVALENCE STATEMENT.  NAMES SPECIFIED IN THE    03140300
C     FIRST LIST ARE NOT EQUATED TO NAMES IN THE SECOND LIST.  THIS     03150300
C     TEST CHECKS THE EQUIVALINCE OF THE VARIABLES IN THE FIRST LIST.   03160300
C                                                                       03170300
C                                                                       03180300
      IVTNUM =   7                                                      03190300
      IF (ICZERO) 30070, 0070, 30070                                    03200300
 0070 CONTINUE                                                          03210300
      IVCOMP = 0                                                        03220300
      IVOE08 = 8                                                        03230300
      IVOE09 = 9                                                        03240300
      IVOE10 = 10                                                       03250300
      IVOE11 = 11                                                       03260300
      IVCORR = 9                                                        03270300
      IVCOMP = IVOE08                                                   03280300
40070 IF (IVCOMP - 9) 20070,10070,20070                                 03290300
30070 IVDELE = IVDELE + 1                                               03300300
      WRITE (I02,80000) IVTNUM                                          03310300
      IF (ICZERO) 10070, 0081, 20070                                    03320300
10070 IVPASS = IVPASS + 1                                               03330300
      WRITE (I02,80002) IVTNUM                                          03340300
      GO TO 0081                                                        03350300
20070 IVFAIL = IVFAIL + 1                                               03360300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03370300
 0081 CONTINUE                                                          03380300
C                                                                       03390300
C     ****  FCVS PROGRAM 300  -  TEST 008  ****                         03400300
C                                                                       03410300
C        THIS TEST CHECKS THE EQUIVALENCE OF THE VARIABLES IN THE       03420300
C     SECOND LIST.                                                      03430300
C                                                                       03440300
C                                                                       03450300
      IVTNUM =   8                                                      03460300
      IF (ICZERO) 30080, 0080, 30080                                    03470300
 0080 CONTINUE                                                          03480300
      IVCOMP = 0                                                        03490300
      IVCORR = 11                                                       03500300
      IVCOMP = IVOE10                                                   03510300
40080 IF (IVCOMP - 11) 20080,10080,20080                                03520300
30080 IVDELE = IVDELE + 1                                               03530300
      WRITE (I02,80000) IVTNUM                                          03540300
      IF (ICZERO) 10080, 0091, 20080                                    03550300
10080 IVPASS = IVPASS + 1                                               03560300
      WRITE (I02,80002) IVTNUM                                          03570300
      GO TO 0091                                                        03580300
20080 IVFAIL = IVFAIL + 1                                               03590300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03600300
 0091 CONTINUE                                                          03610300
C                                                                       03620300
C     ****  FCVS PROGRAM 300  -  TEST 009  ****                         03630300
C                                                                       03640300
C        THIS IS A TEST FOR EQUATING INTEGER VARIABLES IN ONE LIST      03650300
C     WITH INTEGER VARIABLES IN A SECOND LIST OF THE SAME EQUIVALENCE   03660300
C     STATEMENT.  ALL VARIABLES SHOULD BE EQUATED AND SHARE THE SAME    03670300
C     STORAGE UNIT.                                                     03680300
C                                                                       03690300
C                                                                       03700300
      IVTNUM =   9                                                      03710300
      IF (ICZERO) 30090, 0090, 30090                                    03720300
 0090 CONTINUE                                                          03730300
      IVCOMP = 0                                                        03740300
      IVOE12 = 12                                                       03750300
      IVOE13 = 13                                                       03760300
      IVOE14 = 14                                                       03770300
      IVCORR = 14                                                       03780300
      IVCOMP = IVOE13                                                   03790300
40090 IF (IVCOMP - 14) 20090,40091,20090                                03800300
40091 IVCOMP = IVOE12                                                   03810300
40092 IF (IVCOMP - 14) 20090,10090,20090                                03820300
30090 IVDELE = IVDELE + 1                                               03830300
      WRITE (I02,80000) IVTNUM                                          03840300
      IF (ICZERO) 10090, 0101, 20090                                    03850300
10090 IVPASS = IVPASS + 1                                               03860300
      WRITE (I02,80002) IVTNUM                                          03870300
      GO TO 0101                                                        03880300
20090 IVFAIL = IVFAIL + 1                                               03890300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03900300
 0101 CONTINUE                                                          03910300
C                                                                       03920300
C     ****  FCVS PROGRAM 300  -  TEST 010  ****                         03930300
C                                                                       03940300
C        THIS IS A TEST FOR EQUATING INTEGER VARIABLES SPECIFIED IN ONE 03950300
C     EQUIVALENCE STATEMENT WITH INTEGER VARIABLES SPECIFIED IN A       03960300
C     SECOND EQUIVALENCE STATEMENT.  ONE VARIABLE IS SPECIFIED IN BOTH  03970300
C     STATEMENTS, THEREFORE ALL VARIABLES SHOULD BE EQUATED AND SHARE   03980300
C     THE SAME STORAGE UNIT.                                            03990300
C                                                                       04000300
C                                                                       04010300
      IVTNUM =  10                                                      04020300
      IF (ICZERO) 30100, 0100, 30100                                    04030300
 0100 CONTINUE                                                          04040300
      IVCOMP = 0                                                        04050300
      IVOE15 = 15                                                       04060300
      IVOE16 = 16                                                       04070300
      IVOE17 = 17                                                       04080300
      IVCORR = 17                                                       04090300
      IVCOMP = IVOE16                                                   04100300
40100 IF (IVCOMP - 17) 20100,40101,20100                                04110300
40101 IVCOMP = IVOE15                                                   04120300
40102 IF (IVCOMP - 17) 20100,10100,20100                                04130300
30100 IVDELE = IVDELE + 1                                               04140300
      WRITE (I02,80000) IVTNUM                                          04150300
      IF (ICZERO) 10100, 0111, 20100                                    04160300
10100 IVPASS = IVPASS + 1                                               04170300
      WRITE (I02,80002) IVTNUM                                          04180300
      GO TO 0111                                                        04190300
20100 IVFAIL = IVFAIL + 1                                               04200300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04210300
 0111 CONTINUE                                                          04220300
C                                                                       04230300
C     ****  FCVS PROGRAM 300  -  TEST 011  ****                         04240300
C                                                                       04250300
C        THIS IS A TEST FOR EQUATING TWO INTEGER ARRAYS UNQUALIFIED     04260300
C     BY A SUBSCRIPT IN THE EQUIVALENCE STATEMENT.  ALL ARRAY ELEMENTS  04270300
C     SPECIFIED BY THE SAME SUBSCRIPT VALUE, BEGINNING WITH THE FIRST   04280300
C     ARRAY ELEMENT, SHOULD BE EQUATED AND SHARE THE SAME STORAGE UNIT. 04290300
C     THIS TEST CHECKS THE EQUIVALENCE OF THE FIRST ARRAY ELEMENTS.     04300300
C                                                                       04310300
C                                                                       04320300
      IVTNUM =  11                                                      04330300
      IF (ICZERO) 30110, 0110, 30110                                    04340300
 0110 CONTINUE                                                          04350300
      IVCOMP = 0                                                        04360300
      IADE11(1) = 111                                                   04370300
      IADE11(2) = 112                                                   04380300
      IADE12(1) = 121                                                   04390300
      IADE12(2) = 122                                                   04400300
      IADE12(3) = 123                                                   04410300
      IVCORR = 121                                                      04420300
      IVCOMP = IADE11(1)                                                04430300
40110 IF (IVCOMP - 121) 20110,10110,20110                               04440300
30110 IVDELE = IVDELE + 1                                               04450300
      WRITE (I02,80000) IVTNUM                                          04460300
      IF (ICZERO) 10110, 0121, 20110                                    04470300
10110 IVPASS = IVPASS + 1                                               04480300
      WRITE (I02,80002) IVTNUM                                          04490300
      GO TO 0121                                                        04500300
20110 IVFAIL = IVFAIL + 1                                               04510300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04520300
 0121 CONTINUE                                                          04530300
C                                                                       04540300
C     ****  FCVS PROGRAM 300  -  TEST 012  ****                         04550300
C                                                                       04560300
C        THIS TEST CHECKS THE EQUIVALENCE OF THE SECOND ARRAY ELEMENTS. 04570300
C                                                                       04580300
C                                                                       04590300
      IVTNUM =  12                                                      04600300
      IF (ICZERO) 30120, 0120, 30120                                    04610300
 0120 CONTINUE                                                          04620300
      IVCOMP = 0                                                        04630300
      IVCORR = 122                                                      04640300
      IVCOMP = IADE11(2)                                                04650300
40120 IF (IVCOMP - 122) 20120,10120,20120                               04660300
30120 IVDELE = IVDELE + 1                                               04670300
      WRITE (I02,80000) IVTNUM                                          04680300
      IF (ICZERO) 10120, 0131, 20120                                    04690300
10120 IVPASS = IVPASS + 1                                               04700300
      WRITE (I02,80002) IVTNUM                                          04710300
      GO TO 0131                                                        04720300
20120 IVFAIL = IVFAIL + 1                                               04730300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04740300
 0131 CONTINUE                                                          04750300
C                                                                       04760300
C     ****  FCVS PROGRAM 300  -  TEST 013  ****                         04770300
C                                                                       04780300
C        THIS IS A TEST FOR EQUATING TWO REAL ARRAY ELEMENTS.  THIS     04790300
C     TEST CHECKS THE EQUIVALENCE OF THE TWO ARRAY ELEMENTS SPECIFIED   04800300
C     IN THE EQUIVALENCE STATEMENT.                                     04810300
C                                                                       04820300
C                                                                       04830300
      IVTNUM =  13                                                      04840300
      IF (ICZERO) 30130, 0130, 30130                                    04850300
 0130 CONTINUE                                                          04860300
      RVCOMP = 0.0                                                      04870300
      RADE11(4) = 11.4                                                  04880300
      RADE12(2) = 1.22                                                  04890300
      RVCORR = 1.22                                                     04900300
      RVCOMP = RADE11(4)                                                04910300
40130 IF (RVCOMP - 1.2195) 20130,10130,40131                            04920300
40131 IF (RVCOMP - 1.2205) 10130,10130,20130                            04930300
30130 IVDELE = IVDELE + 1                                               04940300
      WRITE (I02,80000) IVTNUM                                          04950300
      IF (ICZERO) 10130, 0141, 20130                                    04960300
10130 IVPASS = IVPASS + 1                                               04970300
      WRITE (I02,80002) IVTNUM                                          04980300
      GO TO 0141                                                        04990300
20130 IVFAIL = IVFAIL + 1                                               05000300
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05010300
 0141 CONTINUE                                                          05020300
C                                                                       05030300
C     ****  FCVS PROGRAM 300  -  TEST 014  ****                         05040300
C                                                                       05050300
C        THIS TEST CHECKS THE EQUIVALENCE OF THE ARRAY ELEMENTS         05060300
C     WITH A SUBSCRIPT VALUE ONE LESS THAN THOSE TESTED IN THE          05070300
C     PREVIOUS TEST.  THESE ELEMENTS SHOULD BE EQUATED AND SHARE THE    05080300
C     SAME STORAGE UNIT DUE TO THE WAY ARRAY ELEMENTS OCCUPY            05090300
C     CONSECUTIVE STORAGE UNITS.                                        05100300
C                                                                       05110300
C                                                                       05120300
      IVTNUM =  14                                                      05130300
      IF (ICZERO) 30140, 0140, 30140                                    05140300
 0140 CONTINUE                                                          05150300
      RVCOMP = 0.0                                                      05160300
      RADE11(3) = .113                                                  05170300
      RADE12(1) = 122.                                                  05180300
      RVCORR = 122.                                                     05190300
      RVCOMP = RADE11(3)                                                05200300
40140 IF (RVCOMP - 121.95) 20140,10140,40141                            05210300
40141 IF (RVCOMP - 122.05) 10140,10140,20140                            05220300
30140 IVDELE = IVDELE + 1                                               05230300
      WRITE (I02,80000) IVTNUM                                          05240300
      IF (ICZERO) 10140, 0151, 20140                                    05250300
10140 IVPASS = IVPASS + 1                                               05260300
      WRITE (I02,80002) IVTNUM                                          05270300
      GO TO 0151                                                        05280300
20140 IVFAIL = IVFAIL + 1                                               05290300
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05300300
 0151 CONTINUE                                                          05310300
C                                                                       05320300
C     ****  FCVS PROGRAM 300  -  TEST 015  ****                         05330300
C                                                                       05340300
C        THIS IS A TEST TO EQUATE AN ARRAY NAME TO AN ARRAY ELEMENT     05350300
C     NAME.                                                             05360300
C                                                                       05370300
C                                                                       05380300
      IVTNUM =  15                                                      05390300
      IF (ICZERO) 30150, 0150, 30150                                    05400300
 0150 CONTINUE                                                          05410300
      IVCOMP = 0                                                        05420300
      IADE13(1) = 131                                                   05430300
      IADE14(3) = 143                                                   05440300
      IVCORR = 143                                                      05450300
      IVCOMP = IADE13(1)                                                05460300
40150 IF (IVCOMP - 143) 20150,10150,20150                               05470300
30150 IVDELE = IVDELE + 1                                               05480300
      WRITE (I02,80000) IVTNUM                                          05490300
      IF (ICZERO) 10150, 0161, 20150                                    05500300
10150 IVPASS = IVPASS + 1                                               05510300
      WRITE (I02,80002) IVTNUM                                          05520300
      GO TO 0161                                                        05530300
20150 IVFAIL = IVFAIL + 1                                               05540300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05550300
 0161 CONTINUE                                                          05560300
C                                                                       05570300
C     ****  FCVS PROGRAM 300  -  TEST 016  ****                         05580300
C                                                                       05590300
C        THIS IS A TEST TO EQUATE AN ARRAY ELEMENT TO AN INTEGER        05600300
C     VARIABLE.                                                         05610300
C                                                                       05620300
C                                                                       05630300
      IVTNUM =  16                                                      05640300
      IF (ICZERO) 30160, 0160, 30160                                    05650300
 0160 CONTINUE                                                          05660300
      IVCOMP = 0                                                        05670300
      IADE15(2) = 152                                                   05680300
      IVOE18 = 18                                                       05690300
      IVCORR = 18                                                       05700300
      IVCOMP = IADE15(2)                                                05710300
40160 IF (IVCOMP - 18) 20160,10160,20160                                05720300
30160 IVDELE = IVDELE + 1                                               05730300
      WRITE (I02,80000) IVTNUM                                          05740300
      IF (ICZERO) 10160, 0171, 20160                                    05750300
10160 IVPASS = IVPASS + 1                                               05760300
      WRITE (I02,80002) IVTNUM                                          05770300
      GO TO 0171                                                        05780300
20160 IVFAIL = IVFAIL + 1                                               05790300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05800300
 0171 CONTINUE                                                          05810300
C                                                                       05820300
C     ****  FCVS PROGRAM 300  -  TEST 017  ****                         05830300
C                                                                       05840300
C        THIS IS A TEST TO EQUATE A ONE DIMENSIONAL ARRAY TO A TWO      05850300
C     DIMENSIONAL ARRAY.  THIS TEST CHECKS THE SECOND ARRAY ELEMENTS.   05860300
C                                                                       05870300
C                                                                       05880300
      IVTNUM =  17                                                      05890300
      IF (ICZERO) 30170, 0170, 30170                                    05900300
 0170 CONTINUE                                                          05910300
      IVCOMP = 0                                                        05920300
      IADE21(2,1) = 212                                                 05930300
      IADE16(2) = 162                                                   05940300
      IVCORR = 162                                                      05950300
      IVCOMP = IADE21(2,1)                                              05960300
40170 IF (IVCOMP - 162) 20170,10170,20170                               05970300
30170 IVDELE = IVDELE + 1                                               05980300
      WRITE (I02,80000) IVTNUM                                          05990300
      IF (ICZERO) 10170, 0181, 20170                                    06000300
10170 IVPASS = IVPASS + 1                                               06010300
      WRITE (I02,80002) IVTNUM                                          06020300
      GO TO 0181                                                        06030300
20170 IVFAIL = IVFAIL + 1                                               06040300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06050300
 0181 CONTINUE                                                          06060300
C                                                                       06070300
C     ****  FCVS PROGRAM 300  -  TEST 018  ****                         06080300
C                                                                       06090300
C        THIS TEST CHECKS THE THIRD ARRAY ELEMENTS FROM THE PREVIOUS    06100300
C     TEST.                                                             06110300
C                                                                       06120300
C                                                                       06130300
      IVTNUM =  18                                                      06140300
      IF (ICZERO) 30180, 0180, 30180                                    06150300
 0180 CONTINUE                                                          06160300
      IVCOMP = 0                                                        06170300
      IADE21(1,2) = 2112                                                06180300
      IADE16(3) = 163                                                   06190300
      IVCORR = 163                                                      06200300
      IVCOMP = IADE21(1,2)                                              06210300
40180 IF (IVCOMP - 163) 20180,10180,20180                               06220300
30180 IVDELE = IVDELE + 1                                               06230300
      WRITE (I02,80000) IVTNUM                                          06240300
      IF (ICZERO) 10180, 0191, 20180                                    06250300
10180 IVPASS = IVPASS + 1                                               06260300
      WRITE (I02,80002) IVTNUM                                          06270300
      GO TO 0191                                                        06280300
20180 IVFAIL = IVFAIL + 1                                               06290300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06300300
 0191 CONTINUE                                                          06310300
C                                                                       06320300
C     ****  FCVS PROGRAM 300  -  TEST 019  ****                         06330300
C                                                                       06340300
C        THIS IS A TEST TO EQUATE TWO INTEGER VARIABLES ONE OF WHICH    06350300
C     IS INITIALIZED IN A DATA STATEMENT.                               06360300
C                                                                       06370300
C                                                                       06380300
      IVTNUM =  19                                                      06390300
      IF (ICZERO) 30190, 0190, 30190                                    06400300
 0190 CONTINUE                                                          06410300
      IVCOMP = 0                                                        06420300
      IVCORR = 19                                                       06430300
      IVCOMP = IVOE20                                                   06440300
40190 IF (IVCOMP - 19) 20190,10190,20190                                06450300
30190 IVDELE = IVDELE + 1                                               06460300
      WRITE (I02,80000) IVTNUM                                          06470300
      IF (ICZERO) 10190, 0201, 20190                                    06480300
10190 IVPASS = IVPASS + 1                                               06490300
      WRITE (I02,80002) IVTNUM                                          06500300
      GO TO 0201                                                        06510300
20190 IVFAIL = IVFAIL + 1                                               06520300
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06530300
 0201 CONTINUE                                                          06540300
C                                                                       06550300
C                                                                       06560300
C     WRITE OUT TEST SUMMARY                                            06570300
C                                                                       06580300
      WRITE (I02,90004)                                                 06590300
      WRITE (I02,90014)                                                 06600300
      WRITE (I02,90004)                                                 06610300
      WRITE (I02,90000)                                                 06620300
      WRITE (I02,90004)                                                 06630300
      WRITE (I02,90020) IVFAIL                                          06640300
      WRITE (I02,90022) IVPASS                                          06650300
      WRITE (I02,90024) IVDELE                                          06660300
      STOP                                                              06670300
90001 FORMAT (1H ,24X,5HFM300)                                          06680300
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM300)                          06690300
C                                                                       06700300
C     FORMATS FOR TEST DETAIL LINES                                     06710300
C                                                                       06720300
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   06730300
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      06740300
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         06750300
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    06760300
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        06770300
C                                                                       06780300
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06790300
C                                                                       06800300
90002 FORMAT (1H1)                                                      06810300
90004 FORMAT (1H )                                                      06820300
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06830300
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   06840300
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         06850300
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  06860300
90014 FORMAT (1H ,5X,46H----------------------------------------------) 06870300
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             06880300
C                                                                       06890300
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 06900300
C                                                                       06910300
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              06920300
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              06930300
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             06940300
      END                                                               06950300
