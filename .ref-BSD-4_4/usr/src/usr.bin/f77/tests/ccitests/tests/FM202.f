      PROGRAM FM202                                                     00010202
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020202
C                                                                       00030202
C         THIS ROUTINE IS THE FIRST ROUTINE TO TEST CHARACTER DATA      00040202
C     TYPES.  CHARACTER TYPE-STATEMENTS SPECIFY CHARACTER VARIABLES OF  00050202
C     LENGTH ONE AND LENGTH TWO.  THE TESTS IN THIS ROUTINE DETERMINE   00060202
C     THAT THE FOLLOWING LANGUAGE FEATURES FUNCTION CORRECTLY.          00070202
C                                                                       00080202
C         (1) CHARACTER ASSIGNMENT STATEMENTS OF THE FORM               00090202
C                                                                       00100202
C             CHARACTER VARIABLE = CHARACTER CONSTANT                   00110202
C             CHARACTER VARIABLE = CHARACTER VARIABLE                   00120202
C                                                                       00130202
C         WHERE THE VARIABLES AND CONSTANTS ARE THE SAME LENGTH.        00140202
C                                                                       00150202
C         (2)  THE REPRESENTATION OF AN APOSTROPHE IN A CHARACTER       00160202
C         CONSTANT IS TWO CONSECUTIVE APOSTROPHES WITH NO INTERVENING   00170202
C         BLANKS.                                                       00180202
C                                                                       00190202
C         (3)  CHARACTER RELATIONAL EXPRESSION OF THE FORM              00200202
C                                                                       00210202
C              CHARACTER VARIABLE  RELOP  CHARACTER CONSTANT            00220202
C              CHARACTER CONSTANT  RELOP  CHARACTER VARIABLE            00230202
C              CHARACTER VARIABLE  RELOP  CHARACTER VARIABLE            00240202
C                                                                       00250202
C         WHERE THE CHARACTER ENTITIES ARE THE SAME LENGTH.             00260202
C                                                                       00270202
C         (4)  CHARACTER RELATIONAL EXPRESSIONS OF THE FORM             00280202
C                                                                       00290202
C              CHARACTER VARIABLE .EQ. CHARACTER CONSTANT               00300202
C                                                                       00310202
C         ARE USED IN THIS ROUTINE TO VERIFY THE CHARACTER ASSIGNMENT   00320202
C         STATEMENTS.                                                   00330202
C                                                                       00340202
C     REFERENCES                                                        00350202
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00360202
C              X3.9-1978                                                00370202
C                                                                       00380202
C         SECTION 4.8,   CHARACTER TYPE                                 00390202
C         SECTION 4.8.1, CHARACTER CONSTANT                             00400202
C         SECTION 6.2,   CHARACTER EXPRESSIONS                          00410202
C         SECTION 6.3.4, CHARACTER RELATIONAL EXPRESSION                00420202
C         SECTION 6.3.5, INTERPRETATION OF CHARACTER RELATIONAL         00430202
C                          EXPRESSIONS                                  00440202
C         SECTION 8.4.2, CHARACTER TYPE-STATEMENT                       00450202
C         SECTION 10.4,  CHARACTER ASSIGNMENT STATEMENT                 00460202
C                                                                       00470202
C                                                                       00480202
C                                                                       00490202
C     ******************************************************************00500202
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00510202
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00520202
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00530202
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00540202
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00550202
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00560202
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00570202
C     THE RESULT OF EXECUTING THESE TESTS.                              00580202
C                                                                       00590202
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00600202
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00610202
C                                                                       00620202
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00630202
C                    DEPARTMENT OF THE NAVY                             00640202
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00650202
C                    WASHINGTON, D.C.   20376                           00660202
C                                                                       00670202
C     ******************************************************************00680202
C                                                                       00690202
C                                                                       00700202
      IMPLICIT LOGICAL (L)                                              00710202
      IMPLICIT CHARACTER*14 (C)                                         00720202
C                                                                       00730202
      CHARACTER *1  CVTN01, CVTN02                                      00740202
      CHARACTER *2  CVTN03, CVTN04                                      00750202
C                                                                       00760202
C                                                                       00770202
C                                                                       00780202
C     INITIALIZATION SECTION.                                           00790202
C                                                                       00800202
C     INITIALIZE CONSTANTS                                              00810202
C     ********************                                              00820202
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00830202
      I01 = 5                                                           00840202
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00850202
      I02 = 6                                                           00860202
C     SYSTEM ENVIRONMENT SECTION                                        00870202
C                                                                       00880202
      I01 = 5                                                           00890202
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00900202
C     (UNIT NUMBER FOR CARD READER).                                    00910202
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00920202
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00930202
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00940202
C                                                                       00950202
      I02 = 6                                                           00960202
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00970202
C     (UNIT NUMBER FOR PRINTER).                                        00980202
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00990202
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01000202
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01010202
C                                                                       01020202
      IVPASS = 0                                                        01030202
      IVFAIL = 0                                                        01040202
      IVDELE = 0                                                        01050202
      ICZERO = 0                                                        01060202
C                                                                       01070202
C     WRITE OUT PAGE HEADERS                                            01080202
C                                                                       01090202
      WRITE (I02,90002)                                                 01100202
      WRITE (I02,90006)                                                 01110202
      WRITE (I02,90008)                                                 01120202
      WRITE (I02,90004)                                                 01130202
      WRITE (I02,90010)                                                 01140202
      WRITE (I02,90004)                                                 01150202
      WRITE (I02,90016)                                                 01160202
      WRITE (I02,90001)                                                 01170202
      WRITE (I02,90004)                                                 01180202
      WRITE (I02,90012)                                                 01190202
      WRITE (I02,90014)                                                 01200202
      WRITE (I02,90004)                                                 01210202
C                                                                       01220202
C                                                                       01230202
C         TEST 1 THROUGH TEST 6 VERIFY THAT THE CHARACTER ASSIGNMENT    01240202
C     STATEMENT                                                         01250202
C                                                                       01260202
C        CHARACTER VARIABLE (LEN 1) = CHARACTER CONSTANT (LEN 1)        01270202
C                                                                       01280202
C     IS CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                  01290202
C                                                                       01300202
C        CHARACTER VARIABLE (LEN 1) RELOP CHARACTER CONSTANT (LEN 1)    01310202
C                                                                       01320202
C     IS USED TO VERIFY THE ASSIGNMENT STATEMENT.  BOTH OF THE ABOVE    01330202
C     STATEMENTS MUST MEET THE LANGUAGE SPECIFICATIONS FOR THESE TESTS  01340202
C     TO PASS.                                                          01350202
C                                                                       01360202
C                                                                       01370202
C     ****  FCVS PROGRAM 202  -  TEST 001  ****                         01380202
C                                                                       01390202
C                                                                       01400202
      IVTNUM =   1                                                      01410202
      IF (ICZERO) 30010, 0010, 30010                                    01420202
 0010 CONTINUE                                                          01430202
      IVCOMP = 0                                                        01440202
      CVTN01 = ' '                                                      01450202
      IVCORR = 1                                                        01460202
      IF (CVTN01 .EQ. ' ') IVCOMP = 1                                   01470202
40010 IF (IVCOMP - 1) 20010,10010,20010                                 01480202
30010 IVDELE = IVDELE + 1                                               01490202
      WRITE (I02,80000) IVTNUM                                          01500202
      IF (ICZERO) 10010, 0021, 20010                                    01510202
10010 IVPASS = IVPASS + 1                                               01520202
      WRITE (I02,80002) IVTNUM                                          01530202
      GO TO 0021                                                        01540202
20010 IVFAIL = IVFAIL + 1                                               01550202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01560202
 0021 CONTINUE                                                          01570202
C                                                                       01580202
C     ****  FCVS PROGRAM 202  -  TEST 002  ****                         01590202
C                                                                       01600202
C                                                                       01610202
      IVTNUM =   2                                                      01620202
      IF (ICZERO) 30020, 0020, 30020                                    01630202
 0020 CONTINUE                                                          01640202
      IVCOMP = 0                                                        01650202
      CVTN01 = 'M'                                                      01660202
      IVCORR = 1                                                        01670202
      IF (CVTN01 .EQ. 'M') IVCOMP = 1                                   01680202
40020 IF (IVCOMP - 1) 20020,10020,20020                                 01690202
30020 IVDELE = IVDELE + 1                                               01700202
      WRITE (I02,80000) IVTNUM                                          01710202
      IF (ICZERO) 10020, 0031, 20020                                    01720202
10020 IVPASS = IVPASS + 1                                               01730202
      WRITE (I02,80002) IVTNUM                                          01740202
      GO TO 0031                                                        01750202
20020 IVFAIL = IVFAIL + 1                                               01760202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01770202
 0031 CONTINUE                                                          01780202
C                                                                       01790202
C     ****  FCVS PROGRAM 202  -  TEST 003  ****                         01800202
C                                                                       01810202
C                                                                       01820202
      IVTNUM =   3                                                      01830202
      IF (ICZERO) 30030, 0030, 30030                                    01840202
 0030 CONTINUE                                                          01850202
      IVCOMP = 0                                                        01860202
      IVCORR = 1                                                        01870202
      CVTN01 = '4'                                                      01880202
      IF (CVTN01 .EQ. '4') IVCOMP = 1                                   01890202
40030 IF (IVCOMP - 1) 20030,10030,20030                                 01900202
30030 IVDELE = IVDELE + 1                                               01910202
      WRITE (I02,80000) IVTNUM                                          01920202
      IF (ICZERO) 10030, 0041, 20030                                    01930202
10030 IVPASS = IVPASS + 1                                               01940202
      WRITE (I02,80002) IVTNUM                                          01950202
      GO TO 0041                                                        01960202
20030 IVFAIL = IVFAIL + 1                                               01970202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01980202
 0041 CONTINUE                                                          01990202
C                                                                       02000202
C     ****  FCVS PROGRAM 202  -  TEST 004  ****                         02010202
C                                                                       02020202
C                                                                       02030202
      IVTNUM =   4                                                      02040202
      IF (ICZERO) 30040, 0040, 30040                                    02050202
 0040 CONTINUE                                                          02060202
      IVCOMP = 0                                                        02070202
      IVCORR = 1                                                        02080202
      CVTN01 = '='                                                      02090202
      IF (CVTN01 .EQ. '=') IVCOMP = 1                                   02100202
40040 IF (IVCOMP - 1) 20040,10040,20040                                 02110202
30040 IVDELE = IVDELE + 1                                               02120202
      WRITE (I02,80000) IVTNUM                                          02130202
      IF (ICZERO) 10040, 0051, 20040                                    02140202
10040 IVPASS = IVPASS + 1                                               02150202
      WRITE (I02,80002) IVTNUM                                          02160202
      GO TO 0051                                                        02170202
20040 IVFAIL = IVFAIL + 1                                               02180202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02190202
 0051 CONTINUE                                                          02200202
C                                                                       02210202
C     ****  FCVS PROGRAM 202  -  TEST 005  ****                         02220202
C                                                                       02230202
C                                                                       02240202
      IVTNUM =   5                                                      02250202
      IF (ICZERO) 30050, 0050, 30050                                    02260202
 0050 CONTINUE                                                          02270202
      IVCOMP = 0                                                        02280202
      IVCORR = 1                                                        02290202
      CVTN01 = '/'                                                      02300202
      IF (CVTN01 .EQ. '/') IVCOMP = 1                                   02310202
40050 IF (IVCOMP - 1) 20050,10050,20050                                 02320202
30050 IVDELE = IVDELE + 1                                               02330202
      WRITE (I02,80000) IVTNUM                                          02340202
      IF (ICZERO) 10050, 0061, 20050                                    02350202
10050 IVPASS = IVPASS + 1                                               02360202
      WRITE (I02,80002) IVTNUM                                          02370202
      GO TO 0061                                                        02380202
20050 IVFAIL = IVFAIL + 1                                               02390202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02400202
 0061 CONTINUE                                                          02410202
C                                                                       02420202
C     ****  FCVS PROGRAM 202  -  TEST 006  ****                         02430202
C                                                                       02440202
C         AN APOSTROPHE IN A CHARACTER CONSTANT IS REPRESENTED BY TWO   02450202
C     CONSECUTIVE APOSTROPHES WITH NO INTERVENING BLANKS.               02460202
C                                                                       02470202
      IVTNUM =   6                                                      02480202
      IF (ICZERO) 30060, 0060, 30060                                    02490202
 0060 CONTINUE                                                          02500202
      IVCOMP = 0                                                        02510202
      IVCORR = 1                                                        02520202
      CVTN01 = ''''                                                     02530202
      IF (CVTN01 .EQ. '''') IVCOMP = 1                                  02540202
40060 IF (IVCOMP - 1) 20060,10060,20060                                 02550202
30060 IVDELE = IVDELE + 1                                               02560202
      WRITE (I02,80000) IVTNUM                                          02570202
      IF (ICZERO) 10060, 0071, 20060                                    02580202
10060 IVPASS = IVPASS + 1                                               02590202
      WRITE (I02,80002) IVTNUM                                          02600202
      GO TO 0071                                                        02610202
20060 IVFAIL = IVFAIL + 1                                               02620202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02630202
 0071 CONTINUE                                                          02640202
C                                                                       02650202
C         TEST 7 THROUGH TEST 12 VERIFY THAT THE CHARACTER ASSIGNMENT   02660202
C     STATEMENTS                                                        02670202
C                                                                       02680202
C         CHARACTER VARIABLE (LEN 1) = CHARACTER CONSTANT (LEN 1)       02690202
C         CHARACTER VARIABLE (LEN 1) = CHARACTER VARIABLE (LEN 1)       02700202
C                                                                       02710202
C     ARE CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                 02720202
C                                                                       02730202
C         CHARACTER VARIABLE (LEN 1) .EQ. CHARACTER CONSTANT (LEN 1)    02740202
C                                                                       02750202
C     IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENTS.        02760202
C                                                                       02770202
C                                                                       02780202
C     ****  FCVS PROGRAM 202  -  TEST 007  ****                         02790202
C                                                                       02800202
C                                                                       02810202
      IVTNUM =   7                                                      02820202
      IF (ICZERO) 30070, 0070, 30070                                    02830202
 0070 CONTINUE                                                          02840202
      IVCOMP = 0                                                        02850202
      IVCORR = 1                                                        02860202
      CVTN01 = ' '                                                      02870202
      CVTN02 = CVTN01                                                   02880202
      IF (CVTN02 .EQ. ' ') IVCOMP = 1                                   02890202
40070 IF (IVCOMP - 1) 20070, 10070, 20070                               02900202
30070 IVDELE = IVDELE + 1                                               02910202
      WRITE (I02,80000) IVTNUM                                          02920202
      IF (ICZERO) 10070, 0081, 20070                                    02930202
10070 IVPASS = IVPASS + 1                                               02940202
      WRITE (I02,80002) IVTNUM                                          02950202
      GO TO 0081                                                        02960202
20070 IVFAIL = IVFAIL + 1                                               02970202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02980202
 0081 CONTINUE                                                          02990202
C                                                                       03000202
C     ****  FCVS PROGRAM 202  -  TEST 008  ****                         03010202
C                                                                       03020202
C                                                                       03030202
      IVTNUM =   8                                                      03040202
      IF (ICZERO) 30080, 0080, 30080                                    03050202
 0080 CONTINUE                                                          03060202
      IVCOMP = 0                                                        03070202
      IVCORR = 1                                                        03080202
      CVTN01 = 'M'                                                      03090202
      CVTN02 = CVTN01                                                   03100202
      IF (CVTN02 .EQ. 'M') IVCOMP = 1                                   03110202
40080 IF (IVCOMP - 1) 20080,10080,20080                                 03120202
30080 IVDELE = IVDELE + 1                                               03130202
      WRITE (I02,80000) IVTNUM                                          03140202
      IF (ICZERO) 10080, 0091, 20080                                    03150202
10080 IVPASS = IVPASS + 1                                               03160202
      WRITE (I02,80002) IVTNUM                                          03170202
      GO TO 0091                                                        03180202
20080 IVFAIL = IVFAIL + 1                                               03190202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03200202
 0091 CONTINUE                                                          03210202
C                                                                       03220202
C     ****  FCVS PROGRAM 202  -  TEST 009  ****                         03230202
C                                                                       03240202
C                                                                       03250202
      IVTNUM =   9                                                      03260202
      IF (ICZERO) 30090, 0090, 30090                                    03270202
 0090 CONTINUE                                                          03280202
      IVCOMP = 0                                                        03290202
      IVCORR = 1                                                        03300202
      CVTN01 = '4'                                                      03310202
      CVTN02 = CVTN01                                                   03320202
      IF (CVTN02 .EQ. '4') IVCOMP = 1                                   03330202
40090 IF (IVCOMP - 1) 20090,10090,20090                                 03340202
30090 IVDELE = IVDELE + 1                                               03350202
      WRITE (I02,80000) IVTNUM                                          03360202
      IF (ICZERO) 10090, 0101, 20090                                    03370202
10090 IVPASS = IVPASS + 1                                               03380202
      WRITE (I02,80002) IVTNUM                                          03390202
      GO TO 0101                                                        03400202
20090 IVFAIL = IVFAIL + 1                                               03410202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03420202
 0101 CONTINUE                                                          03430202
C                                                                       03440202
C     ****  FCVS PROGRAM 202  -  TEST 010  ****                         03450202
C                                                                       03460202
C                                                                       03470202
      IVTNUM =  10                                                      03480202
      IF (ICZERO) 30100, 0100, 30100                                    03490202
 0100 CONTINUE                                                          03500202
      IVCOMP = 0                                                        03510202
      IVCORR = 1                                                        03520202
      CVTN01 = '='                                                      03530202
      CVTN02 = CVTN01                                                   03540202
      IF (CVTN02 .EQ. '=') IVCOMP = 1                                   03550202
40100 IF (IVCOMP - 1) 20100,10100,20100                                 03560202
30100 IVDELE = IVDELE + 1                                               03570202
      WRITE (I02,80000) IVTNUM                                          03580202
      IF (ICZERO) 10100, 0111, 20100                                    03590202
10100 IVPASS = IVPASS + 1                                               03600202
      WRITE (I02,80002) IVTNUM                                          03610202
      GO TO 0111                                                        03620202
20100 IVFAIL = IVFAIL + 1                                               03630202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03640202
 0111 CONTINUE                                                          03650202
C                                                                       03660202
C     ****  FCVS PROGRAM 202  -  TEST 011  ****                         03670202
C                                                                       03680202
C                                                                       03690202
      IVTNUM =  11                                                      03700202
      IF (ICZERO) 30110, 0110, 30110                                    03710202
 0110 CONTINUE                                                          03720202
      IVCOMP =0                                                         03730202
      IVCORR = 1                                                        03740202
      CVTN01 = '/'                                                      03750202
      CVTN02 = CVTN01                                                   03760202
      IF (CVTN02 .EQ. '/') IVCOMP = 1                                   03770202
40110 IF (IVCOMP - 1) 20110,10110,20110                                 03780202
30110 IVDELE = IVDELE + 1                                               03790202
      WRITE (I02,80000) IVTNUM                                          03800202
      IF (ICZERO) 10110, 0121, 20110                                    03810202
10110 IVPASS = IVPASS + 1                                               03820202
      WRITE (I02,80002) IVTNUM                                          03830202
      GO TO 0121                                                        03840202
20110 IVFAIL = IVFAIL + 1                                               03850202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03860202
 0121 CONTINUE                                                          03870202
C                                                                       03880202
C     ****  FCVS PROGRAM 202  -  TEST 012  ****                         03890202
C                                                                       03900202
C         AN APOSTROPHE IN A CHARACTER CONSTANT IS REPRESENTED BY TWO   03910202
C     CONSECUTIVE APOSTROPHES WITH NO INTERVENING BLANKS.               03920202
C                                                                       03930202
      IVTNUM =  12                                                      03940202
      IF (ICZERO) 30120, 0120, 30120                                    03950202
 0120 CONTINUE                                                          03960202
      IVCOMP = 0                                                        03970202
      IVCORR = 1                                                        03980202
      CVTN01 = ''''                                                     03990202
      CVTN02 = CVTN01                                                   04000202
      IF (CVTN02 .EQ. '''') IVCOMP = 1                                  04010202
40120 IF (IVCOMP - 1) 20120,10120,20120                                 04020202
30120 IVDELE = IVDELE + 1                                               04030202
      WRITE (I02,80000) IVTNUM                                          04040202
      IF (ICZERO) 10120, 0131, 20120                                    04050202
10120 IVPASS = IVPASS + 1                                               04060202
      WRITE (I02,80002) IVTNUM                                          04070202
      GO TO 0131                                                        04080202
20120 IVFAIL = IVFAIL + 1                                               04090202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04100202
 0131 CONTINUE                                                          04110202
C                                                                       04120202
C         TEST 13 THROUGH TEST 18 VERIFY THE RESULTS OF THE CHARACTER   04130202
C     RELATIONAL EXPRESSION USING EACH OF THE SIX RELATIONAL OPERATORS  04140202
C     IN THE STATEMENT FORM                                             04150202
C                                                                       04160202
C         CHARACTER VARIABLE (LEN 1) RELOP CHARACTER CONSTANT (LEN 1).  04170202
C                                                                       04180202
C     THE VARIABLE AND CONSTANT CONTAIN THE CHARACTER DATUM C.          04190202
C                                                                       04200202
      CVTN01 = 'C'                                                      04210202
C                                                                       04220202
C     ****  FCVS PROGRAM 202  -  TEST 013  ****                         04230202
C                                                                       04240202
C         RELATIONAL OPERATOR .EQ.                                      04250202
C                                                                       04260202
      IVTNUM =  13                                                      04270202
      IF (ICZERO) 30130, 0130, 30130                                    04280202
 0130 CONTINUE                                                          04290202
      IVCOMP = 0                                                        04300202
      IVCORR = 1                                                        04310202
      IF (CVTN01 .EQ. 'C') IVCOMP = 1                                   04320202
40130 IF (IVCOMP - 1) 20130,10130,20130                                 04330202
30130 IVDELE = IVDELE + 1                                               04340202
      WRITE (I02,80000) IVTNUM                                          04350202
      IF (ICZERO) 10130, 0141, 20130                                    04360202
10130 IVPASS = IVPASS + 1                                               04370202
      WRITE (I02,80002) IVTNUM                                          04380202
      GO TO 0141                                                        04390202
20130 IVFAIL = IVFAIL + 1                                               04400202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04410202
 0141 CONTINUE                                                          04420202
C                                                                       04430202
C     ****  FCVS PROGRAM 202  -  TEST 014  ****                         04440202
C                                                                       04450202
C         RELATIONAL OPERATOR .NE.                                      04460202
C                                                                       04470202
      IVTNUM =  14                                                      04480202
      IF (ICZERO) 30140, 0140, 30140                                    04490202
 0140 CONTINUE                                                          04500202
      IVCOMP = 0                                                        04510202
      IVCORR = 0                                                        04520202
      IF (CVTN01 .NE. 'C') IVCOMP = 1                                   04530202
40140 IF (IVCOMP) 20140,10140,20140                                     04540202
30140 IVDELE = IVDELE + 1                                               04550202
      WRITE (I02,80000) IVTNUM                                          04560202
      IF (ICZERO) 10140, 0151, 20140                                    04570202
10140 IVPASS = IVPASS + 1                                               04580202
      WRITE (I02,80002) IVTNUM                                          04590202
      GO TO 0151                                                        04600202
20140 IVFAIL = IVFAIL + 1                                               04610202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04620202
 0151 CONTINUE                                                          04630202
C                                                                       04640202
C     ****  FCVS PROGRAM 202  -  TEST 015  ****                         04650202
C                                                                       04660202
C         RELATIONAL OPERATOR .LE.                                      04670202
C                                                                       04680202
      IVTNUM =  15                                                      04690202
      IF (ICZERO) 30150, 0150, 30150                                    04700202
 0150 CONTINUE                                                          04710202
      IVCOMP = 0                                                        04720202
      IVCORR = 1                                                        04730202
      IF (CVTN01 .LE. 'C') IVCOMP = 1                                   04740202
      IF (IVCOMP - 1) 20150,10150,20150                                 04750202
30150 IVDELE = IVDELE + 1                                               04760202
      WRITE (I02,80000) IVTNUM                                          04770202
      IF (ICZERO) 10150, 0161, 20150                                    04780202
10150 IVPASS = IVPASS + 1                                               04790202
      WRITE (I02,80002) IVTNUM                                          04800202
      GO TO 0161                                                        04810202
20150 IVFAIL = IVFAIL + 1                                               04820202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04830202
 0161 CONTINUE                                                          04840202
C                                                                       04850202
C     ****  FCVS PROGRAM 202  -  TEST 016  ****                         04860202
C                                                                       04870202
C         RELATIONAL OPERATOR .LT.                                      04880202
C                                                                       04890202
      IVTNUM =  16                                                      04900202
      IF (ICZERO) 30160, 0160, 30160                                    04910202
 0160 CONTINUE                                                          04920202
      IVCOMP=0                                                          04930202
      IVCORR=0                                                          04940202
      IF (CVTN01 .LT. 'C') IVCOMP = 1                                   04950202
      IF (IVCOMP) 20160,10160,20160                                     04960202
30160 IVDELE = IVDELE + 1                                               04970202
      WRITE (I02,80000) IVTNUM                                          04980202
      IF (ICZERO) 10160, 0171, 20160                                    04990202
10160 IVPASS = IVPASS + 1                                               05000202
      WRITE (I02,80002) IVTNUM                                          05010202
      GO TO 0171                                                        05020202
20160 IVFAIL = IVFAIL + 1                                               05030202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05040202
 0171 CONTINUE                                                          05050202
C                                                                       05060202
C     ****  FCVS PROGRAM 202  -  TEST 017  ****                         05070202
C                                                                       05080202
C         RELATIONAL OPERATOR .GE.                                      05090202
C                                                                       05100202
      IVTNUM =  17                                                      05110202
      IF (ICZERO) 30170, 0170, 30170                                    05120202
 0170 CONTINUE                                                          05130202
      IVCOMP = 0                                                        05140202
      IVCORR = 1                                                        05150202
      IF (CVTN01 .GE. 'C') IVCOMP = 1                                   05160202
40170 IF (IVCOMP - 1) 20170,10170,20170                                 05170202
30170 IVDELE = IVDELE + 1                                               05180202
      WRITE (I02,80000) IVTNUM                                          05190202
      IF (ICZERO) 10170, 0181, 20170                                    05200202
10170 IVPASS = IVPASS + 1                                               05210202
      WRITE (I02,80002) IVTNUM                                          05220202
      GO TO 0181                                                        05230202
20170 IVFAIL = IVFAIL + 1                                               05240202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05250202
 0181 CONTINUE                                                          05260202
C                                                                       05270202
C     ****  FCVS PROGRAM 202  -  TEST 018  ****                         05280202
C                                                                       05290202
C         RELATIONAL OPERATOR .GT.                                      05300202
C                                                                       05310202
      IVTNUM =  18                                                      05320202
      IF (ICZERO) 30180, 0180, 30180                                    05330202
 0180 CONTINUE                                                          05340202
      IVCOMP = 0                                                        05350202
      IVCORR = 0                                                        05360202
      IF (CVTN01 .GT. 'C') IVCOMP = 1                                   05370202
40180 IF (IVCOMP) 20180,10180,20180                                     05380202
30180 IVDELE = IVDELE + 1                                               05390202
      WRITE (I02,80000) IVTNUM                                          05400202
      IF (ICZERO) 10180, 0191, 20180                                    05410202
10180 IVPASS = IVPASS + 1                                               05420202
      WRITE (I02,80002) IVTNUM                                          05430202
      GO TO 0191                                                        05440202
20180 IVFAIL = IVFAIL + 1                                               05450202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05460202
 0191 CONTINUE                                                          05470202
C                                                                       05480202
C         TEST 19 THROUGH TEST 21 VERIFY THAT THE CHARACTER ASSIGNMENT  05490202
C     STATEMENT                                                         05500202
C                                                                       05510202
C         CHARACTER VARIABLE (LEN 2) = CHARACTER CONSTANT (LEN 2)       05520202
C                                                                       05530202
C     OPERATES CORRECTLY.  THE CHARACTER RELATIONAL EXPRESSION          05540202
C                                                                       05550202
C         CHARACTER VARIABLE (LEN 2) .EQ. CHARACTER CONSTANT (LEN 2)    05560202
C                                                                       05570202
C     IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENT.         05580202
C                                                                       05590202
C                                                                       05600202
C     ****  FCVS PROGRAM 202  -  TEST 019  ****                         05610202
C                                                                       05620202
C                                                                       05630202
      IVTNUM =  19                                                      05640202
      IF (ICZERO) 30190, 0190, 30190                                    05650202
 0190 CONTINUE                                                          05660202
      IVCOMP =0                                                         05670202
      IVCORR =1                                                         05680202
      CVTN03 = 'AZ'                                                     05690202
      IF (CVTN03 .EQ. 'AZ') IVCOMP = 1                                  05700202
40190 IF (IVCOMP - 1) 20190,10190,20190                                 05710202
30190 IVDELE = IVDELE + 1                                               05720202
      WRITE (I02,80000) IVTNUM                                          05730202
      IF (ICZERO) 10190, 0201, 20190                                    05740202
10190 IVPASS = IVPASS + 1                                               05750202
      WRITE (I02,80002) IVTNUM                                          05760202
      GO TO 0201                                                        05770202
20190 IVFAIL = IVFAIL + 1                                               05780202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05790202
 0201 CONTINUE                                                          05800202
C                                                                       05810202
C     ****  FCVS PROGRAM 202  -  TEST 020  ****                         05820202
C                                                                       05830202
C                                                                       05840202
      IVTNUM =  20                                                      05850202
      IF (ICZERO) 30200, 0200, 30200                                    05860202
 0200 CONTINUE                                                          05870202
      IVCOMP = 0                                                        05880202
      IVCORR = 1                                                        05890202
      CVTN03 = 'B'''                                                    05900202
      IF (CVTN03 .EQ. 'B''') IVCOMP = 1                                 05910202
40200 IF (IVCOMP - 1) 20200,10200,20200                                 05920202
30200 IVDELE = IVDELE + 1                                               05930202
      WRITE (I02,80000) IVTNUM                                          05940202
      IF (ICZERO) 10200, 0211, 20200                                    05950202
10200 IVPASS = IVPASS + 1                                               05960202
      WRITE (I02,80002) IVTNUM                                          05970202
      GO TO 0211                                                        05980202
20200 IVFAIL = IVFAIL + 1                                               05990202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06000202
 0211 CONTINUE                                                          06010202
C                                                                       06020202
C     ****  FCVS PROGRAM 202  -  TEST 021  ****                         06030202
C                                                                       06040202
C                                                                       06050202
      IVTNUM =  21                                                      06060202
      IF (ICZERO) 30210, 0210, 30210                                    06070202
 0210 CONTINUE                                                          06080202
      IVCOMP = 0                                                        06090202
      IVCORR = 1                                                        06100202
      CVTN03 = '//'                                                     06110202
      IF (CVTN03 .EQ. '//') IVCOMP = 1                                  06120202
40210 IF (IVCOMP - 1) 20210,10210,20210                                 06130202
30210 IVDELE = IVDELE + 1                                               06140202
      WRITE (I02,80000) IVTNUM                                          06150202
      IF (ICZERO) 10210, 0221, 20210                                    06160202
10210 IVPASS = IVPASS + 1                                               06170202
      WRITE (I02,80002) IVTNUM                                          06180202
      GO TO 0221                                                        06190202
20210 IVFAIL = IVFAIL + 1                                               06200202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06210202
 0221 CONTINUE                                                          06220202
C                                                                       06230202
C         TEST 22 THROUGH TEST 24 VERIFY THAT THE CHARACTER ASSIGNMENT  06240202
C     STATEMENTS                                                        06250202
C                                                                       06260202
C         CHARACTER VARIABLE (LEN 2) = CHARACTER CONSTANT (LEN 2)       06270202
C         CHARACTER VARIABLE (LEN 2) = CHARACTER VARIABLE (LEN 2)       06280202
C                                                                       06290202
C     OPERATE CORRECTLY.                                                06300202
C                                                                       06310202
C                                                                       06320202
C     ****  FCVS PROGRAM 202  -  TEST 022  ****                         06330202
C                                                                       06340202
C                                                                       06350202
      IVTNUM =  22                                                      06360202
      IF (ICZERO) 30220, 0220, 30220                                    06370202
 0220 CONTINUE                                                          06380202
      IVCOMP = 0                                                        06390202
      IVCORR = 1                                                        06400202
      CVTN03 = 'AZ'                                                     06410202
      CVTN04 = CVTN03                                                   06420202
      IF (CVTN04 .EQ. 'AZ') IVCOMP=1                                    06430202
40220 IF (IVCOMP - 1) 20220,10220,20220                                 06440202
30220 IVDELE = IVDELE + 1                                               06450202
      WRITE (I02,80000) IVTNUM                                          06460202
      IF (ICZERO) 10220, 0231, 20220                                    06470202
10220 IVPASS = IVPASS + 1                                               06480202
      WRITE (I02,80002) IVTNUM                                          06490202
      GO TO 0231                                                        06500202
20220 IVFAIL = IVFAIL + 1                                               06510202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06520202
 0231 CONTINUE                                                          06530202
C                                                                       06540202
C     ****  FCVS PROGRAM 202  -  TEST 023  ****                         06550202
C                                                                       06560202
C                                                                       06570202
      IVTNUM =  23                                                      06580202
      IF (ICZERO) 30230, 0230, 30230                                    06590202
 0230 CONTINUE                                                          06600202
      IVCOMP = 0                                                        06610202
      IVCORR = 1                                                        06620202
      CVTN03 = 'B'''                                                    06630202
      CVTN04 = CVTN03                                                   06640202
      IF (CVTN04 .EQ. 'B''') IVCOMP = 1                                 06650202
40230 IF (IVCOMP - 1) 20230,10230,20230                                 06660202
30230 IVDELE = IVDELE + 1                                               06670202
      WRITE (I02,80000) IVTNUM                                          06680202
      IF (ICZERO) 10230, 0241, 20230                                    06690202
10230 IVPASS = IVPASS + 1                                               06700202
      WRITE (I02,80002) IVTNUM                                          06710202
      GO TO 0241                                                        06720202
20230 IVFAIL = IVFAIL + 1                                               06730202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06740202
 0241 CONTINUE                                                          06750202
C                                                                       06760202
C     ****  FCVS PROGRAM 202  -  TEST 024  ****                         06770202
C                                                                       06780202
C                                                                       06790202
      IVTNUM =  24                                                      06800202
      IF (ICZERO) 30240, 0240, 30240                                    06810202
 0240 CONTINUE                                                          06820202
      IVCOMP = 0                                                        06830202
      IVCORR = 1                                                        06840202
      CVTN03 = '//'                                                     06850202
      CVTN04 = CVTN03                                                   06860202
      IF (CVTN04 .EQ. '//') IVCOMP = 1                                  06870202
40240 IF (IVCOMP - 1) 20240,10240,20240                                 06880202
30240 IVDELE = IVDELE + 1                                               06890202
      WRITE (I02,80000) IVTNUM                                          06900202
      IF (ICZERO) 10240, 0251, 20240                                    06910202
10240 IVPASS = IVPASS + 1                                               06920202
      WRITE (I02,80002) IVTNUM                                          06930202
      GO TO 0251                                                        06940202
20240 IVFAIL = IVFAIL + 1                                               06950202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06960202
 0251 CONTINUE                                                          06970202
C                                                                       06980202
C         TEST 25 THROUGH TEST 30 VERIFY THE RESULTS OF THE CHARACTER   06990202
C     RELATIONAL EXPRESSION USING EACH OF THE SIX RELATIONAL OPERATORS  07000202
C     IN THE EXPRESSION FORM                                            07010202
C                                                                       07020202
C         CHARACTER VARIABLE (LEN 2) RELOP CHARACTER VARIABLE (LEN 2)   07030202
C                                                                       07040202
C     THE VARIABLES CONTAIN THE CHARACTER DATUM CC.                     07050202
C                                                                       07060202
      CVTN03 = 'CC'                                                     07070202
      CVTN04 = 'CC'                                                     07080202
C                                                                       07090202
C     ****  FCVS PROGRAM 202  -  TEST 025  ****                         07100202
C                                                                       07110202
C         RELATIONAL OPERATOR .EQ.                                      07120202
C                                                                       07130202
      IVTNUM =  25                                                      07140202
      IF (ICZERO) 30250, 0250, 30250                                    07150202
 0250 CONTINUE                                                          07160202
      IVCOMP = 0                                                        07170202
      IVCORR = 1                                                        07180202
      IF (CVTN03 .EQ. CVTN04) IVCOMP = 1                                07190202
40250 IF (IVCOMP - 1) 20250,10250,20250                                 07200202
30250 IVDELE = IVDELE + 1                                               07210202
      WRITE (I02,80000) IVTNUM                                          07220202
      IF (ICZERO) 10250, 0261, 20250                                    07230202
10250 IVPASS = IVPASS + 1                                               07240202
      WRITE (I02,80002) IVTNUM                                          07250202
      GO TO 0261                                                        07260202
20250 IVFAIL = IVFAIL + 1                                               07270202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07280202
 0261 CONTINUE                                                          07290202
C                                                                       07300202
C     ****  FCVS PROGRAM 202  -  TEST 026  ****                         07310202
C                                                                       07320202
C         RELATIONAL OPERATOR .NE.                                      07330202
C                                                                       07340202
      IVTNUM =  26                                                      07350202
      IF (ICZERO) 30260, 0260, 30260                                    07360202
 0260 CONTINUE                                                          07370202
      IVCOMP = 0                                                        07380202
      IVCORR = 0                                                        07390202
      IF (CVTN03 .NE. CVTN04) IVCOMP = 1                                07400202
40260 IF (IVCOMP) 20260,10260,20260                                     07410202
30260 IVDELE = IVDELE + 1                                               07420202
      WRITE (I02,80000) IVTNUM                                          07430202
      IF (ICZERO) 10260, 0271, 20260                                    07440202
10260 IVPASS = IVPASS + 1                                               07450202
      WRITE (I02,80002) IVTNUM                                          07460202
      GO TO 0271                                                        07470202
20260 IVFAIL = IVFAIL + 1                                               07480202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07490202
 0271 CONTINUE                                                          07500202
C                                                                       07510202
C     ****  FCVS PROGRAM 202  -  TEST 027  ****                         07520202
C                                                                       07530202
C         RELATIONAL OPERATOR .LE.                                      07540202
C                                                                       07550202
      IVTNUM =  27                                                      07560202
      IF (ICZERO) 30270, 0270, 30270                                    07570202
 0270 CONTINUE                                                          07580202
      IVCOMP = 0                                                        07590202
      IVCORR = 1                                                        07600202
      IF (CVTN03 .LE. CVTN04) IVCOMP = 1                                07610202
40270 IF (IVCOMP - 1) 20270,10270,20270                                 07620202
30270 IVDELE = IVDELE + 1                                               07630202
      WRITE (I02,80000) IVTNUM                                          07640202
      IF (ICZERO) 10270, 0281, 20270                                    07650202
10270 IVPASS = IVPASS + 1                                               07660202
      WRITE (I02,80002) IVTNUM                                          07670202
      GO TO 0281                                                        07680202
20270 IVFAIL = IVFAIL + 1                                               07690202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07700202
 0281 CONTINUE                                                          07710202
C                                                                       07720202
C     ****  FCVS PROGRAM 202  -  TEST 028  ****                         07730202
C                                                                       07740202
C         RELATIONAL OPERATOR .LT.                                      07750202
C                                                                       07760202
      IVTNUM =  28                                                      07770202
      IF (ICZERO) 30280, 0280, 30280                                    07780202
 0280 CONTINUE                                                          07790202
      IVCOMP = 0                                                        07800202
      IVCORR = 0                                                        07810202
      IF (CVTN03 .LT. CVTN04) IVCOMP=1                                  07820202
40280 IF (IVCOMP) 20280,10280,20280                                     07830202
30280 IVDELE = IVDELE + 1                                               07840202
      WRITE (I02,80000) IVTNUM                                          07850202
      IF (ICZERO) 10280, 0291, 20280                                    07860202
10280 IVPASS = IVPASS + 1                                               07870202
      WRITE (I02,80002) IVTNUM                                          07880202
      GO TO 0291                                                        07890202
20280 IVFAIL = IVFAIL + 1                                               07900202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07910202
 0291 CONTINUE                                                          07920202
C                                                                       07930202
C     ****  FCVS PROGRAM 202  -  TEST 029  ****                         07940202
C                                                                       07950202
C         RELATIONAL OPERATOR .GE.                                      07960202
C                                                                       07970202
      IVTNUM =  29                                                      07980202
      IF (ICZERO) 30290, 0290, 30290                                    07990202
 0290 CONTINUE                                                          08000202
      IVCOMP = 0                                                        08010202
      IVCORR = 1                                                        08020202
      IF (CVTN03 .GE. CVTN04) IVCOMP = 1                                08030202
40290 IF (IVCOMP - 1) 20290,10290,20290                                 08040202
30290 IVDELE = IVDELE + 1                                               08050202
      WRITE (I02,80000) IVTNUM                                          08060202
      IF (ICZERO) 10290, 0301, 20290                                    08070202
10290 IVPASS = IVPASS + 1                                               08080202
      WRITE (I02,80002) IVTNUM                                          08090202
      GO TO 0301                                                        08100202
20290 IVFAIL = IVFAIL + 1                                               08110202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08120202
 0301 CONTINUE                                                          08130202
C                                                                       08140202
C     ****  FCVS PROGRAM 202  -  TEST 030  ****                         08150202
C                                                                       08160202
C         RELATIONAL OPERATOR .GT.                                      08170202
C                                                                       08180202
      IVTNUM =  30                                                      08190202
      IF (ICZERO) 30300, 0300, 30300                                    08200202
 0300 CONTINUE                                                          08210202
      IVCOMP = 0                                                        08220202
      IVCORR = 0                                                        08230202
      IF (CVTN03 .GT. CVTN04) IVCOMP = 1                                08240202
40300 IF (IVCOMP) 20300,10300,20300                                     08250202
30300 IVDELE = IVDELE + 1                                               08260202
      WRITE (I02,80000) IVTNUM                                          08270202
      IF (ICZERO) 10300, 0311, 20300                                    08280202
10300 IVPASS = IVPASS + 1                                               08290202
      WRITE (I02,80002) IVTNUM                                          08300202
      GO TO 0311                                                        08310202
20300 IVFAIL = IVFAIL + 1                                               08320202
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08330202
 0311 CONTINUE                                                          08340202
C                                                                       08350202
C                                                                       08360202
C     WRITE OUT TEST SUMMARY                                            08370202
C                                                                       08380202
      WRITE (I02,90004)                                                 08390202
      WRITE (I02,90014)                                                 08400202
      WRITE (I02,90004)                                                 08410202
      WRITE (I02,90000)                                                 08420202
      WRITE (I02,90004)                                                 08430202
      WRITE (I02,90020) IVFAIL                                          08440202
      WRITE (I02,90022) IVPASS                                          08450202
      WRITE (I02,90024) IVDELE                                          08460202
      STOP                                                              08470202
90001 FORMAT (1H ,24X,5HFM202)                                          08480202
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM202)                          08490202
C                                                                       08500202
C     FORMATS FOR TEST DETAIL LINES                                     08510202
C                                                                       08520202
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   08530202
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08540202
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08550202
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08560202
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        08570202
C                                                                       08580202
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08590202
C                                                                       08600202
90002 FORMAT (1H1)                                                      08610202
90004 FORMAT (1H )                                                      08620202
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08630202
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   08640202
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         08650202
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  08660202
90014 FORMAT (1H ,5X,46H----------------------------------------------) 08670202
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08680202
C                                                                       08690202
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 08700202
C                                                                       08710202
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              08720202
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              08730202
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             08740202
      END                                                               08750202
