      PROGRAM FM200                                                     00010200
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020200
C                                                                       00030200
C        THIS ROUTINE IS THE FIRST AUDIT PROGRAM TO CONTAIN A PROGRAM   00040200
C     STATEMENT.  THE FOLLOWING FEATURES FROM CHAPTER 3., CHARACTERS,   00050200
C     LINES AND EXECUTION SEQUENCE ARE TESTED.                          00060200
C                                                                       00070200
C        (1)  ASTERISK (*) IN COLUMN 1 TO DESIGNATE A COMMENT LINE.     00080200
C        (2)  USE OF NON-FORTRAN CHARACTERS WITHIN A COMMENT LINE.      00090200
C        (3)  STATEMENT LABELS ON NONEXECUTABLE STATEMENTS.             00100200
C        (4)  DIGIT 0 IN COLUMN 6 OF AN INITIAL LINE.                   00110200
C        (5)  CONTINUATION LINES - MAXIMUM OF NINE CONTINUATION LINES   00120200
C             (660 CHARACTERS).                                         00130200
C        (6)  BLANK CHARACTERS WITHIN STATEMENTS.                       00140200
C        (7)  BLANK COMMENT LINE, BLANK CHARACTERS IN COLUMNS 1-72.     00150200
C                                                                       00160200
C     THE BASIC FEATURES OF SUBSET FORTRAN WHICH ARE TESTED BY THIS     00170200
C     PROGRAM ARE USED THROUGHOUT THE REST OF THE SUBSET ROUTINES.      00180200
C                                                                       00190200
C     REFERENCES                                                        00200200
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00210200
C              X3.9-1978                                                00220200
C                                                                       00230200
C        SECTION 3.1.6, BLANK CHARACTER                                 00240200
C        SECTION 3.2.1, COMMENT LINE                                    00250200
C        SECTION 3.2.2, INITIAL LINE                                    00260200
C        SECTION 3.2.3, CONTINUATION LINE                               00270200
C        SECTION 3.3, STATEMENTS                                        00280200
C        SECTION 3.4, STATEMENT LABEL                                   00290200
C        SECTION 14.1, PROGRAM STATEMENT                                00300200
C                                                                       00310200
C                                                                       00320200
C     ******************************************************************00330200
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00340200
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00350200
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00360200
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00370200
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00380200
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00390200
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00400200
C     THE RESULT OF EXECUTING THESE TESTS.                              00410200
C                                                                       00420200
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00430200
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00440200
C                                                                       00450200
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00460200
C                    DEPARTMENT OF THE NAVY                             00470200
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00480200
C                    WASHINGTON, D.C.   20376                           00490200
C                                                                       00500200
C     ******************************************************************00510200
C                                                                       00520200
C                                                                       00530200
      IMPLICIT LOGICAL (L)                                              00540200
      IMPLICIT CHARACTER*14 (C)                                         00550200
C                                                                       00560200
   12 INTEGER XVTN01                                                    00570200
   22 DATA IVON02/5/                                                    00580200
C        THE PRECEDING STATEMENTS ARE NONEXECUTABLE STATEMENTS WHICH    00590200
C     CONTAIN STATEMENT LABELS.  THEY ARE REFERENCED IN TESTS 1 AND 2.  00600200
C                                                                       00610200
C                                                                       00620200
C                                                                       00630200
C     INITIALIZATION SECTION.                                           00640200
C                                                                       00650200
C     INITIALIZE CONSTANTS                                              00660200
C     ********************                                              00670200
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00680200
      I01 = 5                                                           00690200
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00700200
      I02 = 6                                                           00710200
C     SYSTEM ENVIRONMENT SECTION                                        00720200
C                                                                       00730200
      I01 = 5                                                           00740200
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750200
C     (UNIT NUMBER FOR CARD READER).                                    00760200
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00770200
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00780200
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00790200
C                                                                       00800200
      I02 = 6                                                           00810200
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00820200
C     (UNIT NUMBER FOR PRINTER).                                        00830200
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00840200
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00850200
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00860200
C                                                                       00870200
      IVPASS = 0                                                        00880200
      IVFAIL = 0                                                        00890200
      IVDELE = 0                                                        00900200
      ICZERO = 0                                                        00910200
C                                                                       00920200
C     WRITE OUT PAGE HEADERS                                            00930200
C                                                                       00940200
      WRITE (I02,90002)                                                 00950200
      WRITE (I02,90006)                                                 00960200
      WRITE (I02,90008)                                                 00970200
      WRITE (I02,90004)                                                 00980200
      WRITE (I02,90010)                                                 00990200
      WRITE (I02,90004)                                                 01000200
      WRITE (I02,90016)                                                 01010200
      WRITE (I02,90001)                                                 01020200
      WRITE (I02,90004)                                                 01030200
      WRITE (I02,90012)                                                 01040200
      WRITE (I02,90014)                                                 01050200
      WRITE (I02,90004)                                                 01060200
C                                                                       01070200
C                                                                       01080200
C        TEST 1 AND TEST 2 REFERENCE VARIABLES DEFINED IN NONEXECUTABLE 01090200
C     STATEMENTS WHICH CONTAIN STATEMENT LABELS.  THE NONEXECUTABLE     01100200
C     STATEMENTS WHICH APPEAR AT THE BEGINNING OF THE PROGRAM ARE       01110200
C            12 INTEGER XVTN01                                          01120200
C            22 DATA IVON02/5/                                          01130200
C                                                                       01140200
C     REFERENCE   X3.9-1977, SECTION 3.4, STATEMENT LABELS              01150200
C                                                                       01160200
C                                                                       01170200
C     ****  FCVS PROGRAM 200  -  TEST 001  ****                         01180200
C                                                                       01190200
C        TEST 001 ASSIGNS AN INTEGER VALUE TO XVTN01 WHICH WAS SPECIFIED01200200
C     AS TYPE INTEGER IN AN INTEGER STATEMENT CONTAINING A STATEMENT    01210200
C     LABEL.                                                            01220200
C                                                                       01230200
      IVTNUM =   1                                                      01240200
      IF (ICZERO) 30010, 0010, 30010                                    01250200
 0010 CONTINUE                                                          01260200
      IVCOMP = 0                                                        01270200
       XVTN01 = 1                                                       01280200
      IVCOMP = XVTN01                                                   01290200
      IVCORR = 1                                                        01300200
40010 IF (IVCOMP - 1) 20010, 10010, 20010                               01310200
30010 IVDELE = IVDELE + 1                                               01320200
      WRITE (I02,80000) IVTNUM                                          01330200
      IF (ICZERO) 10010, 0021, 20010                                    01340200
10010 IVPASS = IVPASS + 1                                               01350200
      WRITE (I02,80002) IVTNUM                                          01360200
      GO TO 0021                                                        01370200
20010 IVFAIL = IVFAIL + 1                                               01380200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01390200
 0021 CONTINUE                                                          01400200
C                                                                       01410200
C     ****  FCVS PROGRAM 200  -  TEST 002  ****                         01420200
C                                                                       01430200
C        TEST 002 CHECKS THE VALUE WHICH WAS ASSIGNED TO IVON02 BY A    01440200
C     DATA STATEMENT WITH A STATEMENT LABEL.                            01450200
C                                                                       01460200
      IVTNUM =   2                                                      01470200
      IF (ICZERO) 30020, 0020, 30020                                    01480200
 0020 CONTINUE                                                          01490200
      IVCOMP = 0                                                        01500200
      IVCOMP = IVON02                                                   01510200
      IVCORR = 5                                                        01520200
40020 IF (IVCOMP - 5) 20020, 10020, 20020                               01530200
30020 IVDELE = IVDELE + 1                                               01540200
      WRITE (I02,80000) IVTNUM                                          01550200
      IF (ICZERO) 10020, 0031, 20020                                    01560200
10020 IVPASS = IVPASS + 1                                               01570200
      WRITE (I02,80002) IVTNUM                                          01580200
      GO TO 0031                                                        01590200
20020 IVFAIL = IVFAIL + 1                                               01600200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01610200
 0031 CONTINUE                                                          01620200
C                                                                       01630200
C        TEST 3 THROUGH TEST 5 USE AN ASTERISK (*) IN COLUMN 1 TO       01640200
C     DENOTE A COMMENT LINE.                                            01650200
C                                                                       01660200
C     REFERENCE   X3.9-1977, SECTION 3.2.1, COMMENT LINE                01670200
C                                                                       01680200
C                                                                       01690200
C     ****  FCVS PROGRAM 200  -  TEST 003  ****                         01700200
C                                                                       01710200
C        GO TO STATEMENT IN ASTERISK COMMENT LINE.                      01720200
C                                                                       01730200
      IVTNUM =   3                                                      01740200
      IF (ICZERO) 30030, 0030, 30030                                    01750200
 0030 CONTINUE                                                          01760200
      IVCOMP = 1                                                        01770200
*          GO TO 20030                                                  01780200
      IVCOMP = 0                                                        01790200
      IVCORR = 0                                                        01800200
40030 IF (IVCOMP) 20030, 10030, 20030                                   01810200
30030 IVDELE = IVDELE + 1                                               01820200
      WRITE (I02,80000) IVTNUM                                          01830200
      IF (ICZERO) 10030, 0041, 20030                                    01840200
10030 IVPASS = IVPASS + 1                                               01850200
      WRITE (I02,80002) IVTNUM                                          01860200
      GO TO 0041                                                        01870200
20030 IVFAIL = IVFAIL + 1                                               01880200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01890200
 0041 CONTINUE                                                          01900200
C                                                                       01910200
C     ****  FCVS PROGRAM 200  -  TEST 004  ****                         01920200
C                                                                       01930200
C        SEVERAL * COMMENT LINES INTERMIXED WITH EXECUTABLE STATEMENTS. 01940200
C                                                                       01950200
      IVTNUM =   4                                                      01960200
      IF (ICZERO) 30040, 0040, 30040                                    01970200
 0040 CONTINUE                                                          01980200
      IVCOMP = 0                                                        01990200
*     THE * COMMENT LINE IS THE SAME AS A C COMMENT LINE.               02000200
      IVCOMP = 1                                                        02010200
*     THE * COMMENT LINES HAVE NO EFFECT ON THE PROGRAM EXECUTION.      02020200
*     THEIR USE IS STRICTLY FOR DOCUMENTATION PURPOSES.                 02030200
      IVCOMP = 2                                                        02040200
*     IVCOMP = 3                                                        02050200
*  40 ANY STATEMENT LABELS ON COMMENT LINES ARE IGNORED.                02060200
      IVCORR = 2                                                        02070200
40040 IF (IVCOMP - 2) 20040, 10040, 20040                               02080200
30040 IVDELE = IVDELE + 1                                               02090200
      WRITE (I02,80000) IVTNUM                                          02100200
      IF (ICZERO) 10040, 0051, 20040                                    02110200
10040 IVPASS = IVPASS + 1                                               02120200
      WRITE (I02,80002) IVTNUM                                          02130200
      GO TO 0051                                                        02140200
20040 IVFAIL = IVFAIL + 1                                               02150200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02160200
 0051 CONTINUE                                                          02170200
C                                                                       02180200
C     ****  FCVS PROGRAM 200  -  TEST 005  ****                         02190200
C                                                                       02200200
C        NONFORTRAN CHARACTERS WITHIN C AND * COMMENT LINES.            02210200
C                                                                       02220200
      IVTNUM =   5                                                      02230200
      IF (ICZERO) 30050, 0050, 30050                                    02240200
 0050 CONTINUE                                                          02250200
      IVCOMP = 1                                                        02260200
*          <>%?   NONFORTRAN CHARACTER                                  02270200
C          <>%?   NONFORTRAN CHARACTER                                  02280200
      IVCOMP = 0                                                        02290200
      IVCORR = 0                                                        02300200
40050 IF (IVCOMP) 20050, 10050, 20050                                   02310200
30050 IVDELE = IVDELE + 1                                               02320200
      WRITE (I02,80000) IVTNUM                                          02330200
      IF (ICZERO) 10050, 0061, 20050                                    02340200
10050 IVPASS = IVPASS + 1                                               02350200
      WRITE (I02,80002) IVTNUM                                          02360200
      GO TO 0061                                                        02370200
20050 IVFAIL = IVFAIL + 1                                               02380200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02390200
 0061 CONTINUE                                                          02400200
C                                                                       02410200
C     ****  FCVS PROGRAM 200  -  TEST 006  ****                         02420200
C                                                                       02430200
C        LINES CONTAINING ONLY BLANK CHARACTERS IN COLUMNS 1 THROUGH    02440200
C     72 ARE COMMENT LINES.                                             02450200
C                                                                       02460200
C     REFERENCE   X3.9-1977, SECTION 3.2.1, COMMENT LINE                02470200
C                                                                       02480200
      IVTNUM =   6                                                      02490200
      IF (ICZERO) 30060, 0060, 30060                                    02500200
 0060 CONTINUE                                                          02510200
      IVCOMP = 0                                                        02520200
                                                                        02530200
      IVCORR = 3                                                        02540200
      IVCOMP = 9                                                        02550200
*        ASTERISK COMMENT LINE FOLLOWED BY BLANK COMMENT LINE.          02560200
                                                                        02570200
*        ASTERISK COMMENT LINE.                                         02580200
      IVCOMP = 3                                                        02590200
40060 IF (IVCOMP - 3) 20060, 10060, 20060                               02600200
30060 IVDELE = IVDELE + 1                                               02610200
      WRITE (I02,80000) IVTNUM                                          02620200
      IF (ICZERO) 10060, 0071, 20060                                    02630200
10060 IVPASS = IVPASS + 1                                               02640200
      WRITE (I02,80002) IVTNUM                                          02650200
      GO TO 0071                                                        02660200
20060 IVFAIL = IVFAIL + 1                                               02670200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02680200
 0071 CONTINUE                                                          02690200
C                                                                       02700200
C        TEST 7 AND TEST 8 CONTAIN THE DIGIT 0 IN COLUMN 6 OF INITIAL   02710200
C     LINES.                                                            02720200
C                                                                       02730200
C     REFERENCE   X3.9-1977, SECTION 3.2.2, INITIAL LINE                02740200
C                                                                       02750200
C                                                                       02760200
C     ****  FCVS PROGRAM 200  -  TEST 007  ****                         02770200
C                                                                       02780200
C        TEST 007 USES THE DIGIT 0 IN COLUMN 6 OF TWO SUCCESSIVE        02790200
C     INITIAL LINES.                                                    02800200
C                                                                       02810200
      IVTNUM =   7                                                      02820200
      IF (ICZERO) 30070, 0070, 30070                                    02830200
 0070 CONTINUE                                                          02840200
      IVCOMP = 0                                                        02850200
     0IVON01 = 5                                                        02860200
     0IVON02 = 6                                                        02870200
      IVCOMP = IVON01 + IVON02                                          02880200
      IVCORR = 11                                                       02890200
40070 IF (IVCOMP - 11) 20070, 10070, 20070                              02900200
30070 IVDELE = IVDELE + 1                                               02910200
      WRITE (I02,80000) IVTNUM                                          02920200
      IF (ICZERO) 10070, 0081, 20070                                    02930200
10070 IVPASS = IVPASS + 1                                               02940200
      WRITE (I02,80002) IVTNUM                                          02950200
      GO TO 0081                                                        02960200
20070 IVFAIL = IVFAIL + 1                                               02970200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02980200
 0081 CONTINUE                                                          02990200
C                                                                       03000200
C     ****  FCVS PROGRAM 200  -  TEST 008  ****                         03010200
C                                                                       03020200
C        TEST 008 MIXES STATEMENTS WITH DIGIT 0 IN COLUMN 6 OF INITIAL  03030200
C     LINE AND COMMENT LINES WITH * IN COLUMN 1.                        03040200
C                                                                       03050200
      IVTNUM =   8                                                      03060200
      IF (ICZERO) 30080, 0080, 30080                                    03070200
 0080 CONTINUE                                                          03080200
      IVCOMP = 0                                                        03090200
*        FIRST INITIAL LINE FOLLOWS.                                    03100200
     0IVON01 = 5                                                        03110200
*        TWO SUCCESSIVE COMMENT LINES,                                  03120200
*        FOLLOWED BY TWO INITIAL LINES.                                 03130200
     0IVON02=4                                                          03140200
     0IVCOMP=IVON01+IVON02                                              03150200
*          FALL THROUGH TO VERIFICATION CODE                            03160200
      IVCORR = 9                                                        03170200
40080 IF (IVCOMP - 9) 20080, 10080, 20080                               03180200
30080 IVDELE = IVDELE + 1                                               03190200
      WRITE (I02,80000) IVTNUM                                          03200200
      IF (ICZERO) 10080, 0091, 20080                                    03210200
10080 IVPASS = IVPASS + 1                                               03220200
      WRITE (I02,80002) IVTNUM                                          03230200
      GO TO 0091                                                        03240200
20080 IVFAIL = IVFAIL + 1                                               03250200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03260200
 0091 CONTINUE                                                          03270200
C                                                                       03280200
C        TEST 9 THROUGH TEST 13 VERIFY THAT CONTINUATION LINES ARE      03290200
C     PERMITTED.                                                        03300200
C                                                                       03310200
C     REFERENCE   X3.9-1977, SECTION 3.2.3, CONTINUATION LINE           03320200
C                                                                       03330200
C                                                                       03340200
C     ****  FCVS PROGRAM 200  -  TEST 009  ****                         03350200
C                                                                       03360200
C        STATEMENT WITH TWO CONTINUATION LINES.                         03370200
C                                                                       03380200
      IVTNUM =   9                                                      03390200
      IF (ICZERO) 30090, 0090, 30090                                    03400200
 0090 CONTINUE                                                          03410200
      IVON01 = 0                                                        03420200
      IVON                                                              03430200
     1    01                                                            03440200
     2       = 2                                                        03450200
      IVCOMP = IVON01                                                   03460200
      IVCORR = 2                                                        03470200
40090 IF (IVCOMP - 2) 20090, 10090, 20090                               03480200
30090 IVDELE = IVDELE + 1                                               03490200
      WRITE (I02,80000) IVTNUM                                          03500200
      IF (ICZERO) 10090, 0101, 20090                                    03510200
10090 IVPASS = IVPASS + 1                                               03520200
      WRITE (I02,80002) IVTNUM                                          03530200
      GO TO 0101                                                        03540200
20090 IVFAIL = IVFAIL + 1                                               03550200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03560200
 0101 CONTINUE                                                          03570200
C                                                                       03580200
C     ****  FCVS PROGRAM 200  -  TEST 010  ****                         03590200
C                                                                       03600200
C        STATEMENT WITH NINE CONTINUATION LINES.                        03610200
C                                                                       03620200
      IVTNUM =  10                                                      03630200
      IF (ICZERO) 30100, 0100, 30100                                    03640200
 0100 CONTINUE                                                          03650200
      IVON01 = 0                                                        03660200
      IVON01 =                                                          03670200
     1         1                                                        03680200
     2        +1                                                        03690200
     3              +1                                                  03700200
     4    +1                                                            03710200
     5      +1                                                          03720200
     6      +1                                                          03730200
     7                                  +1                              03740200
     8                                                      +1          03750200
     9+1                                                                03760200
      IVCOMP = IVON01                                                   03770200
      IVCORR = 9                                                        03780200
40100 IF (IVCOMP - 9) 20100, 10100, 20100                               03790200
30100 IVDELE = IVDELE + 1                                               03800200
      WRITE (I02,80000) IVTNUM                                          03810200
      IF (ICZERO) 10100, 0111, 20100                                    03820200
10100 IVPASS = IVPASS + 1                                               03830200
      WRITE (I02,80002) IVTNUM                                          03840200
      GO TO 0111                                                        03850200
20100 IVFAIL = IVFAIL + 1                                               03860200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03870200
 0111 CONTINUE                                                          03880200
C                                                                       03890200
C     ****  FCVS PROGRAM 200  -  TEST 011  ****                         03900200
C                                                                       03910200
C        TEST 011 CONTAINS THE MAXIMUM NUMBER OF CONTINUATION LINES     03920200
C     PERMITTED IN THE SUBSET LANGUAGE AND EACH OF THE 660 CHARACTERS   03930200
C     IN THE STATEMENT ARE NONBLANK.                                    03940200
C                                                                       03950200
      IVTNUM =  11                                                      03960200
      IF (ICZERO) 30110, 0110, 30110                                    03970200
 0110 CONTINUE                                                          03980200
      IVON01 = 1                                                        03990200
      IVCOMP = 0                                                        04000200
      IVCOMP=IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVO04010200
     1N01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON0104020200
     2+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IV04030200
     3ON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON004040200
     41+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+I04050200
     5VON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON04060200
     601+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+04070200
     7IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVO04080200
     8N01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON0104090200
     9+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+IVON01+1204100200
      IVCORR = 105                                                      04110200
40110 IF (IVCOMP - 105) 20110, 10110, 20110                             04120200
30110 IVDELE = IVDELE + 1                                               04130200
      WRITE (I02,80000) IVTNUM                                          04140200
      IF (ICZERO) 10110, 0121, 20110                                    04150200
10110 IVPASS = IVPASS + 1                                               04160200
      WRITE (I02,80002) IVTNUM                                          04170200
      GO TO 0121                                                        04180200
20110 IVFAIL = IVFAIL + 1                                               04190200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04200200
 0121 CONTINUE                                                          04210200
C                                                                       04220200
C     ****  FCVS PROGRAM 200  -  TEST 012  ****                         04230200
C                                                                       04240200
C        TEST 012 SPLITS A STATEMENT ACROSS 8 CONTINUATION LINES.       04250200
C     THERE IS A STATEMENT LABEL IN COLUMNS 1-5 AND 0 IN COLUMN 6       04260200
C     OF THE INITIAL LINE.                                              04270200
C                                                                       04280200
      IVTNUM =  12                                                      04290200
      IF (ICZERO) 30120, 0120, 30120                                    04300200
 0120 CONTINUE                                                          04310200
      IVON01 = 0                                                        04320200
      GO TO 0122                                                        04330200
 01220   I                                                              04340200
     1    V                                                             04350200
     2     O                                                            04360200
     3      N                                                           04370200
     4       0                                                          04380200
     5        1                                                         04390200
     6         =                                                        04400200
     7          8                                                       04410200
     8           9                                                      04420200
      IVCOMP = IVON01                                                   04430200
      IVCORR = 89                                                       04440200
40120 IF (IVCOMP - 89) 20120, 10120, 20120                              04450200
30120 IVDELE = IVDELE + 1                                               04460200
      WRITE (I02,80000) IVTNUM                                          04470200
      IF (ICZERO) 10120, 0131, 20120                                    04480200
10120 IVPASS = IVPASS + 1                                               04490200
      WRITE (I02,80002) IVTNUM                                          04500200
      GO TO 0131                                                        04510200
20120 IVFAIL = IVFAIL + 1                                               04520200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04530200
 0131 CONTINUE                                                          04540200
C                                                                       04550200
C     ****  FCVS PROGRAM 200  -  TEST 013  ****                         04560200
C                                                                       04570200
C        TEST 013 CONSISTS OF AN INITIAL LINE WHICH CONTAINS ONLY A     04580200
C     STATEMENT LABEL AND A CONTINUATION LINE WHICH CONTAINS THE        04590200
C     EXECUTABLE STATEMENT.                                             04600200
C                                                                       04610200
      IVTNUM =  13                                                      04620200
      IF (ICZERO) 30130, 0130, 30130                                    04630200
 0130 CONTINUE                                                          04640200
      IVCOMP = 0                                                        04650200
 0132                                                                   04660200
     1IVCOMP = 4                                                        04670200
      IVCORR = 4                                                        04680200
40130 IF (IVCOMP - 4) 20130, 10130, 20130                               04690200
30130 IVDELE = IVDELE + 1                                               04700200
      WRITE (I02,80000) IVTNUM                                          04710200
      IF (ICZERO) 10130, 0141, 20130                                    04720200
10130 IVPASS = IVPASS + 1                                               04730200
      WRITE (I02,80002) IVTNUM                                          04740200
      GO TO 0141                                                        04750200
20130 IVFAIL = IVFAIL + 1                                               04760200
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04770200
 0141 CONTINUE                                                          04780200
C                                                                       04790200
C                                                                       04800200
C     WRITE OUT TEST SUMMARY                                            04810200
C                                                                       04820200
      WRITE (I02,90004)                                                 04830200
      WRITE (I02,90014)                                                 04840200
      WRITE (I02,90004)                                                 04850200
      WRITE (I02,90000)                                                 04860200
      WRITE (I02,90004)                                                 04870200
      WRITE (I02,90020) IVFAIL                                          04880200
      WRITE (I02,90022) IVPASS                                          04890200
      WRITE (I02,90024) IVDELE                                          04900200
      STOP                                                              04910200
90001 FORMAT (1H ,24X,5HFM200)                                          04920200
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM200)                          04930200
C                                                                       04940200
C     FORMATS FOR TEST DETAIL LINES                                     04950200
C                                                                       04960200
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   04970200
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      04980200
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         04990200
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    05000200
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        05010200
C                                                                       05020200
C     FORMAT STATEMENTS FOR PAGE HEADERS                                05030200
C                                                                       05040200
90002 FORMAT (1H1)                                                      05050200
90004 FORMAT (1H )                                                      05060200
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05070200
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   05080200
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         05090200
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  05100200
90014 FORMAT (1H ,5X,46H----------------------------------------------) 05110200
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             05120200
C                                                                       05130200
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 05140200
C                                                                       05150200
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              05160200
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              05170200
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             05180200
      END                                                               05190200
