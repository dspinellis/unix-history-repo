      PROGRAM FM252                                                     00010252
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020252
C                                                                       00030252
C                                                                       00040252
C                                                                       00050252
C        THIS PROGRAM TESTS REDEFINITION OF STATEMENT LABELS WITH THE   00060252
C     ASSIGN STATEMENT IN CONJUNCTION WITH THE ASSIGNED GO TO STATEMENT.00070252
C     THE OPTIONAL COMMA IN THE SYNTAX OF THE ASSIGNED GO TO IS TESTED. 00080252
C     THE RANGE OF STATEMENT LABELS ( FROM 00001 TO 99999 ) IS TESTED   00090252
C     USING THE ASSIGN STATEMENT AND THE ASSIGNED GO TO STATEMENT.      00100252
C     IT ALSO TESTS THE OPTIONAL COMMA IN THE SYNTAX OF THE COMPUTED    00110252
C     GO TO STATEMENT AND HAS TESTS ON THE RANGE OF THE INDEX IN THE    00120252
C     COMPUTED GO TO.                                                   00130252
C                                                                       00140252
C                                                                       00150252
C     REFERENCES                                                        00160252
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00170252
C             X3.9-1978                                                 00180252
C        SECTION 10.3,       STATEMENT LABEL ASSIGNMENT (ASSIGN)        00190252
C        SECTION 11.2,       COMPUTED GO TO STATEMENT                   00200252
C        SECTION 11.3,       ASSIGNED GO TO STATEMENT                   00210252
C                                                                       00220252
C                                                                       00230252
C        FM013 - SUBSET LEVEL TESTS OF THE ASSIGN STATEMENT AND THE     00240252
C                ASSIGNED GO TO STATEMENT.                              00250252
C                                                                       00260252
C        FM014, FM052, AND FM053 - SUBSET LEVEL TESTS OF THE COMPUTED   00270252
C                GO TO STATEMENT.                                       00280252
C                                                                       00290252
C                                                                       00300252
C                                                                       00310252
C                                                                       00320252
C     ******************************************************************00330252
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00340252
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00350252
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00360252
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00370252
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00380252
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00390252
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00400252
C     THE RESULT OF EXECUTING THESE TESTS.                              00410252
C                                                                       00420252
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00430252
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00440252
C                                                                       00450252
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00460252
C                    DEPARTMENT OF THE NAVY                             00470252
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00480252
C                    WASHINGTON, D.C.   20376                           00490252
C                                                                       00500252
C     ******************************************************************00510252
C                                                                       00520252
C                                                                       00530252
      IMPLICIT LOGICAL (L)                                              00540252
      IMPLICIT CHARACTER*14 (C)                                         00550252
C                                                                       00560252
C                                                                       00570252
C                                                                       00580252
C     INITIALIZATION SECTION.                                           00590252
C                                                                       00600252
C     INITIALIZE CONSTANTS                                              00610252
C     ********************                                              00620252
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00630252
      I01 = 5                                                           00640252
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00650252
      I02 = 6                                                           00660252
C     SYSTEM ENVIRONMENT SECTION                                        00670252
C                                                                       00680252
      I01 = 5                                                           00690252
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700252
C     (UNIT NUMBER FOR CARD READER).                                    00710252
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00720252
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00730252
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00740252
C                                                                       00750252
      I02 = 6                                                           00760252
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00770252
C     (UNIT NUMBER FOR PRINTER).                                        00780252
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00790252
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00800252
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00810252
C                                                                       00820252
      IVPASS = 0                                                        00830252
      IVFAIL = 0                                                        00840252
      IVDELE = 0                                                        00850252
      ICZERO = 0                                                        00860252
C                                                                       00870252
C     WRITE OUT PAGE HEADERS                                            00880252
C                                                                       00890252
      WRITE (I02,90002)                                                 00900252
      WRITE (I02,90006)                                                 00910252
      WRITE (I02,90008)                                                 00920252
      WRITE (I02,90004)                                                 00930252
      WRITE (I02,90010)                                                 00940252
      WRITE (I02,90004)                                                 00950252
      WRITE (I02,90016)                                                 00960252
      WRITE (I02,90001)                                                 00970252
      WRITE (I02,90004)                                                 00980252
      WRITE (I02,90012)                                                 00990252
      WRITE (I02,90014)                                                 01000252
      WRITE (I02,90004)                                                 01010252
C                                                                       01020252
C                                                                       01030252
C     ****  FCVS PROGRAM 252  -  TEST 001  ****                         01040252
C                                                                       01050252
C        TEST 001 IS AN ASSIGN STATEMENT IN WHICH THE STATEMENT         01060252
C     LABEL IS ACTUALLY FOR A FORMAT STATEMENT.  IN 10.3 - THE STATEMENT01070252
C     LABEL MUST BE THE LABEL OF AN EXECUTABLE STATEMENT OR A FORMAT    01080252
C     STATEMENT.  THE ASSIGN STATEMENT IS FOLLOWED BY A SIMPLE WRITE    01090252
C     TO THE PRINTER.                                                   01100252
C                                                                       01110252
C                                                                       01120252
      IVTNUM =   1                                                      01130252
      IF (ICZERO) 30010, 0010, 30010                                    01140252
 0010 CONTINUE                                                          01150252
      ASSIGN 0012 TO I                                                  01160252
 0012 FORMAT (51H **** ASSIGN FORMAT NUMBER TO INTEGER VARIABLE ****)   01170252
      WRITE (I02, I)                                                    01180252
C        ***** VISUALLY CHECK THE OUTPUT PRINTER LISTING *****          01190252
      IVCOMP = 0                                                        01200252
      IVCORR = 0                                                        01210252
40010 IF ( IVCOMP )  20010, 10010, 20010                                01220252
30010 IVDELE = IVDELE + 1                                               01230252
      WRITE (I02,80000) IVTNUM                                          01240252
      IF (ICZERO) 10010, 0021, 20010                                    01250252
10010 IVPASS = IVPASS + 1                                               01260252
      WRITE (I02,80002) IVTNUM                                          01270252
      GO TO 0021                                                        01280252
20010 IVFAIL = IVFAIL + 1                                               01290252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01300252
 0021 CONTINUE                                                          01310252
C                                                                       01320252
C     ****  FCVS PROGRAM 252  -  TEST 002  ****                         01330252
C                                                                       01340252
C        TEST 002 IS A TEST OF THE ASSIGNED GO TO STATEMENT WITH THE    01350252
C     OPTIONAL COMMA INTENTIONALLY DELETED FROM THE SYNTAX.             01360252
C                  GO TO I (S1, S2, S3)                                 01370252
C                                                                       01380252
C                                                                       01390252
      IVTNUM =   2                                                      01400252
      IF (ICZERO) 30020, 0020, 30020                                    01410252
 0020 CONTINUE                                                          01420252
      IVCOMP = 0                                                        01430252
      IVCORR = 1                                                        01440252
      ASSIGN 0023 TO J                                                  01450252
      GO TO 0025                                                        01460252
 0022 IVCOMP = 0                                                        01470252
      GO TO 40020                                                       01480252
 0023 IVCOMP = 1                                                        01490252
      GO TO 40020                                                       01500252
 0024 IVCOMP = 0                                                        01510252
      GO TO 40020                                                       01520252
 0025 GO TO J (0022, 0023, 0024)                                        01530252
C        NOTE THAT THE OPTIONAL COMMA IS NOT PRESENT AFTER THE J IN     01540252
C     PREVIOUS ASSIGNED GO TO STATEMENT.                                01550252
40020 IF ( IVCOMP - 1 )  20020, 10020, 20020                            01560252
30020 IVDELE = IVDELE + 1                                               01570252
      WRITE (I02,80000) IVTNUM                                          01580252
      IF (ICZERO) 10020, 0031, 20020                                    01590252
10020 IVPASS = IVPASS + 1                                               01600252
      WRITE (I02,80002) IVTNUM                                          01610252
      GO TO 0031                                                        01620252
20020 IVFAIL = IVFAIL + 1                                               01630252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01640252
 0031 CONTINUE                                                          01650252
C                                                                       01660252
C     ****  FCVS PROGRAM 252  -  TEST 003  ****                         01670252
C                                                                       01680252
C        TEST 003 USES A SERIES OF ASSIGN STATEMENTS TO TEST THAT THE   01690252
C     SAME STATEMENT LABEL AND INTEGER VARIABLE CAN BE USED IN A        01700252
C     MULTIPLE REDEFINITION TO THE SAME VALUES.  A SIMPLE ASSIGNED      01710252
C     GO TO IS USED TO TEST THE VALUE OF THE INTEGER VARIABLE M.        01720252
C                                                                       01730252
C                                                                       01740252
      IVTNUM =   3                                                      01750252
      IF (ICZERO) 30030, 0030, 30030                                    01760252
 0030 CONTINUE                                                          01770252
      IVCOMP = 0                                                        01780252
      IVCORR = 1                                                        01790252
      ASSIGN 0033 TO M                                                  01800252
      ASSIGN 0033 TO M                                                  01810252
      ASSIGN 0033 TO M                                                  01820252
      GO TO 0035                                                        01830252
 0032 IVCOMP = 0                                                        01840252
      GO TO 40030                                                       01850252
 0033 IVCOMP = 1                                                        01860252
      GO TO 40030                                                       01870252
 0034 IVCOMP = 0                                                        01880252
      GO TO 40030                                                       01890252
 0035 GO TO M, (0032, 0033, 0034)                                       01900252
40030 IF ( IVCOMP - 1 )  20030, 10030, 20030                            01910252
30030 IVDELE = IVDELE + 1                                               01920252
      WRITE (I02,80000) IVTNUM                                          01930252
      IF (ICZERO) 10030, 0041, 20030                                    01940252
10030 IVPASS = IVPASS + 1                                               01950252
      WRITE (I02,80002) IVTNUM                                          01960252
      GO TO 0041                                                        01970252
20030 IVFAIL = IVFAIL + 1                                               01980252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01990252
 0041 CONTINUE                                                          02000252
C                                                                       02010252
C     ****  FCVS PROGRAM 252  -  TEST 004  ****                         02020252
C                                                                       02030252
C        TEST 004 USES A SERIES OF ASSIGN STATEMENTS TO SET THE INTEGER 02040252
C     VARIABLE K TO A STATEMENT LABEL IN THE PARENTHESIZED LIST OF      02050252
C     STATEMENT LABELS FOR THE ASSIGNED GO TO STATEMENT, THEN TO A      02060252
C     STATEMENT LABEL NOT IN THE LIST AND FINALLY BACK TO A PROPER      02070252
C     STATEMENT LABEL WITHIN THE PARENTHESIZED LIST.  SECTION 11.3      02080252
C     REQUIRES - IF THE PARENTHESIZED LIST IS PRESENT, THE STATEMENT    02090252
C     LABEL ASSIGNED TO  I  MUST BE ONE OF THE STATEMENT LABELS IN      02100252
C     THE LIST.  AN ASSIGNED GO TO STATEMENT IS USED TO TEST THE FINAL  02110252
C     ASSIGNMENT OF STATEMENT LABELS TO THE INTEGER VARIABLE K.         02120252
C                                                                       02130252
C                                                                       02140252
      IVTNUM =   4                                                      02150252
      IF (ICZERO) 30040, 0040, 30040                                    02160252
 0040 CONTINUE                                                          02170252
      IVCOMP = 0                                                        02180252
      IVCORR = 1                                                        02190252
      ASSIGN 0043 TO K                                                  02200252
      ASSIGN 0042 TO K                                                  02210252
 0042 ASSIGN 0043 TO K                                                  02220252
      GO TO 0045                                                        02230252
 0043 IVCOMP = 1                                                        02240252
      GO TO 40040                                                       02250252
 0044 IVCOMP = 0                                                        02260252
      GO TO 40040                                                       02270252
 0045 GO TO K, (0044, 0043)                                             02280252
40040 IF ( IVCOMP - 1 )  20040, 10040, 20040                            02290252
30040 IVDELE = IVDELE + 1                                               02300252
      WRITE (I02,80000) IVTNUM                                          02310252
      IF (ICZERO) 10040, 0051, 20040                                    02320252
10040 IVPASS = IVPASS + 1                                               02330252
      WRITE (I02,80002) IVTNUM                                          02340252
      GO TO 0051                                                        02350252
20040 IVFAIL = IVFAIL + 1                                               02360252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02370252
 0051 CONTINUE                                                          02380252
C                                                                       02390252
C                                                                       02400252
C        THE FOLLOWING TWO TESTS CHECK THE POSSIBLE RANGE OF STATEMENT  02410252
C     LABELS ( FROM 00001 TO 99999 ) BY USING THEM IN ASSIGN STATEMENTS 02420252
C     AND ASSIGNED GO TO STATEMENTS.                                    02430252
C                                                                       02440252
C                                                                       02450252
C                                                                       02460252
C     ****  FCVS PROGRAM 252  -  TEST 005  ****                         02470252
C                                                                       02480252
C        TEST 005 USES A STATEMENT LABEL OF 00001 WHICH IS THE SMALLEST 02490252
C     ALLOWABLE STATEMENT LABEL.                                        02500252
C                                                                       02510252
C                                                                       02520252
      IVTNUM =   5                                                      02530252
      IF (ICZERO) 30050, 0050, 30050                                    02540252
 0050 CONTINUE                                                          02550252
      IVCOMP = 0                                                        02560252
      IVCORR = 1                                                        02570252
      ASSIGN 00001 TO I                                                 02580252
      GO TO 0054                                                        02590252
 0052 IVCOMP = 0                                                        02600252
      GO TO 40050                                                       02610252
00001 IVCOMP = 1                                                        02620252
      GO TO 40050                                                       02630252
 0053 IVCOMP = 0                                                        02640252
      GO TO 40050                                                       02650252
 0054 GO TO I, ( 0052, 00001, 0053 )                                    02660252
40050 IF ( IVCOMP - 1 )  20050, 10050, 20050                            02670252
30050 IVDELE = IVDELE + 1                                               02680252
      WRITE (I02,80000) IVTNUM                                          02690252
      IF (ICZERO) 10050, 0061, 20050                                    02700252
10050 IVPASS = IVPASS + 1                                               02710252
      WRITE (I02,80002) IVTNUM                                          02720252
      GO TO 0061                                                        02730252
20050 IVFAIL = IVFAIL + 1                                               02740252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02750252
 0061 CONTINUE                                                          02760252
C                                                                       02770252
C     ****  FCVS PROGRAM 252  -  TEST 006  ****                         02780252
C                                                                       02790252
C        TEST 006 USES A STATEMENT LABEL OF 99999 WHICH IS THE LARGEST  02800252
C     ALLOWABLE STATEMENT LABEL.                                        02810252
C                                                                       02820252
C                                                                       02830252
      IVTNUM =   6                                                      02840252
      IF (ICZERO) 30060, 0060, 30060                                    02850252
 0060 CONTINUE                                                          02860252
      IVCOMP = 0                                                        02870252
      IVCORR = 1                                                        02880252
      ASSIGN 99999 TO J                                                 02890252
      GO TO 0064                                                        02900252
 0062 IVCOMP = 0                                                        02910252
      GO TO 40060                                                       02920252
99999 IVCOMP = 1                                                        02930252
      GO TO 40060                                                       02940252
 0063 IVCOMP = 0                                                        02950252
      GO TO 40060                                                       02960252
 0064 GO TO J, ( 0062, 99999, 0063 )                                    02970252
40060 IF ( IVCOMP - 1 )  20060, 10060, 20060                            02980252
30060 IVDELE = IVDELE + 1                                               02990252
      WRITE (I02,80000) IVTNUM                                          03000252
      IF (ICZERO) 10060, 0071, 20060                                    03010252
10060 IVPASS = IVPASS + 1                                               03020252
      WRITE (I02,80002) IVTNUM                                          03030252
      GO TO 0071                                                        03040252
20060 IVFAIL = IVFAIL + 1                                               03050252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03060252
 0071 CONTINUE                                                          03070252
C                                                                       03080252
C     ****  FCVS PROGRAM 252  -  TEST 007  ****                         03090252
C                                                                       03100252
C        TEST 007 IS A SYNTAX CHECK ON THE OPTIONAL COMMA IN THE        03110252
C     COMPUTED GO TO STATEMENT.  THE COMMA FOLLOWING THE PARENTHESIZED  03120252
C     LIST OF STATEMENT LABELS IS INTENTIONALLY OMITTED.                03130252
C                  GO TO ( S1, S2, S3 )  I                              03140252
C                                                                       03150252
C                                                                       03160252
      IVTNUM =   7                                                      03170252
      IF (ICZERO) 30070, 0070, 30070                                    03180252
 0070 CONTINUE                                                          03190252
      IVCOMP = 0                                                        03200252
      IVCORR = 1                                                        03210252
      I = 3                                                             03220252
      GO TO 0075                                                        03230252
 0072 IVCOMP = 0                                                        03240252
      I = 1                                                             03250252
      GO TO 0075                                                        03260252
 0073 IVCOMP = 1                                                        03270252
      GO TO 40070                                                       03280252
 0074 IVCOMP = 0                                                        03290252
      I = 2                                                             03300252
      GO TO 0075                                                        03310252
 0075 GO TO ( 0074, 0073, 0072 )  I                                     03320252
40070 IF ( I - 2 )  20070, 40071, 20070                                 03330252
40071 IF ( IVCOMP - 1 )  20070, 10070, 20070                            03340252
30070 IVDELE = IVDELE + 1                                               03350252
      WRITE (I02,80000) IVTNUM                                          03360252
      IF (ICZERO) 10070, 0081, 20070                                    03370252
10070 IVPASS = IVPASS + 1                                               03380252
      WRITE (I02,80002) IVTNUM                                          03390252
      GO TO 0081                                                        03400252
20070 IVFAIL = IVFAIL + 1                                               03410252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03420252
 0081 CONTINUE                                                          03430252
C                                                                       03440252
C     ****  FCVS PROGRAM 252  -  TEST 008  ****                         03450252
C                                                                       03460252
C        TEST 008 USES THE COMPUTED GO TO WITHOUT THE OPTIONAL COMMA    03470252
C     AND HAS A SINGLE STATEMENT LABEL IN THE PARENTHESIZED LIST OF     03480252
C     STATEMENT LABELS.                                                 03490252
C                  GO TO ( S1 ) I                                       03500252
C                                                                       03510252
C                                                                       03520252
      IVTNUM =   8                                                      03530252
      IF (ICZERO) 30080, 0080, 30080                                    03540252
 0080 CONTINUE                                                          03550252
      IVCOMP = 0                                                        03560252
      IVCORR = 1                                                        03570252
      J = 1                                                             03580252
      GO TO 0083                                                        03590252
 0082 IVCOMP = 1                                                        03600252
      GO TO 40080                                                       03610252
 0083 GO TO ( 0082 ) J                                                  03620252
40080 IF ( IVCOMP - 1 )  20080, 10080, 20080                            03630252
30080 IVDELE = IVDELE + 1                                               03640252
      WRITE (I02,80000) IVTNUM                                          03650252
      IF (ICZERO) 10080, 0091, 20080                                    03660252
10080 IVPASS = IVPASS + 1                                               03670252
      WRITE (I02,80002) IVTNUM                                          03680252
      GO TO 0091                                                        03690252
20080 IVFAIL = IVFAIL + 1                                               03700252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03710252
 0091 CONTINUE                                                          03720252
C                                                                       03730252
C                                                                       03740252
C        THE NEXT THREE TESTS OF THE COMPUTED GO TO TEST THE RANGE OF   03750252
C     THE INDEX.                                                        03760252
C                                                                       03770252
C        FORTRAN 77 HAS THE REQUIREMENT IN SECTION 11.2 - IF THE INDEX  03780252
C     IS LESS THAN ONE OR GREATER THAN THE NUMBER OF STATEMENT LABELS IN03790252
C     THE PARENTHESIZED LIST, THE EXECUTION SEQUENCE CONTINUES AS THOUGH03800252
C     A  CONTINUE  STATEMENT WERE EXECUTED.                             03810252
C                                                                       03820252
C                                                                       03830252
C                                                                       03840252
C     ****  FCVS PROGRAM 252  -  TEST 009  ****                         03850252
C                                                                       03860252
C                                                                       03870252
C        TEST 009 USES A VALUE OF THE INDEX OF THE COMPUTED GO TO       03880252
C     STATEMENT GREATER THAN THE NUMBER OF STATEMENT LABELS IN THE      03890252
C     PARENTHESIZED LIST.                                               03900252
C                                                                       03910252
C                                                                       03920252
      IVTNUM =   9                                                      03930252
      IF (ICZERO) 30090, 0090, 30090                                    03940252
 0090 CONTINUE                                                          03950252
      IVCOMP = 0                                                        03960252
      IVCORR = 1                                                        03970252
      K = 3                                                             03980252
      GO TO 0094                                                        03990252
 0092 IVCOMP = 0                                                        04000252
      GO TO 40090                                                       04010252
 0093 IVCOMP = 0                                                        04020252
      GO TO 40090                                                       04030252
 0094 GO TO ( 0092, 0093 )  K                                           04040252
C                                                                       04050252
C        TO REACH THIS STATEMENT THE COMPUTED GO TO WILL HAVE TO BE     04060252
C     EXECUTED AS IF IT WERE A CONTINUE STATEMENT.                      04070252
C                                                                       04080252
      IVCOMP = 1                                                        04090252
40090 IF ( IVCOMP - 1 )  20090, 10090, 20090                            04100252
30090 IVDELE = IVDELE + 1                                               04110252
      WRITE (I02,80000) IVTNUM                                          04120252
      IF (ICZERO) 10090, 0101, 20090                                    04130252
10090 IVPASS = IVPASS + 1                                               04140252
      WRITE (I02,80002) IVTNUM                                          04150252
      GO TO 0101                                                        04160252
20090 IVFAIL = IVFAIL + 1                                               04170252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04180252
 0101 CONTINUE                                                          04190252
C                                                                       04200252
C     ****  FCVS PROGRAM 252  -  TEST 010  ****                         04210252
C                                                                       04220252
C        TEST 010 USES A VALUE OF THE INDEX OF THE COMPUTED GO TO       04230252
C     STATEMENT EQUAL TO ZERO.                                          04240252
C                                                                       04250252
C                                                                       04260252
      IVTNUM =  10                                                      04270252
      IF (ICZERO) 30100, 0100, 30100                                    04280252
 0100 CONTINUE                                                          04290252
      IVCOMP = 0                                                        04300252
      IVCORR = 1                                                        04310252
      I = 0                                                             04320252
      GO TO 0104                                                        04330252
 0102 IVCOMP = 0                                                        04340252
      GO TO 40100                                                       04350252
 0103 IVCOMP = 0                                                        04360252
      GO TO 40100                                                       04370252
 0104 GO TO ( 0103, 0102 ), I                                           04380252
C                                                                       04390252
C        THIS STATEMENT CAN ONLY BE REACHED IF THE COMPUTED GO TO       04400252
C     IS EXECUTED AS IF IT WERE A CONTINUE STATEMENT.                   04410252
C                                                                       04420252
      IVCOMP = 1                                                        04430252
40100 IF ( IVCOMP - 1 )  20100, 10100, 20100                            04440252
30100 IVDELE = IVDELE + 1                                               04450252
      WRITE (I02,80000) IVTNUM                                          04460252
      IF (ICZERO) 10100, 0111, 20100                                    04470252
10100 IVPASS = IVPASS + 1                                               04480252
      WRITE (I02,80002) IVTNUM                                          04490252
      GO TO 0111                                                        04500252
20100 IVFAIL = IVFAIL + 1                                               04510252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04520252
 0111 CONTINUE                                                          04530252
C                                                                       04540252
C     ****  FCVS PROGRAM 252  -  TEST 011  ****                         04550252
C                                                                       04560252
C        TEST 011 USES A VALUE OF THE INDEX OF THE COMPUTED GO TO       04570252
C     EQUAL TO -1.                                                      04580252
C                                                                       04590252
      IVTNUM =  11                                                      04600252
      IF (ICZERO) 30110, 0110, 30110                                    04610252
 0110 CONTINUE                                                          04620252
      IVCOMP = 0                                                        04630252
      IVCORR = 1                                                        04640252
      J = -1                                                            04650252
      GO TO 0114                                                        04660252
 0112 IVCOMP = 0                                                        04670252
      GO TO 40110                                                       04680252
 0113 IVCOMP = 0                                                        04690252
      GO TO 40110                                                       04700252
 0114 GO TO  (0112,0113),J                                              04710252
C                                                                       04720252
C        THIS STATEMENT CAN ONLY BE REACHED IF THE COMPUTED GO TO       04730252
C     IS EXECUTED AS IF IT WERE A CONTINUE STATEMENT.                   04740252
C                                                                       04750252
      IVCOMP = 1                                                        04760252
40110 IF ( IVCOMP - 1 )  20110, 10110, 20110                            04770252
30110 IVDELE = IVDELE + 1                                               04780252
      WRITE (I02,80000) IVTNUM                                          04790252
      IF (ICZERO) 10110, 0121, 20110                                    04800252
10110 IVPASS = IVPASS + 1                                               04810252
      WRITE (I02,80002) IVTNUM                                          04820252
      GO TO 0121                                                        04830252
20110 IVFAIL = IVFAIL + 1                                               04840252
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04850252
 0121 CONTINUE                                                          04860252
C                                                                       04870252
C                                                                       04880252
C     WRITE OUT TEST SUMMARY                                            04890252
C                                                                       04900252
      WRITE (I02,90004)                                                 04910252
      WRITE (I02,90014)                                                 04920252
      WRITE (I02,90004)                                                 04930252
      WRITE (I02,90000)                                                 04940252
      WRITE (I02,90004)                                                 04950252
      WRITE (I02,90020) IVFAIL                                          04960252
      WRITE (I02,90022) IVPASS                                          04970252
      WRITE (I02,90024) IVDELE                                          04980252
      STOP                                                              04990252
90001 FORMAT (1H ,24X,5HFM252)                                          05000252
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM252)                          05010252
C                                                                       05020252
C     FORMATS FOR TEST DETAIL LINES                                     05030252
C                                                                       05040252
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   05050252
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      05060252
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         05070252
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    05080252
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        05090252
C                                                                       05100252
C     FORMAT STATEMENTS FOR PAGE HEADERS                                05110252
C                                                                       05120252
90002 FORMAT (1H1)                                                      05130252
90004 FORMAT (1H )                                                      05140252
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05150252
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   05160252
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         05170252
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  05180252
90014 FORMAT (1H ,5X,46H----------------------------------------------) 05190252
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             05200252
C                                                                       05210252
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 05220252
C                                                                       05230252
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              05240252
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              05250252
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             05260252
      END                                                               05270252
