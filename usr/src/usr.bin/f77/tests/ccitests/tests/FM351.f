      PROGRAM FM351                                                     00010351
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020351
C                                                                       00030351
C          THIS PROGRAM CONTAINS TESTS FOR COMPOUND ARITHMETIC          00040351
C     EXPRESSIONS WHICH NECESSITATE THE APPLICATION OF THE RULES        00050351
C     FOR ARITHMETIC OPERATOR PRECEDENCE.  THESE TESTS INCLUDE ONES     00060351
C     WHICH EXERCIZE THE                                                00070351
C                                                                       00080351
C     (1)  USE OF ALL ARITHMETIC OPERATOR TYPES IN THE SAME STATEMENT.  00090351
C     (2)  USE OF PARENTHESES TO OVERRIDE DEFAULT PRECEDENCES.          00100351
C     (3)  USE OF ALL CLASSES OF PRIMARY OPERANDS.                      00110351
C     (4)  USE OF NESTED FUNCTION REFERENCES.                           00120351
C     (5)  USE OF MIXED DATA TYPES.                                     00130351
C                                                                       00140351
C     REFERENCES -                                                      00150351
C                                                                       00160351
C     AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN, X3.9-197700170351
C                                                                       00180351
C          SECTION 6.1  ARITHMETIC EXPRESSIONS                          00190351
C          SECTION 6.5  PRECEDENCE OF OPERATORS                         00200351
C          SECTION 6.6  EVALUATION OF EXPRESSIONS                       00210351
C                                                                       00220351
C                                                                       00230351
C     ******************************************************************00240351
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00250351
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00260351
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00270351
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00280351
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00290351
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00300351
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00310351
C     THE RESULT OF EXECUTING THESE TESTS.                              00320351
C                                                                       00330351
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00340351
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00350351
C                                                                       00360351
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00370351
C                    DEPARTMENT OF THE NAVY                             00380351
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00390351
C                    WASHINGTON, D.C.   20376                           00400351
C                                                                       00410351
C     ******************************************************************00420351
C                                                                       00430351
C                                                                       00440351
      IMPLICIT LOGICAL (L)                                              00450351
      IMPLICIT CHARACTER*14 (C)                                         00460351
C                                                                       00470351
      DIMENSION IADN11(5), RADN11(5)                                    00480351
      IFOS01(IDON01,IDON02,IDON03) = IDON01 ** IDON02 ** IDON03         00490351
      IFOS02(IDON04,IDON05) = IADN11(IDON04) / IADN11(IDON05)           00500351
      IFOS04(IDON09,IDON10) = IADN11(IDON09) + IABS(IDON10)             00510351
      IFOS03(IDON06,IDON07,IDON08) = IFOS04(IDON06,IDON07) * IDON08     00520351
      RFOS01(RDON01,RDON02,RDON03) = RDON01 ** RDON02 ** RDON03         00530351
      RFOS02(IDON11,IDON12) = RADN11(IDON11) / RADN11(IDON12)           00540351
      RFOS04(IDON13,RDON10) = RADN11(IDON13) + ABS(RDON10)              00550351
      RFOS03(RDON06,RDON07,RDON08) = RFOS04(INT(RDON06),RDON07) * RDON0800560351
      IFOS05(IDON14,IDON16) = RADN11(IDON14) + IABS(IDON16)             00570351
      RFOS06(RDON17,IDON18,RDON19) = IFOS05(INT(RDON17),IDON18) * RDON1900580351
C                                                                       00590351
C                                                                       00600351
C                                                                       00610351
C     INITIALIZATION SECTION.                                           00620351
C                                                                       00630351
C     INITIALIZE CONSTANTS                                              00640351
C     ********************                                              00650351
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00660351
      I01 = 5                                                           00670351
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00680351
      I02 = 6                                                           00690351
C     SYSTEM ENVIRONMENT SECTION                                        00700351
C                                                                       00710351
      I01 = 5                                                           00720351
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00730351
C     (UNIT NUMBER FOR CARD READER).                                    00740351
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00750351
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760351
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00770351
C                                                                       00780351
      I02 = 6                                                           00790351
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00800351
C     (UNIT NUMBER FOR PRINTER).                                        00810351
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00820351
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00830351
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00840351
C                                                                       00850351
      IVPASS = 0                                                        00860351
      IVFAIL = 0                                                        00870351
      IVDELE = 0                                                        00880351
      ICZERO = 0                                                        00890351
C                                                                       00900351
C     WRITE OUT PAGE HEADERS                                            00910351
C                                                                       00920351
      WRITE (I02,90002)                                                 00930351
      WRITE (I02,90006)                                                 00940351
      WRITE (I02,90008)                                                 00950351
      WRITE (I02,90004)                                                 00960351
      WRITE (I02,90010)                                                 00970351
      WRITE (I02,90004)                                                 00980351
      WRITE (I02,90016)                                                 00990351
      WRITE (I02,90001)                                                 01000351
      WRITE (I02,90004)                                                 01010351
      WRITE (I02,90012)                                                 01020351
      WRITE (I02,90014)                                                 01030351
      WRITE (I02,90004)                                                 01040351
C                                                                       01050351
C                                                                       01060351
C          TESTS 1 THROUGH 10 DEAL ENTIRELY WITH INTEGER EXPRESSIONS.   01070351
C                                                                       01080351
C                                                                       01090351
C     ****  FCVS PROGRAM 351  -  TEST 001  ****                         01100351
C                                                                       01110351
C     TEST 1 CHECKS AN INTEGER EXPRESSION WHERE ALL FIVE ARITHMETIC     01120351
C     OPERATORS ARE USED AND ALL OPERAND PRIMARIES ARE SIMPLE INTEGER   01130351
C     VARIABLES.  NO PARENTHESES ARE USED TO UPSET DEFAULT PRECEDENCES. 01140351
C                                                                       01150351
      IVTNUM =   1                                                      01160351
      IF (ICZERO) 30010, 0010, 30010                                    01170351
 0010 CONTINUE                                                          01180351
      IVON01 = 7                                                        01190351
      IVON02 = 3                                                        01200351
      IVON03 = 573                                                      01210351
      IVON04 = 23                                                       01220351
      IVON05 = 3                                                        01230351
      IVON06 = -7                                                       01240351
      IVCOMP = IVON01 ** IVON02 + IVON03 - IVON04 * IVON05 / IVON06     01250351
      IVCORR = 925                                                      01260351
40010 IF (IVCOMP - 925) 20010, 10010, 20010                             01270351
30010 IVDELE = IVDELE + 1                                               01280351
      WRITE (I02,80000) IVTNUM                                          01290351
      IF (ICZERO) 10010, 0021, 20010                                    01300351
10010 IVPASS = IVPASS + 1                                               01310351
      WRITE (I02,80002) IVTNUM                                          01320351
      GO TO 0021                                                        01330351
20010 IVFAIL = IVFAIL + 1                                               01340351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01350351
 0021 CONTINUE                                                          01360351
C                                                                       01370351
C     ****  FCVS PROGRAM 351  -  TEST 002  ****                         01380351
C                                                                       01390351
C          TEST 2, LIKE TEST 1, CHECKS AN INTEGER EXPRESSION WHERE ALL  01400351
C     FIVE ARITHMETIC OPERATORS ARE USED AND ALL OPERANDS ARE SIMPLE    01410351
C     INTEGER VARIABLES; BUT IN THIS TEST, PARENTHESES ARE USED, AS IS  01420351
C     A UNARY OPERATOR.                                                 01430351
C                                                                       01440351
      IVTNUM =   2                                                      01450351
      IF (ICZERO) 30020, 0020, 30020                                    01460351
 0020 CONTINUE                                                          01470351
      IVON01 = 7                                                        01480351
      IVON02 = 3                                                        01490351
      IVON03 = 5                                                        01500351
      IVON04 = -3                                                       01510351
      IVON05 = 3                                                        01520351
      IVCOMP = -(IVON01 / IVON02) + (IVON03 * IVON04 ** IVON05)         01530351
      IVCORR = -137                                                     01540351
40020 IF (IVCOMP + 137) 20020, 10020, 20020                             01550351
30020 IVDELE = IVDELE + 1                                               01560351
      WRITE (I02,80000) IVTNUM                                          01570351
      IF (ICZERO) 10020, 0031, 20020                                    01580351
10020 IVPASS = IVPASS + 1                                               01590351
      WRITE (I02,80002) IVTNUM                                          01600351
      GO TO 0031                                                        01610351
20020 IVFAIL = IVFAIL + 1                                               01620351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01630351
 0031 CONTINUE                                                          01640351
C                                                                       01650351
C     ****  FCVS PROGRAM 351  -  TEST 003  ****                         01660351
C                                                                       01670351
C          TEST 3 IS SIMILAR TO TEST 2 EXCEPT THAT IT EMPLOYS NESTED    01680351
C     PARENTHESES.                                                      01690351
C                                                                       01700351
      IVTNUM =   3                                                      01710351
      IF (ICZERO) 30030, 0030, 30030                                    01720351
 0030 CONTINUE                                                          01730351
      IVON01 = 5                                                        01740351
      IVON02 = 3                                                        01750351
      IVON03 = 5                                                        01760351
      IVON04 = 17                                                       01770351
      IVON05 = 14                                                       01780351
      IVON06 = 3                                                        01790351
      IVCOMP = IVON01 ** (-(IVON02 + (IVON03 - IVON04)) - (IVON05 /     01800351
     1         IVON06))                                                 01810351
      IVCORR = 3125                                                     01820351
40030 IF (IVCOMP - 3125) 20030, 10030, 20030                            01830351
30030 IVDELE = IVDELE + 1                                               01840351
      WRITE (I02,80000) IVTNUM                                          01850351
      IF (ICZERO) 10030, 0041, 20030                                    01860351
10030 IVPASS = IVPASS + 1                                               01870351
      WRITE (I02,80002) IVTNUM                                          01880351
      GO TO 0041                                                        01890351
20030 IVFAIL = IVFAIL + 1                                               01900351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01910351
 0041 CONTINUE                                                          01920351
C                                                                       01930351
C     ****  FCVS PROGRAM 351  -  TEST 004  ****                         01940351
C                                                                       01950351
C          TEST 4 IS SIMILAR TO TEST 2 AND 3 EXCEPT THAT THE            01960351
C     PARENTHESES USED ARE EFFECTIVELY EXTRANEOUS.                      01970351
C                                                                       01980351
      IVTNUM =   4                                                      01990351
      IF (ICZERO) 30040, 0040, 30040                                    02000351
 0040 CONTINUE                                                          02010351
      IVON01 = 3                                                        02020351
      IVON02 = 4                                                        02030351
      IVON03 = 5                                                        02040351
      IVON04 = 2                                                        02050351
      IVON05 = 3                                                        02060351
      IVON06 = 4                                                        02070351
      IVCOMP = ((IVON01) ** (IVON02) + (IVON03) - (IVON04) *            02080351
     1         (IVON05) / (IVON06))                                     02090351
      IVCORR = 85                                                       02100351
40040 IF (IVCOMP - 85) 20040, 10040, 20040                              02110351
30040 IVDELE = IVDELE + 1                                               02120351
      WRITE (I02,80000) IVTNUM                                          02130351
      IF (ICZERO) 10040, 0051, 20040                                    02140351
10040 IVPASS = IVPASS + 1                                               02150351
      WRITE (I02,80002) IVTNUM                                          02160351
      GO TO 0051                                                        02170351
20040 IVFAIL = IVFAIL + 1                                               02180351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02190351
 0051 CONTINUE                                                          02200351
C                                                                       02210351
C     ****  FCVS PROGRAM 351  -  TEST 005  ****                         02220351
C                                                                       02230351
C          TEST 5 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY       02240351
C     INTEGER VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS,      02250351
C     AND USING PARENTHESES TO OVERRIDE PRECEDENCES.                    02260351
C                                                                       02270351
      IVTNUM =   5                                                      02280351
      IF (ICZERO) 30050, 0050, 30050                                    02290351
 0050 CONTINUE                                                          02300351
      IVON01 = 57                                                       02310351
      IVON02 = -3                                                       02320351
      IVON03 = 4                                                        02330351
      IVON04 = -1                                                       02340351
      IVON05 = -5                                                       02350351
      IVON06 = -2                                                       02360351
      IVCOMP = -IVON01 ** (IVON02 + IVON03 - IVON04) *                  02370351
     1         (IVON05 / IVON06)                                        02380351
      IVCORR = -6498                                                    02390351
40050 IF (IVCOMP + 6498) 20050, 10050, 20050                            02400351
30050 IVDELE = IVDELE + 1                                               02410351
      WRITE (I02,80000) IVTNUM                                          02420351
      IF (ICZERO) 10050, 0061, 20050                                    02430351
10050 IVPASS = IVPASS + 1                                               02440351
      WRITE (I02,80002) IVTNUM                                          02450351
      GO TO 0061                                                        02460351
20050 IVFAIL = IVFAIL + 1                                               02470351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02480351
 0061 CONTINUE                                                          02490351
C                                                                       02500351
C     ****  FCVS PROGRAM 351  -  TEST 006  ****                         02510351
C                                                                       02520351
C          TEST 6 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY       02530351
C     INTEGER VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS,      02540351
C     AND USING PARENTHESES TO OVERRIDE PRECEDENCES.                    02550351
C                                                                       02560351
      IVTNUM =   6                                                      02570351
      IF (ICZERO) 30060, 0060, 30060                                    02580351
 0060 CONTINUE                                                          02590351
      IVON01 = 5                                                        02600351
      IVON02 = 3                                                        02610351
      IVON03 = 4                                                        02620351
      IVON04 = 5496                                                     02630351
      IVON05 = 7                                                        02640351
      IVON06 = -3                                                       02650351
      IVCOMP = ((IVON01 * (IVON02 / IVON03)) + IVON04) / IVON05 -       02660351
     1         (-IVON06)                                                02670351
      IVCORR = 782                                                      02680351
40060 IF (IVCOMP - 782) 20060, 10060, 20060                             02690351
30060 IVDELE = IVDELE + 1                                               02700351
      WRITE (I02,80000) IVTNUM                                          02710351
      IF (ICZERO) 10060, 0071, 20060                                    02720351
10060 IVPASS = IVPASS + 1                                               02730351
      WRITE (I02,80002) IVTNUM                                          02740351
      GO TO 0071                                                        02750351
20060 IVFAIL = IVFAIL + 1                                               02760351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02770351
 0071 CONTINUE                                                          02780351
C                                                                       02790351
C     ****  FCVS PROGRAM 351  -  TEST 007  ****                         02800351
C                                                                       02810351
C          IN TEST 7, AN INTEGER EXPRESSION INVOLVING ALL FIVE          02820351
C     ARITHMETIC OPERATORS TOGETHER WITH PARENTHESES IS EVALUATED,      02830351
C     BUT UNLIKE TESTS 1 THROUGH 6 WHERE ALL OPERANDS WERE INTEGER      02840351
C     VARIABLES, THE OPERANDS IN TEST 7 ARE CLASSED AS INTEGER          02850351
C     VARIABLES, INTEGER CONSTANTS, INTEGER ARRAY ELEMENTS, AND INTEGER 02860351
C     FUNCTION REFERENCES.                                              02870351
C                                                                       02880351
      IVTNUM =   7                                                      02890351
      IF (ICZERO) 30070, 0070, 30070                                    02900351
 0070 CONTINUE                                                          02910351
      IVON01 = 573                                                      02920351
      IVON02 = 1                                                        02930351
      IVON03 = 3                                                        02940351
      IVON04 = 2                                                        02950351
      IVON05 = 3                                                        02960351
      IADN11(3) = 3071                                                  02970351
      IVCOMP = (IVON01 + 1) - (5 + IADN11(IVON03)) /                    02980351
     1         (IFOS01(IVON03,IVON04,IVON05) ** IVON02)                 02990351
      IVCORR = 574                                                      03000351
40070 IF (IVCOMP - 574) 20070, 10070, 20070                             03010351
30070 IVDELE = IVDELE + 1                                               03020351
      WRITE (I02,80000) IVTNUM                                          03030351
      IF (ICZERO) 10070, 0081, 20070                                    03040351
10070 IVPASS = IVPASS + 1                                               03050351
      WRITE (I02,80002) IVTNUM                                          03060351
      GO TO 0081                                                        03070351
20070 IVFAIL = IVFAIL + 1                                               03080351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03090351
 0081 CONTINUE                                                          03100351
C                                                                       03110351
C     ****  FCVS PROGRAM 351  -  TEST 008  ****                         03120351
C                                                                       03130351
C          TEST 8 IS IDENTICAL TO TEST 7 EXCEPT THAT PARENTHESES ARE    03140351
C     USED TO CHANGE THE ORDER OF SUB-EXPRESSION EVALUATION.            03150351
C                                                                       03160351
      IVTNUM =   8                                                      03170351
      IF (ICZERO) 30080, 0080, 30080                                    03180351
 0080 CONTINUE                                                          03190351
      IVON01 = 573                                                      03200351
      IVON02 = 1                                                        03210351
      IVON03 = 3                                                        03220351
      IVON04 = 2                                                        03230351
      IVON05 = 3                                                        03240351
      IADN11(3) = 3071                                                  03250351
      IVCOMP = ((IVON01 + 1) - (5 + IADN11(IVON03))) /                  03260351
     1         IFOS01(IVON03,IVON04,IVON05) ** IVON02                   03270351
      IVCORR = 0                                                        03280351
40080 IF (IVCOMP) 20080, 10080, 20080                                   03290351
30080 IVDELE = IVDELE + 1                                               03300351
      WRITE (I02,80000) IVTNUM                                          03310351
      IF (ICZERO) 10080, 0091, 20080                                    03320351
10080 IVPASS = IVPASS + 1                                               03330351
      WRITE (I02,80002) IVTNUM                                          03340351
      GO TO 0091                                                        03350351
20080 IVFAIL = IVFAIL + 1                                               03360351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03370351
 0091 CONTINUE                                                          03380351
C                                                                       03390351
C     ****  FCVS PROGRAM 351  -  TEST 009  ****                         03400351
C                                                                       03410351
C          TEST 9 IS SIMILAR TO TESTS 7 AND 8 EXCEPT THAT THE           03420351
C     FUNCTION REFERENCE IN TURN EVALUATES ARRAY ELEMENTS.              03430351
C                                                                       03440351
      IVTNUM =   9                                                      03450351
      IF (ICZERO) 30090, 0090, 30090                                    03460351
 0090 CONTINUE                                                          03470351
      IVON01 = 7                                                        03480351
      IVON02 = 3                                                        03490351
      IVON03 = 2                                                        03500351
      IVON04 = 1                                                        03510351
      IVON05 = 4                                                        03520351
      IADN11(1) = 5                                                     03530351
      IADN11(2) = 2                                                     03540351
      IADN11(4) = 2                                                     03550351
      IVCOMP = (IVON01 - 8 * IFOS02(IVON04,IVON03)) / IADN11(IVON05) +  03560351
     1         13 ** IVON02                                             03570351
      IVCORR = 2193                                                     03580351
40090 IF (IVCOMP - 2193) 20090, 10090, 20090                            03590351
30090 IVDELE = IVDELE + 1                                               03600351
      WRITE (I02,80000) IVTNUM                                          03610351
      IF (ICZERO) 10090, 0101, 20090                                    03620351
10090 IVPASS = IVPASS + 1                                               03630351
      WRITE (I02,80002) IVTNUM                                          03640351
      GO TO 0101                                                        03650351
20090 IVFAIL = IVFAIL + 1                                               03660351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03670351
 0101 CONTINUE                                                          03680351
C                                                                       03690351
C     ****  FCVS PROGRAM 351  -  TEST 010  ****                         03700351
C                                                                       03710351
C          TEST 10 EVALUATES AN INTEGER EXPRESSION WHICH CONTAINS       03720351
C     FUNCTION REFERENCES NESTED TO THREE LEVELS.  THE OUTER TWO        03730351
C     LEVELS ARE STATEMENT FUNCTION REFERENCES AND THE INNERMOST LEVEL  03740351
C     IS AN INTRINSIC FUNCTION REFERENCE.                               03750351
C                                                                       03760351
      IVTNUM =  10                                                      03770351
      IF (ICZERO) 30100, 0100, 30100                                    03780351
 0100 CONTINUE                                                          03790351
      IVON01 = -51                                                      03800351
      IVON02 = 4                                                        03810351
      IVON03 = -101                                                     03820351
      IVON04 = 13                                                       03830351
      IVON05 = 3                                                        03840351
      IVON06 = 5                                                        03850351
      IVON07 = -37                                                      03860351
      IADN11(4) = 87                                                    03870351
      IADN11(5) = 409                                                   03880351
      IVCOMP = (IVON01 + IFOS03(IVON02,IVON03,IVON04)) * IVON05 -       03890351
     1         IFOS04(IVON06,IVON07)                                    03900351
      IVCORR = 6733                                                     03910351
40100 IF (IVCOMP - 6733) 20100, 10100, 20100                            03920351
30100 IVDELE = IVDELE + 1                                               03930351
      WRITE (I02,80000) IVTNUM                                          03940351
      IF (ICZERO) 10100, 0111, 20100                                    03950351
10100 IVPASS = IVPASS + 1                                               03960351
      WRITE (I02,80002) IVTNUM                                          03970351
      GO TO 0111                                                        03980351
20100 IVFAIL = IVFAIL + 1                                               03990351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04000351
 0111 CONTINUE                                                          04010351
C                                                                       04020351
C          TESTS 11 THROUGH 20 REPEAT TESTS 1 THROUGH 10 EXCEPT THAT    04030351
C     TESTS 11 THROUGH 20 DEAL ENTIRELY WITH REAL ARITHMETIC            04040351
C     EXPRESSIONS.                                                      04050351
C                                                                       04060351
C                                                                       04070351
C     ****  FCVS PROGRAM 351  -  TEST 011  ****                         04080351
C                                                                       04090351
C          TEST 11 TESTS A REAL EXPRESSION WHERE ALL FIVE ARITHMETIC    04100351
C     OPERATORS ARE USED AND ALL OPERAND PRIMARIES ARE SIMPLE REAL      04110351
C     VARIABLES.                                                        04120351
C                                                                       04130351
      IVTNUM =  11                                                      04140351
      IF (ICZERO) 30110, 0110, 30110                                    04150351
 0110 CONTINUE                                                          04160351
      RVON01 = 3.2                                                      04170351
      RVON02 = 23.051                                                   04180351
      RVON03 = 1545 E7                                                  04190351
      RVON04 = -23.457                                                  04200351
      RVON05 = .02 E3                                                   04210351
      RVON06 = 7.210745323 E-10                                         04220351
      RVCOMP = RVON01 ** RVON02 + RVON03 - RVON04 * RVON05 / RVON06     04230351
      RVCORR = 1.10683 E12                                              04240351
40110 IF (RVCOMP - 1.1063 E12)  20110, 10110, 40111                     04250351
40111 IF (RVCOMP - 1.1073 E12)  10110, 10110, 20110                     04260351
30110 IVDELE = IVDELE + 1                                               04270351
      WRITE (I02,80000) IVTNUM                                          04280351
      IF (ICZERO) 10110, 0121, 20110                                    04290351
10110 IVPASS = IVPASS + 1                                               04300351
      WRITE (I02,80002) IVTNUM                                          04310351
      GO TO 0121                                                        04320351
20110 IVFAIL = IVFAIL + 1                                               04330351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04340351
 0121 CONTINUE                                                          04350351
C                                                                       04360351
C     ****  FCVS PROGRAM 351  -  TEST 012  ****                         04370351
C                                                                       04380351
C          TEST 12, LIKE TEST 11, CHECKS A REAL EXPRESSION WHERE ALL    04390351
C     FIVE ARITHMETIC OPERATORS ARE USED AND ALL OPERANDS ARE REAL      04400351
C     VARIABLES, BUT IN TEST 12, PARENTHESES ARE USED, AS IS ALSO A     04410351
C     UNARY OPERATOR.                                                   04420351
C                                                                       04430351
      IVTNUM =  12                                                      04440351
      IF (ICZERO) 30120, 0120, 30120                                    04450351
 0120 CONTINUE                                                          04460351
      RVON01 = 3.2                                                      04470351
      RVON02 = 23.051                                                   04480351
      RVON03 = 1545 E-3                                                 04490351
      RVON04 = 5.75 E-1                                                 04500351
      RVON05 = 2.22 E+1                                                 04510351
      RVCOMP = -(RVON01 / RVON02) + (RVON03 * RVON04 ** RVON05)         04520351
      RVCORR = -.13882                                                  04530351
40120 IF (RVCOMP + .13887) 20120, 10120, 40121                          04540351
40121 IF (RVCOMP + .13877) 10120, 10120, 20120                          04550351
30120 IVDELE = IVDELE + 1                                               04560351
      WRITE (I02,80000) IVTNUM                                          04570351
      IF (ICZERO) 10120, 0131, 20120                                    04580351
10120 IVPASS = IVPASS + 1                                               04590351
      WRITE (I02,80002) IVTNUM                                          04600351
      GO TO 0131                                                        04610351
20120 IVFAIL = IVFAIL + 1                                               04620351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04630351
 0131 CONTINUE                                                          04640351
C                                                                       04650351
C     ****  FCVS PROGRAM 351  -  TEST 013  ****                         04660351
C                                                                       04670351
C          TEST 13 IS SIMILAR TO TEST 12 EXCEPT THAT TEST 13 EMPLOYS    04680351
C     NESTED PARENTHESES.                                               04690351
C                                                                       04700351
      IVTNUM =  13                                                      04710351
      IF (ICZERO) 30130, 0130, 30130                                    04720351
 0130 CONTINUE                                                          04730351
      RVON01 = 3.2                                                      04740351
      RVON02 = -63.051                                                  04750351
      RVON03 = 1545 E-3                                                 04760351
      RVON04 = 5.75 E-1                                                 04770351
      RVON05 = 2.22 E1                                                  04780351
      RVON06 = 0.523                                                    04790351
      RVCOMP = RVON01 ** (-(RVON02 + (RVON03 - RVON04)) -               04800351
     1         (RVON05 / RVON06))                                       04810351
      RVCORR = 8.27757 E9                                               04820351
40130 IF (RVCOMP - 8.2770 E9) 20130, 10130, 40131                       04830351
40131 IF (RVCOMP - 8.2780 E9) 10130, 10130, 20130                       04840351
30130 IVDELE = IVDELE + 1                                               04850351
      WRITE (I02,80000) IVTNUM                                          04860351
      IF (ICZERO) 10130, 0141, 20130                                    04870351
10130 IVPASS = IVPASS + 1                                               04880351
      WRITE (I02,80002) IVTNUM                                          04890351
      GO TO 0141                                                        04900351
20130 IVFAIL = IVFAIL + 1                                               04910351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          04920351
 0141 CONTINUE                                                          04930351
C                                                                       04940351
C     ****  FCVS PROGRAM 351  -  TEST 014  ****                         04950351
C                                                                       04960351
C          TEST 14 IS SIMILAR TO TESTS 12 AND 13 EXCEPT THAT THE        04970351
C     PARENTHESES USED ARE EFFECTIVELY EXTRANEOUS.                      04980351
C                                                                       04990351
      IVTNUM =  14                                                      05000351
      IF (ICZERO) 30140, 0140, 30140                                    05010351
 0140 CONTINUE                                                          05020351
      RVON01 = 5.4515 E18                                               05030351
      RVON02 = .076923                                                  05040351
      RVON03 = 23 E-2                                                   05050351
      RVON04 = 7 E7                                                     05060351
      RVON05 = 45.23 E5                                                 05070351
      RVON06 = 5.65375 E12                                              05080351
      RVCOMP = ((RVON01) ** (RVON02) + (RVON03) - (RVON04) * (RVON05) / 05090351
     1         (RVON06))                                                05100351
      RVCORR = -28.147                                                  05110351
40140 IF (RVCOMP + 28.152) 20140, 10140, 40141                          05120351
40141 IF (RVCOMP + 28.142) 10140, 10140, 20140                          05130351
30140 IVDELE = IVDELE + 1                                               05140351
      WRITE (I02,80000) IVTNUM                                          05150351
      IF (ICZERO) 10140, 0151, 20140                                    05160351
10140 IVPASS = IVPASS + 1                                               05170351
      WRITE (I02,80002) IVTNUM                                          05180351
      GO TO 0151                                                        05190351
20140 IVFAIL = IVFAIL + 1                                               05200351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05210351
 0151 CONTINUE                                                          05220351
C                                                                       05230351
C     ****  FCVS PROGRAM 351  -  TEST 015  ****                         05240351
C                                                                       05250351
C          TEST 15 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY      05260351
C     REAL VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS, AND     05270351
C     USING PARENTHESES TO OVERRIDE PRECEDENCES.                        05280351
C                                                                       05290351
      IVTNUM =  15                                                      05300351
      IF (ICZERO) 30150, 0150, 30150                                    05310351
 0150 CONTINUE                                                          05320351
      RVON01 = .11341 E1                                                05330351
      RVON02 = 7.1417                                                   05340351
      RVON03 = 5.2113 E1                                                05350351
      RVON04 = 10.001                                                   05360351
      RVON05 = 7.241 E5                                                 05370351
      RVON06 = 5.7777 E-3                                               05380351
      RVCOMP = -RVON01 ** (RVON02 + RVON03 - RVON04) * (RVON05 / RVON06)05390351
      RVCORR = -6.1635 E10                                              05400351
40150 IF (RVCOMP + 6.1640 E10) 20150, 10150, 40151                      05410351
40151 IF (RVCOMP + 6.1630 E10) 10150, 10150, 20150                      05420351
30150 IVDELE = IVDELE + 1                                               05430351
      WRITE (I02,80000) IVTNUM                                          05440351
      IF (ICZERO) 10150, 0161, 20150                                    05450351
10150 IVPASS = IVPASS + 1                                               05460351
      WRITE (I02,80002) IVTNUM                                          05470351
      GO TO 0161                                                        05480351
20150 IVFAIL = IVFAIL + 1                                               05490351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05500351
 0161 CONTINUE                                                          05510351
C                                                                       05520351
C     ****  FCVS PROGRAM 351  -  TEST 016  ****                         05530351
C                                                                       05540351
C          TEST 16 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY      05550351
C     REAL VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS, AND     05560351
C     USING PARENTHESES TO OVERRIDE PRECEDENCES.                        05570351
C                                                                       05580351
      IVTNUM =  16                                                      05590351
      IF (ICZERO) 30160, 0160, 30160                                    05600351
 0160 CONTINUE                                                          05610351
      RVON01 = 6.4003 E18                                               05620351
      RVON02 = -3.7717 E-2                                              05630351
      RVON03 = -5.1195 E3                                               05640351
      RVON04 = 1.7521 E14                                               05650351
      RVON05 = 1.0533 E3                                                05660351
      RVON06 = -9.4207 E11                                              05670351
      RVCOMP = ((RVON01 * (RVON02 / RVON03)) + RVON04) / RVON05 -       05680351
     1         (-RVON06)                                                05690351
      RVCORR = -7.3096 E11                                              05700351
40160 IF (RVCOMP + 7.3101 E11) 20160, 10160, 40161                      05710351
40161 IF (RVCOMP + 7.3091 E11) 10160, 10160, 20160                      05720351
30160 IVDELE = IVDELE + 1                                               05730351
      WRITE (I02,80000) IVTNUM                                          05740351
      IF (ICZERO) 10160, 0171, 20160                                    05750351
10160 IVPASS = IVPASS + 1                                               05760351
      WRITE (I02,80002) IVTNUM                                          05770351
      GO TO 0171                                                        05780351
20160 IVFAIL = IVFAIL + 1                                               05790351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          05800351
 0171 CONTINUE                                                          05810351
C                                                                       05820351
C     ****  FCVS PROGRAM 351  -  TEST 017  ****                         05830351
C                                                                       05840351
C          IN TEST 17, A REAL EXPRESSION INVOLVING ALL FIVE ARITHMETIC  05850351
C     OPERATORS IS EVALUATED, BUT UNLIKE TESTS 11 THROUGH 16 WHERE      05860351
C     ALL OPERANDS WERE REAL VARIABLES, THE OPERANDS IN TEST 17 ARE     05870351
C     CLASSED AS REAL VARIABLES, REAL CONSTANTS, REAL ARRAY ELEMENTS,   05880351
C     AND REAL FUNCTION REFERENCES.                                     05890351
C                                                                       05900351
      IVTNUM =  17                                                      05910351
      IF (ICZERO) 30170, 0170, 30170                                    05920351
 0170 CONTINUE                                                          05930351
      RVON01 = 5.247 E10                                                05940351
      IVON01 = 3                                                        05950351
      RVON02 = 1.07 E1                                                  05960351
      RVON03 = 5.23                                                     05970351
      RVON04 = 1.001                                                    05980351
      RVON05 = 1.573                                                    05990351
      RADN11(3) = 0.3947 E18                                            06000351
      RVCOMP = (RVON01 + 3.491 E10) - (4 E17 + RADN11(IVON01)) /        06010351
     1         (RFOS01(RVON03,RVON04,RVON05) ** RVON02)                 06020351
      RVCORR = 7.1526 E10                                               06030351
40170 IF (RVCOMP - 7.1521 E10) 20170, 10170, 40171                      06040351
40171 IF (RVCOMP - 7.1531 E10) 10170, 10170, 20170                      06050351
30170 IVDELE = IVDELE + 1                                               06060351
      WRITE (I02,80000) IVTNUM                                          06070351
      IF (ICZERO) 10170, 0181, 20170                                    06080351
10170 IVPASS = IVPASS + 1                                               06090351
      WRITE (I02,80002) IVTNUM                                          06100351
      GO TO 0181                                                        06110351
20170 IVFAIL = IVFAIL + 1                                               06120351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06130351
 0181 CONTINUE                                                          06140351
C                                                                       06150351
C     ****  FCVS PROGRAM 351  -  TEST 018  ****                         06160351
C                                                                       06170351
C          TEST 18 IS IDENTICAL TO TEST 17 EXCEPT THAT PARENTHESES ARE  06180351
C     USED TO CHANGE THE ORDER OF SUB-EXPRESSION EVALUATION.            06190351
C                                                                       06200351
      IVTNUM =  18                                                      06210351
      IF (ICZERO) 30180, 0180, 30180                                    06220351
 0180 CONTINUE                                                          06230351
      RVON01 = 5.247 E10                                                06240351
      IVON01 = 3                                                        06250351
      RVON02 = 1.07 E1                                                  06260351
      RVON03 = 5.23                                                     06270351
      RVON04 = 1.001                                                    06280351
      RVON05 = 1.573                                                    06290351
      RADN11(3) = 0.3947 E18                                            06300351
      RVCOMP = ((RVON01 + 3.491 E10) - (4 E17 + RADN11(IVON01))) /      06310351
     1         RFOS01(RVON03,RVON04,RVON05) ** RVON02                   06320351
      RVCORR = -1.5854 E10                                              06330351
40180 IF (RVCOMP + 1.5859 E10) 20180, 10180, 40181                      06340351
40181 IF (RVCOMP + 1.5849 E10) 10180, 10180, 20180                      06350351
30180 IVDELE = IVDELE + 1                                               06360351
      WRITE (I02,80000) IVTNUM                                          06370351
      IF (ICZERO) 10180, 0191, 20180                                    06380351
10180 IVPASS = IVPASS + 1                                               06390351
      WRITE (I02,80002) IVTNUM                                          06400351
      GO TO 0191                                                        06410351
20180 IVFAIL = IVFAIL + 1                                               06420351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06430351
 0191 CONTINUE                                                          06440351
C                                                                       06450351
C     ****  FCVS PROGRAM 351  -  TEST 019  ****                         06460351
C                                                                       06470351
C          TEST 19 IS SIMILAR TO TESTS 17 AND 18 EXCEPT THAT THE        06480351
C     FUNCTION REFERENCES IN TURN EVALUATE ARRAY ELEMENTS.              06490351
C                                                                       06500351
      IVTNUM =  19                                                      06510351
      IF (ICZERO) 30190, 0190, 30190                                    06520351
 0190 CONTINUE                                                          06530351
      RVON01 = 5.026 E2                                                 06540351
      RVON02 = 1.386 E1                                                 06550351
      IVON03 = 2                                                        06560351
      RVON04 = 1.9999                                                   06570351
      RVON05 = 4.0127                                                   06580351
      RADN11(1) = 3.004 E18                                             06590351
      RADN11(2) = 2.5705 E-1                                            06600351
      RADN11(4) = 7.993 E16                                             06610351
      RVCOMP = (RVON01 - 5.902 * RFOS02(INT(RVON04),INT(RVON05))) /     06620351
     1         RADN11(IVON03) + 1.5372 ** RVON02                        06630351
      RVCORR = 1.4797 E3                                                06640351
40190 IF (RVCORR - 1.4792 E3) 20190, 10190, 40191                       06650351
40191 IF (RVCORR - 1.4802 E3) 10190, 10190, 20190                       06660351
30190 IVDELE = IVDELE + 1                                               06670351
      WRITE (I02,80000) IVTNUM                                          06680351
      IF (ICZERO) 10190, 0201, 20190                                    06690351
10190 IVPASS = IVPASS + 1                                               06700351
      WRITE (I02,80002) IVTNUM                                          06710351
      GO TO 0201                                                        06720351
20190 IVFAIL = IVFAIL + 1                                               06730351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          06740351
 0201 CONTINUE                                                          06750351
C                                                                       06760351
C     ****  FCVS PROGRAM 351  -  TEST 020  ****                         06770351
C                                                                       06780351
C          TEST 20 EVALUATES A REAL EXPRESSION WHICH CONTAINS FUNCTION  06790351
C     REFERENCES NESTED TO THREE LEVELS.  THE OUTER TWO LEVELS ARE      06800351
C     STATEMENT FUNCTION REFERENCES AND THE INNERMOST LEVEL IS AN       06810351
C     INTRINSIC FUNCTION REFERENCE.                                     06820351
C                                                                       06830351
      IVTNUM =  20                                                      06840351
      IF (ICZERO) 30200, 0200, 30200                                    06850351
 0200 CONTINUE                                                          06860351
      RVON01 = 4.7117 E05                                               06870351
      RVON02 = 5.987                                                    06880351
      RVON03 = 2.00000 E5                                               06890351
      RVON04 = 1.0 E2                                                   06900351
      RVON05 = 1.5222 E9                                                06910351
      IVON06 = 4                                                        06920351
      RVON07 = -3.2107 E14                                              06930351
      RADN11(4) = 7.425 E14                                             06940351
      RADN11(5) = -2.4015 E5                                            06950351
      RVCOMP = (RVON01 + RFOS03(RVON02,RVON03,RVON04)) * RVON05 -       06960351
     1         RFOS04(IVON06,RVON07)                                    06970351
      RVCORR = -6.4580 E15                                              06980351
40200 IF (RVCOMP + 6.4585 E15) 20200, 10200, 40201                      06990351
40201 IF (RVCOMP + 6.4575 E15) 10200, 10200, 20200                      07000351
30200 IVDELE = IVDELE + 1                                               07010351
      WRITE (I02,80000) IVTNUM                                          07020351
      IF (ICZERO) 10200, 0211, 20200                                    07030351
10200 IVPASS = IVPASS + 1                                               07040351
      WRITE (I02,80002) IVTNUM                                          07050351
      GO TO 0211                                                        07060351
20200 IVFAIL = IVFAIL + 1                                               07070351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07080351
 0211 CONTINUE                                                          07090351
C                                                                       07100351
C          TESTS 21 THROUGH 25 DEAL WITH MIXTURES OF REAL AND INTEGER   07110351
C     EXPRESSIONS; I.E., THESE ARE TESTS WHICH EVALUATE EXPRESSIONS     07120351
C     CONTAINING BOTH REAL SUB-EXPRESSIONS AND INTEGER SUB-EXPRESSIONS  07130351
C     AND THEN ASSIGN THE RESULTS TO EITHER AN INTEGER OR A REAL        07140351
C     VARIABLE.                                                         07150351
C                                                                       07160351
C                                                                       07170351
C     ****  FCVS PROGRAM 351  -  TEST 021  ****                         07180351
C                                                                       07190351
C          TEST 21 USES ALL FIVE ARITHMETIC OPERATORS AND A COMBINATION 07200351
C     OF INTEGER AND REAL VARIABLES.  NO PARENTHESES ARE USED.  FINAL   07210351
C     ASSIGNMENT IS TO AN INTEGER VARIABLE.                             07220351
C                                                                       07230351
      IVTNUM =  21                                                      07240351
      IF (ICZERO) 30210, 0210, 30210                                    07250351
 0210 CONTINUE                                                          07260351
      IVON01 = 17                                                       07270351
      IVON02 = 3                                                        07280351
      RVON03 = 5.4732 E+2                                               07290351
      RVON04 = 1.523                                                    07300351
      IVON05 = 798                                                      07310351
      IVCOMP = IVON01 ** IVON02 + RVON03 - RVON04 * IVON05 / IVON01     07320351
      IVCORR = 5388                                                     07330351
40210 IF (IVCOMP - 5388) 20210, 10210, 20210                            07340351
30210 IVDELE = IVDELE + 1                                               07350351
      WRITE (I02,80000) IVTNUM                                          07360351
      IF (ICZERO) 10210, 0221, 20210                                    07370351
10210 IVPASS = IVPASS + 1                                               07380351
      WRITE (I02,80002) IVTNUM                                          07390351
      GO TO 0221                                                        07400351
20210 IVFAIL = IVFAIL + 1                                               07410351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07420351
 0221 CONTINUE                                                          07430351
C                                                                       07440351
C     ****  FCVS PROGRAM 351  -  TEST 022  ****                         07450351
C                                                                       07460351
C          TEST 22 IS LIKE TEST 21 EXCEPT THAT PARENTHESES ARE USED,    07470351
C     AS IS A UNARY OPERATOR.  FINAL ASSIGNMENT IS TO A REAL VARIABLE.  07480351
C                                                                       07490351
      IVTNUM =  22                                                      07500351
      IF (ICZERO) 30220, 0220, 30220                                    07510351
 0220 CONTINUE                                                          07520351
      IVON01 = 798                                                      07530351
      IVON02 = 17                                                       07540351
      RVON03 = 9.34578 E-2                                              07550351
      IVON04 = 15985                                                    07560351
      RVON05 = 0.72357                                                  07570351
      RVCOMP = -(IVON01 / IVON02) + (RVON03 * IVON04 ** RVON05)         07580351
      RVCORR = 5.68717 E1                                               07590351
40220 IF (RVCOMP - 5.6866 E1) 20220, 10220, 40221                       07600351
40221 IF (RVCOMP - 5.6876 E1) 10220, 10220, 20220                       07610351
30220 IVDELE = IVDELE + 1                                               07620351
      WRITE (I02,80000) IVTNUM                                          07630351
      IF (ICZERO) 10220, 0231, 20220                                    07640351
10220 IVPASS = IVPASS + 1                                               07650351
      WRITE (I02,80002) IVTNUM                                          07660351
      GO TO 0231                                                        07670351
20220 IVFAIL = IVFAIL + 1                                               07680351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07690351
 0231 CONTINUE                                                          07700351
C                                                                       07710351
C     ****  FCVS PROGRAM 351  -  TEST 023  ****                         07720351
C                                                                       07730351
C          TEST 23 IS SIMILAR TO TEST 22 EXCEPT THAT IT EMPLOYS NESTED  07740351
C     PARENTHESES.                                                      07750351
C                                                                       07760351
      IVTNUM =  23                                                      07770351
      IF (ICZERO) 30230, 0230, 30230                                    07780351
 0230 CONTINUE                                                          07790351
      IVON01 = 2                                                        07800351
      IVON02 = 183                                                      07810351
      RVON03 = 58.7025                                                  07820351
      IVON04 = 197                                                      07830351
      IVON05 = 87                                                       07840351
      RVON06 = 2.4611 E15                                               07850351
      RVCOMP = IVON01 ** (-(IVON02 + (RVON03 - IVON04)) -               07860351
     1         (IVON05 / RVON06))                                       07870351
      RVCORR = 3.4931 E-14                                              07880351
40230 IF (RVCOMP - 3.4926 E-14) 20230, 10230, 40231                     07890351
40231 IF (RVCOMP - 3.4936 E-14) 10230, 10230, 20230                     07900351
30230 IVDELE = IVDELE + 1                                               07910351
      WRITE (I02,80000) IVTNUM                                          07920351
      IF (ICZERO) 10230, 0241, 20230                                    07930351
10230 IVPASS = IVPASS + 1                                               07940351
      WRITE (I02,80002) IVTNUM                                          07950351
      GO TO 0241                                                        07960351
20230 IVFAIL = IVFAIL + 1                                               07970351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          07980351
 0241 CONTINUE                                                          07990351
C                                                                       08000351
C     ****  FCVS PROGRAM 351  -  TEST 024  ****                         08010351
C                                                                       08020351
C          TEST 24 IS IDENTICAL TO TEST 23 EXCEPT THAT THE FINAL        08030351
C     ASSIGNMENT IS TO AN INTEGER VARIABLE INSTEAD OF A REAL VARIABLE.  08040351
C                                                                       08050351
      IVTNUM =  24                                                      08060351
      IF (ICZERO) 30240, 0240, 30240                                    08070351
 0240 CONTINUE                                                          08080351
      IVON01 = 2                                                        08090351
      IVON02 = 183                                                      08100351
      RVON03 = 58.7025                                                  08110351
      IVON04 = 197                                                      08120351
      IVON05 = 87                                                       08130351
      RVON06 = 2.4611 E15                                               08140351
      IVCOMP = IVON01 ** (-(IVON02 + (RVON03 - IVON04)) -               08150351
     1         (IVON05 / RVON06))                                       08160351
      IVCORR = 0                                                        08170351
40240 IF (IVCOMP) 20240, 10240, 20240                                   08180351
30240 IVDELE = IVDELE + 1                                               08190351
      WRITE (I02,80000) IVTNUM                                          08200351
      IF (ICZERO) 10240, 0251, 20240                                    08210351
10240 IVPASS = IVPASS + 1                                               08220351
      WRITE (I02,80002) IVTNUM                                          08230351
      GO TO 0251                                                        08240351
20240 IVFAIL = IVFAIL + 1                                               08250351
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08260351
 0251 CONTINUE                                                          08270351
C                                                                       08280351
C     ****  FCVS PROGRAM 351  -  TEST 025  ****                         08290351
C                                                                       08300351
C          TEST 25 IS SIMILAR TO TESTS 9 AND 19 EXCEPT THAT A MIXTURE   08310351
C     OF REAL AND INTEGER OPERANDS ARE USED, AND FINAL ASSIGNMENT IS    08320351
C     TO A REAL VARIABLE.                                               08330351
C                                                                       08340351
      IVTNUM =  25                                                      08350351
      IF (ICZERO) 30250, 0250, 30250                                    08360351
 0250 CONTINUE                                                          08370351
      RVON01 = 4.7117                                                   08380351
      RVON02 = 5.998                                                    08390351
      IVON03 = 2                                                        08400351
      RVON04 = 1E2                                                      08410351
      IVON05 = 20                                                       08420351
      IVON06 = 4                                                        08430351
      IVON07 = -3                                                       08440351
      RADN11(4) = 7.425                                                 08450351
      RADN11(5) = -2.4015                                               08460351
      RVCOMP = (RVON01 + RFOS06(AINT(RVON02),IVON03,RVON04)) * IVON05 - 08470351
     1         IFOS05(IVON06,IVON07)                                    08480351
      RVCORR =  84.234                                                  08490351
40250 IF (RVCOMP - 84.229) 20250, 10250, 40251                          08500351
40251 IF (RVCOMP - 84.239) 10250, 10250, 20250                          08510351
30250 IVDELE = IVDELE + 1                                               08520351
      WRITE (I02,80000) IVTNUM                                          08530351
      IF (ICZERO) 10250, 0261, 20250                                    08540351
10250 IVPASS = IVPASS + 1                                               08550351
      WRITE (I02,80002) IVTNUM                                          08560351
      GO TO 0261                                                        08570351
20250 IVFAIL = IVFAIL + 1                                               08580351
      WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                          08590351
 0261 CONTINUE                                                          08600351
C                                                                       08610351
C                                                                       08620351
C     WRITE OUT TEST SUMMARY                                            08630351
C                                                                       08640351
      WRITE (I02,90004)                                                 08650351
      WRITE (I02,90014)                                                 08660351
      WRITE (I02,90004)                                                 08670351
      WRITE (I02,90000)                                                 08680351
      WRITE (I02,90004)                                                 08690351
      WRITE (I02,90020) IVFAIL                                          08700351
      WRITE (I02,90022) IVPASS                                          08710351
      WRITE (I02,90024) IVDELE                                          08720351
      STOP                                                              08730351
90001 FORMAT (1H ,24X,5HFM351)                                          08740351
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM351)                          08750351
C                                                                       08760351
C     FORMATS FOR TEST DETAIL LINES                                     08770351
C                                                                       08780351
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   08790351
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08800351
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08810351
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08820351
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        08830351
C                                                                       08840351
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08850351
C                                                                       08860351
90002 FORMAT (1H1)                                                      08870351
90004 FORMAT (1H )                                                      08880351
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08890351
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   08900351
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         08910351
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  08920351
90014 FORMAT (1H ,5X,46H----------------------------------------------) 08930351
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08940351
C                                                                       08950351
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 08960351
C                                                                       08970351
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              08980351
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              08990351
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             09000351
      END                                                               09010351
