      PROGRAM FM253                                                     00010253
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020253
C                                                                       00030253
C                                                                       00040253
C        THIS ROUTINE IS A TEST OF THE IF-BLOCK.  TESTS WITHIN THIS     00050253
C     ROUTINE ARE FOR THE SYNTAX OF THE BASIC IF ( )  THEN  THROUGH     00060253
C     END IF BLOCK STRUCTURE.                                           00070253
C                                                                       00080253
C        THERE IS ALSO A SERIES OF TESTS TO CHECK THE HIERARCHY AND     00090253
C     ORDER OF EVALUATION IN EXPRESSIONS THAT CONTAIN A COMBINATION OF  00100253
C     ARITHMETIC, RELATIONAL, AND LOGICAL OPERATORS.                    00110253
C                                                                       00120253
C     REFERENCES                                                        00130253
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140253
C             X3.9-1978                                                 00150253
C        SECTION 11.6,       BLOCK IF STATEMENT                         00160253
C        SECTION 11.6.1,     IF-LEVEL                                   00170253
C        SECTION 11.6.2,     IF-BLOCK                                   00180253
C        SECTION 11.6.3,     EXECUTION OF A BLOCK IF STATEMENT          00190253
C                                                                       00200253
C                                                                       00210253
C                                                                       00220253
C                                                                       00230253
C     ******************************************************************00240253
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00250253
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00260253
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00270253
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00280253
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00290253
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00300253
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00310253
C     THE RESULT OF EXECUTING THESE TESTS.                              00320253
C                                                                       00330253
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00340253
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00350253
C                                                                       00360253
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00370253
C                    DEPARTMENT OF THE NAVY                             00380253
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00390253
C                    WASHINGTON, D.C.   20376                           00400253
C                                                                       00410253
C     ******************************************************************00420253
C                                                                       00430253
C                                                                       00440253
      IMPLICIT LOGICAL (L)                                              00450253
      IMPLICIT CHARACTER*14 (C)                                         00460253
C                                                                       00470253
      DIMENSION LADN11(2)                                               00480253
      LOGICAL LVTN01, LVTN02, LATN11(2), LADN11                         00490253
      DATA LADN11/.TRUE., .FALSE./                                      00500253
C                                                                       00510253
C                                                                       00520253
C     **** LOGICAL STATEMENT FUNCTION REFERENCED IN TEST 20 ****        00530253
C                                                                       00540253
      LFIS01 ( L ) = .NOT. L                                            00550253
C                                                                       00560253
C                                                                       00570253
C                                                                       00580253
C                                                                       00590253
C                                                                       00600253
C     INITIALIZATION SECTION.                                           00610253
C                                                                       00620253
C     INITIALIZE CONSTANTS                                              00630253
C     ********************                                              00640253
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00650253
      I01 = 5                                                           00660253
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00670253
      I02 = 6                                                           00680253
C     SYSTEM ENVIRONMENT SECTION                                        00690253
C                                                                       00700253
      I01 = 5                                                           00710253
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720253
C     (UNIT NUMBER FOR CARD READER).                                    00730253
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00740253
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00750253
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00760253
C                                                                       00770253
      I02 = 6                                                           00780253
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00790253
C     (UNIT NUMBER FOR PRINTER).                                        00800253
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00810253
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00820253
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00830253
C                                                                       00840253
      IVPASS = 0                                                        00850253
      IVFAIL = 0                                                        00860253
      IVDELE = 0                                                        00870253
      ICZERO = 0                                                        00880253
C                                                                       00890253
C     WRITE OUT PAGE HEADERS                                            00900253
C                                                                       00910253
      WRITE (I02,90002)                                                 00920253
      WRITE (I02,90006)                                                 00930253
      WRITE (I02,90008)                                                 00940253
      WRITE (I02,90004)                                                 00950253
      WRITE (I02,90010)                                                 00960253
      WRITE (I02,90004)                                                 00970253
      WRITE (I02,90016)                                                 00980253
      WRITE (I02,90001)                                                 00990253
      WRITE (I02,90004)                                                 01000253
      WRITE (I02,90012)                                                 01010253
      WRITE (I02,90014)                                                 01020253
      WRITE (I02,90004)                                                 01030253
C                                                                       01040253
C                                                                       01050253
C     ****  FCVS PROGRAM 253  -  TEST 001  ****                         01060253
C                                                                       01070253
C        TEST 001 USES A VERY SIMPLE BLOCK IF STATEMENT.  THE EXPRESSION01080253
C     WITHIN THE PARENTHESES IS THE LOGICAL CONSTANT  .TRUE.  AND THE   01090253
C     EXECUTABLE STATEMENT WITHIN THE IF-BLOCK OF LEVEL ONE IS AN       01100253
C     INTEGER ARITHMETIC ASSIGNMENT STATEMENT. SINCE THE LOGICAL        01110253
C     EXPRESSION IS TRUE, THEN THE INTEGER ASSIGNMENT STATEMENT ( TRUE  01120253
C     PATH ) SHOULD BE EXECUTED.                                        01130253
C                                                                       01140253
C        THIS IS A SYNTAX CHECK FOR THE BLOCK IF STATEMENT.  SHOULD A   01150253
C     COMPILER NOT BE ABLE TO ACCEPT THE SYNTAX OF THIS BASIC TEST,     01160253
C     THEN ROUTINES FM253, THRU FMXXX NEED NOT BE RUN.                  01170253
C                                                                       01180253
C                                                                       01190253
      IVTNUM =   1                                                      01200253
      IF (ICZERO) 30010, 0010, 30010                                    01210253
 0010 CONTINUE                                                          01220253
      IVCOMP = 0                                                        01230253
      IF ( .TRUE. ) THEN                                                01240253
           IVCOMP = 1                                                   01250253
      END IF                                                            01260253
      IVCORR = 1                                                        01270253
40010 IF ( IVCOMP - 1 )  20010, 10010, 20010                            01280253
30010 IVDELE = IVDELE + 1                                               01290253
      WRITE (I02,80000) IVTNUM                                          01300253
      IF (ICZERO) 10010, 0021, 20010                                    01310253
10010 IVPASS = IVPASS + 1                                               01320253
      WRITE (I02,80002) IVTNUM                                          01330253
      GO TO 0021                                                        01340253
20010 IVFAIL = IVFAIL + 1                                               01350253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01360253
 0021 CONTINUE                                                          01370253
C                                                                       01380253
C     ****  FCVS PROGRAM 253  -  TEST 002  ****                         01390253
C                                                                       01400253
C        TEST 002 USES A LOGICAL VARIABLE SET .FALSE. AS THE LOGICAL    01410253
C     EXPRESSION IN THE BLOCK IF STATEMENT.   BECAUSE THE EXPRESSION    01420253
C     IS FALSE, THE IF-BLOCK (WHICH IS AN INTEGER ARITHMETIC ASSIGNMENT 01430253
C     STATEMENT AND A LOGICAL ASSIGNMENT STATEMENT) SHOULD NOT BE       01440253
C     EXECUTED.                                                         01450253
C                                                                       01460253
C                                                                       01470253
      IVTNUM =   2                                                      01480253
      IF (ICZERO) 30020, 0020, 30020                                    01490253
 0020 CONTINUE                                                          01500253
      IVCOMP = 0                                                        01510253
      IVON01 = 1                                                        01520253
      LVON01 = .FALSE.                                                  01530253
      LVTN01 = .FALSE.                                                  01540253
      IF ( LVON01 )  THEN                                               01550253
           IVON01 = 0                                                   01560253
           LVTN01 = .TRUE.                                              01570253
      END IF                                                            01580253
      IVCORR = 1                                                        01590253
40020 IF ( IVON01 .EQ. 1 )  IVCOMP = 1                                  01600253
40021 IF ( IVCOMP - 1 )  20020, 10020, 20020                            01610253
30020 IVDELE = IVDELE + 1                                               01620253
      WRITE (I02,80000) IVTNUM                                          01630253
      IF (ICZERO) 10020, 0031, 20020                                    01640253
10020 IVPASS = IVPASS + 1                                               01650253
      WRITE (I02,80002) IVTNUM                                          01660253
      GO TO 0031                                                        01670253
20020 IVFAIL = IVFAIL + 1                                               01680253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01690253
 0031 CONTINUE                                                          01700253
C                                                                       01710253
C     ****  FCVS PROGRAM 253  -  TEST 003  ****                         01720253
C                                                                       01730253
C        TEST 003 IS A BLOCK IF STATEMENT WITH AN EMPTY IF-BLOCK.  THE  01740253
C     LOGICAL EXPRESSION IS A LOGICAL ARRAY ELEMENT REFERENCE SET TO    01750253
C     .TRUE.  SECTION 11.6.2 STATES THAT,  AN IF-BLOCK MAY BE EMPTY.    01760253
C     BECAUSE THE LOGICAL EXPRESSION IS TRUE, THE IF-BLOCK SHOULD BE    01770253
C     EXECUTED.  IF THE VALUE OF THE EXPRESSION IS TRUE AND THE IF-BLOCK01780253
C     IS EMPTY, CONTROL IS TRANSFERRED TO THE NEXT END IF STATEMENT     01790253
C     THAT HAS THE SAME IF-LEVEL AS THE BLOCK IF STATEMENT ACCORDING    01800253
C     TO SECTION 11.6.3. IN THIS TEST THE EMPTY IF-BLOCK IS OF LEVEL ONE01810253
C                                                                       01820253
C                                                                       01830253
      IVTNUM =   3                                                      01840253
      IF (ICZERO) 30030, 0030, 30030                                    01850253
 0030 CONTINUE                                                          01860253
      IVCOMP = 0                                                        01870253
      LATN11(1) = .TRUE.                                                01880253
      IF ( LATN11(1) )  THEN                                            01890253
      END IF                                                            01900253
      IVCOMP = 1                                                        01910253
      IVCORR = 1                                                        01920253
40030 IF ( IVCOMP - 1 )  20030, 10030, 20030                            01930253
30030 IVDELE = IVDELE + 1                                               01940253
      WRITE (I02,80000) IVTNUM                                          01950253
      IF (ICZERO) 10030, 0041, 20030                                    01960253
10030 IVPASS = IVPASS + 1                                               01970253
      WRITE (I02,80002) IVTNUM                                          01980253
      GO TO 0041                                                        01990253
20030 IVFAIL = IVFAIL + 1                                               02000253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02010253
 0041 CONTINUE                                                          02020253
C                                                                       02030253
C     ****  FCVS PROGRAM 253  -  TEST 004  ****                         02040253
C                                                                       02050253
C        TEST 004 IS LIKE THE PREVIOUS TEST USING A LOGICAL ARRAY       02060253
C     ELEMENT REFERENCE AS THE LOGICAL EXPRESSION OF THE BLOCK IF       02070253
C     STATEMENT THAT HAS AN EMPTY IF-BLOCK STRUCTURE OF LEVEL ONE.      02080253
C     IN THIS TEST THE LOGICAL EXPRESSION IS FALSE SO CONTROL SHOULD    02090253
C     BE TRANSFERRED TO THE END IF STATEMENT THAT HAS THE SAME IF-LEVEL 02100253
C     AS THE BLOCK IF STATEMENT ACCORDING TO SECTION 11.6.3.            02110253
C                                                                       02120253
C     THE LOGICAL ARRAY ELEMENT REFERENCE  LADN11(2) IS SET TO .FALSE.  02130253
C     IN THE DATA STATEMENT AS FOLLOWS                                  02140253
C                                                                       02150253
C                  DATA LADN11/.TRUE., .FALSE./                         02160253
C                                                                       02170253
C                                                                       02180253
      IVTNUM =   4                                                      02190253
      IF (ICZERO) 30040, 0040, 30040                                    02200253
 0040 CONTINUE                                                          02210253
      IVCOMP = 0                                                        02220253
      IF ( LADN11(2) )  THEN                                            02230253
      END IF                                                            02240253
      IVCOMP = 1                                                        02250253
      IVCORR = 1                                                        02260253
40040 IF ( IVCOMP - 1 )  20040, 10040, 20040                            02270253
30040 IVDELE = IVDELE + 1                                               02280253
      WRITE (I02,80000) IVTNUM                                          02290253
      IF (ICZERO) 10040, 0051, 20040                                    02300253
10040 IVPASS = IVPASS + 1                                               02310253
      WRITE (I02,80002) IVTNUM                                          02320253
      GO TO 0051                                                        02330253
20040 IVFAIL = IVFAIL + 1                                               02340253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02350253
 0051 CONTINUE                                                          02360253
C                                                                       02370253
C                                                                       02380253
C        THE NEXT FOUR TESTS ARE FOR A BLOCK IF STRUCTURE OF LEVEL      02390253
C     TWO IN THE INNERMOST IF-BLOCK.  THIS STRUCTURE IS SHOWN BELOW -   02400253
C                                                                       02410253
C                  IF ( E1 )  THEN                                      02420253
C                       IF-BLOCK 1                                      02430253
C                       IF ( E2 )  THEN                                 02440253
C                            IF-BLOCK 2                                 02450253
C                       END IF                                          02460253
C                  END IF                                               02470253
C     TESTS WILL USE THE FOUR COMBINATIONS OF TRUE AND FALSE FOR E1 AND 02480253
C     E2 RESPECTIVELY TO TEST THE TRANSFER OF CONTROL AS DESCRIBED      02490253
C     IN SECTION 11.6.3.                                                02500253
C                                                                       02510253
C                                                                       02520253
C                                                                       02530253
C     ****  FCVS PROGRAM 253  -  TEST 005  ****                         02540253
C                                                                       02550253
C        TEST 005 USES A FALSE VALUE FOR E1 AND A FALSE VALUE FOR E2.   02560253
C     CONTROL SHOULD BE TRANSFERRED TO THE END IF STATEMENT OF LEVEL 1  02570253
C     WHICH MEANS IF-BLOCK 1 AND IF-BLOCK 2 SHOULD NOT BE EXECUTED.     02580253
C                                                                       02590253
C                                                                       02600253
      IVTNUM =   5                                                      02610253
      IF (ICZERO) 30050, 0050, 30050                                    02620253
 0050 CONTINUE                                                          02630253
      IVCOMP = 1                                                        02640253
      LADN11(2) = .FALSE.                                               02650253
      IF ( 76 .LT. 3 )  THEN                                            02660253
           IVCOMP = IVCOMP * 2                                          02670253
           IF ( ( LADN11(2) ) )  THEN                                   02680253
                IVCOMP = IVCOMP * 3                                     02690253
           END IF                                                       02700253
      END IF                                                            02710253
      IVCORR = 1                                                        02720253
40051 IF ( IVCOMP - 1 )  20050, 10050, 20050                            02730253
30050 IVDELE = IVDELE + 1                                               02740253
      WRITE (I02,80000) IVTNUM                                          02750253
      IF (ICZERO) 10050, 0061, 20050                                    02760253
10050 IVPASS = IVPASS + 1                                               02770253
      WRITE (I02,80002) IVTNUM                                          02780253
      GO TO 0061                                                        02790253
20050 IVFAIL = IVFAIL + 1                                               02800253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02810253
 0061 CONTINUE                                                          02820253
C                                                                       02830253
C     ****  FCVS PROGRAM 253  -  TEST 006  ****                         02840253
C                                                                       02850253
C        TEST 006 USES A FALSE VALUE FOR E1 AND A TRUE VALUE FOR E2.    02860253
C     CONTROL SHOULD BE TRANSFERRED TO THE END IF STATEMENT OF LEVEL 1  02870253
C     WHICH MEANS IF-BLOCK 1 AND IF-BLOCK 2 SHOULD NOT BE EXECUTED.     02880253
C                                                                       02890253
C                                                                       02900253
      IVTNUM =   6                                                      02910253
      IF (ICZERO) 30060, 0060, 30060                                    02920253
 0060 CONTINUE                                                          02930253
      IVCOMP = 1                                                        02940253
      IVON03 = 32767                                                    02950253
      LVTN01 = .TRUE.                                                   02960253
      LVON01 = .TRUE.                                                   02970253
      IF ( .NOT. LVTN01 )  THEN                                         02980253
           IVCOMP = IVCOMP * 2                                          02990253
           IF ( LVON01 .AND. IVON03 .GE. 587 )  THEN                    03000253
                IVCOMP = IVCOMP * 3                                     03010253
           END IF                                                       03020253
      END IF                                                            03030253
      IVCORR = 1                                                        03040253
40061 IF ( IVCOMP - 1 )  20060, 10060, 20060                            03050253
30060 IVDELE = IVDELE + 1                                               03060253
      WRITE (I02,80000) IVTNUM                                          03070253
      IF (ICZERO) 10060, 0071, 20060                                    03080253
10060 IVPASS = IVPASS + 1                                               03090253
      WRITE (I02,80002) IVTNUM                                          03100253
      GO TO 0071                                                        03110253
20060 IVFAIL = IVFAIL + 1                                               03120253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03130253
 0071 CONTINUE                                                          03140253
C                                                                       03150253
C     ****  FCVS PROGRAM 253  -  TEST 007  ****                         03160253
C                                                                       03170253
C        TEST 007 USES A TRUE VALUE FOR E1 AND A FALSE VALUE FOR E2.    03180253
C     IF-BLOCK 1 SHOULD BE EXECUTED, BUT IF-BLOCK 2 SHOULD NOT BE       03190253
C     EXECUTED.                                                         03200253
C                                                                       03210253
C        IF-BLOCK 1 ALSO CONTAINS AN UNCONDITIONAL GO TO AND A CONTINUE 03220253
C     STATEMENT WHICH SHOULD BOTH BE EXECUTED.                          03230253
C                                                                       03240253
C                                                                       03250253
      IVTNUM =   7                                                      03260253
      IF (ICZERO) 30070, 0070, 30070                                    03270253
 0070 CONTINUE                                                          03280253
      IVCOMP = 1                                                        03290253
      IVON03 = 587                                                      03300253
      IVON04 = 3                                                        03310253
      LATN11(1) = .TRUE.                                                03320253
      LATN11(2) = .FALSE.                                               03330253
      IF ( (LATN11(1)) .OR. ((7 * IVON04) .EQ. 21) )  THEN              03340253
           IVCOMP = IVCOMP * 2                                          03350253
           GO TO 0072                                                   03360253
 0072      CONTINUE                                                     03370253
           IF ( 7 .GT. IVON03 .OR. LATN11(2) )  THEN                    03380253
                IVCOMP = IVCOMP * 3                                     03390253
           END IF                                                       03400253
      END IF                                                            03410253
C                                                                       03420253
C        **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2        ****    03430253
C                                                                       03440253
      IVCORR = 2                                                        03450253
40070 IF ( IVCOMP - 2 )  20070, 10070, 20070                            03460253
30070 IVDELE = IVDELE + 1                                               03470253
      WRITE (I02,80000) IVTNUM                                          03480253
      IF (ICZERO) 10070, 0081, 20070                                    03490253
10070 IVPASS = IVPASS + 1                                               03500253
      WRITE (I02,80002) IVTNUM                                          03510253
      GO TO 0081                                                        03520253
20070 IVFAIL = IVFAIL + 1                                               03530253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03540253
 0081 CONTINUE                                                          03550253
C                                                                       03560253
C     ****  FCVS PROGRAM 253  -  TEST 008  ****                         03570253
C                                                                       03580253
C        TEST 008 USES A TRUE VALUE FOR E1 AND A TRUE VALUE FOR E2.     03590253
C     BOTH IF-BLOCK 1 AND IF-BLOCK 2 SHOULD BE EXECUTED.                03600253
C                                                                       03610253
C        IF-BLOCK 1 CONTAINS AN ASSIGN STATEMENT PLUS AN ASSIGNED GO TO 03620253
C     STATEMENT WHICH SHOULD BOTH BE EXECUTED.                          03630253
C                                                                       03640253
C                                                                       03650253
      IVTNUM =   8                                                      03660253
      IF (ICZERO) 30080, 0080, 30080                                    03670253
 0080 CONTINUE                                                          03680253
      IVCOMP = 1                                                        03690253
      IVON04 = 4                                                        03700253
      IVON05 = 2                                                        03710253
      LVON01 = .FALSE.                                                  03720253
      LVTN01 = LVON01                                                   03730253
C                                                                       03740253
      IF ( IVON04 - 1 .LE. 6 .AND. 7 .GE. 5 / IVON05 )  THEN            03750253
           IVCOMP = IVCOMP * 2                                          03760253
           ASSIGN 0083 TO I                                             03770253
           GO TO 0084                                                   03780253
 0082      IVCOMP = IVCOMP * 3                                          03790253
           GO TO 0085                                                   03800253
 0083      IVCOMP = IVCOMP * 5                                          03810253
           GO TO 0085                                                   03820253
 0084      GO TO I, ( 0082, 0083 )                                      03830253
 0085      IF ( .NOT. ( LVTN01 ) )  THEN                                03840253
                IVCOMP = IVCOMP * 7                                     03850253
           END IF                                                       03860253
      END IF                                                            03870253
C                                                                       03880253
C        **** IVCOMP IS DETERMINED BY IVCOMP = 70 = 1 * 2 * 5 * 7   ****03890253
C                                                                       03900253
      IVCORR = 70                                                       03910253
40080 IF ( IVCOMP - 70 )  20080, 10080, 20080                           03920253
30080 IVDELE = IVDELE + 1                                               03930253
      WRITE (I02,80000) IVTNUM                                          03940253
      IF (ICZERO) 10080, 0091, 20080                                    03950253
10080 IVPASS = IVPASS + 1                                               03960253
      WRITE (I02,80002) IVTNUM                                          03970253
      GO TO 0091                                                        03980253
20080 IVFAIL = IVFAIL + 1                                               03990253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04000253
 0091 CONTINUE                                                          04010253
C                                                                       04020253
C        THE NEXT FIVE TESTS ARE FOR A BLOCK IF STRUCTURE OF LEVEL      04030253
C     THREE IN THE INNERMOST IF-BLOCK.  THIS STRUCTURE IS SHOWN BELOW - 04040253
C                                                                       04050253
C             IF ( E1 )  THEN                                           04060253
C                  IF-BLOCK 1                                           04070253
C                  IF ( E2 )  THEN                                      04080253
C                       IF-BLOCK 2                                      04090253
C                       IF ( E3 )  THEN                                 04100253
C                            IF-BLOCK 3                                 04110253
C                       END IF                                          04120253
C                  END IF                                               04130253
C             END IF                                                    04140253
C                                                                       04150253
C     THE FIVE TESTS WILL USE THE FOLLOWING COMBINATIONS OF TRUE AND    04160253
C     FALSE FOR E1, E2, AND E3 AS SHOWN BELOW -                         04170253
C        TEST NUMBER    9   10   11   12   13                           04180253
C             E1        T    T    T    T    F                           04190253
C             E2        T    T    F    F    T                           04200253
C             E3        T    F    T    F    T                           04210253
C                                                                       04220253
C     CONTROL SHOULD BE AS DESCRIBED IN SECTION 11.6.3.                 04230253
C                                                                       04240253
C                                                                       04250253
C                                                                       04260253
C     ****  FCVS PROGRAM 253  -  TEST 009  ****                         04270253
C                                                                       04280253
C        TEST 009 HAS E1, E2, AND E3 AS TRUE.  IF-BLOCK 1, 2, AND 3     04290253
C     SHOULD BE EXECUTED.  IF-BLOCK 1 HAS A COMPUTED GO TO STATEMENT    04300253
C     WHICH SHOULD BE EXECUTED.                                         04310253
C                                                                       04320253
C                                                                       04330253
      IVTNUM =   9                                                      04340253
      IF (ICZERO) 30090, 0090, 30090                                    04350253
 0090 CONTINUE                                                          04360253
      IVCOMP = 1                                                        04370253
      IVON01 = 4                                                        04380253
      IVON02 = 3                                                        04390253
C                                                                       04400253
      IF ( .NOT. IVON01 .EQ. 3 .OR. .NOT. IVON02 .EQ. 4 )  THEN         04410253
           IVCOMP = IVCOMP * 2                                          04420253
           J = 2                                                        04430253
           GO TO 0095                                                   04440253
 0092      IVCOMP = IVCOMP * 3                                          04450253
           GO TO 0096                                                   04460253
 0093      IVCOMP = IVCOMP * 5                                          04470253
           GO TO 0096                                                   04480253
 0094      IVCOMP = IVCOMP * 7                                          04490253
           GO TO 0096                                                   04500253
 0095      GO TO ( 0092, 0093, 0094 ), J                                04510253
 0096      IF ( IVON01 .EQ. 4 .AND. IVON02 .NE. 2 )  THEN               04520253
                IVCOMP = IVCOMP * 11                                    04530253
                IF ( IVON01 .EQ. 4 .AND. .NOT. IVON02 .EQ. 2 )  THEN    04540253
                     IVCOMP = IVCOMP * 13                               04550253
                END IF                                                  04560253
           END IF                                                       04570253
      END IF                                                            04580253
C                                                                       04590253
C        **** IVCOMP IS DETERMINED BY IVCOMP = 1430 = 1*2*5*11*13   ****04600253
C                                                                       04610253
      IVCORR = 1430                                                     04620253
40090 IF ( IVCOMP - 1430 )  20090, 10090, 20090                         04630253
30090 IVDELE = IVDELE + 1                                               04640253
      WRITE (I02,80000) IVTNUM                                          04650253
      IF (ICZERO) 10090, 0101, 20090                                    04660253
10090 IVPASS = IVPASS + 1                                               04670253
      WRITE (I02,80002) IVTNUM                                          04680253
      GO TO 0101                                                        04690253
20090 IVFAIL = IVFAIL + 1                                               04700253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04710253
 0101 CONTINUE                                                          04720253
C                                                                       04730253
C     ****  FCVS PROGRAM 253  -  TEST 010  ****                         04740253
C                                                                       04750253
C                                                                       04760253
C        TEST 010 HAS E1 AND E2 AS TRUE.  E3 IS FALSE.  IF-BLOCK 1 HAS  04770253
C     A LOGICAL IF STATEMENT WHICH SHOULD BE EXECUTED BY TAKING THE     04780253
C     TRUE PATH.  IF-BLOCK 2 HAS AN ARITHMETIC IF STATEMENT WITH THE    04790253
C     VALUE INSIDE THE PARENTHESIS EQUAL TO ZERO.  IF-BLOCK 3 SHOULD NOT04800253
C     BE EXECUTED.                                                      04810253
C                                                                       04820253
C                                                                       04830253
      IVTNUM =  10                                                      04840253
      IF (ICZERO) 30100, 0100, 30100                                    04850253
 0100 CONTINUE                                                          04860253
      IVCOMP = 1                                                        04870253
      IVON01 = +3                                                       04880253
      LVON01 = .FALSE.                                                  04890253
C                                                                       04900253
      IF ( .NOT. LVON01 .AND. .TRUE. .OR. .TRUE. .AND. .NOT. LVON01 )   04910253
     1THEN                                                              04920253
           IVCOMP = IVCOMP * 2                                          04930253
           IF ( 3 .LE. IVON01 )  IVCOMP = IVCOMP * 3                    04940253
           IF ( .NOT.(LVON01.AND..TRUE.).OR.(.TRUE..AND..NOT.LVON01) )  04950253
     1     THEN                                                         04960253
                IF ( 3 - IVON01 )  0103, 0102, 0103                     04970253
 0102           IVCOMP = IVCOMP * 5                                     04980253
                GO TO 0104                                              04990253
 0103           IVCOMP = IVCOMP * 7                                     05000253
 0104           CONTINUE                                                05010253
                IF ( .NOT.(.NOT.(LVON01.AND..TRUE.)).OR..FALSE..AND.    05020253
     1          .NOT.LVON01 )  THEN                                     05030253
                     IVCOMP = IVCOMP * 11                               05040253
                END IF                                                  05050253
           END IF                                                       05060253
      END IF                                                            05070253
C                                                                       05080253
C        **** IVCOMP IS DETERMINED BY IVCOMP = 30 = 1 * 2 * 3 * 5   ****05090253
C                                                                       05100253
      IVCORR = 30                                                       05110253
40100 IF ( IVCOMP - 30 )  20100, 10100, 20100                           05120253
30100 IVDELE = IVDELE + 1                                               05130253
      WRITE (I02,80000) IVTNUM                                          05140253
      IF (ICZERO) 10100, 0111, 20100                                    05150253
10100 IVPASS = IVPASS + 1                                               05160253
      WRITE (I02,80002) IVTNUM                                          05170253
      GO TO 0111                                                        05180253
20100 IVFAIL = IVFAIL + 1                                               05190253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05200253
 0111 CONTINUE                                                          05210253
C                                                                       05220253
C     ****  FCVS PROGRAM 253  -  TEST 011  ****                         05230253
C                                                                       05240253
C                                                                       05250253
C        TEST 011 HAS E1 AND E3 AS TRUE.  E2 IS FALSE.  ONLY IF-BLOCK 1 05260253
C     SHOULD BE EXECUTED.  THIS SET OF BLOCK IF STATEMENTS HAS INTEGER  05270253
C     ASSIGNMENT STATEMENTS BETWEEN THE END IF STATEMENTS.  A CHECK IS  05280253
C     MADE TO DETERMINE IF THESE STATEMENTS HAVE BEEN EXECUTED.         05290253
C                                                                       05300253
C                                                                       05310253
      IVTNUM =  11                                                      05320253
      IF (ICZERO) 30110, 0110, 30110                                    05330253
 0110 CONTINUE                                                          05340253
      IVCOMP = 1                                                        05350253
      LVON01 = .TRUE.                                                   05360253
      LVON02 = .FALSE.                                                  05370253
      LVON03 = .TRUE.                                                   05380253
C                                                                       05390253
      IF ( LVON01 )  THEN                                               05400253
           IVCOMP = IVCOMP * 2                                          05410253
           IF ( LVON02 )  THEN                                          05420253
                IVCOMP = IVCOMP * 3                                     05430253
                IF ( LVON03 )  THEN                                     05440253
                     IVCOMP = IVCOMP * 5                                05450253
                END IF                                                  05460253
                IVCOMP = IVCOMP * 7                                     05470253
           END IF                                                       05480253
           IVCOMP = IVCOMP * 11                                         05490253
      END IF                                                            05500253
      IVCOMP = IVCOMP * 13                                              05510253
C                                                                       05520253
C        **** IVCOMP IS DETERMINED BY IVCOMP = 286 = 1*2*11*13      ****05530253
C                                                                       05540253
      IVCORR = 286                                                      05550253
40110 IF ( IVCOMP - 286 )  20110, 10110, 20110                          05560253
30110 IVDELE = IVDELE + 1                                               05570253
      WRITE (I02,80000) IVTNUM                                          05580253
      IF (ICZERO) 10110, 0121, 20110                                    05590253
10110 IVPASS = IVPASS + 1                                               05600253
      WRITE (I02,80002) IVTNUM                                          05610253
      GO TO 0121                                                        05620253
20110 IVFAIL = IVFAIL + 1                                               05630253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05640253
 0121 CONTINUE                                                          05650253
C                                                                       05660253
C     ****  FCVS PROGRAM 253  -  TEST 012  ****                         05670253
C                                                                       05680253
C                                                                       05690253
C        TEST 012 HAS E1 AS TRUE.  E2 AND E3 ARE FALSE.  ONLY IF-BLOCK 105700253
C     SHOULD BE EXECUTED.  INTEGER ASSIGNMENT STATEMENTS ARE USED TO    05710253
C     DETERMINE THE FLOW OF LOGIC THROUGH THE BLOCK IF STRUCTURE.       05720253
C                                                                       05730253
C                                                                       05740253
      IVTNUM =  12                                                      05750253
      IF (ICZERO) 30120, 0120, 30120                                    05760253
 0120 CONTINUE                                                          05770253
      IVCOMP = 1                                                        05780253
      LVON01 = .TRUE.                                                   05790253
      LVON02 = .FALSE.                                                  05800253
      LVON03 = .FALSE.                                                  05810253
C                                                                       05820253
      IF ( LVON01 )  THEN                                               05830253
           IVCOMP = IVCOMP * 2                                          05840253
           IF ( LVON02 )  THEN                                          05850253
                IVCOMP = IVCOMP * 3                                     05860253
                IF ( LVON03 )  THEN                                     05870253
                     IVCOMP = IVCOMP * 5                                05880253
                END IF                                                  05890253
                IVCOMP = IVCOMP * 7                                     05900253
           END IF                                                       05910253
           IVCOMP = IVCOMP * 11                                         05920253
      END IF                                                            05930253
      IVCOMP = IVCOMP * 13                                              05940253
C                                                                       05950253
C        **** IVCOMP IS DETERMINED BY IVCOMP = 286 = 1*2*11*13      ****05960253
C                                                                       05970253
      IVCORR = 286                                                      05980253
40120 IF ( IVCOMP - 286 )  20120, 10120, 20120                          05990253
30120 IVDELE = IVDELE + 1                                               06000253
      WRITE (I02,80000) IVTNUM                                          06010253
      IF (ICZERO) 10120, 0131, 20120                                    06020253
10120 IVPASS = IVPASS + 1                                               06030253
      WRITE (I02,80002) IVTNUM                                          06040253
      GO TO 0131                                                        06050253
20120 IVFAIL = IVFAIL + 1                                               06060253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06070253
 0131 CONTINUE                                                          06080253
C                                                                       06090253
C     ****  FCVS PROGRAM 253  -  TEST 013  ****                         06100253
C                                                                       06110253
C                                                                       06120253
C        TEST 013 HAS E1 FALSE.  E2 AND E3 ARE TRUE.  NONE OF THE IF-   06130253
C     BLOCKS SHOULD BE EXECUTED.  INTEGER ASSIGNMENT STATEMENTS ARE     06140253
C     USED TO TRACE THE FLOW OF LOGIC THROUGH THE BLOCK IF STRUCTURE.   06150253
C                                                                       06160253
C                                                                       06170253
      IVTNUM =  13                                                      06180253
      IF (ICZERO) 30130, 0130, 30130                                    06190253
 0130 CONTINUE                                                          06200253
      IVCOMP = 1                                                        06210253
      LVON01 = .FALSE.                                                  06220253
      LVON02 = .TRUE.                                                   06230253
      LVON03 = .TRUE.                                                   06240253
C                                                                       06250253
      IF ( LVON01 )  THEN                                               06260253
           IVCOMP = IVCOMP * 2                                          06270253
           IF ( LVON02 )  THEN                                          06280253
                IVCOMP = IVCOMP * 3                                     06290253
                IF ( LVON03 )  THEN                                     06300253
                     IVCOMP = IVCOMP * 5                                06310253
                END IF                                                  06320253
                IVCOMP = IVCOMP * 7                                     06330253
           END IF                                                       06340253
           IVCOMP = IVCOMP * 11                                         06350253
      END IF                                                            06360253
      IVCOMP = IVCOMP * 13                                              06370253
C                                                                       06380253
C        **** IVCOMP IS DETERMINED BY IVCOMP = 13 = 1 * 13          ****06390253
C                                                                       06400253
      IVCORR = 13                                                       06410253
40130 IF ( IVCOMP - 13 )  20130, 10130, 20130                           06420253
30130 IVDELE = IVDELE + 1                                               06430253
      WRITE (I02,80000) IVTNUM                                          06440253
      IF (ICZERO) 10130, 0141, 20130                                    06450253
10130 IVPASS = IVPASS + 1                                               06460253
      WRITE (I02,80002) IVTNUM                                          06470253
      GO TO 0141                                                        06480253
20130 IVFAIL = IVFAIL + 1                                               06490253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06500253
 0141 CONTINUE                                                          06510253
C                                                                       06520253
C     ****  FCVS PROGRAM 253  -  TEST 014  ****                         06530253
C                                                                       06540253
C        TEST 014 IS TO CHECK FOR PROPER TRANSFER OF CONTROL USING      06550253
C     LOGICAL IF STATEMENTS WITHIN IF-BLOCKS AND BRANCHING TO THE       06560253
C     OUTERMOST IF-LEVEL FROM THE INNERMOST IF-LEVEL IN A CONTROLLED    06570253
C     LOOP.  THE INNERMOST IF-LEVEL SHOULD BE EXECUTED 10 TIMES.  A     06580253
C     LOGICAL IF STATEMENT IS USED IN EACH OF THE IF-LEVELS IN CASE     06590253
C     THE EXECUTION LOGIC BRANCHES INCORRECTLY.  THIS SHOULD PREVENT    06600253
C     AN INFINITE LOOP DURING THE EXECUTION OF THIS ROUTINE.            06610253
C                                                                       06620253
C                                                                       06630253
      IVTNUM =  14                                                      06640253
      IF (ICZERO) 30140, 0140, 30140                                    06650253
 0140 CONTINUE                                                          06660253
      IVCOMP = 0                                                        06670253
      IVON01 = 0                                                        06680253
      IVON02 = 0                                                        06690253
      IVON03 = 0                                                        06700253
 0142 IF ( IVON03 .LT. 10 )  THEN                                       06710253
           IVON01 = IVON01 + 1                                          06720253
           IF ( IVON01 .GT. 11 )  GO TO 0143                            06730253
           IF ( IVON03 .LT. 10 )  THEN                                  06740253
                IVON02 = IVON02 + 1                                     06750253
                IF ( IVON02 .GT. 11 )  GO TO 0143                       06760253
                IF ( IVON03 .LT. 10 )  THEN                             06770253
                     IVON03 = IVON03 + 1                                06780253
                     IF ( IVON03 .GT. 11 )  GO TO 0143                  06790253
                     IF ( IVON03 .LE. 10 )  GO TO 0142                  06800253
                END IF                                                  06810253
           END IF                                                       06820253
      END IF                                                            06830253
 0143 CONTINUE                                                          06840253
      IVCOMP = IVON01                                                   06850253
      IVCORR = 10                                                       06860253
40140 IF ( IVCOMP - 10 ) 20140, 10140, 20140                            06870253
30140 IVDELE = IVDELE + 1                                               06880253
      WRITE (I02,80000) IVTNUM                                          06890253
      IF (ICZERO) 10140, 0151, 20140                                    06900253
10140 IVPASS = IVPASS + 1                                               06910253
      WRITE (I02,80002) IVTNUM                                          06920253
      GO TO 0151                                                        06930253
20140 IVFAIL = IVFAIL + 1                                               06940253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06950253
 0151 CONTINUE                                                          06960253
C                                                                       06970253
C        THE NEXT TWO TESTS ARE TO CHECK THE COUNTERS IN IF-LEVEL 2 AND 06980253
C     IF-LEVEL 3 RESPECTIVELY IN THE PREVIOUS TEST.                     06990253
C                                                                       07000253
C                                                                       07010253
C                                                                       07020253
C     ****  FCVS PROGRAM 253  -  TEST 015  ****                         07030253
C                                                                       07040253
C     TEST 015 CHECKS THAT THE INTEGER COUNTER IN IF-LEVEL 2 IN THE     07050253
C     PREVIOUS TEST IS EQUAL TO TEN (10).                               07060253
C                                                                       07070253
C                                                                       07080253
      IVTNUM =  15                                                      07090253
      IF (ICZERO) 30150, 0150, 30150                                    07100253
 0150 CONTINUE                                                          07110253
      IVCOMP = IVON02                                                   07120253
      IVCORR = 10                                                       07130253
40150 IF ( IVCOMP - 10 ) 20150, 10150, 20150                            07140253
30150 IVDELE = IVDELE + 1                                               07150253
      WRITE (I02,80000) IVTNUM                                          07160253
      IF (ICZERO) 10150, 0161, 20150                                    07170253
10150 IVPASS = IVPASS + 1                                               07180253
      WRITE (I02,80002) IVTNUM                                          07190253
      GO TO 0161                                                        07200253
20150 IVFAIL = IVFAIL + 1                                               07210253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07220253
 0161 CONTINUE                                                          07230253
C                                                                       07240253
C     ****  FCVS PROGRAM 253  -  TEST 016  ****                         07250253
C                                                                       07260253
C        TEST 016 CHECKS THAT THE INTEGER COUNTER IN IF-LEVEL 3 IN THE  07270253
C     PREVIOUS TEST IS EQUAL TO TEN (10).                               07280253
C                                                                       07290253
C                                                                       07300253
      IVTNUM =  16                                                      07310253
      IF (ICZERO) 30160, 0160, 30160                                    07320253
 0160 CONTINUE                                                          07330253
      IVCOMP = IVON03                                                   07340253
      IVCORR = 10                                                       07350253
40160 IF ( IVCOMP - 10 )  20160, 10160, 20160                           07360253
30160 IVDELE = IVDELE + 1                                               07370253
      WRITE (I02,80000) IVTNUM                                          07380253
      IF (ICZERO) 10160, 0171, 20160                                    07390253
10160 IVPASS = IVPASS + 1                                               07400253
      WRITE (I02,80002) IVTNUM                                          07410253
      GO TO 0171                                                        07420253
20160 IVFAIL = IVFAIL + 1                                               07430253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07440253
 0171 CONTINUE                                                          07450253
C                                                                       07460253
C        THE NEXT THREE TESTS ARE SIMILAR TO THE PREVIOUS THREE TESTS   07470253
C     IN THAT THEY TEST THE TRANSFER OF CONTROL WITHIN A THREE LEVEL    07480253
C     BLOCK IF STRUCTURE.  EACH OF THE IF-LEVELS ARE EXECUTED AS IF THEY07490253
C     WERE A LOOP USING LOGICAL IF STATEMENTS WITHIN EACH IF-LEVEL.     07500253
C                                                                       07510253
C                                                                       07520253
C                                                                       07530253
C     ****  FCVS PROGRAM 253  -  TEST 017  ****                         07540253
C                                                                       07550253
C        TEST 017 CHECKS THAT THE VALUE OF THE INTEGER COUNTER IVON04 IN07560253
C     IF-LEVEL 1 IN THIS TEST EQUALS 10.                                07570253
C                                                                       07580253
C                                                                       07590253
      IVTNUM =  17                                                      07600253
      IF (ICZERO) 30170, 0170, 30170                                    07610253
 0170 CONTINUE                                                          07620253
      IVCOMP = 0                                                        07630253
      IVON01 = 0                                                        07640253
      IVON02 = 0                                                        07650253
      IVON03 = 0                                                        07660253
      IVON04 = 0                                                        07670253
      IVON05 = 0                                                        07680253
      IVON06 = 0                                                        07690253
C                                                                       07700253
 0172 IF ( IVON01 .LT. 10 )  THEN                                       07710253
           IVON01 = IVON01 + 1                                          07720253
           IVON04 = IVON04 + 1                                          07730253
           IF ( IVON01 .GT. 11 )  GO TO 0175                            07740253
 0173      IF ( IVON02 .LT. 10 )  THEN                                  07750253
                IVON02 = IVON02 + 1                                     07760253
                IVON05 = IVON05 + 1                                     07770253
               IF ( IVON02 .GT. 11 )  GO TO 0175                        07780253
 0174           IF ( IVON03 .LT. 10 )  THEN                             07790253
                     IVON03 = IVON03 + 1                                07800253
                     IVON06 = IVON06 + 1                                07810253
                     IF ( IVON03 .GT. 11 )  GO TO 0175                  07820253
                     IF ( IVON03 .LE. 10 )  GO TO 0174                  07830253
                END IF                                                  07840253
                IVON03 = 0                                              07850253
                IF ( IVON02 .LE. 10 )  GO TO 0173                       07860253
           END IF                                                       07870253
           IVON02 = 0                                                   07880253
           IF ( IVON01 .LE. 10 )  GO TO 0172                            07890253
      END IF                                                            07900253
 0175 CONTINUE                                                          07910253
      IVCOMP = IVON04                                                   07920253
      IVCORR = 10                                                       07930253
40170 IF ( IVCOMP - 10 )  20170, 10170, 20170                           07940253
30170 IVDELE = IVDELE + 1                                               07950253
      WRITE (I02,80000) IVTNUM                                          07960253
      IF (ICZERO) 10170, 0181, 20170                                    07970253
10170 IVPASS = IVPASS + 1                                               07980253
      WRITE (I02,80002) IVTNUM                                          07990253
      GO TO 0181                                                        08000253
20170 IVFAIL = IVFAIL + 1                                               08010253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08020253
 0181 CONTINUE                                                          08030253
C                                                                       08040253
C     ****  FCVS PROGRAM 253  -  TEST 018  ****                         08050253
C                                                                       08060253
C        TEST 018 CHECKS THAT THE VALUE OF THE INTEGER COUNTER IVON05 IN08070253
C     IF-LEVEL 2 OF THE PREVIOUS TEST EQUALS 100.                       08080253
C                                                                       08090253
C                                                                       08100253
      IVTNUM =  18                                                      08110253
      IF (ICZERO) 30180, 0180, 30180                                    08120253
 0180 CONTINUE                                                          08130253
      IVCOMP = IVON05                                                   08140253
      IVCORR = 100                                                      08150253
40180 IF ( IVCOMP - 100 )  20180, 10180, 20180                          08160253
30180 IVDELE = IVDELE + 1                                               08170253
      WRITE (I02,80000) IVTNUM                                          08180253
      IF (ICZERO) 10180, 0191, 20180                                    08190253
10180 IVPASS = IVPASS + 1                                               08200253
      WRITE (I02,80002) IVTNUM                                          08210253
      GO TO 0191                                                        08220253
20180 IVFAIL = IVFAIL + 1                                               08230253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08240253
 0191 CONTINUE                                                          08250253
C                                                                       08260253
C     ****  FCVS PROGRAM 253  -  TEST 019  ****                         08270253
C                                                                       08280253
C        TEST 019 CHECKS THAT THE VALUE OF THE INTEGER COUNTER IVON06 IN08290253
C     IF-LEVEL 3 OF THE PREVIOUS TEST EQUALS 1000.                      08300253
C                                                                       08310253
C                                                                       08320253
      IVTNUM =  19                                                      08330253
      IF (ICZERO) 30190, 0190, 30190                                    08340253
 0190 CONTINUE                                                          08350253
      IVCOMP = IVON06                                                   08360253
      IVCORR = 1000                                                     08370253
40190 IF ( IVCOMP - 1000 )  20190, 10190, 20190                         08380253
30190 IVDELE = IVDELE + 1                                               08390253
      WRITE (I02,80000) IVTNUM                                          08400253
      IF (ICZERO) 10190, 0201, 20190                                    08410253
10190 IVPASS = IVPASS + 1                                               08420253
      WRITE (I02,80002) IVTNUM                                          08430253
      GO TO 0201                                                        08440253
20190 IVFAIL = IVFAIL + 1                                               08450253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08460253
 0201 CONTINUE                                                          08470253
C                                                                       08480253
C     ****  FCVS PROGRAM 253  -  TEST 020  ****                         08490253
C                                                                       08500253
C        TEST 020 USES A LOGICAL STATEMENT FUNCTION  LFIS01(L) AS       08510253
C     THE LOGICAL EXPRESSION IN A BLOCK IF STRUCTURE.  THE LOGICAL      08520253
C     STATEMENT FUNCTION TAKES THE LOGICAL COMPLEMENT OF THE LOGICAL    08530253
C     VALUE SUPPLIED.  THE VALUE OF .FALSE. IS SUPPLIED AND THE LOGICAL 08540253
C     VALUE OF .TRUE. SHOULD BE RETURNED AS THE LOGICAL FUNCTION        08550253
C     REFERENCE.  THE IF-BLOCK OF LEVEL ONE SHOULD BE EXECUTED.         08560253
C                                                                       08570253
C                                                                       08580253
      IVTNUM =  20                                                      08590253
      IF (ICZERO) 30200, 0200, 30200                                    08600253
 0200 CONTINUE                                                          08610253
      IVCOMP = 0                                                        08620253
      LVON01 = .FALSE.                                                  08630253
      IF ( LFIS01( LVON01 ) )  THEN                                     08640253
           IVCOMP = 1                                                   08650253
      END IF                                                            08660253
      IVCORR = 1                                                        08670253
40200 IF ( IVCOMP - 1 )  20200, 10200, 20200                            08680253
30200 IVDELE = IVDELE + 1                                               08690253
      WRITE (I02,80000) IVTNUM                                          08700253
      IF (ICZERO) 10200, 0211, 20200                                    08710253
10200 IVPASS = IVPASS + 1                                               08720253
      WRITE (I02,80002) IVTNUM                                          08730253
      GO TO 0211                                                        08740253
20200 IVFAIL = IVFAIL + 1                                               08750253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08760253
 0211 CONTINUE                                                          08770253
C                                                                       08780253
C                                                                       08790253
C        THE FOLLOWING SERIES OF TESTS ARE TO CHECK THE PRECEDENCE OF   08800253
C     OPERATORS.  THESE INCLUDE ARITHMETIC, RELATIONAL, AND LOGICAL     08810253
C     OPERATORS.  ARITHMETIC OPERATORS ARE CHECKED FIRST FROM THE       08820253
C     EVALUATION OF CERTAIN ARITHMETIC EXPRESSIONS THAT USE ONLY INTEGER08830253
C     VALUES IN THE COMPUTATIONS.  ALL INTERMEDIATE AND FINAL VALUES ARE08840253
C     LESS THAN 32767.  AFTER EACH OF THE STATEMENTS IS TESTED BY ITSELF08850253
C     THEN THE RELATIONAL OPERATORS ARE TESTED USING THE INTEGER VALUES 08860253
C     OBTAINED IN EACH OF THE ARITHMETIC EXPRESSIONS.  IN THIS TEST THE 08870253
C     RELATIONAL EXPRESSIONS ARE COMBINED WITH LOGICAL OPERATORS TO     08880253
C     PRODUCE A LOGICAL EXPRESSION.  FINALLY THE ENTIRE SET OF SIX (6)  08890253
C     ARITHMETIC , RELATIONAL, AND LOGICAL EXPRESSIONS IS COMBINED INTO 08900253
C     ONE LOGICAL IF STATEMENT.                                         08910253
C                                                                       08920253
C                                                                       08930253
C                                                                       08940253
C     ****  FCVS PROGRAM 253  -  TEST 021  ****                         08950253
C                                                                       08960253
C        TEST 021 CHECKS THE ORDER OF EVALUATION WHEN AN ARITHMETIC     08970253
C     EXPRESSION HAS PARENTHESES AND A SERIES OF EXPONENTIATION.  THE   08980253
C     ORDER OF EVALUATION IS SHOWN BELOW -                              08990253
C                                                                       09000253
C        1 + 2 * ( 4 - 2 ) ** 2 ** 3 - 4 / 2                            09010253
C        1 + 2 * ( 2 ) ** 2 ** 3 - 4 / 2                                09020253
C        1 + 2 * 2 ** 8 - 4 / 2                                         09030253
C        1 + 2 * 256 - 4 / 2                                            09040253
C        1 + 512 - 2                                                    09050253
C        513 - 2                                                        09060253
C        511                                                            09070253
C                                                                       09080253
      IVTNUM =  21                                                      09090253
      IF (ICZERO) 30210, 0210, 30210                                    09100253
 0210 CONTINUE                                                          09110253
      IVON01 = 1                                                        09120253
      IVON02 = 2                                                        09130253
      IVON03 = 4                                                        09140253
      IVON04 = 2                                                        09150253
      IVON05 = 4                                                        09160253
      IVON06 = 2                                                        09170253
      IVCOMP = IVON01 + IVON02 * ( IVON03 - IVON04 ) ** 2 ** 3 - IVON05 09180253
     1         / IVON06                                                 09190253
      IVCORR = 511                                                      09200253
40210 IF ( IVCOMP - 511 )  20210, 10210, 20210                          09210253
30210 IVDELE = IVDELE + 1                                               09220253
      WRITE (I02,80000) IVTNUM                                          09230253
      IF (ICZERO) 10210, 0221, 20210                                    09240253
10210 IVPASS = IVPASS + 1                                               09250253
      WRITE (I02,80002) IVTNUM                                          09260253
      GO TO 0221                                                        09270253
20210 IVFAIL = IVFAIL + 1                                               09280253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09290253
 0221 CONTINUE                                                          09300253
C                                                                       09310253
C     ****  FCVS PROGRAM 253  -  TEST 022  ****                         09320253
C                                                                       09330253
C        TEST 022 IS A SERIES OF DIVISIONS FOLLOWED BY A SERIES OF      09340253
C     MULTIPLICATIONS ALL WITHOUT ANY PARENTHESES.                      09350253
C                                                                       09360253
C        16 / 2 / 2 / 2 * 4 * 8                                         09370253
C        8 / 2 / 2 * 4 * 8                                              09380253
C        4 / 2 * 4 * 8                                                  09390253
C        2 * 4 * 8                                                      09400253
C        8 * 8                                                          09410253
C        64                                                             09420253
C                                                                       09430253
C                                                                       09440253
      IVTNUM =  22                                                      09450253
      IF (ICZERO) 30220, 0220, 30220                                    09460253
 0220 CONTINUE                                                          09470253
      IVON07 = 16                                                       09480253
      IVON08 = 2                                                        09490253
      IVON09 = 2                                                        09500253
      IVON10 = 2                                                        09510253
      IVON11 = 4                                                        09520253
      IVON12 = 8                                                        09530253
      IVCOMP = IVON07 / IVON08 / IVON09 / IVON10 * IVON11 * IVON12      09540253
      IVCORR = 64                                                       09550253
40220 IF ( IVCOMP - 64 ) 20220, 10220, 20220                            09560253
30220 IVDELE = IVDELE + 1                                               09570253
      WRITE (I02,80000) IVTNUM                                          09580253
      IF (ICZERO) 10220, 0231, 20220                                    09590253
10220 IVPASS = IVPASS + 1                                               09600253
      WRITE (I02,80002) IVTNUM                                          09610253
      GO TO 0231                                                        09620253
20220 IVFAIL = IVFAIL + 1                                               09630253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09640253
 0231 CONTINUE                                                          09650253
C                                                                       09660253
C     ****  FCVS PROGRAM 253  -  TEST 023  ****                         09670253
C                                                                       09680253
C        TEST 023 HAS ONE SUBTRACTION IMBEDDED IN A SERIES OF ADDITIONS 09690253
C     WITHOUT ANY PARENTHESES.                                          09700253
C                                                                       09710253
C        3 + 4 - 1 + 5                                                  09720253
C        7 - 1 + 5                                                      09730253
C        6 + 5                                                          09740253
C        11                                                             09750253
C                                                                       09760253
C                                                                       09770253
      IVTNUM =  23                                                      09780253
      IF (ICZERO) 30230, 0230, 30230                                    09790253
 0230 CONTINUE                                                          09800253
      IVON13 = 3                                                        09810253
      IVON14 = 4                                                        09820253
      IVON15 = 1                                                        09830253
      IVON16 = 5                                                        09840253
      IVCOMP = IVON13 + IVON14 - IVON15 + IVON16                        09850253
      IVCORR = 11                                                       09860253
40230 IF ( IVCOMP - 11 )  20230, 10230, 20230                           09870253
30230 IVDELE = IVDELE + 1                                               09880253
      WRITE (I02,80000) IVTNUM                                          09890253
      IF (ICZERO) 10230, 0241, 20230                                    09900253
10230 IVPASS = IVPASS + 1                                               09910253
      WRITE (I02,80002) IVTNUM                                          09920253
      GO TO 0241                                                        09930253
20230 IVFAIL = IVFAIL + 1                                               09940253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09950253
 0241 CONTINUE                                                          09960253
C                                                                       09970253
C     ****  FCVS PROGRAM 253  -  TEST 024  ****                         09980253
C                                                                       09990253
C        TEST 024 HAS ADDITION, SUBTRACTION, MULTIPLICATION, DIVISION,  10000253
C     AND EXPONENTIATION WITHOUT PARENTHESES.                           10010253
C                                                                       10020253
C        4 + 4 - 6 * 3 / 3 ** 2                                         10030253
C        4 + 4 - 6 * 3 / 9                                              10040253
C        4 + 4 - 18 / 9                                                 10050253
C        4 + 4 - 2                                                      10060253
C        8 - 2                                                          10070253
C        6                                                              10080253
C                                                                       10090253
C                                                                       10100253
      IVTNUM =  24                                                      10110253
      IF (ICZERO) 30240, 0240, 30240                                    10120253
 0240 CONTINUE                                                          10130253
      IVON17 = 4                                                        10140253
      IVON18 = 4                                                        10150253
      IVON19 = 6                                                        10160253
      IVON20 = 3                                                        10170253
      IVON21 = 3                                                        10180253
      IVON22 = 2                                                        10190253
      IVCOMP = IVON17 + IVON18 - IVON19 * IVON20 / IVON21 ** IVON22     10200253
      IVCORR = 6                                                        10210253
40240 IF ( IVCOMP - 6 )  20240, 10240, 20240                            10220253
30240 IVDELE = IVDELE + 1                                               10230253
      WRITE (I02,80000) IVTNUM                                          10240253
      IF (ICZERO) 10240, 0251, 20240                                    10250253
10240 IVPASS = IVPASS + 1                                               10260253
      WRITE (I02,80002) IVTNUM                                          10270253
      GO TO 0251                                                        10280253
20240 IVFAIL = IVFAIL + 1                                               10290253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10300253
 0251 CONTINUE                                                          10310253
C                                                                       10320253
C     ****  FCVS PROGRAM 253  -  TEST 025  ****                         10330253
C                                                                       10340253
C        TEST 025 IS LIKE TEST NUMBER 021 EXCEPT THAT THE PARENTHESES   10350253
C     HAVE BEEN REMOVED.  THE INTEGER VALUES USED AS INPUT ARE THE SAME.10360253
C     REMOVAL OF THE PARENTHESES CHANGES THE ORDER OF EVALUATION SO THE 10370253
C     FINAL INTEGER RESULT IS DIFFERENT.                                10380253
C                                                                       10390253
C        1 + 2 * 4 - 2 ** 2 ** 3 - 4 / 2                                10400253
C        1 + 2 * 4 - 2 ** 8 - 4 / 2                                     10410253
C        1 + 2 * 4 - 256 - 4 / 2                                        10420253
C        1 + 8 - 256 - 2                                                10430253
C        9 - 256 - 2                                                    10440253
C        -247 - 2                                                       10450253
C        -249                                                           10460253
C                                                                       10470253
      IVTNUM =  25                                                      10480253
      IF (ICZERO) 30250, 0250, 30250                                    10490253
 0250 CONTINUE                                                          10500253
      IVON23 = 1                                                        10510253
      IVON24 = 2                                                        10520253
      IVON25 = 4                                                        10530253
      IVON26 = 2                                                        10540253
      IVON27 = 4                                                        10550253
      IVON28 = 2                                                        10560253
      IVCOMP = IVON23 + IVON24 * IVON25 - IVON26 ** 2 ** 3 - IVON27     10570253
     1          / IVON28                                                10580253
      IVCORR = -249                                                     10590253
40250 IF ( IVCOMP + 249 )  20250, 10250, 20250                          10600253
30250 IVDELE = IVDELE + 1                                               10610253
      WRITE (I02,80000) IVTNUM                                          10620253
      IF (ICZERO) 10250, 0261, 20250                                    10630253
10250 IVPASS = IVPASS + 1                                               10640253
      WRITE (I02,80002) IVTNUM                                          10650253
      GO TO 0261                                                        10660253
20250 IVFAIL = IVFAIL + 1                                               10670253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10680253
 0261 CONTINUE                                                          10690253
C                                                                       10700253
C     ****  FCVS PROGRAM 253  -  TEST 026  ****                         10710253
C                                                                       10720253
C        TEST 026 IS JUST LIKE TEST NUMBER 022 EXCEPT THAT PARENTHESES  10730253
C     HAVE BEEN ADDED.  ALTHOUGH THE INTEGER VALUES ARE THE SAME, THE   10740253
C     PARENTHESES CHANGE THE ORDER OF EVALUATION SO THAT THE FINAL      10750253
C     INTEGER RESULT IS DIFFERENT.                                      10760253
C                                                                       10770253
C        16 / ( 2 / 2 ) / 2 * ( 4 * 8 )                                 10780253
C        16 / ( 1 ) / 2 * ( 32 )                                        10790253
C        16 / 2 * 32                                                    10800253
C        8 * 32                                                         10810253
C        256                                                            10820253
C                                                                       10830253
      IVTNUM =  26                                                      10840253
      IF (ICZERO) 30260, 0260, 30260                                    10850253
 0260 CONTINUE                                                          10860253
      IVON29 = 16                                                       10870253
      IVON30 = 2                                                        10880253
      IVON31 = 2                                                        10890253
      IVON32 = 2                                                        10900253
      IVON33 = 4                                                        10910253
      IVON34 = 8                                                        10920253
      IVCOMP = IVON29 / ( IVON30 / IVON31 ) / IVON32 * ( IVON33 *       10930253
     1          IVON34 )                                                10940253
      IVCORR = 256                                                      10950253
40260 IF ( IVCOMP - 256 )  20260, 10260, 20260                          10960253
30260 IVDELE = IVDELE + 1                                               10970253
      WRITE (I02,80000) IVTNUM                                          10980253
      IF (ICZERO) 10260, 0271, 20260                                    10990253
10260 IVPASS = IVPASS + 1                                               11000253
      WRITE (I02,80002) IVTNUM                                          11010253
      GO TO 0271                                                        11020253
20260 IVFAIL = IVFAIL + 1                                               11030253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11040253
 0271 CONTINUE                                                          11050253
C                                                                       11060253
C     ****  FCVS PROGRAM 253  -  TEST 027  ****                         11070253
C                                                                       11080253
C        TEST 027 COMBINES THE INTEGER RESULTS OBTAINED IN THE PREVIOUS 11090253
C     SIX TESTS AND USES RELATIONAL AND LOGICAL OPERATORS IN ONE        11100253
C     LOGICAL EXPRESSION.  RELATIONAL EXPRESSIONS ARE EVALUATED FIRST   11110253
C     FOLLOWED BY THE LOGICAL OPERATORS .NOT. , .AND., AND .OR. IN THAT 11120253
C     ORDER.                                                            11130253
C                                                                       11140253
C        511 .LT. 64 .OR. .NOT. 11 .LE. 6 .AND. -249 .LE. 256           11150253
C        F .OR. .NOT. F .AND. T                                         11160253
C        F .OR. T .AND. T                                               11170253
C        F .OR. T                                                       11180253
C        T                                                              11190253
C                                                                       11200253
C                                                                       11210253
      IVTNUM =  27                                                      11220253
      IF (ICZERO) 30270, 0270, 30270                                    11230253
 0270 CONTINUE                                                          11240253
      IVON35 = 511                                                      11250253
      IVON36 = 64                                                       11260253
      IVON37 = 11                                                       11270253
      IVON38 = 6                                                        11280253
      IVON39 = -249                                                     11290253
      IVON40 = 256                                                      11300253
      IVCOMP = 0                                                        11310253
      LVON01 = IVON35 .LT. IVON36 .OR. .NOT. IVON37 .LE. IVON38 .AND.   11320253
     1          IVON39 .LE. IVON40                                      11330253
      IF ( LVON01 )  IVCOMP = 1                                         11340253
      IVCORR = 1                                                        11350253
40270 IF ( IVCOMP - 1 )  20270, 10270, 20270                            11360253
30270 IVDELE = IVDELE + 1                                               11370253
      WRITE (I02,80000) IVTNUM                                          11380253
      IF (ICZERO) 10270, 0281, 20270                                    11390253
10270 IVPASS = IVPASS + 1                                               11400253
      WRITE (I02,80002) IVTNUM                                          11410253
      GO TO 0281                                                        11420253
20270 IVFAIL = IVFAIL + 1                                               11430253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11440253
 0281 CONTINUE                                                          11450253
C                                                                       11460253
C     ****  FCVS PROGRAM 253  -  TEST 028  ****                         11470253
C                                                                       11480253
C        TEST 028 IS THE BIGGIE.  IT COMBINES ALL OF THE INTEGER VALUES 11490253
C     AND RESULTS IN THE PREVIOUS SEVEN (7) TESTS.  IF THERE WERE ANY   11500253
C     ERRORS IN ANY OF THE PREVIOUS SEVEN TESTS, THEN THIS TEST SHOULD  11510253
C     ALSO FAIL.                                                        11520253
C                                                                       11530253
C                                                                       11540253
      IVTNUM =  28                                                      11550253
      IF (ICZERO) 30280, 0280, 30280                                    11560253
 0280 CONTINUE                                                          11570253
      IVCOMP = 0                                                        11580253
      IF ( IVON01 + IVON02 * ( IVON03 - IVON04 ) ** 2 ** 3 - IVON05 /   11590253
     1IVON06 .LT. IVON07 / IVON08 / IVON09 / IVON10 * IVON11 * IVON12   11600253
     2.OR. .NOT. IVON13 + IVON14 - IVON15 + IVON16 .LE. IVON17 + IVON18 11610253
     3- IVON19 * IVON20 / IVON21 ** IVON22 .AND. IVON23 + IVON24 *      11620253
     4IVON25 - IVON26 ** 2 ** 3 - IVON27 / IVON28 .LE. IVON29 / ( IVON3011630253
     5 / IVON31 ) / IVON32 * ( IVON33 * IVON34 ) )  IVCOMP = 1          11640253
      IVCORR = 1                                                        11650253
40280 IF ( IVCOMP - 1 )  20280, 10280, 20280                            11660253
30280 IVDELE = IVDELE + 1                                               11670253
      WRITE (I02,80000) IVTNUM                                          11680253
      IF (ICZERO) 10280, 0291, 20280                                    11690253
10280 IVPASS = IVPASS + 1                                               11700253
      WRITE (I02,80002) IVTNUM                                          11710253
      GO TO 0291                                                        11720253
20280 IVFAIL = IVFAIL + 1                                               11730253
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11740253
 0291 CONTINUE                                                          11750253
C                                                                       11760253
C                                                                       11770253
C     WRITE OUT TEST SUMMARY                                            11780253
C                                                                       11790253
      WRITE (I02,90004)                                                 11800253
      WRITE (I02,90014)                                                 11810253
      WRITE (I02,90004)                                                 11820253
      WRITE (I02,90000)                                                 11830253
      WRITE (I02,90004)                                                 11840253
      WRITE (I02,90020) IVFAIL                                          11850253
      WRITE (I02,90022) IVPASS                                          11860253
      WRITE (I02,90024) IVDELE                                          11870253
      STOP                                                              11880253
90001 FORMAT (1H ,24X,5HFM253)                                          11890253
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM253)                          11900253
C                                                                       11910253
C     FORMATS FOR TEST DETAIL LINES                                     11920253
C                                                                       11930253
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   11940253
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      11950253
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         11960253
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    11970253
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        11980253
C                                                                       11990253
C     FORMAT STATEMENTS FOR PAGE HEADERS                                12000253
C                                                                       12010253
90002 FORMAT (1H1)                                                      12020253
90004 FORMAT (1H )                                                      12030253
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            12040253
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   12050253
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         12060253
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  12070253
90014 FORMAT (1H ,5X,46H----------------------------------------------) 12080253
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             12090253
C                                                                       12100253
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 12110253
C                                                                       12120253
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              12130253
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              12140253
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             12150253
      END                                                               12160253
