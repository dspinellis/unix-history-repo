      PROGRAM FM254                                                     00010254
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020254
C                                                                       00030254
C                                                                       00040254
C        THIS ROUTINE IS A TEST OF THE ELSE IF-BLOCK.  TESTS WITHIN THIS00050254
C     ROUTINE ARE FOR THE SYNTAX OF THE BASIC ELSE IF STATEMENT AND     00060254
C     ELSE IF-BLOCK STRUCTURE.                                          00070254
C                                                                       00080254
C     REFERENCES                                                        00090254
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00100254
C            X3.9-1977                                                  00110254
C        SECTION 11.7,       ELSE IF STATEMENT                          00120254
C        SECTION 11.7.1,     ELSE IF-BLOCK                              00130254
C        SECTION 11.7.2,     EXECUTION OF THE ELSE IF STATEMENT         00140254
C                                                                       00150254
C                                                                       00160254
C                                                                       00170254
C                                                                       00180254
C     ******************************************************************00190254
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00200254
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00210254
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00220254
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00230254
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00240254
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00250254
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00260254
C     THE RESULT OF EXECUTING THESE TESTS.                              00270254
C                                                                       00280254
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00290254
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00300254
C                                                                       00310254
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00320254
C                    DEPARTMENT OF THE NAVY                             00330254
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00340254
C                    WASHINGTON, D.C.   20376                           00350254
C                                                                       00360254
C     ******************************************************************00370254
C                                                                       00380254
C                                                                       00390254
      IMPLICIT LOGICAL (L)                                              00400254
      IMPLICIT CHARACTER*14 (C)                                         00410254
C                                                                       00420254
      DIMENSION LADN11(2)                                               00430254
      LOGICAL LVTN01, LVTN02, LATN11(2), LADN11                         00440254
      DATA LADN11/.TRUE., .FALSE./                                      00450254
C                                                                       00460254
C                                                                       00470254
C     **** LOGICAL STATEMENT FUNCTION REFERENCED IN TEST 4   ****       00480254
C                                                                       00490254
      LFIS01 ( L ) = L .AND. L                                          00500254
C                                                                       00510254
C                                                                       00520254
C                                                                       00530254
C                                                                       00540254
C                                                                       00550254
C     INITIALIZATION SECTION.                                           00560254
C                                                                       00570254
C     INITIALIZE CONSTANTS                                              00580254
C     ********************                                              00590254
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00600254
      I01 = 5                                                           00610254
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00620254
      I02 = 6                                                           00630254
C     SYSTEM ENVIRONMENT SECTION                                        00640254
C                                                                       00650254
      I01 = 5                                                           00660254
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670254
C     (UNIT NUMBER FOR CARD READER).                                    00680254
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00690254
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00700254
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00710254
C                                                                       00720254
      I02 = 6                                                           00730254
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00740254
C     (UNIT NUMBER FOR PRINTER).                                        00750254
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00760254
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00770254
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00780254
C                                                                       00790254
      IVPASS = 0                                                        00800254
      IVFAIL = 0                                                        00810254
      IVDELE = 0                                                        00820254
      ICZERO = 0                                                        00830254
C                                                                       00840254
C     WRITE OUT PAGE HEADERS                                            00850254
C                                                                       00860254
      WRITE (I02,90002)                                                 00870254
      WRITE (I02,90006)                                                 00880254
      WRITE (I02,90008)                                                 00890254
      WRITE (I02,90004)                                                 00900254
      WRITE (I02,90010)                                                 00910254
      WRITE (I02,90004)                                                 00920254
      WRITE (I02,90016)                                                 00930254
      WRITE (I02,90001)                                                 00940254
      WRITE (I02,90004)                                                 00950254
      WRITE (I02,90012)                                                 00960254
      WRITE (I02,90014)                                                 00970254
      WRITE (I02,90004)                                                 00980254
C                                                                       00990254
C                                                                       01000254
C                                                                       01010254
C        THE SYNTAX OF THE ELSE IF STATEMENTS IN THE TESTS TO FOLLOW IS 01020254
C                                                                       01030254
C        IF ( E1 )  THEN                                                01040254
C             IF-BLOCK                                                  01050254
C        ELSE IF ( E2 )  THEN                                           01060254
C             ELSE IF-BLOCK                                             01070254
C        END IF                                                         01080254
C                                                                       01090254
C     THE NEXT FOUR TESTS WILL USE THE FOLLOWING COMBINATIONS OF TRUE   01100254
C     AND FALSE FOR E1 AND E2 AS SHOWN BELOW -                          01110254
C        TEST NUMBER    1    2    3    4                                01120254
C             E1        F    F    T    T                                01130254
C             E2        T    F    T    F                                01140254
C                                                                       01150254
C                                                                       01160254
C                                                                       01170254
C                                                                       01180254
C     ****  FCVS PROGRAM 254  -  TEST 001  ****                         01190254
C                                                                       01200254
C        TEST 001 USES A VERY SIMPLE ELSE IF STATEMENT.  THE EXPRESSION 01210254
C     WITHIN THE PARENTHESES IS THE LOGICAL CONSTANT  .TRUE. AND THE    01220254
C     EXECUTABLE STATEMENT WITHIN THE ELSE IF-BLOCK OF LEVEL ONE IS AN  01230254
C     INTEGER ARITHMETIC ASSIGNMENT STATEMENT.  IN THIS TEST THE LOGICAL01240254
C     EXPRESSION E1 IS .FALSE. SO THE IF-BLOCK SHOULD NOT BE EXECUTED.  01250254
C     THE LOGICAL EXPRESSION E2 IS .TRUE. SO THE ELSE IF-BLOCK SHOULD   01260254
C     BE EXECUTED.                                                      01270254
C                                                                       01280254
C                                                                       01290254
      IVTNUM =   1                                                      01300254
      IF (ICZERO) 30010, 0010, 30010                                    01310254
 0010 CONTINUE                                                          01320254
      IVCOMP = 1                                                        01330254
      IF ( .FALSE. )  THEN                                              01340254
           IVCOMP = IVCOMP * 2                                          01350254
      ELSE IF ( .TRUE. )  THEN                                          01360254
           IVCOMP = IVCOMP * 3                                          01370254
      END IF                                                            01380254
C                                                                       01390254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3                01400254
C                                                                       01410254
      IVCORR = 3                                                        01420254
40010 IF ( IVCOMP - 3 )  20010, 10010, 20010                            01430254
30010 IVDELE = IVDELE + 1                                               01440254
      WRITE (I02,80000) IVTNUM                                          01450254
      IF (ICZERO) 10010, 0021, 20010                                    01460254
10010 IVPASS = IVPASS + 1                                               01470254
      WRITE (I02,80002) IVTNUM                                          01480254
      GO TO 0021                                                        01490254
20010 IVFAIL = IVFAIL + 1                                               01500254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01510254
 0021 CONTINUE                                                          01520254
C                                                                       01530254
C     ****  FCVS PROGRAM 254  -  TEST 002  ****                         01540254
C                                                                       01550254
C        TEST 002 HAS E1 .FALSE. AND E2 .FALSE..  NEITHER THE IF-BLOCK  01560254
C     NOR THE ELSE IF-BLOCK SHOULD BE EXECUTED.                         01570254
C                                                                       01580254
C                                                                       01590254
      IVTNUM =   2                                                      01600254
      IF (ICZERO) 30020, 0020, 30020                                    01610254
 0020 CONTINUE                                                          01620254
      IVCOMP = 1                                                        01630254
      LVON01 = .FALSE.                                                  01640254
      LVON02 = .FALSE.                                                  01650254
      IF ( LVON01 )  THEN                                               01660254
           IVCOMP = IVCOMP * 2                                          01670254
      ELSE IF ( LVON02 )  THEN                                          01680254
           IVCOMP = IVCOMP * 3                                          01690254
      END IF                                                            01700254
      IVCORR = 1                                                        01710254
40020 IF ( IVCOMP - 1 )  20020, 10020, 20020                            01720254
30020 IVDELE = IVDELE + 1                                               01730254
      WRITE (I02,80000) IVTNUM                                          01740254
      IF (ICZERO) 10020, 0031, 20020                                    01750254
10020 IVPASS = IVPASS + 1                                               01760254
      WRITE (I02,80002) IVTNUM                                          01770254
      GO TO 0031                                                        01780254
20020 IVFAIL = IVFAIL + 1                                               01790254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01800254
 0031 CONTINUE                                                          01810254
C                                                                       01820254
C     ****  FCVS PROGRAM 254  -  TEST 003  ****                         01830254
C                                                                       01840254
C        TEST 003 HAS E1 AS .TRUE. AND E2 AS .TRUE..  ONLY THE IF-BLOCK 01850254
C     SHOULD BE EXECUTED. THE ELSE IF-BLOCK SHOULD NOT BE EXECUTED.     01860254
C                                                                       01870254
C                                                                       01880254
      IVTNUM =   3                                                      01890254
      IF (ICZERO) 30030, 0030, 30030                                    01900254
 0030 CONTINUE                                                          01910254
      IVCOMP = 1                                                        01920254
      LVON01 = .TRUE.                                                   01930254
      LVON02 = .TRUE.                                                   01940254
      LVTN01 = LVON01                                                   01950254
      LVTN02 = LVON02                                                   01960254
      IF ( LVTN01 )  THEN                                               01970254
           IVCOMP = IVCOMP * 2                                          01980254
      ELSE IF ( LVTN02 )  THEN                                          01990254
           IVCOMP = IVCOMP * 3                                          02000254
      END IF                                                            02010254
C                                                                       02020254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****02030254
C                                                                       02040254
      IVCORR = 2                                                        02050254
40030 IF ( IVCOMP - 2 )  20030, 10030, 20030                            02060254
30030 IVDELE = IVDELE + 1                                               02070254
      WRITE (I02,80000) IVTNUM                                          02080254
      IF (ICZERO) 10030, 0041, 20030                                    02090254
10030 IVPASS = IVPASS + 1                                               02100254
      WRITE (I02,80002) IVTNUM                                          02110254
      GO TO 0041                                                        02120254
20030 IVFAIL = IVFAIL + 1                                               02130254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02140254
 0041 CONTINUE                                                          02150254
C                                                                       02160254
C     ****  FCVS PROGRAM 254  -  TEST 004  ****                         02170254
C                                                                       02180254
C        TEST 004 HAS E1 AS .TRUE. AND E2 AS .FALSE..  ONLY THE IF-BLOCK02190254
C     SHOULD BE EXECUTED.  THE ELSE IF-BLOCK SHOULD NOT BE EXECUTED.    02200254
C                                                                       02210254
C                                                                       02220254
      IVTNUM =   4                                                      02230254
      IF (ICZERO) 30040, 0040, 30040                                    02240254
 0040 CONTINUE                                                          02250254
      IVCOMP = 1                                                        02260254
      LVON01 = .TRUE.                                                   02270254
      LVTN01 = LFIS01 ( LVON01 )                                        02280254
      LVON02 = .FALSE.                                                  02290254
      IF ( LVTN01 )  THEN                                               02300254
           IVCOMP = IVCOMP * 2                                          02310254
      ELSE IF ( LFIS01 ( LVON02 ) )  THEN                               02320254
           IVCOMP = IVCOMP * 3                                          02330254
      END IF                                                            02340254
C                                                                       02350254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****02360254
C                                                                       02370254
      IVCORR = 2                                                        02380254
40040 IF ( IVCOMP - 2 )  20040, 10040, 20040                            02390254
30040 IVDELE = IVDELE + 1                                               02400254
      WRITE (I02,80000) IVTNUM                                          02410254
      IF (ICZERO) 10040, 0051, 20040                                    02420254
10040 IVPASS = IVPASS + 1                                               02430254
      WRITE (I02,80002) IVTNUM                                          02440254
      GO TO 0051                                                        02450254
20040 IVFAIL = IVFAIL + 1                                               02460254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02470254
 0051 CONTINUE                                                          02480254
C                                                                       02490254
C                                                                       02500254
C        THE SYNTAX OF THE ELSE IF STATEMENTS IN THE TESTS TO FOLLOW IS 02510254
C                                                                       02520254
C        IF ( E1 )  THEN                                                02530254
C             IF-BLOCK 1                                                02540254
C        ELSE IF ( E2 )  THEN                                           02550254
C             ELSE IF-BLOCK 1                                           02560254
C        ELSE IF ( E3 )  THEN                                           02570254
C             ELSE IF-BLOCK 2                                           02580254
C        END IF                                                         02590254
C                                                                       02600254
C                                                                       02610254
C                                                                       02620254
C     ****  FCVS PROGRAM 254  -  TEST 005  ****                         02630254
C                                                                       02640254
C        TEST 005 HAS E1 AS TRUE.  E2 AND E3 ARE FALSE.  ONLY IF-BLOCK 102650254
C     SHOULD BE EXECUTED.  ELSE IF-BLOCKS 1 AND 2 SHOULD NOT EXECUTE.   02660254
C                                                                       02670254
C                                                                       02680254
      IVTNUM =   5                                                      02690254
      IF (ICZERO) 30050, 0050, 30050                                    02700254
 0050 CONTINUE                                                          02710254
      IVCOMP = 1                                                        02720254
C     LADN11(1) IS SET TO .TRUE. IN A DATA STATEMENT.                   02730254
      LVON02 = .FALSE.                                                  02740254
      LVON03 = .FALSE.                                                  02750254
      IF ( LADN11(1) )  THEN                                            02760254
           IVCOMP = IVCOMP * 2                                          02770254
      ELSE IF ( LVON02 )  THEN                                          02780254
           IVCOMP = IVCOMP * 3                                          02790254
      ELSE IF ( LVON03 )  THEN                                          02800254
           IVCOMP = IVCOMP * 5                                          02810254
      END IF                                                            02820254
C                                                                       02830254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****02840254
C                                                                       02850254
      IVCORR = 2                                                        02860254
40050 IF ( IVCOMP - 2 )  20050, 10050, 20050                            02870254
30050 IVDELE = IVDELE + 1                                               02880254
      WRITE (I02,80000) IVTNUM                                          02890254
      IF (ICZERO) 10050, 0061, 20050                                    02900254
10050 IVPASS = IVPASS + 1                                               02910254
      WRITE (I02,80002) IVTNUM                                          02920254
      GO TO 0061                                                        02930254
20050 IVFAIL = IVFAIL + 1                                               02940254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02950254
 0061 CONTINUE                                                          02960254
C                                                                       02970254
C     ****  FCVS PROGRAM 254  -  TEST 006  ****                         02980254
C                                                                       02990254
C        TEST 006 HAS E1 AS FALSE, E2 AS TRUE, AND E3 AS FALSE.  ONLY   03000254
C     ELSE IF-BLOCK 1 SHOULD EXECUTE.  IF-BLOCK 1 AND ELSE IF-BLOCK 2   03010254
C     SHOULD NOT EXECUTE.                                               03020254
C                                                                       03030254
C                                                                       03040254
      IVTNUM =   6                                                      03050254
      IF (ICZERO) 30060, 0060, 30060                                    03060254
 0060 CONTINUE                                                          03070254
      IVCOMP = 1                                                        03080254
      LVON01 = .FALSE.                                                  03090254
      LATN11(2) = .TRUE.                                                03100254
      LVON03 = .FALSE.                                                  03110254
      IF ( LVON01 )  THEN                                               03120254
           IVCOMP = IVCOMP * 2                                          03130254
      ELSE IF ( LATN11(2) )  THEN                                       03140254
           IVCOMP = IVCOMP * 3                                          03150254
      ELSE IF ( LVON03 )  THEN                                          03160254
           IVCOMP = IVCOMP * 5                                          03170254
      END IF                                                            03180254
C                                                                       03190254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****03200254
C                                                                       03210254
      IVCORR = 3                                                        03220254
40060 IF ( IVCOMP - 3 )  20060, 10060, 20060                            03230254
30060 IVDELE = IVDELE + 1                                               03240254
      WRITE (I02,80000) IVTNUM                                          03250254
      IF (ICZERO) 10060, 0071, 20060                                    03260254
10060 IVPASS = IVPASS + 1                                               03270254
      WRITE (I02,80002) IVTNUM                                          03280254
      GO TO 0071                                                        03290254
20060 IVFAIL = IVFAIL + 1                                               03300254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03310254
 0071 CONTINUE                                                          03320254
C                                                                       03330254
C     ****  FCVS PROGRAM 254  -  TEST 007  ****                         03340254
C                                                                       03350254
C        TEST 007 HAS E1 AS FALSE, E2 AS FALSE, AND E3 AS TRUE.  ONLY   03360254
C     ELSE IF-BLOCK 2 SHOULD BE EXECUTED.  IF-BLOCK 1 AND ELSE IF-BLOCK 03370254
C     1 SHOULD NOT EXECUTE.                                             03380254
C                                                                       03390254
C                                                                       03400254
      IVTNUM =   7                                                      03410254
      IF (ICZERO) 30070, 0070, 30070                                    03420254
 0070 CONTINUE                                                          03430254
      IVCOMP = 1                                                        03440254
      LVON01 = .FALSE.                                                  03450254
      LVON02 = .FALSE.                                                  03460254
      LVON03 = .TRUE.                                                   03470254
      IF ( LVON01 )  THEN                                               03480254
           IVCOMP = IVCOMP * 2                                          03490254
      ELSE IF ( LVON02 )  THEN                                          03500254
           IVCOMP = IVCOMP * 3                                          03510254
      ELSE IF ( LVON03 )  THEN                                          03520254
           IVCOMP = IVCOMP * 5                                          03530254
      END IF                                                            03540254
C                                                                       03550254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 5 = 1 * 5            ****03560254
C                                                                       03570254
      IVCORR = 5                                                        03580254
40070 IF ( IVCOMP - 5 )  20070, 10070, 20070                            03590254
30070 IVDELE = IVDELE + 1                                               03600254
      WRITE (I02,80000) IVTNUM                                          03610254
      IF (ICZERO) 10070, 0081, 20070                                    03620254
10070 IVPASS = IVPASS + 1                                               03630254
      WRITE (I02,80002) IVTNUM                                          03640254
      GO TO 0081                                                        03650254
20070 IVFAIL = IVFAIL + 1                                               03660254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03670254
 0081 CONTINUE                                                          03680254
C                                                                       03690254
C     ****  FCVS PROGRAM 254  -  TEST 008  ****                         03700254
C                                                                       03710254
C        TEST 008 HAS E1 AS FALSE.  BOTH E2 AND E3 ARE TRUE.  ONLY ELSE 03720254
C     IF-BLOCK 1 SHOULD EXECUTE.  IF-BLOCK 1 AND ELSE IF-BLOCK 2 SHOULD 03730254
C     NOT EXECUTE.  THIS IS A TEST OF THE LOGIC FLOW WHEN ONE OF THE    03740254
C     EXPRESSIONS IN A STRING OF ELSE IF BLOCK STRUCTURES IS TRUE.  ONLY03750254
C     THAT PARTICULAR ELSE IF-BLOCK SHOULD BE EXECUTED.  THE REST OF THE03760254
C     STRING SHOULD BE SKIPPED.                                         03770254
C                                                                       03780254
C                                                                       03790254
      IVTNUM =   8                                                      03800254
      IF (ICZERO) 30080, 0080, 30080                                    03810254
 0080 CONTINUE                                                          03820254
      IVCOMP = 1                                                        03830254
      LVON01 = .FALSE.                                                  03840254
      LVON02 = .TRUE.                                                   03850254
      LVON03 = .TRUE.                                                   03860254
      IF ( LVON01 )  THEN                                               03870254
           IVCOMP = IVCOMP * 2                                          03880254
      ELSE IF ( LVON02 )  THEN                                          03890254
           IVCOMP = IVCOMP * 3                                          03900254
      ELSE IF ( LVON03 )  THEN                                          03910254
           IVCOMP = IVCOMP * 5                                          03920254
      END IF                                                            03930254
C                                                                       03940254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****03950254
C                                                                       03960254
      IVCORR = 3                                                        03970254
40080 IF ( IVCOMP - 3 )  20080, 10080, 20080                            03980254
30080 IVDELE = IVDELE + 1                                               03990254
      WRITE (I02,80000) IVTNUM                                          04000254
      IF (ICZERO) 10080, 0091, 20080                                    04010254
10080 IVPASS = IVPASS + 1                                               04020254
      WRITE (I02,80002) IVTNUM                                          04030254
      GO TO 0091                                                        04040254
20080 IVFAIL = IVFAIL + 1                                               04050254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04060254
 0091 CONTINUE                                                          04070254
C                                                                       04080254
C                                                                       04090254
C        THE FOLLOWING TWO TESTS ARE TO CHECK THE EXECUTION OF AN ELSE  04100254
C     IF STATEMENT WITH AN EMPTY ELSE IF-BLOCK.  THE SYNTAX FOR THE TWO 04110254
C     TESTS IS AS FOLLOWS -                                             04120254
C                                                                       04130254
C        IF ( E1 )  THEN                                                04140254
C             IF-BLOCK 1                                                04150254
C        ELSE IF ( E2 )  THEN                                           04160254
C        ELSE IF ( E3 )  THEN                                           04170254
C             ELSE IF-BLOCK 1                                           04180254
C        END IF                                                         04190254
C                                                                       04200254
C                                                                       04210254
C                                                                       04220254
C     ****  FCVS PROGRAM 254  -  TEST 009  ****                         04230254
C                                                                       04240254
C        TEST 009 HAS E1 FALSE, E2 TRUE, AND E3 AS TRUE.  THE STRUCTURE 04250254
C        ELSE IF ( E2 )  THEN                                           04260254
C     IS FOLLOWED BY AN EMPTY ELSE IF-BLOCK ALLOWED IN SECTION 11.7.1.  04270254
C     IN SECTION 11.7.2,  IF THE VALUE OF THE EXPRESSION IS TRUE AND THE04280254
C     ELSE IF-BLOCK IS EMPTY, CONTROL IS TRANSFERRED TO THE NEXT END IF 04290254
C     STATEMENT THAT HAS THE SAME IF-LEVEL AS THE ELSE IF STATEMENT.    04300254
C     NEITHER IF-BLOCK 1 NOR ELSE IF-BLOCK 1 SHOULD BE EXECUTED.        04310254
C                                                                       04320254
C                                                                       04330254
      IVTNUM =   9                                                      04340254
      IF (ICZERO) 30090, 0090, 30090                                    04350254
 0090 CONTINUE                                                          04360254
      IVCOMP = 1                                                        04370254
      LVON01 = .FALSE.                                                  04380254
      LVON02 = .TRUE.                                                   04390254
      LVON03 = .TRUE.                                                   04400254
      IF ( LVON01 )  THEN                                               04410254
           IVCOMP = IVCOMP * 2                                          04420254
      ELSE IF ( LVON02 )  THEN                                          04430254
      ELSE IF ( LVON03 )  THEN                                          04440254
           IVCOMP = IVCOMP * 3                                          04450254
      END IF                                                            04460254
      IVCORR = 1                                                        04470254
40090 IF ( IVCOMP - 1 )  20090, 10090, 20090                            04480254
30090 IVDELE = IVDELE + 1                                               04490254
      WRITE (I02,80000) IVTNUM                                          04500254
      IF (ICZERO) 10090, 0101, 20090                                    04510254
10090 IVPASS = IVPASS + 1                                               04520254
      WRITE (I02,80002) IVTNUM                                          04530254
      GO TO 0101                                                        04540254
20090 IVFAIL = IVFAIL + 1                                               04550254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04560254
 0101 CONTINUE                                                          04570254
C                                                                       04580254
C     ****  FCVS PROGRAM 254  -  TEST 010  ****                         04590254
C                                                                       04600254
C        TEST 010 ALSO HAS AN EMPTY ELSE IF-BLOCK.  E1 AND E2 ARE FALSE.04610254
C     E3 IS TRUE.  ONLY ELSE IF-BLOCK 1 SHOULD BE EXECUTED.  IF-BLOCK 1 04620254
C     SHOULD NOT BE EXECUTED.  IN SECTION 11.7.2,  IF THE VALUE OF THE  04630254
C     EXPRESSION IS FALSE, CONTROL IS TRANSFERRED TO THE NEXT ELSE IF,  04640254
C     ELSE, OR END IF STATEMENT THAT HAS THE SAME IF-LEVEL AS THE ELSE  04650254
C     IF STATEMENT.                                                     04660254
C                                                                       04670254
C                                                                       04680254
      IVTNUM =  10                                                      04690254
      IF (ICZERO) 30100, 0100, 30100                                    04700254
 0100 CONTINUE                                                          04710254
      IVCOMP = 1                                                        04720254
      LVON01 = .FALSE.                                                  04730254
      LVON02 = .FALSE.                                                  04740254
      LVON03 = .TRUE.                                                   04750254
      IF ( LVON01 )  THEN                                               04760254
           IVCOMP = IVCOMP * 2                                          04770254
      ELSE IF ( LVON02 )  THEN                                          04780254
      ELSE IF ( LVON03 )  THEN                                          04790254
           IVCOMP = IVCOMP * 3                                          04800254
      END IF                                                            04810254
C                                                                       04820254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****04830254
C                                                                       04840254
      IVCORR = 3                                                        04850254
40100 IF ( IVCOMP - 3 )  20100, 10100, 20100                            04860254
30100 IVDELE = IVDELE + 1                                               04870254
      WRITE (I02,80000) IVTNUM                                          04880254
      IF (ICZERO) 10100, 0111, 20100                                    04890254
10100 IVPASS = IVPASS + 1                                               04900254
      WRITE (I02,80002) IVTNUM                                          04910254
      GO TO 0111                                                        04920254
20100 IVFAIL = IVFAIL + 1                                               04930254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04940254
 0111 CONTINUE                                                          04950254
C                                                                       04960254
C                                                                       04970254
C        THE NEXT TWO TESTS USE THE ELSE IF STRUCTURE INSIDE A BLOCKED  04980254
C     IF STRUCTURE OF LEVEL 2 AS FOLLOWS -                              04990254
C                                                                       05000254
C        IF ( E1 )  THEN                                                05010254
C             IF-BLOCK 1                                                05020254
C             IF ( E2 )  THEN                                           05030254
C                  IF-BLOCK 2                                           05040254
C             ELSE IF ( E3 )  THEN                                      05050254
C                  ELSE IF-BLOCK 1                                      05060254
C             ELSE IF ( E4 )  THEN                                      05070254
C                  ELSE IF-BLOCK 2                                      05080254
C             END IF                                                    05090254
C        ELSE IF ( E5 )  THEN                                           05100254
C             ELSE IF-BLOCK 3                                           05110254
C        ELSE IF ( E6 )  THEN                                           05120254
C             ELSE IF-BLOCK 4                                           05130254
C        END IF                                                         05140254
C                                                                       05150254
C                                                                       05160254
C                                                                       05170254
C     ****  FCVS PROGRAM 254  -  TEST 011  ****                         05180254
C                                                                       05190254
C        TEST 011 HAS E1 TRUE, E2 AND E3 AS FALSE, E4, E5, AND ALSO     05200254
C     E6 AS TRUE.  IF-BLOCK 1, AND ELSE IF-BLOCK 2 SHOULD BE EXECUTED.  05210254
C     IF-BLOCK 2, ELSE IF-BLOCK 1, 3, AND 4 SHOULD NOT BE EXECUTED.     05220254
C                                                                       05230254
C                                                                       05240254
      IVTNUM =  11                                                      05250254
      IF (ICZERO) 30110, 0110, 30110                                    05260254
 0110 CONTINUE                                                          05270254
      IVCOMP = 1                                                        05280254
      LVON01 = .TRUE.                                                   05290254
      LVON02 = .FALSE.                                                  05300254
      LVON03 = .FALSE.                                                  05310254
      LVON04 = .TRUE.                                                   05320254
      LVON05 = .TRUE.                                                   05330254
      LVON06 = .TRUE.                                                   05340254
      IF ( LVON01 )  THEN                                               05350254
           IVCOMP = IVCOMP * 2                                          05360254
           IF ( LVON02 )  THEN                                          05370254
                IVCOMP = IVCOMP * 3                                     05380254
           ELSE IF ( LVON03 )  THEN                                     05390254
                IVCOMP = IVCOMP * 5                                     05400254
           ELSE IF ( LVON04 )  THEN                                     05410254
                IVCOMP = IVCOMP * 7                                     05420254
           END IF                                                       05430254
      ELSE IF ( LVON05 )  THEN                                          05440254
           IVCOMP = IVCOMP * 11                                         05450254
      ELSE IF ( LVON06 )  THEN                                          05460254
           IVCOMP = IVCOMP * 13                                         05470254
      END IF                                                            05480254
C                                                                       05490254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 14 = 1 * 2 * 7       ****05500254
C                                                                       05510254
      IVCORR = 14                                                       05520254
40110 IF ( IVCOMP - 14 )  20110, 10110, 20110                           05530254
30110 IVDELE = IVDELE + 1                                               05540254
      WRITE (I02,80000) IVTNUM                                          05550254
      IF (ICZERO) 10110, 0121, 20110                                    05560254
10110 IVPASS = IVPASS + 1                                               05570254
      WRITE (I02,80002) IVTNUM                                          05580254
      GO TO 0121                                                        05590254
20110 IVFAIL = IVFAIL + 1                                               05600254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05610254
 0121 CONTINUE                                                          05620254
C                                                                       05630254
C     ****  FCVS PROGRAM 254  -  TEST 012  ****                         05640254
C                                                                       05650254
C        TEST 012 HAS E1 AS FALSE, E2, E3, AND E4 ARE TRUE, E5 AS FALSE,05660254
C     AND E6 IS TRUE.  ONLY ELSE IF-BLOCK 4 SHOULD BE EXECUTED.  NO     05670254
C     OTHER IF-BLOCK OR ELSE IF-BLOCK SHOULD BE EXECUTED.               05680254
C                                                                       05690254
C                                                                       05700254
      IVTNUM =  12                                                      05710254
      IF (ICZERO) 30120, 0120, 30120                                    05720254
 0120 CONTINUE                                                          05730254
      IVCOMP = 1                                                        05740254
      LVON01 = .FALSE.                                                  05750254
      LVON02 = .TRUE.                                                   05760254
      LVON03 = .TRUE.                                                   05770254
      LVON04 = .TRUE.                                                   05780254
      LVON05 = .FALSE.                                                  05790254
      LVON06 = .TRUE.                                                   05800254
      IF ( LVON01 )  THEN                                               05810254
           IVCOMP = IVCOMP * 2                                          05820254
           IF ( LVON02 )  THEN                                          05830254
                IVCOMP = IVCOMP * 3                                     05840254
           ELSE IF ( LVON03 )  THEN                                     05850254
                IVCOMP = IVCOMP * 5                                     05860254
           ELSE IF ( LVON04 )  THEN                                     05870254
                IVCOMP = IVCOMP * 7                                     05880254
           END IF                                                       05890254
      ELSE IF ( LVON05 )  THEN                                          05900254
           IVCOMP = IVCOMP * 11                                         05910254
      ELSE IF ( LVON06 )  THEN                                          05920254
           IVCOMP = IVCOMP * 13                                         05930254
      END IF                                                            05940254
C                                                                       05950254
C        **** IVCOMP IS DETERMINED BY IVCOMP = 13 = 1 * 13          ****05960254
C                                                                       05970254
      IVCORR = 13                                                       05980254
40120 IF ( IVCOMP - 13 )  20120, 10120, 20120                           05990254
30120 IVDELE = IVDELE + 1                                               06000254
      WRITE (I02,80000) IVTNUM                                          06010254
      IF (ICZERO) 10120, 0131, 20120                                    06020254
10120 IVPASS = IVPASS + 1                                               06030254
      WRITE (I02,80002) IVTNUM                                          06040254
      GO TO 0131                                                        06050254
20120 IVFAIL = IVFAIL + 1                                               06060254
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06070254
 0131 CONTINUE                                                          06080254
C                                                                       06090254
C                                                                       06100254
C     WRITE OUT TEST SUMMARY                                            06110254
C                                                                       06120254
      WRITE (I02,90004)                                                 06130254
      WRITE (I02,90014)                                                 06140254
      WRITE (I02,90004)                                                 06150254
      WRITE (I02,90000)                                                 06160254
      WRITE (I02,90004)                                                 06170254
      WRITE (I02,90020) IVFAIL                                          06180254
      WRITE (I02,90022) IVPASS                                          06190254
      WRITE (I02,90024) IVDELE                                          06200254
      STOP                                                              06210254
90001 FORMAT (1H ,24X,5HFM254)                                          06220254
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM254)                          06230254
C                                                                       06240254
C     FORMATS FOR TEST DETAIL LINES                                     06250254
C                                                                       06260254
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   06270254
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      06280254
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         06290254
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    06300254
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        06310254
C                                                                       06320254
C     FORMAT STATEMENTS FOR PAGE HEADERS                                06330254
C                                                                       06340254
90002 FORMAT (1H1)                                                      06350254
90004 FORMAT (1H )                                                      06360254
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06370254
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   06380254
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         06390254
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  06400254
90014 FORMAT (1H ,5X,46H----------------------------------------------) 06410254
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             06420254
C                                                                       06430254
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 06440254
C                                                                       06450254
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              06460254
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              06470254
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             06480254
      END                                                               06490254
