      PROGRAM FM255                                                     00010255
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020255
C                                                                       00030255
C                                                                       00040255
C        THIS ROUTINE IS A TEST OF THE ELSE STATEMENT.  TESTS WITHIN    00050255
C     THIS ROUTINE ARE FOR THE SYNTAX OF THE BASIC ELSE STATEMENT AND   00060255
C     ELSE BLOCK STRUCTURES.  THE END IF STATEMENT IS USED IN ALL BLOCK 00070255
C     IF STRUCTURES FOR THE ROUTINES FM253, FM254, AND FM255.  FOR EACH 00080255
C     BLOCK IF STATEMENT, THERE MUST BE A CORRESPONDING END IF STATEMENT00090255
C     IN THE SAME PROGRAM UNIT.                                         00100255
C                                                                       00110255
C     REFERENCES                                                        00120255
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00130255
C             X3.9-1977                                                 00140255
C        SECTION 11.8,       ELSE STATEMENT                             00150255
C        SECTION 11.8.1,     ELSE BLOCK                                 00160255
C        SECTION 11.8.2,     EXECUTION OF AN ELSE STATEMENT             00170255
C        SECTION 11.9,       END IF STATEMENT                           00180255
C                                                                       00190255
C                                                                       00200255
C                                                                       00210255
C                                                                       00220255
C     ******************************************************************00230255
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00240255
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00250255
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00260255
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00270255
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00280255
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00290255
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00300255
C     THE RESULT OF EXECUTING THESE TESTS.                              00310255
C                                                                       00320255
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00330255
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00340255
C                                                                       00350255
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00360255
C                    DEPARTMENT OF THE NAVY                             00370255
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00380255
C                    WASHINGTON, D.C.   20376                           00390255
C                                                                       00400255
C     ******************************************************************00410255
C                                                                       00420255
C                                                                       00430255
      IMPLICIT LOGICAL (L)                                              00440255
      IMPLICIT CHARACTER*14 (C)                                         00450255
C                                                                       00460255
C                                                                       00470255
C                                                                       00480255
C     INITIALIZATION SECTION.                                           00490255
C                                                                       00500255
C     INITIALIZE CONSTANTS                                              00510255
C     ********************                                              00520255
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00530255
      I01 = 5                                                           00540255
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00550255
      I02 = 6                                                           00560255
C     SYSTEM ENVIRONMENT SECTION                                        00570255
C                                                                       00580255
      I01 = 5                                                           00590255
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00600255
C     (UNIT NUMBER FOR CARD READER).                                    00610255
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00620255
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00630255
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00640255
C                                                                       00650255
      I02 = 6                                                           00660255
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00670255
C     (UNIT NUMBER FOR PRINTER).                                        00680255
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00690255
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00700255
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00710255
C                                                                       00720255
      IVPASS = 0                                                        00730255
      IVFAIL = 0                                                        00740255
      IVDELE = 0                                                        00750255
      ICZERO = 0                                                        00760255
C                                                                       00770255
C     WRITE OUT PAGE HEADERS                                            00780255
C                                                                       00790255
      WRITE (I02,90002)                                                 00800255
      WRITE (I02,90006)                                                 00810255
      WRITE (I02,90008)                                                 00820255
      WRITE (I02,90004)                                                 00830255
      WRITE (I02,90010)                                                 00840255
      WRITE (I02,90004)                                                 00850255
      WRITE (I02,90016)                                                 00860255
      WRITE (I02,90001)                                                 00870255
      WRITE (I02,90004)                                                 00880255
      WRITE (I02,90012)                                                 00890255
      WRITE (I02,90014)                                                 00900255
      WRITE (I02,90004)                                                 00910255
C                                                                       00920255
C                                                                       00930255
C                                                                       00940255
C        THE SYNTAX OF THE ELSE STATEMENTS IN THE TESTS TO FOLLOW IS    00950255
C                                                                       00960255
C        IF ( E1 )  THEN                                                00970255
C             IF-BLOCK                                                  00980255
C        ELSE                                                           00990255
C             ELSE-BLOCK                                                01000255
C        END IF                                                         01010255
C                                                                       01020255
C        THE NEXT TWO TESTS WILL USE THE FOLLOWING COMBINATIONS OF TRUE 01030255
C     AND FALSE FOR E1 AS SHOWN BELOW -                                 01040255
C        TEST NUMBER    1    2                                          01050255
C             E1        T    F                                          01060255
C                                                                       01070255
C                                                                       01080255
C                                                                       01090255
C     ****  FCVS PROGRAM 255  -  TEST 001  ****                         01100255
C                                                                       01110255
C        TEST 001 USES A VALUE OF .TRUE. FOR E1.  THE IF-BLOCK SHOULD BE01120255
C     EXECUTED.  THE ELSE-BLOCK SHOULD NOT BE EXECUTED.                 01130255
C                                                                       01140255
C                                                                       01150255
      IVTNUM =   1                                                      01160255
      IF (ICZERO) 30010, 0010, 30010                                    01170255
 0010 CONTINUE                                                          01180255
      IVCOMP = 1                                                        01190255
      IF ( .TRUE. )  THEN                                               01200255
           IVCOMP = IVCOMP * 2                                          01210255
      ELSE                                                              01220255
           IVCOMP = IVCOMP * 3                                          01230255
      END IF                                                            01240255
C                                                                       01250255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****01260255
C                                                                       01270255
      IVCORR = 2                                                        01280255
40010 IF ( IVCOMP - 2 )  20010, 10010, 20010                            01290255
30010 IVDELE = IVDELE + 1                                               01300255
      WRITE (I02,80000) IVTNUM                                          01310255
      IF (ICZERO) 10010, 0021, 20010                                    01320255
10010 IVPASS = IVPASS + 1                                               01330255
      WRITE (I02,80002) IVTNUM                                          01340255
      GO TO 0021                                                        01350255
20010 IVFAIL = IVFAIL + 1                                               01360255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01370255
 0021 CONTINUE                                                          01380255
C                                                                       01390255
C     ****  FCVS PROGRAM 255  -  TEST 002  ****                         01400255
C                                                                       01410255
C        TEST 002 HAS E1 AS FALSE.  THE IF-BLOCK SHOULD NOT BE EXECUTED.01420255
C     THE ELSE-BLOCK SHOULD EXECUTE.                                    01430255
C                                                                       01440255
C                                                                       01450255
      IVTNUM =   2                                                      01460255
      IF (ICZERO) 30020, 0020, 30020                                    01470255
 0020 CONTINUE                                                          01480255
      IVCOMP = 1                                                        01490255
      LVON01 = .FALSE.                                                  01500255
      IF ( LVON01 )  THEN                                               01510255
           IVCOMP = IVCOMP * 2                                          01520255
      ELSE                                                              01530255
           IVCOMP = IVCOMP * 3                                          01540255
      END IF                                                            01550255
C                                                                       01560255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****01570255
C                                                                       01580255
      IVCORR = 3                                                        01590255
40020 IF ( IVCOMP - 3 )  20020, 10020, 20020                            01600255
30020 IVDELE = IVDELE + 1                                               01610255
      WRITE (I02,80000) IVTNUM                                          01620255
      IF (ICZERO) 10020, 0031, 20020                                    01630255
10020 IVPASS = IVPASS + 1                                               01640255
      WRITE (I02,80002) IVTNUM                                          01650255
      GO TO 0031                                                        01660255
20020 IVFAIL = IVFAIL + 1                                               01670255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01680255
 0031 CONTINUE                                                          01690255
C                                                                       01700255
C     ****  FCVS PROGRAM 255  -  TEST 003  ****                         01710255
C                                                                       01720255
C        TEST 003 HAS AN EMPTY ELSE-BLOCK.  SECTION 11.8.1 STATES THAT  01730255
C     AN ELSE-BLOCK MAY BE EMPTY.  IN THIS TEST THE VALUE OF E1 IS TRUE.01740255
C     THE IF-BLOCK SHOULD BE EXECUTED.                                  01750255
C                                                                       01760255
C                                                                       01770255
      IVTNUM =   3                                                      01780255
      IF (ICZERO) 30030, 0030, 30030                                    01790255
 0030 CONTINUE                                                          01800255
      IVCOMP = 1                                                        01810255
      LVON01 = .TRUE.                                                   01820255
      IF ( LVON01 )  THEN                                               01830255
           IVCOMP = IVCOMP * 2                                          01840255
      ELSE                                                              01850255
      END IF                                                            01860255
C                                                                       01870255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****01880255
C                                                                       01890255
      IVCORR = 2                                                        01900255
40030 IF ( IVCOMP - 2 )  20030, 10030, 20030                            01910255
30030 IVDELE = IVDELE + 1                                               01920255
      WRITE (I02,80000) IVTNUM                                          01930255
      IF (ICZERO) 10030, 0041, 20030                                    01940255
10030 IVPASS = IVPASS + 1                                               01950255
      WRITE (I02,80002) IVTNUM                                          01960255
      GO TO 0041                                                        01970255
20030 IVFAIL = IVFAIL + 1                                               01980255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01990255
 0041 CONTINUE                                                          02000255
C                                                                       02010255
C     ****  FCVS PROGRAM 255  -  TEST 004  ****                         02020255
C                                                                       02030255
C        TEST 004 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT THE VALUE 02040255
C     OF E1 IS FALSE.  THE IF-BLOCK SHOULD NOT BE EXECUTED.             02050255
C                                                                       02060255
C                                                                       02070255
      IVTNUM =   4                                                      02080255
      IF (ICZERO) 30040, 0040, 30040                                    02090255
 0040 CONTINUE                                                          02100255
      IVCOMP = 1                                                        02110255
      LVON01 = .FALSE.                                                  02120255
      IF ( LVON01 )  THEN                                               02130255
           IVCOMP = IVCOMP * 2                                          02140255
      ELSE                                                              02150255
      END IF                                                            02160255
      IVCORR = 1                                                        02170255
40040 IF ( IVCOMP - 1 )  20040, 10040, 20040                            02180255
30040 IVDELE = IVDELE + 1                                               02190255
      WRITE (I02,80000) IVTNUM                                          02200255
      IF (ICZERO) 10040, 0051, 20040                                    02210255
10040 IVPASS = IVPASS + 1                                               02220255
      WRITE (I02,80002) IVTNUM                                          02230255
      GO TO 0051                                                        02240255
20040 IVFAIL = IVFAIL + 1                                               02250255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02260255
 0051 CONTINUE                                                          02270255
C                                                                       02280255
C     ****  FCVS PROGRAM 255  -  TEST 005  ****                         02290255
C                                                                       02300255
C        TEST 005 USES AN ELSE STATEMENT IN AN IF-LEVEL OF 2.  THE      02310255
C     SYNTAX FOR THIS STRUCTURE IS SHOWN BELOW -                        02320255
C                                                                       02330255
C        IF ( E1 )  THEN                                                02340255
C             IF-BLOCK 1                                                02350255
C             IF ( E2 )  THEN                                           02360255
C                  IF-BLOCK 2                                           02370255
C             ELSE                                                      02380255
C                  ELSE-BLOCK 1                                         02390255
C             END IF                                                    02400255
C        ELSE                                                           02410255
C             ELSE-BLOCK 2                                              02420255
C        END IF                                                         02430255
C                                                                       02440255
C        IN THIS TEST THE VALUES FOR E1 AND E2 ARE BOTH TRUE.  IF-BLOCK 02450255
C     1 AND IF-BLOCK 2 SHOULD BE EXECUTED.  ELSE-BLOCKS 1 AND 2 SHOULD  02460255
C     NOT BE EXECUTED.                                                  02470255
C                                                                       02480255
C                                                                       02490255
      IVTNUM =   5                                                      02500255
      IF (ICZERO) 30050, 0050, 30050                                    02510255
 0050 CONTINUE                                                          02520255
      IVCOMP = 1                                                        02530255
      LVON01 = .TRUE.                                                   02540255
      LVON02 = .TRUE.                                                   02550255
      IF ( LVON01 )  THEN                                               02560255
           IVCOMP = IVCOMP * 2                                          02570255
           IF ( LVON02 )  THEN                                          02580255
                IVCOMP = IVCOMP * 3                                     02590255
           ELSE                                                         02600255
                IVCOMP = IVCOMP * 5                                     02610255
           END IF                                                       02620255
      ELSE                                                              02630255
      IVCOMP = IVCOMP * 7                                               02640255
      END IF                                                            02650255
C                                                                       02660255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 6 = 1 * 2 * 3            02670255
C                                                                       02680255
      IVCORR = 6                                                        02690255
40050 IF ( IVCOMP - 6 )  20050, 10050, 20050                            02700255
30050 IVDELE = IVDELE + 1                                               02710255
      WRITE (I02,80000) IVTNUM                                          02720255
      IF (ICZERO) 10050, 0061, 20050                                    02730255
10050 IVPASS = IVPASS + 1                                               02740255
      WRITE (I02,80002) IVTNUM                                          02750255
      GO TO 0061                                                        02760255
20050 IVFAIL = IVFAIL + 1                                               02770255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02780255
 0061 CONTINUE                                                          02790255
C                                                                       02800255
C     ****  FCVS PROGRAM 255  -  TEST 006  ****                         02810255
C                                                                       02820255
C        TEST 006 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT E1 IS TRUE02830255
C     AND E2 IS FALSE.  IF-BLOCK 1 AND ELSE-BLOCK 1 SHOULD BE EXECUTED. 02840255
C                                                                       02850255
C                                                                       02860255
      IVTNUM =   6                                                      02870255
      IF (ICZERO) 30060, 0060, 30060                                    02880255
 0060 CONTINUE                                                          02890255
      IVCOMP = 1                                                        02900255
      LVON01 = .TRUE.                                                   02910255
      LVON02 = .FALSE.                                                  02920255
      IF ( LVON01 )  THEN                                               02930255
           IVCOMP = IVCOMP * 2                                          02940255
           IF ( LVON02 )  THEN                                          02950255
                IVCOMP = IVCOMP * 3                                     02960255
           ELSE                                                         02970255
                IVCOMP = IVCOMP * 5                                     02980255
           END IF                                                       02990255
      ELSE                                                              03000255
           IVCOMP = IVCOMP * 7                                          03010255
      END IF                                                            03020255
C                                                                       03030255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 10 = 1 * 2 * 5       ****03040255
C                                                                       03050255
      IVCORR = 10                                                       03060255
40060 IF ( IVCOMP - 10 )  20060, 10060, 20060                           03070255
30060 IVDELE = IVDELE + 1                                               03080255
      WRITE (I02,80000) IVTNUM                                          03090255
      IF (ICZERO) 10060, 0071, 20060                                    03100255
10060 IVPASS = IVPASS + 1                                               03110255
      WRITE (I02,80002) IVTNUM                                          03120255
      GO TO 0071                                                        03130255
20060 IVFAIL = IVFAIL + 1                                               03140255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03150255
 0071 CONTINUE                                                          03160255
C                                                                       03170255
C     ****  FCVS PROGRAM 255  -  TEST 007  ****                         03180255
C                                                                       03190255
C        TEST 007 AGAIN USES THE SAME STRUCTURE AS THE PREVIOUS TWO     03200255
C     TESTS.  IN THIS TEST E1 IS FALSE AND E2 IS TRUE.  ONLY ELSE-BLOCK 03210255
C     2 SHOULD BE EXECUTED.                                             03220255
C                                                                       03230255
C                                                                       03240255
      IVTNUM =   7                                                      03250255
      IF (ICZERO) 30070, 0070, 30070                                    03260255
 0070 CONTINUE                                                          03270255
      IVCOMP = 1                                                        03280255
      LVON01 = .FALSE.                                                  03290255
      LVON02 = .TRUE.                                                   03300255
      IF ( LVON01 )  THEN                                               03310255
           IVCOMP = IVCOMP * 2                                          03320255
           IF ( LVON02 )  THEN                                          03330255
                IVCOMP = IVCOMP * 3                                     03340255
           ELSE                                                         03350255
                IVCOMP = IVCOMP * 5                                     03360255
           END IF                                                       03370255
      ELSE                                                              03380255
           IVCOMP = IVCOMP * 7                                          03390255
      END IF                                                            03400255
C                                                                       03410255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 7 = 1 * 7            ****03420255
C                                                                       03430255
      IVCORR = 7                                                        03440255
40070 IF ( IVCOMP - 7 )  20070, 10070, 20070                            03450255
30070 IVDELE = IVDELE + 1                                               03460255
      WRITE (I02,80000) IVTNUM                                          03470255
      IF (ICZERO) 10070, 0081, 20070                                    03480255
10070 IVPASS = IVPASS + 1                                               03490255
      WRITE (I02,80002) IVTNUM                                          03500255
      GO TO 0081                                                        03510255
20070 IVFAIL = IVFAIL + 1                                               03520255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03530255
 0081 CONTINUE                                                          03540255
C                                                                       03550255
C                                                                       03560255
C        THE FOLLOWING TESTS USE A BLOCK IF STRUCTURE OF IF-LEVEL 3.    03570255
C     THE STRUCTURE AS SHOWN BELOW CONTAINS THE IF-THEN, ELSE IF-THEN,  03580255
C     AND ELSE STRUCTURES.                                              03590255
C                                                                       03600255
C        IF ( E1 )  THEN                                                03610255
C             IF-BLOCK 1                                                03620255
C             IF ( E2 )  THEN                                           03630255
C                  IF-BLOCK 2                                           03640255
C                  IF ( E3 )  THEN                                      03650255
C                       IF-BLOCK 3                                      03660255
C                  ELSE IF ( E4 )  THEN                                 03670255
C                       ELSE IF-BLOCK 1                                 03680255
C                  ELSE IF ( E5 )  THEN                                 03690255
C                       ELSE IF-BLOCK 2                                 03700255
C                  ELSE                                                 03710255
C                       ELSE-BLOCK 1                                    03720255
C                  END IF                                               03730255
C                  MORE IF-BLOCK 2                                      03740255
C             ELSE IF ( E6 )  THEN                                      03750255
C                  ELSE IF-BLOCK 3                                      03760255
C             ELSE                                                      03770255
C                  ELSE-BLOCK 2                                         03780255
C             END IF                                                    03790255
C             MORE IF-BLOCK 1                                           03800255
C        ELSE IF ( E7 )  THEN                                           03810255
C             ELSE IF-BLOCK 4                                           03820255
C        ELSE                                                           03830255
C             ELSE-BLOCK 3                                              03840255
C        END IF                                                         03850255
C                                                                       03860255
C        THE TRUE AND FALSE VALUES FOR THE VARIOUS LOGICAL EXPRESSIONS  03870255
C     USED IN THE TESTS TO FOLLOW ARE SHOWN BELOW -                     03880255
C                                                                       03890255
C        TEST NUMBER    8    9   10   11   12   13   14   15            03900255
C             E1        T    T    T    T    T    T    F    F            03910255
C             E2        T    T    T    T    F    F    F    F            03920255
C             E3        T    F    F    F    F    F    F    F            03930255
C             E4        T    F    F    T    F    F    F    F            03940255
C             E5        T    F    T    F    F    F    F    F            03950255
C             E6        T    F    F    F    T    F    F    F            03960255
C             E7        T    F    F    F    F    F    T    F            03970255
C                                                                       03980255
C                                                                       03990255
C                                                                       04000255
C     ****  FCVS PROGRAM 255  -  TEST 008  ****                         04010255
C                                                                       04020255
C        TEST 008 SHOULD EXECUTE IF-BLOCKS 1, 2, AND 3.  IT SHOULD ALSO 04030255
C     EXECUTE MORE IF-BLOCKS 2 AND 1.                                   04040255
C                                                                       04050255
C                                                                       04060255
      IVTNUM =   8                                                      04070255
      IF (ICZERO) 30080, 0080, 30080                                    04080255
 0080 CONTINUE                                                          04090255
      IVCOMP = 1                                                        04100255
      LVON01 = .TRUE.                                                   04110255
      LVON02 = .TRUE.                                                   04120255
      LVON03 = .TRUE.                                                   04130255
      LVON04 = .TRUE.                                                   04140255
      LVON05 = .TRUE.                                                   04150255
      LVON06 = .TRUE.                                                   04160255
      LVON07 = .TRUE.                                                   04170255
      IF ( LVON01 )  THEN                                               04180255
           IVCOMP = IVCOMP * 2                                          04190255
           IF ( LVON02 )  THEN                                          04200255
                IVCOMP = IVCOMP * 3                                     04210255
                IF ( LVON03 )  THEN                                     04220255
                     IVCOMP = IVCOMP * 5                                04230255
                ELSE IF ( LVON04 )  THEN                                04240255
                     IVCOMP = IVCOMP * 7                                04250255
                ELSE IF ( LVON05 )  THEN                                04260255
                     IVCOMP = IVCOMP * 11                               04270255
                ELSE                                                    04280255
                     IVCOMP = IVCOMP * 13                               04290255
                END IF                                                  04300255
                IVCOMP = IVCOMP * 17                                    04310255
           ELSE IF ( LVON06 )  THEN                                     04320255
                IVCOMP = IVCOMP * 19                                    04330255
           ELSE                                                         04340255
                IVCOMP = IVCOMP * 23                                    04350255
           END IF                                                       04360255
           IVCOMP = IVCOMP * 29                                         04370255
      ELSE IF ( LVON07 )  THEN                                          04380255
           IVCOMP = IVCOMP * 31                                         04390255
      ELSE                                                              04400255
           GO TO 0082                                                   04410255
      END IF                                                            04420255
      GO TO 0083                                                        04430255
 0082 IVCOMP = IVCOMP * 37                                              04440255
 0083 CONTINUE                                                          04450255
C                                                                       04460255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 14790 = 1 * 2 * 3 * 5 *  04470255
C                                                      17 * 29      ****04480255
C                                                                       04490255
      IVCORR = 14790                                                    04500255
40080 IF ( IVCOMP - 14790 )  20080, 10080, 20080                        04510255
30080 IVDELE = IVDELE + 1                                               04520255
      WRITE (I02,80000) IVTNUM                                          04530255
      IF (ICZERO) 10080, 0091, 20080                                    04540255
10080 IVPASS = IVPASS + 1                                               04550255
      WRITE (I02,80002) IVTNUM                                          04560255
      GO TO 0091                                                        04570255
20080 IVFAIL = IVFAIL + 1                                               04580255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04590255
 0091 CONTINUE                                                          04600255
C                                                                       04610255
C     ****  FCVS PROGRAM 255  -  TEST 009  ****                         04620255
C                                                                       04630255
C        TEST 009 SHOULD EXECUTE IF-BLOCKS 1 AND 2.  IT SHOULD ALSO     04640255
C     EXECUTE ELSE-BLOCK 1, PLUS MORE IF-BLOCKS 2 AND 1.                04650255
C                                                                       04660255
C                                                                       04670255
      IVTNUM =   9                                                      04680255
      IF (ICZERO) 30090, 0090, 30090                                    04690255
 0090 CONTINUE                                                          04700255
      IVCOMP = 1                                                        04710255
      LVON01 = .TRUE.                                                   04720255
      LVON02 = .TRUE.                                                   04730255
      LVON03 = .FALSE.                                                  04740255
      LVON04 = .FALSE.                                                  04750255
      LVON05 = .FALSE.                                                  04760255
      LVON06 = .FALSE.                                                  04770255
      LVON07 = .FALSE.                                                  04780255
      IF ( LVON01 )  THEN                                               04790255
           IVCOMP = IVCOMP * 2                                          04800255
           IF ( LVON02 )  THEN                                          04810255
                IVCOMP = IVCOMP * 3                                     04820255
                IF ( LVON03 )  THEN                                     04830255
                     IVCOMP = IVCOMP * 13                               04840255
                ELSE IF ( LVON04 )  THEN                                04850255
                     IVCOMP = IVCOMP * 17                               04860255
                ELSE IF ( LVON05 )  THEN                                04870255
                     IVCOMP = IVCOMP * 11                               04880255
                ELSE                                                    04890255
                     IVCOMP = IVCOMP * 5                                04900255
                END IF                                                  04910255
                IVCOMP = IVCOMP * 7                                     04920255
           ELSE IF ( LVON06 )  THEN                                     04930255
                IVCOMP = IVCOMP * 19                                    04940255
           ELSE                                                         04950255
                IVCOMP = IVCOMP * 23                                    04960255
           END IF                                                       04970255
           IVCOMP = IVCOMP * 29                                         04980255
      ELSE IF ( LVON07 )  THEN                                          04990255
           IVCOMP = IVCOMP * 31                                         05000255
      ELSE                                                              05010255
           IF ( .TRUE. )  GO TO 0092                                    05020255
      END IF                                                            05030255
      GO TO 0093                                                        05040255
 0092 IVCOMP = IVCOMP * 37                                              05050255
 0093 CONTINUE                                                          05060255
C                                                                       05070255
C        **** THE ORDER OF THE PRIME INTEGER MULTIPLIERS HAS BEEN       05080255
C     CHANGED TO KEEP THE IVCOMP RESULT SMALLER THAN 32767          ****05090255
C                                                                       05100255
C                                                                       05110255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 6090 = 1 * 2 * 3 * 5 * 7 05120255
C                                                     * 29          ****05130255
C                                                                       05140255
      IVCORR = 6090                                                     05150255
40090 IF ( IVCOMP - 6090 )  20090, 10090, 20090                         05160255
30090 IVDELE = IVDELE + 1                                               05170255
      WRITE (I02,80000) IVTNUM                                          05180255
      IF (ICZERO) 10090, 0101, 20090                                    05190255
10090 IVPASS = IVPASS + 1                                               05200255
      WRITE (I02,80002) IVTNUM                                          05210255
      GO TO 0101                                                        05220255
20090 IVFAIL = IVFAIL + 1                                               05230255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05240255
 0101 CONTINUE                                                          05250255
C                                                                       05260255
C     ****  FCVS PROGRAM 255  -  TEST 010  ****                         05270255
C                                                                       05280255
C        TEST 010 SHOULD EXECUTE IF-BLOCKS 1 AND 2.  IT SHOULD ALSO     05290255
C     EXECUTE ELSE IF-BLOCK 2, PLUS MORE IF-BLOCKS 2 AND 1.             05300255
C                                                                       05310255
C                                                                       05320255
      IVTNUM =  10                                                      05330255
      IF (ICZERO) 30100, 0100, 30100                                    05340255
 0100 CONTINUE                                                          05350255
      IVCOMP = 1                                                        05360255
      LVON01 = .TRUE.                                                   05370255
      LVON02 = .TRUE.                                                   05380255
      LVON03 = .FALSE.                                                  05390255
      LVON04 = .FALSE.                                                  05400255
      LVON05 = .TRUE.                                                   05410255
      LVON06 = .FALSE.                                                  05420255
      LVON07 = .FALSE.                                                  05430255
      IF ( LVON01 )  THEN                                               05440255
           IVCOMP = IVCOMP * 2                                          05450255
           IF ( LVON02 )  THEN                                          05460255
                IVCOMP = IVCOMP * 3                                     05470255
                IF ( LVON03 )  THEN                                     05480255
                     IVCOMP = IVCOMP * 5                                05490255
                ELSE IF ( LVON04 )  THEN                                05500255
                     IVCOMP = IVCOMP * 7                                05510255
                ELSE IF ( LVON05 )  THEN                                05520255
                     IVCOMP = IVCOMP * 11                               05530255
                ELSE                                                    05540255
                     IVCOMP = IVCOMP * 13                               05550255
                END IF                                                  05560255
                IVCOMP = IVCOMP * 17                                    05570255
           ELSE IF ( LVON06 )  THEN                                     05580255
                IVCOMP = IVCOMP * 19                                    05590255
           ELSE                                                         05600255
                IVCOMP = IVCOMP * 23                                    05610255
           END IF                                                       05620255
           IVCOMP = IVCOMP * 29                                         05630255
      ELSE IF ( LVON07 )  THEN                                          05640255
           IVCOMP = IVCOMP * 31                                         05650255
      ELSE                                                              05660255
           ICON01 = 1                                                   05670255
           IF ( ICON01 )  0103, 0102, 0103                              05680255
      END IF                                                            05690255
      GO TO 0103                                                        05700255
 0102 IVCOMP = IVCOMP * 37                                              05710255
 0103 CONTINUE                                                          05720255
C                                                                       05730255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 32538 = 1 * 2 * 3 * 11 * 05740255
C                                                      17 * 29      ****05750255
C                                                                       05760255
      IVCORR = 32538                                                    05770255
40100 IF ( IVCOMP - 32538 )  20100, 10100, 20100                        05780255
30100 IVDELE = IVDELE + 1                                               05790255
      WRITE (I02,80000) IVTNUM                                          05800255
      IF (ICZERO) 10100, 0111, 20100                                    05810255
10100 IVPASS = IVPASS + 1                                               05820255
      WRITE (I02,80002) IVTNUM                                          05830255
      GO TO 0111                                                        05840255
20100 IVFAIL = IVFAIL + 1                                               05850255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05860255
 0111 CONTINUE                                                          05870255
C                                                                       05880255
C     ****  FCVS PROGRAM 255  -  TEST 011  ****                         05890255
C                                                                       05900255
C        TEST 011 SHOULD EXECUTE IF-BLOCKS 1 AND 2.  IT SHOULD ALSO     05910255
C     EXECUTE ELSE IF-BLOCK 1, PLUS MORE IF-BLOCKS 2 AND 1.             05920255
C                                                                       05930255
C                                                                       05940255
      IVTNUM =  11                                                      05950255
      IF (ICZERO) 30110, 0110, 30110                                    05960255
 0110 CONTINUE                                                          05970255
      IVCOMP = 1                                                        05980255
      LVON01 = .TRUE.                                                   05990255
      LVON02 = .TRUE.                                                   06000255
      LVON03 = .FALSE.                                                  06010255
      LVON04 = .TRUE.                                                   06020255
      LVON05 = .FALSE.                                                  06030255
      LVON06 = .FALSE.                                                  06040255
      LVON07 = .FALSE.                                                  06050255
      IF ( LVON01 )  THEN                                               06060255
           IVCOMP = IVCOMP * 2                                          06070255
           IF ( LVON02 )  THEN                                          06080255
                IVCOMP = IVCOMP * 3                                     06090255
                IF ( LVON03 )  THEN                                     06100255
                     IVCOMP = IVCOMP * 5                                06110255
                ELSE IF ( LVON04 )  THEN                                06120255
                     IVCOMP = IVCOMP * 7                                06130255
                ELSE IF ( LVON05 )  THEN                                06140255
                     IVCOMP = IVCOMP * 11                               06150255
                ELSE                                                    06160255
                     IVCOMP = IVCOMP * 13                               06170255
                END IF                                                  06180255
                IVCOMP = IVCOMP * 17                                    06190255
           ELSE IF ( LVON06 )  THEN                                     06200255
                IVCOMP = IVCOMP * 19                                    06210255
           ELSE                                                         06220255
                IVCOMP = IVCOMP * 23                                    06230255
           END IF                                                       06240255
           IVCOMP = IVCOMP * 29                                         06250255
      ELSE IF ( LVON07 )  THEN                                          06260255
           IVCOMP = IVCOMP * 31                                         06270255
      ELSE                                                              06280255
           ASSIGN 0112 TO I                                             06290255
           GO TO I, ( 0113, 0112)                                       06300255
      END IF                                                            06310255
      GO TO 0113                                                        06320255
 0112 IVCOMP = IVCOMP * 37                                              06330255
 0113 CONTINUE                                                          06340255
C                                                                       06350255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 20706 = 1 * 2 * 3 * 7 *  06360255
C                                                      17 * 29      ****06370255
C                                                                       06380255
      IVCORR = 20706                                                    06390255
40110 IF ( IVCOMP - 20706 )  20110, 10110, 20110                        06400255
30110 IVDELE = IVDELE + 1                                               06410255
      WRITE (I02,80000) IVTNUM                                          06420255
      IF (ICZERO) 10110, 0121, 20110                                    06430255
10110 IVPASS = IVPASS + 1                                               06440255
      WRITE (I02,80002) IVTNUM                                          06450255
      GO TO 0121                                                        06460255
20110 IVFAIL = IVFAIL + 1                                               06470255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06480255
 0121 CONTINUE                                                          06490255
C                                                                       06500255
C     ****  FCVS PROGRAM 255  -  TEST 012  ****                         06510255
C                                                                       06520255
C        TEST 012 SHOULD EXECUTE IF-BLOCK 1, ELSE IF-BLOCK 3, AND MORE  06530255
C     IF-BLOCK 1.                                                       06540255
C                                                                       06550255
C                                                                       06560255
      IVTNUM =  12                                                      06570255
      IF (ICZERO) 30120, 0120, 30120                                    06580255
 0120 CONTINUE                                                          06590255
      IVCOMP = 1                                                        06600255
      LVON01 = .TRUE.                                                   06610255
      LVON02 = .FALSE.                                                  06620255
      LVON03 = .FALSE.                                                  06630255
      LVON04 = .FALSE.                                                  06640255
      LVON05 = .FALSE.                                                  06650255
      LVON06 = .TRUE.                                                   06660255
      LVON07 = .FALSE.                                                  06670255
      IF ( LVON01 )  THEN                                               06680255
           IVCOMP = IVCOMP * 2                                          06690255
           IF ( LVON02 )  THEN                                          06700255
                IVCOMP = IVCOMP * 3                                     06710255
                IF ( LVON03 )  THEN                                     06720255
                     IVCOMP = IVCOMP * 5                                06730255
                ELSE IF ( LVON04 )  THEN                                06740255
                     IVCOMP = IVCOMP * 7                                06750255
                ELSE IF ( LVON05 )  THEN                                06760255
                     IVCOMP = IVCOMP * 11                               06770255
                ELSE                                                    06780255
                     IVCOMP = IVCOMP * 13                               06790255
                END IF                                                  06800255
                IVCOMP = IVCOMP * 17                                    06810255
           ELSE IF ( LVON06 )  THEN                                     06820255
                IVCOMP = IVCOMP * 19                                    06830255
           ELSE                                                         06840255
                IVCOMP = IVCOMP * 23                                    06850255
           END IF                                                       06860255
           IVCOMP = IVCOMP * 29                                         06870255
      ELSE IF ( LVON07 )  THEN                                          06880255
           IVCOMP = IVCOMP * 31                                         06890255
      ELSE                                                              06900255
           I = 2                                                        06910255
           GO TO ( 0123, 0122 ), I                                      06920255
      END IF                                                            06930255
      GO TO 0123                                                        06940255
 0122 IVCOMP = IVCOMP * 37                                              06950255
 0123 CONTINUE                                                          06960255
C                                                                       06970255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 1102 = 1 * 2 * 19 * 29   06980255
C                                                                       06990255
      IVCORR = 1102                                                     07000255
40120 IF ( IVCOMP - 1102 )  20120, 10120, 20120                         07010255
30120 IVDELE = IVDELE + 1                                               07020255
      WRITE (I02,80000) IVTNUM                                          07030255
      IF (ICZERO) 10120, 0131, 20120                                    07040255
10120 IVPASS = IVPASS + 1                                               07050255
      WRITE (I02,80002) IVTNUM                                          07060255
      GO TO 0131                                                        07070255
20120 IVFAIL = IVFAIL + 1                                               07080255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07090255
 0131 CONTINUE                                                          07100255
C                                                                       07110255
C     ****  FCVS PROGRAM 255  -  TEST 013  ****                         07120255
C                                                                       07130255
C        TEST 013 SHOULD EXECUTE IF-BLOCK 1, ELSE-BLOCK 2, AND MORE     07140255
C     IF-BLOCK 1.                                                       07150255
C                                                                       07160255
C                                                                       07170255
      IVTNUM =  13                                                      07180255
      IF (ICZERO) 30130, 0130, 30130                                    07190255
 0130 CONTINUE                                                          07200255
      IVCOMP = 1                                                        07210255
      LVON01 = .TRUE.                                                   07220255
      LVON02 = .FALSE.                                                  07230255
      LVON03 = .FALSE.                                                  07240255
      LVON04 = .FALSE.                                                  07250255
      LVON05 = .FALSE.                                                  07260255
      LVON06 = .FALSE.                                                  07270255
      LVON07 = .FALSE.                                                  07280255
      IF ( LVON01 )  THEN                                               07290255
           IVCOMP = IVCOMP * 2                                          07300255
           IF ( LVON02 )  THEN                                          07310255
                IVCOMP = IVCOMP * 3                                     07320255
                IF ( LVON03 )  THEN                                     07330255
                     IVCOMP = IVCOMP * 5                                07340255
                ELSE IF ( LVON04 )  THEN                                07350255
                     IVCOMP = IVCOMP * 7                                07360255
                ELSE IF ( LVON05 )  THEN                                07370255
                     IVCOMP = IVCOMP * 11                               07380255
                ELSE                                                    07390255
                     IVCOMP = IVCOMP * 13                               07400255
                END IF                                                  07410255
                IVCOMP = IVCOMP * 17                                    07420255
           ELSE IF ( LVON06 )  THEN                                     07430255
                IVCOMP = IVCOMP * 19                                    07440255
           ELSE                                                         07450255
                IVCOMP = IVCOMP * 23                                    07460255
           END IF                                                       07470255
           IVCOMP = IVCOMP * 29                                         07480255
      ELSE IF ( LVON07 )  THEN                                          07490255
           IVCOMP = IVCOMP * 31                                         07500255
      ELSE                                                              07510255
           GO TO 0132                                                   07520255
      END IF                                                            07530255
      GO TO 0133                                                        07540255
 0132 IVCOMP = IVCOMP * 37                                              07550255
 0133 CONTINUE                                                          07560255
C                                                                       07570255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 1334 = 1 * 2 * 23 * 29   07580255
C                                                                       07590255
      IVCORR = 1334                                                     07600255
40130 IF ( IVCOMP - 1334 )  20130, 10130, 20130                         07610255
30130 IVDELE = IVDELE + 1                                               07620255
      WRITE (I02,80000) IVTNUM                                          07630255
      IF (ICZERO) 10130, 0141, 20130                                    07640255
10130 IVPASS = IVPASS + 1                                               07650255
      WRITE (I02,80002) IVTNUM                                          07660255
      GO TO 0141                                                        07670255
20130 IVFAIL = IVFAIL + 1                                               07680255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07690255
 0141 CONTINUE                                                          07700255
C                                                                       07710255
C     ****  FCVS PROGRAM 255  -  TEST 014  ****                         07720255
C                                                                       07730255
C        TEST 014 SHOULD ONLY EXECUTE ELSE IF-BLOCK 4.                  07740255
C                                                                       07750255
C                                                                       07760255
      IVTNUM =  14                                                      07770255
      IF (ICZERO) 30140, 0140, 30140                                    07780255
 0140 CONTINUE                                                          07790255
      IVCOMP = 1                                                        07800255
      LVON01 = .FALSE.                                                  07810255
      LVON02 = .FALSE.                                                  07820255
      LVON03 = .FALSE.                                                  07830255
      LVON04 = .FALSE.                                                  07840255
      LVON05 = .FALSE.                                                  07850255
      LVON06 = .FALSE.                                                  07860255
      LVON07 = .TRUE.                                                   07870255
      IF ( LVON01 )  THEN                                               07880255
           IVCOMP = IVCOMP * 2                                          07890255
           IF ( LVON02 )  THEN                                          07900255
                IVCOMP = IVCOMP * 3                                     07910255
                IF ( LVON03 )  THEN                                     07920255
                     IVCOMP = IVCOMP * 5                                07930255
                ELSE IF ( LVON04 )  THEN                                07940255
                     IVCOMP = IVCOMP * 7                                07950255
                ELSE IF ( LVON05 )  THEN                                07960255
                     IVCOMP = IVCOMP * 11                               07970255
                ELSE                                                    07980255
                     IVCOMP = IVCOMP * 13                               07990255
                END IF                                                  08000255
                IVCOMP = IVCOMP * 17                                    08010255
           ELSE IF ( LVON06 )  THEN                                     08020255
                IVCOMP = IVCOMP * 19                                    08030255
           ELSE                                                         08040255
                IVCOMP = IVCOMP * 23                                    08050255
           END IF                                                       08060255
           IVCOMP = IVCOMP * 29                                         08070255
      ELSE IF ( LVON07 )  THEN                                          08080255
           IVCOMP = IVCOMP * 31                                         08090255
      ELSE                                                              08100255
           IF ( .NOT. .FALSE. )  GO TO 0142                             08110255
      END IF                                                            08120255
      GO TO 0143                                                        08130255
 0142 IVCOMP = IVCOMP * 37                                              08140255
 0143 CONTINUE                                                          08150255
C                                                                       08160255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 31 = 1 * 31          ****08170255
C                                                                       08180255
      IVCORR = 31                                                       08190255
40140 IF ( IVCOMP - 31 )  20140, 10140, 20140                           08200255
30140 IVDELE = IVDELE + 1                                               08210255
      WRITE (I02,80000) IVTNUM                                          08220255
      IF (ICZERO) 10140, 0151, 20140                                    08230255
10140 IVPASS = IVPASS + 1                                               08240255
      WRITE (I02,80002) IVTNUM                                          08250255
      GO TO 0151                                                        08260255
20140 IVFAIL = IVFAIL + 1                                               08270255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08280255
 0151 CONTINUE                                                          08290255
C                                                                       08300255
C     ****  FCVS PROGRAM 255  -  TEST 015  ****                         08310255
C                                                                       08320255
C        TEST 015 SHOULD ONLY EXECUTE THE LOGIC IN ELSE-BLOCK 3.  THIS  08330255
C     LOGIC CONSISTS OF AN ARITHMETIC IF STATEMENT WHICH SHOULD TAKE    08340255
C     THE EXPRESSION EQUAL TO ZERO BRANCH TO STATEMENT 0152.            08350255
C                                                                       08360255
C                                                                       08370255
      IVTNUM =  15                                                      08380255
      IF (ICZERO) 30150, 0150, 30150                                    08390255
 0150 CONTINUE                                                          08400255
      IVCOMP = 1                                                        08410255
      LVON01 = .FALSE.                                                  08420255
      LVON02 = .FALSE.                                                  08430255
      LVON03 = .FALSE.                                                  08440255
      LVON04 = .FALSE.                                                  08450255
      LVON05 = .FALSE.                                                  08460255
      LVON06 = .FALSE.                                                  08470255
      LVON07 = .FALSE.                                                  08480255
      IF ( LVON01 )  THEN                                               08490255
           IVCOMP = IVCOMP * 2                                          08500255
           IF ( LVON02 )  THEN                                          08510255
                IVCOMP = IVCOMP * 3                                     08520255
                IF ( LVON03 )  THEN                                     08530255
                     IVCOMP = IVCOMP * 5                                08540255
                ELSE IF ( LVON04 )  THEN                                08550255
                     IVCOMP = IVCOMP * 7                                08560255
                ELSE IF ( LVON05 )  THEN                                08570255
                     IVCOMP = IVCOMP * 11                               08580255
                ELSE                                                    08590255
                     IVCOMP = IVCOMP * 13                               08600255
                END IF                                                  08610255
                IVCOMP = IVCOMP * 17                                    08620255
           ELSE IF ( LVON06 )  THEN                                     08630255
                IVCOMP = IVCOMP * 19                                    08640255
           ELSE                                                         08650255
                IVCOMP = IVCOMP * 23                                    08660255
           END IF                                                       08670255
           IVCOMP = IVCOMP * 29                                         08680255
      ELSE IF ( LVON07 )  THEN                                          08690255
           IVCOMP = IVCOMP * 31                                         08700255
      ELSE                                                              08710255
           ICON01 = 1                                                   08720255
           IF ( ICON01 - 1 )  0153, 0152, 0153                          08730255
      END IF                                                            08740255
      GO TO 0153                                                        08750255
 0152 IVCOMP = IVCOMP * 37                                              08760255
 0153 CONTINUE                                                          08770255
C                                                                       08780255
C        **** IVCOMP IS DETERMINED BY IVCOMP = 37 = 1 * 37          ****08790255
C                                                                       08800255
      IVCORR = 37                                                       08810255
40150 IF ( IVCOMP - 37 )  20150, 10150, 20150                           08820255
30150 IVDELE = IVDELE + 1                                               08830255
      WRITE (I02,80000) IVTNUM                                          08840255
      IF (ICZERO) 10150, 0161, 20150                                    08850255
10150 IVPASS = IVPASS + 1                                               08860255
      WRITE (I02,80002) IVTNUM                                          08870255
      GO TO 0161                                                        08880255
20150 IVFAIL = IVFAIL + 1                                               08890255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08900255
 0161 CONTINUE                                                          08910255
C                                                                       08920255
C     ****  FCVS PROGRAM 255  -  TEST 016  ****                         08930255
C                                                                       08940255
C        TEST 016 IS A TEST OF THE END IF STATEMENT.   SECTION 11.9     08950255
C     PERMITS TRANSFER OF CONTROL FROM ANYWHERE TO AN END IF STATEMENT. 08960255
C     ALSO ACCORDING TO SECTION 11.9 - EXECUTION OF AN END IF STATEMENT 08970255
C     HAS NO EFFECT.                                                    08980255
C                                                                       08990255
C                                                                       09000255
      IVTNUM =  16                                                      09010255
      IF (ICZERO) 30160, 0160, 30160                                    09020255
 0160 CONTINUE                                                          09030255
      IVCOMP = 1                                                        09040255
      LVON01 = .TRUE.                                                   09050255
      IF ( ICZERO )  0163, 0162, 0163                                   09060255
 0162 GO TO 0164                                                        09070255
 0163 IF ( LVON01 )  THEN                                               09080255
           IVCOMP = IVCOMP * 2                                          09090255
      ELSE                                                              09100255
           IVCOMP = IVCOMP * 3                                          09110255
 0164 END IF                                                            09120255
C                                                                       09130255
C        **** IVCOMP SHOULD REMAIN SET TO ONE (1).                  ****09140255
C                                                                       09150255
      IVCORR = 1                                                        09160255
40160 IF ( IVCOMP - 1 )  20160, 10160, 20160                            09170255
30160 IVDELE = IVDELE + 1                                               09180255
      WRITE (I02,80000) IVTNUM                                          09190255
      IF (ICZERO) 10160, 0171, 20160                                    09200255
10160 IVPASS = IVPASS + 1                                               09210255
      WRITE (I02,80002) IVTNUM                                          09220255
      GO TO 0171                                                        09230255
20160 IVFAIL = IVFAIL + 1                                               09240255
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09250255
 0171 CONTINUE                                                          09260255
C                                                                       09270255
C                                                                       09280255
C     WRITE OUT TEST SUMMARY                                            09290255
C                                                                       09300255
      WRITE (I02,90004)                                                 09310255
      WRITE (I02,90014)                                                 09320255
      WRITE (I02,90004)                                                 09330255
      WRITE (I02,90000)                                                 09340255
      WRITE (I02,90004)                                                 09350255
      WRITE (I02,90020) IVFAIL                                          09360255
      WRITE (I02,90022) IVPASS                                          09370255
      WRITE (I02,90024) IVDELE                                          09380255
      STOP                                                              09390255
90001 FORMAT (1H ,24X,5HFM255)                                          09400255
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM255)                          09410255
C                                                                       09420255
C     FORMATS FOR TEST DETAIL LINES                                     09430255
C                                                                       09440255
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   09450255
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      09460255
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         09470255
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    09480255
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        09490255
C                                                                       09500255
C     FORMAT STATEMENTS FOR PAGE HEADERS                                09510255
C                                                                       09520255
90002 FORMAT (1H1)                                                      09530255
90004 FORMAT (1H )                                                      09540255
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09550255
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   09560255
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         09570255
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  09580255
90014 FORMAT (1H ,5X,46H----------------------------------------------) 09590255
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             09600255
C                                                                       09610255
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 09620255
C                                                                       09630255
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              09640255
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              09650255
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             09660255
      END                                                               09670255
