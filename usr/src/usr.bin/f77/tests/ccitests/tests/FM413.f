      PROGRAM FM413                                                     00010413
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020413
C                                                                       00030413
C                                                                       00040413
C        THIS ROUTINE TESTS FOR PROPER PROCESSING OF UNFORMATTED RECORDS00050413
C     IN FILES CONNECTED FOR DIRECT ACCESS.  FOR THE SUBSET LANGUAGE A  00060413
C     FILE CONNECTED FOR DIRECT ACCESS MUST HAVE UNFORMATTED RECORDS    00070413
C     THIS ROUTINE FIRST TESTS SEVERAL SYNTACTICAL VARIATIONS OF THE    00080413
C     READ AND WRITE STATEMENTS USED IN CREATING AND ACCESSING          00090413
C     RECORDS OF THE FILE.  THE OPEN STATEMENT IS USED TO CONNECT       00100413
C     THE FILE TO  A  UNIT  AND ESTABLISH ITS CONNECTION FOR DIRECT     00110413
C     ACCESS.  THE FIRST SERIES OF TESTS CREATE  AND ACCESS   THE       00120413
C     RECORDS OF THE FILE IN RECORD NUMBER SEQUENCE AND THE LAST        00130413
C     SERIES OF TESTS CREATE AND ACCESS   RECORDS OF THE FILE IN RANDOM 00140413
C     ORDER.                                                            00150413
C                                                                       00160413
C        UNFORMATTED RECORDS MAY HAVE BOTH CHARACTER AND NONCHARACTER   00170413
C     DATA AND THIS DATA IS TRANSFERRED WITHOUT EDITING BETWEEN THE     00180413
C     CURRENT RECORD AND THE ENTITIES SPECIFIED BY THE INPUT/OUTPUT     00190413
C     LIST.   THIS ROUTINE BOTH READS AND WRITES RECORDS CONTAINING     00200413
C     THE DATA TYPES OF INTEGER ,REAL AND LOGICAL WITH I/O LIST ITEMS   00210413
C     REPRESENTED AS VARIABLE NAMES, ARRAY ELEMENT NAMES AND ARRAY      00220413
C     NAMES.  THIS ROUTINE DOES NOT TEST DATA OF TYPE CHARACTER.        00230413
C                                                                       00240413
C          ROUTINE FM411 TESTS USE OF UNFORMATTED RECORDS               00250413
C     WITH A FILE CONNECTED FOR SEQUENTIAL ACCESS.                      00260413
C                                                                       00270413
C        THIS ROUTINE TESTS                                             00280413
C                                                                       00290413
C             (1) THE STATEMENT CONSTRUCTS                              00300413
C                                                                       00310413
C                 A. WRITE (U,REC=RN)  VARIABLE-NAME,...                00320413
C                 B. WRITE (U,REC=RN)  ARRAY-ELEMENT-NAME,...           00330413
C                 C. WRITE (U,REC=RN)  ARRAY-NAME,...                   00340413
C                 D. WRITE (U,REC=RN)            -  NO OUTPUT LIST      00350413
C                 E. WRITE (U,REC=RN)  IMPLIED-DO-LIST                  00360413
C                 F. READ (U,REC=RN)   VARIABLE-NAME,...                00370413
C                 G. READ (U,REC=RN)   ARRAY-ELEMENT-NAME,...           00380413
C                 H. READ (U,REC=RN)   ARRAY-NAME,...                   00390413
C                 I. READ (U,REC=RN)             -  NO INPUT LIST       00400413
C                 J. READ (U,REC=RN)   IMPLIED-DO-LIST                  00410413
C                                                                       00420413
C             (2) USE OF A READ STATEMENT WHERE THE NUMBER OF VALUES    00430413
C                 IN THE INPUT LIST IS LESS THAN OR EQUAL TO THE        00440413
C                 NUMBER OF VALUES IN THE RECORD.                       00450413
C             (3) USE OF THE STATEMENT                                  00460413
C                      OPEN (U,ACCESS='DIRECT',RECL=RL)                 00470413
C                 FOR CONNECTING A FILE TO THE UNIT.                    00480413
C                                                                       00490413
C             (4) THAT THE RECORDS OF A DIRECT ACCESS FILE NEED NOT BE  00500413
C                 BE CREATED AND READ IN ORDER OF THEIR RECORD NUMBERS. 00510413
C                                                                       00520413
C             (5) THAT THE VALUES OF THE RECORD MAY BE CHANGED WHEN     00530413
C                 THE RECORD IS REWRITTEN.                              00540413
C     REFERENCES -                                                      00550413
C                                                                       00560413
C           AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,    00570413
C           X3.9-1977                                                   00580413
C                                                                       00590413
C             SECTION 4.1,        DATA TYPES                            00600413
C             SECTION 12.1.2,     UNFORMATTED RECORD                    00610413
C             SECTION 12.2.4,     FILE ACCESS                           00620413
C             SECTION 12.2.4.2,   DIRECT ACCESS                         00630413
C             SECTION 12.3.3,     UNIT SPECIFIER AND IDENTIFIER         00640413
C             SECTION 12.7.2,     END-OF-FILE SPECIFIER                 00650413
C             SECTION 12.8,       READ, WRITE AND PRINT STATEMENTS      00660413
C             SECTION 12.8.1,     CONTROL INFORMATION LIST              00670413
C             SECTION 12.8.2,     INPUT/OUTPUT LIST                     00680413
C             SECTION 12.8.2.1,   INPUT LIST ITEMS                      00690413
C             SECTION 12.8.2.2,   OUTPUT LIST ITEMS                     00700413
C             SECTION 12.8.2.3,   IMPLIED-DO  LIST                      00710413
C             SECTION 12.9.5.1,   UNFORMATTED DATA TRANSFER             00720413
C             SECTION 12.10.1,    OPEN STATEMENT                        00730413
C                                                                       00740413
C                                                                       00750413
C                                                                       00760413
C     ******************************************************************00770413
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00780413
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00790413
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00800413
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00810413
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00820413
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00830413
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00840413
C     THE RESULT OF EXECUTING THESE TESTS.                              00850413
C                                                                       00860413
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00870413
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00880413
C                                                                       00890413
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00900413
C                    DEPARTMENT OF THE NAVY                             00910413
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00920413
C                    WASHINGTON, D.C.   20376                           00930413
C                                                                       00940413
C     ******************************************************************00950413
C                                                                       00960413
C                                                                       00970413
      IMPLICIT LOGICAL (L)                                              00980413
      IMPLICIT CHARACTER*14 (C)                                         00990413
C                                                                       01000413
      LOGICAL  LAON11, LAON21, LAON31, LCONT1, LCONF2, LVONT1, LVONF2   01010413
      LOGICAL  LAON12, LAON22, LAON32, LCONT3, LCONF4, LVONT3, LVONF4   01020413
      LOGICAL  LCONT5, LCONF6, LCONT7, LCONF8, LVONT5, LVONF6, LVONT7   01030413
      LOGICAL LVONF8                                                    01040413
      DIMENSION IDUMP(80)                                               01050413
      DIMENSION IAON11(8), IAON21(2,4), IAON31(2,2,2)                   01060413
      DIMENSION IAON12(8), IAON22(2,4), IAON32(2,2,2)                   01070413
      DIMENSION RAON11(8), RAON21(2,4), RAON31(2,2,2)                   01080413
      DIMENSION RAON12(8), RAON22(2,4), RAON32(2,2,2)                   01090413
      DIMENSION LAON11(8), LAON21(2,4), LAON31(2,2,2)                   01100413
      DIMENSION LAON12(8), LAON22(2,4), LAON32(2,2,2)                   01110413
      DATA  IAON11 /11, -11, 777, -777, 512, -512, -32767, 32767/       01120413
      DATA  IAON21 /11, -11, 777, -777, 512, -512, -32767, 32767/       01130413
      DATA  IAON31 /11, -11, 777, -777, 512, -512, -32767, 32767/       01140413
      DATA  LAON11 /.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE.,  01150413
     1              .TRUE., .FALSE./                                    01160413
      DATA  LAON21 /.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE.,  01170413
     1              .TRUE., .FALSE./                                    01180413
      DATA  LAON31 /.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE.,  01190413
     1              .TRUE., .FALSE./                                    01200413
      DATA  RAON11 /11., -11., 7.77, -7.77,.512, -.512, -32767., 32767./01210413
      DATA  RAON21 /11., -11., 7.77, -7.77,.512, -.512, -32767., 32767./01220413
      DATA  RAON31 /11., -11., 7.77, -7.77,.512, -.512, -32767., 32767./01230413
      ICON21 = 11                                                       01240413
      ICON22 = -11                                                      01250413
      ICON31 = +777                                                     01260413
      ICON32 = -777                                                     01270413
      ICON33 =  512                                                     01280413
      ICON34 = -512                                                     01290413
      ICON55 = -32767                                                   01300413
      ICON56 =  32767                                                   01310413
      RCON21 = 11.                                                      01320413
      RCON22 = -11.                                                     01330413
      RCON31 = +7.77                                                    01340413
      RCON32 = -7.77                                                    01350413
      RCON33 = .512                                                     01360413
      RCON34 = -.512                                                    01370413
      RCON55 = -32767.                                                  01380413
      RCON56 =  32767.                                                  01390413
      LCONT1 = .TRUE.                                                   01400413
      LCONF2 = .FALSE.                                                  01410413
      LCONT3 = .TRUE.                                                   01420413
      LCONF4 = .FALSE.                                                  01430413
      LCONT5 = .TRUE.                                                   01440413
      LCONF6 = .FALSE.                                                  01450413
      LCONT7 = .TRUE.                                                   01460413
      LCONF8 = .FALSE.                                                  01470413
C                                                                       01480413
C          THE FILE USED IN THIS ROUTINE HAS THE FOLLOWING PROPERTIES   01490413
C                                                                       01500413
C                  FILE IDENTIFIER     - I10 (X-NUMBER 10)              01510413
C                  RECORD SIZE         - 80                             01520413
C                  ACCESS METHOD       - DIRECT                         01530413
C                  RECORD TYPE         - UNFORMATTED                    01540413
C                  DESIGNATED DEVICE   - DISK                           01550413
C                  TYPE OF DATA        - INTEGER, REAL AND LOGICAL      01560413
C                  RECORDS IN FILE     - 214                            01570413
C                                                                       01580413
C          THE FIRST 6 FIELDS OF EACH RECORD IN THE FILE UNIQUELY IDENT-01590413
C     IFIES THAT RECORD.  THE REMAINING FIELDS OF THE RECORD CONTAIN    01600413
C     DATA WHICH ARE USED IN TESTING.  A DESCRIPTION OF EACH FIELD      01610413
C     OF THE  PREAMBLE FOLLOWS.                                         01620413
C                                                                       01630413
C                  VARIABLE NAME IN PROGRAM          FIELD NUMBER       01640413
C                  ------------------------          ------------       01650413
C                                                                       01660413
C                  IPROG  (ROUTINE NAME)         -       1              01670413
C                  IFILE  (LOGICAL/X-NUMBER)     -       2              01680413
C                  ITOTR  (RECORDS IN FILE)      -       3              01690413
C                  IRLGN  (LENGTH OF RECORD)     -       4              01700413
C                  IRECN  (RECORD NUMBER)        -       5              01710413
C                  IEOF   (9999 IF LAST RECORD)  -       6              01720413
C                                                                       01730413
C                                                                       01740413
C                                                                       01750413
C                                                                       01760413
C     INITIALIZATION SECTION.                                           01770413
C                                                                       01780413
C     INITIALIZE CONSTANTS                                              01790413
C     ********************                                              01800413
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01810413
      I01 = 5                                                           01820413
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01830413
      I02 = 6                                                           01840413
C     SYSTEM ENVIRONMENT SECTION                                        01850413
C                                                                       01860413
      I01 = 5                                                           01870413
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01880413
C     (UNIT NUMBER FOR CARD READER).                                    01890413
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01900413
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01910413
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01920413
C                                                                       01930413
      I02 = 6                                                           01940413
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01950413
C     (UNIT NUMBER FOR PRINTER).                                        01960413
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01970413
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01980413
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01990413
C                                                                       02000413
      IVPASS = 0                                                        02010413
      IVFAIL = 0                                                        02020413
      IVDELE = 0                                                        02030413
      ICZERO = 0                                                        02040413
C                                                                       02050413
C     WRITE OUT PAGE HEADERS                                            02060413
C                                                                       02070413
      WRITE (I02,90002)                                                 02080413
      WRITE (I02,90006)                                                 02090413
      WRITE (I02,90008)                                                 02100413
      WRITE (I02,90004)                                                 02110413
      WRITE (I02,90010)                                                 02120413
      WRITE (I02,90004)                                                 02130413
      WRITE (I02,90016)                                                 02140413
      WRITE (I02,90001)                                                 02150413
      WRITE (I02,90004)                                                 02160413
      WRITE (I02,90012)                                                 02170413
      WRITE (I02,90014)                                                 02180413
      WRITE (I02,90004)                                                 02190413
C                                                                       02200413
      I10 = 9                                                           02210413
C     I10  CONTAINS THE LOGICAL UNIT NUMBER FOR A DIRECT ACCESS FILE    02220413
C     WITH UNFORMATTED RECORDS                                          02230413
      OPEN(UNIT=I10,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=80)         02240413
CX101        THE CARD IS REPLACED BY CONTENTS OF X-101 CARD             02250413
      IPROG = 413                                                       02260413
      IFILE = I10                                                       02270413
      ITOTR = 214                                                       02280413
      IRLGN = 80                                                        02290413
      IRECN = 0                                                         02300413
      IEOF = 0                                                          02310413
C                                                                       02320413
C                                                                       02330413
C                                                                       02340413
C        TESTS 001 THROUGH 013 OPEN A FILE CONNECTED FOR DIRECT ACCESS  02350413
C     AND WRITE 12 RECORDS INTO THE FILE.  THESE TESTS TEST USE OF THE  02360413
C     ALLOWABLE FORMS OF THE OPEN AND WRITE STATEMENTS ON A FILE        02370413
C     CONNECTED FOR DIRECT ACCESS.  THE WRITE STATEMENT IS USED WITH    02380413
C     THE I/O LIST ITEM AS A VARIABLE, ARRAY ELEMENT AND AN ARRAY.      02390413
C        THE PURPOSE OF TESTS 001 THROUGH 013 IS TO CHECK THE COMPILER'S02400413
C     ABILITY TO HANDLE THE VARIOUS STATEMENT  CONSTRUCTS OF THE OPEN   02410413
C     AND WRITE STATEMENTS.  LATER TESTS WITHIN THIS ROUTINE READ       02420413
C     AND CHECK THE RECORDS WHICH WERE CREATED.                         02430413
C        THE VALUE IN IVCORR FOR TESTS 002 THROUGH 013 IS THE RECORD    02440413
C     NUMBER USED TO WRITE THE RECORD.                                  02450413
C                                                                       02460413
C                                                                       02470413
C                                                                       02480413
C     ****  FCVS PROGRAM 413  -  TEST 001  ****                         02490413
C                                                                       02500413
C                                                                       02510413
C        TEST 001 USES THE OPEN STATEMENT TO CONNECT A FILE FOR DIRECT  02520413
C       ACCESS.  THIS IS THE FIRST ROUTINE TO USE AN OPEN STATEMENT.    02530413
C                                                                       02540413
C                                                                       02550413
      IVTNUM =   1                                                      02560413
      IF (ICZERO) 30010, 0010, 30010                                    02570413
 0010 CONTINUE                                                          02580413
      IVCORR = 1                                                        02590413
      IVCOMP = 0                                                        02600413
      OPEN ( I10, ACCESS = 'DIRECT', RECL = 80 )                        02610413
      IVCOMP = 1                                                        02620413
40010 IF (IVCOMP - 1) 20010, 10010, 20010                               02630413
30010 IVDELE = IVDELE + 1                                               02640413
      WRITE (I02,80000) IVTNUM                                          02650413
      IF (ICZERO) 10010, 0021, 20010                                    02660413
10010 IVPASS = IVPASS + 1                                               02670413
      WRITE (I02,80002) IVTNUM                                          02680413
      GO TO 0021                                                        02690413
20010 IVFAIL = IVFAIL + 1                                               02700413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02710413
 0021 CONTINUE                                                          02720413
C                                                                       02730413
C     ****  FCVS PROGRAM 413  -  TEST 002  ****                         02740413
C                                                                       02750413
C                                                                       02760413
C        TEST 002 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     02770413
C     IS A VARIABLE OF INTEGER TYPE.                                    02780413
C                                                                       02790413
C                                                                       02800413
      IVTNUM =   2                                                      02810413
      IF (ICZERO) 30020, 0020, 30020                                    02820413
 0020 CONTINUE                                                          02830413
      IRECN = 01                                                        02840413
      IVCORR = 01                                                       02850413
      WRITE (I10,REC=01)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    02860413
     1   ICON21, ICON22, ICON31, ICON32, ICON33, ICON34, ICON55, ICON56 02870413
      IVCOMP = IRECN                                                    02880413
40020 IF (IVCOMP - 01)  20020, 10020, 20020                             02890413
30020 IVDELE = IVDELE + 1                                               02900413
      WRITE (I02,80000) IVTNUM                                          02910413
      IF (ICZERO) 10020, 0031, 20020                                    02920413
10020 IVPASS = IVPASS + 1                                               02930413
      WRITE (I02,80002) IVTNUM                                          02940413
      GO TO 0031                                                        02950413
20020 IVFAIL = IVFAIL + 1                                               02960413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02970413
 0031 CONTINUE                                                          02980413
C                                                                       02990413
C     ****  FCVS PROGRAM 413  -  TEST 003  ****                         03000413
C                                                                       03010413
C                                                                       03020413
C        TEST 003 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     03030413
C     IS A VARIABLE OF REAL TYPE.                                       03040413
C                                                                       03050413
C                                                                       03060413
      IVTNUM =   3                                                      03070413
      IF (ICZERO) 30030, 0030, 30030                                    03080413
 0030 CONTINUE                                                          03090413
      IRECN = 02                                                        03100413
      IVCORR = 02                                                       03110413
      WRITE (I10,REC=02)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    03120413
     1   RCON21, RCON22, RCON31, RCON32, RCON33, RCON34, RCON55, RCON56 03130413
      IVCOMP = IRECN                                                    03140413
40030 IF (IVCOMP - 02)  20030, 10030, 20030                             03150413
30030 IVDELE = IVDELE + 1                                               03160413
      WRITE (I02,80000) IVTNUM                                          03170413
      IF (ICZERO) 10030, 0041, 20030                                    03180413
10030 IVPASS = IVPASS + 1                                               03190413
      WRITE (I02,80002) IVTNUM                                          03200413
      GO TO 0041                                                        03210413
20030 IVFAIL = IVFAIL + 1                                               03220413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03230413
 0041 CONTINUE                                                          03240413
C                                                                       03250413
C     ****  FCVS PROGRAM 413  -  TEST 004  ****                         03260413
C                                                                       03270413
C                                                                       03280413
C        TEST 004 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     03290413
C     IS A VARIABLE OF LOGICAL TYPE.                                    03300413
C                                                                       03310413
C                                                                       03320413
      IVTNUM =   4                                                      03330413
      IF (ICZERO) 30040, 0040, 30040                                    03340413
 0040 CONTINUE                                                          03350413
      IRECN = 03                                                        03360413
      IVCORR = 03                                                       03370413
      WRITE (I10,REC=03)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    03380413
     1   LCONT1, LCONF2,  LCONT3, LCONF4, LCONT5, LCONF6, LCONT7, LCONF803390413
      IVCOMP = IRECN                                                    03400413
40040 IF (IVCOMP - 03)  20040, 10040, 20040                             03410413
30040 IVDELE = IVDELE + 1                                               03420413
      WRITE (I02,80000) IVTNUM                                          03430413
      IF (ICZERO) 10040, 0051, 20040                                    03440413
10040 IVPASS = IVPASS + 1                                               03450413
      WRITE (I02,80002) IVTNUM                                          03460413
      GO TO 0051                                                        03470413
20040 IVFAIL = IVFAIL + 1                                               03480413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03490413
 0051 CONTINUE                                                          03500413
C                                                                       03510413
C     ****  FCVS PROGRAM 413  -  TEST 005  ****                         03520413
C                                                                       03530413
C                                                                       03540413
C        TEST 005 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     03550413
C     IS AN ARRAY ELEMENT OF INTEGER TYPE.   ONE, TWO AND THREE         03560413
C     DIMENSION ARRAYS ARE USED.                                        03570413
C                                                                       03580413
C                                                                       03590413
      IVTNUM =   5                                                      03600413
      IF (ICZERO) 30050, 0050, 30050                                    03610413
 0050 CONTINUE                                                          03620413
      IRECN = 04                                                        03630413
      IVCORR = 04                                                       03640413
      WRITE (I10,REC=04)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    03650413
     1   IAON11(1), IAON11(2), IAON21(1,2), IAON21(2,2), IAON31(1,1,2), 03660413
     2   IAON31(2,1,2), IAON11(7), IAON11(8)                            03670413
      IVCOMP = IRECN                                                    03680413
40050 IF (IVCOMP - 04)  20050, 10050, 20050                             03690413
30050 IVDELE = IVDELE + 1                                               03700413
      WRITE (I02,80000) IVTNUM                                          03710413
      IF (ICZERO) 10050, 0061, 20050                                    03720413
10050 IVPASS = IVPASS + 1                                               03730413
      WRITE (I02,80002) IVTNUM                                          03740413
      GO TO 0061                                                        03750413
20050 IVFAIL = IVFAIL + 1                                               03760413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03770413
 0061 CONTINUE                                                          03780413
C                                                                       03790413
C     ****  FCVS PROGRAM 413  -  TEST 006  ****                         03800413
C                                                                       03810413
C                                                                       03820413
C        TEST 006 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     03830413
C     IS AN ARRAY ELEMENT OF REAL TYPE.  ONE, TWO AND THREE             03840413
C     DIMENSION ARRAYS ARE USED.                                        03850413
C                                                                       03860413
C                                                                       03870413
      IVTNUM =   6                                                      03880413
      IF (ICZERO) 30060, 0060, 30060                                    03890413
 0060 CONTINUE                                                          03900413
      IRECN = 05                                                        03910413
      IVCORR = 05                                                       03920413
      WRITE (I10,REC=05)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    03930413
     1   RAON11(1), RAON11(2), RAON21(1,2), RAON21(2,2), RAON31(1,1,2), 03940413
     2   RAON31(2,1,2), RAON11(7), RAON11(8)                            03950413
      IVCOMP = IRECN                                                    03960413
40060 IF (IVCOMP - 05)  20060, 10060, 20060                             03970413
30060 IVDELE = IVDELE + 1                                               03980413
      WRITE (I02,80000) IVTNUM                                          03990413
      IF (ICZERO) 10060, 0071, 20060                                    04000413
10060 IVPASS = IVPASS + 1                                               04010413
      WRITE (I02,80002) IVTNUM                                          04020413
      GO TO 0071                                                        04030413
20060 IVFAIL = IVFAIL + 1                                               04040413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04050413
 0071 CONTINUE                                                          04060413
C                                                                       04070413
C     ****  FCVS PROGRAM 413  -  TEST 007  ****                         04080413
C                                                                       04090413
C                                                                       04100413
C        TEST 007 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     04110413
C     IS AN ARRAY ELEMENT OF LOGICAL TYPE.  ONE, TWO AND THREE          04120413
C     DIMENSION ARRAYS ARE USED.                                        04130413
C                                                                       04140413
C                                                                       04150413
      IVTNUM =   7                                                      04160413
      IF (ICZERO) 30070, 0070, 30070                                    04170413
 0070 CONTINUE                                                          04180413
      IRECN = 06                                                        04190413
      IVCORR = 06                                                       04200413
      WRITE (I10,REC=06)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    04210413
     1   LAON11(1), LAON11(2), LAON21(1,2), LAON21(2,2), LAON31(1,1,2), 04220413
     2   LAON31(2,1,2), LAON11(7), LAON11(8)                            04230413
      IVCOMP = IRECN                                                    04240413
40070 IF (IVCOMP - 06)  20070, 10070, 20070                             04250413
30070 IVDELE = IVDELE + 1                                               04260413
      WRITE (I02,80000) IVTNUM                                          04270413
      IF (ICZERO) 10070, 0081, 20070                                    04280413
10070 IVPASS = IVPASS + 1                                               04290413
      WRITE (I02,80002) IVTNUM                                          04300413
      GO TO 0081                                                        04310413
20070 IVFAIL = IVFAIL + 1                                               04320413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04330413
 0081 CONTINUE                                                          04340413
C                                                                       04350413
C     ****  FCVS PROGRAM 413  -  TEST 008  ****                         04360413
C                                                                       04370413
C                                                                       04380413
C        TEST 008 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     04390413
C     IS AN ARRAY OF INTEGER TYPE.                                      04400413
C                                                                       04410413
C                                                                       04420413
      IVTNUM =   8                                                      04430413
      IF (ICZERO) 30080, 0080, 30080                                    04440413
 0080 CONTINUE                                                          04450413
      IRECN = 07                                                        04460413
      IVCORR = 07                                                       04470413
      WRITE (I10,REC=07)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    04480413
     1   IAON31                                                         04490413
      IVCOMP = IRECN                                                    04500413
40080 IF (IVCOMP - 07)  20080, 10080, 20080                             04510413
30080 IVDELE = IVDELE + 1                                               04520413
      WRITE (I02,80000) IVTNUM                                          04530413
      IF (ICZERO) 10080, 0091, 20080                                    04540413
10080 IVPASS = IVPASS + 1                                               04550413
      WRITE (I02,80002) IVTNUM                                          04560413
      GO TO 0091                                                        04570413
20080 IVFAIL = IVFAIL + 1                                               04580413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04590413
 0091 CONTINUE                                                          04600413
C                                                                       04610413
C     ****  FCVS PROGRAM 413  -  TEST 009  ****                         04620413
C                                                                       04630413
C                                                                       04640413
C        TEST 009 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     04650413
C     IS AN ARRAY OF REAL TYPE.                                         04660413
C                                                                       04670413
C                                                                       04680413
      IVTNUM =   9                                                      04690413
      IF (ICZERO) 30090, 0090, 30090                                    04700413
 0090 CONTINUE                                                          04710413
      IRECN = 08                                                        04720413
      IVCORR = 08                                                       04730413
      WRITE (I10,REC=08)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    04740413
     1   RAON31                                                         04750413
      IVCOMP = IRECN                                                    04760413
40090 IF (IVCOMP - 08)  20090, 10090, 20090                             04770413
30090 IVDELE = IVDELE + 1                                               04780413
      WRITE (I02,80000) IVTNUM                                          04790413
      IF (ICZERO) 10090, 0101, 20090                                    04800413
10090 IVPASS = IVPASS + 1                                               04810413
      WRITE (I02,80002) IVTNUM                                          04820413
      GO TO 0101                                                        04830413
20090 IVFAIL = IVFAIL + 1                                               04840413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04850413
 0101 CONTINUE                                                          04860413
C                                                                       04870413
C     ****  FCVS PROGRAM 413  -  TEST 010  ****                         04880413
C                                                                       04890413
C                                                                       04900413
C        TEST 010 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     04910413
C     IS AN ARRAY OF LOGICAL TYPE.                                      04920413
C                                                                       04930413
C                                                                       04940413
      IVTNUM =  10                                                      04950413
      IF (ICZERO) 30100, 0100, 30100                                    04960413
 0100 CONTINUE                                                          04970413
      IRECN = 09                                                        04980413
      IVCORR = 09                                                       04990413
      WRITE (I10,REC=09)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05000413
     1   LAON31                                                         05010413
      IVCOMP = IRECN                                                    05020413
40100 IF (IVCOMP - 09)  20100, 10100, 20100                             05030413
30100 IVDELE = IVDELE + 1                                               05040413
      WRITE (I02,80000) IVTNUM                                          05050413
      IF (ICZERO) 10100, 0111, 20100                                    05060413
10100 IVPASS = IVPASS + 1                                               05070413
      WRITE (I02,80002) IVTNUM                                          05080413
      GO TO 0111                                                        05090413
20100 IVFAIL = IVFAIL + 1                                               05100413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05110413
 0111 CONTINUE                                                          05120413
C                                                                       05130413
C     ****  FCVS PROGRAM 413  -  TEST 011  ****                         05140413
C                                                                       05150413
C                                                                       05160413
C        TEST 011 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     05170413
C     IS AN IMPLIED-DO   WITH AN ITEM OF INTEGER TYPE.                  05180413
C        THE FIELD VALUES ARE WRITTEN IN MIXED ORDER VIS-A-VIS THE      05190413
C     ELEMENT SEQUENCE OF ARRAY IAON31.  THE SEQUENCE OF VALUES WRITTEN 05200413
C     IN THE RECORD ARE 11, 512, 777, -32767, -11, -512, -777, 32767.   05210413
C                                                                       05220413
C                                                                       05230413
      IVTNUM =  11                                                      05240413
      IF (ICZERO) 30110, 0110, 30110                                    05250413
 0110 CONTINUE                                                          05260413
      IRECN = 10                                                        05270413
      IVCORR = 10                                                       05280413
      WRITE (I10,REC=10)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05290413
     1   (((IAON31 (J,K,I), I=1,2), K=1,2), J=1,2)                      05300413
      IVCOMP = IRECN                                                    05310413
40110 IF (IVCOMP - 10)  20110, 10110, 20110                             05320413
30110 IVDELE = IVDELE + 1                                               05330413
      WRITE (I02,80000) IVTNUM                                          05340413
      IF (ICZERO) 10110, 0121, 20110                                    05350413
10110 IVPASS = IVPASS + 1                                               05360413
      WRITE (I02,80002) IVTNUM                                          05370413
      GO TO 0121                                                        05380413
20110 IVFAIL = IVFAIL + 1                                               05390413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05400413
 0121 CONTINUE                                                          05410413
C                                                                       05420413
C     ****  FCVS PROGRAM 413  -  TEST 012  ****                         05430413
C                                                                       05440413
C                                                                       05450413
C        TEST 012 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     05460413
C     IS AN IMPLIED-DO WITH AN ITEM OF REAL TYPE.  THE FIELD VALUES     05470413
C     (IN FIELD POSITION ORDER) WRITTEN IN THE RECORD ARE 11., -11.,    05480413
C     7.77, -7.77, .512, -.512, -32767., 32767.                         05490413
C                                                                       05500413
C                                                                       05510413
      IVTNUM =  12                                                      05520413
      IF (ICZERO) 30120, 0120, 30120                                    05530413
 0120 CONTINUE                                                          05540413
      IRECN = 11                                                        05550413
      IVCORR = 11                                                       05560413
      WRITE (I10,REC=11)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05570413
     1   (((RAON31 (J,K,I), J=1,2), K=1,2), I=1,2)                      05580413
      IVCOMP = IRECN                                                    05590413
40120 IF (IVCOMP - 11)  20120, 10120, 20120                             05600413
30120 IVDELE = IVDELE + 1                                               05610413
      WRITE (I02,80000) IVTNUM                                          05620413
      IF (ICZERO) 10120, 0131, 20120                                    05630413
10120 IVPASS = IVPASS + 1                                               05640413
      WRITE (I02,80002) IVTNUM                                          05650413
      GO TO 0131                                                        05660413
20120 IVFAIL = IVFAIL + 1                                               05670413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05680413
 0131 CONTINUE                                                          05690413
C                                                                       05700413
C     ****  FCVS PROGRAM 413  -  TEST 013  ****                         05710413
C                                                                       05720413
C                                                                       05730413
C        TEST 013 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     05740413
C     IS AN IMPLIED-DO   WITH AN ITEM OF LOGICAL TYPE.                  05750413
C        THE FIELD VALUES ARE WRITTEN IN MIXED ORDER VIS-A-VIS THE      05760413
C     ELEMENT SEQUENCE OF ARRAY LAON31.  THE SEQUENCE OF VALUES WRITTEN 05770413
C     IN THE RECORD ARE .TRUE., .TRUE., .FALSE., .FALSE., .TRUE., .TRUE.05780413
C     .FALSE, .FALSE.                                                   05790413
C                                                                       05800413
C                                                                       05810413
      IVTNUM =  13                                                      05820413
      IF (ICZERO) 30130, 0130, 30130                                    05830413
 0130 CONTINUE                                                          05840413
      IRECN = 12                                                        05850413
      IVCORR = 12                                                       05860413
      WRITE (I10,REC=12)    IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05870413
     1   (((LAON31 (J,K,I), K=1,2), J=1,2), I=1,2)                      05880413
      IVCOMP = IRECN                                                    05890413
40130 IF (IVCOMP - 12)  20130, 10130, 20130                             05900413
30130 IVDELE = IVDELE + 1                                               05910413
      WRITE (I02,80000) IVTNUM                                          05920413
      IF (ICZERO) 10130, 0141, 20130                                    05930413
10130 IVPASS = IVPASS + 1                                               05940413
      WRITE (I02,80002) IVTNUM                                          05950413
      GO TO 0141                                                        05960413
20130 IVFAIL = IVFAIL + 1                                               05970413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05980413
 0141 CONTINUE                                                          05990413
C                                                                       06000413
C                                                                       06010413
C        TESTS  14 AND 15 TEST THE WRITE WITHOUT OUTPUT LIST ITEMS.     06020413
C                                                                       06030413
C                                                                       06040413
C                                                                       06050413
C                                                                       06060413
C     ****  FCVS PROGRAM 413  -  TEST 014  ****                         06070413
C                                                                       06080413
C                                                                       06090413
C        TEST 014 USES A WRITE STATEMENT WITHOUT ANY OUTPUT LIST ITEMS. 06100413
C     THE OUTPUT LIST ITEMS ARE OPTIONAL  AND THIS TEST USES THIS FORM  06110413
C     TO ESTABLISH A RECORD NUMBER FOR A RECORD IN THE FILE.            06120413
C     ALSO THE LENGTH OF AN UNFORMATTED RECORD MAY BE ZERO.             06130413
C                                                                       06140413
C                  SEE SECTIONS 12.1.2,   UNFORMATTED RECORDS           06150413
C                               12.2.4.2 (5) AND (6), DIRECT ACCESS     06160413
C                               12.8,  READ, WRITE AND PRINT STATEMENTS 06170413
C                                                                       06180413
C                                                                       06190413
      IVTNUM =  14                                                      06200413
      IF (ICZERO) 30140, 0140, 30140                                    06210413
 0140 CONTINUE                                                          06220413
      IRECN = 13                                                        06230413
      IVCORR = 13                                                       06240413
      WRITE (I10,REC=13)                                                06250413
      IVCOMP = IRECN                                                    06260413
40140 IF (IVCOMP - 13)  20140, 10140, 20140                             06270413
30140 IVDELE = IVDELE + 1                                               06280413
      WRITE (I02,80000) IVTNUM                                          06290413
      IF (ICZERO) 10140, 0151, 20140                                    06300413
10140 IVPASS = IVPASS + 1                                               06310413
      WRITE (I02,80002) IVTNUM                                          06320413
      GO TO 0151                                                        06330413
20140 IVFAIL = IVFAIL + 1                                               06340413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06350413
 0151 CONTINUE                                                          06360413
C                                                                       06370413
C     ****  FCVS PROGRAM 413  -  TEST 015  ****                         06380413
C                                                                       06390413
C                                                                       06400413
C        TEST 015 IS SIMILAR TO TEST 014 ABOVE EXCEPT THE RN OF THE     06410413
C     RECORD SPECIFIER (REC = RN) IS AN INTEGER VARIABLE.               06420413
C                                                                       06430413
C                                                                       06440413
      IVTNUM =  15                                                      06450413
      IF (ICZERO) 30150, 0150, 30150                                    06460413
 0150 CONTINUE                                                          06470413
      IRECN = 14                                                        06480413
      IVCORR = 14                                                       06490413
      IREC = 14                                                         06500413
      WRITE (I10,REC = IREC)                                            06510413
      IVCOMP = IRECN                                                    06520413
40150 IF (IVCOMP - 14)  20150, 10150, 20150                             06530413
30150 IVDELE = IVDELE + 1                                               06540413
      WRITE (I02,80000) IVTNUM                                          06550413
      IF (ICZERO) 10150, 0161, 20150                                    06560413
10150 IVPASS = IVPASS + 1                                               06570413
      WRITE (I02,80002) IVTNUM                                          06580413
      GO TO 0161                                                        06590413
20150 IVFAIL = IVFAIL + 1                                               06600413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06610413
 0161 CONTINUE                                                          06620413
C                                                                       06630413
C                                                                       06640413
C        TESTS  16  AND  17  VERIFY THAT RECORDS MAY BE CREATED IN      06650413
C     OTHER THAN SEQUENTIAL ORDER.  ALSO THAT A VARIABLE MAY BY USED    06660413
C     AS THE OPERAND OF THE REC SPECIFIER FOR A WRITE STATEMENT.        06670413
C                                                                       06680413
C                                                                       06690413
C                                                                       06700413
C     ****  FCVS PROGRAM 413  -  TEST 016  ****                         06710413
C                                                                       06720413
C                                                                       06730413
C        TEST 016 TESTS USE OF THE REC SPECIFIER WHERE THE OPERAND      06740413
C     IS A VARIABLE.  THIS TEST IS SIMILAR  TO TEST 15 EXCEPT THE WRITE 06750413
C     STATEMENT CONTAINS OUTPUT LIST ITEMS.  ONE HUNDRED RECORDS ARE    06760413
C     WRITTEN BY INCREMENTING THE VARIABLE BY 2 FOR EACH WRITE.   TEST  06770413
C     032 READS THE RECORDS WRITTEN BY THIS METHOD.                     06780413
C                                                                       06790413
C                                                                       06800413
      IVTNUM =  16                                                      06810413
      IF (ICZERO) 30160, 0160, 30160                                    06820413
 0160 CONTINUE                                                          06830413
      IRECN = 13                                                        06840413
      IREC = 13                                                         06850413
      DO 4132 I = 1,100                                                 06860413
      IREC = IREC + 2                                                   06870413
      IRECN = IRECN + 2                                                 06880413
      WRITE (I10, REC = IREC) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,  06890413
     1   ICON21, ICON22, ICON31, ICON32, ICON33, ICON34, ICON55, ICON56 06900413
 4132 CONTINUE                                                          06910413
      IVCORR = 100                                                      06920413
      IVCOMP = IREC - 113                                               06930413
40160 IF (IVCOMP - 100) 20160, 10160, 20160                             06940413
30160 IVDELE = IVDELE + 1                                               06950413
      WRITE (I02,80000) IVTNUM                                          06960413
      IF (ICZERO) 10160, 0171, 20160                                    06970413
10160 IVPASS = IVPASS + 1                                               06980413
      WRITE (I02,80002) IVTNUM                                          06990413
      GO TO 0171                                                        07000413
20160 IVFAIL = IVFAIL + 1                                               07010413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07020413
 0171 CONTINUE                                                          07030413
C                                                                       07040413
C     ****  FCVS PROGRAM 413  -  TEST 017  ****                         07050413
C                                                                       07060413
C                                                                       07070413
C        TEST  17 IS SIMILAR  TO TEST 16 EXCEPT THE RECORD IS           07080413
C     WRITTEN IN REVERSE ORDER OF RECORD NUMBER.  ONE HUNDERD RECORDS   07090413
C     ARE WRITTEN AND THE VARIABLE OF THE REC SPECIFIER IS DECREMENTED  07100413
C     BY TWO FOR EACH WRITE.                                            07110413
C                                                                       07120413
C                                                                       07130413
      IVTNUM =  17                                                      07140413
      IF (ICZERO) 30170, 0170, 30170                                    07150413
 0170 CONTINUE                                                          07160413
      IRECN = 216                                                       07170413
      IREC = 216                                                        07180413
      IVCOMP = 0                                                        07190413
      DO 4133 I=1,100                                                   07200413
      IREC = IREC - 2                                                   07210413
      IRECN = IRECN - 2                                                 07220413
      WRITE (I10, REC = IREC) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,  07230413
     1   ICON21, ICON22, ICON31, ICON32, ICON33, ICON34, ICON55, ICON56 07240413
      IVCOMP = IVCOMP + 1                                               07250413
 4133 CONTINUE                                                          07260413
      IVCORR = 100                                                      07270413
40170 IF (IVCOMP - 100) 20170, 10170, 20170                             07280413
30170 IVDELE = IVDELE + 1                                               07290413
      WRITE (I02,80000) IVTNUM                                          07300413
      IF (ICZERO) 10170, 0181, 20170                                    07310413
10170 IVPASS = IVPASS + 1                                               07320413
      WRITE (I02,80002) IVTNUM                                          07330413
      GO TO 0181                                                        07340413
20170 IVFAIL = IVFAIL + 1                                               07350413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07360413
 0181 CONTINUE                                                          07370413
C                                                                       07380413
C                                                                       07390413
C        TESTS 018 THROUGH 030 READ AND CHECK THE RECORDS CREATED IN    07400413
C     TESTS 002 THROUGH 014.  EACH OF THE TESTS IN THIS SET IS CHECKING 07410413
C     TWO THINGS.  FIRST, THAT THE READ STATEMENT CONSTRUCT IS ACCEPTED 07420413
C     BY THE COMPILER AND SECOND THAT THE RECORDS CREATED IN TESTS 002  07430413
C     THROUGH 013 AND READ IN THESE TESTS CAN GIVE PREDICTIBLE VALUES.  07440413
C     THE READ STATEMENT IS USED WITH THE I/O LIST ITEM  AS A VARIABLE, 07450413
C     AN ARRAY ELEMENT AND AN ARRAY.                                    07460413
C                                                                       07470413
C                                                                       07480413
C                                                                       07490413
C     ****  FCVS PROGRAM 413  -  TEST 018  ****                         07500413
C                                                                       07510413
C                                                                       07520413
C        TEST 018 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  07530413
C     VARIABLE OF INTEGER TYPE.                                         07540413
C                                                                       07550413
C                                                                       07560413
      IVTNUM =  18                                                      07570413
      IF (ICZERO) 30180, 0180, 30180                                    07580413
 0180 CONTINUE                                                          07590413
      IVON22 = 0                                                        07600413
      IVON56 = 0                                                        07610413
      IVCORR = 30                                                       07620413
      IVCOMP = 1                                                        07630413
      READ (I10, REC = 01) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     07640413
     1   IVON21, IVON22, IVON31, IVON32, IVON33, IVON34, IVON55, IVON56 07650413
      IF (IRECN .EQ. 01)     IVCOMP = IVCOMP * 2                        07660413
      IF (IVON22 .EQ. -11)   IVCOMP = IVCOMP * 3                        07670413
      IF (IVON56 .EQ. 32767) IVCOMP = IVCOMP * 5                        07680413
40180 IF (IVCOMP - 30)  20180, 10180, 20180                             07690413
30180 IVDELE = IVDELE + 1                                               07700413
      WRITE (I02,80000) IVTNUM                                          07710413
      IF (ICZERO) 10180, 0191, 20180                                    07720413
10180 IVPASS = IVPASS + 1                                               07730413
      WRITE (I02,80002) IVTNUM                                          07740413
      GO TO 0191                                                        07750413
20180 IVFAIL = IVFAIL + 1                                               07760413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07770413
 0191 CONTINUE                                                          07780413
C                                                                       07790413
C     ****  FCVS PROGRAM 413  -  TEST 019  ****                         07800413
C                                                                       07810413
C                                                                       07820413
C        TEST 019 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  07830413
C     VARIABLE OF REAL TYPE.                                            07840413
C                                                                       07850413
C                                                                       07860413
      IVTNUM =  19                                                      07870413
      IF (ICZERO) 30190, 0190, 30190                                    07880413
 0190 CONTINUE                                                          07890413
      RVON22 = 0.0                                                      07900413
      RVON31 = 0.0                                                      07910413
      IVCORR = 30                                                       07920413
      IVCOMP = 1                                                        07930413
      READ (I10, REC = 02) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     07940413
     1   RVON21, RVON22, RVON31, RVON32, RVON33, RVON34, RVON55, RVON56 07950413
      IF (IRECN .EQ. 02)      IVCOMP = IVCOMP * 2                       07960413
      IF (RVON22 .EQ. -11.)  IVCOMP = IVCOMP * 3                        07970413
      IF (RVON31 .EQ. 7.77)  IVCOMP = IVCOMP * 5                        07980413
40190 IF (IVCOMP - 30)  20190, 10190, 20190                             07990413
30190 IVDELE = IVDELE + 1                                               08000413
      WRITE (I02,80000) IVTNUM                                          08010413
      IF (ICZERO) 10190, 0201, 20190                                    08020413
10190 IVPASS = IVPASS + 1                                               08030413
      WRITE (I02,80002) IVTNUM                                          08040413
      GO TO 0201                                                        08050413
20190 IVFAIL = IVFAIL + 1                                               08060413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08070413
 0201 CONTINUE                                                          08080413
C                                                                       08090413
C     ****  FCVS PROGRAM 413  -  TEST 020  ****                         08100413
C                                                                       08110413
C                                                                       08120413
C        TEST 020 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  08130413
C     VARIABLE OF LOGICAL TYPE.                                         08140413
C                                                                       08150413
C                                                                       08160413
      IVTNUM =  20                                                      08170413
      IF (ICZERO) 30200, 0200, 30200                                    08180413
 0200 CONTINUE                                                          08190413
      LVONT1 = .FALSE.                                                  08200413
      LVONF6 = .TRUE.                                                   08210413
      IVCORR = 30                                                       08220413
      IVCOMP = 1                                                        08230413
      READ (I10, REC = 03) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     08240413
     1   LVONT1, LVONF2,  LVONT3, LVONF4, LVONT5, LVONF6, LVONT7, LVONF808250413
      IF (IRECN .EQ. 03)     IVCOMP = IVCOMP * 2                        08260413
      IF (.NOT. LVONF6)      IVCOMP = IVCOMP * 3                        08270413
      IF (LVONT1)            IVCOMP = IVCOMP * 5                        08280413
40200 IF (IVCOMP - 30)  20200, 10200, 20200                             08290413
30200 IVDELE = IVDELE + 1                                               08300413
      WRITE (I02,80000) IVTNUM                                          08310413
      IF (ICZERO) 10200, 0211, 20200                                    08320413
10200 IVPASS = IVPASS + 1                                               08330413
      WRITE (I02,80002) IVTNUM                                          08340413
      GO TO 0211                                                        08350413
20200 IVFAIL = IVFAIL + 1                                               08360413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08370413
 0211 CONTINUE                                                          08380413
C                                                                       08390413
C     ****  FCVS PROGRAM 413  -  TEST 021  ****                         08400413
C                                                                       08410413
C                                                                       08420413
C        TEST 021 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  08430413
C     ARRAY ELEMENT OF INTEGER TYPE.  ONE, TWO, AND THREE               08440413
C     DIMENSION ARRAYS ARE USED.                                        08450413
C                                                                       08460413
C                                                                       08470413
      IVTNUM =  21                                                      08480413
      IF (ICZERO) 30210, 0210, 30210                                    08490413
 0210 CONTINUE                                                          08500413
      IAON12(2) = 0                                                     08510413
      IAON12(8) = 0                                                     08520413
      IVCORR = 30                                                       08530413
      IVCOMP = 1                                                        08540413
      READ (I10, REC = 04) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     08550413
     1   IAON12(1), IAON12(2), IAON22(1,2), IAON22(2,2), IAON32(1,1,2), 08560413
     2   IAON32(2,1,2), IAON12(7), IAON12(8)                            08570413
      IF (IRECN .EQ. 04)   IVCOMP = IVCOMP * 2                          08580413
      IF (IAON12(2) .EQ. -11)   IVCOMP = IVCOMP * 3                     08590413
      IF (IAON12(8) .EQ. 32767) IVCOMP = IVCOMP * 5                     08600413
C                                                                       08610413
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 08620413
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           08630413
C                                                                       08640413
40210 IF (IVCOMP - 30)   20210, 10210, 20210                            08650413
30210 IVDELE = IVDELE + 1                                               08660413
      WRITE (I02,80000) IVTNUM                                          08670413
      IF (ICZERO) 10210, 0221, 20210                                    08680413
10210 IVPASS = IVPASS + 1                                               08690413
      WRITE (I02,80002) IVTNUM                                          08700413
      GO TO 0221                                                        08710413
20210 IVFAIL = IVFAIL + 1                                               08720413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08730413
 0221 CONTINUE                                                          08740413
C                                                                       08750413
C     ****  FCVS PROGRAM 413  -  TEST 022  ****                         08760413
C                                                                       08770413
C                                                                       08780413
C        TEST 022 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  08790413
C     ARRAY ELEMENT OF REAL TYPE.  ONE, TWO, AND THREE                  08800413
C     DIMENSION ARRAYS ARE USED.                                        08810413
C                                                                       08820413
C                                                                       08830413
      IVTNUM =  22                                                      08840413
      IF (ICZERO) 30220, 0220, 30220                                    08850413
 0220 CONTINUE                                                          08860413
      RAON22(2,2) = 0.0                                                 08870413
      RAON32(1,1,2) = 0.0                                               08880413
      IVCORR = 30                                                       08890413
      IVCOMP = 1                                                        08900413
      READ (I10, REC = 05) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     08910413
     1   RAON12(1), RAON12(2), RAON22(1,2), RAON22(2,2), RAON32(1,1,2), 08920413
     2   RAON32(2,1,2), RAON12(7), RAON12(8)                            08930413
      IF (IRECN .EQ. 05)             IVCOMP = IVCOMP * 2                08940413
      IF (RAON22(2,2) .EQ. -7.77)    IVCOMP = IVCOMP * 3                08950413
      IF (RAON32(1,1,2) .EQ.  .512 ) IVCOMP = IVCOMP * 5                08960413
C                                                                       08970413
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 08980413
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           08990413
C                                                                       09000413
40220 IF (IVCOMP - 30)   20220, 10220, 20220                            09010413
30220 IVDELE = IVDELE + 1                                               09020413
      WRITE (I02,80000) IVTNUM                                          09030413
      IF (ICZERO) 10220, 0231, 20220                                    09040413
10220 IVPASS = IVPASS + 1                                               09050413
      WRITE (I02,80002) IVTNUM                                          09060413
      GO TO 0231                                                        09070413
20220 IVFAIL = IVFAIL + 1                                               09080413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09090413
 0231 CONTINUE                                                          09100413
C                                                                       09110413
C     ****  FCVS PROGRAM 413  -  TEST 023  ****                         09120413
C                                                                       09130413
C                                                                       09140413
C        TEST 023 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  09150413
C     ARRAY ELEMENT OF LOGICAL TYPE.  ONE, TWO, AND THREE               09160413
C     DIMENSION ARRAYS ARE USED.                                        09170413
C                                                                       09180413
C                                                                       09190413
      IVTNUM =  23                                                      09200413
      IF (ICZERO) 30230, 0230, 30230                                    09210413
 0230 CONTINUE                                                          09220413
      LAON12(1) = .FALSE.                                               09230413
      LAON32(2,1,2) = .TRUE.                                            09240413
      IVCORR = 30                                                       09250413
      IVCOMP = 1                                                        09260413
      READ (I10, REC = 06) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     09270413
     1   LAON12(1), LAON12(2), LAON22(1,2), LAON22(2,2), LAON32(1,1,2), 09280413
     2   LAON32(2,1,2), LAON12(7), LAON12(8)                            09290413
      IF (IRECN .EQ. 06)   IVCOMP = IVCOMP * 2                          09300413
      IF (LAON12(1))          IVCOMP = IVCOMP * 3                       09310413
      IF (.NOT. LAON32(2,1,2))  IVCOMP = IVCOMP * 5                     09320413
40230 IF (IVCOMP - 30)   20230, 10230, 20230                            09330413
30230 IVDELE = IVDELE + 1                                               09340413
      WRITE (I02,80000) IVTNUM                                          09350413
      IF (ICZERO) 10230, 0241, 20230                                    09360413
10230 IVPASS = IVPASS + 1                                               09370413
      WRITE (I02,80002) IVTNUM                                          09380413
      GO TO 0241                                                        09390413
20230 IVFAIL = IVFAIL + 1                                               09400413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09410413
 0241 CONTINUE                                                          09420413
C                                                                       09430413
C     ****  FCVS PROGRAM 413  -  TEST 024  ****                         09440413
C                                                                       09450413
C                                                                       09460413
C        TEST 024 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  09470413
C     ARRAY OF INTEGER TYPE.                                            09480413
C                                                                       09490413
C                                                                       09500413
      IVTNUM =  24                                                      09510413
      IF (ICZERO) 30240, 0240, 30240                                    09520413
 0240 CONTINUE                                                          09530413
      IAON32(2,1,1) = 0                                                 09540413
      IAON32(2,2,2) = 0                                                 09550413
      IVCORR = 30                                                       09560413
      IVCOMP = 1                                                        09570413
      READ (I10, REC = 07) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     09580413
     1   IAON32                                                         09590413
      IF (IRECN .EQ. 07)   IVCOMP = IVCOMP * 2                          09600413
      IF (IAON32(2,1,1) .EQ. -11)    IVCOMP = IVCOMP * 3                09610413
      IF (IAON32(2,2,2) .EQ. 32767)  IVCOMP = IVCOMP * 5                09620413
C                                                                       09630413
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 09640413
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           09650413
C                                                                       09660413
40240 IF (IVCOMP - 30)   20240, 10240, 20240                            09670413
30240 IVDELE = IVDELE + 1                                               09680413
      WRITE (I02,80000) IVTNUM                                          09690413
      IF (ICZERO) 10240, 0251, 20240                                    09700413
10240 IVPASS = IVPASS + 1                                               09710413
      WRITE (I02,80002) IVTNUM                                          09720413
      GO TO 0251                                                        09730413
20240 IVFAIL = IVFAIL + 1                                               09740413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09750413
 0251 CONTINUE                                                          09760413
C                                                                       09770413
C     ****  FCVS PROGRAM 413  -  TEST 025  ****                         09780413
C                                                                       09790413
C                                                                       09800413
C        TEST 025 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  09810413
C     ARRAY OF REAL TYPE.                                               09820413
C                                                                       09830413
C                                                                       09840413
      IVTNUM =  25                                                      09850413
      IF (ICZERO) 30250, 0250, 30250                                    09860413
 0250 CONTINUE                                                          09870413
      RAON32(2,1,1) = 0.0                                               09880413
      RAON32(2,2,2) = 0.0                                               09890413
      IVCORR = 30                                                       09900413
      IVCOMP = 1                                                        09910413
      READ (I10, REC = 08) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     09920413
     1   RAON32                                                         09930413
      IF (IRECN .EQ. 08)   IVCOMP = IVCOMP * 2                          09940413
      IF (RAON32(2,1,1) .EQ. -11.)   IVCOMP = IVCOMP * 3                09950413
      IF (RAON32(2,2,2) .EQ.  32767.) IVCOMP = IVCOMP * 5               09960413
C                                                                       09970413
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 09980413
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           09990413
C                                                                       10000413
40250 IF (IVCOMP - 30)   20250, 10250, 20250                            10010413
30250 IVDELE = IVDELE + 1                                               10020413
      WRITE (I02,80000) IVTNUM                                          10030413
      IF (ICZERO) 10250, 0261, 20250                                    10040413
10250 IVPASS = IVPASS + 1                                               10050413
      WRITE (I02,80002) IVTNUM                                          10060413
      GO TO 0261                                                        10070413
20250 IVFAIL = IVFAIL + 1                                               10080413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10090413
 0261 CONTINUE                                                          10100413
C                                                                       10110413
C     ****  FCVS PROGRAM 413  -  TEST 026  ****                         10120413
C                                                                       10130413
C                                                                       10140413
C        TEST 026 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  10150413
C     ARRAY OF LOGICAL TYPE.                                            10160413
C                                                                       10170413
C                                                                       10180413
      IVTNUM =  26                                                      10190413
      IF (ICZERO) 30260, 0260, 30260                                    10200413
 0260 CONTINUE                                                          10210413
      LAON32(1,1,1) = .FALSE.                                           10220413
      LAON32(2,2,2) = .TRUE.                                            10230413
      IVCORR = 30                                                       10240413
      IVCOMP = 1                                                        10250413
      READ (I10, REC = 09) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     10260413
     1   LAON32                                                         10270413
      IF (IRECN .EQ. 09)   IVCOMP = IVCOMP * 2                          10280413
      IF (LAON32(1,1,1))     IVCOMP = IVCOMP * 3                        10290413
      IF (.NOT. LAON32(2,2,2))    IVCOMP = IVCOMP * 5                   10300413
40260 IF (IVCOMP - 30)   20260, 10260, 20260                            10310413
30260 IVDELE = IVDELE + 1                                               10320413
      WRITE (I02,80000) IVTNUM                                          10330413
      IF (ICZERO) 10260, 0271, 20260                                    10340413
10260 IVPASS = IVPASS + 1                                               10350413
      WRITE (I02,80002) IVTNUM                                          10360413
      GO TO 0271                                                        10370413
20260 IVFAIL = IVFAIL + 1                                               10380413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10390413
 0271 CONTINUE                                                          10400413
C                                                                       10410413
C     ****  FCVS PROGRAM 413  -  TEST 027  ****                         10420413
C                                                                       10430413
C                                                                       10440413
C        TEST 027 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  10450413
C     IMPLIED-DO WITH AN ITEM OF INTEGER TYPE.  THE STORAGE VALUES IN   10460413
C     THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  10470413
C     DIFFERENT STORAGE SEQUENCE IN THE ARRAY THAN FOUND IN THE RECORD  10480413
C     OF THE FILE.  THIS RECORD IS RECORD NUMBER 10 AND WAS CREATED IN  10490413
C     TEST 012 ABOVE.  THE FIELD VALUE, FIELD POSITION, POSITION WITHIN 10500413
C     ARRAY IAON32 AND SUBSCRIPT VALUE AFTER THE READ IS                10510413
C                                                                       10520413
C      VALUE      11    777     512  -32767   -11   -777   -512   32767 10530413
C      FIELD POS   1      3      2      4      5      7      6      8   10540413
C      IAON32      1      2      3      4      5      6      7      8   10550413
C      SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,210560413
C                                                                       10570413
C                                                                       10580413
      IVTNUM =  27                                                      10590413
      IF (ICZERO) 30270, 0270, 30270                                    10600413
 0270 CONTINUE                                                          10610413
      IAON32(2,1,1) = 0                                                 10620413
      IAON32(2,2,1) = 0                                                 10630413
      IVCORR = 30                                                       10640413
      IVCOMP = 1                                                        10650413
      READ (I10, REC = 10) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     10660413
     1   (((IAON32 (J,K,I), K=1,2), J=1,2), I=1,2)                      10670413
      IF (IRECN .EQ. 10)   IVCOMP = IVCOMP * 2                          10680413
      IF (IAON32(2,1,1) .EQ. 777)      IVCOMP = IVCOMP * 3              10690413
      IF (IAON32(2,2,1) .EQ. -32767)   IVCOMP = IVCOMP * 5              10700413
C                                                                       10710413
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 10720413
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           10730413
C                                                                       10740413
40270 IF (IVCOMP - 30)   20270, 10270, 20270                            10750413
30270 IVDELE = IVDELE + 1                                               10760413
      WRITE (I02,80000) IVTNUM                                          10770413
      IF (ICZERO) 10270, 0281, 20270                                    10780413
10270 IVPASS = IVPASS + 1                                               10790413
      WRITE (I02,80002) IVTNUM                                          10800413
      GO TO 0281                                                        10810413
20270 IVFAIL = IVFAIL + 1                                               10820413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10830413
 0281 CONTINUE                                                          10840413
C                                                                       10850413
C     ****  FCVS PROGRAM 413  -  TEST 028  ****                         10860413
C                                                                       10870413
C                                                                       10880413
C        TEST 028 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  10890413
C     IMPLIED-DO WITH AN ITEM OF  REAL   TYPE.  THE STORAGE VALUES IN   10900413
C     THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  10910413
C     SEQUENCE THE SAME AS FOUND IN THE RECORD OF THE FILE.  THIS REC-  10920413
C     ORD IS RECORD NUMBER 011 AND WAS CREATED IN TEST 013 ABOVE.       10930413
C     THE FIELD VALUE, FIELD POSITION, POSITION WITHIN ARRAY RAON32  AND10940413
C     SUBSCRIPT VALUE AFTER THE THE READ IS                             10950413
C                                                                       10960413
C      VALUE      11.   -11.   7.77   -7.77  .512   -.512 -32767. 32767.10970413
C      FIELD POS   1      2      3      4      5      6      7      8   10980413
C      RAON32      1      2      3      4      5      6      7      8   10990413
C      SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,211000413
C                                                                       11010413
C                                                                       11020413
      IVTNUM =  28                                                      11030413
      IF (ICZERO) 30280, 0280, 30280                                    11040413
 0280 CONTINUE                                                          11050413
      RAON32(1,2,1) = 0.0                                               11060413
      RAON32(1,2,2) = 0.0                                               11070413
      IVCORR = 30                                                       11080413
      IVCOMP = 1                                                        11090413
      READ (I10, REC = 11) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     11100413
     1   (((RAON32 (J,K,I), J=1,2), K=1,2), I=1,2)                      11110413
      IF (IRECN .EQ. 11)   IVCOMP = IVCOMP * 2                          11120413
      IF (RAON32(1,2,1) .EQ. 7.77)     IVCOMP = IVCOMP * 3              11130413
      IF (RAON32(1,2,2) .EQ. -32767.)  IVCOMP = IVCOMP * 5              11140413
C                                                                       11150413
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 11160413
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           11170413
C                                                                       11180413
40280 IF (IVCOMP - 30)   20280, 10280, 20280                            11190413
30280 IVDELE = IVDELE + 1                                               11200413
      WRITE (I02,80000) IVTNUM                                          11210413
      IF (ICZERO) 10280, 0291, 20280                                    11220413
10280 IVPASS = IVPASS + 1                                               11230413
      WRITE (I02,80002) IVTNUM                                          11240413
      GO TO 0291                                                        11250413
20280 IVFAIL = IVFAIL + 1                                               11260413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11270413
 0291 CONTINUE                                                          11280413
C                                                                       11290413
C     ****  FCVS PROGRAM 413  -  TEST 029  ****                         11300413
C                                                                       11310413
C                                                                       11320413
C        TEST 029 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  11330413
C     IMPLIED-DO WITH AN ITEM OF LOGICAL TYPE.  THE STORAGE VALUES IN   11340413
C     THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  11350413
C     DIFFERENT STORAGE SEQUENCE IN THE ARRAY THAN FOUND IN THE RECORD  11360413
C     OF THE FILE.  THIS RECORD IS RECORD NUMBER 12 AND WAS CREATED IN  11370413
C     TEST 014 ABOVE.  THE FIELD VALUE, FIELD POSITION, POSITION WITHIN 11380413
C     ARRAY LAON32 AND SUBSCRIPT VALUE AFTER THE READ IS                11390413
C                                                                       11400413
C      VALUE       T      T      F      F      T       T     F      F   11410413
C      FIELD POS   1      5      3      7       2      6     4      8   11420413
C      LAON32      1      2      3      4      5      6      7      8   11430413
C      SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,211440413
C                                                                       11450413
C                                                                       11460413
      IVTNUM =  29                                                      11470413
      IF (ICZERO) 30290, 0290, 30290                                    11480413
 0290 CONTINUE                                                          11490413
      LAON32(1,2,1) = .TRUE.                                            11500413
      LAON32(2,1,1) = .FALSE.                                           11510413
      IVCORR = 30                                                       11520413
      IVCOMP = 1                                                        11530413
      READ (I10, REC = 12) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     11540413
     1   (((LAON32 (J,K,I), I=1,2), K=1,2), J=1,2)                      11550413
      IF (IRECN .EQ. 12)   IVCOMP = IVCOMP * 2                          11560413
      IF ( .NOT. LAON32(1,2,1))   IVCOMP = IVCOMP * 3                   11570413
      IF (LAON32(2,1,1))          IVCOMP = IVCOMP * 5                   11580413
40290 IF (IVCOMP - 30)   20290, 10290, 20290                            11590413
30290 IVDELE = IVDELE + 1                                               11600413
      WRITE (I02,80000) IVTNUM                                          11610413
      IF (ICZERO) 10290, 0301, 20290                                    11620413
10290 IVPASS = IVPASS + 1                                               11630413
      WRITE (I02,80002) IVTNUM                                          11640413
      GO TO 0301                                                        11650413
20290 IVFAIL = IVFAIL + 1                                               11660413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11670413
 0301 CONTINUE                                                          11680413
C                                                                       11690413
C     ****  FCVS PROGRAM 413  -  TEST 030  ****                         11700413
C                                                                       11710413
C                                                                       11720413
C        TEST 030 USES A READ STATEMENT WITHOUT ANY INPUT LIST ITEMS    11730413
C     (INPUT LIST ITEMS ARE OPTIONAL FOR THE READ STATEMENT). THIS      11740413
C     RECORD WAS WRITTEN IN TEST 14 AND SHOULD BE RECORD NUMBER 13.     11750413
C     THE PURPOSE OF THIS TEST IS TO SEE THAT THE STATEMENT CONSTRUCT   11760413
C     IS ACCEPTABLE TO THE COMPILER.                                    11770413
C     ALSO THE LENGTH OF AN UNFORMATTED RECORD MAY BE ZERO.             11780413
C                                                                       11790413
C                  SEE SECTIONS 12.1.2,   UNFORMATTED RECORDS           11800413
C                               12.8,   READ, WRITE AND PRINT STATEMENTS11810413
C                                                                       11820413
C                                                                       11830413
      IVTNUM =  30                                                      11840413
      IF (ICZERO) 30300, 0300, 30300                                    11850413
 0300 CONTINUE                                                          11860413
      IRECN = 13                                                        11870413
      IVCORR = 13                                                       11880413
      READ (I10, REC = 13)                                              11890413
      IVCOMP = IRECN                                                    11900413
40300 IF (IVCOMP - 13)   20300, 10300, 20300                            11910413
30300 IVDELE = IVDELE + 1                                               11920413
      WRITE (I02,80000) IVTNUM                                          11930413
      IF (ICZERO) 10300, 0311, 20300                                    11940413
10300 IVPASS = IVPASS + 1                                               11950413
      WRITE (I02,80002) IVTNUM                                          11960413
      GO TO 0311                                                        11970413
20300 IVFAIL = IVFAIL + 1                                               11980413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11990413
 0311 CONTINUE                                                          12000413
C                                                                       12010413
C     ****  FCVS PROGRAM 413  -  TEST 031  ****                         12020413
C                                                                       12030413
C                                                                       12040413
C        TEST 031 USES A READ STATEMENT IN WHICH THE NUMBER OF VALUES   12050413
C     REQUIRED BY THE INPUT LIST IS LESS THAN THE NUMBER OF VALUES IN   12060413
C     THE RECORD.                                                       12070413
C                                                                       12080413
C                  SEE SECTION 12.9.5.1, UNFORMATED DATA TRANSFER       12090413
C                                                                       12100413
C                                                                       12110413
      IVTNUM =  31                                                      12120413
      IF (ICZERO) 30310, 0310, 30310                                    12130413
 0310 CONTINUE                                                          12140413
      IVON21 = 0                                                        12150413
      IVON22 = 0                                                        12160413
      IVON31 = 0                                                        12170413
      IVCORR = 0                                                        12180413
      IVCOMP = 1                                                        12190413
      READ (I10, REC = 01) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     12200413
     1       IVON21, IVON22, IVON31                                     12210413
      IF (IRECN .EQ. 01)  IVCOMP = IVCOMP * 2                           12220413
      IF (IVON21 .EQ. 11)  IVCOMP = IVCOMP * 3                          12230413
      IF (IVON22 .EQ. -11) IVCOMP = IVCOMP * 5                          12240413
40310 IF (IVCOMP - 30) 20310, 10310, 20310                              12250413
30310 IVDELE = IVDELE + 1                                               12260413
      WRITE (I02,80000) IVTNUM                                          12270413
      IF (ICZERO) 10310, 0321, 20310                                    12280413
10310 IVPASS = IVPASS + 1                                               12290413
      WRITE (I02,80002) IVTNUM                                          12300413
      GO TO 0321                                                        12310413
20310 IVFAIL = IVFAIL + 1                                               12320413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          12330413
 0321 CONTINUE                                                          12340413
C                                                                       12350413
C                                                                       12360413
C        TEST 032 AND 033 VERIFIES THAT RECORDS MAY BE READ IN ANY ORDER12370413
C     ALSO THAT A VARIABLE MAY BE USED AS THE OPERAND OF THE REC SPEC-  12380413
C     IFIER FOR A READ STATEMENT.                                       12390413
C                                                                       12400413
C              SEE SECTION 2.2.4.2(1) , DIRECT ACCESS                   12410413
C                                                                       12420413
C                                                                       12430413
C                                                                       12440413
C     ****  FCVS PROGRAM 413  -  TEST 032  ****                         12450413
C                                                                       12460413
C                                                                       12470413
C        TEST 032 READS THE RECORDS WRITTEN IN TEST 16.  EVERY OTHER    12480413
C     RECORD IS READ FOR A TOTAL OF 100 RECORDS (THE REC SPECIFIER      12490413
C     VARIABLE IS INCREMENTED BY 2).                                    12500413
C                                                                       12510413
C                                                                       12520413
      IVTNUM =  32                                                      12530413
      IF (ICZERO) 30320, 0320, 30320                                    12540413
 0320 CONTINUE                                                          12550413
      IRECCK = 13                                                       12560413
      IRECN = 0                                                         12570413
      IREC = 13                                                         12580413
      IVCOMP = 0                                                        12590413
      DO 4134 I = 1,100                                                 12600413
      IREC = IREC + 2                                                   12610413
      IRECCK = IRECCK + 2                                               12620413
      READ (I10, REC = IREC) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,   12630413
     1   IVON21, IVON22, IVON31, IVON32, IVON33, IVON34, IVON55, IVON56 12640413
      IF (IRECN .EQ. IRECCK)    IVCOMP = IVCOMP + 1                     12650413
 4134 CONTINUE                                                          12660413
      IVCORR = 100                                                      12670413
40320 IF (IVCOMP - 100)  20320, 10320, 20320                            12680413
30320 IVDELE = IVDELE + 1                                               12690413
      WRITE (I02,80000) IVTNUM                                          12700413
      IF (ICZERO) 10320, 0331, 20320                                    12710413
10320 IVPASS = IVPASS + 1                                               12720413
      WRITE (I02,80002) IVTNUM                                          12730413
      GO TO 0331                                                        12740413
20320 IVFAIL = IVFAIL + 1                                               12750413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          12760413
 0331 CONTINUE                                                          12770413
C                                                                       12780413
C     ****  FCVS PROGRAM 413  -  TEST 033  ****                         12790413
C                                                                       12800413
C                                                                       12810413
C        TEST 033 READS THE RECORDS WRITTEN IN TEST 17.  THIS TEST IS   12820413
C     SIMILAR  TO TEST 32 ABOVE EXCEPT THE FILE IS READ IN REVERSE      12830413
C     RECORD NUMBER ORDER.                                              12840413
C                                                                       12850413
C                                                                       12860413
      IVTNUM =  33                                                      12870413
      IF (ICZERO) 30330, 0330, 30330                                    12880413
 0330 CONTINUE                                                          12890413
      IRECCK = 216                                                      12900413
      IRECN = 0                                                         12910413
      IVCOMP = 0                                                        12920413
      IREC = 216                                                        12930413
      DO 4135 I = 1,100                                                 12940413
      IREC = IREC - 2                                                   12950413
      IRECCK = IRECCK - 2                                               12960413
      READ (I10, REC = IREC)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,  12970413
     1   IVON21, IVON22, IVON31, IVON32, IVON33, IVON34, IVON55, IVON56 12980413
      IF (IRECN .EQ. IRECCK)           IVCOMP = IVCOMP + 1              12990413
 4135 CONTINUE                                                          13000413
      IVCORR = 100                                                      13010413
40330 IF (IVCOMP - 100)      20330, 10330, 20330                        13020413
30330 IVDELE = IVDELE + 1                                               13030413
      WRITE (I02,80000) IVTNUM                                          13040413
      IF (ICZERO) 10330, 0341, 20330                                    13050413
10330 IVPASS = IVPASS + 1                                               13060413
      WRITE (I02,80002) IVTNUM                                          13070413
      GO TO 0341                                                        13080413
20330 IVFAIL = IVFAIL + 1                                               13090413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          13100413
 0341 CONTINUE                                                          13110413
C                                                                       13120413
C     ****  FCVS PROGRAM 413  -  TEST 034  ****                         13130413
C                                                                       13140413
C                                                                       13150413
C        TEST 034 VERIFIES THAT THE VALUES OF A RECORD MAY BE CHANGED   13160413
C     WHEN THE RECORD IS REWRITTEN.  RECORD NUMBER 01 IS USED FOR       13170413
C     TESTING.  THE RECORD WAS WRITTEN IN TEST 02 AND READ IN TEST 18.  13180413
C     A RECORD CANNOT BE DELETED FROM THE FILE BUT IT CAN BE REWRITTEN. 13190413
C                                                                       13200413
C                  SEE SECTION  12.2.4.2 (5), DIRECT ACCESS             13210413
C                                                                       13220413
C                                                                       13230413
      IVTNUM =  34                                                      13240413
      IF (ICZERO) 30340, 0340, 30340                                    13250413
 0340 CONTINUE                                                          13260413
      IRECN = 01                                                        13270413
      WRITE (I10, REC = 01)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,   13280413
     1    ICON31, ICON32, ICON21, ICON22, ICON55, ICON56, ICON33, ICON3413290413
      READ (I10, REC=01)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,      13300413
     1    IVON61, IVON62, IVON63, IVON64, IVON65,IVON66, IVON67, IVON68 13310413
      IVCORR = 210                                                      13320413
      IVCOMP = 1                                                        13330413
      IF (IRECN .EQ. 01)             IVCOMP = IVCOMP * 2                13340413
      IF (IVON61 .EQ. 777)           IVCOMP = IVCOMP  * 3               13350413
      IF (IVON62 .EQ. -777)          IVCOMP = IVCOMP * 5                13360413
      IF (IVON66 .EQ. 32767)         IVCOMP = IVCOMP * 7                13370413
40340 IF (IVCOMP - 210)  20340, 10340, 20340                            13380413
30340 IVDELE = IVDELE + 1                                               13390413
      WRITE (I02,80000) IVTNUM                                          13400413
      IF (ICZERO) 10340, 0351, 20340                                    13410413
10340 IVPASS = IVPASS + 1                                               13420413
      WRITE (I02,80002) IVTNUM                                          13430413
      GO TO 0351                                                        13440413
20340 IVFAIL = IVFAIL + 1                                               13450413
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          13460413
 0351 CONTINUE                                                          13470413
C                                                                       13480413
C                                                                       13490413
C        THE FOLLOWING SOURCE CODE BRACKETED BY THE COMMENT LINES       13500413
C     *****  BEGIN-FILE-DUMP SECTION AND *****  END-FILE-DUMP SECTION   13510413
C     MAY OR MAY NOT  APPEAR AS COMMENTS IN THE SOURCE PROGRAM.         13520413
C     THIS CODE IS OPTIONAL AND BY DEFAULT IT IS AUTOMATICALLY COMMENTED13530413
C     OUT BY THE EXECUTIVE ROUTINE.  A DUMP OF THE FILE USED BY THIS    13540413
C     ROUTINE IS PROVIDED BY USING THE *OPT1 EXECUTIVE ROUTINE CONTROL  13550413
C     CARD.  IF THE OPTIONAL CODE IS SELECTED THE ROUTINE WILL DUMP     13560413
C     THE CONTENTS OF THE FILE TO THE PRINT FILE FOLLOWING THE TEST     13570413
C     REPORT AND BEFORE THE TEST REPORT SUMMARY.                        13580413
C                                                                       13590413
CDB**  BEGIN FILE DUMP CODE                                             13600413
C     ITOTR = 214                                                       13610413
C     ILUN  = I10                                                       13620413
C     IRLGN = 80                                                        13630413
C     IRNUM = 1                                                         13640413
C7701 FORMAT (80A1)                                                     13650413
C7702 FORMAT (1X,80A1)                                                  13660413
C7703 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,13H RECORDS - OK)               13670413
C7704 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,27H RECORDS - THERE SHOULD BE ,I13680413
C    13,9H RECORDS.)                                                    13690413
C     DO 7771 IRNUM = 1, ITOTR                                          13700413
C     READ (ILUN, REC = IRNUM) (IDUMP(ICH), ICH = 1, IRLGN)             13710413
C     WRITE (I02,  7702) (IDUMP(ICH), ICH = 1, IRLGN)                   13720413
C7771 CONTINUE                                                          13730413
CDE**      END OF DUMP CODE                                             13740413
C        TEST  034 IS THE LAST TEST IN THIS PROGRAM.  THE ROUTINE SHOULD13750413
C     HAVE MADE 34 EXPLICIT TESTS AND PROCESSED ONE FILE CONNECTED  FOR 13760413
C     DIRECT ACCESS                                                     13770413
C                                                                       13780413
C                                                                       13790413
C                                                                       13800413
C     WRITE OUT TEST SUMMARY                                            13810413
C                                                                       13820413
      WRITE (I02,90004)                                                 13830413
      WRITE (I02,90014)                                                 13840413
      WRITE (I02,90004)                                                 13850413
      WRITE (I02,90000)                                                 13860413
      WRITE (I02,90004)                                                 13870413
      WRITE (I02,90020) IVFAIL                                          13880413
      WRITE (I02,90022) IVPASS                                          13890413
      WRITE (I02,90024) IVDELE                                          13900413
      STOP                                                              13910413
90001 FORMAT (1H ,24X,5HFM413)                                          13920413
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM413)                          13930413
C                                                                       13940413
C     FORMATS FOR TEST DETAIL LINES                                     13950413
C                                                                       13960413
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   13970413
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      13980413
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         13990413
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    14000413
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        14010413
C                                                                       14020413
C     FORMAT STATEMENTS FOR PAGE HEADERS                                14030413
C                                                                       14040413
90002 FORMAT (1H1)                                                      14050413
90004 FORMAT (1H )                                                      14060413
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            14070413
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   14080413
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         14090413
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  14100413
90014 FORMAT (1H ,5X,46H----------------------------------------------) 14110413
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             14120413
C                                                                       14130413
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 14140413
C                                                                       14150413
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              14160413
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              14170413
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             14180413
      END                                                               14190413
