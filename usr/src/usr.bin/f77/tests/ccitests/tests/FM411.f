      PROGRAM FM411                                                     00010411
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020411
C                                                                       00030411
C                                                                       00040411
C        THIS ROUTINE TESTS FOR PROPER PROCESSING OF UNFORMATTED RECORDS00050411
C     WITH A FILE  CONNECTED FOR SEQUENTIAL ACCESS.  UNFORMATTED RECORDS00060411
C     MAY BE READ OR WRITTEN ONLY BY UNFORMATTED INPUT/OUTPUT STATE-    00070411
C     MENTS.  THIS ROUTINE TESTS SEVERAL SYNTACTICAL VARIATIONS OF THE  00080411
C     UNFORMATTED READ AND WRITE STATEMENTS AS WELL AS THE FILE         00090411
C     POSITIONING STATEMENTS BACKSPACE, ENDFILE AND REWIND.   IN        00100411
C     ADDITION UNFORMATTED RECORDS MAY HAVE BOTH CHARACTER AND          00110411
C     NONCHARACTER DATA.  THIS DATA IS TRANSFERRED WITHOUT EDITING      00120411
C     BETWEEN THE CURRENT RECORD AND ENTITIES SPECIFIED BY THE INPUT/   00130411
C     OUTPUT LIST ITEMS.  THIS ROUTINE BOTH READS AND WRITES            00140411
C     RECORDS CONTAINING DATA OF LOGICAL, REAL AND INTEGER TYPE WITH    00150411
C     I/O LIST ITEMS REPRESENTED AS VARIABLE NAMES, ARRAY ELEMENT       00160411
C     NAMES AND ARRAY NAMES.   THIS ROUTINE DOES NOT TEST DATA OF TYPE  00170411
C     CHARACTER.                                                        00180411
C        ROUTINE FM413 TESTS USE OF UNFORMATTED RECORDS WITH A FILE     00190411
C     CONNECTED FOR DIRECT ACCESS.                                      00200411
C                                                                       00210411
C        THIS ROUTINE TESTS                                             00220411
C                                                                       00230411
C             (1) THE STATEMENT CONSTRUCTS                              00240411
C                                                                       00250411
C                 A. WRITE (U)         VARIABLE-NAME,...                00260411
C                 B. WRITE (U)         ARRAY-ELEMENT-NAME,...           00270411
C                 C. WRITE (U)         ARRAY-NAME,...                   00280411
C                 D. WRITE (U)                   -  NO OUTPUT LIST      00290411
C                 E. WRITE (U)         IMPLIED-DO-LIST                  00300411
C                 F. READ (U)          VARIABLE-NAME,...                00310411
C                 G. READ (U)          ARRAY-ELEMENT-NAME,...           00320411
C                 H. READ (U)          ARRAY-NAME,...                   00330411
C                 I. READ (U,END=S)              -  NO INPUT LIST       00340411
C                 J. READ (U,END=S)    VARIABLE-NAME                    00350411
C                 K. READ (U)          IMPLIED-DO-LIST                  00360411
C                                                                       00370411
C             (2) USE OF A READ STATEMENT WHERE THE NUMBER OF VALUES    00380411
C                 IN THE INPUT LIST IS LESS THAN OR EQUAL TO THE        00390411
C                 NUMBER OF VALUES IN THE RECORD.                       00400411
C                                                                       00410411
C             (3) USE OF THE BACKSPACE, REWIND AND ENDFILE STATEMENT    00420411
C                 ON A FILE CONTAINING UNFORMATTED RECORDS.             00430411
C                                                                       00440411
C             (4) USE OF A REWIND STATEMENT ON A FILE THAT IS CONNECTED 00450411
C                 BUT DOES NOT EXIST.                                   00460411
C                                                                       00470411
C             (5) USE OF AN ENDFILE STATEMENT TO CREATE A FILE THAT     00480411
C                 DOES NOT EXIST                                        00490411
C                                                                       00500411
C     REFERENCES -                                                      00510411
C                                                                       00520411
C           AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,    00530411
C           X3.9-1977                                                   00540411
C                                                                       00550411
C             SECTION 4.1,        DATA TYPES                            00560411
C             SECTION 12.1.2,     UNFORMATTED RECORDS                   00570411
C             SECTION 12.2.1,     FILE EXISTENCE                        00580411
C             SECTION 12.2.4,     FILE ACCESS                           00590411
C             SECTION 12.2.4.1,   SEQUENTIAL ACCESS                     00600411
C             SECTION 12.3.3,     UNIT SPECIFIER AND IDENTIFIER         00610411
C             SECTION 12.7.2,     END-OF-FILE SPECIFIER                 00620411
C             SECTION 12.8,       READ, WRITE AND PRINT STATEMENTS      00630411
C             SECTION 12.8.1,     CONTROL INFORMATION LIST              00640411
C             SECTION 12.8.2,     INPUT/OUTPUT LIST                     00650411
C             SECTION 12.8.2.1,   INPUT LIST ITEMS                      00660411
C             SECTION 12.8.2.2,   OUTPUT LIST ITEMS                     00670411
C             SECTION 12.8.2.3,   IMPLIED-DO  LIST                      00680411
C             SECTION 12.9.5.1,   UNFORMATTED DATA TRANSFER             00690411
C             SECTION 12.10.4,    FILE POSITIONING STATEMENTS           00700411
C             SECTION 12.10.4.1   BACKSPACE STATEMENT                   00710411
C             SECTION 12.10.4.2,  ENDFILE STATEMENT                     00720411
C             SECTION 12.10.4.3,  REWIND STATEMENT                      00730411
C                                                                       00740411
C                                                                       00750411
C                                                                       00760411
C     ******************************************************************00770411
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00780411
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00790411
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00800411
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00810411
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00820411
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00830411
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00840411
C     THE RESULT OF EXECUTING THESE TESTS.                              00850411
C                                                                       00860411
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00870411
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00880411
C                                                                       00890411
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00900411
C                    DEPARTMENT OF THE NAVY                             00910411
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00920411
C                    WASHINGTON, D.C.   20376                           00930411
C                                                                       00940411
C     ******************************************************************00950411
C                                                                       00960411
C                                                                       00970411
      IMPLICIT LOGICAL (L)                                              00980411
      IMPLICIT CHARACTER*14 (C)                                         00990411
C                                                                       01000411
      LOGICAL  LAON11, LAON21, LAON31, LCONT1, LCONF2, LVONT1, LVONF2   01010411
      LOGICAL  LAON12, LAON22, LAON32, LCONT3, LCONF4, LVONT3, LVONF4   01020411
      LOGICAL  LCONT5, LCONF6, LCONT7, LCONF8, LVONT5, LVONF6, LVONT7   01030411
      LOGICAL LVONF8                                                    01040411
      DIMENSION IDUMP(80)                                               01050411
      DIMENSION IAON11(8), IAON21(2,4), IAON31(2,2,2)                   01060411
      DIMENSION IAON12(8), IAON22(2,4), IAON32(2,2,2)                   01070411
      DIMENSION RAON11(8), RAON21(2,4), RAON31(2,2,2)                   01080411
      DIMENSION RAON12(8), RAON22(2,4), RAON32(2,2,2)                   01090411
      DIMENSION LAON11(8), LAON21(2,4), LAON31(2,2,2)                   01100411
      DIMENSION LAON12(8), LAON22(2,4), LAON32(2,2,2)                   01110411
      DATA  IAON11 /11, -11, 777, -777, 512, -512, -32767, 32767/       01120411
      DATA  IAON21 /11, -11, 777, -777, 512, -512, -32767, 32767/       01130411
      DATA  IAON31 /11, -11, 777, -777, 512, -512, -32767, 32767/       01140411
      DATA  LAON11 /.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE.,  01150411
     1              .TRUE., .FALSE./                                    01160411
      DATA  LAON21 /.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE.,  01170411
     1              .TRUE., .FALSE./                                    01180411
      DATA  LAON31 /.TRUE., .FALSE., .TRUE., .FALSE., .TRUE., .FALSE.,  01190411
     1              .TRUE., .FALSE./                                    01200411
      DATA  RAON11 /11., -11., 7.77, -7.77,.512, -.512, -32767., 32767./01210411
      DATA  RAON21 /11., -11., 7.77, -7.77,.512, -.512, -32767., 32767./01220411
      DATA  RAON31 /11., -11., 7.77, -7.77,.512, -.512, -32767., 32767./01230411
      ICON21 = 11                                                       01240411
      ICON22 = -11                                                      01250411
      ICON31 = +777                                                     01260411
      ICON32 = -777                                                     01270411
      ICON33 =  512                                                     01280411
      ICON34 = -512                                                     01290411
      ICON55 = -32767                                                   01300411
      ICON56 =  32767                                                   01310411
      RCON21 = 11.                                                      01320411
      RCON22 = -11.                                                     01330411
      RCON31 = +7.77                                                    01340411
      RCON32 = -7.77                                                    01350411
      RCON33 = .512                                                     01360411
      RCON34 = -.512                                                    01370411
      RCON55 = -32767.                                                  01380411
      RCON56 =  32767.                                                  01390411
      LCONT1 = .TRUE.                                                   01400411
      LCONF2 = .FALSE.                                                  01410411
      LCONT3 = .TRUE.                                                   01420411
      LCONF4 = .FALSE.                                                  01430411
      LCONT5 = .TRUE.                                                   01440411
      LCONF6 = .FALSE.                                                  01450411
      LCONT7 = .TRUE.                                                   01460411
      LCONF8 = .FALSE.                                                  01470411
C                                                                       01480411
C          THE FILE USED IN THIS ROUTINE HAS THE FOLLOWING PROPERTIES   01490411
C                                                                       01500411
C                  FILE IDENTIFIER     - I04 (X-NUMBER 04)              01510411
C                  RECORD SIZE         - 80                             01520411
C                  ACCESS METHOD       - SEQUENTIAL                     01530411
C                  RECORD TYPE         - UNFORMATTED                    01540411
C                  DESIGNATED DEVICE   - DISK                           01550411
C                  TYPE OF DATA        - INTEGER, REAL AND LOGICAL      01560411
C                  RECORDS IN FILE     - 142 PLUS ENDFILE RECORD        01570411
C                                                                       01580411
C          THE FIRST 6 FIELDS OF EACH RECORD IN THE FILE UNIQUELY IDENT-01590411
C     IFIES THAT RECORD.  THE REMAINING FIELDS OF THE RECORD CONTAIN    01600411
C     DATA WHICH ARE USED IN TESTING.  A DESCRIPTION OF EACH FIELD      01610411
C     OF THE  PREAMBLE FOLLOWS.                                         01620411
C                                                                       01630411
C                  VARIABLE NAME IN PROGRAM          FIELD NUMBER       01640411
C                  ------------------------          ------------       01650411
C                                                                       01660411
C                  IPROG  (ROUTINE NAME)         -       1              01670411
C                  IFILE  (LOGICAL/X-NUMBER)     -       2              01680411
C                  ITOTR  (RECORDS IN FILE)      -       3              01690411
C                  IRLGN  (LENGTH OF RECORD)     -       4              01700411
C                  IRECN  (RECORD NUMBER)        -       5              01710411
C                  IEOF   (9999 IF LAST RECORD)  -       6              01720411
C                                                                       01730411
C                                                                       01740411
C                                                                       01750411
C                                                                       01760411
C     INITIALIZATION SECTION.                                           01770411
C                                                                       01780411
C     INITIALIZE CONSTANTS                                              01790411
C     ********************                                              01800411
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          01810411
      I01 = 5                                                           01820411
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              01830411
      I02 = 6                                                           01840411
C     SYSTEM ENVIRONMENT SECTION                                        01850411
C                                                                       01860411
      I01 = 5                                                           01870411
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01880411
C     (UNIT NUMBER FOR CARD READER).                                    01890411
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01900411
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01910411
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01920411
C                                                                       01930411
      I02 = 6                                                           01940411
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01950411
C     (UNIT NUMBER FOR PRINTER).                                        01960411
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01970411
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01980411
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01990411
C                                                                       02000411
      IVPASS = 0                                                        02010411
      IVFAIL = 0                                                        02020411
      IVDELE = 0                                                        02030411
      ICZERO = 0                                                        02040411
C                                                                       02050411
C     WRITE OUT PAGE HEADERS                                            02060411
C                                                                       02070411
      WRITE (I02,90002)                                                 02080411
      WRITE (I02,90006)                                                 02090411
      WRITE (I02,90008)                                                 02100411
      WRITE (I02,90004)                                                 02110411
      WRITE (I02,90010)                                                 02120411
      WRITE (I02,90004)                                                 02130411
      WRITE (I02,90016)                                                 02140411
      WRITE (I02,90001)                                                 02150411
      WRITE (I02,90004)                                                 02160411
      WRITE (I02,90012)                                                 02170411
      WRITE (I02,90014)                                                 02180411
      WRITE (I02,90004)                                                 02190411
C                                                                       02200411
      I04 = 8                                                           02210411
C     I04  CONTAINS THE LOGICAL UNIT NUMBER FOR A SEQUENTIAL ACCESS FILE02220411
      OPEN(UNIT=I04,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')             02230411
CX041        THIS CARD IS REPLACED BY CONTENTS OF X-041 CARD            02240411
      IPROG = 411                                                       02250411
      IFILE = I04                                                       02260411
      ITOTR = 142                                                       02270411
      IRLGN = 80                                                        02280411
      IRECN = 0                                                         02290411
      IEOF = 0                                                          02300411
C                                                                       02310411
C     ****  FCVS PROGRAM 411  -  TEST 001  ****                         02320411
C                                                                       02330411
C                                                                       02340411
C        TEST 001 USES THE REWIND STATEMENT ON A FILE THAT IS CONNECTED 02350411
C     BUT DOES NOT EXIST.  THERE SHOULD BE NO EFFECT ON THE FILE WHEN   02360411
C     THIS STATEMENT IS EXECUTED.  CONNECTION OF THE FILE TO A UNIT     02370411
C     IS ASSUMED TO BE DONE BY PRECONNECTION.                           02380411
C                                                                       02390411
C                  SEE SECTION 12.10.4.3,  REWIND STATEMENT             02400411
C                                                                       02410411
C                                                                       02420411
      IVTNUM =   1                                                      02430411
      IF (ICZERO) 30010, 0010, 30010                                    02440411
 0010 CONTINUE                                                          02450411
      IVCORR = 1                                                        02460411
      IVCOMP = 0                                                        02470411
      REWIND I04                                                        02480411
      IVCOMP = 1                                                        02490411
40010 IF (IVCOMP - 1) 20010, 10010, 20010                               02500411
30010 IVDELE = IVDELE + 1                                               02510411
      WRITE (I02,80000) IVTNUM                                          02520411
      IF (ICZERO) 10010, 0021, 20010                                    02530411
10010 IVPASS = IVPASS + 1                                               02540411
      WRITE (I02,80002) IVTNUM                                          02550411
      GO TO 0021                                                        02560411
20010 IVFAIL = IVFAIL + 1                                               02570411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02580411
 0021 CONTINUE                                                          02590411
C                                                                       02600411
C     ****  FCVS PROGRAM 411  -  TEST 002  ****                         02610411
C                                                                       02620411
C                                                                       02630411
C         TEST 002 USES THE ENDFILE STATEMENT TO CREATE A FILE THAT IS  02640411
C     CONNECTED BUT DOES NOT EXIST.  NO RECORDS HAVE BEEN WRITTEN TO    02650411
C     THE FILE BEFORE THE ENDFILE STATEMENT IS EXECUTED.  AS IN THE     02660411
C     PRECEDING TEST, IT IS ASSUMED THAT CONNECTION OF THE FILE TO A    02670411
C     UNIT IS DONE BY PRECONNECTION.                                    02680411
C                                                                       02690411
C                  SEE SECTIONS 12.2.1,   FILE EXISTENCE                02700411
C                               12.10.4.2,  ENDFILE STATEMENT           02710411
C                                                                       02720411
C                                                                       02730411
      IVTNUM =   2                                                      02740411
      IF (ICZERO) 30020, 0020, 30020                                    02750411
 0020 CONTINUE                                                          02760411
      IVCORR = 1                                                        02770411
      IVCOMP = 0                                                        02780411
      ENDFILE I04                                                       02790411
      REWIND  I04                                                       02800411
      READ (I04, END = 0023)  IVON01                                    02810411
C                                                                       02820411
C        TO TEST CREATION OF A FILE VIA A ENDFILE STATEMENT THE FILE    02830411
C     IS REWOUND AND READ.  AN END-OF-FILE CONDITION IS EXPECTED TO     02840411
C     OCCUR ON THE FIRST READ SINCE THE ONLY RECORD WRITTEN TO THE      02850411
C     FILE WAS THE ENDFILE RECORD.                                      02860411
C                                                                       02870411
      IVCOMP = 0                                                        02880411
      GO TO 40020                                                       02890411
 0023 IVCOMP = 1                                                        02900411
40020 IF (IVCOMP - 1)  20020, 10020, 20020                              02910411
30020 IVDELE = IVDELE + 1                                               02920411
      WRITE (I02,80000) IVTNUM                                          02930411
      IF (ICZERO) 10020, 0031, 20020                                    02940411
10020 IVPASS = IVPASS + 1                                               02950411
      WRITE (I02,80002) IVTNUM                                          02960411
      GO TO 0031                                                        02970411
20020 IVFAIL = IVFAIL + 1                                               02980411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02990411
 0031 CONTINUE                                                          03000411
C                                                                       03010411
C                                                                       03020411
C        TESTS 003 THROUGH 019 USE A PRECONNECTED FILE FOR SEQUENTIAL   03030411
C     ACCESS TO WRITE 141 RECORDS TO THE FILE. THESE TESTS TEST USE OF  03040411
C     THE ALLOWABLE FORMS OF THE WRITE STATEMENT ON A  FILE CONNECTED   03050411
C     FOR SEQUENTIAL ACCESS.  THE WRITE STATEMENT IS USED WITH          03060411
C     THE I/O LIST ITEM AS A VARIABLE, ARRAY ELEMENT AND AN ARRAY.      03070411
C        THE PURPOSE OF TESTS 003 THROUGH 019 IS TO CHECK THE COMPILER'S03080411
C     ABILITY TO HANDLE THE VARIOUS STATEMENT  CONSTRUCTS OF THE        03090411
C     WRITE STATEMENT.      LATER TESTS WITHIN THIS ROUINE READ AND     03100411
C     CHECK THE RECORDS WHICH ARE CREATED.                              03110411
C        THE VALUE IN IVCORR FOR TESTS 002 THROUGH 013 IS THE RECORD    03120411
C     NUMBER FOR THE RECORD.                                            03130411
C                                                                       03140411
C                                                                       03150411
C                                                                       03160411
C     ****  FCVS PROGRAM 411  -  TEST 003  ****                         03170411
C                                                                       03180411
C                                                                       03190411
      IVTNUM =   3                                                      03200411
      IF (ICZERO) 30030, 0030, 30030                                    03210411
 0030 CONTINUE                                                          03220411
      REWIND I04                                                        03230411
C     REPOSITION TO BEGINNING OF FILE                                   03240411
C                                                                       03250411
C        TEST 003 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     03260411
C     IS A VARIABLE OF INTEGER TYPE.                                    03270411
C                                                                       03280411
      IRECN = 01                                                        03290411
      IVCORR = 01                                                       03300411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    03310411
     1   ICON21, ICON22, ICON31, ICON32, ICON33, ICON34, ICON55, ICON56 03320411
      IVCOMP = IRECN                                                    03330411
40030 IF (IVCOMP - 01)  20030, 10030, 20030                             03340411
30030 IVDELE = IVDELE + 1                                               03350411
      WRITE (I02,80000) IVTNUM                                          03360411
      IF (ICZERO) 10030, 0041, 20030                                    03370411
10030 IVPASS = IVPASS + 1                                               03380411
      WRITE (I02,80002) IVTNUM                                          03390411
      GO TO 0041                                                        03400411
20030 IVFAIL = IVFAIL + 1                                               03410411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03420411
 0041 CONTINUE                                                          03430411
C                                                                       03440411
C     ****  FCVS PROGRAM 411  -  TEST 004  ****                         03450411
C                                                                       03460411
C                                                                       03470411
C        TEST 004 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     03480411
C     IS A VARIABLE OF REAL TYPE.                                       03490411
C                                                                       03500411
C                                                                       03510411
      IVTNUM =   4                                                      03520411
      IF (ICZERO) 30040, 0040, 30040                                    03530411
 0040 CONTINUE                                                          03540411
      IRECN = 02                                                        03550411
      IVCORR = 02                                                       03560411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    03570411
     1   RCON21, RCON22, RCON31, RCON32, RCON33, RCON34, RCON55, RCON56 03580411
      IVCOMP = IRECN                                                    03590411
40040 IF (IVCOMP - 02)  20040, 10040, 20040                             03600411
30040 IVDELE = IVDELE + 1                                               03610411
      WRITE (I02,80000) IVTNUM                                          03620411
      IF (ICZERO) 10040, 0051, 20040                                    03630411
10040 IVPASS = IVPASS + 1                                               03640411
      WRITE (I02,80002) IVTNUM                                          03650411
      GO TO 0051                                                        03660411
20040 IVFAIL = IVFAIL + 1                                               03670411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03680411
 0051 CONTINUE                                                          03690411
C                                                                       03700411
C     ****  FCVS PROGRAM 411  -  TEST 005  ****                         03710411
C                                                                       03720411
C                                                                       03730411
C        TEST 005 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     03740411
C     IS A VARIABLE OF LOGICAL TYPE.                                    03750411
C                                                                       03760411
C                                                                       03770411
      IVTNUM =   5                                                      03780411
      IF (ICZERO) 30050, 0050, 30050                                    03790411
 0050 CONTINUE                                                          03800411
      IRECN = 03                                                        03810411
      IVCORR = 03                                                       03820411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    03830411
     1   LCONT1, LCONF2,  LCONT3, LCONF4, LCONT5, LCONF6, LCONT7, LCONF803840411
      IVCOMP = IRECN                                                    03850411
40050 IF (IVCOMP - 03)  20050, 10050, 20050                             03860411
30050 IVDELE = IVDELE + 1                                               03870411
      WRITE (I02,80000) IVTNUM                                          03880411
      IF (ICZERO) 10050, 0061, 20050                                    03890411
10050 IVPASS = IVPASS + 1                                               03900411
      WRITE (I02,80002) IVTNUM                                          03910411
      GO TO 0061                                                        03920411
20050 IVFAIL = IVFAIL + 1                                               03930411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03940411
 0061 CONTINUE                                                          03950411
C                                                                       03960411
C     ****  FCVS PROGRAM 411  -  TEST 006  ****                         03970411
C                                                                       03980411
C                                                                       03990411
C        TEST 006 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     04000411
C     IS AN ARRAY ELEMENT OF INTEGER TYPE.   ONE, TWO AND THREE         04010411
C     DIMENSION ARRAYS ARE USED.                                        04020411
C                                                                       04030411
C                                                                       04040411
      IVTNUM =   6                                                      04050411
      IF (ICZERO) 30060, 0060, 30060                                    04060411
 0060 CONTINUE                                                          04070411
      IRECN = 04                                                        04080411
      IVCORR = 04                                                       04090411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    04100411
     1   IAON11(1), IAON11(2), IAON21(1,2), IAON21(2,2), IAON31(1,1,2), 04110411
     2   IAON31(2,1,2), IAON11(7), IAON11(8)                            04120411
      IVCOMP = IRECN                                                    04130411
40060 IF (IVCOMP - 04)  20060, 10060, 20060                             04140411
30060 IVDELE = IVDELE + 1                                               04150411
      WRITE (I02,80000) IVTNUM                                          04160411
      IF (ICZERO) 10060, 0071, 20060                                    04170411
10060 IVPASS = IVPASS + 1                                               04180411
      WRITE (I02,80002) IVTNUM                                          04190411
      GO TO 0071                                                        04200411
20060 IVFAIL = IVFAIL + 1                                               04210411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04220411
 0071 CONTINUE                                                          04230411
C                                                                       04240411
C     ****  FCVS PROGRAM 411  -  TEST 007  ****                         04250411
C                                                                       04260411
C                                                                       04270411
C        TEST 007 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     04280411
C     IS AN ARRAY ELEMENT OF REAL TYPE.  ONE, TWO AND THREE             04290411
C     DIMENSION ARRAYS ARE USED.                                        04300411
C                                                                       04310411
C                                                                       04320411
      IVTNUM =   7                                                      04330411
      IF (ICZERO) 30070, 0070, 30070                                    04340411
 0070 CONTINUE                                                          04350411
      IRECN = 05                                                        04360411
      IVCORR = 05                                                       04370411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    04380411
     1   RAON11(1), RAON11(2), RAON21(1,2), RAON21(2,2), RAON31(1,1,2), 04390411
     2RAON31(2,1,2), RAON11(7), RAON11 (8)                              04400411
      IVCOMP = IRECN                                                    04410411
40070 IF (IVCOMP - 05)  20070, 10070, 20070                             04420411
30070 IVDELE = IVDELE + 1                                               04430411
      WRITE (I02,80000) IVTNUM                                          04440411
      IF (ICZERO) 10070, 0081, 20070                                    04450411
10070 IVPASS = IVPASS + 1                                               04460411
      WRITE (I02,80002) IVTNUM                                          04470411
      GO TO 0081                                                        04480411
20070 IVFAIL = IVFAIL + 1                                               04490411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04500411
 0081 CONTINUE                                                          04510411
C                                                                       04520411
C     ****  FCVS PROGRAM 411  -  TEST 008  ****                         04530411
C                                                                       04540411
C                                                                       04550411
C                                                                       04560411
      IVTNUM =   8                                                      04570411
      IF (ICZERO) 30080, 0080, 30080                                    04580411
 0080 CONTINUE                                                          04590411
      IRECN = 06                                                        04600411
      IVCORR = 06                                                       04610411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    04620411
     1   LAON11(1), LAON11(2), LAON21(1,2), LAON21(2,2), LAON31(1,1,2), 04630411
     2   LAON31(2,1,2), LAON11(7), LAON11(8)                            04640411
      IVCOMP = IRECN                                                    04650411
40080 IF (IVCOMP - 06)  20080, 10080, 20080                             04660411
30080 IVDELE = IVDELE + 1                                               04670411
      WRITE (I02,80000) IVTNUM                                          04680411
      IF (ICZERO) 10080, 0091, 20080                                    04690411
10080 IVPASS = IVPASS + 1                                               04700411
      WRITE (I02,80002) IVTNUM                                          04710411
      GO TO 0091                                                        04720411
20080 IVFAIL = IVFAIL + 1                                               04730411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04740411
 0091 CONTINUE                                                          04750411
C                                                                       04760411
C     ****  FCVS PROGRAM 411  -  TEST 009  ****                         04770411
C                                                                       04780411
C                                                                       04790411
C        TEST 009 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     04800411
C     IS AN ARRAY OF INTEGER TYPE.                                      04810411
C                                                                       04820411
C                                                                       04830411
      IVTNUM =   9                                                      04840411
      IF (ICZERO) 30090, 0090, 30090                                    04850411
 0090 CONTINUE                                                          04860411
      IRECN = 07                                                        04870411
      IVCORR = 07                                                       04880411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    04890411
     1   IAON31                                                         04900411
      IVCOMP = IRECN                                                    04910411
40090 IF (IVCOMP - 07)  20090, 10090, 20090                             04920411
30090 IVDELE = IVDELE + 1                                               04930411
      WRITE (I02,80000) IVTNUM                                          04940411
      IF (ICZERO) 10090, 0101, 20090                                    04950411
10090 IVPASS = IVPASS + 1                                               04960411
      WRITE (I02,80002) IVTNUM                                          04970411
      GO TO 0101                                                        04980411
20090 IVFAIL = IVFAIL + 1                                               04990411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05000411
 0101 CONTINUE                                                          05010411
C                                                                       05020411
C     ****  FCVS PROGRAM 411  -  TEST 010  ****                         05030411
C                                                                       05040411
C                                                                       05050411
C        TEST 010 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     05060411
C     IS AN ARRAY OF REAL TYPE.                                         05070411
C                                                                       05080411
C                                                                       05090411
      IVTNUM =  10                                                      05100411
      IF (ICZERO) 30100, 0100, 30100                                    05110411
 0100 CONTINUE                                                          05120411
      IRECN = 08                                                        05130411
      IVCORR = 08                                                       05140411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05150411
     1   RAON31                                                         05160411
      IVCOMP = IRECN                                                    05170411
40100 IF (IVCOMP - 08)  20100, 10100, 20100                             05180411
30100 IVDELE = IVDELE + 1                                               05190411
      WRITE (I02,80000) IVTNUM                                          05200411
      IF (ICZERO) 10100, 0111, 20100                                    05210411
10100 IVPASS = IVPASS + 1                                               05220411
      WRITE (I02,80002) IVTNUM                                          05230411
      GO TO 0111                                                        05240411
20100 IVFAIL = IVFAIL + 1                                               05250411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05260411
 0111 CONTINUE                                                          05270411
C                                                                       05280411
C     ****  FCVS PROGRAM 411  -  TEST 011  ****                         05290411
C                                                                       05300411
C                                                                       05310411
C        TEST 011 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     05320411
C     IS AN ARRAY OF LOGICAL TYPE.                                      05330411
C                                                                       05340411
C                                                                       05350411
      IVTNUM =  11                                                      05360411
      IF (ICZERO) 30110, 0110, 30110                                    05370411
 0110 CONTINUE                                                          05380411
      IRECN = 09                                                        05390411
      IVCORR = 09                                                       05400411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05410411
     1   LAON31                                                         05420411
      IVCOMP = IRECN                                                    05430411
40110 IF (IVCOMP - 09)  20110, 10110, 20110                             05440411
30110 IVDELE = IVDELE + 1                                               05450411
      WRITE (I02,80000) IVTNUM                                          05460411
      IF (ICZERO) 10110, 0121, 20110                                    05470411
10110 IVPASS = IVPASS + 1                                               05480411
      WRITE (I02,80002) IVTNUM                                          05490411
      GO TO 0121                                                        05500411
20110 IVFAIL = IVFAIL + 1                                               05510411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05520411
 0121 CONTINUE                                                          05530411
C                                                                       05540411
C     ****  FCVS PROGRAM 411  -  TEST 012  ****                         05550411
C                                                                       05560411
C                                                                       05570411
C        TEST 012 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     05580411
C     IS AN IMPLIED-DO   WITH AN ITEM OF INTEGER TYPE.                  05590411
C        THE FIELD VALUES ARE WRITTEN IN MIXED ORDER VIS-A-VIS THE      05600411
C     ELEMENT SEQUENCE OF ARRAY IAON31.  THE SEQUENCE OF VALUES WRITTEN 05610411
C     IN THE RECORD ARE 11, 512, 777, -32767, -11, -512, -777, 32767.   05620411
C                                                                       05630411
C                                                                       05640411
      IVTNUM =  12                                                      05650411
      IF (ICZERO) 30120, 0120, 30120                                    05660411
 0120 CONTINUE                                                          05670411
      IRECN = 10                                                        05680411
      IVCORR = 10                                                       05690411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05700411
     1   (((IAON31 (J,K,I), I=1,2), K=1,2), J=1,2)                      05710411
      IVCOMP = IRECN                                                    05720411
40120 IF (IVCOMP - 10)  20120, 10120, 20120                             05730411
30120 IVDELE = IVDELE + 1                                               05740411
      WRITE (I02,80000) IVTNUM                                          05750411
      IF (ICZERO) 10120, 0131, 20120                                    05760411
10120 IVPASS = IVPASS + 1                                               05770411
      WRITE (I02,80002) IVTNUM                                          05780411
      GO TO 0131                                                        05790411
20120 IVFAIL = IVFAIL + 1                                               05800411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05810411
 0131 CONTINUE                                                          05820411
C                                                                       05830411
C     ****  FCVS PROGRAM 411  -  TEST 013  ****                         05840411
C                                                                       05850411
C                                                                       05860411
C        TEST 013 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     05870411
C     IS AN IMPLIED-DO WITH AN ITEM OF REAL TYPE.  THE FIELD VALUES     05880411
C     (IN FIELD POSITION ORDER) WRITTEN IN THE RECORD ARE 11., -11.,    05890411
C     7.77, -7.77, .512, -.512, -32767., 32767.                         05900411
C                                                                       05910411
C                                                                       05920411
      IVTNUM =  13                                                      05930411
      IF (ICZERO) 30130, 0130, 30130                                    05940411
 0130 CONTINUE                                                          05950411
      IRECN = 11                                                        05960411
      IVCORR = 11                                                       05970411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    05980411
     1   (((RAON31 (J,K,I), J=1,2), K=1,2), I=1,2)                      05990411
      IVCOMP = IRECN                                                    06000411
40130 IF (IVCOMP - 11)  20130, 10130, 20130                             06010411
30130 IVDELE = IVDELE + 1                                               06020411
      WRITE (I02,80000) IVTNUM                                          06030411
      IF (ICZERO) 10130, 0141, 20130                                    06040411
10130 IVPASS = IVPASS + 1                                               06050411
      WRITE (I02,80002) IVTNUM                                          06060411
      GO TO 0141                                                        06070411
20130 IVFAIL = IVFAIL + 1                                               06080411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06090411
 0141 CONTINUE                                                          06100411
C                                                                       06110411
C     ****  FCVS PROGRAM 411  -  TEST 014  ****                         06120411
C                                                                       06130411
C                                                                       06140411
C        TEST 014 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     06150411
C     IS AN IMPLIED-DO   WITH AN ITEM OF LOGICAL TYPE.                  06160411
C        THE FIELD VALUES ARE WRITTEN IN MIXED ORDER (AN ORDER          06170411
C     DIFFERENT THAN TEST 012 ABOVE) VIS-A-VIS THE                      06180411
C     ELEMENT SEQUENCE OF ARRAY LAON31.  THE SEQUENCE OF VALUES WRITTEN 06190411
C     IN THE RECORD ARE .TRUE., .TRUE., .FALSE., .FALSE., .TRUE., .TRUE.06200411
C     .FALSE, .FALSE.                                                   06210411
C                                                                       06220411
C                                                                       06230411
      IVTNUM =  14                                                      06240411
      IF (ICZERO) 30140, 0140, 30140                                    06250411
 0140 CONTINUE                                                          06260411
      IRECN = 12                                                        06270411
      IVCORR = 12                                                       06280411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    06290411
     1   (((LAON31 (J,K,I), K=1,2), J=1,2), I=1,2)                      06300411
      IVCOMP = IRECN                                                    06310411
40140 IF (IVCOMP - 12)  20140, 10140, 20140                             06320411
30140 IVDELE = IVDELE + 1                                               06330411
      WRITE (I02,80000) IVTNUM                                          06340411
      IF (ICZERO) 10140, 0151, 20140                                    06350411
10140 IVPASS = IVPASS + 1                                               06360411
      WRITE (I02,80002) IVTNUM                                          06370411
      GO TO 0151                                                        06380411
20140 IVFAIL = IVFAIL + 1                                               06390411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06400411
 0151 CONTINUE                                                          06410411
C                                                                       06420411
C     ****  FCVS PROGRAM 411  -  TEST 015  ****                         06430411
C                                                                       06440411
C                                                                       06450411
C        TEST 015 USES A WRITE STATEMENT WITHOUT ANY OUTPUT LIST ITEMS. 06460411
C     THE OUTPUT LIST ITEMS ARE OPTIONAL.                               06470411
C     ALSO THE LENGTH OF AN UNFORMATTED RECORD MAY BE ZERO.             06480411
C                                                                       06490411
C                  SEE SECTIONS 12.1.2,   UNFORMATTED RECORDS           06500411
C                               12.8,  READ, WRITE AND PRINT STATEMENTS 06510411
C                                                                       06520411
C                                                                       06530411
      IVTNUM =  15                                                      06540411
      IF (ICZERO) 30150, 0150, 30150                                    06550411
 0150 CONTINUE                                                          06560411
      IRECN = 13                                                        06570411
      IVCORR = 13                                                       06580411
      WRITE (I04)                                                       06590411
      IVCOMP = IRECN                                                    06600411
40150 IF (IVCOMP - 13)  20150, 10150, 20150                             06610411
30150 IVDELE = IVDELE + 1                                               06620411
      WRITE (I02,80000) IVTNUM                                          06630411
      IF (ICZERO) 10150, 0161, 20150                                    06640411
10150 IVPASS = IVPASS + 1                                               06650411
      WRITE (I02,80002) IVTNUM                                          06660411
      GO TO 0161                                                        06670411
20150 IVFAIL = IVFAIL + 1                                               06680411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06690411
 0161 CONTINUE                                                          06700411
C                                                                       06710411
C     ****  FCVS PROGRAM 411  -  TEST 016  ****                         06720411
C                                                                       06730411
C                                                                       06740411
C        TEST 016 IS SIMILAR  TO THE PREVIOUS TEST EXCEPT THE WRITE     06750411
C     STATEMENT CONTAINS OUTPUT LIST ITEMS.  ONE HUNDRED RECORDS ARE    06760411
C     WRITTEN.                                                          06770411
C                                                                       06780411
C                                                                       06790411
      IVTNUM =  16                                                      06800411
      IF (ICZERO) 30160, 0160, 30160                                    06810411
 0160 CONTINUE                                                          06820411
      IRECN = 13                                                        06830411
      DO 4132 I = 1,100                                                 06840411
      IRECN = IRECN + 1                                                 06850411
      WRITE (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,    06860411
     1   ICON21, ICON22, ICON31, ICON32, ICON33, ICON34, ICON55, ICON56 06870411
 4132 CONTINUE                                                          06880411
      IVCORR = 100                                                      06890411
      IVCOMP = IRECN - 13                                               06900411
40160 IF (IVCOMP - 100) 20160, 10160, 20160                             06910411
30160 IVDELE = IVDELE + 1                                               06920411
      WRITE (I02,80000) IVTNUM                                          06930411
      IF (ICZERO) 10160, 0171, 20160                                    06940411
10160 IVPASS = IVPASS + 1                                               06950411
      WRITE (I02,80002) IVTNUM                                          06960411
      GO TO 0171                                                        06970411
20160 IVFAIL = IVFAIL + 1                                               06980411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06990411
 0171 CONTINUE                                                          07000411
C                                                                       07010411
C                                                                       07020411
C        THE NEXT THREE TESTS TEST  USE OF THE BACKSPACE AND ENDFILE    07030411
C     STATEMENTS                                                        07040411
C                                                                       07050411
C                                                                       07060411
C                                                                       07070411
C     ****  FCVS PROGRAM 411  -  TEST 017  ****                         07080411
C                                                                       07090411
C        TEST 017 USES AN ENDFILE STATEMENT TO WRITE AN ENDFILE         07100411
C     RECORD  TO  A  FILE WITH UNFORMATTED RECORDS.  AFTER EXECUTION    07110411
C     OF THIS STATEMENT THE FILE SHOULD BE POSITION AFTER THE ENDFILE   07120411
C     RECORD.                                                           07130411
C                                                                       07140411
C                                                                       07150411
      IVTNUM =  17                                                      07160411
      IF (ICZERO) 30170, 0170, 30170                                    07170411
 0170 CONTINUE                                                          07180411
      IVCORR = 1                                                        07190411
      IVCOMP = 0                                                        07200411
 0172 ENDFILE I04                                                       07210411
      IVCOMP = 1                                                        07220411
40170 IF (IVCOMP - 1)  20170, 10170, 20170                              07230411
C                                                                       07240411
30170 IVDELE = IVDELE + 1                                               07250411
      WRITE (I02,80000) IVTNUM                                          07260411
      IF (ICZERO) 10170, 0181, 20170                                    07270411
10170 IVPASS = IVPASS + 1                                               07280411
      WRITE (I02,80002) IVTNUM                                          07290411
      GO TO 0181                                                        07300411
20170 IVFAIL = IVFAIL + 1                                               07310411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07320411
 0181 CONTINUE                                                          07330411
C                                                                       07340411
C     ****  FCVS PROGRAM 411  -  TEST 018  ****                         07350411
C                                                                       07360411
C                                                                       07370411
C        TEST 018 USES THE BACKSPACE STATEMENT TO REPOSITION THE FILE   07380411
C     BEFORE THE ENDFILE RECORD.                                        07390411
C                                                                       07400411
C                  SEE SECTIONS 12.10.4.1,  BACKSPACE STATEMENT         07410411
C                               12.10.4.2,  ENDFILE STATEMENT           07420411
C                                                                       07430411
C                                                                       07440411
      IVTNUM =  18                                                      07450411
      IF (ICZERO) 30180, 0180, 30180                                    07460411
 0180 CONTINUE                                                          07470411
      IVCORR = 1                                                        07480411
      IVCOMP = 0                                                        07490411
      BACKSPACE I04                                                     07500411
      IVCOMP = 1                                                        07510411
40180 IF (IVCOMP - 1)  20180, 10180, 20180                              07520411
30180 IVDELE = IVDELE + 1                                               07530411
      WRITE (I02,80000) IVTNUM                                          07540411
      IF (ICZERO) 10180, 0191, 20180                                    07550411
10180 IVPASS = IVPASS + 1                                               07560411
      WRITE (I02,80002) IVTNUM                                          07570411
      GO TO 0191                                                        07580411
20180 IVFAIL = IVFAIL + 1                                               07590411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07600411
 0191 CONTINUE                                                          07610411
C                                                                       07620411
C     ****  FCVS PROGRAM 411  -  TEST 019  ****                         07630411
C                                                                       07640411
C                                                                       07650411
C        TEST 019 IS A CONTINUATION OF THE ENDFILE AND BACKSPACE TESTS  07660411
C     (TWO PREVIOUS TESTS).  THIS TEST CONTINUES WRITTING RECORDS TO THE07670411
C     FILE OVER THE ENDFILE RECORD PREVIOUSLY WRITTEN IN TEST 017.      07680411
C     TWENTY EIGHT RECORDS ARE WRITTEN TO THE FILE FOLLOWED BY AN       07690411
C     ENDFILE.                                                          07700411
C                                                                       07710411
C                                                                       07720411
      IVTNUM =  19                                                      07730411
      IF (ICZERO) 30190, 0190, 30190                                    07740411
 0190 CONTINUE                                                          07750411
      IVCOMP = 0                                                        07760411
      IRECN = 113                                                       07770411
      DO 4112 I = 1,28                                                  07780411
      IRECN = IRECN + 1                                                 07790411
      WRITE (I04)   IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,            07800411
     1   ICON21, ICON22, ICON31, ICON32, ICON33, ICON34, ICON55, ICON56 07810411
      IVCOMP = IVCOMP + 1                                               07820411
 4112 CONTINUE                                                          07830411
      IVCORR = 29                                                       07840411
      IEOF =  9999                                                      07850411
      IRECN = IRECN + 1                                                 07860411
      WRITE (I04)   IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF             07870411
      IVCOMP = IVCOMP + 1                                               07880411
      ENDFILE I04                                                       07890411
C                                                                       07900411
C        THERE SHOULD BE A TOTAL OF 142 RECORDS PLUS AN ENDFILE RECORD  07910411
C     IN THE FILE AFTER EXECUTION OF THIS TEST.                         07920411
C                                                                       07930411
40190 IF (IVCOMP - 29) 20190, 10190, 20190                              07940411
30190 IVDELE = IVDELE + 1                                               07950411
      WRITE (I02,80000) IVTNUM                                          07960411
      IF (ICZERO) 10190, 0201, 20190                                    07970411
10190 IVPASS = IVPASS + 1                                               07980411
      WRITE (I02,80002) IVTNUM                                          07990411
      GO TO 0201                                                        08000411
20190 IVFAIL = IVFAIL + 1                                               08010411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08020411
 0201 CONTINUE                                                          08030411
C                                                                       08040411
C                                                                       08050411
C     THE NEXT SERIES OF TESTS READ AND CHECK THE RECORDS CREATED IN    08060411
C     TESTS 03  THROUGH 019.  EACH OF THE TESTS IN THIS SET IS CHECKING 08070411
C     TWO THINGS.  FIRST, THAT THE READ STATEMENT CONSTRUCT IS ACCEPTED 08080411
C     BY THE COMPILER AND SECOND THAT THE RECORDS CREATED IN TESTS 003  08090411
C     THROUGH 019 AND READ IN THESE TESTS CAN GIVE PREDICTIBLE VALUES.  08100411
C     THE READ STATEMENT IS USED WITH THE I/O LIST ITEMS AS A VARIABLE, 08110411
C     AN ARRAY ELEMENT AND AN ARRAY.                                    08120411
C                                                                       08130411
C                                                                       08140411
C                                                                       08150411
C     ****  FCVS PROGRAM 411  -  TEST 020  ****                         08160411
C                                                                       08170411
C                                                                       08180411
C        TEST 020 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  08190411
C     VARIABLE OF INTEGER TYPE.                                         08200411
C                                                                       08210411
C                                                                       08220411
      IVTNUM =  20                                                      08230411
      IF (ICZERO) 30200, 0200, 30200                                    08240411
 0200 CONTINUE                                                          08250411
      REWIND I04                                                        08260411
C        REPOSITION THE FILE TO THE FIRST RECORD                        08270411
      IVON22 = 0                                                        08280411
      IVON56 = 0                                                        08290411
      IVCORR = 30                                                       08300411
      IVCOMP = 1                                                        08310411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     08320411
     1   IVON21, IVON22, IVON31, IVON32, IVON33, IVON34, IVON55, IVON56 08330411
      IF (IRECN .EQ. 01)     IVCOMP = IVCOMP * 2                        08340411
      IF (IVON22 .EQ. -11)   IVCOMP = IVCOMP * 3                        08350411
      IF (IVON56 .EQ. 32767) IVCOMP = IVCOMP * 5                        08360411
40200 IF (IVCOMP - 30)  20200, 10200, 20200                             08370411
30200 IVDELE = IVDELE + 1                                               08380411
      WRITE (I02,80000) IVTNUM                                          08390411
      IF (ICZERO) 10200, 0211, 20200                                    08400411
10200 IVPASS = IVPASS + 1                                               08410411
      WRITE (I02,80002) IVTNUM                                          08420411
      GO TO 0211                                                        08430411
20200 IVFAIL = IVFAIL + 1                                               08440411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08450411
 0211 CONTINUE                                                          08460411
C                                                                       08470411
C     ****  FCVS PROGRAM 411  -  TEST 021  ****                         08480411
C                                                                       08490411
C                                                                       08500411
C        TEST 021 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  08510411
C     VARIABLE OF REAL TYPE.                                            08520411
C                                                                       08530411
C                                                                       08540411
      IVTNUM =  21                                                      08550411
      IF (ICZERO) 30210, 0210, 30210                                    08560411
 0210 CONTINUE                                                          08570411
      RVON22 = 0.0                                                      08580411
      RVON31 = 0.0                                                      08590411
      IVCORR = 30                                                       08600411
      IVCOMP = 1                                                        08610411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     08620411
     1   RVON21, RVON22, RVON31, RVON32, RVON33, RVON34, RVON55, RVON56 08630411
      IF (IRECN .EQ. 02)      IVCOMP = IVCOMP * 2                       08640411
      IF (RVON22 .EQ. -11.)  IVCOMP = IVCOMP * 3                        08650411
      IF (RVON31 .EQ. 7.77)  IVCOMP = IVCOMP * 5                        08660411
40210 IF (IVCOMP - 30)  20210, 10210, 20210                             08670411
30210 IVDELE = IVDELE + 1                                               08680411
      WRITE (I02,80000) IVTNUM                                          08690411
      IF (ICZERO) 10210, 0221, 20210                                    08700411
10210 IVPASS = IVPASS + 1                                               08710411
      WRITE (I02,80002) IVTNUM                                          08720411
      GO TO 0221                                                        08730411
20210 IVFAIL = IVFAIL + 1                                               08740411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08750411
 0221 CONTINUE                                                          08760411
C                                                                       08770411
C     ****  FCVS PROGRAM 411  -  TEST 022  ****                         08780411
C                                                                       08790411
C                                                                       08800411
C        TEST 022 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  08810411
C     VARIABLE OF LOGICAL TYPE.                                         08820411
C                                                                       08830411
C                                                                       08840411
      IVTNUM =  22                                                      08850411
      IF (ICZERO) 30220, 0220, 30220                                    08860411
 0220 CONTINUE                                                          08870411
      LVONT1 = .FALSE.                                                  08880411
      LVONF6 = .TRUE.                                                   08890411
      IVCORR = 30                                                       08900411
      IVCOMP = 1                                                        08910411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     08920411
     1   LVONT1, LVONF2,  LVONT3, LVONF4, LVONT5, LVONF6, LVONT7, LVONF808930411
      IF (IRECN .EQ. 03)     IVCOMP = IVCOMP * 2                        08940411
      IF (.NOT. LVONF6)      IVCOMP = IVCOMP * 3                        08950411
      IF (LVONT1)            IVCOMP = IVCOMP * 5                        08960411
40220 IF (IVCOMP - 30)  20220, 10220, 20220                             08970411
30220 IVDELE = IVDELE + 1                                               08980411
      WRITE (I02,80000) IVTNUM                                          08990411
      IF (ICZERO) 10220, 0231, 20220                                    09000411
10220 IVPASS = IVPASS + 1                                               09010411
      WRITE (I02,80002) IVTNUM                                          09020411
      GO TO 0231                                                        09030411
20220 IVFAIL = IVFAIL + 1                                               09040411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09050411
 0231 CONTINUE                                                          09060411
C                                                                       09070411
C     ****  FCVS PROGRAM 411  -  TEST 023  ****                         09080411
C                                                                       09090411
C                                                                       09100411
C        TEST 023 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  09110411
C     ARRAY ELEMENT OF INTEGER TYPE.  ONE, TWO, AND THREE               09120411
C     DIMENSION ARRAYS ARE USED.                                        09130411
C                                                                       09140411
C                                                                       09150411
      IVTNUM =  23                                                      09160411
      IF (ICZERO) 30230, 0230, 30230                                    09170411
 0230 CONTINUE                                                          09180411
      IAON12(2) = 0                                                     09190411
      IAON12(8) = 0                                                     09200411
      IVCORR = 30                                                       09210411
      IVCOMP = 1                                                        09220411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     09230411
     1   IAON12(1), IAON12(2), IAON22(1,2), IAON22(2,2), IAON32(1,1,2), 09240411
     2   IAON32(2,1,2), IAON12(7), IAON12(8)                            09250411
      IF (IRECN .EQ. 04)   IVCOMP = IVCOMP * 2                          09260411
      IF (IAON12(2) .EQ. -11)   IVCOMP = IVCOMP * 3                     09270411
      IF (IAON12(8) .EQ. 32767) IVCOMP = IVCOMP * 5                     09280411
C                                                                       09290411
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 09300411
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           09310411
C                                                                       09320411
40230 IF (IVCOMP - 30)   20230, 10230, 20230                            09330411
30230 IVDELE = IVDELE + 1                                               09340411
      WRITE (I02,80000) IVTNUM                                          09350411
      IF (ICZERO) 10230, 0241, 20230                                    09360411
10230 IVPASS = IVPASS + 1                                               09370411
      WRITE (I02,80002) IVTNUM                                          09380411
      GO TO 0241                                                        09390411
20230 IVFAIL = IVFAIL + 1                                               09400411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09410411
 0241 CONTINUE                                                          09420411
C                                                                       09430411
C     ****  FCVS PROGRAM 411  -  TEST 024  ****                         09440411
C                                                                       09450411
C                                                                       09460411
C        TEST 024 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  09470411
C     ARRAY ELEMENT OF REAL TYPE.  ONE, TWO, AND THREE                  09480411
C     DIMENSION ARRAYS ARE USED.                                        09490411
C                                                                       09500411
C                                                                       09510411
      IVTNUM =  24                                                      09520411
      IF (ICZERO) 30240, 0240, 30240                                    09530411
 0240 CONTINUE                                                          09540411
      RAON22(2,2) = 0.0                                                 09550411
      RAON32(1,1,2) = 0.0                                               09560411
      IVCORR = 30                                                       09570411
      IVCOMP = 1                                                        09580411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     09590411
     1   RAON12(1), RAON12(2), RAON22(1,2), RAON22(2,2), RAON32(1,1,2), 09600411
     2   RAON32(2,1,2), RAON12(7), RAON12(8)                            09610411
      IF (IRECN .EQ. 05)             IVCOMP = IVCOMP * 2                09620411
      IF (RAON22(2,2) .EQ. -7.77)    IVCOMP = IVCOMP * 3                09630411
      IF (RAON32(1,1,2) .EQ.  .512 ) IVCOMP = IVCOMP * 5                09640411
C                                                                       09650411
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 09660411
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           09670411
C                                                                       09680411
40240 IF (IVCOMP - 30)   20240, 10240, 20240                            09690411
30240 IVDELE = IVDELE + 1                                               09700411
      WRITE (I02,80000) IVTNUM                                          09710411
      IF (ICZERO) 10240, 0251, 20240                                    09720411
10240 IVPASS = IVPASS + 1                                               09730411
      WRITE (I02,80002) IVTNUM                                          09740411
      GO TO 0251                                                        09750411
20240 IVFAIL = IVFAIL + 1                                               09760411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09770411
 0251 CONTINUE                                                          09780411
C                                                                       09790411
C     ****  FCVS PROGRAM 411  -  TEST 025  ****                         09800411
C                                                                       09810411
C                                                                       09820411
C        TEST 025 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  09830411
C     ARRAY ELEMENT OF LOGICAL TYPE.  ONE, TWO, AND THREE               09840411
C     DIMENSION ARRAYS ARE USED.                                        09850411
C                                                                       09860411
C                                                                       09870411
C                                                                       09880411
      IVTNUM =  25                                                      09890411
      IF (ICZERO) 30250, 0250, 30250                                    09900411
 0250 CONTINUE                                                          09910411
      LAON12(1) = .FALSE.                                               09920411
      LAON32(2,1,2) = .TRUE.                                            09930411
      IVCORR = 30                                                       09940411
      IVCOMP = 1                                                        09950411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     09960411
     1   LAON12(1), LAON12(2), LAON22(1,2), LAON22(2,2), LAON32(1,1,2), 09970411
     2   LAON32(2,1,2), LAON12(7), LAON12(8)                            09980411
      IF (IRECN .EQ. 06)   IVCOMP = IVCOMP * 2                          09990411
      IF (LAON12(1))          IVCOMP = IVCOMP * 3                       10000411
      IF (.NOT. LAON32(2,1,2))  IVCOMP = IVCOMP * 5                     10010411
40250 IF (IVCOMP - 30)   20250, 10250, 20250                            10020411
30250 IVDELE = IVDELE + 1                                               10030411
      WRITE (I02,80000) IVTNUM                                          10040411
      IF (ICZERO) 10250, 0261, 20250                                    10050411
10250 IVPASS = IVPASS + 1                                               10060411
      WRITE (I02,80002) IVTNUM                                          10070411
      GO TO 0261                                                        10080411
20250 IVFAIL = IVFAIL + 1                                               10090411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10100411
 0261 CONTINUE                                                          10110411
C                                                                       10120411
C     ****  FCVS PROGRAM 411  -  TEST 026  ****                         10130411
C                                                                       10140411
C                                                                       10150411
C        TEST 026 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  10160411
C     ARRAY OF INTEGER TYPE.                                            10170411
C                                                                       10180411
C                                                                       10190411
      IVTNUM =  26                                                      10200411
      IF (ICZERO) 30260, 0260, 30260                                    10210411
 0260 CONTINUE                                                          10220411
      IAON32(2,1,1) = 0                                                 10230411
      IAON32(2,2,2) = 0                                                 10240411
      IVCORR = 30                                                       10250411
      IVCOMP = 1                                                        10260411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     10270411
     1   IAON32                                                         10280411
      IF (IRECN .EQ. 07)   IVCOMP = IVCOMP * 2                          10290411
      IF (IAON32(2,1,1) .EQ. -11)    IVCOMP = IVCOMP * 3                10300411
      IF (IAON32(2,2,2) .EQ. 32767)  IVCOMP = IVCOMP * 5                10310411
C                                                                       10320411
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 10330411
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           10340411
C                                                                       10350411
40260 IF (IVCOMP - 30)   20260, 10260, 20260                            10360411
30260 IVDELE = IVDELE + 1                                               10370411
      WRITE (I02,80000) IVTNUM                                          10380411
      IF (ICZERO) 10260, 0271, 20260                                    10390411
10260 IVPASS = IVPASS + 1                                               10400411
      WRITE (I02,80002) IVTNUM                                          10410411
      GO TO 0271                                                        10420411
20260 IVFAIL = IVFAIL + 1                                               10430411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10440411
 0271 CONTINUE                                                          10450411
C                                                                       10460411
C     ****  FCVS PROGRAM 411  -  TEST 027  ****                         10470411
C                                                                       10480411
C                                                                       10490411
C        TEST 027 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  10500411
C     ARRAY OF REAL TYPE.                                               10510411
C                                                                       10520411
C                                                                       10530411
      IVTNUM =  27                                                      10540411
      IF (ICZERO) 30270, 0270, 30270                                    10550411
 0270 CONTINUE                                                          10560411
      RAON32(2,1,1) = 0.0                                               10570411
      RAON32(2,2,2) = 0.0                                               10580411
      IVCORR = 30                                                       10590411
      IVCOMP = 1                                                        10600411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     10610411
     1   RAON32                                                         10620411
      IF (IRECN .EQ. 08)   IVCOMP = IVCOMP * 2                          10630411
      IF (RAON32(2,1,1) .EQ. -11.)   IVCOMP = IVCOMP * 3                10640411
      IF (RAON32(2,2,2) .EQ.  32767.) IVCOMP = IVCOMP * 5               10650411
C                                                                       10660411
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 10670411
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           10680411
C                                                                       10690411
40270 IF (IVCOMP - 30)   20270, 10270, 20270                            10700411
30270 IVDELE = IVDELE + 1                                               10710411
      WRITE (I02,80000) IVTNUM                                          10720411
      IF (ICZERO) 10270, 0281, 20270                                    10730411
10270 IVPASS = IVPASS + 1                                               10740411
      WRITE (I02,80002) IVTNUM                                          10750411
      GO TO 0281                                                        10760411
20270 IVFAIL = IVFAIL + 1                                               10770411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10780411
 0281 CONTINUE                                                          10790411
C                                                                       10800411
C     ****  FCVS PROGRAM 411  -  TEST 028  ****                         10810411
C                                                                       10820411
C                                                                       10830411
C        TEST 028 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  10840411
C     ARRAY OF LOGICAL TYPE.                                            10850411
C                                                                       10860411
C                                                                       10870411
      IVTNUM =  28                                                      10880411
      IF (ICZERO) 30280, 0280, 30280                                    10890411
 0280 CONTINUE                                                          10900411
      LAON32(1,1,1) = .FALSE.                                           10910411
      LAON32(2,2,2) = .TRUE.                                            10920411
      IVCORR = 30                                                       10930411
      IVCOMP = 1                                                        10940411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     10950411
     1   LAON32                                                         10960411
      IF (IRECN .EQ. 09)   IVCOMP = IVCOMP * 2                          10970411
      IF (LAON32(1,1,1))     IVCOMP = IVCOMP * 3                        10980411
      IF (.NOT. LAON32(2,2,2))    IVCOMP = IVCOMP * 5                   10990411
40280 IF (IVCOMP - 30)   20280, 10280, 20280                            11000411
30280 IVDELE = IVDELE + 1                                               11010411
      WRITE (I02,80000) IVTNUM                                          11020411
      IF (ICZERO) 10280, 0291, 20280                                    11030411
10280 IVPASS = IVPASS + 1                                               11040411
      WRITE (I02,80002) IVTNUM                                          11050411
      GO TO 0291                                                        11060411
20280 IVFAIL = IVFAIL + 1                                               11070411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11080411
 0291 CONTINUE                                                          11090411
C                                                                       11100411
C     ****  FCVS PROGRAM 411  -  TEST 029  ****                         11110411
C                                                                       11120411
C                                                                       11130411
C        TEST 029 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  11140411
C     IMPLIED-DO WITH AN ITEM OF INTEGER TYPE.  THE STORAGE VALUES IN   11150411
C     THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  11160411
C     DIFFERENT STORAGE SEQUENCE IN THE ARRAY THAN FOUND IN THE RECORD  11170411
C     OF THE FILE.  THIS RECORD IS RECORD NUMBER 10 AND WAS CREATED IN  11180411
C     TEST 012 ABOVE.  THE FIELD VALUE, FIELD POSITION, POSITION WITHIN 11190411
C     ARRAY IAON32 AND SUBSCRIPT VALUE AFTER THE READ IS                11200411
C                                                                       11210411
C      VALUE      11    777     512  -32767   -11   -777   -512   32767 11220411
C      FIELD POS   1      3      2      4      5      7      6      8   11230411
C      IAON32      1      2      3      4      5      6      7      8   11240411
C      SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,211250411
C                                                                       11260411
C                                                                       11270411
      IVTNUM =  29                                                      11280411
      IF (ICZERO) 30290, 0290, 30290                                    11290411
 0290 CONTINUE                                                          11300411
      IAON32(2,1,1) = 0                                                 11310411
      IAON32(2,2,1) = 0                                                 11320411
      IVCORR = 30                                                       11330411
      IVCOMP = 1                                                        11340411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     11350411
     1   (((IAON32 (J,K,I), K=1,2), J=1,2), I=1,2)                      11360411
      IF (IRECN .EQ. 10)   IVCOMP = IVCOMP * 2                          11370411
      IF (IAON32(2,1,1) .EQ. 777)      IVCOMP = IVCOMP * 3              11380411
      IF (IAON32(2,2,1) .EQ. -32767)   IVCOMP = IVCOMP * 5              11390411
C                                                                       11400411
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 11410411
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           11420411
C                                                                       11430411
40290 IF (IVCOMP - 30)   20290, 10290, 20290                            11440411
30290 IVDELE = IVDELE + 1                                               11450411
      WRITE (I02,80000) IVTNUM                                          11460411
      IF (ICZERO) 10290, 0301, 20290                                    11470411
10290 IVPASS = IVPASS + 1                                               11480411
      WRITE (I02,80002) IVTNUM                                          11490411
      GO TO 0301                                                        11500411
20290 IVFAIL = IVFAIL + 1                                               11510411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11520411
 0301 CONTINUE                                                          11530411
C                                                                       11540411
C     ****  FCVS PROGRAM 411  -  TEST 030  ****                         11550411
C                                                                       11560411
C                                                                       11570411
C        TEST 030 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  11580411
C     IMPLIED-DO WITH AN ITEM OF REAL    TYPE.  THE STORAGE VALUES IN   11590411
C     THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  11600411
C     SEQUENCE THE SAME AS FOUND IN THE RECORD OF THE FILE.  THIS REC-  11610411
C     ORD IS RECORD NUMBER 011 AND WAS CREATED IN TEST 013 ABOVE.       11620411
C     THE FIELD VALUE, FIELD POSITION, POSITION WITHIN ARRAY RAON32  AND11630411
C     SUBSCRIPT VALUE AFTER THE THE READ IS                             11640411
C                                                                       11650411
C      VALUE      11.   -11.   7.77   -7.77  .512   -.512 -32767. 32767.11660411
C      FIELD POS   1      2      3      4      5      6      7      8   11670411
C      RAON32      1      2      3      4      5      6      7      8   11680411
C      SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,211690411
C                                                                       11700411
C                                                                       11710411
      IVTNUM =  30                                                      11720411
      IF (ICZERO) 30300, 0300, 30300                                    11730411
 0300 CONTINUE                                                          11740411
      RAON32(1,2,1) = 0.0                                               11750411
      RAON32(1,2,2) = 0.0                                               11760411
      IVCORR = 30                                                       11770411
      IVCOMP = 1                                                        11780411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     11790411
     1   (((RAON32 (J,K,I), J=1,2), K=1,2), I=1,2)                      11800411
      IF (IRECN .EQ. 11)   IVCOMP = IVCOMP * 2                          11810411
      IF (RAON32(1,2,1) .EQ. 7.77)     IVCOMP = IVCOMP * 3              11820411
      IF (RAON32(1,2,2) .EQ. -32767.)  IVCOMP = IVCOMP * 5              11830411
C                                                                       11840411
C        THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 11850411
C     FIELD VALUE AND A POSITIVE FIELD VALUE.                           11860411
C                                                                       11870411
40300 IF (IVCOMP - 30)   20300, 10300, 20300                            11880411
30300 IVDELE = IVDELE + 1                                               11890411
      WRITE (I02,80000) IVTNUM                                          11900411
      IF (ICZERO) 10300, 0311, 20300                                    11910411
10300 IVPASS = IVPASS + 1                                               11920411
      WRITE (I02,80002) IVTNUM                                          11930411
      GO TO 0311                                                        11940411
20300 IVFAIL = IVFAIL + 1                                               11950411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11960411
 0311 CONTINUE                                                          11970411
C                                                                       11980411
C     ****  FCVS PROGRAM 411  -  TEST 031  ****                         11990411
C                                                                       12000411
C                                                                       12010411
C        TEST 031 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  12020411
C     IMPLIED-DO WITH AN ITEM OF LOGICAL TYPE.  THE STORAGE VALUES IN   12030411
C     THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  12040411
C     DIFFERENT STORAGE SEQUENCE IN THE ARRAY THAN FOUND IN THE RECORD  12050411
C     OF THE FILE.  THIS RECORD IS RECORD NUMBER 12 AND WAS CREATED IN  12060411
C     TEST 014 ABOVE.  THE FIELD VALUE, FIELD POSITION, POSITION WITHIN 12070411
C     ARRAY LAON32 AND SUBSCRIPT VALUE AFTER THE READ IS                12080411
C                                                                       12090411
C      VALUE       T      T      F      F      T       T     F      F   12100411
C      FIELD POS   1      5      3      7       2      6     4      8   12110411
C      LAON32      1      2      3      4      5      6      7      8   12120411
C      SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,212130411
C                                                                       12140411
C                                                                       12150411
      IVTNUM =  31                                                      12160411
      IF (ICZERO) 30310, 0310, 30310                                    12170411
 0310 CONTINUE                                                          12180411
      LAON32(1,2,1) = .TRUE.                                            12190411
      LAON32(2,1,1) = .FALSE.                                           12200411
      IVCORR = 30                                                       12210411
      IVCOMP = 1                                                        12220411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     12230411
     1   (((LAON32 (J,K,I), I=1,2), K=1,2), J=1,2)                      12240411
      IF (IRECN .EQ. 12)   IVCOMP = IVCOMP * 2                          12250411
      IF ( .NOT. LAON32(1,2,1))   IVCOMP = IVCOMP * 3                   12260411
      IF (LAON32(2,1,1))          IVCOMP = IVCOMP * 5                   12270411
40310 IF (IVCOMP - 30)   20310, 10310, 20310                            12280411
30310 IVDELE = IVDELE + 1                                               12290411
      WRITE (I02,80000) IVTNUM                                          12300411
      IF (ICZERO) 10310, 0321, 20310                                    12310411
10310 IVPASS = IVPASS + 1                                               12320411
      WRITE (I02,80002) IVTNUM                                          12330411
      GO TO 0321                                                        12340411
20310 IVFAIL = IVFAIL + 1                                               12350411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          12360411
 0321 CONTINUE                                                          12370411
C                                                                       12380411
C     ****  FCVS PROGRAM 411  -  TEST 032  ****                         12390411
C                                                                       12400411
C                                                                       12410411
C        TEST 032 USES A READ STATEMENT WITHOUT ANY INPUT LIST ITEMS    12420411
C     (INPUT LIST ITEMS ARE OPTIONAL FOR THE READ STATEMENT). THIS      12430411
C     RECORD WAS WRITTEN IN TEST 15 AND SHOULD BE RECORD NUMBER 13.     12440411
C     THE PURPOSE OF THIS TEST IS TO SEE THAT THE STATEMENT CONSTRUCT   12450411
C     IS ACCEPTABLE TO THE COMPILER.                                    12460411
C     ALSO THE LENGTH OF AN UNFORMATTED RECORD MAY BE ZERO.             12470411
C                                                                       12480411
C                  SEE SECTIONS 12.1.2,   UNFORMATTED RECORDS           12490411
C                               12.8,   READ, WRITE AND PRINT STATEMENTS12500411
C                                                                       12510411
C                                                                       12520411
      IVTNUM =  32                                                      12530411
      IF (ICZERO) 30320, 0320, 30320                                    12540411
 0320 CONTINUE                                                          12550411
      IRECN = 13                                                        12560411
      IVCORR = 13                                                       12570411
      READ (I04)                                                        12580411
      IVCOMP = IRECN                                                    12590411
40320 IF (IVCOMP - 13)   20320, 10320, 20320                            12600411
30320 IVDELE = IVDELE + 1                                               12610411
      WRITE (I02,80000) IVTNUM                                          12620411
      IF (ICZERO) 10320, 0331, 20320                                    12630411
10320 IVPASS = IVPASS + 1                                               12640411
      WRITE (I02,80002) IVTNUM                                          12650411
      GO TO 0331                                                        12660411
20320 IVFAIL = IVFAIL + 1                                               12670411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          12680411
 0331 CONTINUE                                                          12690411
C                                                                       12700411
C     ****  FCVS PROGRAM 411  -  TEST 033  ****                         12710411
C                                                                       12720411
C                                                                       12730411
C        TEST 033 USES A READ STATEMENT IN WHICH THE NUMBER OF VALUES   12740411
C     REQUIRED BY THE INPUT LIST IS LESS THAN THE NUMBER OF VALUES IN   12750411
C     THE RECORD.  THIS TEST READS RECORD NUMBER 14 WHICH WAS CREATED   12760411
C     IN TEST 016.                                                      12770411
C                                                                       12780411
C                  SEE SECTION 12.9.5.1, UNFORMATED DATA TRANSFER       12790411
C                                                                       12800411
C                                                                       12810411
      IVTNUM =  33                                                      12820411
      IF (ICZERO) 30330, 0330, 30330                                    12830411
 0330 CONTINUE                                                          12840411
      IVON21 = 0                                                        12850411
      IVON22 = 0                                                        12860411
      IVON31 = 0                                                        12870411
      IVCORR = 0                                                        12880411
      IVCOMP = 1                                                        12890411
      READ (I04)           IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,     12900411
     1       IVON21, IVON22, IVON31                                     12910411
      IF (IRECN .EQ. 14)  IVCOMP = IVCOMP * 2                           12920411
      IF (IVON21 .EQ. 11)  IVCOMP = IVCOMP * 3                          12930411
      IF (IVON22 .EQ. -11) IVCOMP = IVCOMP * 5                          12940411
40330 IF (IVCOMP - 30) 20330, 10330, 20330                              12950411
30330 IVDELE = IVDELE + 1                                               12960411
      WRITE (I02,80000) IVTNUM                                          12970411
      IF (ICZERO) 10330, 0341, 20330                                    12980411
10330 IVPASS = IVPASS + 1                                               12990411
      WRITE (I02,80002) IVTNUM                                          13000411
      GO TO 0341                                                        13010411
20330 IVFAIL = IVFAIL + 1                                               13020411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          13030411
 0341 CONTINUE                                                          13040411
C                                                                       13050411
C                                                                       13060411
C        THE FOLLOWING TWO TESTS USE THE READ STATEMENT WITH THE        13070411
C     END SPECIFIER.                                                    13080411
C                                                                       13090411
C                                                                       13100411
C                                                                       13110411
C     ****  FCVS PROGRAM 411  -  TEST 034  ****                         13120411
C                                                                       13130411
C                                                                       13140411
C        TEST 034 USES THE READ STATEMENT WITHOUT ANY I/O LIST ITEMS.   13150411
C     THE FILE IS READ UNTIL AN END-OF-FILE CONDITION OCCURS.           13160411
C                                                                       13170411
      IVTNUM =  34                                                      13180411
      IF (ICZERO) 30340, 0340, 30340                                    13190411
 0340 CONTINUE                                                          13200411
      REWIND I04                                                        13210411
C                                                                       13220411
      IVCOMP = 1                                                        13230411
      IVON01 = 0                                                        13240411
      IVCORR = 6                                                        13250411
      DO 0342 I=1,150                                                   13260411
      READ (I04, END = 0343)                                            13270411
      IVON01 = IVON01 + 1                                               13280411
      IF (IVON01 .GT. 150)  GO TO 40340                                 13290411
 0342 CONTINUE                                                          13300411
      GO TO 40340                                                       13310411
 0343 IVCOMP = IVCOMP * 2                                               13320411
      IF (IVON01 .EQ. 142)   IVCOMP = IVCOMP * 3                        13330411
40340 IF (IVCOMP - 6) 20340, 10340, 20340                               13340411
30340 IVDELE = IVDELE + 1                                               13350411
      WRITE (I02,80000) IVTNUM                                          13360411
      IF (ICZERO) 10340, 0351, 20340                                    13370411
10340 IVPASS = IVPASS + 1                                               13380411
      WRITE (I02,80002) IVTNUM                                          13390411
      GO TO 0351                                                        13400411
20340 IVFAIL = IVFAIL + 1                                               13410411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          13420411
 0351 CONTINUE                                                          13430411
C                                                                       13440411
C     ****  FCVS PROGRAM 411  -  TEST 035  ****                         13450411
C                                                                       13460411
C                                                                       13470411
C        TEST 035  USES THE READ STATEMENT WITH INPUT LIST ITEMS.       13480411
C     THE FILE IS READ UNTIL AN END-OF-FILE CONDITION OCCURS.           13490411
C                                                                       13500411
C                                                                       13510411
      IVTNUM =  35                                                      13520411
      IF (ICZERO) 30350, 0350, 30350                                    13530411
 0350 CONTINUE                                                          13540411
      REWIND I04                                                        13550411
      IVCOMP = 1                                                        13560411
      IVCORR = 6                                                        13570411
      IVON01 = 0                                                        13580411
      IRECCK = 0                                                        13590411
      DO 0352 I = 1,150                                                 13600411
      IRECCK = IRECCK + 1                                               13610411
      IF (IRECCK .EQ. 13)  GO TO 0353                                   13620411
C         TEST 015 WROTE A RECORD WITHOUT ANY I/O LIST ITEMS THEREFORE  13630411
C     THE RECORD IS READ WITHOUT ANY I/O LIST ITEMS.                    13640411
      READ (I04, END = 0354) IPROG, IFILE, ITOTR, IRLGN, IRECN,IEOF     13650411
      GO TO 0355                                                        13660411
 0353 READ (I04)                                                        13670411
      IVON01 = IVON01 + 1                                               13680411
 0355 IF (IRECN .EQ.  IRECCK)  IVON01 = IVON01 + 1                      13690411
 0352 CONTINUE                                                          13700411
      GO TO 40350                                                       13710411
 0354 IVCOMP = IVCOMP * 2                                               13720411
      IF (IVON01 .EQ. 142)  IVCOMP = IVCOMP * 3                         13730411
40350 IF (IVCOMP - 6) 20350, 10350, 20350                               13740411
30350 IVDELE = IVDELE + 1                                               13750411
      WRITE (I02,80000) IVTNUM                                          13760411
      IF (ICZERO) 10350, 0361, 20350                                    13770411
10350 IVPASS = IVPASS + 1                                               13780411
      WRITE (I02,80002) IVTNUM                                          13790411
      GO TO 0361                                                        13800411
20350 IVFAIL = IVFAIL + 1                                               13810411
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          13820411
 0361 CONTINUE                                                          13830411
C                                                                       13840411
C                                                                       13850411
C        THE FOLLOWING SOURCE CODE BRACKETED BY THE COMMENT LINES       13860411
C     *****  BEGIN-FILE-DUMP SECTION AND *****  END-FILE-DUMP SECTION   13870411
C     MAY OR MAY NOT  APPEAR AS COMMENTS IN THE SOURCE PROGRAM.         13880411
C     THIS CODE IS OPTIONAL AND BY DEFAULT IT IS AUTOMATICALLY COMMENTED13890411
C     OUT BY THE EXECUTIVE ROUTINE.  A DUMP OF THE FILE USED BY THIS    13900411
C     ROUTINE IS PROVIDED BY USING THE *OPT1 EXECUTIVE ROUTINE CONTROL  13910411
C     CARD.  IF THE OPTIONAL CODE IS SELECTED THE ROUTINE WILL DUMP     13920411
C     THE CONTENTS OF THE FILE TO THE PRINT FILE FOLLOWING THE TEST     13930411
C     REPORT AND BEFORE THE TEST REPORT SUMMARY.                        13940411
CDB**  BEGIN FILE DUMP CODE                                             13950411
C     REWIND I04                                                        13960411
C     ITOTR = 142                                                       13970411
C     ILUN  = I04                                                       13980411
C     IRLGN = 80                                                        13990411
C     IRNUM = 1                                                         14000411
C7701 FORMAT (80A1)                                                     14010411
C7702 FORMAT (1X,80A1)                                                  14020411
C7703 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,13H RECORDS - OK)               14030411
C7704 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,27H RECORDS - THERE SHOULD BE ,I14040411
C    13,9H RECORDS.)                                                    14050411
C     DO 7771 IRNUM = 1, ITOTR                                          14060411
C     READ (ILUN, END = 7772)  (IDUMP(ICH), ICH = 1, IRLGN)             14070411
C     WRITE (I02,7702)  (IDUMP(ICH), ICH = 1, IRLGN)                    14080411
C7771 CONTINUE                                                          14090411
C7772 CONTINUE                                                          14100411
CDE**      END OF DUMP CODE                                             14110411
C        TEST  035 IS THE LAST TEST IN THIS PROGRAM.  THE ROUTINE SHOULD14120411
C     HAVE MADE 35 EXPLICIT TESTS AND PROCESSED ONE FILE CONNECTED  FOR 14130411
C     SEQUENTIAL ACCESS                                                 14140411
C                                                                       14150411
C                                                                       14160411
C                                                                       14170411
C     WRITE OUT TEST SUMMARY                                            14180411
C                                                                       14190411
      WRITE (I02,90004)                                                 14200411
      WRITE (I02,90014)                                                 14210411
      WRITE (I02,90004)                                                 14220411
      WRITE (I02,90000)                                                 14230411
      WRITE (I02,90004)                                                 14240411
      WRITE (I02,90020) IVFAIL                                          14250411
      WRITE (I02,90022) IVPASS                                          14260411
      WRITE (I02,90024) IVDELE                                          14270411
      STOP                                                              14280411
90001 FORMAT (1H ,24X,5HFM411)                                          14290411
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM411)                          14300411
C                                                                       14310411
C     FORMATS FOR TEST DETAIL LINES                                     14320411
C                                                                       14330411
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   14340411
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      14350411
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         14360411
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    14370411
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        14380411
C                                                                       14390411
C     FORMAT STATEMENTS FOR PAGE HEADERS                                14400411
C                                                                       14410411
90002 FORMAT (1H1)                                                      14420411
90004 FORMAT (1H )                                                      14430411
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            14440411
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   14450411
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         14460411
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  14470411
90014 FORMAT (1H ,5X,46H----------------------------------------------) 14480411
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             14490411
C                                                                       14500411
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 14510411
C                                                                       14520411
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              14530411
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              14540411
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             14550411
      END                                                               14560411
