      PROGRAM FM205                                                     00010205
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020205
C                                                                       00030205
C          THE ROUTINE FM205 TESTS CHARACTER CONSTANTS, CHARACTER       00040205
C     VARIABLES, AND CHARACTER ARRAY ELEMENTS WITH A MAXIMUM LENGTH     00050205
C     OF 57 CHARACTERS.  CHARACTER ASSIGNMENT STATEMENTS AND CHARACTER  00060205
C     RELATIONAL EXPRESSIONS OF THE FOLLOWING STATEMENT FORMS ARE       00070205
C     TESTED IN THIS ROUTINE.                                           00080205
C                                                                       00090205
C          (1)  CHARACTER ASSIGNMENT STATEMENTS                         00100205
C                                                                       00110205
C                  CHARACTER VARIABLE = CHARACTER CONSTANT,             00120205
C                  CHARACTER VARIABLE = CHARACTER VARIABLE,             00130205
C                  CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT,        00140205
C                  CHARACTER ARRAY ELEMENT = CHARACTER VARIABLE,        00150205
C                  CHARACTER ARRAY ELEMENT = CHARACTER ARRAY ELEMENT,   00160205
C                  CHARACTER VARIABLE = CHARACTER ARRAY ELEMENT.        00170205
C                                                                       00180205
C          THE CHARACTER ENTITIES IN AN ASSIGNMENT STATEMENT ARE THE    00190205
C          SAME LENGTH.                                                 00200205
C                                                                       00210205
C          (2)  CHARACTER RELATIONAL EXPRESSIONS                        00220205
C                                                                       00230205
C                  CHARACTER VARIABLE RELOP CHARACTER CONSTANT,         00240205
C                  CHARACTER VARIABLE RELOP CHARACTER VARIABLE,         00250205
C                  CHARACTER ARRAY ELEMENT RELOP CHARACTER CONSTANT,    00260205
C                  CHARACTER ARRAY ELEMENT RELOP CHARACTER VARIABLE,    00270205
C                  CHARACTER ARRAY ELEMENT RELOP CHAR. ARRAY ELEMENT.   00280205
C                                                                       00290205
C          THE CHARACTER ENTITIES IN A RELATIONAL EXPRESSION ARE THE    00300205
C          SAME LENGTH.                                                 00310205
C                                                                       00320205
C     REFERENCES                                                        00330205
C          AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,     00340205
C               X3.9-1978.                                              00350205
C                                                                       00360205
C          SECTION 4.8,   CHARACTER TYPE                                00370205
C          SECTION 4.8.1, CHARACTER CONSTANT                            00380205
C          SECTION 6.2,   CHARACTER EXPRESSIONS                         00390205
C          SECTION 6.3.4, CHARACTER RELATIONAL EXPRESSION               00400205
C          SECTION 6.3.5, INTERPRETATION OF CHARACTER RELATIONAL        00410205
C                            EXPRESSIONS                                00420205
C          SECTION 8.4,2, CHARACTER TYPE-STATEMENT                      00430205
C          SECTION 10.4,  CHARACTER ASSIGNMENT STATEMENT                00440205
C                                                                       00450205
C                                                                       00460205
C     ******************************************************************00470205
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00480205
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00490205
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00500205
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00510205
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00520205
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00530205
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00540205
C     THE RESULT OF EXECUTING THESE TESTS.                              00550205
C                                                                       00560205
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00570205
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00580205
C                                                                       00590205
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00600205
C                    DEPARTMENT OF THE NAVY                             00610205
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00620205
C                    WASHINGTON, D.C.   20376                           00630205
C                                                                       00640205
C     ******************************************************************00650205
C                                                                       00660205
C                                                                       00670205
      IMPLICIT LOGICAL (L)                                              00680205
      IMPLICIT CHARACTER*14 (C)                                         00690205
C                                                                       00700205
      CHARACTER CVTN01*3,CVTN02*7,CVTN03*12                             00710205
      CHARACTER CVTN04*25,CVTN05*41,CVTN06*57                           00720205
      CHARACTER CVTN07*3,CVTN08*7,CVTN09*12                             00730205
      CHARACTER CVTN10*25,CVTN11*41,CVTN12*57                           00740205
      CHARACTER CATN11(6)*3,CATN12(7)*7,CATN13(3)*12                    00750205
      CHARACTER CATN14(2)*25,CATN15(10)*41,CATN16(4)*57                 00760205
C                                                                       00770205
C                                                                       00780205
C                                                                       00790205
C     INITIALIZATION SECTION.                                           00800205
C                                                                       00810205
C     INITIALIZE CONSTANTS                                              00820205
C     ********************                                              00830205
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00840205
      I01 = 5                                                           00850205
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00860205
      I02 = 6                                                           00870205
C     SYSTEM ENVIRONMENT SECTION                                        00880205
C                                                                       00890205
      I01 = 5                                                           00900205
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00910205
C     (UNIT NUMBER FOR CARD READER).                                    00920205
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00930205
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00940205
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00950205
C                                                                       00960205
      I02 = 6                                                           00970205
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00980205
C     (UNIT NUMBER FOR PRINTER).                                        00990205
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01000205
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01010205
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01020205
C                                                                       01030205
      IVPASS = 0                                                        01040205
      IVFAIL = 0                                                        01050205
      IVDELE = 0                                                        01060205
      ICZERO = 0                                                        01070205
C                                                                       01080205
C     WRITE OUT PAGE HEADERS                                            01090205
C                                                                       01100205
      WRITE (I02,90002)                                                 01110205
      WRITE (I02,90006)                                                 01120205
      WRITE (I02,90008)                                                 01130205
      WRITE (I02,90004)                                                 01140205
      WRITE (I02,90010)                                                 01150205
      WRITE (I02,90004)                                                 01160205
      WRITE (I02,90016)                                                 01170205
      WRITE (I02,90001)                                                 01180205
      WRITE (I02,90004)                                                 01190205
      WRITE (I02,90012)                                                 01200205
      WRITE (I02,90014)                                                 01210205
      WRITE (I02,90004)                                                 01220205
C                                                                       01230205
C                                                                       01240205
C          TEST 87 THROUGH TEST 92 VERIFY THE CHARACTER ASSIGNMENT      01250205
C     STATEMENT                                                         01260205
C                                                                       01270205
C          CHARACTER VARIABLE = CHARACTER CONSTANT                      01280205
C                                                                       01290205
C     IS CORRECT.  THE VARIABLE AND CONSTANT ARE THE SAME LENGTH, AND   01300205
C     THE LENGTHS 3, 7, 12, 25, 41, AND 57 ARE USED IN THESE TESTS.     01310205
C                                                                       01320205
C                                                                       01330205
C     ****  FCVS PROGRAM 205  -  TEST 087  ****                         01340205
C                                                                       01350205
C                                                                       01360205
      IVTNUM =  87                                                      01370205
      IF (ICZERO) 30870, 0870, 30870                                    01380205
 0870 CONTINUE                                                          01390205
      IVCOMP = 0                                                        01400205
      CVTN01 = 'ABC'                                                    01410205
      IF (CVTN01 .EQ. 'ABC') IVCOMP = 1                                 01420205
      IVCORR = 1                                                        01430205
40870 IF (IVCOMP - 1) 20870, 10870, 20870                               01440205
30870 IVDELE = IVDELE + 1                                               01450205
      WRITE (I02,80000) IVTNUM                                          01460205
      IF (ICZERO) 10870, 0881, 20870                                    01470205
10870 IVPASS = IVPASS + 1                                               01480205
      WRITE (I02,80002) IVTNUM                                          01490205
      GO TO 0881                                                        01500205
20870 IVFAIL = IVFAIL + 1                                               01510205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01520205
 0881 CONTINUE                                                          01530205
C                                                                       01540205
C     ****  FCVS PROGRAM 205  -  TEST 088  ****                         01550205
C                                                                       01560205
C                                                                       01570205
      IVTNUM =  88                                                      01580205
      IF (ICZERO) 30880, 0880, 30880                                    01590205
 0880 CONTINUE                                                          01600205
      IVCOMP = 0                                                        01610205
      IVCORR = 1                                                        01620205
      CVTN02 = 'ABCDEFG'                                                01630205
      IF (CVTN02 .EQ. 'ABCDEFG') IVCOMP = 1                             01640205
40880 IF (IVCOMP - 1) 20880, 10880, 20880                               01650205
30880 IVDELE = IVDELE + 1                                               01660205
      WRITE (I02,80000) IVTNUM                                          01670205
      IF (ICZERO) 10880, 0891, 20880                                    01680205
10880 IVPASS = IVPASS + 1                                               01690205
      WRITE (I02,80002) IVTNUM                                          01700205
      GO TO 0891                                                        01710205
20880 IVFAIL = IVFAIL + 1                                               01720205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01730205
 0891 CONTINUE                                                          01740205
C                                                                       01750205
C     ****  FCVS PROGRAM 205  -  TEST 089  ****                         01760205
C                                                                       01770205
C                                                                       01780205
      IVTNUM =  89                                                      01790205
      IF (ICZERO) 30890, 0890, 30890                                    01800205
 0890 CONTINUE                                                          01810205
      IVCOMP = 0                                                        01820205
      IVCORR = 1                                                        01830205
      CVTN03 = 'ABCDEFGHIJKL'                                           01840205
      IF (CVTN03 .EQ. 'ABCDEFGHIJKL') IVCOMP = 1                        01850205
40890 IF (IVCOMP - 1) 20890, 10890, 20890                               01860205
30890 IVDELE = IVDELE + 1                                               01870205
      WRITE (I02,80000) IVTNUM                                          01880205
      IF (ICZERO) 10890, 0901, 20890                                    01890205
10890 IVPASS = IVPASS + 1                                               01900205
      WRITE (I02,80002) IVTNUM                                          01910205
      GO TO 0901                                                        01920205
20890 IVFAIL = IVFAIL + 1                                               01930205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01940205
 0901 CONTINUE                                                          01950205
C                                                                       01960205
C     ****  FCVS PROGRAM 205  -  TEST 090  ****                         01970205
C                                                                       01980205
C                                                                       01990205
      IVTNUM =  90                                                      02000205
      IF (ICZERO) 30900, 0900, 30900                                    02010205
 0900 CONTINUE                                                          02020205
      IVCOMP = 0                                                        02030205
      IVCORR = 1                                                        02040205
      CVTN04 = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                              02050205
      IF (CVTN04 .EQ. 'ABCDEFGHIJKLMNOPQRSTUVWXY') IVCOMP = 1           02060205
40900 IF (IVCOMP - 1) 20900, 10900, 20900                               02070205
30900 IVDELE = IVDELE + 1                                               02080205
      WRITE (I02,80000) IVTNUM                                          02090205
      IF (ICZERO) 10900, 0911, 20900                                    02100205
10900 IVPASS = IVPASS + 1                                               02110205
      WRITE (I02,80002) IVTNUM                                          02120205
      GO TO 0911                                                        02130205
20900 IVFAIL = IVFAIL + 1                                               02140205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02150205
 0911 CONTINUE                                                          02160205
C                                                                       02170205
C     ****  FCVS PROGRAM 205  -  TEST 091  ****                         02180205
C                                                                       02190205
C                                                                       02200205
      IVTNUM =  91                                                      02210205
      IF (ICZERO) 30910, 0910, 30910                                    02220205
 0910 CONTINUE                                                          02230205
      IVCOMP = 0                                                        02240205
      IVCORR = 1                                                        02250205
      CVTN05 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'              02260205
      IF (CVTN05 .EQ. 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO')      02270205
     1  IVCOMP = 1                                                      02280205
40910 IF (IVCOMP - 1) 20910, 10910, 20910                               02290205
30910 IVDELE = IVDELE + 1                                               02300205
      WRITE (I02,80000) IVTNUM                                          02310205
      IF (ICZERO) 10910, 0921, 20910                                    02320205
10910 IVPASS = IVPASS + 1                                               02330205
      WRITE (I02,80002) IVTNUM                                          02340205
      GO TO 0921                                                        02350205
20910 IVFAIL = IVFAIL + 1                                               02360205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02370205
 0921 CONTINUE                                                          02380205
C                                                                       02390205
C     ****  FCVS PROGRAM 205  -  TEST 092  ****                         02400205
C                                                                       02410205
C                                                                       02420205
      IVTNUM =  92                                                      02430205
      IF (ICZERO) 30920, 0920, 30920                                    02440205
 0920 CONTINUE                                                          02450205
      IVCOMP = 0                                                        02460205
      IVCORR = 1                                                        02470205
      CVTN06 =                                                          02480205
     1  'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE'     02490205
      IF (CVTN06 .EQ.                                                   02500205
     1  'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE')    02510205
     2     IVCOMP = 1                                                   02520205
40920 IF (IVCOMP - 1) 20920, 10920, 20920                               02530205
30920 IVDELE = IVDELE + 1                                               02540205
      WRITE (I02,80000) IVTNUM                                          02550205
      IF (ICZERO) 10920, 0931, 20920                                    02560205
10920 IVPASS = IVPASS + 1                                               02570205
      WRITE (I02,80002) IVTNUM                                          02580205
      GO TO 0931                                                        02590205
20920 IVFAIL = IVFAIL + 1                                               02600205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02610205
 0931 CONTINUE                                                          02620205
C                                                                       02630205
C          TEST 93 THROUGH TEST 96 VERIFY THE CHARACTER ASSIGNMENT      02640205
C     STATEMENTS                                                        02650205
C                                                                       02660205
C          CHARACTER VARIABLE = CHARACTER CONSTANT                      02670205
C          CHARACTER VARIABLE = CHARACTER VARIABLE                      02680205
C                                                                       02690205
C     ARE CORRECT.  THE VARIABLES AND CONSTANT ARE THE SAME LENGTH,     02700205
C     AND THE LENGTHS 3, 12, 25, AND 57 ARE USED IN THESE TESTS.        02710205
C                                                                       02720205
C                                                                       02730205
C     ****  FCVS PROGRAM 205  -  TEST 093  ****                         02740205
C                                                                       02750205
C                                                                       02760205
      IVTNUM =  93                                                      02770205
      IF (ICZERO) 30930, 0930, 30930                                    02780205
 0930 CONTINUE                                                          02790205
      IVCOMP = 0                                                        02800205
      IVCORR = 1                                                        02810205
      CVTN07 = '   '                                                    02820205
      CVTN01 = 'ABC'                                                    02830205
      CVTN07 = CVTN01                                                   02840205
      IF (CVTN07 .EQ. 'ABC') IVCOMP = 1                                 02850205
40930 IF (IVCOMP - 1) 20930, 10930, 20930                               02860205
30930 IVDELE = IVDELE + 1                                               02870205
      WRITE (I02,80000) IVTNUM                                          02880205
      IF (ICZERO) 10930, 0941, 20930                                    02890205
10930 IVPASS = IVPASS + 1                                               02900205
      WRITE (I02,80002) IVTNUM                                          02910205
      GO TO 0941                                                        02920205
20930 IVFAIL = IVFAIL + 1                                               02930205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02940205
 0941 CONTINUE                                                          02950205
C                                                                       02960205
C     ****  FCVS PROGRAM 205  -  TEST 094  ****                         02970205
C                                                                       02980205
C                                                                       02990205
      IVTNUM =  94                                                      03000205
      IF (ICZERO) 30940, 0940, 30940                                    03010205
 0940 CONTINUE                                                          03020205
      IVCOMP = 0                                                        03030205
      IVCORR = 1                                                        03040205
      CVTN03 = 'ABCDEFGHIJKL'                                           03050205
      CVTN09 = '            '                                           03060205
      CVTN09 = CVTN03                                                   03070205
      IF (CVTN09 .EQ. 'ABCDEFGHIJKL') IVCOMP = 1                        03080205
40940 IF (IVCOMP - 1) 20940, 10940, 20940                               03090205
30940 IVDELE = IVDELE + 1                                               03100205
      WRITE (I02,80000) IVTNUM                                          03110205
      IF (ICZERO) 10940, 0951, 20940                                    03120205
10940 IVPASS = IVPASS + 1                                               03130205
      WRITE (I02,80002) IVTNUM                                          03140205
      GO TO 0951                                                        03150205
20940 IVFAIL = IVFAIL + 1                                               03160205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03170205
 0951 CONTINUE                                                          03180205
C                                                                       03190205
C     ****  FCVS PROGRAM 205  -  TEST 095  ****                         03200205
C                                                                       03210205
C                                                                       03220205
      IVTNUM =  95                                                      03230205
      IF (ICZERO) 30950, 0950, 30950                                    03240205
 0950 CONTINUE                                                          03250205
      IVCOMP = 0                                                        03260205
      IVCORR = 1                                                        03270205
      CVTN04 = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                              03280205
      CVTN10 = '                         '                              03290205
      CVTN10 = CVTN04                                                   03300205
      IF (CVTN10 .EQ. 'ABCDEFGHIJKLMNOPQRSTUVWXY') IVCOMP = 1           03310205
40950 IF (IVCOMP - 1) 20950, 10950, 20950                               03320205
30950 IVDELE = IVDELE + 1                                               03330205
      WRITE (I02,80000) IVTNUM                                          03340205
      IF (ICZERO) 10950, 0961, 20950                                    03350205
10950 IVPASS = IVPASS + 1                                               03360205
      WRITE (I02,80002) IVTNUM                                          03370205
      GO TO 0961                                                        03380205
20950 IVFAIL = IVFAIL + 1                                               03390205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03400205
 0961 CONTINUE                                                          03410205
C                                                                       03420205
C     ****  FCVS PROGRAM 205  -  TEST 096  ****                         03430205
C                                                                       03440205
C                                                                       03450205
      IVTNUM =  96                                                      03460205
      IF (ICZERO) 30960, 0960, 30960                                    03470205
 0960 CONTINUE                                                          03480205
      IVCOMP = 0                                                        03490205
      IVCORR = 1                                                        03500205
      CVTN12 = '   '                                                    03510205
      CVTN06 =                                                          03520205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE'    03530205
      CVTN12 = CVTN06                                                   03540205
      IF (CVTN12 .EQ.                                                   03550205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE')   03560205
     2     IVCOMP = 1                                                   03570205
40960 IF (IVCOMP - 1) 20960, 10960, 20960                               03580205
30960 IVDELE = IVDELE + 1                                               03590205
      WRITE (I02,80000) IVTNUM                                          03600205
      IF (ICZERO) 10960, 0971, 20960                                    03610205
10960 IVPASS = IVPASS + 1                                               03620205
      WRITE (I02,80002) IVTNUM                                          03630205
      GO TO 0971                                                        03640205
20960 IVFAIL = IVFAIL + 1                                               03650205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03660205
 0971 CONTINUE                                                          03670205
C                                                                       03680205
C          TEST 97 AND TEST 98 VERIFY THE CHARACTER ASSIGNMENT          03690205
C     STATEMENT                                                         03700205
C                                                                       03710205
C          CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                 03720205
C                                                                       03730205
C     IS CORRECT.  THE ARRAY ELEMENT AND CONSTANT ARE THE SAME LENGTH,  03740205
C     AND THE LENGTHS 25 AND 41 ARE USED IN THESE TESTS.                03750205
C                                                                       03760205
C                                                                       03770205
C     ****  FCVS PROGRAM 205  -  TEST 097  ****                         03780205
C                                                                       03790205
C                                                                       03800205
      IVTNUM =  97                                                      03810205
      IF (ICZERO) 30970, 0970, 30970                                    03820205
 0970 CONTINUE                                                          03830205
      IVCOMP = 0                                                        03840205
      IVCORR = 1                                                        03850205
      CATN14(1) = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                           03860205
      IF (CATN14(1) .EQ. 'ABCDEFGHIJKLMNOPQRSTUVWXY') IVCOMP = 1        03870205
40970 IF (IVCOMP - 1) 20970, 10970, 20970                               03880205
30970 IVDELE = IVDELE + 1                                               03890205
      WRITE (I02,80000) IVTNUM                                          03900205
      IF (ICZERO) 10970, 0981, 20970                                    03910205
10970 IVPASS = IVPASS + 1                                               03920205
      WRITE (I02,80002) IVTNUM                                          03930205
      GO TO 0981                                                        03940205
20970 IVFAIL = IVFAIL + 1                                               03950205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          03960205
 0981 CONTINUE                                                          03970205
C                                                                       03980205
C     ****  FCVS PROGRAM 205  -  TEST 098  ****                         03990205
C                                                                       04000205
C                                                                       04010205
      IVTNUM =  98                                                      04020205
      IF (ICZERO) 30980, 0980, 30980                                    04030205
 0980 CONTINUE                                                          04040205
      IVCOMP = 0                                                        04050205
      IVCORR = 1                                                        04060205
      CATN15(8) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'           04070205
      IF (CATN15(8) .EQ.                                                04080205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO') IVCOMP = 1        04090205
40980 IF (IVCOMP - 1) 20980, 10980, 20980                               04100205
30980 IVDELE = IVDELE + 1                                               04110205
      WRITE (I02,80000) IVTNUM                                          04120205
      IF (ICZERO) 10980, 0991, 20980                                    04130205
10980 IVPASS = IVPASS + 1                                               04140205
      WRITE (I02,80002) IVTNUM                                          04150205
      GO TO 0991                                                        04160205
20980 IVFAIL = IVFAIL + 1                                               04170205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04180205
 0991 CONTINUE                                                          04190205
C                                                                       04200205
C          TEST 99 AND TEST 100 VERIFY THE CHARACTER ASSIGNMENT         04210205
C     STATEMENTS                                                        04220205
C                                                                       04230205
C              CHARACTER VARIABLE = CHARACTER CONSTANT                  04240205
C              CHARACTER ARRAY ELEMENT = CHARACTER VARIABLE             04250205
C                                                                       04260205
C     ARE CORRECT.  THE CHARACTER ENTITIES ARE THE SAME LENGTH,         04270205
C     AND THE LENGTHS 3 AND 57 ARE USED IN THESE TESTS.                 04280205
C                                                                       04290205
C                                                                       04300205
C     ****  FCVS PROGRAM 205  -  TEST 099  ****                         04310205
C                                                                       04320205
C                                                                       04330205
      IVTNUM =  99                                                      04340205
      IF (ICZERO) 30990, 0990, 30990                                    04350205
 0990 CONTINUE                                                          04360205
      IVCOMP = 0                                                        04370205
      IVCORR = 1                                                        04380205
      CVTN01 = 'ABC'                                                    04390205
      CATN11(5) = CVTN01                                                04400205
      IF (CATN11(5) .EQ. 'ABC') IVCOMP = 1                              04410205
40990 IF (IVCOMP - 1) 20990, 10990, 20990                               04420205
30990 IVDELE = IVDELE + 1                                               04430205
      WRITE (I02,80000) IVTNUM                                          04440205
      IF (ICZERO) 10990, 1001, 20990                                    04450205
10990 IVPASS = IVPASS + 1                                               04460205
      WRITE (I02,80002) IVTNUM                                          04470205
      GO TO 1001                                                        04480205
20990 IVFAIL = IVFAIL + 1                                               04490205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04500205
 1001 CONTINUE                                                          04510205
C                                                                       04520205
C     ****  FCVS PROGRAM 205  -  TEST 100  ****                         04530205
C                                                                       04540205
C                                                                       04550205
      IVTNUM = 100                                                      04560205
      IF (ICZERO) 31000, 1000, 31000                                    04570205
 1000 CONTINUE                                                          04580205
      IVCOMP = 0                                                        04590205
      IVCORR = 1                                                        04600205
      CVTN06 =                                                          04610205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE'    04620205
      CATN16(3) = CVTN06                                                04630205
      IF (CATN16(3) .EQ.                                                04640205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE')   04650205
     2     IVCOMP = 1                                                   04660205
41000 IF (IVCOMP - 1) 21000, 11000, 21000                               04670205
31000 IVDELE = IVDELE + 1                                               04680205
      WRITE (I02,80000) IVTNUM                                          04690205
      IF (ICZERO) 11000, 1011, 21000                                    04700205
11000 IVPASS = IVPASS + 1                                               04710205
      WRITE (I02,80002) IVTNUM                                          04720205
      GO TO 1011                                                        04730205
21000 IVFAIL = IVFAIL + 1                                               04740205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04750205
 1011 CONTINUE                                                          04760205
C                                                                       04770205
C          TEST 101 AND TEST 102 VERIFY THE CHARACTER ASSIGNMENT        04780205
C     STATEMENTS                                                        04790205
C                                                                       04800205
C          CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                 04810205
C          CHARACTER ARRAY ELEMENT = CHARACTER ARRAY ELEMENT            04820205
C                                                                       04830205
C     ARE CORRECT.  THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND     04840205
C     THE LENGTHS 7 AND 41 ARE USED IN THESE TESTS.                     04850205
C                                                                       04860205
C                                                                       04870205
C     ****  FCVS PROGRAM 205  -  TEST 101  ****                         04880205
C                                                                       04890205
C                                                                       04900205
      IVTNUM = 101                                                      04910205
      IF (ICZERO) 31010, 1010, 31010                                    04920205
 1010 CONTINUE                                                          04930205
      IVCOMP = 0                                                        04940205
      IVCORR = 1                                                        04950205
      CATN12(3) = 'ABCDEFG'                                             04960205
      CATN12(4) = CATN12(3)                                             04970205
      IF (CATN12(4) .EQ. 'ABCDEFG') IVCOMP = 1                          04980205
41010 IF (IVCOMP - 1) 21010, 11010, 21010                               04990205
31010 IVDELE = IVDELE + 1                                               05000205
      WRITE (I02,80000) IVTNUM                                          05010205
      IF (ICZERO) 11010, 1021, 21010                                    05020205
11010 IVPASS = IVPASS + 1                                               05030205
      WRITE (I02,80002) IVTNUM                                          05040205
      GO TO 1021                                                        05050205
21010 IVFAIL = IVFAIL + 1                                               05060205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05070205
 1021 CONTINUE                                                          05080205
C                                                                       05090205
C     ****  FCVS PROGRAM 205  -  TEST 102  ****                         05100205
C                                                                       05110205
C                                                                       05120205
      IVTNUM = 102                                                      05130205
      IF (ICZERO) 31020, 1020, 31020                                    05140205
 1020 CONTINUE                                                          05150205
      IVCOMP = 0                                                        05160205
      IVCORR = 1                                                        05170205
      CATN15(3) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'           05180205
      CATN15(4) = CATN15(3)                                             05190205
      IF (CATN15(4) .EQ.                                                05200205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO') IVCOMP = 1        05210205
41020 IF (IVCOMP - 1) 21020, 11020, 21020                               05220205
31020 IVDELE = IVDELE + 1                                               05230205
      WRITE (I02,80000) IVTNUM                                          05240205
      IF (ICZERO) 11020, 1031, 21020                                    05250205
11020 IVPASS = IVPASS + 1                                               05260205
      WRITE (I02,80002) IVTNUM                                          05270205
      GO TO 1031                                                        05280205
21020 IVFAIL = IVFAIL + 1                                               05290205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05300205
 1031 CONTINUE                                                          05310205
C                                                                       05320205
C          TEST 103 AND TEST 104 VERIFY THE CHARACTER ASSIGNMENT        05330205
C     STATEMENTS                                                        05340205
C                                                                       05350205
C          CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                 05360205
C          CHARACTER VARIABLE = CHARACTER ARRAY ELEMENT                 05370205
C                                                                       05380205
C     ARE CORRECT.  THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND     05390205
C     THE LENGTHS 12 AND 25 ARE USED.                                   05400205
C                                                                       05410205
C                                                                       05420205
C     ****  FCVS PROGRAM 205  -  TEST 103  ****                         05430205
C                                                                       05440205
C                                                                       05450205
      IVTNUM = 103                                                      05460205
      IF (ICZERO) 31030, 1030, 31030                                    05470205
 1030 CONTINUE                                                          05480205
      IVCOMP = 0                                                        05490205
      IVCORR = 1                                                        05500205
      CATN13(1) = 'ABCDEFGHIJKL'                                        05510205
      CVTN09 = '            '                                           05520205
      CVTN09 = CATN13(1)                                                05530205
      IF (CVTN09 .EQ. 'ABCDEFGHIJKL') IVCOMP = 1                        05540205
41030 IF (IVCOMP - 1) 21030, 11030, 21030                               05550205
31030 IVDELE = IVDELE + 1                                               05560205
      WRITE (I02,80000) IVTNUM                                          05570205
      IF (ICZERO) 11030, 1041, 21030                                    05580205
11030 IVPASS = IVPASS + 1                                               05590205
      WRITE (I02,80002) IVTNUM                                          05600205
      GO TO 1041                                                        05610205
21030 IVFAIL = IVFAIL + 1                                               05620205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05630205
 1041 CONTINUE                                                          05640205
C                                                                       05650205
C     ****  FCVS PROGRAM 205  -  TEST 104  ****                         05660205
C                                                                       05670205
C                                                                       05680205
      IVTNUM = 104                                                      05690205
      IF (ICZERO) 31040, 1040, 31040                                    05700205
 1040 CONTINUE                                                          05710205
      IVCOMP = 0                                                        05720205
      IVCORR = 1                                                        05730205
      CATN14(1) = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                           05740205
      CVTN10 = '                         '                              05750205
      CVTN10 = CATN14(1)                                                05760205
      IF (CVTN10 .EQ. 'ABCDEFGHIJKLMNOPQRSTUVWXY') IVCOMP = 1           05770205
41040 IF (IVCOMP - 1) 21040, 11040, 21040                               05780205
31040 IVDELE = IVDELE + 1                                               05790205
      WRITE (I02,80000) IVTNUM                                          05800205
      IF (ICZERO) 11040, 1051, 21040                                    05810205
11040 IVPASS = IVPASS + 1                                               05820205
      WRITE (I02,80002) IVTNUM                                          05830205
      GO TO 1051                                                        05840205
21040 IVFAIL = IVFAIL + 1                                               05850205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05860205
 1051 CONTINUE                                                          05870205
C                                                                       05880205
C          TEST 105 THROUGH TEST 110 VERIFY THE CHARACTER RELATIONAL    05890205
C     EXPRESSION USING EACH OF THE SIX RELATIONAL OPERATORS IN THE      05900205
C     STATEMENT FORM                                                    05910205
C                                                                       05920205
C          CHARACTER VARIABLE RELOP CHARACTER CONSTANT                  05930205
C                                                                       05940205
C     THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND THE LENGTHS       05950205
C     3, 7, 12, 25, 41, AND 57 ARE USED IN THESE TESTS.                 05960205
C                                                                       05970205
C                                                                       05980205
C     ****  FCVS PROGRAM 205  -  TEST 105  ****                         05990205
C                                                                       06000205
C                                                                       06010205
      IVTNUM = 105                                                      06020205
      IF (ICZERO) 31050, 1050, 31050                                    06030205
 1050 CONTINUE                                                          06040205
      IVCOMP = 0                                                        06050205
      IVCORR = 1                                                        06060205
      CVTN07 = 'ZAB'                                                    06070205
      IF (CVTN07 .EQ. 'ZAB') IVCOMP = 1                                 06080205
41050 IF (IVCOMP - 1) 21050, 11050, 21050                               06090205
31050 IVDELE = IVDELE + 1                                               06100205
      WRITE (I02,80000) IVTNUM                                          06110205
      IF (ICZERO) 11050, 1061, 21050                                    06120205
11050 IVPASS = IVPASS + 1                                               06130205
      WRITE (I02,80002) IVTNUM                                          06140205
      GO TO 1061                                                        06150205
21050 IVFAIL = IVFAIL + 1                                               06160205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06170205
 1061 CONTINUE                                                          06180205
C                                                                       06190205
C     ****  FCVS PROGRAM 205  -  TEST 106  ****                         06200205
C                                                                       06210205
C                                                                       06220205
      IVTNUM = 106                                                      06230205
      IF (ICZERO) 31060, 1060, 31060                                    06240205
 1060 CONTINUE                                                          06250205
      IVCOMP = 0                                                        06260205
      IVCORR = 1                                                        06270205
      CVTN08 = 'ABDDEEF'                                                06280205
      IF (CVTN08 .GT. 'ABCDEEF') IVCOMP = 1                             06290205
41060 IF (IVCOMP - 1) 21060, 11060, 21060                               06300205
31060 IVDELE = IVDELE + 1                                               06310205
      WRITE (I02,80000) IVTNUM                                          06320205
      IF (ICZERO) 11060, 1071, 21060                                    06330205
11060 IVPASS = IVPASS + 1                                               06340205
      WRITE (I02,80002) IVTNUM                                          06350205
      GO TO 1071                                                        06360205
21060 IVFAIL = IVFAIL + 1                                               06370205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06380205
 1071 CONTINUE                                                          06390205
C                                                                       06400205
C     ****  FCVS PROGRAM 205  -  TEST 107  ****                         06410205
C                                                                       06420205
C                                                                       06430205
      IVTNUM = 107                                                      06440205
      IF (ICZERO) 31070, 1070, 31070                                    06450205
 1070 CONTINUE                                                          06460205
      IVCOMP = 0                                                        06470205
      IVCORR = 1                                                        06480205
      CVTN09 = 'ZXYZZZABCDEF'                                           06490205
      IF (CVTN09 .LT. 'ZXYZZZACCDEF') IVCOMP = 1                        06500205
41070 IF (IVCOMP - 1) 21070, 11070, 21070                               06510205
31070 IVDELE = IVDELE + 1                                               06520205
      WRITE (I02,80000) IVTNUM                                          06530205
      IF (ICZERO) 11070, 1081, 21070                                    06540205
11070 IVPASS = IVPASS + 1                                               06550205
      WRITE (I02,80002) IVTNUM                                          06560205
      GO TO 1081                                                        06570205
21070 IVFAIL = IVFAIL + 1                                               06580205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06590205
 1081 CONTINUE                                                          06600205
C                                                                       06610205
C     ****  FCVS PROGRAM 205  -  TEST 108  ****                         06620205
C                                                                       06630205
C                                                                       06640205
      IVTNUM = 108                                                      06650205
      IF (ICZERO) 31080, 1080, 31080                                    06660205
 1080 CONTINUE                                                          06670205
      IVCOMP = 0                                                        06680205
      IVCORR = 1                                                        06690205
      CVTN10 = 'ABCDEFGHIJKKMNOPQRSTUVWXY'                              06700205
      IF ('ABCDEFGHIJKLMNOPQRSTUVWXY' .NE. CVTN10) IVCOMP = 1           06710205
41080 IF (IVCOMP - 1) 21080, 11080, 21080                               06720205
31080 IVDELE = IVDELE + 1                                               06730205
      WRITE (I02,80000) IVTNUM                                          06740205
      IF (ICZERO) 11080, 1091, 21080                                    06750205
11080 IVPASS = IVPASS + 1                                               06760205
      WRITE (I02,80002) IVTNUM                                          06770205
      GO TO 1091                                                        06780205
21080 IVFAIL = IVFAIL + 1                                               06790205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06800205
 1091 CONTINUE                                                          06810205
C                                                                       06820205
C     ****  FCVS PROGRAM 205  -  TEST 109  ****                         06830205
C                                                                       06840205
C                                                                       06850205
      IVTNUM = 109                                                      06860205
      IF (ICZERO) 31090, 1090, 31090                                    06870205
 1090 CONTINUE                                                          06880205
      IVCOMP = 0                                                        06890205
      IVCORR = 1                                                        06900205
      CVTN11 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZAABCDEFGHIJKLMN'              06910205
      IF ('ABCDEFGHIJKLMNOPQRSTUVWXYZABBCDEFGHIJKLMN' .GE. CVTN11)      06920205
     1     IVCOMP = 1                                                   06930205
41090 IF (IVCOMP - 1) 21090, 11090, 21090                               06940205
31090 IVDELE = IVDELE + 1                                               06950205
      WRITE (I02,80000) IVTNUM                                          06960205
      IF (ICZERO) 11090, 1101, 21090                                    06970205
11090 IVPASS = IVPASS + 1                                               06980205
      WRITE (I02,80002) IVTNUM                                          06990205
      GO TO 1101                                                        07000205
21090 IVFAIL = IVFAIL + 1                                               07010205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07020205
 1101 CONTINUE                                                          07030205
C                                                                       07040205
C     ****  FCVS PROGRAM 205  -  TEST 110  ****                         07050205
C                                                                       07060205
C                                                                       07070205
      IVTNUM = 110                                                      07080205
      IF (ICZERO) 31100, 1100, 31100                                    07090205
 1100 CONTINUE                                                          07100205
      IVCOMP = 0                                                        07110205
      IVCORR = 1                                                        07120205
      CVTN12 =                                                          07130205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZAAAAA'    07140205
      IF ('ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYAAAAAA'   07150205
     1    .LE. CVTN12) IVCOMP = 1                                       07160205
41100 IF (IVCOMP - 1) 21100, 11100, 21100                               07170205
31100 IVDELE = IVDELE + 1                                               07180205
      WRITE (I02,80000) IVTNUM                                          07190205
      IF (ICZERO) 11100, 1111, 21100                                    07200205
11100 IVPASS = IVPASS + 1                                               07210205
      WRITE (I02,80002) IVTNUM                                          07220205
      GO TO 1111                                                        07230205
21100 IVFAIL = IVFAIL + 1                                               07240205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07250205
 1111 CONTINUE                                                          07260205
C                                                                       07270205
C          TEST 111 AND TEST 112 VERIFY THE CHARACTER RELATIONAL        07280205
C     EXPRESSION OF THE FORM                                            07290205
C                                                                       07300205
C          CHARACTER VARIABLE RELOP CHARACTER VARIABLE                  07310205
C                                                                       07320205
C     THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND THE LENGTHS 3     07330205
C     AND 41 ARE USED IN THESE TESTS.                                   07340205
C                                                                       07350205
C                                                                       07360205
C     ****  FCVS PROGRAM 205  -  TEST 111  ****                         07370205
C                                                                       07380205
C                                                                       07390205
      IVTNUM = 111                                                      07400205
      IF (ICZERO) 31110, 1110, 31110                                    07410205
 1110 CONTINUE                                                          07420205
      IVCOMP = 1                                                        07430205
      IVCORR = 3                                                        07440205
      CVTN01 = 'ABC'                                                    07450205
      CVTN07 = 'BBC'                                                    07460205
      IF (CVTN01 .EQ. CVTN07) IVCOMP = IVCOMP * 2                       07470205
      IF (CVTN01 .NE. CVTN07) IVCOMP = IVCOMP * 3                       07480205
41110 IF (IVCOMP - 3) 21110, 11110, 21110                               07490205
31110 IVDELE = IVDELE + 1                                               07500205
      WRITE (I02,80000) IVTNUM                                          07510205
      IF (ICZERO) 11110, 1121, 21110                                    07520205
11110 IVPASS = IVPASS + 1                                               07530205
      WRITE (I02,80002) IVTNUM                                          07540205
      GO TO 1121                                                        07550205
21110 IVFAIL = IVFAIL + 1                                               07560205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07570205
 1121 CONTINUE                                                          07580205
C                                                                       07590205
C     ****  FCVS PROGRAM 205  -  TEST 112  ****                         07600205
C                                                                       07610205
C                                                                       07620205
      IVTNUM = 112                                                      07630205
      IF (ICZERO) 31120, 1120, 31120                                    07640205
 1120 CONTINUE                                                          07650205
      IVCOMP = 1                                                        07660205
      IVCORR = 6                                                        07670205
      CVTN05 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'              07680205
      CVTN11 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'              07690205
      IF (CVTN05 .GE. CVTN11) IVCOMP = IVCOMP * 2                       07700205
      IF (CVTN05 .LE. CVTN11) IVCOMP = IVCOMP * 3                       07710205
41120 IF (IVCOMP - 6) 21120, 11120, 21120                               07720205
31120 IVDELE = IVDELE + 1                                               07730205
      WRITE (I02,80000) IVTNUM                                          07740205
      IF (ICZERO) 11120, 1131, 21120                                    07750205
11120 IVPASS = IVPASS + 1                                               07760205
      WRITE (I02,80002) IVTNUM                                          07770205
      GO TO 1131                                                        07780205
21120 IVFAIL = IVFAIL + 1                                               07790205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07800205
 1131 CONTINUE                                                          07810205
C                                                                       07820205
C          TEST 113 AND TEST 114 VERIFY THE CHARACTER RELATIONAL        07830205
C     EXPRESSION OF THE FORM                                            07840205
C                                                                       07850205
C          CHARACTER ARRAY ELEMENT RELOP CHARACTER CONSTANT             07860205
C                                                                       07870205
C     THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND THE LENGTHS 7 AND 07880205
C     25 ARE USED IN THESE TESTS.                                       07890205
C                                                                       07900205
C                                                                       07910205
C     ****  FCVS PROGRAM 205  -  TEST 113  ****                         07920205
C                                                                       07930205
C                                                                       07940205
      IVTNUM = 113                                                      07950205
      IF (ICZERO) 31130, 1130, 31130                                    07960205
 1130 CONTINUE                                                          07970205
      IVCOMP = 1                                                        07980205
      IVCORR = 6                                                        07990205
      CATN12(3) = 'AB012CD'                                             08000205
      IF (CATN12(3) .LT. 'AB013CD') IVCOMP = IVCOMP * 2                 08010205
      IF ('AB013CD' .GT. CATN12(3)) IVCOMP = IVCOMP * 3                 08020205
41130 IF (IVCOMP - 6) 21130, 11130, 21130                               08030205
31130 IVDELE = IVDELE + 1                                               08040205
      WRITE (I02,80000) IVTNUM                                          08050205
      IF (ICZERO) 11130, 1141, 21130                                    08060205
11130 IVPASS = IVPASS + 1                                               08070205
      WRITE (I02,80002) IVTNUM                                          08080205
      GO TO 1141                                                        08090205
21130 IVFAIL = IVFAIL + 1                                               08100205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08110205
 1141 CONTINUE                                                          08120205
C                                                                       08130205
C     ****  FCVS PROGRAM 205  -  TEST 114  ****                         08140205
C                                                                       08150205
C                                                                       08160205
      IVTNUM = 114                                                      08170205
      IF (ICZERO) 31140, 1140, 31140                                    08180205
 1140 CONTINUE                                                          08190205
      IVCOMP = 1                                                        08200205
      IVCORR = 2                                                        08210205
      CATN14(1) = 'ABCDEFGHIJKLMNOPQRSTUVWXX'                           08220205
      IF (CATN14(1) .NE. 'ABCDEFGHIJKLMNOPQRSTUVWXY')                   08230205
     1     IVCOMP = IVCOMP * 2                                          08240205
      IF (CATN14(1) .EQ. 'ABCDEFGHIJKLMNOPQRSTUVWXY')                   08250205
     1     IVCOMP = IVCOMP * 3                                          08260205
41140 IF (IVCOMP - 2) 21140, 11140, 21140                               08270205
31140 IVDELE = IVDELE + 1                                               08280205
      WRITE (I02,80000) IVTNUM                                          08290205
      IF (ICZERO) 11140, 1151, 21140                                    08300205
11140 IVPASS = IVPASS + 1                                               08310205
      WRITE (I02,80002) IVTNUM                                          08320205
      GO TO 1151                                                        08330205
21140 IVFAIL = IVFAIL + 1                                               08340205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08350205
 1151 CONTINUE                                                          08360205
C                                                                       08370205
C     ****  FCVS PROGRAM 205  -  TEST 115  ****                         08380205
C                                                                       08390205
C          TEST 115 VERIFIES THE CHARACTER RELATIONAL EXPRESSION        08400205
C     OF THE FORM                                                       08410205
C                                                                       08420205
C          CHARACTER ARRAY ELEMENT RELOP CHARACTER VARIABLE             08430205
C                                                                       08440205
C     THE CHARACTER ENTITIES ARE 12 CHARACTERS IN LENGTH.               08450205
C                                                                       08460205
      IVTNUM = 115                                                      08470205
      IF (ICZERO) 31150, 1150, 31150                                    08480205
 1150 CONTINUE                                                          08490205
      IVCOMP = 1                                                        08500205
      IVCORR = 2                                                        08510205
      CATN13(3) = 'ABC+AAB/CDDF'                                        08520205
      IF (CATN13(3) .LT. 'BBC+AAB/CCCC') IVCOMP = IVCOMP * 2            08530205
      IF (CATN13(3) .GT. 'BBC+AAB/CCCC') IVCOMP = IVCOMP * 3            08540205
41150 IF (IVCOMP - 2) 21150, 11150, 21150                               08550205
31150 IVDELE = IVDELE + 1                                               08560205
      WRITE (I02,80000) IVTNUM                                          08570205
      IF (ICZERO) 11150, 1161, 21150                                    08580205
11150 IVPASS = IVPASS + 1                                               08590205
      WRITE (I02,80002) IVTNUM                                          08600205
      GO TO 1161                                                        08610205
21150 IVFAIL = IVFAIL + 1                                               08620205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08630205
 1161 CONTINUE                                                          08640205
C                                                                       08650205
C     ****  FCVS PROGRAM 205  -  TEST 116  ****                         08660205
C                                                                       08670205
C          TEST 116 VERIFIES THE CHARACTER RELATIONAL EXPRESSION        08680205
C     OF THE FORM                                                       08690205
C                                                                       08700205
C          CHARACTER ARRAY ELEMENT RELOP CHARACTER ARRAY ELEMENT        08710205
C                                                                       08720205
C     THE CHARACTER ENTITIES ARE 57 CHARACTERS IN LENGTH.               08730205
C                                                                       08740205
      IVTNUM = 116                                                      08750205
      IF (ICZERO) 31160, 1160, 31160                                    08760205
 1160 CONTINUE                                                          08770205
      IVCOMP = 1                                                        08780205
      IVCORR = 30                                                       08790205
      CATN16(1) =                                                       08800205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ//012'    08810205
      CATN16(2) =                                                       08820205
     1   'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ//112'    08830205
      IF (CATN16 (1) .LT. CATN16 (2)) IVCOMP = IVCOMP * 2               08840205
      IF (CATN16 (1) .NE. CATN16 (2)) IVCOMP = IVCOMP * 3               08850205
      IF (CATN16 (1) .LE. CATN16 (2)) IVCOMP = IVCOMP * 5               08860205
41160 IF (IVCOMP - 30) 21160, 11160, 21160                              08870205
31160 IVDELE = IVDELE + 1                                               08880205
      WRITE (I02,80000) IVTNUM                                          08890205
      IF (ICZERO) 11160, 1171, 21160                                    08900205
11160 IVPASS = IVPASS + 1                                               08910205
      WRITE (I02,80002) IVTNUM                                          08920205
      GO TO 1171                                                        08930205
21160 IVFAIL = IVFAIL + 1                                               08940205
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08950205
 1171 CONTINUE                                                          08960205
C                                                                       08970205
C                                                                       08980205
C     WRITE OUT TEST SUMMARY                                            08990205
C                                                                       09000205
      WRITE (I02,90004)                                                 09010205
      WRITE (I02,90014)                                                 09020205
      WRITE (I02,90004)                                                 09030205
      WRITE (I02,90000)                                                 09040205
      WRITE (I02,90004)                                                 09050205
      WRITE (I02,90020) IVFAIL                                          09060205
      WRITE (I02,90022) IVPASS                                          09070205
      WRITE (I02,90024) IVDELE                                          09080205
      STOP                                                              09090205
90001 FORMAT (1H ,24X,5HFM205)                                          09100205
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM205)                          09110205
C                                                                       09120205
C     FORMATS FOR TEST DETAIL LINES                                     09130205
C                                                                       09140205
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   09150205
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      09160205
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         09170205
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    09180205
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        09190205
C                                                                       09200205
C     FORMAT STATEMENTS FOR PAGE HEADERS                                09210205
C                                                                       09220205
90002 FORMAT (1H1)                                                      09230205
90004 FORMAT (1H )                                                      09240205
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09250205
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   09260205
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         09270205
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  09280205
90014 FORMAT (1H ,5X,46H----------------------------------------------) 09290205
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             09300205
C                                                                       09310205
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 09320205
C                                                                       09330205
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              09340205
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              09350205
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             09360205
      END                                                               09370205
