C                                                                       00010018
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020018
C                                                                       00030018
C     FM018                                                             00040018
C                                                                       00050018
C             THIS ROUTINE CONTINUES TESTS OF THE FORTRAN               00060018
C     LOGICAL    IF STATEMENT IN ALL OF THE VARIOUS FORMS.    THE       00070018
C     FOLLOWING LOGICAL OPERANDS ARE USED FOR THIS ROUTINE - LOGICAL    00080018
C     CONSTANTS, LOGICAL VARIABLES, LOGICAL ARRAY ELEMENTS, AND         00090018
C     ARITHMETIC EXPRESSIONS WITH VARIOUS RELATIONAL OPERATORS.  BOTH   00100018
C     THE TRUE AND FALSE BRANCHES ARE TESTED IN THE SERIES OF TESTS.    00110018
C                                                                       00120018
C      REFERENCES                                                       00130018
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140018
C              X3.9-1978                                                00150018
C                                                                       00160018
C        SECTION 4.7.1, LOGICAL CONSTANT                                00170018
C        SECTION 6, EXPRESSIONS                                         00180018
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00190018
C        SECTION 6.3, RELATIONAL EXPRESSIONS                            00200018
C        SECTION 6.4, LOGICAL EXPRESSIONS                               00210018
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00220018
C        SECTION 10, ASSIGNMENT STATEMENTS                              00230018
C        SECTION 10.2, LOGICAL ASSIGNMENT STATEMENT                     00240018
C        SECTION 11.5, LOGICAL IF STATEMENT                             00250018
C                                                                       00260018
      LOGICAL  LCTNT1, LCTNT2, LATN1A(2)                                00270018
      DIMENSION IADN11(2)                                               00280018
C                                                                       00290018
C      **********************************************************       00300018
C                                                                       00310018
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00320018
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00330018
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00340018
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00350018
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00360018
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00370018
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00380018
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00390018
C     OF EXECUTING THESE TESTS.                                         00400018
C                                                                       00410018
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00420018
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00430018
C                                                                       00440018
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00450018
C                                                                       00460018
C                  DEPARTMENT OF THE NAVY                               00470018
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00480018
C                  WASHINGTON, D.C.  20376                              00490018
C                                                                       00500018
C      **********************************************************       00510018
C                                                                       00520018
C                                                                       00530018
C                                                                       00540018
C     INITIALIZATION SECTION                                            00550018
C                                                                       00560018
C     INITIALIZE CONSTANTS                                              00570018
C      **************                                                   00580018
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00590018
      I01 = 5                                                           00600018
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00610018
      I02 = 6                                                           00620018
C     SYSTEM ENVIRONMENT SECTION                                        00630018
C                                                                       00640018
      I01 = 5                                                           00650018
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00660018
C     (UNIT NUMBER FOR CARD READER).                                    00670018
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00680018
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00690018
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00700018
C                                                                       00710018
      I02 = 6                                                           00720018
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00730018
C     (UNIT NUMBER FOR PRINTER).                                        00740018
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00750018
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00760018
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00770018
C                                                                       00780018
      IVPASS=0                                                          00790018
      IVFAIL=0                                                          00800018
      IVDELE=0                                                          00810018
      ICZERO=0                                                          00820018
C                                                                       00830018
C     WRITE PAGE HEADERS                                                00840018
      WRITE (I02,90000)                                                 00850018
      WRITE (I02,90001)                                                 00860018
      WRITE (I02,90002)                                                 00870018
      WRITE (I02, 90002)                                                00880018
      WRITE (I02,90003)                                                 00890018
      WRITE (I02,90002)                                                 00900018
      WRITE (I02,90004)                                                 00910018
      WRITE (I02,90002)                                                 00920018
      WRITE (I02,90011)                                                 00930018
      WRITE (I02,90002)                                                 00940018
      WRITE (I02,90002)                                                 00950018
      WRITE (I02,90005)                                                 00960018
      WRITE (I02,90006)                                                 00970018
      WRITE (I02,90002)                                                 00980018
      IVTNUM = 500                                                      00990018
C                                                                       01000018
C      ****  TEST 500  ****                                             01010018
C     TEST 500  -  LIKE TEST 197.  TRUE  .OR.  TRUE    TRUE PATH        01020018
C           TEST OF THE FORTRAN INCLUSIVE OR  (LE)  .OR.  (LT)          01030018
C                                                                       01040018
C                                                                       01050018
      IF (ICZERO) 35000, 5000, 35000                                    01060018
 5000 CONTINUE                                                          01070018
      IVON01 = 0                                                        01080018
      LCTNT1 = .TRUE.                                                   01090018
      LCTNT2 = .TRUE.                                                   01100018
      IF ( LCTNT1 .OR. LCTNT2 )  IVON01 = 1                             01110018
      GO TO 45000                                                       01120018
35000 IVDELE = IVDELE + 1                                               01130018
      WRITE (I02,80003) IVTNUM                                          01140018
      IF (ICZERO) 45000, 5011, 45000                                    01150018
45000 IF ( IVON01 - 1 )  25000, 15000, 25000                            01160018
15000 IVPASS = IVPASS + 1                                               01170018
      WRITE (I02,80001) IVTNUM                                          01180018
      GO TO 5011                                                        01190018
25000 IVFAIL = IVFAIL + 1                                               01200018
      IVCOMP = IVON01                                                   01210018
      IVCORR = 1                                                        01220018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01230018
 5011 CONTINUE                                                          01240018
      IVTNUM = 501                                                      01250018
C                                                                       01260018
C      ****  TEST 501  ****                                             01270018
C     TEST 501  -  TEST OF PARENTHESES AROUND A LOGICAL EXPRESSION      01280018
C           (  (LE)  )  .OR.  (LT)                                      01290018
C           USES LOGICAL VARIABLES SET IN LOGICAL ASSIGNMENT  STATEMENTS01300018
C           ( FALSE )  .OR.  FALSE    FALSE PATH                        01310018
C                                                                       01320018
C                                                                       01330018
      IF (ICZERO) 35010, 5010, 35010                                    01340018
 5010 CONTINUE                                                          01350018
      IVON01 = 1                                                        01360018
      LCTNT1 = .FALSE.                                                  01370018
      LCTNT2 = .FALSE.                                                  01380018
      IF ( (LCTNT1) .OR. LCTNT2 )  IVON01 = 0                           01390018
      GO TO 45010                                                       01400018
35010 IVDELE = IVDELE + 1                                               01410018
      WRITE (I02,80003) IVTNUM                                          01420018
      IF (ICZERO) 45010, 5021, 45010                                    01430018
45010 IF ( IVON01 - 1 )  25010, 15010, 25010                            01440018
15010 IVPASS = IVPASS + 1                                               01450018
      WRITE (I02,80001) IVTNUM                                          01460018
      GO TO 5021                                                        01470018
25010 IVFAIL = IVFAIL + 1                                               01480018
      IVCOMP = IVON01                                                   01490018
      IVCORR = 1                                                        01500018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01510018
 5021 CONTINUE                                                          01520018
      IVTNUM = 502                                                      01530018
C                                                                       01540018
C      ****  TEST 502  ****                                             01550018
C     TEST 502  -  LIKE TEST 501 EXCEPT THAT IT IT IS OF THE FORM       01560018
C           (LE)  .OR.  ( (LT) )        TRUE  .OR.  (TRUE)              01570018
C           TRUE PATH                                                   01580018
C                                                                       01590018
C                                                                       01600018
      IF (ICZERO) 35020, 5020, 35020                                    01610018
 5020 CONTINUE                                                          01620018
      IVON01 = 0                                                        01630018
      LCTNT1 = .TRUE.                                                   01640018
      LCTNT2 = .TRUE.                                                   01650018
      IF ( LCTNT1 .OR. ( LCTNT2 ) )   IVON01 = 1                        01660018
      GO TO 45020                                                       01670018
35020 IVDELE = IVDELE + 1                                               01680018
      WRITE (I02,80003) IVTNUM                                          01690018
      IF (ICZERO) 45020, 5031, 45020                                    01700018
45020 IF ( IVON01 - 1 )  25020, 15020, 25020                            01710018
15020 IVPASS = IVPASS + 1                                               01720018
      WRITE (I02,80001) IVTNUM                                          01730018
      GO TO 5031                                                        01740018
25020 IVFAIL = IVFAIL + 1                                               01750018
      IVCOMP = IVON01                                                   01760018
      IVCORR = 1                                                        01770018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01780018
 5031 CONTINUE                                                          01790018
      IVTNUM = 503                                                      01800018
C                                                                       01810018
C      ****  TEST 503  ****                                             01820018
C     TEST 503  -  TEST OF PARENTHESES IN LOGICAL EXPRESSIONS           01830018
C           (  (LE)  )  .OR.  (  (LT)  )                                01840018
C           (FALSE) .OR. (TRUE)    TRUE PATH                            01850018
C                                                                       01860018
C                                                                       01870018
      IF (ICZERO) 35030, 5030, 35030                                    01880018
 5030 CONTINUE                                                          01890018
      IVON01 = 0                                                        01900018
      LCTNT1 = .FALSE.                                                  01910018
      LCTNT2 = .TRUE.                                                   01920018
      IF ( (LCTNT1) .OR. (LCTNT2) )  IVON01 = 1                         01930018
      GO TO 45030                                                       01940018
35030 IVDELE = IVDELE + 1                                               01950018
      WRITE (I02,80003) IVTNUM                                          01960018
      IF (ICZERO) 45030, 5041, 45030                                    01970018
45030 IF ( IVON01 - 1 )  25030, 15030, 25030                            01980018
15030 IVPASS = IVPASS + 1                                               01990018
      WRITE (I02,80001) IVTNUM                                          02000018
      GO TO 5041                                                        02010018
25030 IVFAIL = IVFAIL + 1                                               02020018
      IVCOMP = IVON01                                                   02030018
      IVCORR = 1                                                        02040018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02050018
 5041 CONTINUE                                                          02060018
      IVTNUM = 504                                                      02070018
C                                                                       02080018
C      ****  TEST 504  ****                                             02090018
C     TEST 504  -  LIKE TEST 503 ONLY MORE PARENTHESES   TRUE PATH      02100018
C                                                                       02110018
C                                                                       02120018
      IF (ICZERO) 35040, 5040, 35040                                    02130018
 5040 CONTINUE                                                          02140018
      IVON01 = 0                                                        02150018
      LCTNT1 = .TRUE.                                                   02160018
      LCTNT2 = .FALSE.                                                  02170018
      IF ( ( (LCTNT1) .OR. (LCTNT2) ) )  IVON01 = 1                     02180018
      GO TO 45040                                                       02190018
35040 IVDELE = IVDELE + 1                                               02200018
      WRITE (I02,80003) IVTNUM                                          02210018
      IF (ICZERO) 45040, 5051, 45040                                    02220018
45040 IF ( IVON01 - 1 )  25040, 15040, 25040                            02230018
15040 IVPASS = IVPASS + 1                                               02240018
      WRITE (I02,80001) IVTNUM                                          02250018
      GO TO 5051                                                        02260018
25040 IVFAIL = IVFAIL + 1                                               02270018
      IVCOMP = IVON01                                                   02280018
      IVCORR = 1                                                        02290018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02300018
 5051 CONTINUE                                                          02310018
      IVTNUM = 505                                                      02320018
C                                                                       02330018
C      ****  TEST 505  ****                                             02340018
C     TEST 505  -  TEST OF PARENTHESES WITH .AND.  FALSE PATH           02350018
C                                                                       02360018
C                                                                       02370018
      IF (ICZERO) 35050, 5050, 35050                                    02380018
 5050 CONTINUE                                                          02390018
      IVON01 = 1                                                        02400018
      LCTNT1 = .FALSE.                                                  02410018
      LCTNT2 = .FALSE.                                                  02420018
      IF ( (LCTNT1) .AND. LCTNT2 )  IVON01 = 0                          02430018
      GO TO 45050                                                       02440018
35050 IVDELE = IVDELE + 1                                               02450018
      WRITE (I02,80003) IVTNUM                                          02460018
      IF (ICZERO) 45050, 5061, 45050                                    02470018
45050 IF ( IVON01 - 1 )  25050, 15050, 25050                            02480018
15050 IVPASS = IVPASS + 1                                               02490018
      WRITE (I02,80001) IVTNUM                                          02500018
      GO TO 5061                                                        02510018
25050 IVFAIL = IVFAIL + 1                                               02520018
      IVCOMP = IVON01                                                   02530018
      IVCORR = 1                                                        02540018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02550018
 5061 CONTINUE                                                          02560018
      IVTNUM = 506                                                      02570018
C                                                                       02580018
C      ****  TEST 506  ****                                             02590018
C     TEST 506  -  LIKE TEST 505  FALSE PATH                            02600018
C                                                                       02610018
C                                                                       02620018
      IF (ICZERO) 35060, 5060, 35060                                    02630018
 5060 CONTINUE                                                          02640018
      IVON01 = 1                                                        02650018
      LCTNT1 = .FALSE.                                                  02660018
      LCTNT2 = .TRUE.                                                   02670018
      IF ( LCTNT1 .AND. (LCTNT2) )  IVON01 = 0                          02680018
      GO TO 45060                                                       02690018
35060 IVDELE = IVDELE + 1                                               02700018
      WRITE (I02,80003) IVTNUM                                          02710018
      IF (ICZERO) 45060, 5071, 45060                                    02720018
45060 IF ( IVON01 - 1 )  25060, 15060, 25060                            02730018
15060 IVPASS = IVPASS + 1                                               02740018
      WRITE (I02,80001) IVTNUM                                          02750018
      GO TO 5071                                                        02760018
25060 IVFAIL = IVFAIL + 1                                               02770018
      IVCOMP = IVON01                                                   02780018
      IVCORR = 1                                                        02790018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02800018
 5071 CONTINUE                                                          02810018
      IVTNUM = 507                                                      02820018
C                                                                       02830018
C      ****  TEST 507  ****                                             02840018
C     TEST 507  -  MORE PARENTHESES WITH LOGICAL .AND.  FALSE PATH      02850018
C                                                                       02860018
C                                                                       02870018
      IF (ICZERO) 35070, 5070, 35070                                    02880018
 5070 CONTINUE                                                          02890018
      IVON01 = 1                                                        02900018
      LCTNT1 = .TRUE.                                                   02910018
      LCTNT2 = .FALSE.                                                  02920018
      IF ( (LCTNT1) .AND. (LCTNT2) )  IVON01 = 0                        02930018
      GO TO 45070                                                       02940018
35070 IVDELE = IVDELE + 1                                               02950018
      WRITE (I02,80003) IVTNUM                                          02960018
      IF (ICZERO) 45070, 5081, 45070                                    02970018
45070 IF ( IVON01 - 1 )  25070, 15070, 25070                            02980018
15070 IVPASS = IVPASS + 1                                               02990018
      WRITE (I02,80001) IVTNUM                                          03000018
      GO TO 5081                                                        03010018
25070 IVFAIL = IVFAIL + 1                                               03020018
      IVCOMP = IVON01                                                   03030018
      IVCORR = 1                                                        03040018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03050018
 5081 CONTINUE                                                          03060018
      IVTNUM = 508                                                      03070018
C                                                                       03080018
C      ****  TEST 508  ****                                             03090018
C     TEST 508  -  TEST OF LOGICAL .NOT. WITH PARENTHESES AROUND A LOGIC03100018
C           PRIMARY.  FOR THIS TEST A LOGICAL ARRAY ELEMENT IS USED AS  03110018
C           THE LOGICAL PRIMARY.  .NOT. (FALSE)   TRUE PATH.            03120018
C                                                                       03130018
C                                                                       03140018
      IF (ICZERO) 35080, 5080, 35080                                    03150018
 5080 CONTINUE                                                          03160018
      IVON01 = 0                                                        03170018
      LATN1A(1) = .FALSE.                                               03180018
      IF ( .NOT. (LATN1A(1)) )  IVON01 = 1                              03190018
      GO TO 45080                                                       03200018
35080 IVDELE = IVDELE + 1                                               03210018
      WRITE (I02,80003) IVTNUM                                          03220018
      IF (ICZERO) 45080, 5091, 45080                                    03230018
45080 IF ( IVON01 - 1 )  25080, 15080, 25080                            03240018
15080 IVPASS = IVPASS + 1                                               03250018
      WRITE (I02,80001) IVTNUM                                          03260018
      GO TO 5091                                                        03270018
25080 IVFAIL = IVFAIL + 1                                               03280018
      IVCOMP = IVON01                                                   03290018
      IVCORR = 1                                                        03300018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03310018
 5091 CONTINUE                                                          03320018
      IVTNUM = 509                                                      03330018
C                                                                       03340018
C      ****  TEST 509  ****                                             03350018
C     TEST 509  -  LIKE TEST 508 EXCEPT THAT THE WHOLE EXPRESSION       03360018
C           IS IN PARENTHESES.  FALSE PATH                              03370018
C                                                                       03380018
C                                                                       03390018
      IF (ICZERO) 35090, 5090, 35090                                    03400018
 5090 CONTINUE                                                          03410018
      IVON01 = 1                                                        03420018
      LATN1A(2) = .TRUE.                                                03430018
      IF ( ( .NOT. (LATN1A(2)) ) )  IVON01 = 0                          03440018
      GO TO 45090                                                       03450018
35090 IVDELE = IVDELE + 1                                               03460018
      WRITE (I02,80003) IVTNUM                                          03470018
      IF (ICZERO) 45090, 5101, 45090                                    03480018
45090 IF ( IVON01 - 1 )  25090, 15090, 25090                            03490018
15090 IVPASS = IVPASS + 1                                               03500018
      WRITE (I02,80001) IVTNUM                                          03510018
      GO TO 5101                                                        03520018
25090 IVFAIL = IVFAIL + 1                                               03530018
      IVCOMP = IVON01                                                   03540018
      IVCORR = 1                                                        03550018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03560018
 5101 CONTINUE                                                          03570018
      IVTNUM = 510                                                      03580018
C                                                                       03590018
C      ****  TEST 510  ****                                             03600018
C     TEST 510  -  INTEGER CONSTANT EXPONIENTATION                      03610018
C           RELATIONAL EXPRESSION USING  .EQ.  TRUE PATH                03620018
C                                                                       03630018
C                                                                       03640018
      IF (ICZERO) 35100, 5100, 35100                                    03650018
 5100 CONTINUE                                                          03660018
      IVON01 = 0                                                        03670018
      IF ( 3 ** 3 .EQ. 27 )  IVON01 = 1                                 03680018
      GO TO 45100                                                       03690018
35100 IVDELE = IVDELE + 1                                               03700018
      WRITE (I02,80003) IVTNUM                                          03710018
      IF (ICZERO) 45100, 5111, 45100                                    03720018
45100 IF ( IVON01 - 1 )  25100, 15100, 25100                            03730018
15100 IVPASS = IVPASS + 1                                               03740018
      WRITE (I02,80001) IVTNUM                                          03750018
      GO TO 5111                                                        03760018
25100 IVFAIL = IVFAIL + 1                                               03770018
      IVCOMP = IVON01                                                   03780018
      IVCORR = 1                                                        03790018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03800018
 5111 CONTINUE                                                          03810018
      IVTNUM = 511                                                      03820018
C                                                                       03830018
C      ****  TEST 511  ****                                             03840018
C     TEST 511  -  EXPONIENTIATION USING AN INTEGER VARIABLE            03850018
C           RELATIONAL EXPRESSION USING  .NE.  FALSE PATH               03860018
C                                                                       03870018
C                                                                       03880018
      IF (ICZERO) 35110, 5110, 35110                                    03890018
 5110 CONTINUE                                                          03900018
      IVON01 = 1                                                        03910018
      IVON02 = 3                                                        03920018
      IF ( IVON02 ** 3 .NE. 27 )  IVON01 = 0                            03930018
      GO TO 45110                                                       03940018
35110 IVDELE = IVDELE + 1                                               03950018
      WRITE (I02,80003) IVTNUM                                          03960018
      IF (ICZERO) 45110, 5121, 45110                                    03970018
45110 IF ( IVON01 - 1 )  25110, 15110, 25110                            03980018
15110 IVPASS = IVPASS + 1                                               03990018
      WRITE (I02,80001) IVTNUM                                          04000018
      GO TO 5121                                                        04010018
25110 IVFAIL = IVFAIL + 1                                               04020018
      IVCOMP = IVON01                                                   04030018
      IVCORR = 1                                                        04040018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04050018
 5121 CONTINUE                                                          04060018
      IVTNUM = 512                                                      04070018
C                                                                       04080018
C      ****  TEST 512  ****                                             04090018
C     TEST 512  -  LIKE TEST 511  USES  .LE.  TRUE PATH                 04100018
C                                                                       04110018
C                                                                       04120018
      IF (ICZERO) 35120, 5120, 35120                                    04130018
 5120 CONTINUE                                                          04140018
      IVON01 = 0                                                        04150018
      IVON02 = 3                                                        04160018
      IF ( 3 ** IVON02 .LE. 27 )  IVON01 = 1                            04170018
      GO TO 45120                                                       04180018
35120 IVDELE = IVDELE + 1                                               04190018
      WRITE (I02,80003) IVTNUM                                          04200018
      IF (ICZERO) 45120, 5131, 45120                                    04210018
45120 IF ( IVON01 - 1 )  25120, 15120, 25120                            04220018
15120 IVPASS = IVPASS + 1                                               04230018
      WRITE (I02,80001) IVTNUM                                          04240018
      GO TO 5131                                                        04250018
25120 IVFAIL = IVFAIL + 1                                               04260018
      IVCOMP = IVON01                                                   04270018
      IVCORR = 1                                                        04280018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04290018
 5131 CONTINUE                                                          04300018
      IVTNUM = 513                                                      04310018
C                                                                       04320018
C      ****  TEST 513  ****                                             04330018
C     TEST 513  -  LIKE TEST 511 BUT USES ALL INTEGER VARIABLES         04340018
C           RELATIONAL EXPRESSION USES  .LT.  FALSE PATH                04350018
C                                                                       04360018
C                                                                       04370018
      IF (ICZERO) 35130, 5130, 35130                                    04380018
 5130 CONTINUE                                                          04390018
      IVON01 = 1                                                        04400018
      IVON02 = 3                                                        04410018
      IVON03 = 27                                                       04420018
      IF ( IVON02 ** IVON02 .LT. IVON03 )  IVON01 = 0                   04430018
      GO TO 45130                                                       04440018
35130 IVDELE = IVDELE + 1                                               04450018
      WRITE (I02,80003) IVTNUM                                          04460018
      IF (ICZERO) 45130, 5141, 45130                                    04470018
45130 IF ( IVON01 - 1 )  25130, 15130, 25130                            04480018
15130 IVPASS = IVPASS + 1                                               04490018
      WRITE (I02,80001) IVTNUM                                          04500018
      GO TO 5141                                                        04510018
25130 IVFAIL = IVFAIL + 1                                               04520018
      IVCOMP = IVON01                                                   04530018
      IVCORR = 1                                                        04540018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04550018
 5141 CONTINUE                                                          04560018
      IVTNUM = 514                                                      04570018
C                                                                       04580018
C      ****  TEST 514  ****                                             04590018
C     TEST 514  -  LIKE TEST 511 BUT USES INTEGER ARRAY ELEMENTS        04600018
C           RELATIONAL EXPRESSION USES .GE.  TRUE PATH                  04610018
C                                                                       04620018
C                                                                       04630018
      IF (ICZERO) 35140, 5140, 35140                                    04640018
 5140 CONTINUE                                                          04650018
      IVON01 = 0                                                        04660018
      IVON02 = 3                                                        04670018
      IADN11(1) = 3                                                     04680018
      IADN11(2) = 27                                                    04690018
      IF ( IADN11(1) ** IVON02 .GE. IADN11(2) )  IVON01 = 1             04700018
      GO TO 45140                                                       04710018
35140 IVDELE = IVDELE + 1                                               04720018
      WRITE (I02,80003) IVTNUM                                          04730018
      IF (ICZERO) 45140, 5151, 45140                                    04740018
45140 IF ( IVON01 - 1 )  25140, 15140, 25140                            04750018
15140 IVPASS = IVPASS + 1                                               04760018
      WRITE (I02,80001) IVTNUM                                          04770018
      GO TO 5151                                                        04780018
25140 IVFAIL = IVFAIL + 1                                               04790018
      IVCOMP = IVON01                                                   04800018
      IVCORR = 1                                                        04810018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04820018
 5151 CONTINUE                                                          04830018
      IVTNUM = 515                                                      04840018
C                                                                       04850018
C      ****  TEST 515  ****                                             04860018
C     TEST 515  -  LIKE TEST 514 BUT USES ALL INTEGER ARRAY ELEMENTS    04870018
C           RELATIONAL EXPRESSION USES  .GT.  FALSE PATH                04880018
C                                                                       04890018
C                                                                       04900018
      IF (ICZERO) 35150, 5150, 35150                                    04910018
 5150 CONTINUE                                                          04920018
      IVON01 = 1                                                        04930018
      IADN11(1) = 3                                                     04940018
      IADN11(2) = 27                                                    04950018
      IF ( IADN11(1) ** IADN11(1) .GT. IADN11(2) )  IVON01 = 0          04960018
      GO TO 45150                                                       04970018
35150 IVDELE = IVDELE + 1                                               04980018
      WRITE (I02,80003) IVTNUM                                          04990018
      IF (ICZERO) 45150, 5161, 45150                                    05000018
45150 IF ( IVON01 - 1 )  25150, 15150, 25150                            05010018
15150 IVPASS = IVPASS + 1                                               05020018
      WRITE (I02,80001) IVTNUM                                          05030018
      GO TO 5161                                                        05040018
25150 IVFAIL = IVFAIL + 1                                               05050018
      IVCOMP = IVON01                                                   05060018
      IVCORR = 1                                                        05070018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05080018
 5161 CONTINUE                                                          05090018
      IVTNUM = 516                                                      05100018
C                                                                       05110018
C      ****  TEST 516  ****                                             05120018
C     TEST 516  -  TEST OF INTEGER MULTIPLICATION USING INTEGER         05130018
C           CONSTANTS.  RELATIONAL EXPRESSION USES  .LT.  TRUE PATH     05140018
C                                                                       05150018
C                                                                       05160018
      IF (ICZERO) 35160, 5160, 35160                                    05170018
 5160 CONTINUE                                                          05180018
      IVON01 = 0                                                        05190018
      IVON02 = 587                                                      05200018
      IF ( 3 * 3 .LT. IVON02 )  IVON01 = 1                              05210018
      GO TO 45160                                                       05220018
35160 IVDELE = IVDELE + 1                                               05230018
      WRITE (I02,80003) IVTNUM                                          05240018
      IF (ICZERO) 45160, 5171, 45160                                    05250018
45160 IF ( IVON01 - 1 )  25160, 15160, 25160                            05260018
15160 IVPASS = IVPASS + 1                                               05270018
      WRITE (I02,80001) IVTNUM                                          05280018
      GO TO 5171                                                        05290018
25160 IVFAIL = IVFAIL + 1                                               05300018
      IVCOMP = IVON01                                                   05310018
      IVCORR = 1                                                        05320018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05330018
 5171 CONTINUE                                                          05340018
      IVTNUM = 517                                                      05350018
C                                                                       05360018
C      ****  TEST 517  ****                                             05370018
C     TEST 517  -  INTEGER MULTIPLICATION WITH INTEGER CONSTANTS,       05380018
C           VARIABLES, AND ARRAY ELEMENTS.  RELATIONAL EXPRESSION USES  05390018
C           .GT.  FALSE PATH                                            05400018
C                                                                       05410018
C                                                                       05420018
      IF (ICZERO) 35170, 5170, 35170                                    05430018
 5170 CONTINUE                                                          05440018
      IVON01 = 1                                                        05450018
      IVON02 = 32767                                                    05460018
      IADN11(1) = 3                                                     05470018
      IF ( IADN11(1) * 587 .GT. IVON02 )  IVON01 = 0                    05480018
      GO TO 45170                                                       05490018
35170 IVDELE = IVDELE + 1                                               05500018
      WRITE (I02,80003) IVTNUM                                          05510018
      IF (ICZERO) 45170, 5181, 45170                                    05520018
45170 IF ( IVON01 - 1 )  25170, 15170, 25170                            05530018
15170 IVPASS = IVPASS + 1                                               05540018
      WRITE (I02,80001) IVTNUM                                          05550018
      GO TO 5181                                                        05560018
25170 IVFAIL = IVFAIL + 1                                               05570018
      IVCOMP = IVON01                                                   05580018
      IVCORR = 1                                                        05590018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05600018
 5181 CONTINUE                                                          05610018
      IVTNUM = 518                                                      05620018
C                                                                       05630018
C      ****  TEST 518  ****                                             05640018
C     TEST 518  -  INTEGER MULTIPLICATION AND EXPONIENTATION            05650018
C           RELATIONAL EXPRESSION USES  .EQ.  TRUE PATH                 05660018
C                                                                       05670018
C                                                                       05680018
      IF (ICZERO) 35180, 5180, 35180                                    05690018
 5180 CONTINUE                                                          05700018
      IVON01 = 0                                                        05710018
      IVON02 = 3                                                        05720018
      IVON03 = 27                                                       05730018
      IADN11(2) = 3                                                     05740018
      IF ( IADN11(2) ** 2 * IVON02 .EQ. IVON03 )  IVON01 = 1            05750018
      GO TO 45180                                                       05760018
35180 IVDELE = IVDELE + 1                                               05770018
      WRITE (I02,80003) IVTNUM                                          05780018
      IF (ICZERO) 45180, 5191, 45180                                    05790018
45180 IF ( IVON01 - 1 )  25180, 15180, 25180                            05800018
15180 IVPASS = IVPASS + 1                                               05810018
      WRITE (I02,80001) IVTNUM                                          05820018
      GO TO 5191                                                        05830018
25180 IVFAIL = IVFAIL + 1                                               05840018
      IVCOMP = IVON01                                                   05850018
      IVCORR = 1                                                        05860018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05870018
 5191 CONTINUE                                                          05880018
      IVTNUM = 519                                                      05890018
C                                                                       05900018
C      ****  TEST 519  ****                                             05910018
C     TEST 519  -  INTEGER DIVISION.  RELATIONAL EXPRESSION  .NE.       05920018
C           FALSE PATH                                                  05930018
C                                                                       05940018
C                                                                       05950018
      IF (ICZERO) 35190, 5190, 35190                                    05960018
 5190 CONTINUE                                                          05970018
      IVON01 = 1                                                        05980018
      IVON02 = 27                                                       05990018
      IADN11(1) = 3                                                     06000018
      IF ( IVON02 / 9 .NE. IADN11(1) )  IVON01 = 0                      06010018
      GO TO 45190                                                       06020018
35190 IVDELE = IVDELE + 1                                               06030018
      WRITE (I02,80003) IVTNUM                                          06040018
      IF (ICZERO) 45190, 5201, 45190                                    06050018
45190 IF ( IVON01 - 1 )  25190, 15190, 25190                            06060018
15190 IVPASS = IVPASS + 1                                               06070018
      WRITE (I02,80001) IVTNUM                                          06080018
      GO TO 5201                                                        06090018
25190 IVFAIL = IVFAIL + 1                                               06100018
      IVCOMP = IVON01                                                   06110018
      IVCORR = 1                                                        06120018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06130018
 5201 CONTINUE                                                          06140018
      IVTNUM = 520                                                      06150018
C                                                                       06160018
C      ****  TEST 520  ****                                             06170018
C     TEST 520  -  INTEGER VARIABLE DIVISION.  RELATIONAL EXPRESSION    06180018
C           USES .GE.  TRUE PATH                                        06190018
C                                                                       06200018
C                                                                       06210018
      IF (ICZERO) 35200, 5200, 35200                                    06220018
 5200 CONTINUE                                                          06230018
      IVON01 = 0                                                        06240018
      IVON02 = 32767                                                    06250018
      IVON03 = 3                                                        06260018
      IVON04 = 9999                                                     06270018
      IVON05 = 587                                                      06280018
      IF ( IVON02 / IVON03 .GE. IVON04 / IVON05 )  IVON01 = 1           06290018
      GO TO 45200                                                       06300018
35200 IVDELE = IVDELE + 1                                               06310018
      WRITE (I02,80003) IVTNUM                                          06320018
      IF (ICZERO) 45200, 5211, 45200                                    06330018
45200 IF ( IVON01 - 1 )  25200, 15200, 25200                            06340018
15200 IVPASS = IVPASS + 1                                               06350018
      WRITE (I02,80001) IVTNUM                                          06360018
      GO TO 5211                                                        06370018
25200 IVFAIL = IVFAIL + 1                                               06380018
      IVCOMP = IVON01                                                   06390018
      IVCORR = 1                                                        06400018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06410018
 5211 CONTINUE                                                          06420018
      IVTNUM = 521                                                      06430018
C                                                                       06440018
C      ****  TEST 521  ****                                             06450018
C     TEST 521  -  INTEGER DIVISION AND EXPONIENTATION                  06460018
C           RELATIONAL EXPRESSION USES  .LT.  FALSE PATH                06470018
C                                                                       06480018
C                                                                       06490018
      IF (ICZERO) 35210, 5210, 35210                                    06500018
 5210 CONTINUE                                                          06510018
      IVON01 = 1                                                        06520018
      IVON02 = 587                                                      06530018
      IVON03 = 3                                                        06540018
      IADN11(2) = 3                                                     06550018
      IF ( IVON02 / IADN11(2) ** 3 .LT. 3 ** IVON03 / IVON02 ) IVON01 =006560018
      IF ( IVON02 / IADN11(2) ** 3 .LT. 3 ** IVON03 / IVON02 )  IVON01=006570018
      GO TO 45210                                                       06580018
35210 IVDELE = IVDELE + 1                                               06590018
      WRITE (I02,80003) IVTNUM                                          06600018
      IF (ICZERO) 45210, 5221, 45210                                    06610018
45210 IF ( IVON01 - 1 )  25210, 15210, 25210                            06620018
15210 IVPASS = IVPASS + 1                                               06630018
      WRITE (I02,80001) IVTNUM                                          06640018
      GO TO 5221                                                        06650018
25210 IVFAIL = IVFAIL + 1                                               06660018
      IVCOMP = IVON01                                                   06670018
      IVCORR = 1                                                        06680018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06690018
 5221 CONTINUE                                                          06700018
      IVTNUM = 522                                                      06710018
C                                                                       06720018
C      ****  TEST 522  ****                                             06730018
C     TEST 522  -  TESTS 522 THRU 535 ARE TESTS OF SIGNED TERMS         06740018
C           +(T)  ALSO  -(T)                                            06750018
C           RELATIONAL EXPRESSION USES .GT.  TRUE PATH                  06760018
C                                                                       06770018
C                                                                       06780018
      IF (ICZERO) 35220, 5220, 35220                                    06790018
 5220 CONTINUE                                                          06800018
      IVON01 = 0                                                        06810018
      IF ( 3 .GT. -3 )  IVON01 = 1                                      06820018
      GO TO 45220                                                       06830018
35220 IVDELE = IVDELE + 1                                               06840018
      WRITE (I02,80003) IVTNUM                                          06850018
      IF (ICZERO) 45220, 5231, 45220                                    06860018
45220 IF ( IVON01 - 1 )  25220, 15220, 25220                            06870018
15220 IVPASS = IVPASS + 1                                               06880018
      WRITE (I02,80001) IVTNUM                                          06890018
      GO TO 5231                                                        06900018
25220 IVFAIL = IVFAIL + 1                                               06910018
      IVCOMP = IVON01                                                   06920018
      IVCORR = 1                                                        06930018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06940018
 5231 CONTINUE                                                          06950018
      IVTNUM = 523                                                      06960018
C                                                                       06970018
C      ****  TEST 523  ****                                             06980018
C     TEST 523  -  TEST OF SIGNED ZERO  .LT.  FALSE PATH                06990018
C                                                                       07000018
C                                                                       07010018
      IF (ICZERO) 35230, 5230, 35230                                    07020018
 5230 CONTINUE                                                          07030018
      IVON01 = 1                                                        07040018
      IF ( 0 .LT. -0 )  IVON01 = 0                                      07050018
      GO TO 45230                                                       07060018
35230 IVDELE = IVDELE + 1                                               07070018
      WRITE (I02,80003) IVTNUM                                          07080018
      IF (ICZERO) 45230, 5241, 45230                                    07090018
45230 IF ( IVON01 - 1 )  25230, 15230, 25230                            07100018
15230 IVPASS = IVPASS + 1                                               07110018
      WRITE (I02,80001) IVTNUM                                          07120018
      GO TO 5241                                                        07130018
25230 IVFAIL = IVFAIL + 1                                               07140018
      IVCOMP = IVON01                                                   07150018
      IVCORR = 1                                                        07160018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07170018
 5241 CONTINUE                                                          07180018
      IVTNUM = 524                                                      07190018
C                                                                       07200018
C      ****  TEST 524  ****                                             07210018
C     TEST 524  -  TEST OF SIGNED ZERO  .LE.  TRUE PATH                 07220018
C                                                                       07230018
C                                                                       07240018
      IF (ICZERO) 35240, 5240, 35240                                    07250018
 5240 CONTINUE                                                          07260018
      IVON01 = 0                                                        07270018
      IF ( 0 .LE. -0 )  IVON01 = 1                                      07280018
      GO TO 45240                                                       07290018
35240 IVDELE = IVDELE + 1                                               07300018
      WRITE (I02,80003) IVTNUM                                          07310018
      IF (ICZERO) 45240, 5251, 45240                                    07320018
45240 IF ( IVON01 - 1 )  25240, 15240, 25240                            07330018
15240 IVPASS = IVPASS + 1                                               07340018
      WRITE (I02,80001) IVTNUM                                          07350018
      GO TO 5251                                                        07360018
25240 IVFAIL = IVFAIL + 1                                               07370018
      IVCOMP = IVON01                                                   07380018
      IVCORR = 1                                                        07390018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07400018
 5251 CONTINUE                                                          07410018
      IVTNUM = 525                                                      07420018
C                                                                       07430018
C      ****  TEST 525  ****                                             07440018
C     TEST 525  -  TEST OF SIGNED ZERO  .EQ.  TRUE PATH                 07450018
C                                                                       07460018
C                                                                       07470018
      IF (ICZERO) 35250, 5250, 35250                                    07480018
 5250 CONTINUE                                                          07490018
      IVON01 = 0                                                        07500018
      IF ( 0 .EQ. -0 )  IVON01 = 1                                      07510018
      GO TO 45250                                                       07520018
35250 IVDELE = IVDELE + 1                                               07530018
      WRITE (I02,80003) IVTNUM                                          07540018
      IF (ICZERO) 45250, 5261, 45250                                    07550018
45250 IF ( IVON01 - 1 )  25250, 15250, 25250                            07560018
15250 IVPASS = IVPASS + 1                                               07570018
      WRITE (I02,80001) IVTNUM                                          07580018
      GO TO 5261                                                        07590018
25250 IVFAIL = IVFAIL + 1                                               07600018
      IVCOMP = IVON01                                                   07610018
      IVCORR = 1                                                        07620018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07630018
 5261 CONTINUE                                                          07640018
      IVTNUM = 526                                                      07650018
C                                                                       07660018
C      ****  TEST 526  ****                                             07670018
C     TEST 526  -  TEST OF SIGNED ZERO  .NE.  FALSE PATH                07680018
C                                                                       07690018
C                                                                       07700018
      IF (ICZERO) 35260, 5260, 35260                                    07710018
 5260 CONTINUE                                                          07720018
      IVON01 = 1                                                        07730018
      IF ( 0 .NE. -0 )  IVON01 = 0                                      07740018
      GO TO 45260                                                       07750018
35260 IVDELE = IVDELE + 1                                               07760018
      WRITE (I02,80003) IVTNUM                                          07770018
      IF (ICZERO) 45260, 5271, 45260                                    07780018
45260 IF ( IVON01 - 1 )  25260, 15260, 25260                            07790018
15260 IVPASS = IVPASS + 1                                               07800018
      WRITE (I02,80001) IVTNUM                                          07810018
      GO TO 5271                                                        07820018
25260 IVFAIL = IVFAIL + 1                                               07830018
      IVCOMP = IVON01                                                   07840018
      IVCORR = 1                                                        07850018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07860018
 5271 CONTINUE                                                          07870018
      IVTNUM = 527                                                      07880018
C                                                                       07890018
C      ****  TEST 527  ****                                             07900018
C     TEST 527  -  TEST OF SIGNED ZERO  .GE.  TRUE PATH                 07910018
C                                                                       07920018
C                                                                       07930018
      IF (ICZERO) 35270, 5270, 35270                                    07940018
 5270 CONTINUE                                                          07950018
      IVON01 = 0                                                        07960018
      IF ( 0 .GE. -0 )  IVON01 = 1                                      07970018
      GO TO 45270                                                       07980018
35270 IVDELE = IVDELE + 1                                               07990018
      WRITE (I02,80003) IVTNUM                                          08000018
      IF (ICZERO) 45270, 5281, 45270                                    08010018
45270 IF ( IVON01 - 1 )  25270, 15270, 25270                            08020018
15270 IVPASS = IVPASS + 1                                               08030018
      WRITE (I02,80001) IVTNUM                                          08040018
      GO TO 5281                                                        08050018
25270 IVFAIL = IVFAIL + 1                                               08060018
      IVCOMP = IVON01                                                   08070018
      IVCORR = 1                                                        08080018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08090018
 5281 CONTINUE                                                          08100018
      IVTNUM = 528                                                      08110018
C                                                                       08120018
C      ****  TEST 528  ****                                             08130018
C     TEST 528  -  TEST OF SIGNED ZERO  .GT.  FALSE PATH                08140018
C                                                                       08150018
C                                                                       08160018
      IF (ICZERO) 35280, 5280, 35280                                    08170018
 5280 CONTINUE                                                          08180018
      IVON01 = 1                                                        08190018
      IF ( 0 .GT. -0 )  IVON01 = 0                                      08200018
      GO TO 45280                                                       08210018
35280 IVDELE = IVDELE + 1                                               08220018
      WRITE (I02,80003) IVTNUM                                          08230018
      IF (ICZERO) 45280, 5291, 45280                                    08240018
45280 IF ( IVON01 - 1 )  25280, 15280, 25280                            08250018
15280 IVPASS = IVPASS + 1                                               08260018
      WRITE (I02,80001) IVTNUM                                          08270018
      GO TO 5291                                                        08280018
25280 IVFAIL = IVFAIL + 1                                               08290018
      IVCOMP = IVON01                                                   08300018
      IVCORR = 1                                                        08310018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08320018
 5291 CONTINUE                                                          08330018
      IVTNUM = 529                                                      08340018
C                                                                       08350018
C      ****  TEST 529  ****                                             08360018
C     TEST 529  -  TEST OF 32767 AND -32766  .GT.  TRUE PATH            08370018
C                                                                       08380018
C                                                                       08390018
      IF (ICZERO) 35290, 5290, 35290                                    08400018
 5290 CONTINUE                                                          08410018
      IVON01 = 0                                                        08420018
      IF ( 32767 .GT. -32766 )  IVON01 = 1                              08430018
      GO TO 45290                                                       08440018
35290 IVDELE = IVDELE + 1                                               08450018
      WRITE (I02,80003) IVTNUM                                          08460018
      IF (ICZERO) 45290, 5301, 45290                                    08470018
45290 IF ( IVON01 - 1 )  25290, 15290, 25290                            08480018
15290 IVPASS = IVPASS + 1                                               08490018
      WRITE (I02,80001) IVTNUM                                          08500018
      GO TO 5301                                                        08510018
25290 IVFAIL = IVFAIL + 1                                               08520018
      IVCOMP = IVON01                                                   08530018
      IVCORR = 1                                                        08540018
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08550018
 5301 CONTINUE                                                          08560018
C                                                                       08570018
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             08580018
99999 CONTINUE                                                          08590018
      WRITE (I02,90002)                                                 08600018
      WRITE (I02,90006)                                                 08610018
      WRITE (I02,90002)                                                 08620018
      WRITE (I02,90002)                                                 08630018
      WRITE (I02,90007)                                                 08640018
      WRITE (I02,90002)                                                 08650018
      WRITE (I02,90008)  IVFAIL                                         08660018
      WRITE (I02,90009) IVPASS                                          08670018
      WRITE (I02,90010) IVDELE                                          08680018
C                                                                       08690018
C                                                                       08700018
C     TERMINATE ROUTINE EXECUTION                                       08710018
      STOP                                                              08720018
C                                                                       08730018
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08740018
90000 FORMAT (1H1)                                                      08750018
90002 FORMAT (1H )                                                      08760018
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08770018
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08780018
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08790018
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08800018
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08810018
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08820018
C                                                                       08830018
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08840018
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08850018
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08860018
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08870018
C                                                                       08880018
C     FORMAT STATEMENTS FOR TEST RESULTS                                08890018
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08900018
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08910018
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08920018
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08930018
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08940018
C                                                                       08950018
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM018)                          08960018
      END                                                               08970018
